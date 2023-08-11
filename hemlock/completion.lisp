;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Written by Skef Wholey and Blaine Burks.
;;; General idea stolen from Jim Salem's TMC LISPM completion code.
;;;

(in-package :hemlock)



;;;; The Completion Database.

;;; The top level structure here is an array that gets indexed with the
;;; first three characters of the word to be completed.  That will get us to
;;; a list of the strings with that prefix sorted in most-recently-used order.
;;; The number of strings in any given bucket will never exceed
;;; Completion-Bucket-Size-Limit.  Strings are stored in the database in
;;; lowercase form always.

;; this stuff was moved into gui application for initialization - akw




;;; Mapping strings into buckets.

;;; The characters that are considered parts of "words" change from mode
;;; to mode.
;;;



;;; The difference between Lisp mode and the other modes is pretty radical in
;;; this respect.  These are interesting too, but they're on by default: #\*,
;;; #\-, and #\_.  #\' is on by default too, but it's uninteresting in "Lisp"
;;; mode.
;;;




(defmacro completion-char-p (char)
  `(= (the fixnum (character-attribute :completion-wordchar ,char)) 1))

;;; COMPLETION-BUCKET-FOR returns the Completion-Bucket that might hold a
;;; completion for the given String.  With optional Value, sets the bucket.
;;;
(defun completion-bucket-for (string length &optional (value nil value-p))
  (declare (simple-string string)
           (fixnum length))
  (when (and (>= length 3)
             (completion-char-p (char string 0))
             (completion-char-p (char string 1))
             (completion-char-p (char string 2)))
    (let ((index (mod (logxor (ash
                               (logxor
                                (ash (hi::search-hash-code (schar string 0))
                                     5)
                                (hi::search-hash-code (schar string 1)))
                               3)
                              (hi::search-hash-code (schar string 2)))
                      completion-table-size)))
      (declare (fixnum index))
      (if value-p
          (setf (svref *completions* index) value)
          (svref *completions* index)))))

(defsetf completion-bucket-for completion-bucket-for)


;;; FIND-COMPLETION returns the most recent string matching the given
;;; Prefix, or Nil if nothing appropriate is in the database.  We assume
;;; the Prefix is passed to us in lowercase form so we can use String=.  If
;;; we find something appropriate, we bring it to the front of the list.
;;; Prefix-Length, if supplied restricts us to look at just the start of
;;; the string...
;;;
(defun find-completion (prefix &optional (prefix-length (length prefix)))
  (declare (simple-string prefix)
           (fixnum prefix-length))
  (let ((bucket (completion-bucket-for prefix prefix-length)))
    (do ((list bucket (cdr list)))
        ((null list))
      (let ((completion (car list)))
        (declare (simple-string completion))
        (when (and (>= (length completion) prefix-length)
                   (string= prefix completion
                            :end1 prefix-length
                            :end2 prefix-length))
          (unless (eq list bucket)
            (rotatef (car list) (car bucket)))
          (return completion))))))

;;; RECORD-COMPLETION saves string in the completion database as the first item
;;; in the bucket, that's the most recently used completion.  If the bucket is
;;; full, drop the oldest item in the list.  If string is already in the
;;; bucket, simply move it to the front.  The way we move an element to the
;;; front requires a full bucket to be at least three elements long.
;;;
(defun record-completion (string)
  (declare (simple-string string))
  (let ((string-length (length string)))
    (declare (fixnum string-length))
    (when (> string-length 3)
      (let ((bucket (completion-bucket-for string string-length))
            (limit (value completion-bucket-size)))
        (do ((list bucket (cdr list))
             (last nil list)
             (length 1 (1+ length)))
            ((null list)
             (setf (completion-bucket-for string string-length)
                   (cons string bucket)))
          (cond ((= length limit)
                 (setf (car list) string)
                 (setf (completion-bucket-for string string-length) list)
                 (setf (cdr list) bucket)
                 (setf (cdr last) nil)
                 (return))
                ((string= string (the simple-string (car list)))
                 (unless (eq list bucket)
                   (rotatef (car list) (car bucket)))
                 (return))))))))

;;; ROTATE-COMPLETIONS rotates the completion bucket for the given Prefix.
;;; We just search for the first thing in the bucket with the Prefix, then
;;; move that to the end of the list.  If there ain't no such thing there,
;;; or if it's already at the end, we do nothing.
;;;
(defun rotate-completions (prefix &optional (prefix-length (length prefix)))
  (declare (simple-string prefix))
  (let ((bucket (completion-bucket-for prefix prefix-length)))
    (do ((list bucket (cdr list))
         (prev nil list))
        ((null list))
      (let ((completion (car list)))
        (declare (simple-string completion))
        (when (and (>= (length completion) prefix-length)
                   (string= prefix completion
                            :end1 prefix-length :end2 prefix-length))
          (when (cdr list)
            (if prev
                (setf (cdr prev) (cdr list))
                (setf (completion-bucket-for prefix prefix-length) (cdr list)))
            (setf (cdr (last list)) list)
            (setf (cdr list) nil))
          (return nil))))))



;;;; Hemlock interface.



(defcommand "Completion Mode" (window p)
  "Toggles Completion Mode in the current buffer."
  "Toggles Completion Mode in the current buffer."
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (setf (buffer-minor-mode buffer buffer "Completion")
          (not (buffer-minor-mode buffer "Completion")))))


;;; Consecutive alphanumeric keystrokes that start a word cause a possible
;;; completion to be displayed in the echo area's modeline, the status line.
;;; Since most insertion is building up a word that was already started, we
;;; keep track of the word in *completion-prefix* that the user is typing.  The
;;; length of the thing is kept in *completion-prefix-length*.
;;;
(defconstant completion-prefix-max-size 100)

(defvar *completion-prefix* (make-string completion-prefix-max-size))

(defvar *completion-prefix-length* 0)


;;; "Completion Self Insert" does different stuff depending on whether or
;;; not the thing to be inserted is Completion-Char-P.  If it is, then we
;;; try to come up with a possible completion, using Last-Command-Type to
;;; tense things up a bit.  Otherwise, if Last-Command-Type says we were
;;; just doing a word, then we record that word in the database.
;;;
(defcommand "Completion Self Insert" (window p)
  "Insert the last character typed, showing possible completions.  With prefix
   argument insert the character that many times."
  "Implements \"Completion Self Insert\". Calling this function is not
   meaningful."
  (let ((char (hemlock-ext:key-event-char *last-key-event-typed*)))
    (unless char (editor-error "Can't insert that character."))
    (cond ((completion-char-p char)
           ;; If start of word not already in *completion-prefix*, put it
           ;; there.
           (unless (eq (last-command-type) :completion-self-insert)
             (set-completion-prefix (window-buffer window)))
           ;; Then add new stuff.
           (cond ((and p (> p 1))
                  (fill *completion-prefix* (char-downcase char)
                        :start *completion-prefix-length*
                        :end (+ *completion-prefix-length* p))
                  (incf *completion-prefix-length* p))
                 (t
                  (setf (schar *completion-prefix* *completion-prefix-length*)
                        (char-downcase char))
                  (incf *completion-prefix-length*)))
           ;; Display possible completion, if any.
           (display-possible-completion *completion-prefix*
                                        *completion-prefix-length*)
           (setf (last-command-type) :completion-self-insert))
          (t
           (when (eq (last-command-type) :completion-self-insert)
             (record-completion (subseq *completion-prefix*
                                        0 *completion-prefix-length*)))))))

;;; SET-COMPLETION-PREFIX grabs any completion-wordchars immediately before
;;; point and stores these into *completion-prefix*.
;;;
(defun set-completion-prefix (buffer)
  (let* ((point (buffer-point buffer))
         (point-line (mark-line point)))
    (cond ((and (previous-character buffer point)
                (completion-char-p (previous-character buffer point)))
           (with-mark ((mark point))
             (reverse-find-attribute buffer mark :completion-wordchar #'zerop)
             (unless (eq (mark-line mark) point-line)
               (editor-error "No completion wordchars on this line!"))
             (let ((insert-string (nstring-downcase
                                   (region-to-string buffer
                                    (region mark point)))))
               (replace *completion-prefix* insert-string)
               (setq *completion-prefix-length* (length insert-string)))))
          (t
           (setq *completion-prefix-length* 0)))))


(defcommand "Completion Complete Word" (window p)
  "Complete the word if we've got a completion, fixing up the case.  Invoking
   this immediately in succession rotates through possible completions in the
   buffer.  If there is no currently displayed completion, this tries to choose
   a completion from text immediately before the point and displays the
   completion if found."
  "Complete the word if we've got a completion, fixing up the case."
  (declare (ignore p))
  (let ((last-command-type (last-command-type)))
    ;; If the user has been cursoring around and then tries to complete,
    ;; let him.
    ;;
    (unless (member last-command-type '(:completion-self-insert :completion))
      (set-completion-prefix (window-buffer window))
      (setf last-command-type :completion-self-insert))
    (case last-command-type
      (:completion-self-insert
       (do-completion (window-buffer window)))
      (:completion
       (rotate-completions *completion-prefix* *completion-prefix-length*)
       (do-completion (window-buffer window)))))
  (setf (last-command-type) :completion))

(defcommand "List Possible Completions" (window p)
  "List all possible completions of the prefix the user has typed."
  "List all possible completions of the prefix the user has typed."
  (declare (ignore p))
  (let ((last-command-type (last-command-type)))
    (unless (member last-command-type '(:completion-self-insert :completion))
      (set-completion-prefix (window-buffer window)))
    (let* ((prefix *completion-prefix*)
           (prefix-length *completion-prefix-length*)
           (bucket (completion-bucket-for prefix prefix-length)))
      (with-pop-up-display (s :current-window window)
        (dolist (completion bucket)
          (when (and (> (length completion) prefix-length)
                     (string= completion prefix
                              :end1 prefix-length
                              :end2 prefix-length))
            (write-line completion s))))))
  ;; Keep the redisplay hook from clearing any possibly displayed completion.
  (setf (last-command-type) :completion-self-insert))

(defvar *last-completion-mark* nil)

(defun do-completion (buffer)
  (let ((completion (find-completion *completion-prefix*
                                     *completion-prefix-length*))
        (point (buffer-point buffer)))
    (when completion
      (if *last-completion-mark*
          (move-mark *last-completion-mark* point)
          (setq *last-completion-mark* (copy-mark point :temporary)))
      (let ((mark *last-completion-mark*))
        (reverse-find-attribute buffer mark :completion-wordchar #'zerop)
        (let* ((region (region mark point))
               (string (region-to-string buffer region)))
          (declare (simple-string string))
          (delete-region buffer region)
          (let* ((first (position-if #'alpha-char-p string))
                 (next (if first (position-if #'alpha-char-p string
                                              :start (1+ first)))))
            ;; Often completions start with asterisks when hacking on Lisp
            ;; code, so we look for alphabetic characters.
            (insert-string buffer point
                           ;; Leave the cascading IF's alone.
                           ;; Writing this as a COND, using LOWER-CASE-P as
                           ;; the test is not equivalent to this code since
                           ;; numbers (and such) are nil for LOWER-CASE-P and
                           ;; UPPER-CASE-P.
                           (if (and first (upper-case-p (schar string first)))
                               (if (and next
                                        (upper-case-p (schar string next)))
                                   (string-upcase completion)
                                   (word-capitalize completion))
                               completion))))))))


;;; WORD-CAPITALIZE is like STRING-CAPITALIZE except that it treats apostrophes
;;; the Right Way.
;;;
(defun word-capitalize (string)
  (let* ((length (length string))
         (strung (make-string length)))
    (do  ((i 0 (1+ i))
          (new-word t))
         ((= i length))
      (let ((char (schar string i)))
        (cond ((or (alphanumericp char)
                   (char= char #\'))
               (setf (schar strung i)
                     (if new-word (char-upcase char) (char-downcase char)))
               (setq new-word nil))
              (t
               (setf (schar strung i) char)
               (setq new-word t)))))
    strung))

(defcommand "Completion Rotate Completions" (window p)
  "Show another possible completion in the status line, if there is one.
   If there is no currently displayed completion, this tries to choose a
   completion from text immediately before the point and displays the
   completion if found.  With an argument, rotate the completion ring that many
   times."
  "Show another possible completion in the status line, if there is one.
   With an argument, rotate the completion ring that many times."
  (unless (eq (last-command-type) :completion-self-insert)
    (set-completion-prefix (window-buffer window))
    (setf (last-command-type) :completion-self-insert))
  (dotimes (i (or p 1))
    (rotate-completions *completion-prefix* *completion-prefix-length*))
  (display-possible-completion *completion-prefix* *completion-prefix-length*)
  (setf (last-command-type) :completion-self-insert))


;;;; Nifty database and parsing machanisms.



(defvar *completion-default-default-database-filename*
  "hemlock-completions.txt"
  "The file that will be defaultly written to and read from by \"Save
   Completions\" and \"Read Completions\".")

(defcommand "Save Completions" (window p)
  "Writes the current completion database to a file, defaultly the value of
   \"Completion Database Filename\".  With an argument, prompts for a
   filename."
  "Writes the current completion database to a file, defaultly the value of
   \"Completion Database Filename\".  With an argument, prompts for a
   filename."
  (let ((filename (or (and (not p) (value completion-database-filename))
                      (prompt-for-file
                       window
                       :must-exist nil
                       :default *completion-default-default-database-filename*
                       :prompt "File to write completions to: "))))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
      (message "Saving completions...")
      (dotimes (i (length *completions*))
        (let ((bucket (svref *completions* i)))
          (when bucket
            (write i :stream s :base 10 :radix 10)
            (write-char #\newline s)
            (dolist (completion bucket)
              (write-line completion s))
            (terpri s))))
      (message "Done."))))

(defcommand "Read Completions" (window p)
  "Reads some completions from a file, defaultly the value of \"Completion
   Database File\".  With an argument, prompts for a filename."
  "Reads some completions from a file, defaultly the value of \"Completion
   Database File\".  With an argument, prompts for a filename."
  (let ((filename (or (and (not p) (value completion-database-filename))
                      (prompt-for-file
                       window
                       :must-exist nil
                       :default *completion-default-default-database-filename*
                       :prompt "File to read completions from: ")))
        (index nil)
        (completion nil))
    (with-open-file (s filename :if-does-not-exist :error)
      (message "Reading in completions...")
      (loop
        (let ((new-completions '()))
          (unless (setf index (read-preserving-whitespace s nil nil))
            (return))
          ;; Zip past the newline that I know is directly after the number.
          ;; All this to avoid consing.  I love it.
          (read-char s)
          (loop
            (setf completion (read-line s))
            (when (string= completion "") (return))
            (unless (member completion (svref *completions* index))
              (push completion new-completions)))
          (let ((new-bucket (nconc (nreverse new-completions)
                                            (svref *completions* index))))
            (setf (svref *completions* index) new-bucket)
            (do ((completion new-bucket (cdr completion))
                 (end (1- (value completion-bucket-size)))
                 (i 0 (1+ i)))
                ((endp completion))
              (when (= i end) (setf (cdr completion) nil))))))
      (message "Done."))))

(defcommand "Parse Buffer for Completions" (window p)
  "Zips over a buffer slamming everything that is a valid completion word
   into the completion hashtable."
  "Zips over a buffer slamming everything that is a valid completion word
   into the completion hashtable."
  (declare (ignore p))
  (let ((buffer (prompt-for-buffer window :prompt "Buffer to parse: "
                                   :must-exist t
                                   :default (window-buffer window)
                                   :default-string (buffer-name
                                                    (window-buffer window)))))
    (with-mark ((word-start (buffer-start-mark buffer) :right-inserting)
                (word-end (buffer-start-mark buffer) :left-inserting)
                (buffer-end-mark (buffer-start-mark buffer)))
      (message "Starting parse of ~S..." (buffer-name buffer))
      (loop
        (unless (find-attribute buffer word-start :completion-wordchar) (return))
        (record-completion
         (region-to-string buffer (region word-start
                                   (or (find-attribute buffer
                                        (move-mark word-end word-start)
                                        :completion-wordchar #'zerop)
                                       buffer-end-mark))))
        (move-mark word-start word-end))
      (message "Done."))))



;;;; Modeline hackery:

(defvar *completion-mode-possibility* "")

(defvar *completion-modeline-field* (modeline-field :completion))

(defun display-possible-completion (prefix
                                    &optional (prefix-length (length prefix)))
  (let ((old *completion-mode-possibility*))
    (setq *completion-mode-possibility*
          (or (find-completion prefix prefix-length) ""))
    (unless (eq old *completion-mode-possibility*)
      (update-modeline-field *echo-area-buffer* *echo-area-window*
                             *completion-modeline-field*))))

(defun clear-completion-display ()
  (unless (= (length (the simple-string *completion-mode-possibility*)) 0)
    (setq *completion-mode-possibility* "")
    (update-modeline-field *echo-area-buffer* *echo-area-window*
                           *completion-modeline-field*)))


;;; COMPLETION-REDISPLAY-FUN erases any completion displayed in the status line.
;;;
(defun completion-redisplay-fun (window)
  (declare (ignore window))
  (unless (eq (last-command-type) :completion-self-insert)
    (clear-completion-display)))
;;;
(add-hook redisplay-hook #'completion-redisplay-fun)


;;;; The following is an attempt at Lisp symbol completion..
;;;;
;;;; Can this be merged with the rest of the file? 
;;;;

;;;; Fixme: when there are several completions, a pop up pops up only if the
;;;; user re-does the command with a prefix argument.  In other emacsen
;;;; it's enough to just re-type TAB.

(defun starts-with-p (string prefix)
  (let ((mismatch (mismatch string prefix)))
    (or (null mismatch) (= mismatch (length prefix)))))

(defun %find-symbol-completion-matches (packname symname)
  (let ((package (or (and packname (find-package (canonical-case packname)))
                     :cl)))
    (iter:iter
     (iter:for c in-package package)
     (let ((str (string-downcase c)))
       (when (starts-with-p str symname)
         (iter:collect str))))))

(defun %find-symbol-completion/request
    (current-window show-matches-p prefix packname symname)
  (let ((matches (%find-symbol-completion-matches packname symname)))
    (%find-symbol-completion/results
     current-window
     show-matches-p prefix matches)))

(defun %find-symbol-completion/results (current-window show-matches-p prefix matches)
  (let ((completion
         (cond
          ((cdr matches)
           (let ((match (car matches)))
             (dolist (next (cdr matches))
               (setf match (subseq match 0 (mismatch match next))))
             (if (find match matches :test 'equal)
                 (message "~D other match~:P, use prefix to see"
                          (1- (length matches)))
                 (message "~D possible match~:P, use prefix to see"
                          (length matches)))
             (values (if prefix
                         (concatenate 'string prefix ":" match)
                         match)
                     matches)))
          (matches
           (if prefix
               (concatenate 'string prefix ":" (car matches))
               (car matches)))
          (t
           (message "no matches")
           nil))))
    (when completion
      (%insert-completion (window-buffer current-window) completion))
    (when (and show-matches-p matches)
      (split-window-command current-window nil)
      (make-completelist-buffer current-window (mapcar #'make-completelist-entry matches)))))

(defun %insert-completion (buffer completion)
  (let ((point (buffer-point buffer)))
    (if *last-completion-mark*
        (move-mark *last-completion-mark* point)
        (setq *last-completion-mark* (copy-mark point :temporary)))
    (let ((mark *last-completion-mark*))
      (reverse-find-attribute buffer mark :completion-wordchar #'zerop)
      (let* ((region (region mark point))
             (string (region-to-string buffer region)))
        (declare (simple-string string))
        (delete-region buffer region)
        (let* ((first (position-if #'alpha-char-p string))
               (next (if first (position-if #'alpha-char-p string
                                            :start (1+ first)))))
          ;; Often completions start with asterisks when hacking on Lisp
          ;; code, so we look for alphabetic characters.
          (insert-string buffer point
                         ;; Leave the cascading IF's alone.
                         ;; Writing this as a COND, using LOWER-CASE-P as
                         ;; the test is not equivalent to this code since
                         ;; numbers (and such) are nil for LOWER-CASE-P and
                         ;; UPPER-CASE-P.
                         (if (and first (upper-case-p (schar string first)))
                             (if (and next
                                      (upper-case-p (schar string next)))
                                 (string-upcase completion)
                                 (word-capitalize completion))
                             completion)))))))


(defun find-symbol-completion (current-window show-matches-p prefix)
  (multiple-value-bind (package-prefix symname)
                       (let ((p (position #\: prefix)))
                         (if p
                             (values (subseq prefix 0 p)
                                     (string-downcase (subseq prefix (1+ p))))
                             (values nil
                                     (string-downcase prefix))))
    (%find-symbol-completion/request
     current-window
     (and show-matches-p t)
     package-prefix
     (or package-prefix (package-at-point (window-buffer current-window) (buffer-point (window-buffer current-window))))
     symname)))

(defun complete-symbol (window p &optional show-matches-p)
  (declare (ignore p))
  (find-symbol-completion window show-matches-p (symbol-string-at-point (window-buffer window))))

(defun editor-find-symbol-completion (current-window show-matches-p prefix)
  (multiple-value-bind (package-prefix symname)
                       (let ((p (position #\: prefix)))
                         (if p
                             (values (subseq prefix 0 p)
                                     (string-downcase (subseq prefix (1+ p))))
                             (values nil
                                     (string-downcase prefix))))
    (let* ((package (or package-prefix (package-at-point (window-buffer current-window) (buffer-point (window-buffer current-window)))))
           (matches (%find-symbol-completion-matches package symname)))
      (%find-symbol-completion/results
       current-window
       (and show-matches-p t)
       package-prefix matches))))

(defun editor-complete-symbol (current-window &optional show-matches-p)
  (editor-find-symbol-completion current-window show-matches-p (symbol-string-at-point)))



(defcommand "Complete For Mode" (window p)
  "" ""
  (funcall (value completion-function) window p)
  (krma.gui::redraw-buffer window))

(defcommand "Indent Or Complete" (window p)
  """"
  (if (let* ((buffer (window-buffer window))
             (mark (buffer-point buffer)))
        (or (zerop (mark-charpos mark))
            ;; why doesn't this work?
            ;; (test-char (next-character mark) :lisp-syntax :whitespace)
            (find (previous-character buffer mark) '(#\space #\tab #\newline))))
      (indent-command window p)
      (complete-for-mode-command window p))
  (krma.gui::redraw-buffer window))

;;; Mode

(defvar *completelist-entries* nil)
(defvar *completelist-entries-end*)
;;;

(defun make-completelist-entry (str)
  (internal-make-fuzzylist-entry str nil nil "????????"))

;;; This is the completelist buffer if it exists.
;;;
(defvar *completelist-buffer* nil)

;;; This is the cleanup method for deleting *completelist-buffer*.
;;;
(defun delete-completelist-buffers (buffer)
  (when (eq buffer *completelist-buffer*)
    (setf *completelist-buffer* nil)
    (setf *completelist-entries* nil)))


;;;; Commands.



(defcommand "Completelist Quit" (window p)
  "Kill the completelist buffer."
  ""
  (declare (ignore p))
  (when *completelist-buffer*
        (delete-buffer-if-possible *completelist-buffer* (window-buffer window))))

(defun completelist-entry-from-mark (buffer mark)
  (array-element-from-mark buffer (buffer-point buffer) *completelist-entries*))

(defcommand "Completelist Find Definition" (window p)
  "" ""
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (let ((entry (completelist-entry-from-mark buffer (buffer-point buffer))))
      (when entry
        (change-to-definition entry)))))

(defcommand "Completelist Pick" (window p)
  "" ""
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (let ((entry (completelist-entry-from-mark buffer (buffer-point buffer))))
      (when entry
        (completelist-quit-command window nil)
        (delete-window-command window nil)
        (%insert-completion buffer (fuzz-completed-string entry))))))

(defun refresh-completelist (buf entries)
  (with-writable-buffer (buf)
    (delete-region buf (buffer-region buf))
    (setf *completelist-entries-end* (length entries))
    (setf *completelist-entries* (coerce entries 'vector))
    (with-output-to-mark (s (buffer-point buf))
      (dolist (entry entries)
        (completelist-write-line entry s)))))

(defun make-completelist-buffer (current-window entries)
  (let ((buf (or *completelist-buffer*
                 (make-buffer "*Completelist*" :modes '("Completelist")))))
    (setf *completelist-buffer* buf)
    (refresh-completelist buf entries)
    (let ((fields (buffer-modeline-fields *completelist-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :completelist-cmds)
                      (make-modeline-field
                       :name :completelist-cmds :width 18
                       :function
                       #'(lambda (buffer window)
                           (declare (ignore buffer window))
                           "  Type ? for help.")))))
      (setf (buffer-modeline-fields *completelist-buffer*) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf current-window)))

(defun completelist-write-line (entry s)
  (format s "~A ~40T~A~%"
          (fuzz-completed-string entry)
          ;; (fuzz-score entry)
          ;; (fuzz-chunks entry)
          (fuzz-classification-string entry)))

(defcommand "Completelist Help" (window p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Completelist"))
