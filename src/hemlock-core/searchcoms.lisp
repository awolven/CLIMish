;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains searching and replacing commands.
;;;

(in-package :hemlock)



;;;; Some global state.

(defvar *last-search-string* () "Last string searched for.")
(defvar *last-search-pattern*
  (new-search-pattern :string-insensitive :forward "Foo")
  "Search pattern we keep around so we don't cons them all the time.")



(defun get-search-pattern (buffer string direction)
  (declare (simple-string string))
  (when (zerop (length string)) (editor-error))
  (setq *last-search-string* string)
  (setq *last-search-pattern*
        (new-search-pattern (if (value buffer string-search-ignore-case)
                                :string-insensitive
                                :string-sensitive)
                            direction string *last-search-pattern*)))



;;;; Vanilla searching.

(defun vanilla-search (current-window string direction)
  (unless string
    (setq string (clim:accept 'string
                              :current-window current-window
                              :prompt (ecase direction
                                        (:forward "Search: ")
                                        (:backward "Reverse Search: "))
                              :default *last-search-string*
                              :help "String to search for")))
  (let* ((buffer (window-buffer current-window))
         (pattern (get-search-pattern buffer string direction))
         (point (buffer-point buffer))
         (mark (copy-mark point))
         (won (find-pattern buffer point pattern)))
    (cond (won
           (character-offset buffer point won)
           (if (region-active-p buffer)
               (delete-mark mark)
               (push-buffer-mark buffer mark)))
          (t (delete-mark mark)
             (editor-error)))))

(defhcommand forward-search-command (window p &optional string)
  (:pretty-name "Forward Search")
  (:documentation "Do a forward search for a string.
  Prompt for the string and leave the point after where it is found.")
  (:help-string "Searches for the specified String in the current buffer."))

(defmethod forward-search-command (window p &optional string)
  (declare (ignore p))
  (vanilla-search window string :forward))

(defhcommand reverse-search-command (window p &optional string)
  (:pretty-name "Reverse Search")
  (:documentation "Do a backward search for a string.
  Prompt for the string and leave the point before where it is found.")
  (:help-string "Searches backwards for the specified String in the current buffer."))

(defmethod reverse-search-command (window p &optional string)
  (declare (ignore p))
  (vanilla-search window string :backward))



;;;; Incremental searching.

(defun i-search-pattern (buffer string direction)
  (setq *last-search-pattern*
        (new-search-pattern (if (value buffer string-search-ignore-case)
                                :string-insensitive
                                :string-sensitive)
                            direction string *last-search-pattern*)))

;;;      %I-SEARCH-ECHO-REFRESH refreshes the echo buffer for incremental
;;; search.
;;;
(defun %i-search-echo-refresh (string direction failure)
  (when (interactive)
    (clear-echo-area)
    (format *echo-area-stream*
            "~@[~A ~]~:[Reverse I-Search~;I-Search~]: ~A"
            failure (eq direction :forward) string)
    (finish-output *echo-area-stream*)))

(defun i-search (current-window buffer direction)
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" direction nil)
  (let* ((point (buffer-point buffer))
         (save-start (copy-mark point :temporary)))
    (with-mark ((here point))
      (when (eq (catch 'exit-i-search
                  (%i-search current-window buffer "" point here direction nil))
                :control-g)
        (move-mark point save-start)
        (climish::redraw-buffer current-window)
        (invoke-hook buffer abort-hook)
        (editor-error))
      (if (region-active-p buffer)
          (delete-mark save-start)
          (push-buffer-mark buffer save-start)))))

(defhcommand incremental-search-command (window p)
  (:pretty-name "Incremental Search")
  (:documentation "Searches for input string as characters are provided.
  These are the default I-Search command characters:  ^Q quotes the
  next character typed.  Backspace cancels the last character typed.  ^S
  repeats forward, and ^R repeats backward.  ^R or ^S with empty string
  either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with
  an empty search string calls the non-incremental search command.
  Other control characters cause exit and execution of the appropriate
  command.  If the search fails at some point, ^G and backspace may be
  used to backup to a non-failing point; also, ^S and ^R may be used to
  look the other way.  ^G during a successful search aborts and returns
  point to where it started.")
  (:help-string "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."))

(defmethod incremental-search-command (window p)
  (declare (ignore p))
  (i-search window (window-buffer window) :forward))


(defhcommand reverse-incremental-search-command (window p)
  (:pretty-name "Reverse Incremental Search")
  (:documentation
   "Searches for input string as characters are provided.
  These are the default I-Search command characters:  ^Q quotes the
  next character typed.  Backspace cancels the last character typed.  ^S
  repeats forward, and ^R repeats backward.  ^R or ^S with empty string
  either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with
  an empty search string calls the non-incremental search command.
  Other control characters cause exit and execution of the appropriate
  command.  If the search fails at some point, ^G and backspace may be
  used to backup to a non-failing point; also, ^S and ^R may be used to
  look the other way.  ^G during a successful search aborts and returns
  point to where it started.")
  (:help-string
   "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."))

(defmethod reverse-incremental-search-command (window p)
  (declare (ignore p))
  (i-search window (window-buffer window) :backward))

;;;      %I-SEARCH recursively (with support functions) searches to provide
;;; incremental searching.  There is a loop in case the recursion is ever
;;; unwound to some call.  curr-point must be saved since point is clobbered
;;; with each recursive call, and the point must be moved back before a
;;; different letter may be typed at a given call.  In the CASE at :cancel
;;; and :control-g, if the string is not null, an accurate pattern for this
;;; call must be provided when %I-SEARCH-CHAR-EVAL is called a second time
;;; since it is possible for ^S or ^R to be typed.
;;;
(defun %i-search (current-window buffer string point trailer direction failure)
  (do* ((curr-point (copy-mark point :temporary))
        (curr-trailer (copy-mark trailer :temporary)))
       (nil)
    (let ((peek-event (my-listen *editor-input*)))
      (case (%i-search-char-eval current-window
                                 buffer
                                 (climish::input-event-id peek-event) string point trailer
                                 direction failure)
        (:cancel
         (%i-search-echo-refresh string direction failure)
         (unless (zerop (length string))
           (i-search-pattern buffer string direction)))
        (:return-cancel
         (unless (zerop (length string)) (return :cancel))
         (beep))
        (:control-g
         (when failure (return :control-g))
         (%i-search-echo-refresh string direction nil)
         (unless (zerop (length string))
           (i-search-pattern buffer string direction))))
      (move-mark point curr-point)
      (move-mark trailer curr-trailer))))


(defun my-listen (input)
  (let ((event nil))
    (loop
      until (setq event
                  (climish::peek-next-event input))
      do (sleep 0.001)
      finally (return event))))

(defun next-event-character/id (input)
  (climish::input-event-id
   (climish::read-event input)))

(defun discard-next-event (input)
  (climish::read-event input))

(defun maybe-discard-char-event (input)
  (sleep 0.01) ;; race condition problem.
  (let ((peeked-event (climish::peek-next-event input)))
    (when (typep peeked-event 'clui::character-event-mixin)
      (discard-next-event input))))
  
(defun %i-search-test-peeked-event
    (current-window buffer peeked-event string point trailer direction failure)
  (cond ((or (logical-key-event-p peeked-event :forward-search)
             (logical-key-event-p peeked-event :backward-search))
         (%i-search-control-s-or-r current-window buffer
                                   (next-event-character/id *editor-input*)
                                   string point trailer
                                   direction failure))
        
        ((logical-key-event-p peeked-event :cancel)
         (discard-next-event *editor-input*) ;; discard this key (backspace or delete)
         :return-cancel)
        
        ((logical-key-event-p peeked-event :abort)
         (discard-next-event *editor-input*) ;; discard this key (ctrl-g)
         (unless failure
           (clear-echo-area)
           (message "Search aborted.")
           (throw 'exit-i-search :control-g))
         :control-g)
        
        ((logical-key-event-p peeked-event :quote)
         (discard-next-event *editor-input*) ;; discard this key (ctrl-q)
         (%i-search-printed-char current-window buffer
                                 (next-event-character/id *editor-input*)
                                 string point trailer
                                 direction failure))
        
        ((and (zerop (length string)) (logical-key-event-p peeked-event :exit))
         (discard-next-event *editor-input*)
         (if (eq direction :forward)
             (forward-search-command current-window nil)
             (reverse-search-command current-window nil))
         (throw 'exit-i-search nil))
        
        ((logical-key-event-p peeked-event :exit)
         (discard-next-event *editor-input*)
         (unless (zerop (length string))
           (setf *last-search-string* string))
         (throw 'exit-i-search nil))

        (t nil)))

;;;      %I-SEARCH-CHAR-EVAL evaluates the last character typed and takes
;;; necessary actions.
;;;
(defun %i-search-char-eval (current-window buffer peeked-event string point trailer direction failure)
  (declare (simple-string string))
  (let ((test (%i-search-test-peeked-event current-window buffer peeked-event string point trailer direction failure)))
    (or test
        (cond ((progn (discard-next-event *editor-input*)
                      (let* ((next-event (my-listen *editor-input*))
                             (character (clui::input-event-character next-event)))
                        character))
               (%i-search-printed-char current-window buffer
                                       peeked-event
                                       string point trailer
                                       direction failure))

              (t (unless (zerop (length string))
                   (setf *last-search-string* string))
                 (throw 'exit-i-search nil))))))
        

;;;      %I-SEARCH-CONTROL-S-OR-R handles repetitions in the search.  Note
;;; that there cannot be failure in the last COND branch: since the direction
;;; has just been changed, there cannot be a failure before trying a new
;;; direction.
;;;
(defun %i-search-control-s-or-r (current-window buffer key-event string point trailer
                                           direction failure)
  (let ((forward-direction-p (eq direction :forward))
        (forward-character-p (logical-key-event-p key-event :forward-search)))
    (cond ((zerop (length string))
           (%i-search-empty-string current-window buffer
                                   point trailer direction forward-direction-p
                                   forward-character-p))
          ((eq forward-direction-p forward-character-p)
           (cond (failure
                  ;; Wrap around
                  (let ((backup (copy-mark point)))
                    (ecase direction
                      (:forward (buffer-start point buffer))
                      (:backward (buffer-end point buffer)))
                    (move-mark trailer point)
                    (%i-search-echo-refresh string direction "Wrapped")
                    (%i-search-find-pattern current-window buffer
                                            string point trailer
                                            direction backup)))
                 (t
                  (%i-search-find-pattern current-window buffer
                                          string point (move-mark trailer point)
                                          direction))))
          (t
           (let ((new-direction (if forward-character-p :forward :backward)))
             (%i-search-echo-refresh string new-direction nil)
             (i-search-pattern buffer string new-direction)
             (%i-search-find-pattern current-window buffer string point (move-mark trailer point)
                                     new-direction))))))


;;;      %I-SEARCH-EMPTY-STRING handles the empty string case when a ^S
;;; or ^R is typed.  If the direction and character typed do not agree,
;;; then merely switch directions.  If there was a previous string, search
;;; for it, else flash at the guy.
;;;
(defun %i-search-empty-string (current-window buffer point trailer direction forward-direction-p
                                     forward-character-p)
  (cond ((eq forward-direction-p (not forward-character-p))
         (let ((direction (if forward-character-p :forward :backward)))
           (%i-search-echo-refresh "" direction nil)
           (%i-search current-window buffer "" point trailer direction nil)))
        (*last-search-string*
         (%i-search-echo-refresh *last-search-string* direction nil)
         (i-search-pattern buffer *last-search-string* direction)
         (%i-search-find-pattern current-window buffer *last-search-string* point trailer direction))
        (t (beep))))


;;;      %I-SEARCH-PRINTED-CHAR handles the case of standard character input.
;;; If the direction is backwards, we have to be careful not to MARK-AFTER
;;; the end of the buffer or to include the next character at the beginning
;;; of the search.
;;;
(defun %i-search-printed-char (current-window buffer key-event string point trailer direction failure)
  (declare (ignore key-event))
  ;; a key-event got us here, we are expecting a character next
  ;; should come almost right away, but if not and we get here, another keystroke will be necessary
  (let* ((event (my-listen *editor-input*))
         (tchar (clui::input-event-character event)))
    (if (null tchar)
        (unless (%i-search-test-peeked-event current-window buffer (climish::input-event-id event)
                                             string point trailer direction failure)
          (editor-error "Not a text character -- ~S"
                        (next-event-character/id *editor-input*)))
        (progn
          (discard-next-event *editor-input*)
          #+NIL
          (when (interactive)
            (let ((cr (current-region buffer nil nil)))
              (if (string= string "")
                  (progn (push-buffer-mark buffer (copy-mark point) t)
                         (move-mark (region-end cr) point)
                         (character-offset (region-end cr) 1)))))

          (when (interactive)
            (insert-character *echo-area-buffer* (buffer-point *echo-area-buffer*) tchar)
            (force-output *echo-area-stream*))
          (let ((new-string (concatenate 'simple-string string (string tchar))))
            (i-search-pattern buffer new-string direction)
            (cond (failure (%i-search current-window buffer new-string point trailer direction failure))
                  ((and (eq direction :backward) (next-character buffer trailer))
                   (%i-search-find-pattern current-window buffer new-string point (mark-after buffer trailer)
                                           direction))
                  (t
                   (%i-search-find-pattern current-window buffer new-string point trailer direction))))))))


;;;      %I-SEARCH-FIND-PATTERN takes a pattern for a string and direction
;;; and finds it, updating necessary pointers for the next call to %I-SEARCH.
;;; If the search failed, tell the user and do not move any pointers.
;;;
(defun %i-search-find-pattern (current-window buffer string point trailer direction &optional backup)
  (let ((found-offset (find-pattern buffer trailer *last-search-pattern*)))
    (cond (found-offset
           (cond ((eq direction :forward)
                  (character-offset buffer (move-mark point trailer) found-offset))
                 (t
                  (move-mark point trailer)
                  (character-offset buffer trailer found-offset)))

           (push-buffer-mark buffer trailer t)

           (if (hi::maybe-recenter-window-on-mark current-window point)
               (climish::redraw-buffer current-window)
               (hi::update-window-image current-window))

           (%i-search current-window buffer string point trailer direction nil))
          (t
           (when backup
             (move-mark point backup)
             (move-mark trailer backup)
             (if (hi::maybe-recenter-window-on-mark current-window point)
                 (climish::redraw-buffer current-window)
                 (hi::update-window-image current-window)))
           (%i-search-echo-refresh string direction "Failing")
           (if (interactive)
               (beep)
               (editor-error "I-Search failed."))
           (%i-search current-window buffer string point trailer direction "Failing")))))



;;;; Replacement commands:

(defhcommand replace-string-command (window p &optional target replacement)

  (:pretty-name "Replace String")
  (:help-string "Replaces the specified Target string with the specified Replacement
   string in the current buffer for all occurrences after the point or within
   the active region, depending on whether it is active.")
  (:documentation
   "Replaces the specified Target string with the specified Replacement
   string in the current buffer for all occurrences after the point or within
   the active region, depending on whether it is active.  The prefix argument
   may limit the number of replacements."))

(defmethod replace-string-command (window p &optional (target
                                                       (clim:accept 'string
                                                                    :current-window window
                                                                    :prompt "Replace String: "
                                                                    :help "Target string"
                                                                    :default *last-search-string*))
                                      (replacement (clim:accept 'string
                                                                :current-window window
                                                                :prompt "With: "
                                                                :help "Replacement string")))
  (multiple-value-bind (ignore count)
                       (query-replace-function window p target replacement
                                               "Replace String" t)
    (declare (ignore ignore))
    (message "~D Occurrences replaced." count)))

(defhcommand query-replace-command (window p &optional target replacement)
  (:pretty-name "Query Replace")
  (:help-string 
   "Replaces the Target string with the Replacement string if confirmation
   from the keyboard is given.  If the region is active, limit queries to
   occurrences that occur within it, otherwise use point to end of buffer.")
  (:documentation
   "Replaces the Target string with the Replacement string if confirmation
   from the keyboard is given.  If the region is active, limit queries to
   occurrences that occur within it, otherwise use point to end of buffer.
   A prefix argument may limit the number of queries."))

(defmethod query-replace-command (window p &optional
                                             (target (clim:accept 'string
                                                                  :current-window window
                                                                  :prompt "Query Replace: "
                                                                  :help "Target string"
                                                                  :default *last-search-string*))
                                             (replacement (clim:accept 'string
                                                                       :current-window window
                                                                       :prompt "With: "
                                                                       :help "Replacement string")))
  (let ((buffer (window-buffer window)))
    (let ((mark (copy-mark (buffer-point buffer))))
      (multiple-value-bind (ignore count)
          (query-replace-function window p target replacement
                                  "Query Replace")
        (declare (ignore ignore))
        (message "~D Occurrences replaced." count))
      (push-buffer-mark buffer mark))))




(defstruct (replace-undo (:constructor make-replace-undo (mark region)))
  mark
  region)

(setf (documentation 'replace-undo-mark 'function)
      "Return the mark where a replacement was made.")
(setf (documentation 'replace-undo-region 'function)
      "Return region deleted due to replacement.")

(defvar *query-replace-undo-data* nil)

;;; REPLACE-THAT-CASE replaces a string case-sensitively.  Lower, Cap and Upper
;;; are the original, capitalized and uppercase replacement strings.  Mark is a
;;; :left-inserting mark after the text to be replaced.  Length is the length
;;; of the target string.  If dumb, then do a simple replace.  This pushes
;;; an undo information structure into *query-replace-undo-data* which
;;; QUERY-REPLACE-FUNCTION uses.
;;;
(defun replace-that-case (buffer lower cap upper mark length dumb)
  (character-offset buffer mark (- length))
  (let ((insert (cond (dumb lower)
                      ((upper-case-p (next-character buffer mark))
                       (mark-after buffer mark)
                       (prog1 (if (upper-case-p (next-character buffer mark)) upper cap)
                              (mark-before buffer mark)))
                      (t lower))))
    (with-mark ((undo-mark1 mark :left-inserting)
                (undo-mark2 mark :left-inserting))
      (character-offset buffer undo-mark2 length)
      (push (make-replace-undo
             ;; Save :right-inserting, so the INSERT-STRING at mark below
             ;; doesn't move the copied mark the past replacement.
             (copy-mark mark :right-inserting)
             (delete-and-save-region buffer (region undo-mark1 undo-mark2)))
            *query-replace-undo-data*))
    (insert-string buffer mark insert)))

;;; QUERY-REPLACE-FUNCTION does the work for the main replacement commands:
;;; "Query Replace", "Replace String", "Group Query Replace", "Group Replace".
;;; Name is the name of the command for undoing purposes.  If doing-all? is
;;; true, this replaces all ocurrences for the non-querying commands.  This
;;; returns t if it completes successfully, and nil if it is aborted.  As a
;;; second value, it returns the number of replacements.
;;;
;;; The undo method, before undo'ing anything, makes all marks :left-inserting.
;;; There's a problem when two replacements are immediately adjacent, such as
;;;    foofoo
;;; replacing "foo" with "bar".  If the marks were still :right-inserting as
;;; REPLACE-THAT-CASE makes them, then undo'ing the first replacement would
;;; bring the two marks together due to the DELETE-CHARACTERS.  Then inserting
;;; the region would move the second replacement's mark to be before the first
;;; replacement.
;;;
(defun query-replace-function (current-window count target replacement name
                               &optional (doing-all? nil))
  (declare (simple-string replacement))
  (let ((buffer (window-buffer current-window)))
  (let ((replacement-len (length replacement))
        (*query-replace-undo-data* nil))
    (when (and count (minusp count))
      (editor-error "Replacement count is negative."))
    (get-search-pattern buffer target :forward)
    (unwind-protect
        (query-replace-loop current-window (get-count-region buffer) (or count -1) target replacement
                            replacement-len (buffer-point buffer) doing-all?)
      (let ((undo-data (nreverse *query-replace-undo-data*)))
        (save-for-undo name
          #'(lambda ()
              (dolist (ele undo-data)
                (setf (mark-kind (replace-undo-mark ele)) :left-inserting))
              (dolist (ele undo-data)
                (let ((mark (replace-undo-mark ele)))
                  (delete-characters buffer mark replacement-len)
                  (ninsert-region buffer mark (replace-undo-region ele)))))
          #'(lambda ()
              (dolist (ele undo-data)
                (delete-mark (replace-undo-mark ele))))))))))

;;; QUERY-REPLACE-LOOP is the essence of QUERY-REPLACE-FUNCTION.  The first
;;; value is whether we completed all replacements, nil if we aborted.  The
;;; second value is how many replacements occurred.
;;;
(defun query-replace-loop (current-window region count target replacement replacement-len
                           point doing-all?)
  (let ((buffer (window-buffer current-window)))
    (with-mark ((last-found point)
                ;; Copy REGION-END before moving point to REGION-START in case
                ;; the end is point.  Also, make it permanent in case we make
                ;; replacements on the last line containing the end.
                (stop-mark (region-end region) :left-inserting))
      (move-mark point (region-start region))
      (let ((length (length target))
            (cap (string-capitalize replacement))
            (upper (string-upcase replacement))
            (dumb (not (and (every #'(lambda (ch) (or (not (both-case-p ch))
                                                      (lower-case-p ch)))
                                   (the string replacement))
                            (value buffer case-replace)))))
        (values
         (loop
           (let ((won (find-pattern buffer point *last-search-pattern*)))
             (when (or (null won) (zerop count) (mark> point stop-mark))
               (character-offset buffer (move-mark point last-found) replacement-len)
               (return t))
             (decf count)
             (move-mark last-found point)
             (character-offset buffer point length)
             (if doing-all?
                 (replace-that-case buffer replacement cap upper point length dumb)
                 (command-case
                     (:prompt
                      "Query replace: "
                      :help "Type one of the following single-character commands:"
                      :change-window nil :bind key-event
                      :current-window current-window)
                   (:yes "Replace this occurrence."
                         (replace-that-case buffer replacement cap upper point length
                                            dumb))
                   (:no "Don't replace this occurrence, but continue.")
                   (:do-all "Replace this and all remaining occurrences."
                     (replace-that-case buffer replacement cap upper point length
                                        dumb)
                     (setq doing-all? t))
                   (:do-once "Replace this occurrence, then exit."
                     (replace-that-case buffer replacement cap upper point length
                                        dumb)
                     (return nil))
                   (:recursive-edit
                    "Go into a recursive edit at the current position."
                    (do-recursive-edit)
                    (get-search-pattern buffer target :forward))
                   (:exit "Exit immediately."
                          (return nil))
                   (t (unget-key-event key-event *editor-input*)
                      (return nil))))))
         (length (the list *query-replace-undo-data*)))))))



;;;; Occurrence searching.

(defhcommand list-matching-lines-command (window p &optional string)
  (:pretty-name "List Matching Lines")
  (:documentation "Prompts for a search string and lists all matching lines after the point or
   within the current-region, depending on whether it is active or not.
   With an argument, lists p lines before and after each matching line.")
  (:help-string
   "Prompts for a search string and lists all matching lines after the point or
   within the current-region, depending on whether it is active or not.
   With an argument, lists p lines before and after each matching line."))

(defmethod list-matching-lines-command (window p &optional string)
  (let ((buffer (window-buffer window)))
    (unless string
      (setf string (clim:accept 'string
                                :current-window window
                                :prompt "List Matching: "
                                :default *last-search-string*
                                :help "String to search for")))
    (let ((pattern (get-search-pattern buffer string :forward))
          (matching-lines nil)
          (region (get-count-region buffer)))
      (with-mark ((mark (region-start region))
                  (end-mark (region-end region)))
        (loop
          (when (or (null (find-pattern buffer mark pattern)) (mark> mark end-mark))
            (return))
          (setf matching-lines
                (nconc matching-lines (list-lines buffer mark (or p 0))))
          (unless (line-offset mark 1 0)
            (return))))
      (with-pop-up-display (s :height (length matching-lines) :current-window window)
        (dolist (line matching-lines)
          (write-line line s))))))

;;; LIST-LINES creates a lists of strings containing (num) lines before the
;;; line that the point is on, the line that the point is on, and (num)
;;; lines after the line that the point is on. If (num) > 0, a string of
;;; dashes will be added to make life easier for List Matching Lines.
;;;
(defun list-lines (buffer mark num)
  (if (<= num 0)
      (list (line-string buffer (mark-line mark)))
      (with-mark ((mark mark)
                  (beg-mark mark))
        (unless (line-offset buffer beg-mark (- num))
          (buffer-start beg-mark buffer))
        (unless (line-offset buffer mark num)
          (buffer-end mark buffer))
        (let ((lines (list "--------")))
          (loop
            (push (line-string buffer (mark-line mark)) lines)
            (when (same-line-p mark beg-mark)
              (return lines))
            (line-offset buffer mark -1))))))

(defhcommand delete-matching-lines-command (window p &optional string)
  (:pretty-name "Delete Matching Lines")
  (:documentation
   "Deletes all lines that match the search pattern using delete-region. If
   the current region is active, limit the search to it. The argument is
   ignored.")
  (:help-string
   "Deletes all lines that match the search pattern using delete-region. If
   the current region is active, limit the search to it. The argument is
   ignored."))

(defmethod delete-matching-lines-command (window p &optional string)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (unless string
      (setf string (clim:accept 'string
                                :current-window window
                                :prompt "Delete Matching: "
                                :default *last-search-string*
                                :help "String to search for")))
    (let* ((region (get-count-region buffer))
           (pattern (get-search-pattern buffer string :forward))
           (start-mark (region-start region))
           (end-mark (region-end region)))
      (with-mark ((bol-mark start-mark :left-inserting)
                  (eol-mark start-mark :right-inserting))
        (loop
          (unless (and (find-pattern buffer bol-mark pattern) (mark< bol-mark end-mark))
            (return))
          (move-mark eol-mark bol-mark)
          (line-start bol-mark)
          (unless (line-offset eol-mark 1 0)
            (buffer-end eol-mark buffer))
          (delete-region buffer (region bol-mark eol-mark)))))))

(defhcommand delete-non-matching-lines-command (window p &optional string)
  (:pretty-name "Delete Non-Matching Lines")
  (:documentation
   "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored.")
  (:help-string
   "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."))

(defmethod delete-non-matching-lines-command (window p &optional string)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (unless string
      (setf string (clim:accept 'string
                                :current-window window
                                :prompt "Delete Non-Matching:"
                                :default *last-search-string*
                                :help "String to search for")))
    (let* ((region (get-count-region buffer))
           (start-mark (region-start region))
           (stop-mark (region-end region))
           (pattern (get-search-pattern buffer string :forward)))
      (with-mark ((beg-mark start-mark :left-inserting)
                  (end-mark start-mark :right-inserting))
        (loop
          (move-mark end-mark beg-mark)
          (cond ((and (find-pattern buffer end-mark pattern) (mark< end-mark stop-mark))
                 (line-start end-mark)
                 (delete-region buffer (region beg-mark end-mark))
                 (unless (line-offset beg-mark 1 0)
                   (return)))
                (t
                 (delete-region buffer (region beg-mark stop-mark))
                 (return))))))))

(defhcommand count-occurrances-command (window p &optional string)
  (:pretty-name "Count Occurrences")
  (:documentation
   "Prompts for a search string and counts occurrences of it after the point or
   within the current-region, depending on whether it is active or not. The
   argument is ignored.")
  (:help-string
   "Prompts for a search string and counts occurrences of it after the point or
   within the current-region, depending on whether it is active or not. The
   argument is ignored."))

(defmethod count-occurrances-command (window p &optional string)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (unless string
      (setf string (clim:accept 'string
                                :current-window window
                                :prompt "Count Occurrences: "
                                :default *last-search-string*
                                :help "String to search for")))
    (message "~D occurrence~:P"
             (count-occurrences-region buffer (get-count-region buffer) string))))

(defun count-occurrences-region (buffer region string)
  (let ((pattern (get-search-pattern buffer string :forward))
        (end-mark (region-end region)))
    (let ((occurrences 0))
      (with-mark ((mark (region-start region)))
        (loop
          (let ((won (find-pattern buffer mark pattern)))
            (when (or (null won) (mark> mark end-mark))
              (return))
            (incf occurrences)
            (character-offset buffer mark won))))
      occurrences)))
