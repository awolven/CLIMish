;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;
;;; This file contains the implementation of Auto Fill Mode.  Also,
;;;   paragraph and region filling stuff is here.
;;;

(in-package :hemlock)


;;; Fill Mode should be defined with some transparent bindings (linefeed and
;;; return) but with some that are not (space), so until this is possible, we
;;; kludge this effect by altering Auto Fill Linefeed and Auto Fill Return.



;;;; -- Commands --

(defhcommand auto-fill-mode-command (window p)
  (:pretty-name "Auto Fill Mode")
  (:documentation
   "Breaks lines between words at the right margin.
   A positive argument turns Fill mode on, while zero or a negative
   argument turns it off.  With no arguments, it is toggled.  When space
   is typed, text that extends past the right margin is put on the next
   line.  The right column is controlled by Fill Column.")
  (:help-string
   "Determine if in Fill mode or not and set the mode accordingly."))

(defmethod auto-fill-mode-command (window p)
  (let* ((buffer (window-buffer window)))
    (setf (buffer-minor-mode buffer "Fill" buffer)
          (if p
              (plusp p)
              (not (buffer-minor-mode buffer "Fill"))))))


;;; This command should not have a transparent binding since it sometimes does
;;; not insert a spaces, and transparency would propagate to "Self Insert".
(defhcommand auto-fill-space-command (window p)
  (:pretty-name "Auto Fill Space")
  (:documentation
   "Insert space and a CRLF if text extends past margin.
   If arg is 0, then may break line but will not insert the space.
   If arg is positive, then inserts that many spaces without filling.")
  (:help-string
   "Insert space and CRLF if text extends past margin.
   If arg is 0, then may break line but will not insert the space.
   If arg is positive, then inserts that many spaces without filling."))

(defmethod auto-fill-space-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (check-fill-prefix buffer (value buffer fill-prefix) (value buffer fill-column) point)
    (cond ((and p (plusp p))
           (dotimes (x p) (insert-character buffer point #\space)))
          ((and p (zerop p)) (%auto-fill-space window buffer point nil))
          (t (%auto-fill-space window buffer point t)))))


(defhcommand auto-fill-linefeed-command (window p)
  (:pretty-name "Auto Fill Linefeed")
  (:documentation "Does an immediate CRLF inserting Fill Prefix if it exists.")
  (:help-string "Does an immediate CRLF inserting Fill Prefix if it exists."))

(defmethod auto-fill-linefeed-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (check-fill-prefix buffer (value buffer fill-prefix) (value buffer fill-column) point)
    (%auto-fill-space window buffer point nil)
    ;; The remainder of this function should go away when
    ;; transparent key bindings are per binding instead of
    ;; per mode.
    #+NOTYET
    (multiple-value-bind (command t-bindings)
        
                         (get-command #k"Linefeed" :current)
      (declare (ignore command)) ;command is this one, so don't invoke it
      (dolist (c t-bindings) (funcall *invoke-hook* c p)))
    #+NOTYET
    (indent-new-line-command window nil)))



(defhcommand auto-fill-return-command (window p)
  (:pretty-name "Auto Fill Return")
  (:documentation "Does an Auto Fill Space with a prefix argument of 0
   followed by a newline.")
  (:help-string "Does an Auto Fill Space with a prefix argument of 0
   followed by a newline."))

(defmethod auto-fill-return-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (check-fill-prefix buffer (value buffer fill-prefix) (value buffer fill-column) point)
    (%auto-fill-space window buffer point nil)
    ;; The remainder of this function should go away when
    ;; transparent key bindings are per binding instead of
    ;; per mode.
    #+NOTYET
    (multiple-value-bind (command t-bindings)
        (get-command #k"Return" :current)
      (declare (ignore command)) ;command is this one, so don't invoke it
      (dolist (c t-bindings) (funcall *invoke-hook* c p)))
    (new-line-command window nil)))



(defhcommand fill-paragraph-command (window p)
  (:pretty-name "Fill Paragraph")
  (:documentation
   "Fill this or next paragraph.
   Point stays fixed, but text may move past it due to filling.
   A paragraph is delimited by a blank line, a line beginning with a
   special character (@,\,-,',and .), or it is begun with a line with at
   least one whitespace character starting it.  Prefixes are ignored or
   skipped over before determining if a line starts or delimits a
   paragraph.")
  (:help-string
   "Fill this or next paragraph.
   Point stays fixed, but text may move past it due to filling."))

(defmethod fill-paragraph-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (prefix (value buffer fill-prefix))
         (prefix-len (length prefix))
         (column (if p (abs p) (value buffer fill-column))))
    (with-mark ((m point))
      (let ((paragraphp (paragraph-offset buffer m 1)))
        (unless (or paragraphp
                    (and (last-line-p m)
                         (end-line-p buffer m)
                         (not (blank-line-p buffer (mark-line m)))))
          (editor-error))
        ;;
        ;; start and end get deleted by the undo cleanup function
        (let ((start (copy-mark m :right-inserting))
              (end (copy-mark m :left-inserting)))
          (%fill-paragraph-start buffer start prefix prefix-len)
          (let* ((region (region start end))
                 (undo-region (copy-region buffer region)))
            (fill-region region prefix column)
            (make-region-undo buffer :twiddle "Fill Paragraph" region undo-region)))))))


(defhcommand fill-region-command (window p)
  (:pretty-name "Fill Region")
  (:documentation "Fill text from point to mark.")
  (:help-string "Fill text from point to mark."))

(defmethod fill-region-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (region (current-region buffer))
         (prefix (value buffer fill-prefix))
         (column (if p (abs p) (value buffer fill-column))))
    (check-fill-prefix buffer prefix column point)
    (check-region-query-size window region)
    (fill-region-by-paragraphs region prefix column)))



(defhcommand set-fill-column-command (window p)
  (:pretty-name "Set Fill Column")
  (:documentation "Set Fill Column to current column or argument.
   If argument is provided use its absolute value.")
  (:help-string "Set Fill Column to current column or argument.
   If argument is provided use its absolute value."))

(defmethod set-fill-column-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (new-column (or (and p (abs p))
                         (mark-column buffer point))))
    (defhvar "Fill Column" "This buffer's fill column"
      :value new-column  :buffer buffer)
    (message "Fill Column = ~D" new-column)))


(defhcommand set-fill-prefix-command (window p)
  (:pretty-name "Set Fill Prefix")
  (:documentation
   "Define Fill Prefix from the current line.
   All of the current line up to point is the prefix.  This may be
   turned off by placing point at the beginning of a line when setting.")
  (:help-string
   "Define Fill Prefix from the current line.
   All of the current line up to point is the prefix.  This may be
   turned off by placing point at the beginning of a line when setting."))

(defmethod set-fill-prefix-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (with-mark ((mark point))
      (line-start mark)
      (let ((val (if (mark/= mark point) (region-to-string buffer (region mark point)))))
        (defhvar "Fill Prefix" "This buffer's fill prefix"
          :value val  :buffer buffer)
        (message "Fill Prefix now ~:[empty~;~:*~S~]" val)))))

;; (declaim (optimize (speed 2))); turn off byte compilation.

;;;; -- Auto Filling --

;;;      %AUTO-FILL-SPACE takes a point and an argument indicating
;;; whether it should insert a space or not.  If point is past Fill
;;; Column then text is filled. Usually  the else clause of the if
;;; will be executed.  If the then clause is executed, then the first
;;; branch of the COND will usually be executed.  The first branch
;;; handles the case of the end of a word extending past Fill Column
;;; while the second handles whitespace preceded by non-whitespace
;;; extending past the Fill Column.  The last branch is for those who
;;; like to whitespace out a blank line.

(defun %auto-fill-space (current-window buffer point insertp)
  "Insert space, but CRLF if text extends past margin.
   If arg is 0, then may break line but will not insert the space.
   If arg is positive, then inserts that many spaces without filling."
  (if (> (mark-column buffer point) (value buffer fill-column))
      (with-mark ((mark1 point :left-inserting))
        (let ((not-all-blank (reverse-find-attribute buffer mark1 :whitespace #'zerop))
              (prefix (value buffer fill-prefix))
              (column (value buffer fill-column)))
          (cond ((and not-all-blank (mark= point mark1))
                 (%auto-fill-word-past-column current-window buffer point mark1 insertp prefix column))
                ((and not-all-blank (same-line-p mark1 point))
                 (delete-region buffer (region mark1 point))
                 (if (> (mark-column buffer point) column)
                     (%auto-fill-word-past-column current-window buffer point mark1 insertp prefix column)
                     (%filling-set-next-line current-window buffer point nil prefix)))
                (t
                 (line-start mark1 (mark-line point))
                 (delete-region buffer (region mark1 point))
                 (%filling-set-next-line current-window buffer point nil prefix)))))
      (if insertp (insert-character buffer point #\space))))



;;;      %AUTO-FILL-WORD-PAST-COLUMN takes a point, a second mark that is
;;; mark= at the end of some word, and an indicator of whether a space
;;; should be inserted or not.  First, point is moved before the previous
;;; "word."  If the word is effectively the only word on the line, it
;;; should not be moved down to the next line as it will leave a blank
;;; line.  The third branch handles when the typing began in the middle of
;;; some line (that is, right in front of some word).  Note that the else
;;; clause is the usual case.

(defun %auto-fill-word-past-column (current-window buffer point mark1 insertp prefix column)
  (let ((point-moved-p (reverse-find-attribute buffer point :whitespace)))
    (with-mark ((mark2 point :left-inserting))
      (cond ((or (not point-moved-p)
                 (%auto-fill-blank-before-p buffer point prefix))
             (move-mark point mark1)
             (%filling-set-next-line current-window buffer point nil prefix))
            ((%auto-fill-line-as-region-p buffer point mark2 column)
             (if (and insertp
                      (not (or (end-line-p buffer mark1)
                               (whitespace-attribute-p (next-character buffer mark1)))))
                 (insert-character buffer mark1 #\space))
             (auto-fill-line-as-region buffer point (move-mark mark2 point) prefix column)
             (move-mark point mark1)
             (if (and insertp (end-line-p buffer point))
                 (insert-character buffer point #\space)))
            ((not (or (end-line-p buffer mark1)
                      (whitespace-attribute-p (next-character buffer mark1))))
             (insert-character buffer mark1 #\space)
             (%filling-set-next-line current-window buffer point nil prefix)
             (mark-after buffer point)
             (%auto-fill-clean-previous-line buffer mark1 mark2))
            (t
             (%filling-set-next-line current-window buffer point insertp prefix)
             (%auto-fill-clean-previous-line buffer mark1 mark2))))))



;;; AUTO-FILL-LINE-AS-REGION basically grabs a line as a region and fills
;;; it.  However, it knows about comments and makes auto filling a comment
;;; line as a region look the same as a typical "back up a word and break
;;; the line."  When there is a comment, then region starts where the
;;; comment starts instead of the beginning of the line, but the presence
;;; of a prefix overrides all this.

(defun auto-fill-line-as-region (buffer point mark prefix column)
  (let* ((start (value buffer comment-start))
         (begin (value buffer comment-begin))
         (end (value buffer comment-end)))
    (declare (ignorable begin))
    (line-start mark)
    (cond ((and (not prefix) start (to-line-comment mark start))
           (fill-region buffer
                        (region mark (line-end buffer point))
                        #+NOTYET
                        (gen-comment-prefix mark start begin)
                        column)
           (when end
             (line-start point)
             (do ()
                 ((mark>= mark point))
               #+NOTYET
               (if (not (to-comment-end mark end)) (insert-string buffer mark end))
               (line-offset mark 1 0))))
          (t (fill-region (region mark (line-end buffer point)) prefix column)))))



(defun %auto-fill-blank-before-p (buffer point prefix)
  "is true if whitespace only precedes point except for the prefix."
  (or (blank-before-p buffer point)
      (with-mark ((temp point))
        (reverse-find-attribute buffer temp :whitespace #'zerop)
        (<= (mark-column buffer temp) (length prefix)))))



;;;      %AUTO-FILL-LINE-AS-REGION-P determines if the line point and mark2
;;; sit on is so long that it might as well be filled as if it were a
;;; region.  Mark2 is mark= to point at the beginning of the last word on
;;; the line and is then moved over the whitespace before point.  If the
;;; word end prior the last word on the line is on the same line and not
;;; before column, then fill the line as a region.

(defun %auto-fill-line-as-region-p (buffer point mark2 column)
  (reverse-find-attribute buffer mark2 :whitespace #'zerop)
  (and (same-line-p mark2 point)
       (> (mark-column buffer mark2) column)))



(defun %auto-fill-clean-previous-line (buffer mark1 mark2)
  (when (line-offset buffer mark1 -1)
    (line-end buffer mark1)
    (move-mark mark2 mark1)
    (unless (and (reverse-find-attribute buffer mark1 :whitespace #'zerop)
                 (same-line-p mark1 mark2))
      (line-start mark1 (mark-line mark2)))
    (delete-region buffer (region mark1 mark2))))



;;; %FILLING-SET-NEXT-LINE gets a new blank line and sets it up with the
;;; prefix and places the point correctly.  The argument point must alias
;;; (current-point).

(defun %filling-set-next-line (current-window buffer point insertp prefix)
  (cond ((and (value buffer auto-fill-space-indent) (not prefix))
         #+NOTYET
         (indent-new-comment-line-command current-window nil))
        (t (new-line-command current-window nil)
           (if prefix (insert-string buffer point prefix))))
  (if (not (find-attribute buffer point :whitespace)) (line-end buffer point))
  (if insertp (insert-character buffer point #\space)))



;;;; -- Paragraph Filling --


;;;      %FILL-PARAGRAPH-START takes a mark that has just been moved
;;; forward over some paragraph.  After moving to the beginning of it, we
;;; place the mark appropriately for filling the paragraph as a region.

(defun %fill-paragraph-start (buffer mark prefix prefix-len)
  (declare (ignorable prefix prefix-len))
  (paragraph-offset buffer mark -1)
  (skip-prefix-if-here buffer mark prefix prefix-len)
  (if (text-blank-line-p buffer mark)
      (line-offset mark 1 0)
      (line-start mark)))



;;;; -- Region Filling --


;;;      FILL-REGION-BY-PARAGRAPHS finds paragraphs and uses region filling
;;; primitives to fill them.  Tmark2 is only used for the first paragraph; we
;;; need a mark other than start in case start is in the middle of a paragraph
;;; instead of between two.
;;;
(defun fill-region-by-paragraphs (buffer region &optional
                                         (prefix (value buffer fill-prefix))
                                         (column (value buffer fill-column)))
  "Finds paragraphs in region and fills them as distinct regions using
   FILL-REGION."
  (with-mark ((start (region-start region) :left-inserting))
    (with-mark ((tmark1 start :left-inserting)
                (tmark2 start :left-inserting)) ;only used for first para.
      (let ((region (region (copy-mark (region-start region)) ;deleted by undo.
                            (copy-mark (region-end region))))
            (undo-region (copy-region buffer region))
            (end (region-end region))
            (prefix-len (length prefix))
            (paragraphp (paragraph-offset buffer tmark1 1)))
        (when paragraphp
          (%fill-paragraph-start buffer (move-mark tmark2 tmark1) prefix prefix-len)
          (if (mark>= tmark2 start) (move-mark start tmark2))
          (cond ((mark>= tmark1 end)
                 (fill-region-aux buffer start end prefix prefix-len column))
                (t
                 (fill-region-aux buffer start tmark1 prefix prefix-len column)
                 (do ((paragraphp (mark-paragraph buffer start tmark1)
                                  (mark-paragraph buffer start tmark1)))
                     ((not paragraphp))
                   (if (mark> start end)
                       (return)
                       (cond ((mark>= tmark1 end)
                              (fill-region-aux buffer start end prefix
                                               prefix-len column)
                              (return))
                             (t (fill-region-aux buffer start tmark1
                                                 prefix prefix-len column))))))))
        (make-region-undo buffer :twiddle "Fill Region" region undo-region)))))

(defun fill-region (buffer region &optional
                           (prefix (value buffer fill-prefix))
                           (column (value buffer fill-column)))
  "Fills a region using the given prefix and column."
  (let ((prefix (if (and prefix (string= prefix "")) () prefix)))
    (with-mark ((start (region-start region) :left-inserting))
      (check-fill-prefix buffer prefix column start)
      (fill-region-aux buffer start (region-end region)
                       prefix (length prefix) column))))



;;;      FILL-REGION-AUX grinds over a region between fill-mark and
;;; end-mark deleting blank lines and filling lines.  For each line, the
;;; extra whitespace between words is collapsed to one space, and at the
;;; end and beginning of the line it is deleted.  We do not return after
;;; realizing that fill-mark is after end-mark if the line needs to be
;;; broken; it may be the case that there are several filled line lengths
;;; of material before end-mark on the current line.

(defun fill-region-aux (buffer fill-mark end-mark prefix prefix-len column)
  (if (and (start-line-p fill-mark) prefix)
      (fill-region-prefix-line buffer fill-mark prefix prefix-len))
  (with-mark ((mark1 fill-mark :left-inserting)
              (cmark fill-mark :left-inserting))
    (do ((collapse-p t))
        (nil)
      (line-end buffer fill-mark)
      (line-start (move-mark mark1 fill-mark))
      (skip-prefix-if-here buffer mark1 prefix prefix-len)
      (cond ((mark>= fill-mark end-mark)
             (if (mark= fill-mark end-mark)
                 (fill-region-clear-eol buffer fill-mark))
             (cond ((> (mark-column buffer end-mark) column)
                    (when collapse-p
                      (fill-region-collapse-whitespace buffer cmark end-mark)
                      (setf collapse-p nil))
                    (fill-region-break-line buffer fill-mark prefix
                                            prefix-len end-mark column))
                   (t (return))))
            ((blank-after-p buffer mark1)
             (fill-region-delete-blank-lines buffer fill-mark end-mark prefix prefix-len)
             (cond ((mark< fill-mark end-mark)
                    (if prefix
                        (fill-region-prefix-line buffer fill-mark prefix prefix-len))
                    (fill-region-clear-bol buffer fill-mark)
                    (move-mark cmark fill-mark))
                   (t (return)))
             (setf collapse-p t))
            (t (fill-region-clear-eol buffer fill-mark)
               (if collapse-p (fill-region-collapse-whitespace buffer cmark fill-mark))
               (cond ((> (mark-column buffer fill-mark) column)
                      (fill-region-break-line buffer fill-mark prefix
                                              prefix-len end-mark column)
                      (setf collapse-p nil))
                     (t (fill-region-get-next-line buffer fill-mark column
                                                   prefix prefix-len end-mark)
                        (move-mark cmark fill-mark)
                        (setf collapse-p t))))))
    (move-mark fill-mark end-mark)))



;;;      FILL-REGION-BREAK-LINE breaks lines as close to the low side
;;; column as possible.  The first branch handles a word lying across
;;; column while the second takes care of whitespace passing column.  If
;;; FILL-REGION-WORD-PAST-COLUMN encountered a single word stretching over
;;; column, it would leave an extra opened line that needs to be cleaned up
;;; or filled up.

(defun fill-region-break-line (buffer fill-mark prefix prefix-length
                                          end-mark column)
  (with-mark ((mark1 fill-mark :left-inserting))
    (move-to-column buffer mark1 column)
    (cond ((not (whitespace-attribute-p (next-character buffer mark1)))
           (if (not (find-attribute buffer mark1 :whitespace))
               (line-end buffer mark1))
           (move-mark fill-mark mark1)
           (if (eq (fill-region-word-past-column buffer fill-mark mark1 prefix)
                   :handled-oversized-word)
               (if (mark>= fill-mark end-mark)
                   (delete-characters buffer (line-start fill-mark)
                                      prefix-length)
                   (delete-characters buffer fill-mark 1))))
          (t (move-mark fill-mark mark1)
             (unless (and (reverse-find-attribute buffer mark1 :whitespace #'zerop)
                          (same-line-p mark1 fill-mark))
               (line-start mark1 (mark-line fill-mark)))
             ;; forward find must move mark because of cond branch we are in.
             (find-attribute fill-mark :whitespace #'zerop)
             (unless (same-line-p mark1 fill-mark)
               (line-end fill-mark (mark-line mark1)))
             (delete-region buffer (region mark1 fill-mark))
             (insert-character buffer fill-mark #\newline)
             (if prefix (insert-string buffer fill-mark prefix))))))



;;;      FILL-REGION-WORD-PAST-COLUMN takes a point and a second mark that
;;; is mark= at the end of some word.  First, point is moved before the
;;; previous "word."  If the word is effectively the only word on the line,
;;; it should not be moved down to the next line as it will leave a blank
;;; line.

(defun fill-region-word-past-column (buffer point mark1 prefix)
  (with-mark ((mark2 (copy-mark point :left-inserting)))
    (let ((point-moved-p (reverse-find-attribute buffer point :whitespace))
          (hack-for-fill-region :handled-normal-case))
      (cond ((or (not point-moved-p)
                 (%auto-fill-blank-before-p buffer point prefix))
             (setf hack-for-fill-region :handled-oversized-word)
             (move-mark point mark1)
             (fill-region-set-next-line buffer point prefix))
            (t (fill-region-set-next-line buffer point prefix)
               (%auto-fill-clean-previous-line buffer mark1 mark2)))
      hack-for-fill-region)))

(defun fill-region-set-next-line (buffer point prefix)
  (insert-character buffer point #\newline)
  (if prefix (insert-string buffer point prefix))
  (if (not (find-attribute buffer point :whitespace)) (line-end buffer point)))



;;;      FILL-REGION-GET-NEXT-LINE gets another line when the current one
;;; is short of the fill column.  It cleans extraneous whitespace from the
;;; beginning of the next line to fill.  To save typical redisplay the
;;; length of the first word is added to the ending column of the current
;;; line to see if it extends past the fill column; if it does, then the
;;; fill-mark is left on the new line instead of merging the new line with
;;; the current one.  The fill-mark is left after a prefix (if there is one)
;;; on a new line, before the first word brought up to the current line, or
;;; after the end mark.

(defun fill-region-get-next-line (buffer fill-mark column prefix prefix-len end-mark)
  (let ((prev-end-pos (mark-column buffer fill-mark))
        (two-spaces-p (fill-region-insert-two-spaces-p buffer fill-mark)))
    (with-mark ((tmark fill-mark :left-inserting))
      (fill-region-find-next-line buffer fill-mark prefix prefix-len end-mark)
      (move-mark tmark fill-mark)
      (cond ((mark< fill-mark end-mark)
             (skip-prefix-if-here buffer tmark prefix prefix-len)
             (fill-region-clear-bol buffer tmark)
             (let ((beginning-pos (mark-column buffer tmark)))
               (find-attribute buffer tmark :whitespace)
               (cond ((> (+ prev-end-pos (if two-spaces-p 2 1)
                            (- (mark-column buffer tmark) beginning-pos))
                         column)
                      (if prefix
                          (fill-region-prefix-line buffer fill-mark prefix prefix-len)))
                     (t
                      (if (and prefix
                               (%line-has-prefix-p buffer fill-mark prefix prefix-len))
                          (delete-characters fill-mark prefix-len))
                      (delete-characters buffer fill-mark -1)
                      (insert-character buffer fill-mark #\space)
                      (if two-spaces-p (insert-character buffer fill-mark #\space))))))
            (t
             (mark-after buffer fill-mark))))))



;;;      FILL-REGION-FIND-NEXT-LINE finds the next non-blank line, modulo
;;; fill prefixes, and deletes the intervening lines.  Fill-mark is left at
;;; the beginning of the next line.

(defun fill-region-find-next-line (buffer fill-mark prefix prefix-len end-mark)
  (line-offset fill-mark 1 0)
  (when (mark< fill-mark end-mark)
    (skip-prefix-if-here buffer fill-mark prefix prefix-len)
    (if (blank-after-p buffer fill-mark)
        (fill-region-delete-blank-lines buffer fill-mark end-mark prefix prefix-len)
        (line-start fill-mark))))



;;;      FILL-REGION-DELETE-BLANK-LINES deletes the blank line mark is on
;;; and all successive blank lines.  Mark is left at the beginning of the
;;; first non-blank line by virtue of its placement and region deletions.

(defun fill-region-delete-blank-lines (buffer mark end-mark prefix prefix-len)
  (line-start mark)
  (with-mark ((tmark mark :left-inserting))
    (do ((linep (line-offset tmark 1 0) (line-offset tmark 1 0)))
        ((not linep)
         (move-mark tmark end-mark)
         (delete-region buffer (region mark tmark)))
      (skip-prefix-if-here buffer tmark prefix prefix-len)
      (when (mark>= tmark end-mark)
        (move-mark tmark end-mark)
        (delete-region buffer (region mark tmark))
        (return))
      (unless (blank-after-p buffer tmark)
        (line-start tmark)
        (delete-region buffer (region mark tmark))
        (return)))))



;;;      FILL-REGION-CLEAR-BOL clears the initial whitespace on a line
;;; known to be non-blank.  Note that the fill prefix is not considered, so
;;; the mark must have been moved over it already if there is one.

(defun fill-region-clear-bol (buffer mark)
  (with-mark ((tmark mark :left-inserting))
    (find-attribute tmark :whitespace #'zerop)
    (unless (mark= mark tmark)
      (delete-region buffer (region mark tmark)))))



;;;      FILL-REGION-COLLAPSE-WHITESPACE deletes extra whitespace between
;;; blocks of non-whitespace characters from mark1 to mark2.  Tabs are
;;; converted into a single space.  Mark2 must be on the same line as mark1
;;; since there is no concern of newlines, prefixes on a new line, blank
;;; lines between blocks of non-whitespace characters, etc.

(defun fill-region-collapse-whitespace (buffer mark1 mark2)
  (with-mark ((tmark mark1 :left-inserting))
    ;; skip whitespace at beginning of line or single space between words
    (find-attribute mark1 :whitespace #'zerop)
    (unless (mark>= mark1 mark2)
      (do ()
          (nil)
        (if (not (find-attribute buffer mark1 :whitespace)) ;not end of buffer
            (return))
        (if (mark>= mark1 mark2) (return))
        (if (char/= (next-character buffer mark1) #\space)
            ;; since only on one line, must be tab or space
            (setf (next-character buffer mark1) #\space))
        (move-mark tmark mark1)
        (if (mark= (mark-after buffer mark1) mark2) (return))
        (let ((char (next-character buffer mark1)))
          (when (and (fill-region-insert-two-spaces-p buffer tmark)
                     (char= char #\space))
            ;; if at the end of a sentence, don't blow away the second space
            (if (mark= (mark-after buffer mark1) mark2)
                (return)
                (setf char (next-character buffer mark1))))
          (when (whitespace-attribute-p char) ;more whitespace than necessary
            (find-attribute (move-mark tmark mark1) :whitespace #'zerop)
            (if (mark>= tmark mark2) (move-mark tmark mark2))
            (delete-region buffer (region mark1 tmark))))))))



;;;      FILL-REGION-CLEAR-EOL must check the result of
;;; REVERSE-FIND-ATTRIBUTE because if fill-mark did not move, then we are
;;; only whitespace away from the beginning of the buffer.

(defun fill-region-clear-eol (buffer fill-mark)
  (with-mark ((mark1 fill-mark :left-inserting))
    (unless (and (reverse-find-attribute buffer mark1 :whitespace #'zerop)
                 (same-line-p mark1 fill-mark))
      (line-start mark1 (mark-line fill-mark)))
    (delete-region buffer (region mark1 fill-mark))))



(defun fill-region-prefix-line (buffer fill-mark prefix prefix-length)
  (if (%line-has-prefix-p buffer fill-mark prefix prefix-length)
      (character-offset buffer fill-mark prefix-length)
      (insert-string buffer fill-mark prefix)))



(defun %line-has-prefix-p (buffer mark prefix prefix-length)
  (declare (simple-string prefix))
  (if (>= (line-length buffer (mark-line mark)) prefix-length)
      (string= prefix (the simple-string (line-string buffer (mark-line mark)))
               :end2 prefix-length)))



;;;      FILL-REGION-INSERT-TWO-SPACES-P returns true if a sentence
;;; terminator is followed by any number of "closing characters" such as
;;; ",',),etc.  If there is a sentence terminator at the end of the current
;;; line, it must be assumed to be the end of a sentence as opposed to an
;;; abbreviation.  Why?  Because EMACS does, and besides, what would Lisp
;;; code be without heuristics.

(defun fill-region-insert-two-spaces-p (buffer mark)
  (do ((n 0 (1+ n)))
      ((not (sentence-closing-char-attribute-p (previous-character buffer mark)))
       (cond ((sentence-terminator-attribute-p (previous-character buffer mark))
              (character-offset buffer mark n))
             (t (character-offset buffer mark n) nil)))
    (mark-before buffer mark)))



(defun check-fill-prefix (buffer prefix column mark)
  (when prefix
    (insert-character buffer mark #\newline)
    (insert-character buffer mark #\newline)
    (mark-before buffer mark)
    (insert-string buffer mark prefix)
    (let ((pos (mark-column buffer mark)))
      (declare (simple-string prefix))
      (mark-after buffer mark)
      (delete-characters buffer mark (- (+ (length prefix) 2)))
      (if (>= pos column)
          (editor-error
           "The fill prefix length is longer than the fill column.")))))
