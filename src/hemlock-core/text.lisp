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
;;; This file contains stuff that operates on units of texts, such as
;;;    paragraphs, sentences, lines, and words.
;;;

(in-package :hemlock)

;;;; -- New Variables --



;;; The standard paragraph delimiting function is DEFAULT-PARA-DELIM-FUNCTION
(defun default-para-delim-function (buffer mark)
  "Return whether or not to break on this line."
  (paragraph-delimiter-attribute-p (next-character buffer mark)))

;;;; -- Paragraph Commands --

(defhcommand forward-paragraph-command (window p)
  (:pretty-name "Forward Paragraph")
  (:documentation "moves point to the end of the current (next) paragraph.")
  (:help-string "moves point to the end of the current (next) paragraph."))

(defmethod forward-paragraph-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (unless (paragraph-offset buffer point (or p 1))
      (buffer-end point buffer)
      (editor-error))))

(defhcommand backward-paragraph-command (window p)
  (:pretty-name "Backward Paragraph")
  (:documentation "moves point to the start of the current (previous) paragraph.")
  (:help-string "moves point to the start of the current (previous) paragraph."))

(defmethod backward-paragraph-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (unless (paragraph-offset buffer point (- (or p 1)))
      (buffer-start point buffer)
      (editor-error))))

(defhcommand mark-paragraph-command (window p)
  (:pretty-name "Mark Paragraph")
  (:documenation
   "Put mark and point around current or next paragraph.
   A paragraph is delimited by a blank line, a line beginning with a
   special character (@,\,-,',and .), or it is begun with a line with at
   least one whitespace character starting it.  Prefixes are ignored or
   skipped over before determining if a line starts or delimits a
   paragraph.")
  (:help-string "Put mark and point around current or next paragraph."))

(defmethod mark-paragraph-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (mark (copy-mark point :temporary)))
    (if (mark-paragraph buffer point mark)
        (push-buffer-mark mark t)
        (editor-error))))

(defun mark-paragraph (buffer mark1 mark2)
  "Mark the next or current paragraph, setting mark1 to the beginning and mark2
   to the end.  This uses \"Fill Prefix\", and mark1 is always on the first
   line of the paragraph.  If no paragraph is found, then the marks are not
   moved, and nil is returned."
  (with-mark ((tmark1 mark1)
              (tmark2 mark2))
    (let* ((prefix (value buffer fill-prefix))
           (prefix-len (length prefix))
           (paragraphp (paragraph-offset buffer tmark2 1)))
      (when (or paragraphp
                (and (last-line-p tmark2)
                     (end-line-p buffer tmark2)
                     (not (blank-line-p buffer (mark-line tmark2)))))
        (mark-before buffer (move-mark tmark1 tmark2))
        (%fill-paragraph-start buffer tmark1 prefix prefix-len)
        (move-mark mark1 tmark1)
        (move-mark mark2 tmark2)))))



(eval-when (:compile-toplevel :execute)

;;;      %MARK-TO-PARAGRAPH moves mark to next immediate (current)
;;; paragraph in the specified direction.  Nil is returned when no
;;; paragraph is found.  NOTE: the order of the arguments to OR within the
;;; first branch of the COND must be as it is, and mark must be at the
;;; beginning of the line it is on.
(defmacro %mark-to-paragraph (buffer mark prefix prefix-length
                              &optional (direction :forward))
  (let ((buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer))
       (do ((skip-prefix-p)
            (paragraph-delim-function (value buffer paragraph-delimiter-function)))
           (nil)
         (setf skip-prefix-p
               (and ,prefix (%line-has-prefix-p ,buffer-sym ,mark ,prefix ,prefix-length)))
         (if skip-prefix-p (character-offset ,buffer-sym ,mark ,prefix-length))
         (let ((next-char (next-character ,buffer-sym ,mark)))
           (cond ((and (not (blank-after-p ,buffer ,mark))
                       (or (whitespace-attribute-p next-char)
                           (not (funcall paragraph-delim-function ,mark))))
                  (return (if skip-prefix-p (line-start ,mark) ,mark)))
                 (,(if (eq direction :forward)
                       `(last-line-p ,mark)
                       `(first-line-p ,mark))
                  (if skip-prefix-p (line-start ,mark))
                  (return nil)))
           (line-offset ,buffer-sym ,mark ,(if (eq direction :forward) 1 -1) 0))))))


;;;      %PARAGRAPH-OFFSET-AUX is the inner loop of PARAGRAPH-OFFSET.  It
;;; moves over a paragraph to find the beginning or end depending on
;;; direction.  Prefixes on a line are ignored or skipped over before it
;;; is determined if the line is a paragraph boundary.
(defmacro %paragraph-offset-aux (buffer mark prefix prefix-length
                                 &optional (direction :forward))
  (let ((buffer-sym (gensym)))
  `(let ((,buffer-sym ,buffer))
     (do ((paragraph-delim-function (value buffer paragraph-delimiter-function))
        (skip-prefix-p))
       (nil)
     (setf skip-prefix-p
           (and ,prefix (%line-has-prefix-p ,buffer-sym ,mark ,prefix ,prefix-length)))
     (if skip-prefix-p (character-offset ,buffer-sym ,mark ,prefix-length))
     (cond ((or (blank-after-p ,buffer-sym ,mark)
                (funcall paragraph-delim-function ,mark))
            (return (line-start ,mark)))
           (,(if (eq direction :forward)
                 `(last-line-p ,mark)
                 `(first-line-p ,mark))
            (return ,(if (eq direction :forward)
                         `(line-end ,buffer-sym ,mark)
                         `(line-start ,mark)))))
     (line-offset ,buffer-sym ,mark ,(if (eq direction :forward) 1 -1) 0)))))

); eval-when



;;;      PARAGRAPH-OFFSET takes a mark and a number of paragraphs to
;;; move over.  If the specified number of paragraphs does not exist in
;;; the direction indicated by the sign of number, then nil is
;;; returned, otherwise the mark is returned.

(defun paragraph-offset (buffer mark number &optional (prefix (value buffer fill-prefix)))
  "moves mark past the specified number of paragraph, forward if the number
   is positive and vice versa.  If the specified number of paragraphs do
   not exist in the direction indicated by the sign of the number, then nil
   is returned, otherwise the mark is returned."
  (if (plusp number)
      (%paragraph-offset-forward buffer mark number prefix)
      (%paragraph-offset-backward buffer mark number prefix)))



;;;      %PARAGRAPH-OFFSET-FORWARD moves mark forward over number
;;; paragraphs.  The first branch of the COND is necessary for the side
;;; effect provided by LINE-OFFSET.  If %MARK-TO-PARAGRAPH left tmark at
;;; the beginning of some paragraph %PARAGRAPH-OFFSET-AUX will think it has
;;; moved mark past a paragraph, so we make sure tmark is inside the
;;; paragraph or after it.

(defun %paragraph-offset-forward (buffer mark number prefix)
  (do* ((n number (1- n))
        (tmark (line-start (copy-mark mark :temporary)))
        (prefix-length (length prefix))
        (paragraphp (%mark-to-paragraph buffer tmark prefix prefix-length)
                    (if (plusp n)
                        (%mark-to-paragraph buffer tmark prefix prefix-length))))
       ((zerop n) (move-mark mark tmark))
    (cond ((and paragraphp (not (line-offset buffer tmark 1))) ;
           (if (or (> n 1) (and (last-line-p mark) (end-line-p buffer mark)))
               (return nil))
           (return (line-end buffer (move-mark mark tmark))))
          (paragraphp (%paragraph-offset-aux buffer tmark prefix prefix-length))
          (t (return nil)))))



(defun %paragraph-offset-backward (buffer mark number prefix)
  (declare (ignorable buffer))
  (with-mark ((tmark1 mark)
              (tmark2 mark))
    (do* ((n (abs number) (1- n))
          (prefix-length (length prefix))
          (paragraphp (%para-offset-back-find-para buffer tmark1 prefix
                                                   prefix-length mark)
                      (if (plusp n)
                          (%para-offset-back-find-para buffer tmark1 prefix
                                                       prefix-length tmark2))))
         ((zerop n) (move-mark mark tmark1))
      (cond ((and paragraphp (first-line-p tmark1))
             (if (and (first-line-p mark) (start-line-p mark))
                 (return nil)
                 (if (> n 1) (return nil))))
            (paragraphp
             (%paragraph-offset-aux tmark1 prefix prefix-length :backward)
             (%para-offset-back-place-mark buffer tmark1 prefix prefix-length))
            (t (return nil))))))



;;;      %PARA-OFFSET-BACK-PLACE-MARK makes sure that mark is in
;;; the right place when it has been moved backward over a paragraph.  The
;;; "right place" is defined to be where EMACS leaves it for a given
;;; situation or where it is necessary to ensure the mark's skipping
;;; backward over another paragraph if PARAGRAPH-OFFSET was given an
;;; argument with a greater magnitude than one.  I believe these two
;;; constraints are equivalent; that is, neither changes what the other
;;; would dictate.

(defun %para-offset-back-place-mark (buffer mark prefix prefix-length)
  (skip-prefix-if-here buffer mark prefix prefix-length)
  (cond ((text-blank-line-p buffer mark) (line-start mark))
        ((not (first-line-p mark))
         (line-offset mark -1 0)
         (skip-prefix-if-here buffer mark prefix prefix-length)
         (if (text-blank-line-p buffer mark)
             (line-start mark)
             (line-offset mark 1 0)))))



(defun %para-offset-back-find-para (buffer mark1 prefix prefix-length mark2)
  (move-mark mark2 mark1)
  (line-start mark1)
  (let ((para-p (%mark-to-paragraph mark1 prefix prefix-length :backward)))
    (cond ((and para-p (same-line-p mark1 mark2))
           (skip-prefix-if-here buffer mark1 prefix prefix-length)
           (find-attribute mark1 :whitespace #'zerop)
           (cond ((mark<= mark2 mark1)
                  (line-offset mark1 -1 0)
                  (%mark-to-paragraph mark1 prefix prefix-length :backward))
                 (t (line-start mark1))))
          (t para-p))))



;;;; -- Sentence Commands --

(defhcommand forward-sentence-command (window p)
  (:pretty-name "Forward Sentence")
  (:documentation
   "Moves forward one sentence or the specified number.
   A sentence terminates with a .,?, or ! followed by any number of closing
   delimiters (such as \",',),],>,|) which are followed by either two
   spaces or a newline.")
  (:help-string "Moves forward one sentence or the specified number."))

(defmethod forward-sentence-command (window p)
  (let ((buffer (window-buffer window)))
    (unless (sentence-offset buffer (buffer-point buffer) (or p 1))
      (editor-error))))



(defhcommand backward-sentence-command (window p)
  (:pretty-name "Backward Sentence")
  (:documentation
   "Moves backward one sentence or the specified number.
   A sentence terminates with a .,?, or ! followed by any number of closing
   delimiters (such as \",',),],>,|) which are followed by either two
   spaces or a newline.")
  (:help-string "Moves backward one sentence or the specified number."))

(defmethod backward-sentence-command (window p)
    (let ((buffer (window-buffer window)))
      (unless (sentence-offset buffer (buffer-point buffer) (- (or p 1)))
        (editor-error))))



(defhcommand mark-sentence-command (window p)
  (:pretty-name "Mark Sentence")
  (:documentation
   "Put mark and point around current or next sentence.
   A sentence terminates with a .,?, or ! followed by any number of closing
   delimiters (such as \",',),],>,|) which are followed by either two
   spaces or a newline.")
  (:help-string "Put mark and point around current or next sentence."))

(defmethod mark-sentence-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (end (copy-mark point :temporary)))
    (unless (sentence-offset buffer end 1) (editor-error))
    (move-mark point end)
    (sentence-offset buffer point -1)
    (push-buffer-mark end t)))


(defhcommand forward-kill-sentence-command (window p)
  (:pretty-name "Forward Kill Sentence")
  (:documentation
   "Kill forward to end of sentence.")
  (:help-string
   "Kill forward to end of sentence."))

(defmethod forward-kill-sentence-command (window p)
  (when window
    (let ((buffer (window-buffer window)))
      (when buffer
        (let ((offset (or p 1))
              (point (buffer-point buffer)))
          (with-mark ((mark point))
            (if (sentence-offset buffer mark offset)
                (if (plusp offset)
                    (kill-region buffer (region point mark) :kill-forward)
                    (kill-region buffer (region mark point) :kill-backward))
                (editor-error))))))))

(defhcommand backward-kill-sentence-command (window p)
  (:pretty-name "Backward Kill Sentence")
  (:documentation
   "Kill backward to beginning of sentence.")
  (:help-string
   "Kill backward to beginning of sentence."))

(defmethod backward-kill-sentence-command (window p)
  (forward-kill-sentence-command window (- (or p 1))))

;;;      SENTENCE-OFFSET-END-P returns true if mark is at the end of a
;;; sentence.  If that the end of a sentence, it leaves mark at an
;;; appropriate position with respect to the sentence-terminator character,
;;; the beginning of the next sentence, and direction.  See the commands
;;; "Forward Sentence" and "Backward Sentence" for a definition of a sentence.

(eval-when (:compile-toplevel :execute)
  (defmacro sentence-offset-end-p (buffer mark &optional (direction :forward))
    (let ((buffer-sym (gensym)))
      `(let ((,buffer-sym ,buffer)
             (start (mark-charpos ,mark)))
         (do ()
             ((not (sentence-closing-char-attribute-p (next-character ,buffer-sym ,mark))))
           (mark-after ,buffer-sym ,mark))
         (let ((next (next-character ,buffer-sym ,mark)))
           (cond ((or (not next)
                      (char= next #\newline))
                  ,(if (eq direction :forward) mark `(mark-after ,buffer-sym ,mark)))
                 ((and (char= next #\space)
                       (member (next-character ,buffer-sym (mark-after ,buffer-sym ,mark))
                               '(nil #\space #\newline)))
                  ,(if (eq direction :forward)
                       `(mark-before ,buffer-sym ,mark)
                       `(mark-after ,buffer-sym ,mark)))
                 (t (move-to-position ,mark start)
                    nil))))))
); eval-when



;;;      SENTENCE-OFFSET-FIND-END moves in the direction direction stopping
;;; at sentence terminating characters until either there are not any more
;;; such characters or one is found that defines the end of a sentence.
;;; When looking backwards, we may be at the beginning of some sentence,
;;; and if we are, then we must move mark before the sentence terminator;
;;; otherwise, we would find the immediately preceding sentence terminator
;;; and end up right where we started.

(eval-when (:compile-toplevel :execute)
  (defmacro sentence-offset-find-end (buffer mark &optional (direction :forward))
    (let ((buffer-sym (gensym)))
      `(let ((,buffer-sym ,buffer))
         ,@(if (eq direction :backward)
               `((reverse-find-attribute ,buffer ,mark :whitespace #'zerop)
                 (when (fill-region-insert-two-spaces-p ,mark)
                   (reverse-find-attribute ,buffer ,mark :sentence-terminator)
                   (mark-before ,buffer-sym ,mark))))
         (do ((foundp) (endp)) (nil)
           (setf foundp ,(if (eq direction :forward)
                             `(find-attribute ,buffer-sym ,mark :sentence-terminator)
                             `(reverse-find-attribute ,buffer ,mark :sentence-terminator)))
           (setf endp ,(if (eq direction :forward)
                           `(if foundp (progn (mark-after ,buffer-sym ,mark)
                                              (sentence-offset-end-p ,buffer-sym ,mark)))
                           `(if foundp (sentence-offset-end-p ,buffer-sym ,mark :backward))))
           (if endp (return ,mark))
           ,(if (eq direction :forward)
                `(unless foundp (return nil))
                `(if foundp (mark-before ,buffer-sym ,mark) (return nil)))))))
  )                                     ; eval-when



;;;      SENTENCE-OFFSET takes a mark and a number of paragraphs to move
;;; over.  If the specified number of paragraphs does not exist in
;;; the direction indicated by the sign of the number, then nil is returned,
;;; otherwise the mark is returned.

(defun sentence-offset (buffer mark number)
  (if (plusp number)
      (sentence-offset-forward buffer mark number)
      (sentence-offset-backward buffer mark (abs number))))



;;;      SENTENCE-OFFSET-FORWARD tries to move mark forward over number
;;; sentences.  If it can, then mark is moved and returned; otherwise, mark
;;; remains unmoved, and nil is returned.  When tmark2 is moved to the end
;;; of a new paragraph, we reverse find for a non-whitespace character to
;;; bring tmark2 to the end of the previous line.  This is necessary to
;;; detect if tmark1 is at the end of the paragraph, in which case tmark2
;;; wants to be moved over another paragraph.

(defun sentence-offset-forward (buffer mark number)
  (with-mark ((tmark1 mark)
              (tmark2 mark))
    (do ((n number (1- n))
         (found-paragraph-p))
        ((zerop n) (move-mark mark tmark1))
      (when (and (mark<= tmark2 tmark1)
                 (setf found-paragraph-p (paragraph-offset buffer tmark2 1)))
        (reverse-find-attribute buffer tmark2 :whitespace #'zerop)
        (when (mark>= tmark1 tmark2)
          (line-offset tmark2 1 0)
          (setf found-paragraph-p (paragraph-offset buffer tmark2 1))
          (reverse-find-attribute buffer tmark2 :whitespace #'zerop)))
      (cond ((sentence-offset-find-end buffer tmark1)
             (if (mark> tmark1 tmark2) (move-mark tmark1 tmark2)))
            (found-paragraph-p (move-mark tmark1 tmark2))
            (t (return nil))))))



(defun sentence-offset-backward (buffer mark number)
  (declare (ignorable buffer))
  (with-mark ((tmark1 mark)
              (tmark2 mark)
              (tmark3 mark))
    (do* ((n number (1- n))
          (prefix (value buffer fill-prefix))
          (prefix-length (length prefix))
          (found-paragraph-p
           (cond ((paragraph-offset buffer tmark2 -1)
                  (sent-back-place-para-start buffer tmark2 prefix prefix-length)
                  t))))
         ((zerop n) (move-mark mark tmark1))
      (move-mark tmark3 tmark1)
      (when (and (sent-back-para-start-p buffer tmark1 tmark3 prefix prefix-length)
                 (setf found-paragraph-p
                       (paragraph-offset buffer (move-mark tmark2 tmark3) -1)))
        (paragraph-offset buffer (move-mark tmark1 tmark2) 1)
        (sent-back-place-para-start buffer tmark2 prefix prefix-length))
      (cond ((sentence-offset-find-end tmark1 :backward)
             (if (mark< tmark1 tmark2) (move-mark tmark1 tmark2)))
            (found-paragraph-p (move-mark tmark1 tmark2))
            (t (return nil))))))



(defun sent-back-para-start-p (buffer mark1 mark2 prefix prefix-length)
  (skip-prefix-if-here buffer (line-start mark2) prefix prefix-length)
  (cond ((text-blank-line-p buffer mark2)
         (line-start mark2))
        ((whitespace-attribute-p (next-character buffer mark2))
         (find-attribute mark2 :whitespace #'zerop)
         (if (mark= mark1 mark2) (line-offset mark2 -1 0)))
        ((and (mark= mark2 mark1) (line-offset mark2 -1 0))
         (skip-prefix-if-here buffer mark2 prefix prefix-length)
         (if (text-blank-line-p buffer mark2)
             (line-start mark2)))))



(defun sent-back-place-para-start (buffer mark2 prefix prefix-length)
  (skip-prefix-if-here buffer mark2 prefix prefix-length)
  (when (text-blank-line-p buffer mark2)
    (line-offset mark2 1 0)
    (skip-prefix-if-here buffer mark2 prefix prefix-length))
  (find-attribute mark2 :whitespace #'zerop))



;;;; -- Transposing Stuff --

(defhcommand transpose-words-command (window p)
  (:pretty-name "Transpose Words")
  (:documentation
   "Transpose the words before and after the cursor.
   With a positive argument it transposes the words before and after the
   cursor, moves right, and repeats the specified number of times,
   dragging the word to the left of the cursor right.  With a negative
   argument, it transposes the two words to the left of the cursor, moves
   between them, and repeats the specified number of times, exactly undoing
   the positive argument form.")
  (:help-string
   "Transpose the words before and after the cursor."))

(defmethod transpose-words-command (window p)
  (let* ((num (or p 1))
         (buffer (when window (window-buffer window)))
         (point (when buffer (buffer-point buffer))))
    (when buffer
      (with-mark ((mark point :left-inserting)
                  (start point :left-inserting))
        (let ((mark-prev nil)
              (mark-next nil))
          (with-buffer-globals (buffer)
            (setq mark-prev (previous-character buffer mark))
            (setq mark-next (next-character buffer mark)))
          (cond ((plusp num)
                 (let ((forwardp (word-offset buffer point num))
                       (backwardp (if (or (word-delimiter-attribute-p mark-next)
                                          (word-delimiter-attribute-p mark-prev))
                                      (word-offset buffer mark -1)
                                      (word-offset buffer mark -2))))
                   (if (and forwardp backwardp)
                       (transpose-words-forward buffer mark point start)
                       (editor-error))))
                ((minusp num)
                 (let ((enoughp (word-offset buffer point (1- num))))
                   (if (word-delimiter-attribute-p mark-prev)
                       (reverse-find-attribute buffer mark :word-delimiter #'zerop)
                       (find-attribute buffer mark :word-delimiter))
                   (if enoughp
                       (transpose-words-backward buffer point mark start)
                       (editor-error))))
                (t (editor-error)))))
      (mapcar #'climish::redraw-buffer (buffer-windows buffer)))))


(defun transpose-words-forward (buffer mark1 end mark2)
  (with-buffer-globals (buffer)
    (with-mark ((tmark1 mark1 :left-inserting)
                (tmark2 mark2 :left-inserting))
      (find-attribute buffer tmark1 :word-delimiter)
      (do ((region1 (delete-and-save-region buffer (region mark1 tmark1))))
          ((mark= tmark2 end) (ninsert-region buffer end region1))
        (word-offset buffer tmark2 1)
        (reverse-find-attribute buffer (move-mark tmark1 tmark2) :word-delimiter)
        (ninsert-region buffer mark1 (delete-and-save-region buffer (region tmark1 tmark2)))
        (move-mark mark1 tmark1)))))


(defun transpose-words-backward (buffer start mark1 mark2)
  (with-buffer-globals (buffer)
    (with-mark ((tmark1 mark1 :left-inserting)
                (tmark2 mark2 :left-inserting))
      (reverse-find-attribute buffer tmark1 :word-delimiter)
      (move-mark mark2 mark1)
      (do ((region1 (delete-and-save-region buffer (region tmark1 mark1))))
          ((mark= tmark1 start) (ninsert-region buffer start region1))
        (word-offset buffer tmark1 -1)
        (find-attribute buffer (move-mark tmark2 tmark1) :word-delimiter)
        (ninsert-region buffer mark1 (delete-and-save-region buffer (region tmark1 tmark2)))
        (move-mark mark1 tmark1)))))

(defhcommand transpose-lines-command (window p)
  (:pretty-name "Transpose Lines")
  (:documentation
   "Transpose the current line with the line before the cursor.
   With a positive argument it transposes the current line with the one
   before, moves down a line, and repeats the specified number of times,
   dragging the originally current line down.  With a negative argument, it
   transposes the two lines to the prior to the current, moves up a line,
   and repeats the specified number of times, exactly undoing the positive
   argument form.  With a zero argument, it transposes the lines at point
   and mark.")
  (:help-string "Transpose the current line with the line before the cursor."))

(defmethod transpose-lines-command (window p)
  (let* ((num (or p 1))
         (buffer (window-buffer window))
         (point (buffer-point buffer)))
    (with-mark ((mark point :left-inserting))
      (cond ((plusp num)
             (if (and (line-offset mark -1 0)
                      (line-offset point num 0))
                 (transpose-lines buffer mark point)
                 (editor-error)))
            ((minusp num)
             (cond ((and (line-offset mark (1- num) 0)
                         (line-offset point -1 0))
                    (transpose-lines buffer point mark)
                    (move-mark point mark))
                   (t (editor-error))))
            (t
             (rotatef (line-string buffer (mark-line point))
                      (line-string buffer (mark-line (current-mark buffer))))
             (line-start point))))))


(defun transpose-lines (buffer mark1 mark2)
  (with-mark ((tmark1 mark1))
    (line-offset buffer tmark1 1)
    (ninsert-region buffer mark2 (delete-and-save-region buffer (region mark1 tmark1)))))



;;;; -- Utilities --

(defun skip-prefix-if-here (buffer mark prefix prefix-length)
  (if (and prefix (%line-has-prefix-p buffer mark prefix prefix-length))
      (character-offset buffer mark prefix-length)))



(defun text-blank-line-p (buffer mark)
  (let ((next-char (next-character buffer mark)))
    (or (blank-after-p buffer mark)
        (and (funcall (value buffer paragraph-delimiter-function) mark)
             (not (whitespace-attribute-p next-char))))))



(defun whitespace-attribute-p (char)
  (= (character-attribute :whitespace char) 1))

(defun sentence-terminator-attribute-p (char)
  (= (character-attribute :sentence-terminator char) 1))

(defun sentence-closing-char-attribute-p (char)
  (= (character-attribute :sentence-closing-char char) 1))

(defun paragraph-delimiter-attribute-p (char)
  (= (character-attribute :paragraph-delimiter char) 1))

(defun word-delimiter-attribute-p (char)
  (= (character-attribute :word-delimiter char) 1))
