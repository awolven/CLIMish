;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock indentation commands
;;;
;;; Written by Bill Maddox and Bill Chiles
;;;
(in-package :hemlock)





(defmacro .indent-using-tabs. (mark column)
  "Inserts at mark a maximum number of tabs and a minimum number of spaces to
   move mark to column.  This assumes mark is at the beginning of a line."
  (let ((mark-sym (gensym "INDENT-USING-TABS_MARK"))
        (column-sym (gensym "INDENT-USING-TABS_COLUMN")))
    `(let ((,mark-sym ,mark)
           (,column-sym ,column))
       (multiple-value-bind (tabs spaces)
           (floor ,column-sym (hi::value .buffer. spaces-per-tab))
         (dotimes (i tabs) (insert-character .buffer. ,mark-sym #\tab))
         (dotimes (i spaces) (insert-character .buffer. ,mark-sym #\space))))))

(defun indent-using-tabs (buffer mark column)
  (with-buffer-globals (buffer)
    (.indent-using-tabs. mark column)))

(defmacro .indent-using-spaces. (mark column)
  "Inserts some spaces at MARK so that it moves to COLUMN.  This assumes
   mark is at the beginning of a line."
  (let ((mark-sym (gensym "INDENT-USING-SPACES_MARK"))
        (column-sym (gensym "INDENT-USING-SPACES_COLUMN")))
    `(let ((,mark-sym ,mark)
           (,column-sym ,column))
       (insert-string .buffer. ,mark-sym (make-string ,column-sym :initial-element #\space)))))

(defun indent-using-spaces (buffer mark column)
  (with-buffer-globals (buffer)
    (.indent-using-spaces. mark column)))

(defmacro .indent-to-column. (mark column)
  "Insert white space at MARK so that it moves to COLUMN.  This assumes mark is
   at the beginning of a line.  When 'indent-with-tabs is true it uses tabs a
   maximum number of tabs and a minimum number os spaces to move mark to column,
   otherwise spaces are used."
  (let ((mark-sym (gensym "INDENT-TO-COLUMN_MARK"))
        (column-sym (gensym "INDENT-TO-COLUMN_COLUMN")))
    `(let ((,mark-sym ,mark)
           (,column-sym ,column))
       (if (hi::value .buffer. indent-with-tabs)
           (indent-using-tabs .buffer. ,mark-sym ,column-sym)
           (indent-using-spaces .buffer. ,mark-sym ,column-sym)))))

(defun indent-to-column (buffer mark column)
  (with-buffer-globals (buffer)
    (.indent-to-column. mark column)))

(defmacro .indent-to-tab-stop. (mark)
  (let ((mark-sym (gensym "INDENT-TO-TAB-STOP_MARK")))
    `(let ((,mark-sym ,mark))
       (cond ((hi::value .buffer. indent-with-tabs)
              (hi::.insert-character. ,mark-sym #\tab))
             (t
              (let ((tab-size (hi::value .buffer. spaces-per-tab)))
                (.indent-using-spaces. ,mark-sym (* (ceiling (1+ (.mark-column. ,mark-sym))
                                                             tab-size)
                                                    tab-size))))))))

(defun indent-to-tab-stop (buffer mark)
  (with-buffer-globals (buffer)
    (.indent-to-tab-stop. mark)))

(defmacro .indent-region. (region)
  "Invokes function held by Hemlock variable \"Indent Function\" on every
   line of region."
  (let ((region-sym (gensym "INDENT-REGION_REGION")))
    `(let ((,region-sym ,region))
       (let* ((indent-function (hi::value .buffer. indent-function)))
         (with-mark ((start (region-start ,region-sym) :left-inserting)
                     (end (region-end ,region-sym)))
           (line-start start)
           (line-start end)
           (loop (when (mark= start end)
                   (funcall indent-function .buffer. start)
                   (return))
                 (funcall indent-function .buffer. start)
                 (line-offset start 1 0)))))))

(defun indent-region (buffer region)
  (with-buffer-globals (buffer)
    (.indent-region. region)))

(defmacro .indent-region-for-commands. (region)
  "Indents region undoably with INDENT-REGION."
  (let ((region-sym (gensym "INDENT-REGION_REGION")))
    `(let ((,region-sym ,region))
       (let* ((start (region-start ,region-sym))
              (end (region-end ,region-sym))
              (undo-region (hi::.copy-region. (region (line-start start) (line-end buffer end)))))
         (.indent-region. ,region-sym)
         (make-region-undo .buffer.
                           :twiddle "Indent"
                           (region (line-start (copy-mark start :left-inserting))
                                   (line-end buffer (copy-mark end :right-inserting)))
                           undo-region)))))

(defun indent-region-for-commands (buffer region)
  (with-buffer-globals (buffer)
    (.indent-region-for-commands. region)))

(defmacro .generic-indent. (mark)
  (let ((mark-sym (gensym "GENERIC-INDENT_MARK")))
    `(let* ((,mark-sym ,mark)
            (line (mark-line ,mark-sym))
            (prev (do ((line (line-previous line) (line-previous line)))
                      ((or (null line) (not (blank-line-p .buffer. line))) line))))
       (unless prev (editor-error))
       (line-start mark prev)
       (find-attribute mark :space #'zerop)
       (let ((indentation (mark-column .buffer. mark)))
         (line-start mark line)
         (delete-horizontal-space .buffer. mark)
         (indent-to-column .buffer. mark indentation)))))

(defun generic-indent (buffer mark)
  (with-buffer-globals (buffer)
    (.generic-indent. mark)))

#+MOVEDTOINSERT-NEWLINE-COMMAND
(defhcommand indent-new-line-command (window p)
  (:pretty-name "Indent New Line")
  (:documentation
   "Moves point to a new blank line and indents it.
   Any whitespace before point is deleted.  The value of \"Indent Function\"
   is used for indentation unless there is a Fill Prefix, in which case it is
   used.  Any argument is passed onto \"New Line\".")
  (:help-string
   "Moves point to a new blank line and indents it.
   Any whitespace before point is deleted.  The value of \"Indent Function\"
   is used for indentation unless there is a Fill Prefix, in which case it is
   used.  Any argument is passed onto \"New Line\"."))

#+NIL
(defmethod indent-new-line-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (let ((prefix (hi::value fill-prefix)))
      (delete-horizontal-space buffer point)
      (if (buffer-minor-mode buffer "Interactive-Character-Stream")
          (confirm-line-of-input-command window p)
          (progn
            (new-line-command window p)
            (if prefix
                (insert-string buffer point prefix)
                (funcall (hi::value indent-function) buffer point)))))
    (climish::redraw-buffer window)))


(defhcommand indent-command (window p)
  (:pretty-name "Indent")
  (:documentation
   "Invokes function held by the Hemlock variable \"Indent Function\",
   moving point past region if called with argument.")
  (:help-string
   "Invokes function held by the Hemlock variable \"Indent Function\"
   moving point past region if called with argument.")
  (:applicability nil))

(defmethod indent-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (with-mark ((mark point :left-inserting))
      (cond ((or (not p) (zerop p))
             (funcall (hi::value buffer indent-function) buffer mark))
            (t
             (if (plusp p)
                 (unless (line-offset buffer point (1- p))
                   (buffer-end point buffer))
                 (unless (line-offset buffer mark (1+ p))
                   (buffer-start mark buffer)))
             (indent-region-for-commands buffer (region mark point))
             (find-attribute buffer (line-start point) :whitespace #'zerop))))
    (climish::redraw-buffer window)))
  

(defhcommand indent-region-command (window p)
  (:pretty-name "Indent Region")
  (:documentation
   "Invokes function held by Hemlock variable \"Indent Function\" on every
   line between point and mark, inclusively.")
  (:help-string
   "Invokes function held by Hemlock variable \"Indent Function\" on every
   line between point and mark, inclusively."))

(defmethod indent-region-command (window p)
  (declare (ignore p))
  (when window
    (let ((buffer (window-buffer window)))
      (when buffer
        (let* ((region (current-region buffer)))
          (with-mark ((start (region-start region) :left-inserting)
                      (end (region-end region) :left-inserting))
            (indent-region-for-commands buffer (region start end))))))))



(defhcommand center-line-command (window p)
  (:pretty-name "Center Line")
  (:documentation "Centers current line using \"Fill Column\".  If an argument is supplied,
   it is used instead of the \"Fill Column\".")
  (:help-string
   "Centers current line using fill-column."))

(defmethod center-line-command (window p)
  (let ((buffer (window-buffer window)))
    (when buffer
      (let* ((point (buffer-point buffer))
             (region (if (region-active-p buffer)
                         (current-region buffer)
                         (region point point)))
             (end (region-end region)))
        (with-mark ((temp (region-start region) :left-inserting))
          (loop
            (when (mark> temp end) (return))
            (delete-horizontal-space buffer (line-end buffer temp))
            (delete-horizontal-space buffer (line-start temp))
            (let* ((len (line-length buffer (mark-line temp)))
                   (spaces (- (or p (hi::value buffer fill-column)) len)))
              (if (and (plusp spaces)
                       (not (zerop len)))
                  (indent-to-column buffer temp (ceiling spaces 2)))
              (unless (line-offset buffer temp 1) (return))
              (line-start temp))))))))


(defhcommand quote-tab-command (window p)
  (:pretty-name "Quote Tab")
  (:documentation "Insert tab character.")
  (:help-string "Insert tab character."))

(defmethod quote-tab-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (if (and p (> p 1))
        (insert-string buffer point (make-string p :initial-element #\tab))
        (insert-character buffer point #\tab))))


(defhcommand open-line-command (window p)
  (:pretty-name "Open Line")
  (:documentation "Inserts a newline into the buffer without moving the point.")
  (:help-string "Inserts a newline into the buffer without moving the point.
  With argument, inserts p newlines."))

(defmethod open-line-command (window p)
  (let ((buffer (window-buffer window)))
    (let ((point (buffer-point buffer))
          (count (if p p 1)))
      (if (not (minusp count))
          (dotimes (i count)
            (insert-character buffer point #\newline)
            (mark-before buffer point))
          (editor-error)))))


(defhcommand new-line-command (window p)
  (:pretty-name "New Line")
  (:documentation "Moves the point to a new blank line.
  A newline is inserted if the next two lines are not already blank.
  With an argument, repeats p times.")
  (:help-string "Moves the point to a new blank line."))

(defmethod new-line-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (let ((count (if p p 1)))
      (if (not (minusp count))
          (do* ((next (line-next (mark-line point))
                      (line-next (mark-line point)))
                (i 1 (1+ i)))
               ((> i count))
            (cond ((and (blank-after-p buffer point)
                        next (blank-line-p buffer next)
                        (let ((after (line-next next)))
                          (or (not after) (blank-line-p buffer after))))
                   (line-start point next)
                   (let ((len (line-length buffer next)))
                     (unless (zerop len)
                       (delete-characters buffer point len))))
                  (t
                   (insert-character buffer point #\newline))))
          (editor-error)))))

;; look in gui-application for space attributes

(defun delete-horizontal-space (buffer mark)
  "Deletes all :space characters on either side of mark."
  (with-mark ((start mark))
    (reverse-find-attribute buffer start :space #'zerop)
    (find-attribute buffer mark :space #'zerop)
    (delete-region buffer (region start mark))))

(defhcommand delete-indentation-command (window p)
  (:pretty-name "Delete Indentation")
  (:documentation 
   "Join current line with the previous one, deleting excess whitespace.
  All whitespace is replaced with a single space, unless it is at the beginning
  of a line, immmediately following a \"(\", or immediately preceding a \")\",
  in which case the whitespace is merely deleted.  If the preceeding character
  is a sentence terminator, two spaces are left instead of one.  If a prefix
  argument is given, the following line is joined with the current line.")
  (:help-string "Join current line with the previous one, deleting excess whitespace."))

(defmethod delete-indentation-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (with-mark ((m point :right-inserting))
      (when p (line-offset buffer m 1))
      (line-start m)
      (unless (delete-characters buffer m -1) (editor-error "No previous line."))
      (delete-horizontal-space buffer m)
      (let ((prev (previous-character buffer m)))
        (when (and prev (char/= prev #\newline))
          (cond ((not (zerop (character-attribute :sentence-terminator prev)))
                 (insert-string buffer m "  "))
                ((not (or (eq (character-attribute :lisp-syntax prev) :open-paren)
                          (eq (character-attribute :lisp-syntax (next-character buffer m))
                              :close-paren)))
                 (insert-character buffer m #\space))))))))


(defhcommand delete-horizontal-space-command (window p)
  (:pretty-name "Delete Horizontal Space")
  (:documentation "Delete spaces and tabs surrounding the point.")
  (:help-string "Delete spaces and tabs surrounding the point."))

(defmethod delete-horizontal-space-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (delete-horizontal-space buffer point)))

(defhcommand just-one-space-command (window p)
  (:pretty-name "Just One Space")
  (:documentation "Leave one space.
  Surrounding space is deleted, and then one space is inserted.
  with prefix argument insert that number of spaces.")
  (:help-string "Delete surrounding space and insert P spaces."))

(defmethod just-one-space-command (window p)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (delete-horizontal-space buffer point)
    (dotimes (i (or p 1)) (insert-character buffer point #\space))))

(defhcommand back-to-indentation-command (window p)
  (:pretty-name "Back to Indentation")
  (:documentation
   "Move point to the first non-whitespace character on the line.")
  (:help-string "Move point to the first non-whitespace character on the line."))

(defmethod back-to-indentation-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (line-start point)
    (find-attribute point :whitespace #'zerop)))

(defhcommand indent-rigidly-command (window p)
  (:pretty-name "Indent Rigidly")
  (:documentation
   "Indent the region rigidly by p spaces.
   Each line in the region is moved p spaces to the right (left if p is
   negative).  When moving a line to the left, tabs are converted to spaces.")
  (:help-string "Indent the region rigidly p spaces to the right (left if p is negative)."))

(defmethod indent-rigidly-command (window p)
  (let* ((buffer (window-buffer window)))
    (let ((p (or p (hi::value buffer spaces-per-tab)))
          (region (current-region buffer)))
      (with-mark ((mark1 (region-start region) :left-inserting)
                  (mark2 (region-end region) :left-inserting))
        (line-start mark1)
        (line-start mark2)
        (do ()
            ((mark= mark1 mark2))
          (cond ((empty-line-p buffer mark1))
                ((blank-after-p buffer mark1)
                 (delete-characters buffer mark1 (line-length buffer (mark-line mark1))))
                (t (find-attribute mark1 :whitespace #'zerop)
                   (let ((new-column (+ p (mark-column buffer mark1))))
                     (delete-characters buffer mark1 (- (mark-charpos mark1)))
                     (when (plusp new-column)
                       (indent-to-column buffer mark1 new-column)))))
          (line-offset mark1 1 0))))))
