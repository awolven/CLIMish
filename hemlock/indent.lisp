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
           (floor ,column-sym (hi::value spaces-per-tab))
         (dotimes (i tabs) (hi::.insert-character. ,mark-sym #\tab))
         (dotimes (i spaces) (hi::.insert-character. ,mark-sym #\space))))))

(defmacro .indent-using-spaces. (mark column)
  "Inserts some spaces at MARK so that it moves to COLUMN.  This assumes
   mark is at the beginning of a line."
  (let ((mark-sym (gensym "INDENT-USING-SPACES_MARK"))
        (column-sym (gensym "INDENT-USING-SPACES_COLUMN")))
    `(let ((,mark-sym ,mark)
           (,column-sym ,column))
       (hi::.insert-string. ,mark-sym (make-string ,column-sym :initial-element #\space)))))





(defmacro .indent-to-column. (mark column)
  "Insert white space at MARK so that it moves to COLUMN.  This assumes mark is
   at the beginning of a line.  When 'indent-with-tabs is true it uses tabs a
   maximum number of tabs and a minimum number os spaces to move mark to column,
   otherwise spaces are used."
  (let ((mark-sym (gensym "INDENT-TO-COLUMN_MARK"))
        (column-sym (gensym "INDENT-TO-COLUMN_COLUMN")))
    `(let ((,mark-sym ,mark)
           (,column-sym ,column))
       (if (hi::value indent-with-tabs)
           (.indent-using-tabs. ,mark-sym ,column-sym)
           (.indent-using-spaces. ,mark-sym ,column-sym)))))

(defmacro .indent-to-tab-stop. (mark)
  (let ((mark-sym (gensym "INDENT-TO-TAB-STOP_MARK")))
    `(let ((,mark-sym ,mark))
       (cond ((hi::value indent-with-tabs)
              (hi::.insert-character. ,mark-sym #\tab))
             (t
              (let ((tab-size (hi::value spaces-per-tab)))
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
       (let* ((indent-function (hi::value indent-function)))
         (with-mark ((start (region-start ,region-sym) :left-inserting)
                     (end (region-end ,region-sym)))
           (line-start start)
           (line-start end)
           (loop (when (mark= start end)
                   (funcall indent-function .buffer. start)
                   (return))
                 (funcall indent-function .buffer. start)
                 (line-offset start 1 0)))))))

(defmacro .indent-region-for-commands. (region)
  "Indents region undoably with INDENT-REGION."
  (let ((region-sym (gensym "INDENT-REGION_REGION")))
    `(let ((,region-sym ,region))
       (let* ((start (region-start ,region-sym))
              (end (region-end ,region-sym))
              (undo-region (hi::.copy-region. (region (line-start start) (line-end end)))))
         (.indent-region. ,region-sym)
         (make-region-undo :twiddle "Indent"
                           (region (line-start (copy-mark start :left-inserting))
                                   (line-end (copy-mark end :right-inserting)))
                           undo-region)))))




(defmacro .generic-indent. (mark)
  (let ((mark-sym (gensym "GENERIC-INDENT_MARK")))
    `(let* ((,mark-sym ,mark)
            (line (mark-line ,mark-sym))
            (prev (do ((line (line-previous line) (line-previous line)))
                      ((or (null line) (not (.blank-line-p. line))) line))))
       (unless prev (editor-error))
       (line-start mark prev)
       (find-attribute mark :space #'zerop)
       (let ((indentation (.mark-column. mark)))
         (line-start mark line)
         (.delete-horizontal-space. mark)
         (.indent-to-column. mark indentation)))))


(defcommand "Indent New Line" (p)
  "Moves point to a new blank line and indents it.
   Any whitespace before point is deleted.  The value of \"Indent Function\"
   is used for indentation unless there is a Fill Prefix, in which case it is
   used.  Any argument is passed onto \"New Line\"."
  "Moves point to a new blank line and indents it.
   Any whitespace before point is deleted.  The value of \"Indent Function\"
   is used for indentation unless there is a Fill Prefix, in which case it is
   used.  Any argument is passed onto \"New Line\"."
  (with-buffer-globals ((window-buffer (current-window)))
    (let ((prefix (hi::value fill-prefix)))
      (.delete-horizontal-space. .point.)
      (new-line-command p)
      (if prefix
          (hi::.insert-string. .point. prefix)
          (funcall (hi::value indent-function) .buffer. .point.)))))


(defcommand "Indent" (p)
  "Invokes function held by the Hemlock variable \"Indent Function\",
   moving point past region if called with argument."
  "Invokes function held by the Hemlock variable \"Indent Function\"
   moving point past region if called with argument."
  (with-buffer-globals ((window-buffer (current-window)))
    (with-mark ((mark .point. :left-inserting))
      (cond ((or (not p) (zerop p))
             (funcall (hi::value indent-function) .buffer. mark))
            (t
             (if (plusp p)
                 (unless (line-offset .point. (1- p))
                   (buffer-end .point.))
                 (unless (line-offset mark (1+ p))
                   (buffer-start mark)))
             (.indent-region-for-commands. (region mark .point.))
             (find-attribute (line-start .point.) :whitespace #'zerop))))))

(defcommand "Indent Region" (p)
  "Invokes function held by Hemlock variable \"Indent Function\" on every
   line between point and mark, inclusively."
  "Invokes function held by Hemlock variable \"Indent Function\" on every
   line between point and mark, inclusively."
  (declare (ignore p))
  (let ((window (current-window)))
    (when window
      (let ((buffer (window-buffer window)))
        (when buffer
          (with-buffer-globals (buffer)
            (let* ((region (current-region .buffer.)))
              (with-mark ((start (region-start region) :left-inserting)
                          (end (region-end region) :left-inserting))
                (.indent-region-for-commands. (region start end))))))))))



(defcommand "Center Line" (p)
  "Centers current line using \"Fill Column\".  If an argument is supplied,
   it is used instead of the \"Fill Column\"."
    "Centers current line using fill-column."
  (with-buffer-globals (.buffer.)
    (let* ((region (if (region-active-p .buffer.)
                       (current-region .buffer.)
                       (region .point. .point.)))
           (end (region-end region)))
      (with-mark ((temp (region-start region) :left-inserting))
        (loop
          (when (mark> temp end) (return))
          (.delete-horizontal-space. (line-end temp))
          (.delete-horizontal-space. (line-start temp))
          (let* ((len (line-length (mark-line temp)))
                 (spaces (- (or p (hi::value fill-column)) len)))
            (if (and (plusp spaces)
                     (not (zerop len)))
                (.indent-to-column. temp (ceiling spaces 2)))
            (unless (line-offset temp 1) (return))
            (line-start temp)))))))


(defcommand "Quote Tab" (p)
  "Insert tab character."
    "Insert tab character."
  (with-buffer-globals ((window-buffer (current-window)))
    (if (and p (> p 1))
        (hi::.insert-string. .point. (make-string p :initial-element #\tab))
        (hi::.insert-character. .point. #\tab))))


(defcommand "Open Line" (p)
  "Inserts a newline into the buffer without moving the point."
  "Inserts a newline into the buffer without moving the point.
  With argument, inserts p newlines."
  (with-buffer-globals ((window-buffer (current-window)))
    (let ((point .point.)
          (count (if p p 1)))
      (if (not (minusp count))
          (dotimes (i count)
            (hi::.insert-character. point #\newline)
            (mark-before point))
          (editor-error)))))


(defcommand "New Line" (p)
  "Moves the point to a new blank line.
  A newline is inserted if the next two lines are not already blank.
  With an argument, repeats p times."
    "Moves the point to a new blank line."
  (with-buffer-globals ((window-buffer (current-window)))
    (let ((point .point.)
          (count (if p p 1)))
      (if (not (minusp count))
          (do* ((next (line-next (mark-line point))
                      (line-next (mark-line point)))
                (i 1 (1+ i)))
               ((> i count))
            (cond ((and (blank-after-p point)
                        next (blank-line-p next)
                        (let ((after (line-next next)))
                          (or (not after) (blank-line-p after))))
                   (line-start point next)
                   (let ((len (line-length next)))
                     (unless (zerop len)
                       (hi::.delete-characters. point len))))
                  (t
                   (hi::.insert-character. point #\newline))))
          (editor-error)))))








(defcommand "Delete Indentation" (p)
  "Join current line with the previous one, deleting excess whitespace.
  All whitespace is replaced with a single space, unless it is at the beginning
  of a line, immmediately following a \"(\", or immediately preceding a \")\",
  in which case the whitespace is merely deleted.  If the preceeding character
  is a sentence terminator, two spaces are left instead of one.  If a prefix
  argument is given, the following line is joined with the current line."
    "Join current line with the previous one, deleting excess whitespace."
  (with-buffer-globals ((window-buffer (current-window)))
    (with-mark ((m .point. :right-inserting))
      (when p (line-offset m 1))
      (line-start m)
      (unless (hi::.delete-characters. m -1) (editor-error "No previous line."))
      (.delete-horizontal-space. m)
      (let ((prev (previous-character m)))
        (when (and prev (char/= prev #\newline))
          (cond ((not (zerop (character-attribute :sentence-terminator prev)))
                 (hi::.insert-string. m "  "))
                ((not (or (eq (character-attribute :lisp-syntax prev) :open-paren)
                          (eq (character-attribute :lisp-syntax (next-character m))
                              :close-paren)))
                 (hi::.insert-character. m #\space))))))))


(defcommand "Delete Horizontal Space" (p)
  "Delete spaces and tabs surrounding the point."
  "Delete spaces and tabs surrounding the point."
  (declare (ignore p))
  (with-buffer-globals ((window-buffer (current-window)))
    (.delete-horizontal-space. .point.)))

(defcommand "Just One Space" (p)
    "Leave one space.
  Surrounding space is deleted, and then one space is inserted.
  with prefix argument insert that number of spaces."
    "Delete surrounding space and insert P spaces."
  (with-buffer-globals ((window-buffer (current-window)))
    (let ((point .point.))
      (.delete-horizontal-space. .point.)
      (dotimes (i (or p 1)) (hi::.insert-character. point #\space)))))

(defcommand "Back to Indentation" (p)
  "Move point to the first non-whitespace character on the line."
  "Move point to the first non-whitespace character on the line."
  (declare (ignore p))
  (with-buffer-globals ((window-buffer (current-window)))
    (let ((point .point.))
      (line-start point)
      (find-attribute point :whitespace #'zerop))))

(defcommand "Indent Rigidly" (p)
  "Indent the region rigidly by p spaces.
   Each line in the region is moved p spaces to the right (left if p is
   negative).  When moving a line to the left, tabs are converted to spaces."
    "Indent the region rigidly p spaces to the right (left if p is negative)."
  (with-buffer-globals ((window-buffer (current-window)))
    (let ((p (or p (hi::value spaces-per-tab)))
          (region (current-region .buffer.)))
      (with-mark ((mark1 (region-start region) :left-inserting)
                  (mark2 (region-end region) :left-inserting))
        (line-start mark1)
        (line-start mark2)
        (do ()
            ((mark= mark1 mark2))
          (cond ((empty-line-p mark1))
                ((blank-after-p mark1)
                 (hi::.delete-characters. mark1 (line-length (mark-line mark1))))
                (t (find-attribute mark1 :whitespace #'zerop)
                   (let ((new-column (+ p (.mark-column. mark1))))
                     (hi::.delete-characters. mark1 (- (mark-charpos mark1)))
                     (when (plusp new-column)
                       (.indent-to-column. mark1 new-column)))))
          (line-offset mark1 1 0))))))
