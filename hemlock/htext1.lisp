;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock Text-Manipulation functions.
;;; Written by Skef Wholey.
;;;
;;; The code in this file implements the functions in the "Representation
;;; of Text," "Buffers," and "Predicates" chapters of the Hemlock design
;;; document.
;;;

(in-package :hemlock-internals)

(defvar .buffer.)


(defun invoke-with-buffer-globals (buffer thunk)
  (bt:with-recursive-lock-held ((hi::buffer-lock buffer))
    (funcall thunk)))


(defmacro with-buffer-globals ((buffer) &body body)
  (let ((buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer))
       (invoke-with-buffer-globals
        ,buffer-sym
        (lambda ()
          (let* ((.buffer. ,buffer-sym))
            (with-slots (.point.
                         *internal-temp-region*
                         *internal-temp-mark*
                         last-was-undo-p
                         this-is-undo-p
                         undoing-undo-list
                         open-line
                         open-chars
                         line-cache-length
                         right-open-pos
                         left-open-pos) .buffer.
              ,@body)))))))

;;;; Representation of Text:

;;; Line cache mechanism.
;;;
;;; The "open line" is used when inserting and deleting characters from a line.
;;; It acts as a cache that provides a more flexible (but more expensive)
;;; representation of the line for multiple insertions and deletions.  When a
;;; line is open, it is represented as a vector of characters and two indices:
;;;
;;; +-----------------------------------------------------------+
;;; | F | O | O |   | B | x | x | x | x | x | x | x | x | A | R |
;;; +-----------------------------------------------------------+
;;;                       ^                               ^
;;;                   Left Pointer                   Right Pointer
;;;
;;; The open line is represented by 4 special variables:
;;;     Open-Line: the line object that is opened
;;;     Open-Chars: the vector of cached characters
;;;     Left-Open-Pos: index of first free character in the gap
;;;     Right-Open-Pos: index of first used character after the gap
;;;
;;; Note:
;;;    Any modificiation of the line cache must be protected by
;;; Without-Interrupts.  This is done automatically by modifying-buffer; other
;;; users beware.

;;(defvar line-cache-length 200
;;  "Length of Open-Chars.")

;;(defvar open-line ()
;;  "Line open for hacking on.")

;;(defvar open-chars  (make-string line-cache-length)
;;  "Vector of characters for hacking on.")

;;(defvar left-open-pos 0
;;  "Index to first free character to left of mark in Open-Chars.")

;;(defvar right-open-pos 0
;;  "Index to first used character to right of mark in Open-Chars.")

(defmacro .grow-open-chars. (&optional (new-length nil))
  "Grows Open-Chars to twice its current length, or the New-Length if
  specified."
  (let ((new-length-sym (gensym "GROW-OPEN-CHARS_NEW-LENGTH")))
    `(let ((,new-length-sym (or ,new-length (* line-cache-length 2))))
       (let ((new-chars (make-string ,new-length-sym))
             (new-right (- ,new-length-sym (- line-cache-length right-open-pos))))
         (%sp-byte-blt open-chars 0 new-chars 0 left-open-pos)
         (%sp-byte-blt open-chars right-open-pos new-chars new-right ,new-length-sym)
         (setq right-open-pos new-right)
         (setq open-chars new-chars)
         (setq line-cache-length ,new-length-sym)))))

(defun grow-open-chars (buffer &optional (new-length nil))
  (with-buffer-globals (buffer)
    (.grow-open-chars. new-length)))

(defmacro .close-line. ()
  "Stuffs the characters in the currently open line back into the line they
  came from, and sets open-line to Nil."
  `(when open-line
     (hemlock-ext:without-interrupts
       (let* ((length (+ left-open-pos (- line-cache-length right-open-pos)))
              (string (make-string length)))
         (%sp-byte-blt open-chars 0 string 0 left-open-pos)
         (%sp-byte-blt open-chars right-open-pos string left-open-pos length)
         (setf (line-chars open-line) string)
         (setf open-line nil)))))

(defun close-line (buffer)
  (with-buffer-globals (buffer)
    (.close-line.)))

;;; We stick decrementing fixnums in the line-chars slot of the open line
;;; so that whenever the cache is changed the chars are no longer eq.
;;; They decrement so that they will be distinct from positive fixnums,
;;; which might mean something else.
;;;
(defvar *cache-modification-tick* -1
  "The counter for the fixnums we stick in the chars of the cached line.")

(defmacro .open-line. (line mark)
  "Closes the current Open-Line and opens the given Line at the Mark.
  Don't call this, use modifying-line instead."
  (let ((mark-sym (gensym "OPEN-LINE_MARK"))
        (line-sym (gensym "OPEN-LINE_LINE")))
    `(let ((,mark-sym ,mark)
           (,line-sym ,line))
       (cond ((eq ,line-sym open-line)
              (let ((charpos (mark-charpos ,mark-sym)))
                (cond ((< charpos left-open-pos) ; BLT 'em right!
                       (let ((right-start (- right-open-pos
                                             (- left-open-pos charpos))))
                         (%sp-byte-blt open-chars charpos
                                       open-chars right-start
                                       right-open-pos)
                         (setq left-open-pos charpos)
                         (setq right-open-pos right-start)))
                      ((> charpos left-open-pos) ; BLT 'em left!
                       (%sp-byte-blt open-chars right-open-pos
                                     open-chars left-open-pos
                                     charpos)
                       (setq right-open-pos
                             (+ right-open-pos (- charpos left-open-pos)))
                       (setq left-open-pos charpos)))))

             (t
              (.close-line.)
              (let* ((chars (line-chars ,line-sym))
                     (len (length chars)))
                (declare (simple-string chars))
                (when (> len line-cache-length)
                  (setq line-cache-length (* len 2))
                  (setq open-chars (make-string line-cache-length)))
                (setq open-line ,line-sym)
                (setq left-open-pos (mark-charpos ,mark-sym))
                (setq right-open-pos
                      (- line-cache-length (- (length chars) left-open-pos)))
                (%sp-byte-blt chars 0 open-chars 0 left-open-pos)
                (%sp-byte-blt chars left-open-pos open-chars right-open-pos
                              line-cache-length)))))))

(defun open-line (buffer line mark)
  (with-buffer-globals (buffer)
    (.open-line. line mark)))


;;;; Some macros for Text hacking:


(defmacro .modifying-line. (line mark)
  "Checks to see if the Line is already opened at the Mark, and calls Open-Line
  if not.  Sticks a tick in the open-line's chars.  This must be called within
  the body of a Modifying-Buffer form."
  `(progn
     (unless (and (= (mark-charpos ,mark) left-open-pos) (eq ,line open-line))
       (.open-line. ,line ,mark))
     (setf (line-chars open-line) (decf *cache-modification-tick*))))

(defun modifying-line (buffer line mark)
  (with-buffer-globals (buffer)
    (.modifying-line. line mark)))

;;; Now-Tick tells us when now is and isn't.
;;;
(defvar now-tick 0 "Current tick.")

(defmacro tick ()
  "Increments the ``now'' tick."
  `(incf now-tick))


;;; Yeah, the following is kind of obscure, but at least it doesn't
;;; call Bufferp twice.  The without-interrupts is just to prevent
;;; people from being screwed by interrupting when the buffer structure
;;; is in an inconsistent state.
;;;
(defun invoke-modifying-buffer (fun buffer)
  "Does groovy stuff for modifying buffers."
  (when (bufferp buffer)
    (unless (buffer-writable buffer)
      (editor-error "Buffer ~S is read only." (buffer-name buffer)))
    (let ((buffer-lock (buffer-lock buffer)))
      (bt:with-recursive-lock-held (buffer-lock)
        (when (< (buffer-modified-tick buffer)
                 (buffer-unmodified-tick buffer))
          (invoke-hook hemlock::buffer-modified-hook buffer t))
        (setf (buffer-modified-tick buffer) (tick))
        ;; FIXME: what is hemlock-ext:without-interrupts for?
        (hemlock-ext:without-interrupts (funcall fun))))))

(defmacro modifying-buffer (buffer &body forms)
  "Does groovy stuff for modifying buffers."
  (let ((buffer-sym (gensym "MODIFYING-BUFFER_BUFFER")))
    `(let ((,buffer-sym ,buffer))
       (invoke-modifying-buffer (lambda () ,@forms) ,buffer-sym))))

(defmacro always-change-line (mark new-line)
  (let ((mark-sym (gensym "ALWAYS-CHANGE-LINE_MARK"))
        (new-line-sym (gensym "ALWAYS-CHANGE-LINE_NEW-LINE")))
    `(let* ((,mark-sym ,mark)
            (,new-line-sym ,new-line)
            (old-line (mark-line ,mark-sym)))
       (when ,new-line-sym
         (when old-line
           (when (not (eq (mark-%kind ,mark-sym) :temporary))
             (do ((scan (line-marks old-line) (cdr scan))
                  (prev () scan))
                 ((eq (car scan) ,mark-sym)
                  (if prev
                      (setf (cdr prev) (cdr scan))
                      (setf (line-marks old-line) (cdr scan)))
                  (setf (cdr scan) (line-marks ,new-line-sym)
                        (line-marks ,new-line-sym) scan)))))
         (setf (mark-line ,mark-sym) ,new-line-sym)))))


(defmacro change-line (mark new-line)
  (let ((mark-sym (gensym "CHANGE-LINE_MARK"))
        (new-line-sym (gensym "CHANGE-LINE_NEW-LINE")))
    `(let* ((,mark-sym ,mark)
            (,new-line-sym ,new-line)
            (old-line (mark-line ,mark-sym)))
       (when ,new-line-sym
         (when old-line
           (unless (or (eq (mark-%kind ,mark-sym) :temporary)
                       (eq old-line ,new-line-sym))
             (do ((scan (line-marks old-line) (cdr scan))
                  (prev () scan))
                 ((eq (car scan) ,mark-sym)
                  (if prev
                      (setf (cdr prev) (cdr scan))
                      (setf (line-marks old-line) (cdr scan)))
                  (setf (cdr scan) (line-marks ,new-line-sym)
                        (line-marks ,new-line-sym) scan)))))
         (setf (mark-line ,mark-sym) ,new-line-sym)))))


;;; MOVE-SOME-MARKS  --  Internal
;;;
;;;    Move all the marks from the line Old to New, performing some
;;; function on their charpos'es.  Charpos is bound to the charpos of
;;; the mark, and the result of the evaluation of the last form in
;;; the body should be the new charpos for the mark.  If New is
;;; not supplied then the marks are left on the old line.
;;;
(defmacro move-some-marks ((charpos old &optional new) &body body)
  (let ((last (gensym "MOVE-SOME-MARKS_LAST"))
        (mark (gensym "MOVE-SOME-MARKS_MARK"))
        (marks (gensym "MOVE-SOME-MARKS_MARK")))
    (if new
        `(let ((,marks (line-marks ,old)))
           (do ((,mark ,marks (cdr ,mark))
                (,last nil ,mark))
               ((null ,mark)
                (when ,last
                  (shiftf (cdr ,last) (line-marks ,new) ,marks))
                (setf (line-marks ,old) nil))
             (setf (mark-line (car ,mark)) ,new)
             (setf (mark-charpos (car ,mark))
                   (let ((,charpos (mark-charpos (car ,mark))))
                     ,@body))))
        `(dolist (,mark (line-marks ,old))
           (setf (mark-charpos ,mark)
                 (let ((,charpos (mark-charpos ,mark)))
                   ,@body))))))

;;; Maybe-Move-Some-Marks  --  Internal
;;;
;;;    Like Move-Some-Marks, but only moves the mark if the
;;; charpos is greater than the bound, OR the charpos equals the bound
;;; and the marks %kind is :left-inserting.
;;;
(defmacro maybe-move-some-marks ((charpos old &optional new) bound &body body)
  (let ((mark (gensym)) (marks (gensym)) (prev (gensym)))
    (if new
        `(do ((,mark (line-marks ,old))
              (,marks (line-marks ,new))
              (,prev ()))
             ((null ,mark)
              (setf (line-marks ,new) ,marks))
           (let ((,charpos (mark-charpos (car ,mark))))
             (cond
               ((or (> ,charpos ,bound)
                    (and (= ,charpos ,bound)
                         (eq (mark-%kind (car ,mark)) :left-inserting)))
                (setf (mark-line (car ,mark)) ,new)
                (setf (mark-charpos (car ,mark)) (progn ,@body))
                (if ,prev
                    (setf (cdr ,prev) (cdr ,mark))
                    (setf (line-marks ,old) (cdr ,mark)))
                (rotatef (cdr ,mark) ,marks ,mark))
               (t
                (setq ,prev ,mark  ,mark (cdr ,mark))))))
        `(dolist (,mark (line-marks ,old))
           (let ((,charpos (mark-charpos ,mark)))
             (when (or (> ,charpos ,bound)
                       (and (= ,charpos ,bound)
                            (eq (mark-%kind ,mark) :left-inserting)))
               (setf (mark-charpos ,mark) (progn ,@body))))))))


;;; Maybe-Move-Some-Marks*  --  Internal
;;;
;;;    Like Maybe-Move-Some-Marks, but ignores the mark %kind.
;;;
(defmacro maybe-move-some-marks* ((charpos old &optional new) bound &body body)
  (let ((mark (gensym)) (marks (gensym)) (prev (gensym)))
    (if new
        `(do ((,mark (line-marks ,old))
              (,marks (line-marks ,new))
              (,prev ()))
             ((null ,mark)
              (setf (line-marks ,new) ,marks))
           (let ((,charpos (mark-charpos (car ,mark))))
             (cond
               ((> ,charpos ,bound)
                (setf (mark-line (car ,mark)) ,new)
                (setf (mark-charpos (car ,mark)) (progn ,@body))
                (if ,prev
                    (setf (cdr ,prev) (cdr ,mark))
                    (setf (line-marks ,old) (cdr ,mark)))
                (rotatef (cdr ,mark) ,marks ,mark))
               (t
                (setq ,prev ,mark  ,mark (cdr ,mark))))))
        `(dolist (,mark (line-marks ,old))
           (let ((,charpos (mark-charpos ,mark)))
             (when (> ,charpos ,bound)
               (setf (mark-charpos ,mark) (progn ,@body))))))))

;;;; Lines.

(defun line-length (buffer line)
  "Returns the number of characters on the line."
  (if (linep line)
      (with-buffer-globals (buffer)
        (line-length* line))
      (error "~S is not a line!" line)))

(defun line-buffer (line)
  "Returns the buffer with which the Line is associated.  If the line is
  not in any buffer then Nil is returned."
  (let ((buffer (line-%buffer line)))
    (if (bufferp buffer) buffer)))

(defmacro .line-string. (line)
  "Returns the characters in the line as a string.  The resulting string
  must not be destructively modified.  This may be set with Setf."
  (let ((line-sym (gensym "LINE-STRING_LINE")))
    `(let ((,line-sym ,line))
       (if (eq ,line-sym open-line)
           (.close-line.)
           (line-chars ,line-sym)))))

(defun line-string (buffer line)
  (with-buffer-globals (buffer)
    (.line-string. line)))

(defmacro .set-line-string. (string line)
  "Replace the contents of a line."
  (let ((string-sym (gensym "SET-LINE-STRING_STRING"))
        (line-sym (gensym "SET-LINE-STRING_LINE")))
    `(let* ((,string-sym ,string)
            (,line-sym ,line)
            (buffer (line-%buffer ,line-sym)))
       (modifying-buffer buffer
         (unless (simple-string-p ,string-sym)
           (setq ,string-sym (coerce ,string-sym 'simple-string)))
         (when (eq ,line-sym open-line) (setq open-line nil))
         (let ((length (length (the simple-string string))))
           (dolist (m (line-marks ,line-sym))
             (if (eq (mark-%kind m) :left-inserting)
                 (setf (mark-charpos m) length)
                 (setf (mark-charpos m) 0))))
         (setf (line-chars ,line-sym) ,string-sym)))))

(defun (setf line-string) (string line buffer)
  (with-buffer-globals (buffer)
    (.set-line-string. string line)))


(defmacro .line-character. (line index)
  "Return the Index'th character in Line.  If the index is the length of the
  line then #\newline is returned."
  (let ((line-sym (gensym "LINE-CHARACTER_LINE"))
        (index-sym (gensym "LINE-CHARACTER_INDEX")))
    `(let ((,line-sym ,line)
           (,index-sym ,index))
       (if (eq ,line-sym open-line)
           (if (< ,index-sym left-open-pos)
               (schar open-chars ,index-sym)
               (let ((index (+ ,index-sym (- right-open-pos left-open-pos))))
                 (if (= index line-cache-length)
                     #\newline
                     (schar open-chars index))))
           (let ((chars (line-chars ,line-sym)))
             (declare (simple-string chars))
             (if (= ,index-sym (length chars))
                 #\newline
                 (schar chars ,index-sym)))))))

(defun line-character (buffer line index)
  (with-buffer-globals (buffer)
    (.line-character. line index)))




;;;; Marks.

(defun mark (line charpos &optional (kind :temporary))
  "Returns a mark to the Charpos'th character of the Line.  Kind is the
  kind of mark to make, one of :temporary (the default), :left-inserting
  or :right-inserting."
  (let ((mark (internal-make-mark line charpos kind)))
    (if (not (eq kind :temporary))
        (push mark (line-marks line)))
    mark))

(defun mark-kind (mark)
  "Returns the kind of the given Mark, :Temporary, :Left-Inserting, or
  :Right-Inserting.  This may be set with Setf."
  (mark-%kind mark))

(defun (setf mark-kind) (kind mark)
  "Used to set the kind of a mark."
  (let ((line (mark-line mark)))
    (cond ((eq kind :temporary)
           (setf (line-marks line) (delq mark (line-marks line)))
           (setf (mark-%kind mark) kind))
          ((or (eq kind :left-inserting) (eq kind :right-inserting))
           (if (not (member mark (line-marks line)))
               (push mark (line-marks line)))
           (setf (mark-%kind mark) kind))
          (t
           (error "~S is an invalid mark type." kind)))))

(defun copy-mark (mark &optional (kind (mark-%kind mark)))
  "Returns a new mark pointing to the same position as Mark.  The kind
  of mark created may be specified by Kind, which defaults to the
  kind of the copied mark."
  (let ((mark (internal-make-mark (mark-line mark) (mark-charpos mark) kind)))
    (if (not (eq kind :temporary))
        (push mark (line-marks (mark-line mark))))
    mark))

(defun delete-mark (mark)
  "Deletes the Mark.  This should be done to any mark that may not be
  temporary which is no longer needed."
  (if (not (eq (mark-%kind mark) :temporary))
      (let ((line (mark-line mark)))
        (when line
          (setf (line-marks line) (delq mark (line-marks line))))
        nil))
  (setf (mark-line mark) nil))

(defun move-to-position (mark charpos &optional (line (mark-line mark)))
  "Changes the Mark to point to the given character position on the Line,
  which defaults to the line the mark is currently on."
  (change-line mark line)
  (setf (mark-charpos mark) charpos)
  mark)


;;;; Regions.

(defun region (start end)
  "Returns a region constructed from the marks Start and End."
  (let ((l1 (mark-line start))
        (l2 (mark-line end)))
    (unless (eq (line-%buffer l1) (line-%buffer l2))
      (error "Can't make a region with lines of different buffers."))
    (unless (if (eq l1 l2)
                (<= (mark-charpos start) (mark-charpos end))
                (< (line-number l1) (line-number l2)))
      (error "Start ~S is after end ~S." start end)))
  (internal-make-region start end))

;;; The *Disembodied-Buffer-Counter* exists to give that are not in any buffer
;;; unique buffer slots.

(defvar *disembodied-buffer-counter* 0
  "``Buffer'' given to lines in regions not in any buffer.")

(defclass disembodied-buffer ()
  ((buffer-lock :reader hi::buffer-lock :initform (bt:make-recursive-lock))
   (id :initarg :id)
   ;; "globals"
   (open-line :initform nil)
   (open-chars :initform (make-string 200))
   (left-open-pos :initform 0)
   (right-open-pos :initform 0)
   (line-cache-length :accessor buffer-line-cache-length :initform 200)

   (*internal-temp-region* :initform (let ((line (make-line :chars "" :number 0)))
                                       (internal-make-region (mark line 0 :right-inserting)
                                                             (mark line 0 :left-inserting))))
   (*internal-temp-mark* :initform (internal-make-mark nil nil :temporary))

   (last-was-undo-p :initform nil)
   (this-is-undo-p :initform nil)
   (undoing-undo-list :initform nil)))

(defmethod buffer-%writable ((thing disembodied-buffer))
  ;; i had to make this function, not sure why, maybe something was awry at the time
  nil)

(defmethod buffer-%name ((thing disembodied-buffer))
  ;; i had to make this function, not sure why, maybe something was awry at the time
  (format nil "disembodied ~S" (slot-value thing 'id)))


(defun make-empty-region ()
  "Returns a region with start and end marks pointing to the start of one empty
  line.  The start mark is right-inserting and the end mark is left-inserting."
  (let* ((line (make-line :chars ""  :number 0
                          :%buffer (make-instance 'disembodied-buffer
                                                  :id (incf *disembodied-buffer-counter*))))
         (start (mark line 0 :right-inserting))
         (end (mark line 0 :left-inserting)))
    (internal-make-region start end)))

;;; Line-Increment is the default difference for line numbers when we don't
;;; know any better.

(defconstant line-increment 256 "Default difference for line numbers.")

;;; Renumber-Region is used internally to keep line numbers in ascending order.
;;; The lines in the region are numbered starting with the given Start value
;;; by increments of the given Step value.  It returns the region.

(defun renumber-region (region &optional (start 0) (step line-increment))
  (do ((line (mark-line (region-start region)) (line-next line))
       (last-line (mark-line (region-end region)))
       (number start (+ number step)))
      ((eq line last-line)
       (setf (line-number line) number)
       region)
    (setf (line-number line) number))
  region)

;;; Renumber-Region-Containing renumbers the region containing the given line.

(defun renumber-region-containing (line)
  (cond ((line-buffer line)
         (renumber-region (buffer-region (line-%buffer line))))
        (t
         (do ((line line (line-previous line))
              (number 0 (- number line-increment)))
             ((null line))
           (setf (line-number line) number))
         (do ((line (line-next line) (line-next line))
              (number line-increment (+ number line-increment)))
             ((null line))
           (setf (line-number line) number)))))


;;; Number-Line numbers a newly created line.  The line has to have a previous
;;; line.
(defun number-line (line)
  (let ((prev (line-number (line-previous line)))
        (next (line-next line)))
    (if (null next)
        (setf (line-number line) (+ prev line-increment))
        (let ((new (+ prev (truncate (- (line-number next) prev) 2))))
          (if (= new prev)
              (renumber-region-containing line)
              (setf (line-number line) new))))))



;;;; Buffers.

;;; BUFFER-SIGNATURE is the exported interface to the internal function,
;;; BUFFER-MODIFIED-TICK
;;;
(defun buffer-signature (buffer)
  "Returns an arbitrary number which reflects the buffers current
  \"signature.\" The value returned by buffer-signature is guaranteed
  to be eql to the value returned by a previous call of buffer-signature
  iff the buffer has not been modified between the calls."
  (unless (bufferp buffer)
    (error "~S is not a buffer." buffer))
  (buffer-modified-tick buffer))



;;;; Predicates:


(defun start-line-p (mark)
  "Returns T if the Mark points before the first character in a line, Nil
  otherwise."
  (= (mark-charpos mark) 0))

(defun end-line-p (buffer mark)
  "Returns T if the Mark points after the last character in a line, Nil
  otherwise."
  (= (mark-charpos mark) (line-length buffer (mark-line mark))))

(defmacro .empty-line-p. (mark)
  "Returns T if the line pointer to by Mark contains no characters, Nil
  or otherwise."
  (let ((mark-sym (gensym "EMPTY-LINE_P_MARK")))
    `(let* ((,mark-sym ,mark)
            (line (mark-line ,mark-sym)))
       (if (eq line open-line)
           (and (= left-open-pos 0) (= right-open-pos line-cache-length))
           (= (length (line-chars line)) 0)))))

(defun empty-line-p (buffer mark)
  (with-buffer-globals (buffer)
    (.empty-line-p. mark)))



(eval-when (:compile-toplevel :execute)
(defmacro check-range (chars start end)
  `(do ((i ,start (1+ i)))
       ((= i ,end) t)
     (when (zerop (character-attribute :whitespace (schar ,chars i)))
       (return nil)))))
;;;
(defmacro .blank-between-positions. (line start end)
  (let ((line-sym (gensym "BLANK-BETWEEN-POSITIONS_LINE"))
        (start-sym (gensym "BLANK-BETWEEN-POSITIONS_START"))
        (end-sym (gensym "BLANK-BETWEEN-POSITIONS_END")))
    `(let ((,line-sym ,line)
           (,start-sym ,start)
           (,end-sym ,end))
       (if (eq ,line-sym open-line)
           (let ((gap (- right-open-pos left-open-pos)))
             (cond ((>= ,start-sym left-open-pos)
                    (check-range open-chars (+ ,start-sym gap) (+ ,end-sym gap)))
                   ((<= ,end-sym left-open-pos)
                    (check-range open-chars ,start-sym ,end-sym))
                   (t
                    (and (check-range open-chars ,start-sym left-open-pos)
                         (check-range open-chars right-open-pos (+ ,end-sym gap))))))
           (let ((chars (line-chars ,line-sym)))
             (check-range chars ,start-sym ,end-sym))))))

;;; blank-between-positions  --  Internal
;;;
;;;    Check if a line is blank between two positions.  Used by blank-XXX-p.
;;;

(defun blank-between-positions (buffer line start end)
  (with-buffer-globals (buffer)
    (.blank-between-positions. line start end)))

(defmacro .blank-line-p. (line)
  (let ((line-sym (gensym "BLANK-LINE-P_LINE")))
    `(let ((,line-sym ,line))
       (.blank-between-positions. ,line-sym 0 (line-length .buffer. ,line-sym)))))

(defun blank-line-p (buffer line)
  "True if line contains only characters with a :whitespace attribute of 1."
  (with-buffer-globals (buffer)
    (.blank-line-p. line)))



(defmacro .blank-before-p. (mark)
  "True is all of the characters before Mark on the line it is on have a
  :whitespace attribute of 1."
  (let ((mark-sym (gensym "BLANK-BEFORE-P_MARK")))
    `(let ((,mark-sym ,mark))
       (.blank-between-positions. (mark-line ,mark-sym) 0 (mark-charpos ,mark-sym)))))

(defun blank-before-p (buffer mark)
  (with-buffer-globals (buffer)
    (.blank-before-p. mark)))

(defmacro .blank-after-p. (mark)
  "True if all characters on the part part of the line after Mark have
  a :whitespace attribute of 1."
  (let ((mark-sym (gensym "BLANK-AFTER-P_MARK")))
    `(let* ((,mark-sym ,mark)
            (line (mark-line ,mark-sym)))
       (.blank-between-positions. line (mark-charpos ,mark-sym)
                                  (line-length .buffer. line)))))

(defun blank-after-p (buffer mark)
  (with-buffer-globals (buffer)
    (.blank-after-p. mark)))

(defun same-line-p (mark1 mark2)
  "Returns T if Mark1 and Mark2 point to the same line, Nil otherwise."
  (eq (mark-line mark1) (mark-line mark2)))

(defun mark< (mark1 mark2)
  "Returns T if Mark1 points to a character before Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (< (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (< (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark<= (mark1 mark2)
  "Returns T if Mark1 points to a character at or before Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (< (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (<= (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark> (mark1 mark2)
  "Returns T if Mark1 points to a character after Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (> (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (> (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark>= (mark1 mark2)
  "Returns T if Mark1 points to a character at or after Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (> (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (>= (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark= (mark1 mark2)
  "Returns T if both marks point to the same position, Nil otherwise."
  (and (eq (mark-line mark1) (mark-line mark2))
       (= (mark-charpos mark1) (mark-charpos mark2))))

(defun mark/= (mark1 mark2)
  "Returns T if both marks point to different positions, Nil otherwise."
  (not (and (eq (mark-line mark1) (mark-line mark2))
            (= (mark-charpos mark1) (mark-charpos mark2)))))

(defun line< (line1 line2)
  "Returns T if Line1 comes before Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (< (line-number line1) (line-number line2)))

(defun line<= (line1 line2)
  "Returns T if Line1 comes before or is the same as Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (<= (line-number line1) (line-number line2)))

(defun line>= (line1 line2)
  "Returns T if Line1 comes after or is the same as Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (>= (line-number line1) (line-number line2)))

(defun line> (line1 line2)
  "Returns T if Line1 comes after Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (> (line-number line1) (line-number line2)))

(defun lines-related (line1 line2)
  "Returns T if an order relation exists between Line1 and Line2."
  (eq (line-%buffer line1) (line-%buffer line2)))

(defun first-line-p (mark)
  "Returns T if the line pointed to by mark has no previous line,
  Nil otherwise."
  (null (line-previous (mark-line mark))))

(defun last-line-p (mark)
  "Returns T if the line pointed to by mark has no next line,
  Nil otherwise."
  (null (line-next (mark-line mark))))
