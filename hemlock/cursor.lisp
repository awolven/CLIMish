;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Rob MacLachlan
;;;
;;; Cursor: Routines for cursor positioning and recentering
;;;
(in-package :hemlock-internals)


(defconstant winning-char #b01 "Bit for a char that prints normally")
(defconstant losing-char #b10 "Bit for char with funny representation.")
(defvar *losing-character-mask*
  (make-character-set
   :page0 (make-array 256 :element-type '(mod 256)
                      :initial-element winning-char)  
   :table (make-hash-table)
   :default winning-char)
  "This is a character set used by redisplay to find funny chars.")
(defvar *print-representation-char-set* nil
  "Redisplay's handle on the :print-representation attribute.")

;;;  Do a find-character-with-attribute on the *losing-character-mask*.
(defmacro %fcwa (str start end mask)
  `(%sp-find-character-with-attribute
    ,str ,start ,end *losing-character-mask* ,mask))

;;; Get the print-representation of a character.
(defmacro get-rep (ch)
  `(char-set-ref *print-representation-char-set* (char-code ,ch)))


;;; cached-real-line-length
;;;
;;;    The same as Real-Line-Length, except does it for the cached line.
;;; the line argument is ignored, but present to make the arglists the
;;; same.
;;;
(defmacro .cached-real-line-length. (line width start end)
  (let ((line-sym (gensym))
        (width-sym (gensym))
        (start-sym (gensym))
        (end-sym (gensym)))
    `(let ((,line-sym ,line)
           (,width-sym ,width)
           (,start-sym ,start)
           (,end-sym ,end))
       (declare (fixnum ,width-sym ,start-sym ,end-sym))
       (declare (ignore ,line-sym))
       (let ((offset (- right-open-pos left-open-pos))
             (bound 0))
         (declare (fixnum offset bound))
         (cond
           ((>= ,start-sym left-open-pos)
            (setq ,start-sym (+ ,start-sym offset)
                  bound (setq ,end-sym (+ ,end-sym offset))))
           ((> ,end-sym left-open-pos)
            (setq bound left-open-pos
                  ,end-sym (+ ,end-sym offset)))
           (t
            (setq bound ,end-sym)))

         (do ((xpos 0)
              (ypos 0)
              (losing 0)
              (dy 0))
             (())
           (declare (fixnum xpos ypos dy)
                    (type (or fixnum null) losing))
           (when (= ,start-sym bound)
             (when (= ,start-sym ,end-sym) (return (values xpos ypos)))
             (setq ,start-sym right-open-pos  bound ,end-sym))
           (setq losing (%fcwa open-chars ,start-sym bound losing-char))
           (cond
             (losing
              (multiple-value-setq (dy xpos)
                (truncate (+ xpos (- losing ,start-sym)) ,width-sym))
              (setq ypos (+ ypos dy)  ,start-sym losing)
              (do ((last (or (%fcwa open-chars ,start-sym bound winning-char) bound)) str)
                  ((= ,start-sym last))
                (declare (fixnum last))
                (setq str (get-rep (schar open-chars ,start-sym)))
                (incf ,start-sym)
                (unless (simple-string-p str) (setq str (funcall str xpos)))
                (multiple-value-setq (dy xpos)
                  (truncate (+ xpos (strlen str)) ,width-sym))
                (setq ypos (+ ypos dy))))
             (t
              (multiple-value-setq (dy xpos)
                (truncate (+ xpos (- bound ,start-sym)) ,width-sym))
              (setq ypos (+ ypos dy)  ,start-sym bound))))))))

(defun cached-real-line-length (buffer line width start end)
  (with-buffer-globals (buffer)
    (.cached-real-line-length. line width start end)))


;;; real-line-length
;;;
;;;    Return as values the X position and the number of times wrapped if
;;; one to display the characters from Start to End of Line starting at an
;;; X position of 0 wrapping Width wide.
;;; %SP-Find-Character-With-Attribute is used to find charaters
;;; with funny representation much as in Compute-Line-Image.
;;;
(defmacro .real-line-length. (line width start end)
  (let ((line-sym (gensym))
        (width-sym (gensym))
        (start-sym (gensym))
        (end-sym (gensym)))
    `(let ((,line-sym ,line)
           (,width-sym ,width)
           (,start-sym ,start)
           (,end-sym ,end))
       (declare (fixnum ,width-sym ,start-sym ,end-sym))
       (do ((xpos 0)
            (ypos 0)
            (chars (line-chars ,line-sym))
            (losing 0)
            (dy 0))
           ((= ,start-sym ,end-sym) (values xpos ypos))
         (declare (fixnum xpos ypos dy) (simple-string chars)
                  (type (or fixnum null) losing))
         (setq losing (%fcwa chars ,start-sym ,end-sym losing-char))
         (when (null losing)
           (multiple-value-setq (dy xpos) (truncate (+ xpos (- ,end-sym ,start-sym)) ,width-sym))
           (return (values xpos (+ ypos dy))))
         (multiple-value-setq (dy xpos) (truncate (+ xpos (- losing ,start-sym)) ,width-sym))
         (setq ypos (+ ypos dy)  ,start-sym losing)
         (do ((last (or (%fcwa chars ,start-sym ,end-sym winning-char) ,end-sym)) str)
             ((= ,start-sym last))
           (declare (fixnum last))
           (setq str (get-rep (schar chars ,start-sym)))
           (incf ,start-sym)
           (unless (simple-string-p str) (setq str (funcall str xpos)))
           (multiple-value-setq (dy xpos) (truncate (+ xpos (strlen str)) ,width-sym))
           (setq ypos (+ ypos dy)))))))

(defun real-line-length (buffer line width start end)
  (with-buffer-globals (buffer)
    (.real-line-length. line width start end)))

;; these .such-and-such. macros capture symbols
;; and only should be called in the proper lexical environment

;;; Mark-Column  --  Public
;;;
;;;    Find the X position of a mark supposing that it were displayed
;;; in an infinitely wide screen.
;;;
(defmacro .mark-column. (mark)
  "Find the X position at which Mark would be displayed if it were on
  an infinitely wide screen.  This takes into account tabs and control
  characters."
  (let ((mark-sym (gensym)))
    `(let* ((,mark-sym ,mark)
            (charpos (mark-charpos ,mark-sym))
            (line (mark-line ,mark-sym)))
       (if (eq line open-line)
           (values (.cached-real-line-length. line 10000 0 charpos))
           (values (.real-line-length. line 10000 0 charpos))))))

(defun mark-column (buffer mark)
  (with-buffer-globals (buffer)
    (.mark-column. mark)))

;;; find-line
;;;
;;;    Find a dis-line which line is displayed on which starts before
;;; charpos, setting ypos and dis-line to the dis-line and it's index.
;;; Offset is expected to be the mark-charpos of the display-start for
;;; the window initially, and is set to offset within line that
;;; Dis-Line begins.  Charpos is the mark-charpos of the mark we want
;;; to find.  Check if same as *redisplay-favorite-line* and then scan
;;; if not.
;;;
(defmacro .find-line. (window line offset charpos ypos dis-lines dis-line)
  (declare (ignore charpos))
  (let ((line-sym (gensym)))
    `(let ((,line-sym ,line))
       (cond
         ;; No lines at all, fail.
         ((eq ,dis-lines (slot-value ,window 'the-sentinel)) nil)
         ;; On the first line, offset is already set, so just set dis-line and
         ;; ypos and fall through.
         ((and (car ,dis-lines) (eq (dis-line-line (car ,dis-lines)) ,line-sym))
          (setq ,dis-line ,dis-lines ,ypos 0))
         ;; Look farther down.
         ((do ((l (cdr ,dis-lines) (cdr l)))
              ((eq l (slot-value ,window 'the-sentinel)))
            (when (car l)
              (when (eq (dis-line-line (car l)) ,line-sym)
                (setq ,dis-line l ,ypos (dis-line-position (car l)) ,offset 0)
                (return t)))))
         (t (error "Horrible flaming lossage, Sorry Man."))))))


;;; Find-Position  --  Internal
;;;
;;;    Return the charpos which corresponds to the specified X position
;;; within Line.  If there is no such position between Start and End then
;;; rutne NIL.
;;;
(defmacro .find-position. (line position start end width)
  (let ((line-sym (gensym))
        (position-sym (gensym))
        (start-sym (gensym))
        (end-sym (gensym))
        (width-sym (gensym)))
    `(let ((,line-sym ,line)
           (,position-sym ,position)
           (,start-sym ,start)
           (,end-sym ,end)
           (,width-sym ,width))
       (do* ((cached (eq ,line open-line))
             (lo ,start-sym)
             (hi (1- ,end-sym))
             (probe (truncate (+ lo hi) 2) (truncate (+ lo hi) 2)))
            ((> lo hi)
             (if (= lo ,end-sym) nil hi))
         (let ((val (if cached
                        (.cached-real-line-length. ,line-sym ,width-sym ,start-sym probe)
                        (.real-line-length. ,line-sym ,width-sym ,start-sym probe))))
           (cond ((= val ,position-sym) (return probe))
                 ((< val ,position-sym) (setq lo (1+ probe)))
                 (t (setq hi (1- probe)))))))))

(defun find-position (buffer line position start end width)
  (with-buffer-globals (buffer)
    (.find-position. line position start end width)))



;;; Move-To-Column  --  Public
;;;
;;;    Just look up the charpos using find-position...
;;;
(defmacro .move-to-column. (mark column)
  "Move Mark to the specified Column on Line.  This function is analogous
  to Move-To-Position, but it deals with the physical screen position
  as returned by Mark-Column; the mark is moved to before the character
  which would be displayed in Column if the line were displayed on
  an infinitely wide screen.  If the column specified is greater than
  the column of the last character, then Nil is returned and the mark
  is not modified."
  (let ((mark-sym (gensym))
        (column-sym (gensym)))
    `(let* ((,mark-sym ,mark)
            (line (mark-line ,mark-sym))
            (,column-sym ,column)
            (res (.find-position. line ,column-sym 0 (line-length line) 10000)))
       (when res
         (move-to-position ,mark-sym res line)))))

(defun move-to-column (buffer mark column)
  (with-buffer-globals (buffer)
    (.move-to-column. mark column)))

;;; find-charpos
;;;
;;;    Special-Case mark at end of line, if not punt out to real-line-length
;;; function.  Return the correct values.
;;;
(defmacro .find-charpos. (line offset charpos length ypos dis-line width
                             mac chars)
  (declare (ignore chars))
  (let ((line-sym (gensym "FIND-CHARPOS_LINE"))
        (offset-sym (gensym "FIND-CHARPOS_OFFSET"))
        (charpos-sym (gensym "FIND-CHARPOS_CHARPOS"))
        (length-sym (gensym "FIND-CHARPOS_LENGTH"))
        (ypos-sym (gensym "FIND-CHARPOS_YPOS"))
        (dis-line-sym (gensym "FIND-CHARPOS_DIS-LINE"))
        (width-sym (gensym "FIND-CHARPOS_WIDTH")))
    `(let ((,line-sym ,line)
           (,offset-sym ,offset)
           (,charpos-sym ,charpos)
           (,length-sym ,length)
           (,ypos-sym ,ypos)
           (,dis-line-sym ,dis-line)
           (,width-sym ,width))
       (cond
         ((= ,charpos-sym ,length-sym)
          (do ((trail ,dis-line-sym dl)
               (dl (cdr ,dis-line-sym) (cdr dl)))
              ((not (eq (dis-line-line (car dl)) ,line-sym))
               (setq ,dis-line-sym (car trail)  ,ypos-sym (dis-line-position ,dis-line-sym))))
          (values (min (dis-line-length ,dis-line-sym) (1- ,width-sym)) ,ypos-sym))

         ((= ,charpos-sym (1- ,length-sym))
          (multiple-value-bind (x dy)
              (,mac ,line-sym (1- ,width-sym) ,offset-sym ,charpos-sym)
            (if (and (not (zerop dy)) (zerop x))
                (values (1- ,width-sym) (1- (+ ,ypos-sym dy)))
                (values x (+ ,ypos-sym dy)))))
         (t
          (multiple-value-bind (x dy)
              (,mac ,line-sym (1- ,width-sym) ,offset-sym ,charpos-sym)
            (values x (+ ,ypos-sym dy))))))))




;;;; Mark-To-Cursorpos
;;;
;;; Since performance analysis showed that HALF of the time in the editor
;;; was being spent in this function, I threw all of the tricks in the
;;; book at it to try and make it tenser.
;;;
;;; The algorithm is roughly as follows:
;;;
;;;    1) Eliminate the annoying boundry condition of the mark being
;;; off the end of the window, if it is return NIL now.
;;;    2) If the charpos is on or immediately after the last character
;;; in the line, then find the last dis-line on which the line is
;;; displayed.  We know that the mark is at the end of this dis-line
;;; because it is known to be on the screen.  X position is trivially
;;; derived from the dis-line-length.
;;;    3) Call Real-Line-Length or Cached-Real-Line-Length to get the
;;; X position and number of times wrapped.



;;; mark-to-cursorpos  --  Public
;;;
;;;    Return as multiple values the x and y position within window of
;;; mark.  NIL is returned if the mark is not displayed in the window given
;;;
;;;
(defun mark-to-cursorpos (mark window) ;; this needs to be changed to %rt-mark-to-cursorpos
  "Return the (x, y) position of mark within window, or NIL if not displayed."
  (handler-case
      (with-slots (the-sentinel) window
        (with-buffer-globals ((window-buffer window))
          (let* ((line (mark-line mark))
                 (number (line-number line))
                 (charpos (mark-charpos mark))
                 (dis-lines (cdr (window-first-line window)))
                 (width (window-width window))
                 (start (window-display-start window))
                 (offset (mark-charpos start))
                 (start-number (line-number (mark-line start)))
                 (end (window-display-end window))
                 (end-number (line-number (mark-line end)))
                 (ypos 0)
                 dis-line)
            (declare (fixnum width charpos ypos number end-number))
            (cond
              ((or (< number start-number)
                   (and (= number start-number) (< charpos offset))
                   (> number end-number)
                   (and (= number end-number) (> charpos (mark-charpos end)))) nil)
              ((.find-line. window line offset charpos ypos dis-lines dis-line)
               (cond
                 ((eq line open-line)
                  (let ((len (- line-cache-length (- right-open-pos left-open-pos))))
                    (declare (fixnum len))
                    (.find-charpos. line offset charpos len ypos dis-line width
                                    .cached-real-line-length. open-chars)))
                 (t
                  (let* ((chars (line-chars line))
                         (len (strlen chars)))
                    (declare (fixnum len) (simple-string chars))
                    (.find-charpos. line offset charpos len ypos dis-line width
                                    .real-line-length. chars)))))))))
    (error () (values 0 0))))

;;; Cursorpos-To-Mark  --  Public
;;;
;;;    Find the right dis-line, then zero in on the correct position
;;; using real-line-length.
;;;
(defun cursorpos-to-mark (x y window)
  (with-slots (the-sentinel) window
    (with-buffer-globals ((window-buffer window))
      (let ((width (window-width window))
            (first (window-first-line window)))
        (when (>= x width)
          (return-from cursorpos-to-mark nil))
        (do* ((prev first dl)
              (dl (cdr first) (cdr dl))
              (ppos (mark-charpos (window-display-start window))
                    (if dl
                        (if (eq (dis-line-line (car dl)) (dis-line-line (car prev)))
                            (dis-line-end (car prev)) 0)
                        0)))
             ((eq dl the-sentinel)
              (copy-mark (window-display-end window) :temporary))
          
          (when (= (dis-line-position (car dl)) y)
            (let* ((line (dis-line-line (car dl)))
                   (end (dis-line-end (car dl))))
              (return (mark line (or (.find-position. line x ppos end width) end))))))))))

;;; Dis-Line-Offset-Guess  --  Internal
;;;
;;;    Move Mark by Offset display lines.  The mark is assumed to be at the
;;; beginning of a display line, and we attempt to leave it at one.  We assume
;;; all characters print one wide.  Width is the width of the window we are
;;; displaying in.
;;;
(defmacro .dis-line-offset-guess. (mark offset width)
  (let ((mark-sym (gensym))
        (offset-sym (gensym))
        (width-sym (gensym)))
    `(let* ((,mark-sym ,mark)
            (,offset-sym ,offset)
            (,width-sym ,width)
            (w (1- ,width-sym)))
       (if (minusp ,offset-sym)
           (dotimes (i (- ,offset-sym) t)
             (let ((pos (mark-charpos ,mark-sym)))
               (if (>= pos w)
                   (character-offset ,mark-sym (- w))
                   (let ((prev (line-previous (mark-line ,mark-sym))))
                     (unless prev (return nil))
                     (multiple-value-bind (lines chars)
                         (truncate (line-length prev) w)
                       (.move-to-position. ,mark-sym
                           (cond ((zerop lines) 0)
                                 ((< chars 2)
                                  (* w (1- lines)))
                                 (t
                                  (* w lines)))
                           prev))))))
           (dotimes (i ,offset-sym t)
             (let ((left (- (line-length (mark-line ,mark-sym))
                            (mark-charpos ,mark-sym))))
               (if (> left ,width-sym)
                   (character-offset ,mark-sym w)
                   (unless (line-offset ,mark-sym 1 0)
                     (return nil)))))))))

(defun dis-line-offset-guess (buffer mark offset width)
  (with-buffer-globals (buffer)
    (.dis-line-offset-guess. mark offset width)))

;;; scroll-window  --  Public
;;;
;;;    This is not really right, since it uses dis-line-offset-guess.
;;; Probably if there is any screen overlap then we figure it out
;;; exactly.
;;;

;; scroll-window can be called in any thread

(defmacro .scroll-window. (window n)
  "Scroll Window down N lines, up if negative."
  (let ((window-sym (gensym "SCROLL-WINDOW_WINDOW"))
        (n-sym (gensym "SCROLL-WINDOW_N")))
    `(let* ((,window-sym ,window)
            (,n-sym ,n)
            (start (window-display-start ,window-sym))
            (point (window-point ,window-sym))
            (width (window-width ,window-sym))
            (height (window-height ,window-sym)))
       (cond ((.dis-line-offset-guess. start ,n-sym width))
             ((minusp ,n-sym)
              (buffer-start start .buffer.))
             (t
              (buffer-end start .buffer.)
              (let ((fraction (- (truncate height 3) height)))
                (.dis-line-offset-guess. start fraction width))))
       (let ((iscurrent (eq ,window-sym (krma.gui::focused-window (krma::main-window krma::*app*))))
             (bpoint .point.))
         (when iscurrent (move-mark point bpoint))
         (unless (displayed-p point ,window-sym)
           (move-mark point start)
           (.dis-line-offset-guess. point (round (window-height ,window-sym) 2)
                                    width)
           (when iscurrent (move-mark bpoint point))))
       (update-window-image ,window-sym)
       t)))

(defun scroll-window (window n)
  (with-buffer-globals ((window-buffer window))
    (.scroll-window. window n)))


;; to be called inside with-buffer-globals
(defmacro .goto-buffer-start. ()
  "Moves the point to the start of the current buffer."
  `(let ((point .point.))
     ;;(push-buffer-mark (copy-mark point))
     (buffer-start point)))

(defun goto-buffer-start (buffer)
  (with-buffer-globals (buffer)
    (.goto-buffer-start.)))

;; to be called inside with-buffer-globals
(defmacro .goto-buffer-end. ()
  "Moves the point to the end of the current buffer."
  `(let ((point .point.))
     ;;(push-buffer-mark (copy-mark point))
     (buffer-end point)))

(defun goto-buffer-end (buffer)
  (with-buffer-globals (buffer)
    (.goto-buffer-end.)))

;;; %Displayed-P  --  Internal
;;;
;;;    If Mark is within the displayed bounds in Window, then return true,
;;; otherwise false.  We assume the window image is up to date.
;;;

(defun displayed-p (mark window)
  (let ((start (window-display-start window))
        (end (window-display-end window)))
    (not (or (mark< mark start) (mark> mark end)
             (if (mark= mark end)
                 (let ((ch (next-character end)))
                   (and ch (char/= ch #\newline)))
                 nil)))))


;;; Displayed-p  --  Public
;;;
;;;    Update the window image and then check if the mark is displayed.
;;;

#+NIL
(defun displayed-p (mark window)
  "Return true if Mark is displayed on Window, false otherwise."
  (%rt-displayed-p mark window))

;;; center-window  --  Public
;;;
;;;    Try to move the start of window so that Mark is on a line in the
;;; center.
;;;
;; center-window: a window operation to be called in render thread
(defun center-window-on-mark (window mark)
  "Adjust the start of Window so that Mark is displayed on the center line."
  ;; note: this doesn't move the mark
  (with-buffer-globals ((window-buffer window))
    (let ((height (window-height window))
          (start (window-display-start window)))
      (move-mark start mark)
      (unless (.dis-line-offset-guess. start (- (truncate height 2))
                                       (window-width window))
        (move-mark start (buffer-start-mark .buffer.)))
      ;; If that doesn't work, panic and make the start the point.
      (unless (displayed-p mark window)
        (move-mark start mark)))))

(defun bottom-window-on-mark (window mark)
  "Adjust the start of Window so that Mark is displayed on the center line."
  ;; note: this doesn't move the mark
  (with-buffer-globals ((window-buffer window))
    (let ((height (window-height window))
          (start (window-display-start window))
          (end (window-display-end window)))
      (move-mark start mark)
      (move-mark end mark)
      ;; i don't really understand how to use dis-line-offset-guess
      ;; i basically just use it where the original code uses it
      (unless (.dis-line-offset-guess. start (- (1- height))
                                       (window-width window))
        (move-mark start (buffer-start-mark .buffer.)))
      ;; If that doesn't work, panic and make the start the point.
      (unless (displayed-p mark window)
        (move-mark start mark)))))


;;; maybe-recenter-window  --  Internal
;;;
;;;     Update the dis-lines for Window and recenter if the point is off
;;; the screen.
;;;
(defun maybe-recenter-window-on-mark (window mark)
  (let ((retval nil))
    (unless (displayed-p mark window)
      (center-window-on-mark window mark)
      (setq retval t))
    retval))

(defun maybe-rebottom-window-on-mark (window mark)
  (let ((retval nil))
    (unless (displayed-p mark window)
      (bottom-window-on-mark window mark)
      (setq retval t))
    retval))


(defun redraw-all-buffer-windows (buffer)
  (mapcar #'krma.gui::redraw-buffer (buffer-windows buffer)))

