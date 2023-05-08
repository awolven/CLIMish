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
;;; This file contains implementation independant functions that
;;; build window images from the buffer structure.
;;;
(in-package :hemlock-internals)

#+NIL(defvar the-sentinel
  (list (make-window-dis-line ""))
  "This dis-line, which has several interesting properties, is used to end
  lists of dis-lines.")
#+NIL(setf (dis-line-line (car the-sentinel))
      (make-line :number most-positive-fixnum :chars "" :%buffer (make-instance 'disembodied-buffer :id -1)))
#+NIL(setf (dis-line-position (car the-sentinel)) most-positive-fixnum)
#+NIL(setf (dis-line-old-chars (car the-sentinel)) :unique-thing)

(defun make-the-sentinel ()
  (let ((dis-line (make-window-dis-line "")))
    (setf (dis-line-line dis-line)
          (make-line :number most-positive-fixnum
                     :chars ""
                     :%buffer (make-instance 'disembodied-buffer :id -1))
          (dis-line-position dis-line) most-positive-fixnum
          (dis-line-old-chars dis-line) :unique-thing)
    (list dis-line)))


(defmethod print-object ((object disembodied-buffer) stream)
  (print-unreadable-object (object stream)
    (format stream "disembodied-buffer id = ~S" (slot-value object 'id))))

(defconstant unaltered-bits #b000
  "This is the value of the dis-line-flags when a line is neither moved nor
  changed nor new.")
(defconstant changed-bit #b001
  "This bit is set in the dis-line-flags when a line is found to be changed.")
(defconstant moved-bit #b010
  "This bit is set in the dis-line-flags when a line is found to be moved.")
(defconstant new-bit #b100
  "This bit is set in the dis-line-flags when a line is found to be new.")
(defconstant retag-bit #b1000
  "This bit is set in the dis-line-flags when a line has an updated tag.")


;;; move-lines  --  Internal
;;;
;;;    This function is called by Maybe-Change-Window when it believes that
;;; a line needs to be inserted or deleted.  When called it finishes the
;;; image-update for the entire rest of the window.  Here and many other
;;; places the phrase "dis-line" is often used to mean a pointer into the
;;; window's list of dis-lines.
;;;
;;; Window - The window whose image needs to be updated.
;;; Changed - True if the first-changed line has already been set, if false
;;;  we must set it.
;;; String - The overhang string to be added to the beginning of the first
;;;  line image we build.  If no overhang then this is NIL.
;;; Underhang - The number of trailing chars of String to use.
;;; Line - The line at which we are to continue building the image.  This
;;;  may be NIL, in which case we are at the end of the buffer.
;;; Offset - The charpos within Line to continue at.
;;; Current - The dis-line which caused Maybe-Change-Window to choke; it
;;;  may be the-sentinel, it may not be the dummy line at head of the
;;;  window's dis-lines.  This is the dis-line at which Maybe-Change-Window
;;;  turns over control, it should not be one whose image it built.
;;; Trail - This is the dis-line which immediately precedes Current in the
;;;  dis-line list.  It may be the dummy dis-line, it may not be the sentinel.
;;; Width - (window-width window)



;;; maybe-change-window  --  Internal
;;;
;;;    This macro is "Called" in update-window-image whenever it finds that
;;; the chars of the line and the dis-line don't match.  This may happen for
;;; several reasons:
;;;
;;; 1] The previous line was unchanged, but wrapped, so the dis-line-chars
;;; are nil.  In this case we just skip over the extension lines.
;;;
;;; 2] A line is changed but not moved; update the line noting whether the
;;; next line is moved because of this, and bugging out to Move-Lines if
;;; it is.
;;;
;;; 3] A line is deleted, off the top of the screen, or moved.  Bug out
;;; to Move-Lines.
;;;
;;;    There are two possible results, either we return NIL, and Line,
;;; Trail and Current are updated, or we return T, in which case
;;; Update-Window-Image should terminate immediately.  Changed is true
;;; if a changed line changed lines has been found.
;;;
;;(eval-when (:compile-toplevel :load-toplevel :execute)

;;); eval-when

;;; update-window-image  --  Internal
;;;
;;;    This is the function which redisplay calls when it wants to ensure that
;;; a window-image is up-to-date.  The main loop here is just to zoom through
;;; the lines and dis-lines, bugging out to Maybe-Change-Window whenever
;;; something interesting happens.
;;;
(defun update-window-image (window)
  (format #+sbcl sb-sys::*stdout* #-sbcl *debug-io* "~%update-window-image ~S" window)
  (finish-output #+sbcl sb-sys::*stdout* #-sbcl *debug-io*)
  (with-slots (the-sentinel) window
    (when (window-buffer window)
      (with-buffer-globals ((window-buffer window))
        ;; Make the-sentinel in this buffer so we don't delete it.
        (setf (line-%buffer (dis-line-line (car the-sentinel))) .buffer.)
        (macrolet ((compute-cached-line-image (index dis-line xpos width)
                     (let ((index-sym (gensym))
                           (dis-line-sym (gensym))
                           (xpos-sym (gensym))
                           (width-sym (gensym)))
                       `(let ((,index-sym ,index)
                              (,dis-line-sym ,dis-line)
                              (,xpos-sym ,xpos)
                              (,width-sym ,width))
                          (prog ((gap (- right-open-pos left-open-pos))
                                 (dest (dis-line-chars ,dis-line-sym))
                                 (done-p (= right-open-pos line-cache-length))
                                 (losing 0)
                                 string underhang)
                             (declare (fixnum gap) (simple-string dest)
                                      (type (or fixnum null) losing))
                             LEFT-LOOP
                             (cond
                               (underhang
                                (update-and-punt ,dis-line-sym ,width-sym string underhang ,index-sym))
                               ((null ,xpos-sym)
                                (update-and-punt ,dis-line-sym ,width-sym nil 0 ,index-sym))
                               ((>= ,index-sym left-open-pos)
                                (go RIGHT-START)))
                             (setq losing (%fcwa open-chars ,index-sym left-open-pos losing-char))
                             (cond
                               (losing
                                (display-some-chars open-chars ,index-sym losing dest ,xpos-sym ,width-sym nil)
                                ;; If we we didn't wrap then display some losers...
                                (if ,xpos-sym
                                    (display-losing-chars open-chars ,index-sym left-open-pos dest ,xpos-sym
                                                          ,width-sym string underhang string-get-rep
                                                          (and done-p (= ,index-sym left-open-pos)))
                                    (update-and-punt ,dis-line-sym ,width-sym nil 0 ,index-sym)))
                               (t
                                (display-some-chars open-chars ,index-sym left-open-pos dest ,xpos-sym ,width-sym done-p)))
                             (go LEFT-LOOP)

                             RIGHT-START
                             (setq ,index-sym (+ ,index-sym gap))
                             RIGHT-LOOP
                             (cond
                               (underhang
                                (update-and-punt ,dis-line-sym ,width-sym string underhang (- ,index-sym gap)))
                               ((null ,xpos-sym)
                                (update-and-punt ,dis-line-sym ,width-sym nil 0 (- ,index-sym gap)))
                               ((= ,index-sym line-cache-length)
                                (update-and-punt ,dis-line-sym ,xpos-sym nil nil (- ,index-sym gap))))
                             (setq losing (%fcwa open-chars ,index-sym line-cache-length losing-char))
                             (cond
                               (losing
                                (display-some-chars open-chars ,index-sym losing dest ,xpos-sym ,width-sym nil)
                                (cond
                                  ;; Did we wrap?
                                  ((null ,xpos-sym)
                                   (update-and-punt ,dis-line-sym ,width-sym nil 0 (- ,index-sym gap)))
                                  (t
                                   (display-losing-chars open-chars ,index-sym line-cache-length dest ,xpos-sym
                                                         ,width-sym string underhang string-get-rep))))
                               (t
                                (display-some-chars open-chars ,index-sym line-cache-length dest ,xpos-sym ,width-sym t)))
                             (go RIGHT-LOOP)))))

                   (compute-line-image (string underhang line offset dis-line width)
                     (let ((string-sym (gensym))
                           (underhang-sym (gensym))
                           (line-sym (gensym))
                           (offset-sym (gensym))
                           (dis-line-sym (gensym))
                           (width-sym (gensym)))
                       `(let ((,string-sym ,string)
                              (,underhang-sym ,underhang)
                              (,line-sym ,line)
                              (,offset-sym ,offset)
                              (,dis-line-sym ,dis-line)
                              (,width-sym ,width))
                          ;;
                          ;; Check the tag.
                          ;;
                          ;; FIXME This isn't sufficient, because we don't get called in cases
                          ;; where line contents are unchanged.  For now, the backend papers
                          ;; over that, but this should get moved elsewhere.
                          ;;
                          (sync-dis-line-tag ,line-sym ,dis-line-sym)
                          ;;
                          ;; Release any old font-changes.
                          (let ((changes (dis-line-font-changes ,dis-line-sym)))
                            (when changes
                              (do ((prev changes current)
                                   (current (font-change-next changes)
                                            (font-change-next current)))
                                  ((null current)
                                   (setf (dis-line-font-changes ,dis-line-sym) nil)
                                   (shiftf (font-change-next prev) *free-font-changes* changes))
                                (setf (font-change-mark current) nil))))
                          ;;
                          ;; If the line has any Font-Marks, add Font-Changes for them.

                          (let ((marks (line-marks ,line-sym)))
                            (when (dolist (m marks nil)
                                    (when (fast-font-mark-p m) (return t)))
                              (let ((prev nil))
                                ;;
                                ;; Find the last Font-Mark with charpos less than Offset.  If there is
                                ;; such a Font-Mark, then there is a font-change to this font at X = 0.
                                (let ((max -1)
                                      (max-mark nil))
                                  (dolist (m marks)
                                    (when (fast-font-mark-p m)
                                      (let ((charpos (mark-charpos m)))
                                        (when (and (< charpos ,offset-sym) (> charpos max))
                                          (setq max charpos  max-mark m)))))
                                  (when max-mark
                                    (setq prev (alloc-font-change 0 (font-mark-font max-mark) max-mark))
                                    (setf (dis-line-font-changes ,dis-line-sym) prev)))
                                ;;
                                ;; Repeatedly scan through marks, adding a font-change for the
                                ;; smallest Font-Mark with a charpos greater than Bound, until
                                ;; we find no such mark.
                                (do ((bound (1- ,offset-sym) min)
                                     (min most-positive-fixnum most-positive-fixnum)
                                     (min-mark nil nil))
                                    (())
                                  (dolist (m marks)
                                    (when (fast-font-mark-p m)
                                      (let ((charpos (mark-charpos m)))
                                        (when (and (> charpos bound) (< charpos min))
                                          (setq min charpos  min-mark m)))))
                                  (unless min-mark (return nil))
                                  (let ((len (if (eq ,line-sym open-line)
                                                 (.cached-real-line-length. ,line-sym 10000 ,offset-sym min)
                                                 (.real-line-length. ,line-sym 10000 ,offset-sym min))))
                                    (when (< len ,width-sym)
                                      (let ((new-fc (alloc-font-change
                                                     (+ len
                                                        (if ,string-sym
                                                            (- (length (the simple-string ,string-sym)) ,underhang-sym)
                                                            0))
                                                     (font-mark-font min-mark)
                                                     min-mark)))
                                        (if prev
                                            (setf (font-change-next prev) new-fc)
                                            (setf (dis-line-font-changes ,dis-line-sym) new-fc))
                                        (setq prev new-fc))))))))
                          ;;
                          ;; Recompute the line image.
                          (cond
                            (,string-sym
                             (let ((len (strlen ,string-sym))
                                   (chars (dis-line-chars ,dis-line-sym))
                                   (xpos 0))
                               (declare (type (or fixnum null) xpos) (simple-string chars))
                               (display-some-chars ,string-sym ,underhang len chars xpos ,width-sym nil)
                               (cond
                                 ((null xpos)
                                  (values ,string-sym ,underhang-sym ,offset-sym))
                                 ((eq ,line-sym open-line)
                                  (compute-cached-line-image ,offset-sym ,dis-line-sym xpos ,width-sym))
                                 (t
                                  (compute-normal-line-image ,line-sym ,offset-sym ,dis-line-sym xpos ,width-sym)))))
                            ((eq ,line-sym open-line)
                             (compute-cached-line-image ,offset-sym ,dis-line-sym 0 ,width-sym))
                            (t
                             (compute-normal-line-image ,line-sym ,offset-sym ,dis-line-sym 0 ,width-sym)))))))

          (flet ((move-lines (changed string underhang line offset trail current width)
                   (declare (fixnum offset width))
                   (do* ((delta 0)
                         (cc (car current))
                         (old-line (dis-line-line cc))
                         ;; Can't use current, since might be the-sentinel.
                         (pos (1+ (dis-line-position (car trail))))
                         ;; Are we on an extension line?
                         (is-wrapped (eq line (dis-line-line (car trail))))
                         (last (window-last-line window))
                         (last-line (dis-line-line (car last)))
                         (save trail)
                         (height (window-height window))
                         (spare-lines (window-spare-lines window))
                         (buffer (setf (line-%buffer (dis-line-line (car the-sentinel)))
                                       .buffer.))
                         (start offset) new-num)
                        ((or (= pos height) (null line))
                         ;;    If we have run off the bottom or run out of lines then we are
                         ;; done.  At this point Trail is the last line displayed and Current is
                         ;; whatever comes after it, possibly the-sentinel.
                         ;;    We always say that last-changed is the last line so that we
                         ;; don't have to max in the old last-changed.
                         (setf (window-last-changed window) trail)
                         ;; If there are extra lines at the end that need to be deleted
                         ;; and haven't been already then link them into the free-list.
                         (unless (eq last trail)
                           ;; This test works, because if the old last line was either
                           ;; deleted or another line was inserted after it then it's
                           ;; cdr would be something else.
                           (when (eq (cdr last) the-sentinel)
                             (shiftf (cdr last) spare-lines (cdr trail) the-sentinel))
                           (setf (window-last-line window) trail))
                         (setf (window-spare-lines window) spare-lines)
                         ;;    If first-changed has not been set then we set the first-changed
                         ;; to the first line we looked at if it does not come after the
                         ;; new position of the old first-changed.
                         (unless changed
                           (when (> (dis-line-position (car (window-first-changed window)))
                                    (dis-line-position (car save)))
                             (setf (window-first-changed window) (cdr save)))))

                     (setq new-num (line-number line))
                     ;; If a line has been deleted, it's line-%buffer is smashed; we unlink
                     ;; any dis-line which displayed such a line.

                     (cond
                       ((or (null old-line) (neq (line-%buffer old-line) .buffer.))
                        (do ((ptr (cdr current) (cdr ptr))
                             (prev current ptr))
                            ((eq (line-%buffer (dis-line-line (car ptr))) .buffer.)
                             (setq delta (- pos (1+ (dis-line-position (car prev)))))
                             (shiftf (cdr trail) (cdr prev) spare-lines current ptr)))
                        (setq cc (car current)  old-line (dis-line-line cc)))
                       ;; If the line-number of the old line is less than the line-number
                       ;; of the line we want to display then the old line must be off the top
                       ;; of the screen - delete it.  The-Sentinel fails this test because
                       ;; it's line-number is most-positive-fixnum.
                       ((< (line-number old-line) new-num)
                        (do ((ptr (cdr current) (cdr ptr))
                             (prev current ptr))
                            ((>= (line-number (dis-line-line (car ptr))) new-num)
                             (setq delta (- pos (1+ (dis-line-position (car prev)))))
                             (shiftf (cdr trail) (cdr prev) spare-lines current ptr)))
                        (setq cc (car current)  old-line (dis-line-line cc)))
                       ;; New line comes before old line, insert it, punting when
                       ;; we hit the bottom of the screen.
                       ((neq line old-line)
                        (do ((chars (unless is-wrapped (line-chars line)) nil) new)
                            (())
                          (setq new (car spare-lines))
                          (setf (dis-line-old-chars new) chars
                                (dis-line-position new) pos
                                (dis-line-line new) line
                                (dis-line-delta new) 0
                                (dis-line-flags new) new-bit)
                          (setq pos (1+ pos)  delta (1+ delta))
                          (multiple-value-setq (string underhang start)
                            (compute-line-image string underhang line start new width))
                          (rotatef (cdr trail) spare-lines (cdr spare-lines))
                          (setq trail (cdr trail))
                          (cond ((= pos height)
                                 (return nil))
                                ((null underhang)
                                 (setq start 0  line (line-next line))
                                 (return nil))))
                        (setq is-wrapped nil))
                       ;; The line is the same, possibly moved.  We add in the delta and
                       ;; or in the moved bit so that if redisplay punts in the middle
                       ;; the information is not lost.
                       ((eq (line-chars line) (dis-line-old-chars cc))
                        ;; If the line is the old bottom line on the screen and it has moved and
                        ;; is full length, then mash the old-chars and quit so that the image
                        ;; will be recomputed the next time around the loop, since the line might
                        ;; have been wrapped off the bottom of the screen.
                        (cond
                          ((and (eq line last-line)
                                (= (dis-line-length cc) width)
                                (not (zerop delta)))
                           (setf (dis-line-old-chars cc) :another-unique-thing))
                          (t
                           (do ()
                               ((= pos height))
                             (unless (zerop delta)
                               (setf (dis-line-position cc) pos)
                               (incf (dis-line-delta cc) delta)
                               (setf (dis-line-flags cc) (logior (dis-line-flags cc) moved-bit)))
                             (shiftf trail current (cdr current))
                             (setq cc (car current)  old-line (dis-line-line cc)  pos (1+ pos))
                             (when (not (eq old-line line))
                               (setq start 0  line (line-next line))
                               (return nil))))))
                       ;; The line is changed, possibly moved.
                       (t
                        (do ((chars (line-chars line) nil))
                            (())
                          (multiple-value-setq (string underhang start)
                            (compute-line-image string underhang line start cc width))
                          (setf (dis-line-flags cc) (logior (dis-line-flags cc) changed-bit)
                                (dis-line-old-chars cc) chars
                                (dis-line-position cc) pos)
                          (unless (zerop delta)
                            (incf (dis-line-delta cc) delta)
                            (setf (dis-line-flags cc) (logior (dis-line-flags cc) moved-bit)))
                          (shiftf trail current (cdr current))
                          (setq cc (car current)  old-line (dis-line-line cc)  pos (1+ pos))
                          (cond ((= pos height)
                                 (return nil))
                                ((null underhang)
                                 (setq start 0  line (line-next line))
                                 (return nil))
                                ((not (eq old-line line))
                                 (setq is-wrapped t)
                                 (return nil)))))))))

            (macrolet ((maybe-change-window (changed line offset trail current width)
                         `(let* ((cc (car ,current))
                                 (old-line (dis-line-line cc)))
                            (cond
                                                             
                              ;; We have run into a continuation line, skip over any.
                              ((and (null (dis-line-old-chars cc))
                                    (eq old-line (dis-line-line (car ,trail))))
                               (do ((ptr (cdr ,current) (cdr ptr))
                                    (prev ,current ptr))
                                   ((not (eq (dis-line-line (car ptr)) old-line))
                                    (setq ,trail prev  ,current ptr) nil)))
                              ;; A line is changed.
                              ((eq old-line ,line)
                               (unless ,changed
                                 (when (< (dis-line-position cc)
                                          (dis-line-position (car (window-first-changed window))))
                                   (setf (window-first-changed window) ,current)
                                   (setq ,changed t)))
                               (do ((chars (line-chars ,line) nil)
                                    (start ,offset) string underhang)
                                   (())
                                 (multiple-value-setq (string underhang start)
                                   (compute-line-image string underhang ,line start cc ,width))
                                 (setf (dis-line-flags cc) (logior (dis-line-flags cc) changed-bit))
                                 (setf (dis-line-old-chars cc) chars)
                                 (setq ,trail ,current  ,current (cdr ,current)  cc (car ,current))
                                 (cond
                                   ((eq (dis-line-line cc) ,line)
                                    (unless underhang
                                      (move-lines t nil 0 (line-next ,line) 0 ,trail ,current
                                                  ,width)
                                      (return t)))
                                   (underhang
                                    (move-lines t string underhang ,line start ,trail
                                                ,current ,width)
                                    (return t))
                                   (t
                                    (setq ,line (line-next ,line))
                                    (when (> (dis-line-position (car ,trail))
                                             (dis-line-position (car (window-last-changed window))))
                                      (setf (window-last-changed window) ,trail))
                                    (return nil)))))
                              (t
                               (move-lines ,changed nil 0 ,line ,offset ,trail ,current
                                           ,width)
                               t)))))

              (let* ((trail (window-first-line window))
                     (current (cdr trail))
                     (display-start (window-display-start window))
                     (line (mark-line display-start))
                     (width (window-width window))
                     changed
                     (emergency-exit 0))


                (do ((i (- (hi::window-height window)) (1+ i))
                     (res ()
                          (cons (hi::make-window-dis-line (make-string (hi::window-width window))) res)))
                    ((= i (hi::window-height window)) (setf (hi::window-spare-lines window) res)))

                (cond
                  ;; If the first line or its charpos has changed then bug out.
                  ((cond ((and (eq (dis-line-old-chars (car current)) (line-chars line))
                               (mark= display-start (window-old-start window)))
                          (setq trail current  current (cdr current)  line (line-next line))
                          nil)
                         (t
                          ;; Force the line image to be invalid in case the start moved
                          ;; and the line wrapped onto the screen.  If we started at the
                          ;; beginning of the line then we don't need to.
                          (unless (zerop (mark-charpos (window-old-start window)))
                            (unless (eq current the-sentinel)
                              (setf (dis-line-old-chars (car current)) :another-unique-thing)))
                          (let ((start-charpos (mark-charpos display-start)))
                            (move-mark (window-old-start window) display-start)
                            ;;(window changed line offset trail current width)
                            (maybe-change-window changed line start-charpos trail current width)))))
                  (t
                   (prog ()
                      (go TOP)
                      STEP
                      (setf (dis-line-line (car current)) line)
                      (setq trail current  current (cdr current)  line (line-next line))
                      TOP
                      (incf emergency-exit)
                      (when (>= emergency-exit (* 1000 (1+ (window-height window))))
                        (print "emergency exit" #+sbcl sb-sys::*stdout* #-sbcl *debug-io*)
                        (finish-output #+sbcl sb-sys::*stdout* #-sbcl *debug-io*)
                        (return nil))
                      (cond ((null line)
                             (go DONE))
                            ((eq (line-chars line) (dis-line-old-chars (car current)))
                             (go STEP)))
                      ;;
                      ;; We found a suspect line.
                      ;; See if anything needs to be updated, if we bugged out, punt.
                      (when (and (eq current the-sentinel)
                                 (= (dis-line-position (car trail))
                                    (1- (window-height window))))
                        (return nil))
                      (when (maybe-change-window changed line 0 trail current width)
                        (return nil))
                      (go TOP)

                      DONE
                      ;;
                      ;; We hit the end of the buffer. If lines need to be deleted bug out.
                      (unless (eq current the-sentinel)
                        (maybe-change-window changed line 0 trail current width))
                      (return nil))))
                ;;
                ;; Update the display-end mark.
                (let ((dl (car (window-last-line window))))
                  (move-to-position (window-display-end window) (dis-line-end dl) (dis-line-line dl)))

                (let* (
                       ;; window point is where we're going to draw the cursor
                       ;; when dumb-window-redisplay happens
                       (point (window-point window)))
                  ;; want this move mark to happen during update-window-image timeslice of render loop
                  (move-mark point .point.))
                ))))))))
