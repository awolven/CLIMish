(in-package :hemlock.x11)


#+NIL
(defun make-bitmap-device (&rest initargs)
  (apply #'make-instance 'vktk::vktk-device initargs))

(defvar *hemlock-windows*
  (hi::make-object-set "Hemlock Windows" 'hi::default-clx-event-handler))

;;;; Some window making parameters.

;;; These could be parameters, but they have to be set after the display is
;;; opened.  These are set in INIT-BITMAP-SCREEN-MANAGER.

(defvar *default-background-pixel* nil
  "Default background color.  It defaults to white.")

(defvar *default-foreground-pixel* nil
  "Default foreground color.  It defaults to black.")

(defvar *default-margin-pixel* nil
  "Default margin color.  It defaults to grey.")

(defvar *foreground-background-xor* nil
  "The LOGXOR of *default-background-pixel* and *default-foreground-pixel*.")

(defvar *default-border-pixmap* nil
  "This is the default color of X window borders.  It defaults to a
  grey pattern.")

(defvar *highlight-border-pixmap* nil
  "This is the color of the border of the current window when the mouse
  cursor is over any Hemlock window.")

;;;; Making a Window.

(defparameter minimum-window-height 100
  "If the window created by splitting a window would be shorter than this,
  then we create an overlapped window the same size instead.")

;;; The width must be that of a tab for the screen image builder, and the
;;; height must be one line (two with a modeline).
;;;
(defconstant minimum-window-lines 2
  "Windows must have at least this many lines.")
(defconstant minimum-window-columns 10
  "Windows must be at least this many characters wide.")

(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant xwindow-border-width 2 "X border around X windows")
(defconstant xwindow-border-width*2 (* xwindow-border-width 2))
); eval-when

;;; We must name windows (set the "name" property) to get around a bug in
;;; awm and twm.  They will not handle menu clicks without a window having
;;; a name.  We set the name to this silly thing.
;;;
(defvar *hemlock-window-count* 0)
;;;
(defun new-hemlock-window-name ()
  (let ((*print-base* 10))
    (format nil "Hemlock ~S" (incf *hemlock-window-count*))))

(declaim (inline surplus-window-height surplus-window-height-w/-modeline))
;;;
(defun surplus-window-height (thumb-bar-p)
  (+ hunk-top-border (if thumb-bar-p
                         hunk-thumb-bar-bottom-border
                         hunk-bottom-border)))
;;;
(defun surplus-window-height-w/-modeline (thumb-bar-p)
  (+ (surplus-window-height thumb-bar-p)
     hunk-modeline-top
     hunk-modeline-bottom))

(defvar *create-window-hook* 'default-create-window-hook
  "Hemlock calls this function when it makes a new X window for a new group.
   It passes as arguments the X display, x (from MAKE-WINDOW), y (from
   MAKE-WINDOW), width (from MAKE-WINDOW), height (from MAKE-WINDOW), a name
   for the window's icon-name, font-family (from MAKE-WINDOW), modelinep (from
   MAKE-WINDOW), and whether the window will have a thumb-bar meter.  The
   function returns a window or nil.")

#+NIL
(defmethod device-make-window ((device vktk::vktk-device) id start modelinep window font-family
                               ask-user x y width height proportion)
  (let* ((thumb-bar-p (value hemlock::thumb-bar-meter))
         (hunk (make-bitmap-hunk
                :position 0
                :font-family font-family :trashed t
                :input-handler nil
                :device device
                :thumb-bar-p (and modelinep thumb-bar-p))))
    (let ((window-group nil))
      (declare (ignore window-group))

      (set-hunk-size hunk width height modelinep)

      (setf (bitmap-hunk-window hunk)
            (window-for-hunk hunk id x y start modelinep))

      (cond ((eq start (buffer-start-mark *echo-area-buffer*))
             (setf (bitmap-hunk-previous hunk) hunk)
             (setf (bitmap-hunk-next hunk) hunk))
            ((or window ask-user)
             (unless
                 ;; This occurs when we make the world's first Hemlock window.
                 *current-window*
               (setq *current-window* (bitmap-hunk-window hunk)))
             (setf (bitmap-hunk-previous hunk) hunk)
             (setf (bitmap-hunk-next hunk) hunk))
            (t
             (let ((h (window-hunk *current-window*)))
               (shiftf (bitmap-hunk-next hunk) (bitmap-hunk-next h) hunk)
               (setf (bitmap-hunk-previous (bitmap-hunk-next hunk)) hunk)
               (setf (bitmap-hunk-previous hunk) h))))

      (push hunk (device-hunks device))
      (bitmap-hunk-window hunk))))

(defun set-hunk-size (hunk w h &optional modelinep)
  (let* ((font-family (hemlock::bitmap-hunk-font-family hunk))
         (font-width (font-family-width font-family))
         (font-height (font-family-height font-family)))
    (setf (bitmap-hunk-height hunk) h)
    (setf (bitmap-hunk-width hunk) w)
    (setf (bitmap-hunk-char-width hunk)
          (max (truncate (- w hunk-left-border) font-width)
               minimum-window-columns))
    (let* ((h-minus-borders (- h hunk-top-border
                               (bitmap-hunk-bottom-border hunk)))
           (hwin (bitmap-hunk-window hunk))
           (modelinep (or modelinep (and hwin (window-modeline-buffer hwin)))))
      (setf (bitmap-hunk-char-height hunk)
            (max (if modelinep
                     (- (ceiling (- h-minus-borders
                                  hunk-modeline-top hunk-modeline-bottom)
                               font-height)
                        3)

                     (truncate h-minus-borders font-height))
                 minimum-window-lines))
      (setf (bitmap-hunk-modeline-pos hunk)
            (if modelinep
                (- h-minus-borders font-height hunk-modeline-top hunk-modeline-bottom))))))

(defun window-for-hunk (hunk id x y start modelinep)
  (check-type start mark)
  (setf (bitmap-hunk-changed-handler hunk) #'window-changed)
  (let* ((buffer (line-buffer (mark-line start)))
         (width (bitmap-hunk-char-width hunk))
         (height (bitmap-hunk-char-height hunk)))
    (when (or (< height minimum-window-lines)
              (< width minimum-window-columns))
      (error "Window too small."))
    (unless buffer (error "Window start is not in a buffer."))
    (let ((first (cons dummy-line (hi::make-the-sentinel))))
      (let ((window
              (internal-make-window
               :id id
               :hunk hunk
               :x x :y y
               :display-start (copy-mark start :right-inserting)
               :old-start (copy-mark start :temporary)
               :display-end (copy-mark start :right-inserting)
               :%buffer buffer
               :point (copy-mark (buffer-point buffer))
               :height height
               :width width
               :tick -1
               :first-line first
               :last-line (cdr first)
               :first-changed (cdr first)
               :last-changed first
               :the-sentinel (cdr first))))

        (setf (slot-value hunk 'the-sentinel) (cdr first))

        (push window *window-list*)
        (push window (buffer-windows buffer))
        ;;
        ;; Make the dis-lines.
        (do ((i (- height) (1+ i))
             (res ()
                  (cons (make-window-dis-line (make-string width)) res)))
            ((= i height) (setf (window-spare-lines window) res)))
        ;;
        ;; Make the image up to date.
        ;;(update-window-image window)
        (update-window window)
        (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
        ;;
        ;; If there is a modeline, set it up.
        (when modelinep
          (setup-modeline-image buffer window)

          (setf (bitmap-hunk-modeline-dis-line hunk)
                (window-modeline-dis-line window)))
        window))))

;;; BITMAP-HUNK-BOTTOM-BORDER -- Internal.
;;;
(defun bitmap-hunk-bottom-border (hunk)
  (if (bitmap-hunk-thumb-bar-p hunk)
      hunk-thumb-bar-bottom-border
      hunk-bottom-border))

;;; Window-Changed  --  Internal
;;;
;;;    The bitmap-hunk changed handler for windows.  This is only called if
;;; the hunk is not locked.  We invalidate the window image and change its
;;; size, then do a full redisplay.
;;;
(defun window-changed (hunk)
  (let ((window (bitmap-hunk-window hunk))
        (the-sentinel (slot-value hunk 'the-sentinel)))
    ;;
    ;; Nuke all the lines in the window image.
    (unless (eq (cdr (window-first-line window)) the-sentinel)
      (shiftf (cdr (window-last-line window))
              (window-spare-lines window)
              (cdr (window-first-line window))
              the-sentinel))
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
    ;;
    ;; Add some new spare lines if needed.  If width is greater,
    ;; reallocate the dis-line-chars.
    (let* ((res (window-spare-lines window))
           (new-width (bitmap-hunk-char-width hunk))
           (new-height (bitmap-hunk-char-height hunk))
           (width (length (the simple-string (dis-line-chars (car res))))))
      (declare (list res))
      (when (> new-width width)
        (setq width new-width)
        (dolist (dl res)
          (setf (dis-line-chars dl) (make-string new-width))))
      (setf (window-height window) new-height (window-width window) new-width)
      (do ((i (- (* new-height 2) (length res)) (1- i)))
          ((minusp i))
        (push (make-window-dis-line (make-string width)) res))
      (setf (window-spare-lines window) res)
      ;;
      ;; Force modeline update.
      (let ((ml-buffer (window-modeline-buffer window)))
        (when ml-buffer
          (let ((dl (window-modeline-dis-line window))
                (chars (make-string new-width))
                (len (min new-width (window-modeline-buffer-len window))))
            (setf (dis-line-old-chars dl) nil)
            (setf (dis-line-chars dl) chars)
            (replace chars ml-buffer :end1 (length chars) :end2 (length ml-buffer))
            (setf (dis-line-length dl) len)
            (setf (dis-line-flags dl) changed-bit)))))
    ;;
    ;; Prepare for redisplay.
    (setf (window-tick window) (tick))
    hunk))

(defmethod device-dumb-redisplay ((device bitmap-device) window)
  (dumb-window-redisplay window))

(defun dumb-window-redisplay (window)
  (let* ((hunk (window-hunk window))
         (the-sentinel (slot-value hunk 'the-sentinel))
         (first (window-first-line window))
         (x (hi::window-x window))
         (y (hi::window-y window))
         (width (window-width window))
         (height (window-height window))
         (imgui-padding-y 28.0f0)
         (default-font (default-font *app*))
         (font-size (slot-value default-font 'size)))

    (when (and x y width height)
      (setq x (vk:clampf x))
      (setq y (vk:clampf y))
      (setq width (vk:clampf width))
      (setq height (vk:clampf height))
      (if (not (eq window *echo-area-window*))
	  #+NIL
          (ig::igRenderFrame (+ x hunk-left-border)
                             (+ y imgui-padding-y)
                             (+ (+ x hunk-left-border) (* width 10.0f0))
                             (+ (+ y imgui-padding-y) (* height font-size))
                             #xff333333
                             t
                             8.0f0

			     )
	  #+NIL
          (ig::igRenderFrame (vk:clampf (+ hunk-left-border))
                             0.0f0
                             (+ (+ hunk-left-border) (* width 10.0f0))
                             (+ (* height font-size))
                             #xff333333
                             t
                             8.0f0
                             )))

;;    (hunk-reset hunk)
    (do ((i 0 (1+ i))
         (dl (cdr first) (cdr dl)))
        ((eq dl the-sentinel)
         (setf (window-old-lines window) (1- i)))
      (dumb-line-redisplay hunk (car dl)))
    (setf (window-first-changed window) the-sentinel
          (window-last-changed window) first)

    (when (window-modeline-buffer window)
      (hunk-replace-modeline hunk)
      (setf (dis-line-flags (window-modeline-dis-line window))
            unaltered-bits))
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))

    (when (eq window (current-window))
      (let ((device (device-hunk-device hunk))
            (point (window-point window)))

        (multiple-value-bind (x y) (mark-to-cursorpos point window)
          (when x ;; actually draw the cursor
            (device-put-region device hunk x y)
            (device-put-cursor device hunk x y)))
        ))))

#+NIL
(defmethod hi::device-next-window ((device vktk::vktk-device) window)
  (device-hunk-window (device-hunk-next (window-hunk window))))

;;; DUMB-LINE-REDISPLAY is used when the line is known to be cleared already.
;;;
(defun dumb-line-redisplay (hunk dl)
  (hunk-write-line hunk dl)
  (when dl
    (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0)))

#+NIL
(defmethod device-put-cursor ((device vktk::vktk-device) hunk x y)
  (with-slots (hi::cursor-item hi::cursor-hunk) device
    (setf hi::cursor-hunk hunk)
    (let ((imgui-padding-y 28.0f0))
      (setq x (* x 10.0f0))
      (setq y (* y 19.0f0))
      (if (eq (bitmap-hunk-window hunk) *echo-area-window*)
          (ig::igRenderFrame (vk:clampf (+ x (* 2 hunk-left-border)))
                             (vk:clampf (+ y hunk-top-border))
                         (vk:clampf (+ x (* 2 hunk-left-border) 10))
                         (vk:clampf (+ y hunk-top-border 19))
                         #x8800ffff
                         nil
                         0.0f0)
          (ig::igRenderFrame (vk:clampf (+ x hunk-left-border))
                         (vk:clampf (+ y imgui-padding-y))
                         (vk:clampf (+ x hunk-left-border 10))
                         (vk:clampf (+ y imgui-padding-y 19))
                         #x8800ffff
                         nil
                         0.0f0)))))

#+NIL
(defmethod device-put-region ((device vktk::vktk-device) hunk xc yc)
  (let* ((window (bitmap-hunk-window hunk))
         (buffer (when window (window-buffer window))))

    (unless (eq window *echo-area-window*)

      (when (region-active-p buffer)
        (let* ((region (when buffer (current-region buffer nil nil)))
               (start-mark (when region (region-start region)))
               (end-mark (when region (region-end region)))
               (start-line (when start-mark (mark-line start-mark)))
               (end-line (when end-mark (mark-line end-mark)))
               (imgui-padding-y 28.0f0))

          (when region
            (multiple-value-bind (dx1 dy1) (mark-to-cursorpos start-mark window)
              (unless dy1
                (setq dx1 0 dy1 0)) ;; display region is set to top of window
              (multiple-value-bind (dx2 dy2) (mark-to-cursorpos end-mark window)
                (unless dy2
                  (setq dx2 (window-width window) dy2 (window-height window)))
                ;;(destructuring-bind (buf2 y2 x2) (hi::mark-position end-mark)
                ;;(declare (ignore buf2))
                (if (same-line-p start-mark end-mark)
                    (let ((x1-px (* dx1 10))
                          (y1-px (* dy1 19))
                          (x2-px (* dx2 10))
                          (y2-px (* dy2 19)))
                      (ig::igRenderFrame (vk:clampf (+ x1-px hunk-left-border))
                                         (vk:clampf (+ y1-px imgui-padding-y))
                                         (vk:clampf (+ x2-px hunk-left-border 10))
                                         (vk:clampf (+ y2-px imgui-padding-y 19))
                                         #x22888888
                                         t
                                         0.0f0))
                    (let ((x1-px (* dx1 10))
                          (y1-px (* dy1 19))
                          (x2-px (* dx2 10))
                          (y2-px (* dy2 19)))
                      (ig::igRenderFrame (vk:clampf (+ x1-px hunk-left-border))
                                         (vk:clampf (+ y1-px imgui-padding-y))
                                         (vk:clampf (+ (* 10 (if (mark> start-mark (window-point window))
                                                              (line-length start-mark)
                                                              (window-width window)))
                                                    hunk-left-border 10)
                                                )
                                         (vk:clampf (+ y1-px imgui-padding-y 19))
                                         #x22888888
                                         t
                                         0.0f0)
                      (let ()
                        (do ((line (if (mark>= start-mark (window-point window))
                                       start-line
                                       (mark-line (window-point window)))
                                   (line-next line))
                             (y (1+ dy1) (1+ y)))
                            ((>= y dy2))
                          (let ((y-px (* y 19)))
                          (ig::igRenderFrame (vk:clampf (+ 0 hunk-left-border))
                                             (vk:clampf (+ y-px imgui-padding-y))
                                             (vk:clampf (+ (* 10 (window-width window)) hunk-left-border 10))
                                             (vk:clampf (+ y-px imgui-padding-y 19))
                                             #x22888888
                                             t
                                             0.0f0))))
                      (ig::igRenderFrame (vk:clampf (+ 0 hunk-left-border))
                                         (vk:clampf (+ y2-px imgui-padding-y))
                                         (vk:clampf (+ x2-px hunk-left-border 10))
                                         (vk:clampf (+ y2-px imgui-padding-y 19))
                                         #x22888888
                                         t
                                         0.0f0)))))))))))

(progn
(defun window-group-changed (window-group new-width new-height)
  (declare (ignore window-group new-width new-height))
      (values))
)

(defun hunk-reconfigured (object event-key event-window window x y width
                                 height border-width above-sibling
                                 override-redirect-p send-event-p)
  (declare (ignore event-key event-window window x y border-width
                   above-sibling override-redirect-p send-event-p))
  (typecase object
    (hemlock.x11::bitmap-hunk
     (when (or (/= width (bitmap-hunk-width object))
               (/= height (bitmap-hunk-height object)))
       (hunk-changed object width height nil)
       ;; Under X11, don't redisplay since an exposure event is coming next.
       (setf (bitmap-hunk-trashed object) t)))
    (window-group
     (let ((old-width (window-group-width object))
           (old-height (window-group-height object)))
       (when (or (/= width old-width) (/= height old-height))
         (hi::window-group-changed object width height))))))
;;;
#+NIL
(hemlock-internals::serve-configure-notify *hemlock-windows* #'hunk-reconfigured)

(defun hunk-changed (hunk new-width new-height redisplay)
  (set-hunk-size hunk new-width new-height)
  (funcall (bitmap-hunk-changed-handler hunk) hunk)
  (when redisplay (dumb-window-redisplay (bitmap-hunk-window hunk))))
