(in-package :hi)

(defclass hi::window ()
  ())

(defclass text-editor-window-mixin (hi::window)
  ((krma::title :initform "Hemlock")

   (krma::x :accessor window-x)
   (krma::y :accessor window-y)

   (width
    :accessor window-width
    :initform nil
    :initarg :width-chars
    :documentation "Width of the window in characters.")

   (height
    :accessor window-height
    :initform nil
    :initarg :height-lines
    :documentation "Height of the window in lines.")

   (tick
    :accessor window-tick
    :initform nil
    :initarg :tick
    :documentation "The last time this window was updated.")

   (%buffer
    :accessor window-%buffer
    :initform nil
    :initarg :%buffer
    :documentation "buffer displayed in this window.")

   (old-start
    :accessor window-old-start
    :initform nil
    :initarg :old-start
    :documentation "The charpos of the first char displayed.")

   (first-line
    :accessor window-first-line
    :initform nil
    :initarg :first-line
    :documentation "The head of the list of dis-lines.")

   (last-line
    :accessor window-last-line
    :initform nil
    :initarg :last-line
    :documentation "The last dis-line displayed.")

   (first-changed
    :accessor window-first-changed
    :initform nil
    :initarg :first-changed
    :documentation "The first changed dis-line on last update.")

   (last-changed
    :accessor window-last-changed
    :initform nil
    :initarg :last-changed
    :documentation "The last changed dis-line.")

   (spare-lines
    :accessor window-spare-lines
    :initform nil
    :initarg :spare-lines
    :documentation "The head of the list of unused dis-lines")

   (old-lines
    :accessor window-old-lines
    :initform 0
    :initarg :old-lines
    :documentation "Slot used by display to keep state info")

   (display-start
    :accessor window-display-start
    :initform nil
    :initarg :display-start
    :documentation "first character position displayed")

   (display-end
    :accessor window-display-end
    :initform nil
    :initarg :display-end
    :documentation "last character displayed")

   (point
    :accessor window-point
    :initform nil
    :initarg :point
    :documentation "Where the cursor is in this window.")

   (modeline-dis-line
    :accessor window-modeline-dis-line
    :initform nil
    :initarg :modeline-dis-line
    :documentation "Dis-line for modeline display.")

   (modeline-buffer
    :accessor window-modeline-buffer
    :initform nil
    :initarg :modeline-buffer
    :documentation "Complete string of all modeline data.")

   (modeline-buffer-len
    :accessor window-modeline-buffer-len
    :initform nil
    :initarg :modeline-buffer-len
    :documentation "Valid chars in modeline-buffer.")

   (display-recentering
    :accessor window-display-recentering
    :initform nil
    :initarg :display-recentering
    :documentation "Tells whether redisplay recenters window, regardless of whether it is current.")

   (the-sentinel ;; the sentinels are specific to each window 'cause they get line-%buffer set
    :initarg :the-sentinel
    :documentation "This dis-line, which has several interesting properties, is used to end
  lists of dis-lines.")

   (font-family
    :accessor window-font-family
    :initform nil
    :initarg :font-family
    :documentation "The default font for a hemlock window.")))




(defun internal-make-window (&rest args)
  (apply #'make-instance 'text-editor-window args))

(defmethod print-object ((object text-editor-window-mixin) stream)
  (hi::%print-hwindow object stream nil))

(defun hi::windowp (thing)
  (typep thing 'text-editor-window-mixin))

(defclass hi::buffer () ())

(defclass text-editor-buffer (hi::buffer)
  ((lock :reader buffer-lock
         :initform (bt:make-recursive-lock "buffer lock"))
   (open-line :accessor buffer-open-line
              :documentation "Line open for hacking on."
              :initform nil)
   (open-chars :accessor buffer-open-chars
               :documentation "A Vector of characters for hacking on."
               :initform (make-string 200))
   (left-open-pos :accessor buffer-left-open-pos
                  :documentation "Index to first free character to left of mark in Open-Chars."
                  :initform 0)
   (right-open-pos :accessor buffer-right-open-pos
                   :documentation "Index to first used character to right of mark in Open-Chars."
                   :initform 0)
   (line-cache-length :accessor buffer-line-cache-length
                      :initform 200
                      :documentation "Length of Open-Chars.")

   (*internal-temp-region* :initform (make-empty-region))

   (*internal-temp-mark* :initform (internal-make-mark nil nil :temporary))

   (last-was-undo-p :initform nil)
   (this-is-undo-p :initform nil)
   (undoing-undo-list :initform nil)

   (%name :accessor buffer-%name :initform nil :initarg :%name)
   (%region :accessor buffer-%region :initform nil :initarg :%region)
   (%pathname :accessor buffer-%pathname :initform nil :initarg :%pathname)
   (modes :accessor buffer-modes :initform nil :initarg :modes)
   (mode-objects :accessor buffer-mode-objects :initform nil :initarg :mode-objects)
   (bindings :accessor buffer-bindings :initform nil :initarg :bindings)
   (.point. :accessor buffer-point-1 :initform nil :initarg :point)
   (%writable :accessor buffer-%writable :initform t :initarg :%writable)
   (modified-tick :accessor buffer-modified-tick :initform -2 :initarg :modified-tick)
   (unmodified-tick :accessor buffer-unmodified-tick :initform -1 :initarg :unmodified-tick)
   (windows :accessor buffer-windows :initform nil :initarg :windows)
   (var-values :accessor buffer-var-values :initform nil :initarg :var-values)
   (variables :accessor buffer-variables :initform nil :initarg :variables)
   (write-date :accessor buffer-write-date :initform nil :initarg :write-date)
   (active-region-p :accessor buffer-active-region-p :initform nil :initarg :active-region-p)
   (display-start :accessor buffer-display-start :initform nil :initarg :display-start)
   (%modeline-fields :accessor buffer-%modeline-fields :initform nil :initarg :%modeline-fields)
   (delete-hook :accessor buffer-delete-hook :initform nil :initarg :delete-hook)
   (widget :accessor buffer-widget :initform nil :initarg :widget)
   (tag-line-number :accessor buffer-tag-line-number :initform -1 :initarg :tag-line-number)
   (undo-p :accessor buffer-undo-p :initform t :initarg :undo-p)
   (undo-list :accessor buffer-undo-list :initform nil :initarg :undo-list)

   (plist :accessor buffer-plist :initform nil)))

#+NIL
(defmethod (setf buffer-active-region-p) (value (buffer text-editor-buffer))
  (setf (%buffer-active-region-p buffer) value))

(defmethod buffer-point (buffer)
  (unless buffer
    (cl-user::print-backtrace :stream #+SBCL sb-sys::*stdout* #-sbcl *debug-io*)
    (finish-output #+sbcl sb-sys::*stdout* #-sbcl *debug-io*))
  (buffer-point-1 buffer))

(defmethod internal-make-buffer (&rest args)
  (apply #'make-instance 'text-editor-buffer args))

(defmethod print-object ((object text-editor-buffer) stream)
  (%print-hbuffer object stream nil))

(defun hi::bufferp (thing)
  (typep thing 'text-editor-buffer))

#+NIL
(defclass vktk::vktk-device (hemlock.x11::bitmap-device)
  ((cursor-hunk :initform nil
                :documentation "The hunk that has the cursor."
                :accessor device-cursor-hunk)
   (cursor-item :initform nil
                :accessor device-cursor-item)))
