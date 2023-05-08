(in-package :hi)

(defclass window ()
  ((width-in-chars
    :accessor window-width
    :initform nil
    :initarg :width-chars
    :documentation "Width of the window in characters.")

   (height-in-lines
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

   (old-width-in-chars
    :accessor window-old-width-in-chars
    :initform 0)    

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
    :accessor window-the-sentinel
    :initarg :the-sentinel
    :documentation "This dis-line, which has several interesting properties, is used to end
  lists of dis-lines.")

   (font-family
    :accessor window-font-family
    :initform nil
    :initarg :font-family
    :documentation "The default font for a hemlock window.")))

(defmethod windowp (thing)
  (declare (ignore thing))
  nil)

(defmethod windowp ((thing window))
  t)

(defmethod initialize-instance :after ((view window)
				       &rest args
				       &key
					 (buffer (make-instance 'buffer))
					 (start (buffer-start-mark buffer))
					 (display-start (copy-mark start :right-inserting))
					 (old-start (copy-mark start :temporary))
					 (display-end (copy-mark start :right-inserting))
					 (point (copy-mark (buffer-point buffer)))
					 (tick -1)
					 (the-sentinel (make-the-sentinel))
					 (first-line (cons dummy-line the-sentinel))
					 (last-line the-sentinel)
					 (first-changed the-sentinel)
					 (last-changed first-line)
				       &allow-other-keys)
  (declare (ignore args))
  
  (setf (window-display-start view) display-start)
  (setf (window-old-start view) old-start)
  (setf (window-display-end view) display-end)
  (setf (window-tick view) tick)
  (setf (window-point view) point)
  (setf (window-first-line view) first-line)
  (setf (window-last-line view) last-line)
  (setf (window-first-changed view) first-changed)
  (setf (window-last-changed view) last-changed)
  (setf (window-the-sentinel view) the-sentinel)
  (setf (window-width view) 80)
  (setf (window-height view) 35)
  (setf (window-buffer view) buffer)

  (values))

(defmethod initialize-instance :around ((view window)
					&rest initargs
					&key &allow-other-keys)
  (declare (ignore initargs))

  (call-next-method)
  
  (do ((i (- (window-height view)) (1+ i))
       (res ()
	    (cons (make-window-dis-line (make-string (window-width view))) res)))
      ((= i (window-height view)) (setf (window-spare-lines view) res)))
  
  (invoke-hook hemlock::make-window-hook view)
  view)


(defclass buffer ()
  ((lock
    :reader buffer-lock
    :initform (bt:make-recursive-lock "buffer lock"))
   
   (open-line
    :accessor buffer-open-line
    :documentation "Line open for hacking on."
    :initform nil)
   
   (open-chars
    :accessor buffer-open-chars
    :documentation "A Vector of characters for hacking on."
    :initform (make-string 200))
   
   (left-open-pos
    :accessor buffer-left-open-pos
    :documentation "Index to first free character to left of mark in Open-Chars."
    :initform 0)
   
   (right-open-pos
    :accessor buffer-right-open-pos
    :documentation "Index to first used character to right of mark in Open-Chars."
    :initform 0)
   
   (line-cache-length
    :accessor buffer-line-cache-length
    :initform 200
    :documentation "Length of Open-Chars.")

   (*internal-temp-region*
    :initform (make-empty-region))

   (*internal-temp-mark*
    :initform (internal-make-mark nil nil :temporary))

   (last-was-undo-p
    :initform nil)
   
   (this-is-undo-p
    :initform nil)
   
   (undoing-undo-list
    :initform nil)

   (%name
    :accessor buffer-%name
    :initform nil
    :initarg :name)
   
   (%region
    :accessor buffer-%region
    :initform nil
    :initarg :%region)
   
   (%pathname
    :accessor buffer-%pathname
    :initform nil
    :initarg :%pathname)
   
   (modes
    :accessor buffer-modes
    :initform nil
    :initarg :modes)
   
   (mode-objects
    :accessor buffer-mode-objects
    :initform nil
    :initarg :mode-objects)
   
   (bindings :accessor buffer-bindings
	     :initform nil
	     :initarg :bindings)
   
   (.point.
    :accessor buffer-point
    :initform nil
    :initarg :point)
   
   (%writable
    :accessor buffer-%writable
    :initform t
    :initarg :%writable)
   
   (modified-tick
    :accessor buffer-modified-tick
    :initform -2
    :initarg :modified-tick)
   
   (unmodified-tick
    :accessor buffer-unmodified-tick
    :initform -1
    :initarg :unmodified-tick)
   
   (windows
    :accessor buffer-windows
    :initform nil
    :initarg :windows)
   
   (var-values
    :accessor buffer-var-values
    :initform nil
    :initarg :var-values)
   
   (variables
    :accessor buffer-variables
    :initform nil
    :initarg :variables)
   
   (write-date
    :accessor buffer-write-date
    :initform nil
    :initarg :write-date)
   
   (active-region-p
    :accessor buffer-active-region-p
    :initform nil
    :initarg :active-region-p)
   
   (display-start
    :accessor buffer-display-start
    :initform nil
    :initarg :display-start)
   
   (%modeline-fields
    :accessor buffer-%modeline-fields
    :initform nil
    :initarg :%modeline-fields)
   
   (delete-hook
    :accessor buffer-delete-hook
    :initform nil
    :initarg :delete-hook)
   
   (widget
    :accessor buffer-widget
    :initform nil
    :initarg :widget)
   
   (tag-line-number
    :accessor buffer-tag-line-number
    :initform -1
    :initarg :tag-line-number)
   
   (undo-p
    :accessor buffer-undo-p
    :initform t
    :initarg :undo-p)
   
   (undo-list
    :accessor buffer-undo-list
    :initform nil
    :initarg :undo-list)

   (plist
    :accessor buffer-plist
    :initform nil)))

(defmethod bufferp (thing)
  (declare (ignore thing))
  nil)

(defmethod bufferp ((thing buffer))
  t)

(defmethod print-object ((object buffer) stream)
  (%print-hbuffer object stream nil))

(defmethod initialize-instance :after ((buffer buffer)
				       &rest args
				       &key
					 (name "*scratch*")
					 current-buffer 
					 (modes (value hemlock::default-modes))
				       (modeline-fields
					(value hemlock::default-modeline-fields))
				       delete-hook
				       &allow-other-keys)
  (declare (ignore args))
  (unless (listp delete-hook)
    (error ":delete-hook is a list of functions -- ~S." delete-hook))

  (let* ((region (make-empty-region))
	 (object (getstring "Fundamental" *mode-names*)))

    (setf (buffer-%name buffer) name)
    (setf (buffer-%region buffer) region)
    (setf (buffer-modes buffer) (list (mode-object-name object)))
    (setf (buffer-mode-objects buffer) (list object))
    (setf (buffer-point buffer) (copy-mark (region-end region)))
    (setf (buffer-display-start buffer) (copy-mark (region-start region)))
    (setf (buffer-delete-hook buffer) delete-hook)
    (setf (buffer-variables buffer) (make-string-table))

    (sub-set-buffer-modeline-fields buffer modeline-fields)

    (setf (line-%buffer (mark-line (region-start region))) buffer)

    (push buffer *buffer-list*)

    (setf (getstring name *buffer-names*) buffer)

    (unless (equalp modes '("Fundamental"))
      (setf (buffer-major-mode buffer current-buffer) (car modes))
      (dolist (m (cdr modes))
	(setf (buffer-minor-mode buffer current-buffer m) t)))

    #+NOTYET
    (invoke-hook hemlock::make-buffer-hook buffer)

    (values)))
