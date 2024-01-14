(in-package :climish)

(defcommand insert-character-command (window character p)
  (:pretty-name "Insert Character")
  (:documentation "This command is the value for the default command.")
  (:help-string "This command is the value for the default command.")
  (:preparation (list
		 (or (focused-window application-frame) *echo-area-window*)

		 ;; potential race condition:
		 ;; character-event has 1/100th of a second to show up in queue
		 ;; otherwse event is ignored
		 ;; so if computer is running very slowly character events will be dropped
		 (let ((event (get-next-event application-frame 0.01)))
			     
		   (if event

		       (progn
			 (setf (window-last-event application-frame) event)
			 (let ((event-id (input-event-id event)))
				     
			   (if (characterp event-id)
				       
			       event-id
				       
			       (throw :reset nil))))

		       (throw :reset nil)))

		 (universal-argument application-frame)))
  
  (:applicability (hemlock-applicability application-frame))
  (:generic-function-class text-editor-command)
  (:resolution (values)))

(defmethod insert-character-command (window character p)
  (handle-character-input window character (or p 1)))

(defclass application-frame-mixin (event-queue-mixin krma-window-mixin)
  ((vk::clear-value :initform (make-array 4 :element-type 'single-float
					    :initial-contents (list 0.84f0 0.84f0 0.84f0 1.0f0)))
   (focused-window :initform nil :accessor focused-window)
   (resize-window :initform nil :accessor window-to-resize)
   (drag-window :initform nil :accessor window-to-drag)
   (drag-last-x :initform nil :accessor drag-last-x)
   (drag-last-y :initform nil :accessor drag-last-y)
   (last-event :initform nil :accessor window-last-event)
   (popup-window :initform nil :accessor popup-window)
   (command-loop-go-semaphore :initform (bt:make-semaphore :name "command-loop-go-semaphore") :accessor command-loop-go-semaphore)
   (default-command :initform #'insert-character-command :accessor default-command)
   (universal-argument :initform nil :accessor universal-argument)
   (applicable-commands :initform nil :accessor applicable-commands)
   (command-table :initform (make-hash-table :test 'eq) :accessor command-table)
   (command-stack :initform nil :accessor command-stack)
   (selection-set :initform nil :accessor selection-set)
   (echo-area-input-mode :initform nil :accessor echo-area-input-mode)))

(defmethod clim:handle-event ((window application-frame-mixin) (event clui::input-event-mixin))
  ;; this forwarding logic is done in a primary method because
  ;; if it were done in an :around method, any :before or :after events
  ;; directed to the application frame would not run
  ;; even though those :before and :after events do not specialize on view-mixins
  ;; it is important that they run, because they do things like enqueue events
  ;; into the event queue for command processing and manage view-mixins for resizing
  ;; drag and drop and such at the whole application-frame level.
  (let ((2d-object (krma::most-specifically-hovered-2d-object
		    (krma::krma-select-box-2d (clui::window-display window)) 0 0)))

    (if (view-p 2d-object)

	;; forward event to view
	(clim:handle-event 2d-object event)

	(progn

	  (when (typep event 'clim:pointer-button-press-event)
	    ;; since we know this event was not forwarded,
	    ;; set the focused-window to nil, meaning, the focus is on the
	    ;; application frame itself and not any of it's homemade views
	    ;; this ensures the context can be computed correctly for
	    ;; command applicability
	    (setf (focused-window window) nil))
	
	  (call-next-method)))))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::window-resize-event-mixin))
  ;; if window-framebuffer-size is already correct for application-frame
  ;; then bashing this attribute will cause the echo area size and position to recompute
  (when hi::*echo-area-window*
    (setf (send hi::*echo-area-window* parent) window)
    (rm-redraw-node-recursively hi::*echo-area-window*))
  (values))

(defmethod initialize-instance :after ((window application-frame-mixin) &rest initargs)
  (declare (ignore initargs))
  (start-command-loop window)
  (values))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::character-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clim:key-press-event))
  (let ((event-id (input-event-id event)))
    (unless (modifier-key-p event-id)
      ;; modifier key events like Control key by itself are filtered out, it would just add noise
      ;; to the gesture-processor
      ;; the gesture processor sees the modifiers on regular keys
      (lparallel.queue:push-queue event (input-event-queue window)))))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clim:pointer-button-press-event))
  (let* ((dpy (clui::window-display window))
	 (select-box-2d (krma::krma-select-box-2d dpy))
	 (select-box-3d (krma::krma-select-box-3d dpy)))
    (setf (clui::clicked-items event) (append (krma::all-hovered-2d select-box-2d 0 0)
					      (krma::all-hovered-3d select-box-3d 0 0)))
    
    ;; make sure the event-queue sees this event
    (lparallel.queue:push-queue event (input-event-queue window))))

(defmethod clim:handle-event :after ((self application-frame-mixin) (event clim:pointer-button-release-event))
  (let ((resize-window (window-to-resize self)))
    (when resize-window  
      (setf (resize-mode resize-window) nil)
      (setf (window-to-resize self) nil)
      ;; create a resize event for a subwindow
      (clim:handle-event resize-window (make-instance 'clui:window-resize-event :window resize-window))))

  (setf (window-to-drag self) nil)

  ;; make sure the event-queue sees this event
  (lparallel.queue:push-queue event (input-event-queue self))
  (values))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::pointer-wheel-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::spaceball-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::joystick-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((self application-frame-mixin) (event clim:pointer-motion-event))
  (let ((resize-window (window-to-resize self)))
    (when resize-window
      (multiple-value-bind (x y) (clui:window-position resize-window)
	(multiple-value-bind (width height) (clui:window-size resize-window)
	  (ecase (resize-mode resize-window)
	    (:north (resize-window resize-window
				   x
				   (clim:pointer-event-y event)
				   width
				   (max (+ (- y (clim:pointer-event-y event)) height) 20)))
	    (:northwest (resize-window resize-window
				       (clim:pointer-event-x event)
				       (clim:pointer-event-y event)
				       (max (+ width (- x (clim:pointer-event-x event))) 20)
				       (max (+ (- y (clim:pointer-event-y event)) height) 20)))
	    (:northeast (resize-window resize-window
				       x
				       (clim:pointer-event-y event)
				       (max (- (clim:pointer-event-x event) x) 20)
				       (max (+ (- y (clim:pointer-event-y event)) height) 20)))
	    (:south (resize-window resize-window
				   x
				   y
				   width
				   (max (+ (- (clim:pointer-event-y event) y)) 20)))
	    (:southwest (resize-window resize-window
				       (clim:pointer-event-x event)
				       y
				       (max (+ width (- x (clim:pointer-event-x event))) 20)
				       (max (+ (- (clim:pointer-event-y event) y)) 20)))
	    (:southeast (resize-window resize-window
				       x
				       y
				       (max (- (clim:pointer-event-x event) x) 20)
				       (max (+ (- (clim:pointer-event-y event) y)) 20)))
	    (:east (resize-window resize-window
				  x
				  y
				  (max (- (clim:pointer-event-x event) x) 20)
				  height))
	    (:west (resize-window resize-window
				  (clim:pointer-event-x event)
				  y
				  (max (+ width (- x (clim:pointer-event-x event))) 20)
				  height)))))))
  
  (let ((drag-window (window-to-drag self)))
    (when drag-window

      (change-class event 'clui::pointer-button-hold-and-drag-event-mixin)
      
      (setf (clui::pointer-drag-delta-x event) (- (clim:pointer-event-x event) (drag-last-x self)))
      
      (setf (clui::pointer-drag-delta-y event) (- (clim:pointer-event-y event) (drag-last-y self)))
	  
      (clim:handle-event self event)))

  ;; make sure the event-queue sees this event:
  (lparallel.queue:push-queue event (input-event-queue self))
  (values))

(defmethod clim:handle-event ((self application-frame-mixin) (event clui::pointer-button-hold-and-drag-event-mixin))
  (setf (drag-last-x self) (clim:pointer-event-x event))
  (setf (drag-last-y self) (clim:pointer-event-y event))
  (let* ((dx (clui::pointer-drag-delta-x event))
	 (dy (clui::pointer-drag-delta-y event)))
    (multiple-value-bind (x y) (window-position (window-to-drag self))
      (set-window-position (window-to-drag self) (+ x dx) (+ y dy)))))

(defclass application-frame (application-frame-mixin)
  ())


(defmethod initialize-instance :after ((window application-frame) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (width height) (window-framebuffer-size window)
    (setf (krma::window-viewports window)
	  (list (krma::make-viewport
		 :x 0 :y 0
		 :width width
		 :height height
		 :2d-camera (krma::make-camera
			     :proj-matrix (mortho-vulkan 0 width height 0 0 krma::+select-box-2d-depth+)
			     :view-matrix (mlookat (vec3 0 0 krma::+select-box-2d-depth+) (vec3 0 0 0) (vec3 0 1 0)))
		 :3d-camera (krma::make-camera
			     :proj-matrix (mperspective-vulkan
					   45 (/ width height)
					   krma::*default-znear* krma::*default-zfar*)
			     :view-matrix (mlookat (vec3 0 0 1500) (vec3 0 0 0) (vec3 0 1 0)))))))
  (values))



#+win32
(defclass win32::application-frame (application-frame-mixin win32:krma-enabled-window-mixin) ())

(defclass x11::application-frame (application-frame-mixin x11:krma-enabled-window-mixin) ())

#+cocoa
(defclass cocoa::application-frame (application-frame-mixin cocoa:krma-enabled-window-mixin) ())



#+win32
(defmethod clui::get-a-win32-window-class ((display win32:display-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'win32::application-frame errorp))

#+x11
(defmethod clui::get-an-x11-window-class ((display x11:local-server-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'x11::application-frame errorp))

#+cocoa
(defmethod clui::get-a-cocoa-window-class ((display cocoa:display-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'cocoa::application-frame errorp))
