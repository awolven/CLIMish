(in-package :climish)

(defcommand insert-character-command (window character p)
  (:pretty-name "Insert Character")
  (:documentation "This command is the value for the default command.")
  (:help-string "This command is the value for the default command.")
  (:argument-computation (list
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
  
  (:applicability (hemlock-applicability application-frame)))

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
   (command-table :initform (make-hash-table :test 'eq) :accessor command-table)))

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

(defmethod clim:handle-event :after ((window application-frame-mixin)
				     (event clui::character-event-mixin))
  (let ((character (clui::input-event-character event)))
    (declare (ignorable character))
    (lparallel.queue:push-queue event (input-event-queue window))))

(defmethod clim:handle-event :after ((window application-frame-mixin)
				     (event clim:key-press-event))
  (let ((event-id (input-event-id event)))
    (unless (modifier-key-p event-id)
      (lparallel.queue:push-queue event (input-event-queue window)))))

(defmethod clim:handle-event :after ((window application-frame-mixin)
				     (event clim:pointer-button-press-event))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin)
				     (event clui::pointer-wheel-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin)
				     (event clui::spaceball-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin)
				     (event clui::joystick-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

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
