(in-package :climish)

(defcommand insert-character-command (window character p)
  (:pretty-name "Insert Character")
  (:documentation "This command is the value for the default command.")
  (:help-string "This command is the value for the default command.")
  (:preparation (list
		 
		 (focused-view (or (focused-window application-frame) *echo-area-window*))

		 ;; potential race condition:
		 ;; character-event has 1/100th of a second to show up in queue
		 ;; otherwse event is ignored
		 ;; so if computer is running very slowly character events will be dropped
		 (let ((event (read-event application-frame :timeout 0.01)))
			     
		   (if event

		       (progn
			 (setf (window-last-event application-frame) event)
			 (let ((event-id (input-event-id event)))
				     
			   (if (characterp event-id)
				       
			       event-id
			       
			       (throw :abort nil))))

		       (throw :abort nil)))

		 (universal-argument application-frame)))
  
  (:applicability (hemlock-applicability application-frame))
  (:generic-function-class text-editor-command)
  (:resolution (values)))

(defmethod insert-character-command (window character p)
  (handle-character-input window character (or p 1)))

(defclass application-frame-mixin
    (input-protocol-mixin trivial-gray-streams:fundamental-character-output-stream krma-window-mixin)
  ((vk::clear-value :initform (make-array 4 :element-type 'single-float
					    :initial-contents (list 0.84f0 0.84f0 0.84f0 1.0f0)))
   (focused-window :accessor focused-window)
   (resize-window :initform nil :accessor window-to-resize)
   (drag-window :initform nil :accessor window-to-drag)
   (dragging-select-box? :initform nil :accessor dragging-select-box?)
   (drag-initial-x :initform nil :accessor drag-initial-x)
   (drag-initial-y :initform nil :accessor drag-initial-y)
   (drag-last-x :initform nil :accessor drag-last-x)
   (drag-last-y :initform nil :accessor drag-last-y)
   (last-event :initform nil :accessor window-last-event)
   (last-keyboard-event :initform nil :accessor window-last-keyboard-event)
   (maybe-drag :initform nil :accessor window-maybe-drag)
   (popup-window :initform nil :accessor popup-window)
   (command-loop-go-semaphore :initform (bt:make-semaphore :name "command-loop-go-semaphore") :accessor command-loop-go-semaphore)
   (default-command :initform #'insert-character-command :accessor default-command)
   (universal-argument :initform nil :accessor universal-argument)
   (applicable-commands :initform nil :accessor applicable-commands)
   (command-table :initform (make-hash-table :test 'eq) :accessor command-table :accessor frame-command-table)
   (command-stack :initform nil :accessor command-stack)
   (selection-set :initform nil :accessor selection-set)
   (input-context :initform nil :accessor input-context)
   (echo-area-input-mode :initform nil :accessor echo-area-input-mode)
   (hovered :initform () :accessor hovered)
   (hovered-3d :initform () :accessor hovered-3d)
   (ll-package :initform :krma :accessor lisp-listener-package)))

(defmethod focused-view ((frame application-frame-mixin))
  (focused-window frame))

(defmethod clim:handle-event ((window application-frame-mixin) (event clui::input-event-mixin))
  ;; this forwarding logic is done in a primary method because
  ;; if it were done in an :around method, any :before or :after events
  ;; directed to the application frame would not run
  ;; even though those :before and :after events do not specialize on view-mixins
  ;; it is important that they run, because they do things like enqueue events
  ;; into the event queue for command processing and manage view-mixins for resizing
  ;; drag and drop and such at the whole application-frame level.
  (let ((2d-object (most-specifically-hovered-2d-object
		    (krma::krma-select-box-2d window) 0 0)))

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
	    (setf (focused-window window) window)
	    (compute-applicable-commands window))
	
	  (call-next-method)))))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::window-resize-event-mixin))
  ;; if window-framebuffer-size is already correct for application-frame
  ;; then bashing this attribute will cause the echo area size and position to recompute
  (when hi::*echo-area-window*
    (let ((self hi::*echo-area-window*))
      (rm-erase-node-recursively self)
      (setf (the parent) window)
      (resize-text-editor-window-before self)
      (adhoc-scene-graph::rm-draw-node-recursively self)))
  (values))

(defmethod clim:handle-event ((window application-frame-mixin) (event clui::window-monitor-switched-event-mixin))
  (when (boundp '*style*)
    (setf (send *style* monitor) (window-monitor window))
    (let* ((viewports (krma::window-viewports window))
	   (scenes (mapcar #'krma::viewport-scene viewports))
	   (windows (remove-if-not #'view-p (alexandria:flatten (mapcar #'krma::node-children scenes)))))
    (mapcar #'(lambda (window)
		(when (typep window 'hi::window)
		  (invalidate-hemlock-window-size window))
		#+NIL(rm-redraw-node-recursively window))
	    windows)))
  (values))

(defmethod initialize-instance :before ((window application-frame-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (focused-window window) window)
  (values))

(defmethod initialize-instance :after ((window application-frame-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (krma::immediate-mode-work-function-5 (clui:window-display window))
	(lambda ()
	  (monitor-select-boxes window)))
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

  (let ((select-box-2d (krma::krma-select-box-2d window))
	(select-box-3d (krma::krma-select-box-3d window)))
    (setf (clui::clicked-items event) (append (all-hovered-2d select-box-2d 0 0)
					      (all-hovered-3d select-box-3d 0 0)))
    
    ;; make sure the event-queue sees this event
    (lparallel.queue:push-queue event (input-event-queue window)))
  (values))

(defmethod clim:handle-event :after ((self application-frame-mixin) (event clim:pointer-button-release-event))
  (let ((resize-window (window-to-resize self)))
    (when resize-window  
      (setf (resize-mode resize-window) nil)
      (setf (window-to-resize self) nil)
      ;; create a resize event for a subwindow
      (clim:handle-event resize-window (make-instance 'clui:window-resize-event :window resize-window))))

  (setf (window-to-drag self) nil)
  (setf (window-maybe-drag self) nil)

  ;; make sure the event-queue sees this event
  (lparallel.queue:push-queue event (input-event-queue self))
  (values))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::pointer-wheel-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::spaceball-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event :after ((window application-frame-mixin) (event clui::joystick-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clim:handle-event ((self application-frame-mixin) (event clim:pointer-motion-event))
  (let ((2d-object (most-specifically-hovered-2d-object
		    (krma::krma-select-box-2d self) 0 0)))

    (if (view-p 2d-object)

	;; forward event to view
	(clim:handle-event 2d-object event)

	(if (window-maybe-drag self)

	    (let* ((start-x (clim:pointer-event-x (window-maybe-drag self)))
		   (start-y (clim:pointer-event-y (window-maybe-drag self)))
		   (monitor (window-monitor self))
		   (monitor-width-mm (clui::monitor-width-mm monitor))
		   (video-mode (clui::get-monitor-video-mode monitor)) 
		   (monitor-width-px (clui::video-mode-width video-mode))
		   (px-per-mm (/ monitor-width-px monitor-width-mm))
		   (curr-x (clim:pointer-event-x event))
		   (curr-y (clim:pointer-event-y event))
		   (delta-px (sqrt (+ (expt (- curr-x start-x) 2)
				      (expt (- curr-y start-y) 2))))
		   (delta-mm (/ delta-px px-per-mm)))

	      (when (> delta-mm 2)
		    
		(setf (dragging-select-box? self) t)
		(setf (window-maybe-drag self) nil)
		    
		(setf (drag-initial-x self) start-x)
		(setf (drag-initial-y self) start-y)
		    
		(setf (krma::krma-select-box-x0 self) start-x)
		(setf (krma::krma-select-box-y0 self) start-y)
		    
		(change-class event 'clui::pointer-button-hold-and-drag-event-mixin)
		    
		(setf (clui::pointer-drag-delta-x event) (- (clim:pointer-event-x event)
							    start-x))
		    
		(setf (clui::pointer-drag-delta-y event) (- (clim:pointer-event-y event)
							    start-y))
		    
		(clim:handle-event self event)))

	    (if (dragging-select-box? self)
	    
		(progn
		  (setf (krma::krma-select-box-x1 self) (clim:pointer-event-x event))
		  (setf (krma::krma-select-box-y1 self) (clim:pointer-event-y event))

		  (setf (krma::immediate-mode-work-function-4 (window-display self))
			(draw-select-box-closure-generator
			 self (krma::viewport-scene (first (krma::window-viewports self)))))
	      
		  (monitor-selection-set self))

		(let ((last-event (clui::display-last-event (clui::window-display self))))

		  (if (and (typep last-event 'clui::pointer-button-press-event-mixin)
			   (eq clui::+pointer-left-button+ (clui::input-event-code last-event)))

		      (progn

			(setf (window-maybe-drag self) last-event))
	  
		      (let ((mouse-x (clim:pointer-event-x event))
			    (mouse-y (clim:pointer-event-y event)))
	      
			(setf (krma::krma-select-box-x0 self) mouse-x)
			(setf (krma::krma-select-box-x1 self) mouse-x)
			(setf (krma::krma-select-box-y0 self) mouse-y)
			(setf (krma::krma-select-box-y1 self) mouse-y)))))))
	
	  (values)))

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
  
  (when (window-to-drag self)
    (let* ((dx (clui::pointer-drag-delta-x event))
	   (dy (clui::pointer-drag-delta-y event)))
      (multiple-value-bind (x y) (window-position (window-to-drag self))
	(set-window-position (window-to-drag self) (+ x dx) (+ y dy)))))

  (when (dragging-select-box? self)
    #+NIL
    (setf (krma::krma-select-box-coords self) (vec4 (drag-initial-x self)
						    (drag-initial-y self)
						    (clim:pointer-event-x event)
						    (clim:pointer-event-y event))))
  
  (values))

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

(defun recursive-command-loop (application-frame)
  (compute-applicable-commands application-frame)
  (let ((command))
    (tagbody
     again
       (catch :abort
	 (setq command (read-frame-command application-frame))
	 (climish::maybe-throw-presentation command (climish::input-context application-frame) nil nil)
	 (invoke-command command application-frame))
       (go again))))


#+NIL
(define-presentation-type not-everywhere-command ()
  :inherit-from 't)


#+NIL
(define-presentation-method presentation-typep (object (type not-everywhere-command))
  (and (presentation-typep object 'command)
       (not (presentation-typep object 'everywhere-command))))

#+NIL
(define-presentation-method presentation-subtypep ((sub rgn::cad-command)
						   (super (eql 'not-everywhere-command)))
  (and (presentation-subtypep sub 'command)
       (not (presentation-subtypep sub 'everywhere-command))))

#+NIL
(define-presentation-method presentation-subtypep ((sub rgn::cad-command) (super (eql 'not-everywhere-command)))
  t)

#+NIL
(define-presentation-method presentation-subtypep ((sub rgn::cad-command) (super command))
  t)
#+NIL
(define-presentation-method accept ((type not-everywhere-command) (stream application-frame-mixin)
								  view &key)
  (catch :accept
    (recursive-command-loop stream)))
#+NIL
(define-presentation-method accept ((type climish::command) (stream application-frame-mixin)
								  view &key)
  (catch :accept
    (recursive-command-loop stream)))




(defmethod default-frame-top-level ((application-frame application-frame-mixin))
  (compute-applicable-commands application-frame)
  (let (#+NOTYET(#+CCL *debugger-hook* #+SBCL sb-ext:*invoke-debugger-hook* #'gui-debug-condition))
    (restart-bind ((ignore #'(lambda (&optional c)
			       (declare (ignorable c))
			       (throw :reset nil))))
      
      (handler-bind
	  
	  ((editor-error #'(lambda (c)
			     (mapcar (lambda (buffer)
				       (bt:release-lock (buffer-lock buffer)))
				     hi::*buffer-list*)
			     (clear-echo-area)
			     (let ((string (editor-error-format-string c)))
			       (when string
				 (apply #'message string
					(editor-error-format-arguments c)))
			       (throw :abort nil)))))

	(let ((thing))
	  
	  (tagbody
	   restart
	     (setq thing (catch :reset
			   (catch :abort
			     (read-frame-command application-frame))))
	   invoke
	     (catch :reset
	       (catch :abort
		 (invoke-command thing application-frame)))
	     
	     (go restart)))))))
	     


(defun read-frame-command (application-frame)  
  (let ((command-table (command-table application-frame))
	(event)
	(event-id)
	(table-chain)
	(event-chain)
	(result))

    (tagbody
       
     RESET 
       (setq event-chain ())
       (setq table-chain (list command-table))
       
     GET (setq event (read-event application-frame))
       (unless event (go GET))
       (setf (window-last-event application-frame) event)
       (when (typep event 'clim:key-press-event)
	 (setf (window-last-keyboard-event application-frame) event))
       (setq event-id (input-event-id event))

     CHECK 
       
       (cond ((characterp event-id)
	      ;; we don't accept character events here
	      ;; without being first preceded by a key event
	      (go RESET))

	     ((eq event-id #k"C-g")

	      ;;(hi::invoke-hook hemlock::abort-hook)
			   
	      (message "Abort")
			   
	      (when table-chain
		;; go up one level
		(pop table-chain))

	      (pop (command-stack application-frame))
	      ;;(compute-applicable-commands application-frame)
	      (let* ((focused-window (focused-window application-frame))
		     (focused-view (focused-view focused-window)))
		(when (typep focused-view 'text-editor-view-mixin)
		  (deactivate-region (window-buffer focused-view))))
	      (setf (dragging-select-box? application-frame) nil)
	      
	      (unless table-chain
		;; we've over aborted:
		(go RESET))

	      (throw :abort nil)
	      #+NIL
	      (go GET))
				
	     (t (push event-id event-chain)
		(setq result (gethash event-id (car table-chain)))

		(cond ((null result)

		       (let ((command (default-command application-frame)))
			 (when command
			   (return-from read-frame-command command)))

		       (go RESET))
		       
		      ((hash-table-p result)
		       (push result table-chain)
		       (GO GET))

		      (t (return-from read-frame-command result))))))))

(defun start-command-loop (application-frame)
  (bt:make-thread #'(lambda ()
		      (bt:wait-on-semaphore (command-loop-go-semaphore application-frame))
		      (default-frame-top-level application-frame)) :name "command loop")
  #+SBCL(sb-thread:release-foreground (bt:current-thread)))

(defmethod trivial-gray-streams:stream-write-char ((stream application-frame-mixin) character)
  (write-char hi::*echo-area-stream* character)
  (values))

(defmethod trivial-gray-streams:stream-finish-output ((stream application-frame-mixin))
  (finish-output hi::*echo-area-stream*))

(defmethod trivial-gray-streams:stream-force-output ((stream application-frame-mixin))
  (force-output hi::*echo-area-stream*))

(defmethod trivial-gray-streams:stream-write-string ((stream application-frame-mixin) string
				&optional (start 0) (end (length string)))
  (write-string string hi::*echo-area-stream* :start start :end end))



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

(defun draw-select-box-closure-generator (window scene)
  #'(lambda ()
      (when (dragging-select-box? window)
	(draw-select-box window scene))))

(defun draw-select-box (window scene)
  (scene-draw-filled-2d-rectangle-list
   scene :select-box #xff00ff33
   (list (krma::krma-select-box-x0 window) (krma::krma-select-box-y0 window)
	 (krma::krma-select-box-x1 window) (krma::krma-select-box-y1 window))	   
   0 0))

(defun most-specifically-hovered-2d (2d-select-box x y)
  (when 2d-select-box
    (loop for i from (1- krma::+select-box-2d-depth+) downto 0
	  do (when (not (zerop (aref 2d-select-box x y i)))
	       (return (aref 2d-select-box x y i)))
	  finally (return nil))))


(defun most-specifically-hovered-2d-object (2d-select-box x y)
  (when 2d-select-box
    (let ((hovered (most-specifically-hovered-2d 2d-select-box x y)))
      (when hovered
	(let ((object (object-from-id hovered)))
	  object)))))

(defun most-specifically-hovered-3d (3d-select-box x y)
  (when 3d-select-box
    (loop for i from 0 to (1- krma::+select-box-3d-depth+)
	  do (when (not (zerop (aref 3d-select-box x y i)))
	       (return (aref 3d-select-box x y i)))
	  finally (return nil))))

(defun most-specifically-hovered-3d-object (3d-select-box x y)
  (when 3d-select-box
    (let ((hovered (most-specifically-hovered-3d 3d-select-box x y)))
      (when hovered
	(let ((object (object-from-id hovered)))
	  object)))))

(defun all-hovered-2d (2d-select-box x y)
  (loop for i from 0 below krma::+select-box-2d-depth+
	with result = ()
	unless (zerop (aref 2d-select-box x y i))
	do (push (aref 2d-select-box x y i) result)
	finally (return result)))

(defun all-hovered-3d (3d-select-box x y)
  (loop for i from 0 below krma::+select-box-3d-depth+
	with result = ()
	unless (zerop (aref 3d-select-box x y i))
	do (push (aref 3d-select-box x y i) result)
	finally (return result)))

(defun all-hovered-ss (ss)
  (loop for i from 0 below krma::+select-box-3d-depth+
	with result = ()
	unless (zerop (aref ss i))
	do (push (aref ss i) result)
     finally (return result)))


(defun focused-hovered-2d (2d-select-box x y)
  (first (remove-if #'(lambda (list)
			(every #'zerop list))
		    (loop for i from (1- krma::+select-box-2d-depth+) downto (1- krma::+view-depth+) by krma::+view-depth+
		       collect (loop for j from i downto 0 repeat krma::+view-depth+
				  unless (zerop (aref 2d-select-box x y j))
				  collect (aref 2d-select-box x y j))))))

(defun monitor-select-boxes (window)
  (unless (dragging-select-box? window)
    (let* ((all-hovered (all-hovered-2d (krma::krma-select-box-2d window) 0 0)))
    
      (flet ((exit-event (hovered)
	       (let ((object (object-from-id hovered)))
		 (when object
		   (if (clui::window-p object)
		       (multiple-value-bind (x y) (window-cursor-position object)
			 (clim:handle-event object (make-instance 'clui::pointer-exit-event
								  :window object
								  :timestamp (get-internal-real-time)
								  :x x :y y)))
		       (clim:handle-event object (make-instance 'clui::pointer-exit-event
								:window object
								:timestamp (get-internal-real-time)))))))
	     (enter-event (hovered)
	       (let ((object (object-from-id hovered)))
		 (when object
		   (if (clui::window-p object)
		       (multiple-value-bind (x y) (window-cursor-position object)
			 (clim:handle-event object (make-instance 'clui::pointer-enter-event
								  :window object
								  :timestamp (get-internal-real-time)
								  :x x :y y)))
		       (clim:handle-event object (make-instance 'clui::pointer-enter-event
								:window object
								:timestamp (get-internal-real-time))))))))

	;;(print (krma-select-box-2d window))

	(let ((difference (set-difference (hovered window) all-hovered)))
	  ;;(format t "~%0: ~S" difference)
	  (mapcar #'exit-event difference))

	(let ((difference (set-difference all-hovered (hovered window))))
	  ;;(format t "~%1: ~S" difference)
	  (mapcar #'enter-event difference))

	;;(format t "~%2: ~S" all-hovered)
	(setf (hovered window) all-hovered)))

    (unless (hovered window)
  
      (let* ((all-hovered (list (most-specifically-hovered-3d (krma::krma-select-box-3d window) 0 0))))
	;;(print all-hovered)
	(flet ((exit-event (hovered)
		 (let ((object (object-from-id hovered)))
		   (when object
		     (if (clui::window-p object)
			 (multiple-value-bind (x y) (window-cursor-position object)
			   (clim:handle-event object (make-instance 'clui::pointer-exit-event
								    :window object
								    :timestamp (get-internal-real-time)
								    :x x :y y)))
			 (clim:handle-event object (make-instance 'clui::pointer-exit-event
								  :window object
								  :timestamp (get-internal-real-time)))))))
	       (enter-event (hovered)
		 (let ((object (object-from-id hovered)))
		   (when object
		     (if (clui::window-p object)
			 (multiple-value-bind (x y) (window-cursor-position object)
			   (clim:handle-event object (make-instance 'clui::pointer-enter-event
								    :window object
								    :timestamp (get-internal-real-time)
								    :x x :y y)))
			 (clim:handle-event object (make-instance 'clui::pointer-enter-event
								  :window object
								  :timestamp (get-internal-real-time))))))))

	  ;;(print (krma-select-box-2d dpy))

	  (let ((difference (set-difference (hovered-3d window) all-hovered)))
	    ;;(format t "~%0: ~S" difference)
	    (mapcar #'exit-event difference))

	  (let ((difference (set-difference all-hovered (hovered-3d window))))
	    ;;(format t "~%1: ~S" difference)
	    (mapcar #'enter-event difference))

	  ;;(format t "~%2: ~S" all-hovered)
	  (setf (hovered-3d window) all-hovered))))))

(defun monitor-selection-set (window)
  (let* ((all-hovered (all-hovered-ss (krma::krma-selection-set-table window))))

	(flet ((exit-event (hovered)
		 (let ((object (object-from-id hovered)))
		   (when object
		     (if (clui::window-p object)
			 (multiple-value-bind (x y) (window-cursor-position object)
			   (clim:handle-event object (make-instance 'clui::pointer-exit-event
								    :window object
								    :timestamp (get-internal-real-time)
								    :x x :y y)))
			 (clim:handle-event object (make-instance 'clui::pointer-exit-event
								  :window object
								  :timestamp (get-internal-real-time)))))))
	       (enter-event (hovered)
		 (let ((object (object-from-id hovered)))
		   (when object
		     (if (clui::window-p object)
			 (multiple-value-bind (x y) (window-cursor-position object)
			   (clim:handle-event object (make-instance 'clui::pointer-enter-event
								    :window object
								    :timestamp (get-internal-real-time)
								    :x x :y y)))
			 (clim:handle-event object (make-instance 'clui::pointer-enter-event
								  :window object
								  :timestamp (get-internal-real-time))))))))

	  ;;(print (krma-select-box-2d window))

	  (let ((difference (set-difference (hovered window) all-hovered)))
	    ;;(format t "~%0: ~S" difference)
	    (mapcar #'exit-event difference))

	  (let ((difference (set-difference all-hovered (hovered window))))
	    ;;(format t "~%1: ~S" difference)
	    (mapcar #'enter-event difference))

	  ;;(format t "~%2: ~S" all-hovered)
	  (setf (hovered window) all-hovered))))
