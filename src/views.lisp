(in-package :krma.gui)

(defclass krma-gui-window-mixin (event-queue-mixin krma-window-mixin)
  ((vk::clear-value :initform (make-array 4 :element-type 'single-float
					    :initial-contents (list 0.84f0 0.84f0 0.84f0 1.0f0)))
   (focused-window :initform nil :accessor focused-window)
   (resize-window :initform nil :accessor window-to-resize)
   (drag-window :initform nil :accessor window-to-drag)
   (drag-last-x :initform nil :accessor drag-last-x)
   (drag-last-y :initform nil :accessor drag-last-y)
   (last-event :initform nil :accessor window-last-event)
   (command-loop-go-semaphore :initform (bt:make-semaphore :name "command-loop-go-semaphore") :accessor command-loop-go-semaphore)))

(defclass krma-gui-window (krma-gui-window-mixin)
  ())


(defmethod initialize-instance :after ((window krma-gui-window) &rest initargs)
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

(defmethod handle-event :after ((window krma-gui-window-mixin) (event clui::window-resize-event-mixin))
  ;; if window-framebuffer-size is already correct for krma-gui-window
  ;; then bashing this attribute will cause the echo area size and position to recompute
  (when hi::*echo-area-window*
    (setf (send hi::*echo-area-window* parent) window)
    (rm-redraw-node-recursively hi::*echo-area-window*))
  (values))

#+win32
(defclass win32::krma-gui-window (krma-gui-window win32:krma-enabled-window-mixin) ())

(defclass x11::krma-gui-window (krma-gui-window x11:krma-enabled-window-mixin) ())

#+cocoa
(defclass cocoa::krma-gui-window (krma-gui-window cocoa:krma-enabled-window-mixin) ())

#+win32
(defmethod clui::get-a-win32-window-class ((display win32:desktop-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'win32::krma-gui-window errorp))

#+x11
(defmethod clui::get-an-x11-window-class ((display x11:local-server-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'x11::krma-gui-window errorp))

#+cocoa
(defmethod clui::get-a-cocoa-window-class ((display cocoa:desktop-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'cocoa::krma-gui-window errorp))

(defmethod transform-coordinate ((vec 3dm.f:vec2) (node node-mixin))
  (transform-coordinate (vec3 (vx vec) (vy vec) 0) node))

(defmethod transform-coordinate ((vec 3dm.d::vec2) (node node-mixin))
  (transform-coordinate (vec3 (vx vec) (vy vec) 0) node))

(defmethod transform-coordinate ((vec 3dm.rat::vec2) (node node-mixin))
  (transform-coordinate (vec3 (vx vec) (vy vec) 0) node))

(defun font-size (font)
  (3b-bmfont-common:size (slot-value font 'krma::data)))

(defun calc-text-size (string font)
  (let* ((data (font-data font))
	 (glyph-table (slot-value data '3b-bmfont-common::chars)))
    
    (loop for i from 0 below (length string)
	  with glyph = nil
	  with width
	  with height
	  with dx = 0.0f0
	  with x0
	  with y0
	  with x1
	  with y1
	  with min-x = 0.0f0
	  with max-x = 0.0f0
	  with min-y = 0.0f0
	  with max-y = 0.0f0
	  do (setq glyph (gethash (char string i) glyph-table))
	     (setq width (float (3b-bmfont:glyph-width glyph) 1.0f0))
	     (setq height (float (3b-bmfont:glyph-height glyph) 1.0f0))
	     (setq x0 (+ dx (3b-bmfont:glyph-xoffset glyph)))
	     (setq y0 (3b-bmfont:glyph-yoffset glyph))
	     (setq x1 (+ x0 width))
	     (setq y1 (+ y0 height))
	   
	     (setq min-x (min x0 min-x))
	     (setq max-x (max x1 max-x))
	     (setq min-y (min y0 min-y))
	     (setq max-y (max y1 max-y))

	     (incf dx (float (3b-bmfont:glyph-xadvance glyph) 1.0f0))
	  finally (return (values min-x min-y max-x max-y)))))

(defobject 2d-box-mixin ()
  :inputs
  ((x (if (the parent) (send (the parent) x) 0))
   (y (if (the parent) (send (the parent) y) 0))
   (width (if (the parent) (send (the parent) width) 10))
   (height (if (the parent) (send (the parent) height) 10)))

  :attributes
  ((x0 :uncached (the x))
   (y0 :uncached (the y))

   (x1 (+ (the x) (the width)))
   (y1 (+ (the y) (the height)))))

(defobject client-rect (2d-box-mixin)
  :attributes
  ((parent (the superior))))

(defobject view-mixin (2d-box-mixin node-mixin)
  
  :inputs
  ((parent (the superior))
   (window :defaulting)
   (style :defaulting))

  :attributes
  ((layer (if (the superior)
	      (1+ (the superior layer))
	      0)))

  :components
  ((client-rect :type 'client-rect)))

(defmethod untransform-coordinate ((vec vec3) (self view-mixin))
  (let ((vec4 (m* (vec4 (- (vx vec) (the x)) (- (vy vec) (the y)) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod untransform-global-coordinate ((vec vec3) (self view-mixin))
  (let ((vec4 (m* (vec4 (- (vx vec) (the x)) (- (vy vec) (the y)) (vz vec) 1) (the inverse-global-matrix))))
    (safe-euclid vec4)))
		

(defmethod view-p (object)
  (declare (ignore object))
  nil)

(defmethod view-p ((object view-mixin))
  t)

(defobject group-owner-view-mixin (group-owner-mixin view-mixin))

(defobject border (selectable-mixin group-mixin view-mixin)
  :inputs
  ((color (the style border-color))
   (border-size 1)
   (base-layer 13)

   (x (ecase (the index)
	((:west :north :south) (+ (the window x) (- (/ (the border-size) 2))))
	((:east) (+ (the window x) (the window width) (- (/ (the border-size) 2))))))
   (y (ecase (the index)
	((:west :east :north) (+ (the window y) (- (/ (the border-size) 2))))
	((:south) (+ (the window y) (the window height) (- (/ (the border-size) 2))))))
   (width (ecase (the index)
	    ((:north :south) (+ (the border-size) (the window width)))
	    ((:east :west) (the border-size))))
   (height (ecase (the index)
	     ((:north :south) (the border-size))
	     ((:east :west) (+ (the border-size) (the window height))))))
  
  :attributes
  ((index (first (the indices)))
   (layer (ecase (the index)
	    ((:north :south) (+ (the base-layer) 1))
	    ((:east :west) (+ (the base-layer) 0))))))

(defobject resize-border (border)
  :inputs
  ((color #xffffff00)))



(defmethod clui:handle-event ((self resize-border) (event clui::pointer-enter-event-mixin))
  (let* ((north-id (the root (resize-borders :north) object-id))
	 (south-id (the root (resize-borders :south) object-id))
	 (east-id (the root (resize-borders :east) object-id))
	 (west-id (the root (resize-borders :west) object-id))
	 (display (clui::window-display (clui::window-parent (the root))))
	 (hovered (krma::focused-hovered-2d (krma::krma-select-box-2d display) 0 0))
	 (cursor-class (when hovered
			 (cond ((find north-id hovered)
				(cond ((find west-id hovered) 'clui::resize-NW-cursor)
				      ((find east-id hovered) 'clui::resize-NE-cursor)				      
				      (t 'clui::resize-N-cursor)))
			       ((find south-id hovered)
				(cond ((find west-id hovered) 'clui::resize-SW-cursor)
				      ((find east-id hovered) 'clui::resize-SE-cursor)				      
				      (t 'clui::resize-S-cursor)))
			       ((find east-id hovered) 'clui::resize-E-cursor)
			       ((find west-id hovered) 'clui::resize-W-cursor)
			       (t nil)))))
    (when cursor-class
      (setf (clui::window-cursor (clui::window-parent (the root))) (make-instance cursor-class :display display)))))

(defmethod clui:handle-event ((self resize-border) (event clui::pointer-button-press-event-mixin))
  (let* ((north-id (the root (resize-borders :north) object-id))
	 (south-id (the root (resize-borders :south) object-id))
	 (east-id (the root (resize-borders :east) object-id))
	 (west-id (the root (resize-borders :west) object-id))
	 (display (clui::window-display (clui::window-parent (the root))))
	 (hovered (krma::focused-hovered-2d (krma::krma-select-box-2d display) 0 0))
	 (resize-mode (when hovered
			 (cond ((find north-id hovered)
				(cond ((find west-id hovered) :northwest)
				      ((find east-id hovered) :northeast)				      
				      (t :north)))
			       ((find south-id hovered)
				(cond ((find west-id hovered) :southwest)
				      ((find east-id hovered) :southeast)				      
				      (t :south)))
			       ((find east-id hovered) :east)
			       ((find west-id hovered) :west)
			       (t nil)))))
    (setf (resize-mode (the root)) resize-mode)
    (when resize-mode
      (setf (window-to-resize (clui::window-parent (the root))) (the root)))
    (values)))

(defmethod clui:handle-event :after ((self krma-gui-window-mixin) (event clui::pointer-button-release-event-mixin))
  (let ((resize-window (window-to-resize self)))
    (when resize-window  
      (setf (resize-mode resize-window) nil)
      (setf (window-to-resize self) nil)
      (clui:handle-event resize-window (make-instance 'clui:window-resize-event :window resize-window))))

  (setf (window-to-drag self) nil)

  ;; make sure the event-queue sees this event
  (lparallel.queue:push-queue event (input-event-queue self))
  (values))



(defmethod clui:handle-event :after ((self krma-gui-window-mixin) (event clui::pointer-motion-event-mixin))
  (let ((resize-window (window-to-resize self)))
    (when resize-window
      (multiple-value-bind (x y) (clui:window-position resize-window)
	(multiple-value-bind (width height) (clui:window-size resize-window)
	  (ecase (resize-mode resize-window)
	    (:north (resize-window resize-window
				   x
				   (clui::pointer-event-y event)
				   width
				   (max (+ (- y (clui::pointer-event-y event)) height) 20)))
	    (:northwest (resize-window resize-window
				       (clui::pointer-event-x event)
				       (clui::pointer-event-y event)
				       (max (+ width (- x (clui::pointer-event-x event))) 20)
				       (max (+ (- y (clui::pointer-event-y event)) height) 20)))
	    (:northeast (resize-window resize-window
				       x
				       (clui::pointer-event-y event)
				       (max (- (clui::pointer-event-x event) x) 20)
				       (max (+ (- y (clui::pointer-event-y event)) height) 20)))
	    (:south (resize-window resize-window
				   x
				   y
				   width
				   (max (+ (- (clui::pointer-event-y event) y)) 20)))
	    (:southwest (resize-window resize-window
				       (clui::pointer-event-x event)
				       y
				       (max (+ width (- x (clui::pointer-event-x event))) 20)
				       (max (+ (- (clui::pointer-event-y event) y)) 20)))
	    (:southeast (resize-window resize-window
				       x
				       y
				       (max (- (clui::pointer-event-x event) x) 20)
				       (max (+ (- (clui::pointer-event-y event) y)) 20)))
	    (:east (resize-window resize-window
				  x
				  y
				  (max (- (clui::pointer-event-x event) x) 20)
				  height))
	    (:west (resize-window resize-window
				  (clui::pointer-event-x event)
				  y
				  (max (+ width (- x (clui::pointer-event-x event))) 20)
				  height)))))))
  
  (let ((drag-window (window-to-drag self)))
    (when drag-window

      (change-class event 'clui::pointer-button-hold-and-drag-event-mixin)
      
      (setf (clui::pointer-drag-delta-x event) (- (clui::pointer-event-x event) (drag-last-x self)))
      
      (setf (clui::pointer-drag-delta-y event) (- (clui::pointer-event-y event) (drag-last-y self)))
	  
      (clui:handle-event self event)))

  ;; make sure the event-queue sees this event:
  (lparallel.queue:push-queue event (input-event-queue self))
  (values))

(defgeneric resize-window (window new-x new-y new-width new-height))



(defmethod clui:handle-event ((self resize-border) (event clui::pointer-exit-event-mixin))
  (setf (clui::window-cursor (clui::window-parent (the root))) (make-instance 'clui::arrow-cursor :display (clui::window-display (clui::window-parent (the root)))))
  (values))

(defmethod rm-draw-node ((self resize-border))
  (let ((scene (the scene)))
    (push
     (scene-add-filled-2d-rectangle-list-primitive
      scene (the group) nil (the color)
      (list (the x0) (the y0) (the x1) (the y1))
      (the object-id)
      (the layer))
     (the primitive-handles))))

(defmethod rm-draw-node ((self border))
  (unless (the window iconified?)
    (let ((scene (the scene)))
      (push
       (scene-add-filled-2d-rectangle-list-primitive
	scene (the group) nil (the color)
	(list (the x0) (the y0) (the x1) (the y1))
	(if (typep (the root (resize-borders (the index))) 'null-object)
	    0
	    (the root (resize-borders (the index)) object-id))
	(the layer))
       (the primitive-handles)))))



(defobject title-bar-button-mixin (selectable-mixin group-mixin view-mixin)
  :inputs
  ((size))
  
  :attributes
  ((center-x (/ (+ (the x0) (the x1)) 2))
   (center-y (/ (+ (the y0) (the y1)) 2))
   (width (+ (the size) (* 2 (the style frame-padding-x))))
   (height (+ (the size) (* 2 (the style frame-padding-y))))))


(defobject iconify-button (title-bar-button-mixin))


(defobject close-button (title-bar-button-mixin)
  :attributes
  ((cross-extent (1- (* (the size) 0.5 0.7071)))))

(defobject transparency-button (title-bar-button-mixin)
  :inputs
  ((opaque? t)))


(defobject menu-bar (selectable-mixin group-mixin view-mixin))


(defobject title-bar (selectable-mixin group-mixin view-mixin)

  :inputs
  ((title)
   (button-size)
   
   (iconify-button? t)
   (close-button? t))
  
  :attributes
  ((height (the style title-bar-height))
   
   (pad-left (+ (the style frame-padding-x) (if (eq (the style window-menu-button-position) :left)
						  (the button-size)
						  0)))
   (pad-right (the style frame-padding-x)))
  
  :components
  ((close-button :type (if (the close-button?) 'close-button 'null-object)
		 group (the group)
		 size (the button-size)
		 x (- (the x1)
			   (the button-size)
			   (* 2 (the style frame-padding-x))))

   (iconfiy-button :type (if (the iconify-button?) 'iconify-button 'null-object)
		    group (the group)
		    size (the button-size)
		    x (ecase (the style window-menu-button-position)
			     (:left 0)
			     (:right (- (the x1)
					(if (the close-button?)
					    (* 2 (the button-size))
					    (the button-size))
					4
					(* 2 (the style frame-padding-x))))))
   (transparency-button :type 'transparency-button
			group (the group)
			size (the button-size)
			x (ecase (the style window-menu-button-position)
			     (:left 0)
			     (:right (- (the x1)
					(if (the close-button?)
					    (* 3 (the button-size))
					    (* 2 (the button-size)))
					(if (the close-button?)
					    8 4)				
					(* 2 (the style frame-padding-x))))))))

(defmethod clui:handle-event :after ((self title-bar) (event clui::pointer-button-release-event-mixin))

  (setf (window-to-drag (clui::window-parent (the root))) nil)
  
  (values))

(defmethod rm-draw-node ((self title-bar))
  (let* ((scene (the scene))
	 (style (the style))
	 (window (the window))
	 (group (the group))
	 (focused? (send window focused?)))
    
    (if (send window iconified?)
	(let ((title-bar-color (if focused? (send style title-bg-active-color) (send style title-bg-collapsed-color))))
	  ;; draw just title bar
	  (push
	   (scene-add-filled-2d-rectangle-list-primitive
	    scene group nil title-bar-color
	    (list (the x0) (the y0) (the x1) (the y1))
	    (the object-id)
	    (the layer))
	   (the primitive-handles)))
    
	(let ((title-bar-color (if focused? (send style title-bg-active-color) (send style title-bg-color))))
	  (push
	   (scene-add-filled-2d-rectangle-list-primitive
	    scene group nil title-bar-color
	    (list (the x0) (the y0) (the x1) (the y1))
	    (the object-id)
	    (the layer))
	   (the primitive-handles))))
	  

    (let ((layout-r-x0 (+ (the x0) (send style frame-padding-x)))
	  (layout-r-y0 (the y0)))

      (scene-add-text-primitive
       scene group nil (send style current-font) (send style text-color) layout-r-x0 layout-r-y0
       (send window title)
       (the object-id)
       (1+ (the layer))))))

(defclass ring-element-mixin ()
  ((next :initform nil :accessor next)
   (prev :initform nil :accessor prev)))

(defun ring-initialize (new-element)
  (setf (next new-element) new-element)
  (setf (prev new-element) new-element)
  (values))
  


(defun ring-link-new-element-at-start (new start)
  (let ((last (prev start)))
    (setf (next new) start)
    (setf (prev new) last)
    (setf (prev start) new)
    (setf (next last) new))
  (values))

(defun ring-remove-element (element)
  (setf (next (prev element)) (next element))
  (setf (prev (next element)) (prev element))
  (values))
  

(defobject homemade-window-mixin (ring-element-mixin selectable-mixin group-owner-view-mixin)
  :metaclass homemade-window-class

  
  :slots
  ((command-table :initform (make-hash-table :test #'eq) :reader command-table)
   (resize-mode :initform nil :accessor resize-mode))

  :inputs
  ((parent (main-window *app*))
   (scene (application-scene *app*))
   (style *style*)
   (font (the style current-font))
   (iconified? (:noticer (if value
			     (progn
			       (rm-erase-node-recursively self)
			       (setf (the focused?) nil)
			       (rm-draw-node-recursively (the title-bar)))
			     (setf (the focused?) t)))
	       nil)
   (focused? (:noticer (if value
			   (%focus-window self)
			   (%unfocus-window self)))
	     nil)
   (resizable? t)
   (menu-bar? nil)
   (scrollbar-x? nil)
   (scrollbar-y? nil)
   (title-bar? t)
   (iconify-button? t)
   (close-button? t)
   (rounding? nil)
   (alpha 0.7)
   
   (window self)
   
   (title "krma")

   (border-size 1)

   (x 0)
   (y 0)
   (width (the style default-initial-window-width))
   (height (the style default-initial-window-height)))

  :attributes
  ((z-index :modifiable 0)
   (layer (* (the z-index) krma::+view-depth+))
   (border-indices '((:north) (:south) (:east) (:west))))

  :components
  ((client-rect :type 'client-rect
		y (if (the title-bar?) (the title-bar y1) (the y))
		height (if (the title-bar?) (- (the height) (the title-bar height)) (the height)))

   (title-bar :type (if (the title-bar?) 'title-bar 'null-object)
	      title (the title)
	      iconify-button? (the iconify-button?)
	      close-button? (the close-button?)
	      button-size (font-size (the font)))

   (menu-bar :type (if (the menu-bar?) 'menu-bar 'null-object))

   (resize-borders :type (if (the resizable?) 'resize-border 'null-object)
		   :aggregate (:indices (the border-indices))
		   border-size 6
		   base-layer (+ (the layer) 11))

   (border :type 'border
	   :aggregate (:indices (the border-indices))
	   border-size 2
	   base-layer (+ (the layer) 13))

   (horizontal-scrollbar :type (if (the scrollbar-x?) 'scrollbar 'null-object)
			 axis :horizontal)

   (vertical-scrollbar :type (if (the scrollbar-y?) 'scrollbar 'null-object)
		       axis :vertical)))

(defmethod resize-window ((window homemade-window-mixin) new-x new-y new-width new-height)
  (clui:set-window-size window new-width new-height)
  (clui:set-window-position window new-x new-y)
  (values))

(defmethod node-parent ((self homemade-window-mixin))
  ;; node-parent ::= node in the scene graph
  (the scene))

(defmethod window-parent ((self homemade-window-mixin))
  (the parent))



(defmethod clui:handle-event ((window krma-window-mixin) (event clui::pointer-event-mixin))
  (let ((x (clui::pointer-event-x event))
	(y (clui::pointer-event-y event)))

    (let ((2d-object (krma::most-specifically-hovered-2d-object
		      (krma::krma-select-box-2d (clui::window-display window)) 0 0)))

      (if (view-p 2d-object)
	  (progn

	    (setf (clui::pointer-event-native-x event) x
		  (clui::pointer-event-native-y event) y)

	    (clui:handle-event 2d-object event))

	  (call-next-method)))))

(defmethod clui:handle-event ((window krma-window-mixin) (event clui::input-event-mixin))
  (let ((2d-object (krma::most-specifically-hovered-2d-object
		    (krma::krma-select-box-2d (clui::window-display window)) 0 0)))

    (if (view-p 2d-object)

	(clui:handle-event 2d-object event)
	
	(call-next-method))))

(defmethod clui:handle-event ((self title-bar) (event clui::pointer-motion-event-mixin))
  (let ((last-event (clui::display-last-event
		     (clui::window-display (window-parent (the window))))))

    (when (or (typep last-event 'clui::pointer-button-press-event-mixin))

      (setf (window-to-drag (clui::window-parent (the root))) (the window))

      (change-class event 'clui::pointer-button-hold-and-drag-event-mixin)

      (setf (clui::pointer-drag-delta-x event) (- (clui::pointer-event-x event) (clui::pointer-event-x last-event)))

      (setf (clui::pointer-drag-delta-y event) (- (clui::pointer-event-y event) (clui::pointer-event-y last-event)))
      
      (clui:handle-event (clui::window-parent (the root)) event))))

(defmethod clui:handle-event ((self krma-gui-window-mixin) (event clui::pointer-button-hold-and-drag-event-mixin))
  (setf (drag-last-x self) (clui::pointer-event-x event))
  (setf (drag-last-y self) (clui::pointer-event-y event))
  (let* ((dx (clui::pointer-drag-delta-x event))
	 (dy (clui::pointer-drag-delta-y event)))
    (multiple-value-bind (x y) (window-position (window-to-drag self))
      (set-window-position (window-to-drag self) (+ x dx) (+ y dy)))))

(defmethod window-cursor-position ((self view-mixin))
  (multiple-value-bind (x y) (window-cursor-position (window-parent (the root)))
    (let ((coord (untransform-global-coordinate (vec3 x y 0) self)))
      (values (vx coord) (vy coord)))))
      

(defmethod destroy-window ((homemade-window homemade-window-mixin))
  (rm-erase-node-recursively homemade-window)
  (ring-remove-element homemade-window)
  (let ((scene (send homemade-window scene)))
    ;; remove the graphical representation from the draw-lists
    (setf (krma::node-children scene)
	  (remove homemade-window (krma::node-children scene)))
    (when (eq homemade-window (focused-window (clui::window-parent homemade-window)))
      (setf (focused-window (clui::window-parent homemade-window)) nil))
    (forget-object-recursively homemade-window)
    (unless (krma::node-children scene)
      (setf (krma::immediate-mode-work-function-1 (clui::window-display (window-parent homemade-window))) nil))
    #+ccl(ccl:gc)
    #+sbcl(sb-sys::gc :full t)
    #+NOTNOW
    (unless (eq (next homemade-window) *echo-area-window*)
      (focus-window (next homemade-window)))
    (values)))

(defmethod clui:handle-event ((button close-button) (event clui::pointer-button-press-event-mixin))
  (let ((homemade-window (send button window)))
    (destroy-window homemade-window)))

(defmethod clui:handle-event ((self transparency-button) (event clui::pointer-button-press-event-mixin))
  (if (the opaque?)
      (setf (the opaque?) nil)
      (setf (the opaque?) t))
  (rm-redraw-node-recursively (the window))
  (values))

(defmethod clui:handle-event ((button iconify-button) (event clui::pointer-button-press-event-mixin))
  (let ((homemade-window (send button window)))
    (if (send homemade-window iconified?)
	(setf (send homemade-window iconified?) nil)
	(setf (send homemade-window iconified?) t))
    (values)))

(defmethod clui:handle-event ((button title-bar-button-mixin) (event clui::pointer-boundary-event-mixin))
  (call-next-method)
  (rm-redraw-node-recursively button)
  (values))

(defmethod clui:handle-event :before ((self selectable-mixin) (event clui::pointer-enter-event-mixin))
  (setf (the hovered?) t))

(defmethod clui:handle-event :before ((self selectable-mixin) (event clui::pointer-exit-event-mixin))
  (setf (the hovered?) nil))

(defmethod clui::window-p ((window homemade-window-mixin))
  t)

(defmethod %focus-window ((self homemade-window-mixin))
  (if (the iconified?)
      (setf (the iconified?) nil) ;; will reenter %focus-window with (the iconified?) --> nil
      (let ((scene (the scene)))
	(let ((siblings (remove self (krma::node-children scene))))
	  (setq siblings (sort siblings #'< :key #'(lambda (self) (the z-index))))
	  (loop for i from 0
		for sibling in siblings
		do (setf (send sibling z-index) i)
		finally (setf (the z-index) (length siblings))
			(mapcar #'rm-redraw-node-recursively (nconc siblings (list self)))
			(setf (focused-window (window-parent (the root))) self))
	  (values)))))

(defmethod %unfocus-window ((self homemade-window-mixin))
  (do ((next (next self) (next next)))
	((eq next self))
    (unless (send next iconified?)
      (setf (send next focused?) t)
      (return))))

(defmethod clui:focus-window ((self homemade-window-mixin))
  (unless (eq self (focused-window (clui::window-parent (the root))))
    (setf (the focused?) t))
  (values))

(defmethod clui::unfocus-window ((self homemade-window-mixin))
  (setf (the focused?) nil)
  (values))

(defmethod clui::iconify-window ((self homemade-window-mixin))
  (setf (the iconified?) t)
  (values))

(defmethod clui::restore-window ((self homemade-window-mixin))
  (when (the iconified? t)
    (setf (the iconified?) nil))
  (values))

(defmethod clui::window-iconified? ((self homemade-window-mixin))
  (the iconified?))

(defmethod (setf clui::window-iconified?) (value (self homemade-window-mixin))
  (setf (the iconified?) (and value t)))

(defmethod clui:window-position ((self view-mixin))
  (let* ((vec4 (m* (vec4 (the x) (the y) 0 1) (the matrix)))
	 (vec3 (safe-euclid vec4)))
    (values (vx vec3) (vy vec3) (vz vec3))))

(defmethod global-window-position ((self view-mixin))
  (let* ((vec4 (m* (vec4 (the x) (the y) 0 1) (the global-matrix)))
	 (vec3 (safe-euclid vec4)))
    (values (vx vec3) (vy vec3) (vz vec3))))
    
(defmethod clui:set-window-position ((self view-mixin) x y)
  (translate-node self (vec3 (- x (the x)) (- y (the y)) 0))
  (values))

(defmethod clui:set-window-position ((self group-owner-view-mixin) x y)
  (let* ((dx (- x (the x)))
	 (dy (- y (the y)))
	 (vec3 (vec3 dx dy 0)))
    (setf (the origin) vec3)
    (values)))      
  
(defmethod clui:window-size ((self view-mixin))
  (values (the width) (the height)))

(defmethod clui:set-window-size ((self group-owner-view-mixin) (width real) (height real))
  (setf (the width) width
	(the height) height)
  (rm-redraw-node-recursively self)
  (values))

(defmethod transform-coordinate ((vec vec2) (view view-mixin))
  (transform-coordinate (vec3 (vx vec) (vy vec) (send view layer)) view))

(defmethod initialize-instance :after ((self homemade-window-mixin) &rest initargs)
  (declare (ignore initargs))

  (let ((highest-z-index 0)
	(os-window (window-parent (the root))))
    
    (when (krma::node-children (the scene))
      (setq highest-z-index
	    (loop for part in (krma::node-children (the scene))
		  maximize (the-part z-index))))
	
    (setf (the z-index) (1+ highest-z-index))
    (setf (krma::node-children (the scene)) (append (krma::node-children (the scene)) (list self)))

    (if (focused-window os-window)
	
	(ring-link-new-element-at-start self (focused-window os-window))

	(ring-initialize self)))

  (unless (eq self (first (krma::node-children (the scene)))) ;; echo area window
    (let ((next (next self)))
      (if (eq next *echo-area-window*)
	  (set-window-position self 40 30)
	  (multiple-value-bind (x y) (window-position next)
	    (set-window-position self (+ x 40) (+ y 30))))))
			     

  (values))



(defmethod rm-draw-node ((self homemade-window-mixin))
  
  (unless (the iconified?)
    (let* ((style (the style))
	   (scene (the scene))
	   (group (the group))
	   (color (copy-seq (send style window-bg-color))))
      (when (the title-bar?)
	(when (the title-bar transparency-button opaque?)
	  (setf (aref color 3) 1.0f0)))
      ;; draw window background
      (scene-add-filled-2d-rectangle-list-primitive
       scene group nil color
       (list (the x0) (if (the title-bar?) (the title-bar y1) (the y0)) (the x1) (the y1))
       (the object-id)
       (the layer)))))

  

#+NOTYET
(defun get-window-scrollbar-box (window &optional (axis :vertical))
  (let* ((box-x0 (box-x0 window))
	 (box-y0 (box-y0 window))
	 (box-x1 (box-x1 window))
	 (box-y1 (box-y1 window))
	 (client-rect (window-client-rect window))
	 (client-x0 (box-x0 client-box))
	 (client-y0 (box-y0 client-box))
	 (client-x1 (box-x1 client-box))
	 (client-y1 (box-y1 client-box))
	 (border-size (window-border-size window))
	 (scrollbar-size (if (eq axis :horizontal)
			     (elt (window-scrollbar-sizes window) 0)
			     (elt (window-scrollbar-sizes window) 1))))
    (case axis
      (:horizontal (values client-x0 (max box-y0 (- box-y1 border-size scrollbar-size))
			   client-x1 box-y1))
      (otherwise
       (unless (eq axis :vertical)
	 (warn "`axis' ~S is not valid, wanted :horizontal or :vertical" axis))
       (values (max box-x0 (- box-x1 border-size scrollbar-size)) client-y0
	       box-x1 client-y1)))))

#+NOTYET
(defun add-scrollbar (scrollbar axis &key (style *style*))
  (let* ((window (window-parent scrollbar))
	 (scene (node-scene scrollbar))
	 (group (object-group scrollbar))
	 (client-rect (window-client-rect window))
	 (size-avail (if (eq axis :horizontal)
			 (- (box-x1 client-box)
			    (box-x0 client-rect))
			 (- (box-y1 client-rect)
			    (box-y0 client-rect))))
	 (size-contents (if (eq axis :horizontal)
			    (+ size-avail (* 2.0f0 (elt (window-padding window) 0)))
			    (+ size-avail (* 2.0f0 (elt (window-padding window) 1))))))
    (multiple-value-bind (bb-x0 bb-y0 bb-x1 bb-y1)
	(get-window-scrollbar-rect window axis)
      (scene-add-scrollbar scene group bb-x0 bb-y0 bb-x1 bb-y1
			   window
			   axis
			   (if (eq axis :horizontal)
			       (elt (scroll-amount window) 0)
			       (elt (scroll-amount window) 1))
			   size-avail size-contents
			   :style style))))

#+NOTYET
(defun scene-add-scrollbar (scene group bb-x0 bb-y0 bb-x1 bb-y1 window axis scroll-amount size-avail size-contents &key (style *style*))
  (let ((frame-width (- bb-x1 bb-x0))
	(frame-height (- bb-y1 bb-y0))
	(font-size (font-size (current-font style))))

    (unless (and (plusp frame-width) (plusp frame-height))
      (warn "invalid scrollbar size: ~S X ~S" frame-width frame-height)
      (return-from scene-add-scrollbar-1 (values)))

    (let ((alpha 1.0f0))
      (when (and (eq axis :vertical) (< frame-height (+ font-size (* 2.0f0 (frame-padding-y style)))))
	(setq alpha (saturate (/ (- frame-height font-size) (* 2.0f0 (frame-padding-y style)))))))
    (unless (plusp alpha)
      (return-from scene-add-scrollbar-1 (values)))

    (let ((allow-interaction? )))))
								     


			    

(defmethod rm-draw-node ((self close-button))
  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (send style current-font)))
	 (cross-color (send style text-color))
	 (object-id (the object-id))
	 (group (the group))
	 (layer (the layer))
	 (hovered? (the hovered?)))

    (push (scene-add-filled-2d-circle-primitive scene group nil (if hovered? (send style button-hovered-color) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						layer)
	  (the primitive-handles))

    (let ((cross-extent (the cross-extent)))
      (push (scene-add-2d-line-primitive scene group nil 2.0f0 cross-color
					 (+ (the center-x) cross-extent)
					 (+ (the center-y) cross-extent)
					 (- (the center-x) cross-extent)
					 (- (the center-y) cross-extent)
					 object-id
					 (1+ layer))
	    (the primitive-handles))
    
      (push (scene-add-2d-line-primitive scene group nil 2.0f0 cross-color
					 (+ (the center-x) cross-extent)
					 (- (the center-y) cross-extent)
					 (- (the center-x) cross-extent)
					 (+ (the center-y) cross-extent)
					 object-id
					 (1+ layer))
	    (the primitive-handles)))

    (values)))

(defmethod rm-draw-node ((self transparency-button))
  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (send style current-font)))
	 (cross-color (send style text-color))
	 (object-id (the object-id))
	 (group (the group))
	 (layer (the layer))
	 (hovered? (the hovered?)))

    (push (scene-add-filled-2d-circle-primitive scene group nil (if hovered? (send style button-hovered-color) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						layer)
	  (the primitive-handles))

    (let ((side-extent (max 2.0f0 (1+ (* 0.33f0 font-size))))
	  (side-extent2 (max 2.0f0 (1+ (* 0.22f0 font-size)))))
      (push (scene-add-2d-rectangle-primitive scene group nil 2.0f0 cross-color
					 (- (the center-x) side-extent)
					 (- (the center-y) side-extent)
					 (+ (the center-x) side-extent)
					 (+ (the center-y) side-extent)
					 object-id
					 (1+ layer))
	    (the primitive-handles))

      (when (the opaque?)
	(push (scene-add-filled-2d-rectangle-list-primitive
	       scene group nil cross-color
	       (list (- (the center-x) side-extent2)
		     (- (the center-y) side-extent2)
		     (+ (the center-x) side-extent2)
		     (+ (the center-y) side-extent2))
	       object-id
	       (1+ layer))
	    (the primitive-handles)))

      
      #+NIL
      (push (scene-add-2d-line-primitive scene group nil 2.0f0 cross-color
					 (- (the center-x) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 (- (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 (+ (the center-x) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 (- (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 object-id
					 (1+ layer))
	    (the primitive-handles)))

    (values)))

(defun scene-add-arrow (scene group pos-x pos-y color dir scale object-id layer &key (style *style*))
  (let* ((h (font-size (send style current-font)))
	 (r (* h 0.40f0 scale))
	 (center-x (+ pos-x (* h 0.50f0)))
	 (center-y (+ pos-y (* h 0.50f0 scale)))
	 (a-x)
	 (a-y)
	 (b-x)
	 (b-y)
	 (c-x)
	 (c-y))

    (case dir
      ((:up :down)
       (when (eq dir :up) (setq r (- r)))
       (setq a-x 0.0f0
	     a-y (*  0.750 r)
	     b-x (* -0.866 r)
	     b-y (* -0.750 r)
	     c-x (*  0.866 r)
	     c-y (* -0.750 r)))
      ((:left :right)
       (when (eq dir :left) (setq r (- r)))
       (setq a-x (*  0.750 r)
	     a-y 0.0f0
	     b-x (* -0.750 r)
	     b-y (*  0.866 r)
	     c-x (* -0.750 r)
	     c-y (* -0.866 r)))
      (otherwise
       (warn "argument `dir' is not valid: ~S, needs to be :up, :down, :left, :right" dir)
       (setq a-x 0.0f0
	     a-y (*  0.750 r)
	     b-x (* -0.866 r)
	     b-y (* -0.750 r)
	     c-x (*  0.866 r)
	     c-y (* -0.750 r)))) ;; down

    (scene-add-filled-2d-triangle-list-primitive
     scene group nil color
     (list (+ center-x a-x) (+ center-y a-y)
	   (+ center-x b-x) (+ center-y b-y)
	   (+ center-x c-x) (+ center-y c-y))
     object-id
     layer)))

(defun scene-add-arrow-pointing-at (scene group pos-x pos-y half-size-x half-size-y color dir &optional (object-id 0) (layer 0))
  (let ((a-x)
	(a-y)
	(b-x)
	(b-y))
    
    (case dir
      (:left
       (setq a-x (+ pos-x half-size-x))
       (setq a-y (- pos-y half-size-y))
       (setq b-x (+ pos-x half-size-x))
       (setq b-y (+ pos-y half-size-y)))
      (:right
       (setq a-x (- pos-x half-size-x))
       (setq a-y (+ pos-y half-size-y))
       (setq b-x (- pos-x half-size-x))
       (setq b-y (- pos-y half-size-y)))
      (:up
       (setq a-x (+ pos-x half-size-x))
       (setq a-y (+ pos-y half-size-y))
       (setq b-x (- pos-x half-size-x))
       (setq b-y (+ pos-y half-size-y)))
      (otherwise
       (unless (eq dir :down)
	 (warn "argument `dir' is not valid: ~S, needs to be :up, :down, :left, :right" dir))
       (setq a-x (- pos-x half-size-x))
       (setq a-y (- pos-y half-size-y))
       (setq b-x (+ pos-x half-size-x))
       (setq b-y (- pos-y half-size-y))))

    (scene-add-filled-2d-triangle-list-primitive
     scene group nil color
     (list a-x a-y
	   b-x b-y
	   pos-x pos-y)
     object-id
     layer)))    

(defun scene-add-arrow-dock-menu (scene group p-min-x p-min-y size color object-id layer &key (style *style*))
  (declare (ignorable style))
  (scene-add-filled-2d-rectangle-list-primitive
   scene group nil color
   (list (+ p-min-x (* size 0.10f0))
	 (+ p-min-y (* size 0.15f0))
	 (+ p-min-x (* size 0.70f0))
	 (+ p-min-y (* size 0.30f0)))
   object-id
   layer)
  (scene-add-arrow-pointing-at scene group
			       (+ p-min-x (* size 0.40f0))
			       (+ p-min-y (* size 0.85f0))
			       (* size 0.30f0)
			       (* size 0.40f0)
			       color
			       :down
			       object-id
			       layer))  

(defmethod rm-draw-node ((self iconify-button))

  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (send style current-font)))
	 (text-col (send style text-color))
	 (object-id (the object-id))
	 (group (the group))
	 (layer (the layer))
	 (hovered? (the hovered?))
	 (window (the window))
	 (dock-node? nil))

    (push (scene-add-filled-2d-circle-primitive scene group nil (if hovered? (send style button-hovered-color) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						layer)
	  (the primitive-handles))

    (if dock-node?
	(push (scene-add-arrow-dock-menu scene group
					 (+ (the x) (send style frame-padding-x))
					 (+ (the y) (send style frame-padding-y))
					 (font-size (send style current-font))
					 text-col
					 object-id
					 (1+ layer)
					 :style style)
	      (the primitive-handles))
	(push (scene-add-arrow scene group
			       (+ (the x) (send style frame-padding-x))
			       (+ (the y) (send style frame-padding-y))
			       text-col (if (send window iconified?) :right :down)
			       1.0f0
			       object-id
			       (1+ layer)
			       :style style)
	      (the primitive-handles)))
    (values)))

#+NOTYET
(defmethod rm-draw-node ((menu-bar menu-bar))
  (let ((scene (node-scene menu-bar))
	(style (the style))
	(group (send menu-bar group))
	(window (window-parent menu-bar)))
    (scene-add-filled-2d-rectangle-list-primitive
     scene group nil (menu-bar-bg-color style)
     (list (+ (box-x0 menu-bar) (send window border-size))
	   (box-y0 menu-bar)
	   (- (box-x1 menu-bar) (send window border-size))
	   (box-y1 menu-bar))
     (send menu-bar object-id)
     (send menu-bar layer))
    (when (and (plusp (frame-border-size style))
	       (> (box-y1 menu-bar) (+ (box-y window) (box-height window))))
      (scene-add-2d-line-primitive scene group nil (frame-border-size style) (border-color style)
			 (box-x0 menu-bar) (box-y1 menu-bar)
			 (box-x1 menu-bar) (box-y1 menu-bar)
			 (send menu-bar object-id)
			 (send menu-bar layer)))))



(defmethod clui:handle-event ((window homemade-window-mixin) (event clui::pointer-button-press-event-mixin))
  (call-next-method)
  (setf (send window focused?) t)
  (values))

(defmethod clui:handle-event ((self title-bar) (event clui::pointer-button-press-event-mixin))
  (call-next-method)
  (unless (the window iconified?)
    (setf (the window focused?) t))
  (values))

(defmethod clui:handle-event ((window krma-window-mixin) (event clui::pointer-button-press-event-mixin))
  ;;(call-next-method)
  (let* ((hovered (krma::most-specifically-hovered-2d (krma::krma-select-box-2d (clui::window-display window)) 0 0)))
    (if hovered
	(let ((object (krma::object-from-id hovered)))
	  (when object
	    (clui:handle-event object event)))
	(focus-window *echo-area-window*))
    (values)))

(defobject popup-window-mixin (homemade-window-mixin))

#+NOTYET
(defmethod get-window-bg-color-accessor ((window popup-window-mixin))
  #'popup-bg-color)		      
  
(defobject homemade-window (homemade-window-mixin))

(defun make-test-window (&optional (app *app*))
  
  (let ((w (make-instance 'homemade-window
			  'origin (vec3 25 50 0)
			  'width 300
			  'height 200
			  'parent (main-window app)
			  'scene (application-scene app))))
    w))

