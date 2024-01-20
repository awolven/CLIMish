(in-package :climish)



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

(defobject view-mixin (2d-box-mixin node-mixin clui::window-mixin)
  
  :inputs
  ((parent (the superior))
   (window :defaulting)
   (style :defaulting))

  :attributes
  ((elevation (if (the superior)
		  (1+ (the superior elevation))
		  0)))

  :components
  ((client-rect :type 'client-rect)))

(defmethod application-frame ((view view-mixin))
  (clui::window-parent (send view root)))

(defmethod application-frame ((frame application-frame-mixin))
  frame)

(defmethod hi::window-buffer (self)
  nil)

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
   (base-elevation 13)

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
   (elevation (ecase (the index)
	    ((:north :south) (+ (the base-elevation) 1))
	    ((:east :west) (+ (the base-elevation) 0))))))

(defobject resize-border (border)
  :inputs
  ((color #xffffff00)))

(defmethod clim:handle-event :around ((view view-mixin) (event clim:pointer-event))
  (setf (clim:pointer-event-native-x event) (clim:pointer-event-x event)
	(clim:pointer-event-native-y event) (clim:pointer-event-y event))
  (when (next-method-p)
    (call-next-method)))

(defmethod clim:handle-event ((self resize-border) (event clim:pointer-enter-event))
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

(defmethod clim:handle-event ((self resize-border) (event clim:pointer-button-press-event))
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







(defgeneric resize-window (window new-x new-y new-width new-height))



(defmethod clim:handle-event ((self resize-border) (event clim:pointer-exit-event))
  (setf (clui::window-cursor (clui::window-parent (the root))) (make-instance 'clui::arrow-cursor :display (clui::window-display (clui::window-parent (the root)))))
  (values))

(defmethod rm-draw-node ((self resize-border))
  (let ((scene (the scene)))
    (push
     (medium-add-filled-2d-rectangle-list-primitive
      scene (the group) nil (the color)
      (list (the x0) (the y0) (the x1) (the y1))
      (the object-id)
      (the elevation))
     (the primitive-handles))))

(defmethod rm-draw-node ((self border))
  (unless (the window iconified?)
    (let ((scene (the scene)))
      (push
       (medium-add-filled-2d-rectangle-list-primitive
	scene (the group) nil (the color)
	(list (the x0) (the y0) (the x1) (the y1))
	(if (typep (the root (resize-borders (the index))) 'null-object)
	    0
	    (the root (resize-borders (the index)) object-id))
	(the elevation))
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

   (transparency-button? t)
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

   (iconify-button :type (if (the iconify-button?) 'iconify-button 'null-object)
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
   (transparency-button :type (if (the transparency-button?) 'transparency-button 'null-object)
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

(defmethod clim:handle-event :after ((self title-bar) (event clim:pointer-button-release-event))

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
	   (medium-add-filled-2d-rectangle-list-primitive
	    scene group nil title-bar-color
	    (list (the x0) (the y0) (the x1) (the y1))
	    (the object-id)
	    (the elevation))
	   (the primitive-handles)))
    
	(let ((title-bar-color (if focused? (send style title-bg-active-color) (send style title-bg-color))))
	  (push
	   (medium-add-filled-2d-rectangle-list-primitive
	    scene group nil title-bar-color
	    (list (the x0) (the y0) (the x1) (the y1))
	    (the object-id)
	    (the elevation))
	   (the primitive-handles))))
	  

    (let ((layout-r-x0 (+ (the x0) (send style frame-padding-x)))
	  (layout-r-y0 (the y0)))

      (medium-add-text-primitive
       scene group nil (send style current-font) (send style text-color) layout-r-x0 layout-r-y0
       (send window title)
       (the object-id)
       (1+ (the elevation))))))

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
   (command-continuation :initform nil :accessor window-command-continuation)
   (resize-mode :initform nil :accessor resize-mode))

  :inputs
  ((parent (main-window clim:*default-frame-manager*))
   (scene (krma:default-medium))
   (style *style*)
   (color (the style window-bg-color))
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
   (transparency-button? t)
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
   (elevation (* (the z-index) krma::+view-depth+))
   (border-indices '((:north) (:south) (:east) (:west))))

  :components
  ((client-rect :type 'client-rect
		y (if (the title-bar?) (the title-bar y1) (the y))
		height (if (the title-bar?) (- (the height) (the title-bar height)) (the height)))

   (title-bar :type (if (the title-bar?) 'title-bar 'null-object)
	      title (the title)
	      iconify-button? (the iconify-button?)
	      close-button? (the close-button?)
	      transparency-button? (the transparency-button?)
	      button-size (font-size (the font)))

   (menu-bar :type (if (the menu-bar?) 'menu-bar 'null-object))

   (resize-borders :type (if (the resizable?) 'resize-border 'null-object)
		   :aggregate (:indices (the border-indices))
		   border-size 6
		   base-elevation (+ (the elevation) 11))

   (border :type 'border
	   :aggregate (:indices (the border-indices))
	   border-size 2
	   base-elevation (+ (the elevation) 13))

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


#+NOTNOW
(defmethod clim:handle-event ((window krma-window-mixin) (event clim:pointer-event))
  (let ((x (clim:pointer-event-x event))
	(y (clim:pointer-event-y event)))

    (let ((2d-object (krma::most-specifically-hovered-2d-object
		      (krma::krma-select-box-2d (clui::window-display window)) 0 0)))

      (if (view-p 2d-object)
	  (progn

	    (setf (clim:pointer-event-native-x event) x
		  (clim:pointer-event-native-y event) y)

	    (clim:handle-event 2d-object event))

	  (call-next-method)))))




(defmethod clim:handle-event ((self title-bar) (event clim:pointer-motion-event))
  (let ((last-event (clui::display-last-event
		     (clui::window-display (window-parent (the window))))))

    (when (or (typep last-event 'clui::pointer-button-press-event-mixin))

      (setf (window-to-drag (clui::window-parent (the root))) (the window))

      (change-class event 'clui::pointer-button-hold-and-drag-event-mixin)

      (setf (clui::pointer-drag-delta-x event) (- (clim:pointer-event-x event) (clim:pointer-event-x last-event)))

      (setf (clui::pointer-drag-delta-y event) (- (clim:pointer-event-y event) (clim:pointer-event-y last-event)))
      
      (clim:handle-event (clui::window-parent (the root)) event))))



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
    (when (eq homemade-window (focused-window (application-frame homemade-window)))
      (unfocus-window homemade-window))
    (forget-object-recursively homemade-window)
    (unless (krma::node-children scene)
      (setf (krma::immediate-mode-work-function-1 (clui::window-display (window-parent homemade-window))) nil))
    (values)))

(defmethod clim:handle-event ((button close-button) (event clim:pointer-button-press-event))
  (let ((homemade-window (send button window)))
    (destroy-window homemade-window)))

(defmethod clim:handle-event ((self transparency-button) (event clim:pointer-button-press-event))
  (if (the opaque?)
      (setf (the opaque?) nil)
      (setf (the opaque?) t))
  (rm-redraw-node-recursively (the window))
  (values))

(defmethod clim:handle-event ((button iconify-button) (event clim:pointer-button-press-event))
  (let ((homemade-window (send button window)))
    (if (send homemade-window iconified?)
	(setf (send homemade-window iconified?) nil)
	(setf (send homemade-window iconified?) t))
    (values)))

(defmethod clim:handle-event ((button title-bar-button-mixin) (event clim:pointer-boundary-event))
  (call-next-method)
  (rm-redraw-node-recursively button)
  (values))



(defmethod clui::window-p ((window homemade-window-mixin))
  t)

(defmethod %focus-window ((self homemade-window-mixin))
  (if (the iconified?)
      (setf (the iconified?) nil) ;; will reenter %focus-window with (the iconified?) --> nil
      (let ((scene (the scene)))
	(let ((siblings (remove self (remove-if-not #'(lambda (node)
							(typep node 'homemade-window-mixin))
						    (krma::node-children scene)))))
	  (setq siblings (sort siblings #'< :key #'(lambda (self) (the z-index))))
	  (loop for i from 0
		for sibling in siblings
		do (setf (send sibling z-index) i)
		finally (setf (the z-index) (length siblings))
			(mapcar #'rm-redraw-node-recursively (nconc siblings (list self)))
			(setf (focused-window (application-frame self)) self))
	  (unless (typep self 'context-menu)
	    (climish::compute-applicable-commands (application-frame self)))
	  (values)))))

(defmethod %focus-window ((self application-frame-mixin))
  (setf (focused-window self) self)
  (values))  

(defmethod %unfocus-window ((self homemade-window-mixin))
  (when (eq (focused-window (application-frame self)) self)
    (setf (focused-window (application-frame self))
	  (or (unless (eq (prev self) self) ;; only one window and it's me.
		(prev self))
	      (application-frame self)))
    (compute-applicable-commands (application-frame self))))

(defmethod clui:focus-window ((self homemade-window-mixin))
  (unless (eq self (focused-window (application-frame self)))
    (setf (the focused?) t))
  (values))

(defmethod clui:focus-window ((self application-frame-mixin))
  (%focus-window self))

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
  (transform-coordinate (vec3 (vx vec) (vy vec) (send view elevation)) view))

(defmethod initialize-instance :after ((self homemade-window-mixin) &rest initargs)
  (declare (ignore initargs))

  (let ((highest-z-index 0)
	(application-frame (window-parent (the root))))

    (let ((siblings (remove-if-not #'(lambda (thing)
				       (typep thing 'homemade-window-mixin))
				   (krma::node-children (the scene)))))
      (when siblings
	(setq highest-z-index
	      (loop for part in siblings
		    maximize (the-part z-index)))))
	
    (setf (the z-index) (1+ highest-z-index))
    (setf (krma::node-children (the scene)) (append (krma::node-children (the scene)) (list self)))

    (if (not (eq (focused-window application-frame) application-frame))
	
	(ring-link-new-element-at-start self (focused-window application-frame))

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
    (let* ((scene (the scene))
	   (group (the group))
	   (color (copy-seq (the color))))
      (when (the title-bar?)
	(when (the title-bar transparency-button?)
	  (when (the title-bar transparency-button opaque?)
	    (setf (aref color 3) 1.0f0))))
      ;; draw window background
      (medium-add-filled-2d-rectangle-list-primitive
       scene group nil color
       (list (the x0) (if (the title-bar?) (the title-bar y1) (the y0)) (the x1) (the y1))
       (the object-id)
       (the elevation)))))

  

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
      (medium-add-scrollbar scene group bb-x0 bb-y0 bb-x1 bb-y1
			   window
			   axis
			   (if (eq axis :horizontal)
			       (elt (scroll-amount window) 0)
			       (elt (scroll-amount window) 1))
			   size-avail size-contents
			   :style style))))

#+NOTYET
(defun medium-add-scrollbar (scene group bb-x0 bb-y0 bb-x1 bb-y1 window axis scroll-amount size-avail size-contents &key (style *style*))
  (let ((frame-width (- bb-x1 bb-x0))
	(frame-height (- bb-y1 bb-y0))
	(font-size (font-size (current-font style))))

    (unless (and (plusp frame-width) (plusp frame-height))
      (warn "invalid scrollbar size: ~S X ~S" frame-width frame-height)
      (return-from medium-add-scrollbar-1 (values)))

    (let ((alpha 1.0f0))
      (when (and (eq axis :vertical) (< frame-height (+ font-size (* 2.0f0 (frame-padding-y style)))))
	(setq alpha (saturate (/ (- frame-height font-size) (* 2.0f0 (frame-padding-y style)))))))
    (unless (plusp alpha)
      (return-from medium-add-scrollbar-1 (values)))

    (let ((allow-interaction? )))))
								     


			    

(defmethod rm-draw-node ((self close-button))
  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (send style current-font)))
	 (cross-color (send style text-color))
	 (object-id (the object-id))
	 (group (the group))
	 (elevation (the elevation))
	 (hovered? (the hovered?)))

    (push (medium-add-filled-2d-circle-primitive scene group nil (if hovered? (send style button-hovered-color) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						elevation)
	  (the primitive-handles))

    (let ((cross-extent (the cross-extent)))
      (push (medium-add-2d-line-primitive scene group nil 2.0f0 cross-color
					 (+ (the center-x) cross-extent)
					 (+ (the center-y) cross-extent)
					 (- (the center-x) cross-extent)
					 (- (the center-y) cross-extent)
					 object-id
					 (1+ elevation))
	    (the primitive-handles))
    
      (push (medium-add-2d-line-primitive scene group nil 2.0f0 cross-color
					 (+ (the center-x) cross-extent)
					 (- (the center-y) cross-extent)
					 (- (the center-x) cross-extent)
					 (+ (the center-y) cross-extent)
					 object-id
					 (1+ elevation))
	    (the primitive-handles)))

    (values)))

(defmethod rm-draw-node ((self transparency-button))
  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (send style current-font)))
	 (cross-color (send style text-color))
	 (object-id (the object-id))
	 (group (the group))
	 (elevation (the elevation))
	 (hovered? (the hovered?)))

    (push (medium-add-filled-2d-circle-primitive scene group nil (if hovered? (send style button-hovered-color) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						elevation)
	  (the primitive-handles))

    (let ((side-extent (max 2.0f0 (1+ (* 0.33f0 font-size))))
	  (side-extent2 (max 2.0f0 (1+ (* 0.22f0 font-size)))))
      (push (medium-add-2d-rectangle-primitive scene group nil 2.0f0 cross-color
					 (- (the center-x) side-extent)
					 (- (the center-y) side-extent)
					 (+ (the center-x) side-extent)
					 (+ (the center-y) side-extent)
					 object-id
					 (1+ elevation))
	    (the primitive-handles))

      (when (the opaque?)
	(push (medium-add-filled-2d-rectangle-list-primitive
	       scene group nil cross-color
	       (list (- (the center-x) side-extent2)
		     (- (the center-y) side-extent2)
		     (+ (the center-x) side-extent2)
		     (+ (the center-y) side-extent2))
	       object-id
	       (1+ elevation))
	    (the primitive-handles)))

      
      #+NIL
      (push (medium-add-2d-line-primitive scene group nil 2.0f0 cross-color
					 (- (the center-x) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 (- (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 (+ (the center-x) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 (- (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size))))
					 object-id
					 (1+ elevation))
	    (the primitive-handles)))

    (values)))

(defun medium-add-arrow (scene group pos-x pos-y color dir scale object-id elevation &key (style *style*))
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

    (medium-add-filled-2d-triangle-list-primitive
     scene group nil color
     (list (+ center-x a-x) (+ center-y a-y)
	   (+ center-x b-x) (+ center-y b-y)
	   (+ center-x c-x) (+ center-y c-y))
     object-id
     elevation)))

(defun medium-add-arrow-pointing-at (scene group pos-x pos-y half-size-x half-size-y color dir &optional (object-id 0) (elevation 0))
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

    (medium-add-filled-2d-triangle-list-primitive
     scene group nil color
     (list a-x a-y
	   b-x b-y
	   pos-x pos-y)
     object-id
     elevation)))    

(defun medium-add-arrow-dock-menu (scene group p-min-x p-min-y size color object-id elevation &key (style *style*))
  (declare (ignorable style))
  (medium-add-filled-2d-rectangle-list-primitive
   scene group nil color
   (list (+ p-min-x (* size 0.10f0))
	 (+ p-min-y (* size 0.15f0))
	 (+ p-min-x (* size 0.70f0))
	 (+ p-min-y (* size 0.30f0)))
   object-id
   elevation)
  (medium-add-arrow-pointing-at scene group
			       (+ p-min-x (* size 0.40f0))
			       (+ p-min-y (* size 0.85f0))
			       (* size 0.30f0)
			       (* size 0.40f0)
			       color
			       :down
			       object-id
			       elevation))  

(defmethod rm-draw-node ((self iconify-button))

  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (send style current-font)))
	 (text-col (send style text-color))
	 (object-id (the object-id))
	 (group (the group))
	 (elevation (the elevation))
	 (hovered? (the hovered?))
	 (window (the window))
	 (dock-node? nil))

    (push (medium-add-filled-2d-circle-primitive scene group nil (if hovered? (send style button-hovered-color) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						elevation)
	  (the primitive-handles))

    (if dock-node?
	(push (medium-add-arrow-dock-menu scene group
					 (+ (the x) (send style frame-padding-x))
					 (+ (the y) (send style frame-padding-y))
					 (font-size (send style current-font))
					 text-col
					 object-id
					 (1+ elevation)
					 :style style)
	      (the primitive-handles))
	(push (medium-add-arrow scene group
			       (+ (the x) (send style frame-padding-x))
			       (+ (the y) (send style frame-padding-y))
			       text-col (if (send window iconified?) :right :down)
			       1.0f0
			       object-id
			       (1+ elevation)
			       :style style)
	      (the primitive-handles)))
    (values)))

#+NOTYET
(defmethod rm-draw-node ((menu-bar menu-bar))
  (let ((scene (node-scene menu-bar))
	(style (the style))
	(group (send menu-bar group))
	(window (window-parent menu-bar)))
    (medium-add-filled-2d-rectangle-list-primitive
     scene group nil (menu-bar-bg-color style)
     (list (+ (box-x0 menu-bar) (send window border-size))
	   (box-y0 menu-bar)
	   (- (box-x1 menu-bar) (send window border-size))
	   (box-y1 menu-bar))
     (send menu-bar object-id)
     (send menu-bar elevation))
    (when (and (plusp (frame-border-size style))
	       (> (box-y1 menu-bar) (+ (box-y window) (box-height window))))
      (medium-add-2d-line-primitive scene group nil (frame-border-size style) (border-color style)
			 (box-x0 menu-bar) (box-y1 menu-bar)
			 (box-x1 menu-bar) (box-y1 menu-bar)
			 (send menu-bar object-id)
			 (send menu-bar elevation)))))



(defmethod clim:handle-event :before ((window homemade-window-mixin) (event clim:pointer-button-press-event))
  ;;(when (next-method-p)
    ;;(call-next-method))
  (setf (send window focused?) t)
  (values))

(defmethod clim:handle-event :before ((self title-bar) (event clim:pointer-button-press-event))
  ;;(when (next-method-p)
    ;;(call-next-method))
  (unless (the window iconified?)
    (setf (the window focused?) t))
  (values))

(defobject popup-window-mixin (homemade-window-mixin))

#+NOTYET
(defmethod get-window-bg-color-accessor ((window popup-window-mixin))
  #'popup-bg-color)		      
  
(defobject homemade-window (homemade-window-mixin))

(defun make-test-window (&optional (parent (main-window clim:*default-frame-manager*))
				   (scene (krma:default-medium)))
  
  (let ((w (make-instance 'homemade-window
			  'origin (vec3 25 50 0)
			  'width 300
			  'height 200
			  'parent parent
			  'scene scene)))
    w))

