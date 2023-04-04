(in-package :krma.gui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass command-table-mixin ()
    ((ht :initform (make-hash-table) :reader command-table-table)))

  (defmethod table-lookup ((table command-table-mixin) key)
    (gethash key (command-table-table table)))

  (defmethod (setf table-lookup) (thing (table command-table-mixin) key)
    (setf (gethash key (command-table-table table)) thing))
  
  (defclass command-table (command-table-mixin)
    ())

  (defclass command-table-with-classes-mixin (command-table-mixin)
    ())
  
  (defclass toplevel-command-table (command-table-with-classes-mixin)
    ()))

(defvar *toplevel-command-table* (make-instance 'toplevel-command-table))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass homemade-window-class (adhoc:adhoc-class)
    ((command-table :initform (make-instance 'toplevel-command-table))))

  (defmethod command-table (class)
    (loop for c in (class-precedence-list class)
	  when (slot-exists-p c 'command-table)
	    do (return (slot-value c 'command-table))
	  finally (return *toplevel-command-table*)))    
  
  (defmethod validate-superclass ((subclass homemade-window-class) (superclass adhoc:adhoc-class))
    t))

(defclass event-queue-mixin ()
  ((input-event-queue :initform (lparallel.queue:make-queue) :reader input-event-queue)))

(defmethod command-table-p ((thing command-table-mixin))
  t)

(defmethod command-table-p (thing)
  (declare (ignore thing))
  nil)

(defclass krma-gui-window-mixin (event-queue-mixin krma-window-mixin)
  ((focused-window :initform nil :accessor focused-window)))

#+win32
(defclass win32::krma-gui-window (krma-gui-window-mixin win32:krma-enabled-window-mixin) ())

(defclass x11::krma-gui-window (krma-gui-window-mixin x11:krma-enabled-window-mixin) ())

#+cocoa
(defclass cocoa::krma-gui-window (krma-gui-window-mixin cocoa:krma-enabled-window-mixin) ())

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

(defparameter *style-classic-plist*
  (list :text                     #(0.90f0 0.90f0 0.90f0 1.00f0)
	:text-disabled            #(0.60f0 0.60f0 0.60f0 1.00f0)
	:window-bg                #(0.00f0 0.00f0 0.00f0 0.70f0)
	:child-bg                 #(0.00f0 0.00f0 0.00f0 0.00f0)
	:popup-bg                 #(0.11f0 0.11f0 0.14f0 0.92f0)
	:border                   #(0.50f0 0.50f0 0.50f0 0.50f0)
	:border-shadow            #(0.00f0 0.00f0 0.00f0 0.00f0)
	:frame-bg                 #(0.43f0 0.43f0 0.43f0 0.39f0)
	:frame-bg-hovered         #(0.47f0 0.47f0 0.69f0 0.40f0)
	:frame-bg-active          #(0.42f0 0.41f0 0.64f0 0.69f0)
	:title-bg                 #(0.27f0 0.27f0 0.54f0 0.83f0)
	:title-bg-active          #(0.32f0 0.32f0 0.63f0 0.87f0)
	:title-bg-collapsed       #(0.40f0 0.40f0 0.80f0 0.20f0)
	:menu-bar-bg              #(0.40f0 0.40f0 0.55f0 0.80f0)
	:scrollbar-bg             #(0.20f0 0.25f0 0.30f0 0.60f0)
	:scrollbar-grab           #(0.40f0 0.40f0 0.80f0 0.30f0)
	:scrollbar-grab-hovered   #(0.40f0 0.40f0 0.80f0 0.40f0)
	:scrollbar-grab-active    #(0.41f0 0.39f0 0.80f0 0.60f0)
	:checkmark                #(0.90f0 0.90f0 0.90f0 0.50f0)
	:slider-grab              #(1.00f0 1.00f0 1.00f0 0.30f0)
	:slider-grab-active       #(0.41f0 0.39f0 0.80f0 0.60f0)
	:button                   #(0.35f0 0.40f0 0.61f0 0.62f0)
	:button-hovered           #(0.40f0 0.48f0 0.71f0 0.79f0)
	:button-active            #(0.46f0 0.54f0 0.80f0 1.00f0)
	:header                   #(0.40f0 0.40f0 0.90f0 0.45f0)
	:header-hovered           #(0.45f0 0.45f0 0.90f0 0.80f0)
	:header-active            #(0.53f0 0.53f0 0.87f0 0.80f0)
	:separator                #(0.50f0 0.50f0 0.50f0 0.60f0)
	:separator-hovered        #(0.60f0 0.60f0 0.70f0 1.00f0)
	:separator-active         #(0.70f0 0.70f0 0.90f0 1.00f0)
	:resize-grip              #(1.00f0 1.00f0 1.00f0 0.16f0)
	:resize-grip-hovered      #(0.78f0 0.82f0 1.00f0 0.60f0)
	:resize-grip-active       #(0.78f0 0.82f0 1.00f0 0.90f0)

	:docking-empty-bg         #(0.20f0 0.20f0 0.20f0 1.00f0)
	:plot-lines               #(1.00f0 1.00f0 1.00f0 1.00f0)
	:plot-lines-hovered       #(0.90f0 0.70f0 0.00f0 1.00f0)
	:plot-histogram           #(0.90f0 0.70f0 0.00f0 1.00f0)
	:plot-histogram-hovered   #(1.00f0 0.60f0 0.00f0 1.00f0)
	:text-selected-bg         #(0.00f0 0.00f0 1.00f0 0.35f0)
	:drag-drop-target         #(1.00f0 1.00f0 0.00f0 0.90f0)

	:nav-windowing-highlight  #(1.00f0 1.00f0 1.00f0 0.70f0)
	:nav-windowing-dim-bg     #(0.80f0 0.80f0 0.80f0 0.20f0)
	:modal-window-dim-bg      #(0.20f0 0.20f0 0.20f0 0.35f0)

	:frame-border-size 1
	:frame-padding-x 1
	:frame-padding-y 1
	:item-inner-spacing-x 4
	:window-title-align-x 1
	:window-menu-button-position :right
	:title-bar-height 20
	:default-window-border-size 1))

(defclass gui-style ()
  ((text :accessor text-color)
   (text-disabled :accessor text-disabled-color)
   (window-bg :accessor window-bg-color)
   (child-bg :accessor child-bg-color)
   (popup-bg :accessor popup-bg-color)
   (border :accessor border-color)
   (border-shadow :accessor border-shadow-color)
   (frame-bg :accessor frame-bg-color)
   (frame-bg-hovered :accessor frame-bg-hovered-color)
   (frame-bg-active :accessor frame-bg-active-color)
   (title-bg :accessor title-bg-color)
   (title-bg-active :accessor title-bg-active-color)
   (title-bg-collapsed :accessor title-bg-collapsed-color)
   (menu-bar-bg :accessor menu-bar-bg-color)
   (scrollbar-bg :accessor scrollbar-bg-color)
   (scrollbar-grab :accessor scrollbar-grab-color)
   (scrollbar-grab-hovered :accessor scrollbar-grab-hovered-color)
   (scrollbar-grab-active :accessor scrollbar-grab-active-color)
   (checkmark :accessor checkmark-color)
   (slider-grab :accessor slider-grab-color)
   (slider-grab-active :accessor slider-grab-active-color)
   (button :accessor button-color)
   (button-hovered :accessor button-hovered-color)
   (button-active :accessor button-active-color)
   (header :accessor header-color)
   (header-hovered :accessor header-hovered-color)
   (header-active :accessor header-active-color)
   (separator :accessor separator-color)
   (separator-hovered :accessor separator-hovered-color)
   (separator-active :accessor separator-active-color)
   (resize-grip :accessor resize-grip-color)
   (resize-grip-hovered :accessor resize-grip-hovered-color)
   (resize-grip-active :accessor resize-grip-active-color)
   (tab :accessor tab-color)
   (tab-hovered :accessor tab-hovered-color)
   (tab-active :accessor tab-active-color)
   (tab-unfocused :accessor tab-unfocused-color)
   (tab-unfocused-active :accessor tab-unfocused-active-color)
   (docking-preview :accessor docking-preview-color)
   (docking-empty-bg :accessor docking-empty-bg-color)
   (plot-lines :accessor plot-lines-color)
   (plot-lines-hoverered :accessor plot-lines-hovered-color)
   (plot-histogram :accessor plot-histogram-color)
   (plot-histogram-hovered :accessor plot-histogram-hovered-color)
   (text-selected-bg :accessor text-selected-bg-color)
   (drag-drop-target :accessor drag-drop-target-color)
   (nav-highlight :accessor nav-highlight-color)
   (nav-windowing-highlight :accessor nav-windowing-highlight-color)
   (nav-windowing-dim-bg :accessor nav-windowing-dim-bg-color)
   (modal-window-dim-bg :accessor modal-window-dim-bg-color)
   (frame-border-size :accessor frame-border-size)
   (frame-padding-x :accessor frame-padding-x)
   (frame-padding-y :accessor frame-padding-y)
   (item-inner-spacing-x :accessor item-inner-spacing-x)
   (window-title-align-x :accessor window-title-align-x)
   (window-menu-button-position :accessor window-menu-button-position)
   (title-bar-height :accessor title-bar-height)
   (default-window-border-size :accessor default-window-border-size)

   (current-font :initform nil :accessor current-font)))

(defun lerp (a b s)
  (map 'vector #'+ a (map 'vector #'(lambda (x) (* x s)) (map 'vector #'- a b))))

(defun saturate (float)
  (when (minusp float) (return-from saturate 0.0f0))
  (when (> float 1.0f0) (return-from saturate 1.0f0))
  float)

(defun make-gui-style (style-plist &optional (display (default-display)))
  (let ((style-object (make-instance 'gui-style)))
    (setf (text-color style-object) (getf style-plist :text)
	  (text-disabled-color style-object) (getf style-plist :text-disabled)
	  (window-bg-color style-object) (getf style-plist :window-bg)
	  (child-bg-color style-object) (getf style-plist :child-bg)
	  (popup-bg-color style-object) (getf style-plist :popup-bg)
	  (border-color style-object) (getf style-plist :border)
	  (border-shadow-color style-object) (getf style-plist :border-shadow)
	  (frame-bg-color style-object) (getf style-plist :frame-bg)
	  (frame-bg-hovered-color style-object) (getf style-plist :frame-bg-hovered)
	  (frame-bg-active-color style-object) (getf style-plist :frame-bg-active)
	  (title-bg-color style-object) (getf style-plist :title-bg)
	  (title-bg-active-color style-object) (getf style-plist :title-bg-active)
	  (title-bg-collapsed-color style-object) (getf style-plist :title-bg-collapsed)
	  (menu-bar-bg-color style-object) (getf style-plist :menu-bar-bg)
	  (scrollbar-bg-color style-object) (getf style-plist :scrollbar-bg)
	  (scrollbar-grab-color style-object) (getf style-plist :scrollbar-grab)
	  (scrollbar-grab-hovered-color style-object) (getf style-plist :scrollbar-grab-hovered)
	  (scrollbar-grab-active-color style-object) (getf style-plist :scrollbar-grab-active)
	  (checkmark-color style-object) (getf style-plist :checkmark)
	  (slider-grab-color style-object) (getf style-plist :slider-grab)
	  (slider-grab-active-color style-object) (getf style-plist :slider-grab)
	  (button-color style-object) (getf style-plist :button)
	  (button-hovered-color style-object) (getf style-plist :button-hovered)
	  (header-color style-object) (getf style-plist :header)
	  (header-hovered-color style-object) (getf style-plist :header-hovered)
	  (header-active-color style-object) (getf style-plist :header-active)
	  (separator-color style-object) (getf style-plist :separator)
	  (separator-hovered-color style-object) (getf style-plist :separator-hovered)
	  (resize-grip-color style-object) (getf style-plist :resize-grip)
	  (resize-grip-hovered-color style-object) (getf style-plist :resize-grip-hovered)
	  (resize-grip-active-color style-object) (getf style-plist :resize-grip-active)
	  (tab-color style-object) (lerp (getf style-plist :header) (getf style-plist :title-bg-active) 0.80)
	  (tab-hovered-color style-object) (getf style-plist :header-hovered)
	  (tab-active-color style-object) (lerp (getf style-plist :header-active) (getf style-plist :title-bg-active) 0.60)
	  (tab-unfocused-color style-object) (lerp (getf style-plist :tab) (getf style-plist :title-bg) 0.80)
	  (tab-unfocused-active-color style-object) (lerp (getf style-plist :tab-active) (getf style-plist :title-bg) 0.80)
	  (docking-preview-color style-object) (let ((c (copy-seq (getf style-plist :header))))
					   (setf (elt c 3) (* (elt c 3) 0.7))
					   c)
	  (docking-empty-bg-color style-object) (getf style-plist :docking-empty-bg)
	  (plot-lines-color style-object) (getf style-plist :plot-lines)
	  (plot-lines-hovered-color style-object) (getf style-plist :plot-lines-hovered)
	  (plot-histogram-color style-object) (getf style-plist :plot-histogram)
	  (plot-histogram-hovered-color style-object) (getf style-plist :plot-histogram-hovered)
	  (text-selected-bg-color style-object) (getf style-plist :text-selected-bg)
	  (drag-drop-target-color style-object) (getf style-plist :drag-drop-target)
	  (nav-highlight-color style-object) (getf style-plist :nav-highlight)
	  (nav-windowing-highlight-color style-object) (getf style-plist :nav-windowing-highlight)
	  (nav-windowing-dim-bg-color style-object) (getf style-plist :nav-windowing-dim-bg)
	  (modal-window-dim-bg-color style-object) (getf style-plist :modal-window-dim-bg)
	  
	  (frame-border-size style-object) (getf style-plist :frame-border-size)
	  (frame-padding-x style-object) (getf style-plist :frame-padding-x)
	  (frame-padding-y style-object) (getf style-plist :frame-padding-y)
	  (item-inner-spacing-x style-object) (getf style-plist :item-inner-spacing-x)
	  (window-title-align-x style-object) (getf style-plist :window-title-align-x)
	  (window-menu-button-position style-object) (getf style-plist :window-menu-button-position)
	  (title-bar-height style-object) (getf style-plist :title-bar-height)
	  (default-window-border-size style-object) (getf style-plist :default-window-border-size)

	  (current-font style-object) (default-system-font display))	  
    
    style-object))

(defun make-gui-style-classic (&optional (display (default-display)))
  (make-gui-style *style-classic-plist* display))

(defclass krma-gui-application-mixin (krma-application-mixin)
  ())



(defclass krma-gui-application (krma-gui-application-mixin)
  ())

(defmethod initialize-instance :after ((app krma-gui-application) &rest initargs)
  (declare (ignore initargs))
  (unless (boundp '*style*)
    (setq *style* (make-gui-style-classic (application-display app)))))

(defmethod transform-coordinate ((vec vec2) (node node-mixin))
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

(defobject rect-mixin ()
  :inputs
  ((rect-x 0)
   (rect-y 0)
   (rect-width 10)
   (rect-height 10))

  :attributes
  ((rect-x0 :uncached (the rect-x))
   (rect-y0 :uncached (the rect-y))

   (rect-x1 (+ (the rect-x) (the rect-width)))
   (rect-y1 (+ (the rect-y) (the rect-height)))))

(defobject client-rect (rect-mixin))

(defobject view-mixin (rect-mixin node-mixin)
  
  :inputs
  ((parent (the superior))
   (window)
   (style :defaulting))

  :attributes
  (#+NIL(all-hovered :uncached (krma::krma-hovered (clui::window-display (window-parent (the window)))))
   #+NIL(generally-hovered? :uncached (find (the object-id) (the all-hovered)))
   #+NIL(hovered? :uncached (eq (the object-id) (first (the all-hovered))))
   
   (layer (if (the superior)
	      (1+ (the superior layer))
	      0))))

(defmethod view-p (object)
  (declare (ignore object))
  nil)

(defmethod view-p ((object view-mixin))
  t)

(defobject group-owner-view-mixin (group-owner-mixin view-mixin))

(defobject title-bar-button-mixin (selectable-mixin group-mixin view-mixin)
  :inputs
  ((size))
  
  :attributes
  ((center-x (/ (+ (the rect-x0) (the rect-x1)) 2))
   (center-y (/ (+ (the rect-y0) (the rect-y1)) 2))
   (rect-width (+ (the size) (* 2 (frame-padding-x (the style)))))
   (rect-height (+ (the size) (* 2 (frame-padding-y (the style)))))))


(defobject collapse-button (title-bar-button-mixin))


(defobject close-button (title-bar-button-mixin)
  :attributes
  ((cross-extent (1- (* (the size) 0.5 0.7071)))))


(defobject menu-bar (selectable-mixin group-mixin view-mixin))


(defobject title-bar (selectable-mixin group-mixin view-mixin)

  :inputs
  ((title)
   (rect-width)
   (button-size)
   
   (collapse-button? t)
   (close-button? t))
  
  :attributes
  ((rect-height (title-bar-height (the style)))
   
   (pad-left (+ (frame-padding-x (the style)) (if (eq (window-menu-button-position (the style)) :left)
						  (the button-size)
						  0)))
   (pad-right (frame-padding-x (the style))))
  
  :components
  ((close-button :type (if (the close-button?) 'close-button 'null-object)
		 group (the group)
		 window (the window)
		 size (the button-size)
		 rect-x (- (the rect-x1)
			   (the button-size)
			   (* 2 (frame-padding-x (the style))))
		 rect-y (the rect-y0))

   (collapse-button :type (if (the collapse-button?) 'collapse-button 'null-object)
		    group (the group)
		    size (the button-size)
		    window (the window)
		    rect-x (ecase (window-menu-button-position (the style))
			     (:left 0)
			     (:right (- (the rect-x1)
					(if (the close-button?)
					    (* 2 (the button-size))
					    (the button-size))
					4
					(* 2 (frame-padding-x (the style))))))
		    rect-y (the rect-y))))

(defmethod rm-draw-node ((self title-bar))
  (let ((scene (the scene))
	(style (the style))
	(window (the superior))
	(group (the group))
	(highlighted? (the hovered?)))
    
    (if (send window collapsed?)
	(let ((title-bar-color (if highlighted? (title-bg-active-color style) (title-bg-collapsed-color style))))
	  ;; draw just title bar
	  (push
	   (scene-add-filled-2d-rectangle-list-primitive
	    scene group nil title-bar-color
	    (list (the rect-x0) (the rect-y0) (the rect-x1) (the rect-y1))
	    (the object-id)
	    (the layer))
	   (the primitive-handles)))
    
	(let ((title-bar-color (if highlighted? (title-bg-active-color style) (title-bg-color style))))
	  (push
	   (scene-add-filled-2d-rectangle-list-primitive
	    scene group nil title-bar-color
	    (list (the rect-x0) (the rect-y0) (the rect-x1) (the rect-y1))
	    (the object-id)
	    (the layer))
	   (the primitive-handles))))
	  

    (let ((layout-r-x0 (+ (the rect-x0) (frame-padding-x style)))
	  (layout-r-y0 (the rect-y0)))

      (scene-add-text-primitive
       scene group nil (current-font style) (text-color style) layout-r-x0 layout-r-y0
       (send window title)
       (the object-id)
       (1+ (the layer))))))

(defobject resize-grips (selectable-mixin group-mixin view-mixin))

(defobject homemade-window-mixin (selectable-mixin group-owner-view-mixin)
  :metaclass homemade-window-class

  :inputs
  ((parent (main-window *app*))
   (scene (application-scene *app*))
   (style *style*)
   (font (current-font (the style)))
   (collapsed? nil)
   (resizable? nil)
   (menu-bar? nil)
   (scrollbar-x? nil)
   (scrollbar-y? nil)
   (title-bar? t)
   (collapse-button? t)
   (close-button? t)
   (iconified? nil)
   (rounding? nil)
   (alpha 0.7)
   
   (window self)
   
   (title "krma")

   (border-size 0)

   (rect-width 300)
   (rect-height 200))

  :attributes
  ((z-index :modifiable 0)
   (layer (* (the z-index) krma::+view-depth+)))

  :components
  ((client-rect :type 'client-rect
		rect-x (the rect-x)
		rect-y (if (the title-bar?) (the title-bar rect-y1) (the rect-y))
		rect-width (the rect-width)
		rect-height (if (the title-bar?) (- (the rect-height) (the title-bar rect-height)) (the rect-height)))
   
   (title-bar :type (if (the title-bar?) 'title-bar 'null-object)
	      title (the title)
	      window self
	      rect-x (the rect-x)
	      rect-y (the rect-y)
	      rect-width (the rect-width)
	      collapse-button? (the collapse-button?)
	      close-button? (the close-button?)
	      button-size (font-size (the font)))

   (menu-bar :type (if (the menu-bar?) 'menu-bar 'null-object)
	     window self)

   (resize-grips :type (if (the resizable?) 'resize-grips 'null-object)
		 window self)

   (horizontal-scrollbar :type (if (the scrollbar-x?) 'scrollbar 'null-object)
			 axis :horizontal
			 window self)

   (vertical-scrollbar :type (if (the scrollbar-y?) 'scrollbar 'null-object)
		       axis :vertical
		       window self)))

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

(defmethod clui:handle-event ((title-bar title-bar) (event clui::pointer-motion-event-mixin))
  (let ((last-event (clui::display-last-event
		     (clui::window-display (window-parent (send title-bar window))))))

    (when (or (typep last-event 'clui::pointer-button-press-event-mixin)
	      (typep last-event 'clui::pointer-button-hold-and-drag-event-mixin))

      (change-class event 'clui::pointer-button-hold-and-drag-event-mixin)

      (setf (clui::pointer-drag-delta-x event) (- (clui::pointer-event-x event)
						  (clui::pointer-event-x last-event)))

      (setf (clui::pointer-drag-delta-y event) (- (clui::pointer-event-y event)
						  (clui::pointer-event-y last-event)))
      
      (clui:handle-event (regenerate! title-bar) event))))

(defmethod clui:handle-event ((title-bar title-bar) (event clui::pointer-button-hold-and-drag-event-mixin))
  (let* ((dx (clui::pointer-drag-delta-x event))
	 (dy (clui::pointer-drag-delta-y event)))
    (multiple-value-bind (x y) (window-position (send title-bar root))
      (set-window-position (send title-bar window)
			   (+ x dx) (+ y dy)))))

(defmethod window-cursor-position ((self view-mixin))
  (multiple-value-bind (x y) (window-cursor-position (window-parent (the root)))
    (let ((coord (untransform-global-coordinate (vec3 x y 0) self)))
      (values (vx coord) (vy coord)))))
      
      

(defmethod clui:handle-event ((button close-button) (event clui::pointer-button-press-event-mixin))
  ;; remove the graphical representation from the draw-lists
  (let ((homemade-window (send button window)))
    (rm-erase-node-recursively homemade-window)
    (forget-object-recursively homemade-window)
    #+ccl(ccl:gc)
    #+sbcl(gc t)))

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

(defmethod clui:focus-window ((self homemade-window-mixin))
  (let ((scene (the scene)))
    (let ((siblings (remove self (krma::node-children scene))))
      (setq siblings (sort siblings #'< :key #'(lambda (self) (the z-index))))
      (loop for i from 0
	    for sibling in siblings
	    do (setf (send sibling z-index) i)
	    finally (setf (the z-index) (length siblings))
		    (mapcar #'rm-redraw-node-recursively (nconc siblings (list self)))
		    (setf (focused-window (window-parent (the root))) self))
      (values))))

(defmethod clui:window-position ((self view-mixin))
  (let* ((vec4 (m* (vec4 0 0 0 1) (the matrix)))
	 (vec3 (safe-euclid vec4)))
    (values (vx vec3) (vy vec3) (vz vec3))))

(defmethod global-window-position ((self view-mixin))
  (let* ((vec4 (m* (vec4 0 0 0 1) (the global-matrix)))
	 (vec3 (safe-euclid vec4)))
    (values (vx vec3) (vy vec3) (vz vec3))))
    
(defmethod clui:set-window-position ((view view-mixin) x y)
  (translate-node view (vec3 x y 0))
  (values))

(defmethod clui:set-window-position ((view group-owner-view-mixin) x y)
  (multiple-value-bind (cur-x cur-y) (clui:window-position view)
    (let* ((dx (- x cur-x))
	   (dy (- y cur-y))
	   (vec3 (vec3 dx dy 0)))
      (translate-node view vec3)
      (values))))      
  
(defmethod clui:window-size ((self view-mixin))
  (values (the rect-width) (the rect-height)))

(defmethod clui:set-window-size ((self group-owner-view-mixin) (width real) (height real))
  (setf (the rect-width) width
	(the rect-height) height)
  (rm-redraw-node-recursively self)
  (values))

(defmethod transform-coordinate ((vec vec2) (view view-mixin))
  (transform-coordinate (vec3 (vx vec) (vy vec) (send view layer)) view))

(defmethod initialize-instance :after ((self homemade-window-mixin) &rest initargs)
  (declare (ignore initargs))

  (let ((highest-z-index 0))
    
    (when (krma::node-children (the scene))
      (setq highest-z-index
	    (loop for part in (krma::node-children (the scene))
		  maximize (the-part z-index))))
	
    (setf (the z-index) (1+ highest-z-index))
    (setf (krma::node-children (the scene)) (append (krma::node-children (the scene)) (list self)))
    (setf (focused-window (window-parent (the root))) self))

  (values))


(defmethod rm-draw-node ((self homemade-window-mixin))
  
  (unless (the iconified?)
    (let ((style (the style))
	  (scene (the scene))
	  (group (the group)))
      ;; draw window background
      (scene-add-filled-2d-rectangle-list-primitive
       scene group nil (window-bg-color style)
       (list (the rect-x0) (the rect-y0) (the rect-x1) (the rect-y1))
       (the object-id)
       (the layer)))))

#+NOTYET
(defun get-window-scrollbar-rect (window &optional (axis :vertical))
  (let* ((rect-x0 (rect-x0 window))
	 (rect-y0 (rect-y0 window))
	 (rect-x1 (rect-x1 window))
	 (rect-y1 (rect-y1 window))
	 (client-rect (window-client-rect window))
	 (client-x0 (rect-x0 client-rect))
	 (client-y0 (rect-y0 client-rect))
	 (client-x1 (rect-x1 client-rect))
	 (client-y1 (rect-y1 client-rect))
	 (border-size (window-border-size window))
	 (scrollbar-size (if (eq axis :horizontal)
			     (elt (window-scrollbar-sizes window) 0)
			     (elt (window-scrollbar-sizes window) 1))))
    (case axis
      (:horizontal (values client-x0 (max rect-y0 (- rect-y1 border-size scrollbar-size))
			   client-x1 rect-y1))
      (otherwise
       (unless (eq axis :vertical)
	 (warn "`axis' ~S is not valid, wanted :horizontal or :vertical" axis))
       (values (max rect-x0 (- rect-x1 border-size scrollbar-size)) client-y0
	       rect-x1 client-y1)))))

#+NOTYET
(defun add-scrollbar (scrollbar axis &key (style *style*))
  (let* ((window (window-parent scrollbar))
	 (scene (node-scene scrollbar))
	 (group (object-group scrollbar))
	 (client-rect (window-client-rect window))
	 (size-avail (if (eq axis :horizontal)
			 (- (rect-x1 client-rect)
			    (rect-x0 client-rect))
			 (- (rect-y1 client-rect)
			    (rect-y0 client-rect))))
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
	 (font-size (font-size (current-font style)))
	 (cross-color (text-color style))
	 (object-id (the object-id))
	 (group (the group))
	 (layer (the layer))
	 (hovered? (the hovered?)))

    (push (scene-add-filled-2d-circle-primitive scene group nil (if hovered? (button-hovered-color *style*) 0)
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

(defun scene-add-arrow (scene group pos-x pos-y color dir scale object-id layer &key (style *style*))
  (let* ((h (font-size (current-font style)))
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

(defmethod rm-draw-node ((self collapse-button))

  (let* ((scene (the scene))
	 (style (the style))
	 (font-size (font-size (current-font style)))
	 (text-col (text-color style))
	 (object-id (the object-id))
	 (group (the group))
	 (layer (the layer))
	 (hovered? (the hovered?))
	 (window (the window))
	 (dock-node? nil))

    (push (scene-add-filled-2d-circle-primitive scene group nil (if hovered? (button-hovered-color *style*) 0)
						(the center-x) (the center-y) (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						layer)
	  (the primitive-handles))

    (if dock-node?
	(push (scene-add-arrow-dock-menu scene group
					 (+ (the rect-x) (frame-padding-x style))
					 (+ (the rect-y) (frame-padding-y style))
					 (font-size (current-font style))
					 text-col
					 object-id
					 (1+ layer)
					 :style style)
	      (the primitive-handles))
	(push (scene-add-arrow scene group
			       (+ (the rect-x) (frame-padding-x style))
			       (+ (the rect-y) (frame-padding-y style))
			       text-col (if (send window collapsed?) :right :down)
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
     (list (+ (rect-x0 menu-bar) (send window border-size))
	   (rect-y0 menu-bar)
	   (- (rect-x1 menu-bar) (send window border-size))
	   (rect-y1 menu-bar))
     (send menu-bar object-id)
     (send menu-bar layer))
    (when (and (plusp (frame-border-size style))
	       (> (rect-y1 menu-bar) (+ (rect-y window) (rect-height window))))
      (scene-add-2d-line-primitive scene group nil (frame-border-size style) (border-color style)
			 (rect-x0 menu-bar) (rect-y1 menu-bar)
			 (rect-x1 menu-bar) (rect-y1 menu-bar)
			 (send menu-bar object-id)
			 (send menu-bar layer)))))



(defmethod clui:handle-event ((window homemade-window-mixin) (event clui::pointer-button-press-event-mixin))
  (call-next-method)
  (clui:focus-window window))

(defmethod clui:handle-event ((self title-bar) (event clui::pointer-button-press-event-mixin))
  (call-next-method)
  (clui:focus-window (the window)))

(defmethod clui:handle-event ((window krma-window-mixin) (event clui::pointer-button-press-event-mixin))
  (call-next-method)
  (let* ((hovered (krma::most-specifically-hovered-2d (krma::krma-select-box-2d (clui::window-display window)) 0 0)))
    (when hovered
      (let ((object (krma::object-from-id hovered)))
	(when object
	  (clui:handle-event object event))))
    (values)))

(defobject popup-window-mixin (homemade-window-mixin))

(defmethod get-window-bg-color-accessor ((window popup-window-mixin))
  #'popup-bg-color)		      
  
(defobject homemade-window (homemade-window-mixin))

(defun make-test-window (&optional (app *app*))
  
  (let ((w (make-instance 'homemade-window
			  'translation (vec3 25 50 0)
			  'rect-width 300
			  'rect-height 200
			  'parent (main-window *app*)
			  'scene (application-scene app))))
    w))

	       
#+NOTYET
(defmethod forget-object ((self title-bar))

  (call-next-method)
  
  (when (the close-button?)
    (forget-object (the close-button)))

  (when (the collapse-button?)
    (forget-object (the collapse-button)))

  (values))

#+NOTYET
(defmethod forget-object ((self homemade-window-mixin))

  ;; remove the references to the object so it will no longer 
  ;; handle events or be re-added, and can be garbage collected
  (call-next-method)

  (when (the title-bar?)
    (forget-object (the title-bar)))

  (when (the menu-bar?)
    (forget-object (the menu-bar)))

  (setf (krma::node-children (the scene)) (remove self (krma::node-children (the scene))))

  (values))
