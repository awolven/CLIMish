(in-package :climish)

(defobject widget-mixin ())

(defobject text-editor-widget-mixin (selectable-mixin text-editor-view-mixin widget-mixin)
  :inputs
  ((modeline? :visual nil)
   (group :visual (the superior group))
   (iconified? (the window iconified?))
   (color :visual #x11111111))

  :attributes
  ((font (the superior font))
   (height (* 1.5 (font-size (the font)))))

  :components
  ((border :type 'border
	   :aggregate (:indices `((:east) (:west) (:north) (:south)))
	   border-size 2
	   base-elevation (+ (the elevation) 13))))

(defmethod clim:handle-event ((self text-editor-widget-mixin) (event clim:pointer-button-press-event))
  (focus-window (the window))
  (setf (the window focused-view) self)
  (values))
  

(defobject text-editor-widget (text-editor-widget-mixin))
  

(defobject sample-widget-window (homemade-window-mixin)

  :inputs
  ((focused-view :visual (the text-widget1))

   (widgets (remove-if-not #'(lambda (child)
			       (typep child 'widget-mixin))
			   (the children))))
  
  :components
  ((text-widget1 :type 'text-editor-widget
		 x 100 y 100
		 width 300)
   (text-widget2 :type 'text-editor-widget
		 x 100 y 200
		 width 300)))


(defmethod rm-draw-node ((self text-editor-widget-mixin))
  (unless (the iconified?)
    (push (scene-add-filled-2d-rectangle-list-primitive
	   (the scene)
	   (the group)
	   nil
	   (the color)
	   (list (the x0)
		 (the y0)
		 (the x1)
		 (the y1))
	   (the object-id)
	   (the elevation))
	  (the primitive-handles))
    (values)))

(defun make-widget-test (&optional (parent (main-window clim:*default-frame-manager*))
			   (scene (krma:default-scene)))
  (let* ((w (make-instance 'sample-widget-window
			   'origin (vec3 250 250 0)
			   'width 500
			   'height 300
			   'parent parent
			   'scene scene))
	 (widget (send w text-widget1))
	 (buffer (window-buffer widget)))

    (insert-string buffer (buffer-point buffer) "FOO!")

    (redraw-buffer widget)
    w))
			  

