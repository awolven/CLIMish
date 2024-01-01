(in-package :climish)

(defobject context-menu-mixin (homemade-window-mixin)
  :inputs
  ((menu-bar? nil)
   (scrollbar-x? nil)
   (scrollbar-y? nil)
   (title-bar? nil)
   (iconify-button? nil)
   (close-button? nil)

   (width 475)
   (height 700)

   (color (the style title-bg-active-color)))

  :components
  ((button1 :type 'context-button
	    title "Line")
   (button2 :type 'context-button
	    title "Circular Arc"
	    y (+ 10 (the button1 y1)))
   (button3 :type 'context-button
	    title "Elliptical Arc"
	    y (+ 10 (the button2 y1)))
   (button4 :type 'context-button
	    title "Bezier Curve"
	    y (+ 10 (the button3 y1)))
   (button5 :type 'context-button
	    title "NURBS Curve"
	    y (+ 10 (the button4 y1)))))


(defobject context-menu (context-menu-mixin))
  
(defmethod clim:handle-event ((self context-menu-mixin) (event clim:pointer-exit-event))
  (destroy-window self))

(defobject button-mixin (selectable-mixin group-mixin view-mixin)
  :inputs
  ((title "ok")
   (width (- (the superior width) 20))
   (height 54)
   (x 10)
   (y 10)))

(defobject context-button (button-mixin)
  :inputs
  ((color (the style window-bg-color))))

(defmethod rm-draw-node ((self button-mixin))
  (medium-add-filled-2d-rectangle-list-primitive
   (the scene) (the group) nil (the color)
   (list (the x0) (the y0) (the x1) (the y1))
   (the object-id)
   (the elevation))
  (medium-add-text (the scene) (the group) (the style current-font) #x000000ff (the x0) (the y0) (the title) (the object-id) (1+ (the elevation))))
  
