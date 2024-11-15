(in-package :climish)

(defmethod clim:presentation-object (self)
  self)

(defmethod clim:presentation-type (self)
  (type-of self))


(defobject context-menu-mixin (homemade-window-mixin)
  :inputs
  (applicable-commands
   (menu-bar? nil)
   (scrollbar-x? nil)
   (scrollbar-y? nil)
   (title-bar? t)
   (title "Options")
   (iconify-button? nil)
   (transparency-button? nil)
   (close-button? nil)
   (buttons-per-column 50)
   (button-height 34)
   (column-width (loop for i from 0 below (the button size)
		       maximize (the (button i) text-size)))

   (width (* (ceiling (the button size) (the buttons-per-column)) (the column-width)))
   (height (+ (* (min (the buttons-per-column) (the button size)) (the button-height))
	      (the title-bar height)))

   (color (the style title-bg-active-color))
   (commands (let* ((frame (application-frame self))
		    (base-candidates
		      (remove-if #'(lambda (command)
				     (typep command 'everywhere-command))
				 (the applicable-commands))))
	       (sort 
		(if (eq (focused-window frame) frame)
		    (remove-if #'(lambda (command)
				   (typep command 'text-editor-command))
			       base-candidates)
		    base-candidates)
		#'(lambda (a b)
		    (declare (ignore b))
		    (if (cad-command-p a)
			t
			nil)))))
   (titles (loop for command in (the commands)
		 collect (let ((gestures (gethash command *accelerator-table*)))
			   (if (null command)
			       "                            "
			       (concatenate 'string
					    " "
					    (hemlock::command-name command)
					    (when gestures
					      (list #\, #\Space))
					    (when gestures
					      (apply #'concatenate 'string
						     (mapcar #'(lambda (string)
								 (format nil "~A " string))
							     (mapcar #'(lambda (id)
									 (with-output-to-string (stream)
									   (print-pretty-input-event-id id stream)))
								     (first gestures))))))))))
   (text-sizes (mapcar #'(lambda (title)
			   (third (multiple-value-list
				   (calc-text-size title (the font)))))
		       (the titles))))

  :components
  ((button :type 'context-button
	   :aggregate (:size (max 1 (length (the commands))))
	   command (if (zerop (length (the commands)))
		       nil
		       (nth (the-part index) (the commands)))
	   height (the button-height)
	   width (the column-width)
	   y (let ((i (mod (the-part index) (the buttons-per-column))))
	       (+ (* i (the button-height)) (the title-bar height)))
	   x (let ((i (floor (the-part index) (the buttons-per-column))))
	       (* i (the column-width)))
	   title (nth (the-part index) (the titles))
	   command (nth (the-part index) (the commands))
	   text-size (nth (the-part index) (the text-sizes))
	   presentation-object (nth (the-part index) (the commands))
	   presentation-type (type-of (nth (the-part index) (the commands))))))


(defobject context-menu (context-menu-mixin))

(define-everywhere-command hemlock::point-to-here-command (window p)
  (:pretty-name "Point to Here")
  (:documentation
   "Move the point to the position of the mouse.
   If in the modeline, move to the absolute position in the file indicated by
   the position within the modeline, pushing the old position on the mark
   stack.  This supplies a function \"Generic Pointer Up\" invokes if it runs
   without any intervening generic pointer up predecessors running.  If the
   position of the pointer is different than the current point when the user
   invokes \"Generic Pointer Up\", then this function pushes a buffer mark at
   point and moves point to the pointer's position.  This allows the user to
   mark off a region with the mouse.")
  (:help-string
   "Move the point to the position of the mouse."))

(defmethod hemlock::point-to-here-command ((window context-menu-mixin) p)
  (let* ((frame (climish::application-frame window))
         (event (window-last-event frame)))
    (let ((objects-clicked (mapcar #'krma::object-from-id (clui::clicked-items event))))
      (loop for object in objects-clicked
	    do (unwind-protect (maybe-provide frame
					      (clim:presentation-object object)
					      (clim:presentation-object object))
		 (destroy-window window))))))

(defmethod clim:handle-event ((self context-menu-mixin) (event clim:pointer-exit-event))
  (destroy-window self))

(defobject button-mixin (clim:presentation selectable-mixin group-mixin view-mixin)
  :inputs
  (title			  
   command
   text-size
   (width (- (the superior width) 20))
   (height 34)
   (x 10)
   (y 10)
   presentation-object
   presentation-type))

(defmethod clim:presentation-object ((self button-mixin))
  (the presentation-object))

(defmethod clim:presentation-type ((self button-mixin))
  (the presentation-type))

(defobject context-button (button-mixin)
  :inputs
  ((color :visual (the style window-bg-color))))

(defmethod rm-draw-node ((self button-mixin))
  (when (the command)
    (push
     (scene-add-filled-2d-rectangle-list-primitive
      (the scene) (the group) nil (if (the hovered?)
				      (the style button-hovered-color)
				      (the color))
     (list (the x0) (the y0) (the x1) (the y1))
     (the object-id)
     (the elevation))
     (the primitive-handles))
    (push (scene-add-text-primitive (the scene) (the group) nil
				    (the style current-font) #x000000ff
				    (the x0) (the y0) (the title)
				    (the object-id) (1+ (the elevation)))
	  (the primitive-handles))))
  
