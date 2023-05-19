(in-package :krma.gui)

(defobject modeline (selectable-mixin group-mixin view-mixin)
  :inputs
  ((bg-color #xaaaaaa99)
   (fg-color #x000000ff))
  
  :attributes
  ((height (the style title-bar-height))

   (pad-left (+ (the style frame-padding-x) (if (eq (the style window-menu-button-position) :left)
						  (the button-size)
						  0)))
   (pad-right (the style frame-padding-x))))

(defmethod rm-draw-node ((self modeline))
  (unless (the window iconified?)
    (let ((scene (the scene))
	  (group (the group)))
    
      (push (scene-add-filled-2d-rectangle-list-primitive
	     scene group nil (the bg-color)
	     (list (the x0) (the y0) (the x1) (the y1))
	     (the object-id)
	     (the layer))
	    (the primitive-handles))

      (push (scene-add-text-primitive scene group nil (the superior font) (the fg-color) (the x0) (the y0)
				      (subseq (hi::window-modeline-buffer (the superior)) 0
					      (hi::window-modeline-buffer-len (the superior)))
				      (the object-id)
				      (1+ (the layer)))
	    (the primitive-handles))
      (values))))

(defobject text-editor-window-mixin (text-editor-view-mixin homemade-window-mixin)
  :inputs
  ((modeline? t))

  :attributes
  ((title :uncached (or (hi::buffer-%name (hi::window-buffer self)) "hemlock")))

  :components
  ((modeline :type (if (the modeline?) 'modeline 'null-object)
	     y (+ (the client-rect y) (the client-rect height)))

   (client-rect :type 'client-rect
		y (if (the title-bar?) (the title-bar y1) (the y))
		height (cond ((and (the title-bar?) (the modeline?))
			      (- (the height) (* 2 (the style title-bar-height))))
			     ((or (the title-bar?) (the modeline?))
			      (- (the height) (the style title-bar-height)))
			     (t (the height))))))

(defmethod initialize-instance :after ((self text-editor-window-mixin) &rest initargs)
  (declare (ignorable initargs))
  (hi::setup-modeline-image (hi::window-buffer self) self)
  (values))

(defobject text-editor-window (text-editor-window-mixin))

(defun make-hemlock-window ()
  (make-instance 'text-editor-window 'x 100 'y 75))

(defun command-save-file (window p)
  (declare (ignore window))
  (dotimes (n p)
    (print 'hiya)))



