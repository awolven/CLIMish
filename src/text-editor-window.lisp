(in-package :krma.gui)

(defobject modeline (selectable-mixin group-mixin view-mixin)
  :inputs
  ((bg-color #x73737399)
   (fg-color #x000000ff))
  
  :attributes
  ((height (the style title-bar-height))

   (pad-left (+ (the style frame-padding-x) (if (eq (the style window-menu-button-position) :left)
						  (the button-size)
						  0)))
   (pad-right (the style frame-padding-x))))

(defmethod rm-draw-node ((self modeline))
  (unless (the window iconified?)
    (hi::update-modeline-fields (hi::window-buffer (the window)) (the window))
    (let ((scene (the scene))
	  (group (the group)))
    
      (push (scene-add-filled-2d-rectangle-list-primitive
	     scene group nil (the bg-color)
	     (list (the x0) (the y0) (the x1) (the y1))
	     (the object-id)
	     (the layer))
	    (the primitive-handles))

      (when (hi::window-modeline-buffer (the superior))
	(push (scene-add-text-primitive scene group nil (the superior font) (the fg-color) (the x0) (the y0)
					(subseq (hi::window-modeline-buffer (the superior)) 0
						(hi::window-modeline-buffer-len (the superior)))
					(the object-id)
					(1+ (the layer)))
	      (the primitive-handles)))
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

(defmethod destroy-window :after ((view text-editor-window-mixin))
  (let ((buffer (hi::window-buffer view)))
    (setf (hi::buffer-windows buffer) (remove view (hi::buffer-windows buffer)))
    (unless (hi::buffer-windows buffer)
      (hi::delete-buffer buffer *echo-area-buffer*)))
  (values))

(defun make-hemlock-window ()
  (make-instance 'text-editor-window 'x 100 'y 75))

(defmethod print-object ((object text-editor-window-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (send object title) stream)))

(defun find-or-create-text-editor-window-for-buffer (buffer)
  (let ((window (or (first (hi::buffer-windows buffer))
		    (make-instance 'text-editor-window :buffer buffer))))
    (focus-window window)
    window))

(defun find-or-create-typeout-window (buffer-name)
  (let ((first (focused-window (clui::window-parent *echo-area-window*)))
	(buffer (hi::make-unique-buffer buffer-name :modes '("Typeout"))))
    (flet ((is-typeout? (w)
	     (string= (send w title) "Random Typeout")))
      (if (is-typeout? first)
	  (progn
	    (setf (window-buffer first) buffer)
	    (setf (send first visible?) t)
	    (focus-window first)
	    first)
	  (progn
	    (do ((w (next first) (next w)))
		((eq w first))
	      (when (is-typeout? w)
		(progn
		  (setf (window-buffer w) buffer)
		  (setf (send w visible?) t)
		  (focus-window w)
		  (return-from find-or-create-typeout-window w))))
	    (make-instance 'text-editor-window :buffer buffer 'visible? t))))))


      



