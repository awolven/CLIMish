(in-package :climish)

(defobject modeline (selectable-mixin group-mixin view-mixin)
  :inputs
  ((bg-color :visual #x73737399)
   (fg-color :visual #x000000ff))
  
  :attributes
  ((height (the style title-bar-height))

   (pad-left (+ (the style frame-padding-x) (if (eq (the style window-menu-button-position) :left)
						  (the button-size)
						  0)))
   (pad-right (the style frame-padding-x))
   
   (iconified? (the window iconified?))))

(defmethod rm-draw-node ((self modeline))
  (when (window-buffer (the window))
    (unless (the iconified?)
      (hi::update-modeline-fields (hi::window-buffer (the window)) (the window))
      (let ((scene (the scene))
	    (group (the group)))
    
	(push (scene-add-filled-2d-rectangle-list-primitive
	       scene group nil (the bg-color)
	       (list (the x0) (the y0) (the x1) (the y1))
	       (the object-id)
	       (the elevation))
	      (the primitive-handles))

	(let ((modeline (hi::window-modeline-buffer (the superior))))
	  (when modeline
	    (push (scene-add-text-primitive scene group nil (the superior font) (the fg-color) (the x0) (the y0)
					    (subseq modeline 0
						    (min (length modeline)
							 (hi::window-modeline-buffer-len (the superior))))
					    (the object-id)
					    (1+ (the elevation)))
		  (the primitive-handles))))
	(values)))))

(defobject text-editor-window-mixin (text-editor-view-mixin homemade-window-mixin)
  :inputs
  ((modeline? :visual t))

  :attributes
  ((title :uncached (or (when (hi::window-buffer self) (hi::buffer-%name (hi::window-buffer self)))
			"hemlock"))

   (base-z (* (the z-index) krma::+view-depth+))
   
   (translation-matrix
    (let* ((origin (the origin))
	   (x (vx origin))
	   (y (vy origin))
	   (z (the base-z)))
      (mat4 1 0 0 x
	    0 1 0 y
	    0 0 1 z
	    0 0 0 1))))

  :components
  ((modeline :type (if (the modeline?) 'modeline 'null-object)
	     :eager t
	     y (+ (the client-rect y) (the client-rect height)))

   (client-rect :type 'client-rect
		:eager t
		y (if (the title-bar?) (the title-bar y1) (the y))
		height (cond ((and (the title-bar?) (the modeline?))
			      (- (the height) (* 2 (the style title-bar-height))))
			     ((or (the title-bar?) (the modeline?))
			      (- (the height) (the style title-bar-height)))
			     (t (the height))))))

(defmethod initialize-instance ((self text-editor-window-mixin) &rest initargs)
  (declare (ignorable initargs))
  (prog1 (call-next-method)
    (when (hi::window-buffer self)
      (hi::setup-modeline-image (hi::window-buffer self) self))))

(defobject text-editor-window (text-editor-window-mixin))
(defobject lisp-listener-window (text-editor-window-mixin))
(defobject cmd-window (text-editor-window-mixin))
(defobject dired-window (text-editor-window-mixin))

(defun find-or-create-window-of-class (class &rest initargs)
  (let* ((start hi::*echo-area-window*)
	 (prev start))

    (tagbody
     prev
       (setq prev (prev prev))

       (when (eq prev start)
	 (return-from find-or-create-window-of-class (apply #'make-instance class initargs)))

       (when (typep prev class)
	 (return-from find-or-create-window-of-class prev))

       (go prev))))

       

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

(defmethod find-or-create-text-editor-window-for-buffer-1
    ((frame application-frame-mixin) buffer)
  (let ((window (or (first (hi::buffer-windows buffer))
		    (let ((focused (focused-window frame)))
		      (when (typep focused 'application-frame-mixin)
			(setq focused *echo-area-window*))
		      (if (or (eq focused *echo-area-window*)
			      (not (typep focused 'text-editor-window)))
			  (let ((next (next focused)))
			    (if (or (eq next *echo-area-window*)
				    (not (typep focused 'text-editor-window)))
				(make-instance 'text-editor-window :buffer buffer)
				next))
			  focused)))))
    (change-to-buffer buffer window)
    (focus-window window)
    window))
	

(defun find-or-create-text-editor-window-for-buffer (buffer)
  (find-or-create-text-editor-window-for-buffer-1
   (application-frame *echo-area-window*) buffer))

(defun find-or-create-typeout-window (buffer-name)
  (let (#+NIL(first (focused-window (climish::application-frame *echo-area-window*)))
	(buffer (hi::make-unique-buffer buffer-name :modes '("Typeout"))))
    (make-instance 'text-editor-window :buffer buffer 'visible? t)
    #+NIL
    (flet ((is-typeout? (w)
	     (string= (send w title) "Random Typeout")))
      (if (is-typeout? first)
	  (progn
	    (setf (window-buffer first) buffer)
	    (setf (send first visible?) t)
	    ;;;(focus-window first)
	    first)
	  (let ((found? nil))
	    (do ((w (next first) (next w)))
		((eq w first))
	      (when (is-typeout? w)
		(setq found? t)
		(setf (window-buffer w) buffer)
		(setf (send w visible?) t)
		  ;;;(focus-window w)
		(return-from find-or-create-typeout-window w)))
	    (unless found?
	      ))))))


      



