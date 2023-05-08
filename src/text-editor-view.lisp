(in-package :krma.gui)

(defobject buffer-view (group-mixin view-mixin)
  
  :components
  ((text :type 'buffer-view-text)))

(defobject buffer-view-text (group-owner-view-mixin))

(defobject text-editor-view-mixin (view-mixin hi::window)
  :metaclass text-editor-view-class

  :components
  ((buffer-view :type 'buffer-view
		x (+ (the client-rect x) (the style frame-padding-x))
		y (+ (the client-rect y) (the style frame-padding-y))
		width (- (the client-rect width) (* 2 (the style frame-padding-x)))
		height (- (the client-rect height) (* 2 (the style frame-padding-y))))))

(defmethod resize-window :after ((view text-editor-view-mixin) new-x new-y new-width new-height)
  (declare (ignore new-x new-y new-height new-width))
  (let ((font-family (hi::window-font-family view)))
    (setf (hi::window-width view) (floor (send view client-rect width) (slot-value (slot-value font-family 'krma::data) '3b-bmfont-common::space-size))
	  (hi::window-height view) (floor (send view client-rect height) (font-size font-family)))

    (invalidate-hemlock-window-size view)
    
    (hi::update-window-image view)
    (redraw-buffer view)
    (values)))

(defun invalidate-hemlock-window-size (view)
  (with-slots (hi::the-sentinel) view
    (unless (eq (cdr (hi::window-first-line view)) hi::the-sentinel)
      (shiftf (cdr (hi::window-last-line view))
	      (hi::window-spare-lines view)
	      (cdr (hi::window-first-line view))
	      hi::the-sentinel))
                
    (do ((i (- (hi::window-height view)) (1+ i))
	 (res ()
	      (cons (hi::make-window-dis-line (make-string (hi::window-width view))) res)))
	((= i (hi::window-height view)) (setf (hi::window-spare-lines view) res)))))

(defmethod destroy-window :after ((view text-editor-view-mixin))
  (setf (hi::buffer-windows (hi::window-buffer view)) (remove view (hi::buffer-windows (hi::window-buffer view))))
  (values))	

(defmethod initialize-instance :after ((view text-editor-view-mixin) &rest initargs
				       &key (font-family (send view style current-font))
					 (width-chars
					  (floor (/ (send view width)
						    (slot-value (slot-value font-family 'krma::data) '3b-bmfont-common::space-size))))
					 (height-lines
					  (floor (/ (send view height)
						    (slot-value (slot-value font-family 'krma::data) '3b-bmfont-common::line-height))))
					 &allow-other-keys)
  (declare (ignore initargs))

  (setf (hi::window-font-family view) font-family)
  (setf (hi::window-width view) width-chars)
  (setf (hi::window-height view) height-lines)
  
  (values))

(defun draw-cursor-closure-generator (window scene)
  #'(lambda ()
      (let ((time (get-internal-real-time)))
	(when (evenp (floor time (* 0.5 internal-time-units-per-second)))
	  (let ((self (focused-window window)))
	    (when self
	      (when (typep self 'text-editor-window-mixin)
		(multiple-value-bind (ox oy) (global-window-position (the buffer-view text))
		  (multiple-value-bind (x y) (hi::mark-to-cursorpos (hi::window-point self) self)
		    (when x
		      (let* ((font (the font))
			     (font-size (font-size font))
			     (font-data (font-data font))
			     (space-width (slot-value font-data '3b-bmfont-common::space-size))
			     (xval (+ (* x space-width) ox))
			     (yval (+ (* y font-size) oy)))
			(scene-draw-filled-2d-rectangle-list
			 scene :cursor #xffff0077
			 (list xval yval (+ xval space-width) (+ yval font-size))
			 0 (1+ (the buffer-view text layer))))))))))))))

(defmethod initialize-instance :around ((self text-editor-view-mixin) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (let ((os-window (window-parent (the root))))
    (setf (krma::immediate-mode-work-function-1 (clui::window-display os-window))
	  (draw-cursor-closure-generator os-window (the scene))))
  (rebuild-command-table self)
  self)

#+NIL
(setf (krma::immediate-mode-work-function-1 (clui::window-display (main-window *app*))) nil)


(defconstant hi::+default-fg-color+ 0)
(defconstant hi::+comment-fg-color+ 1)
(defconstant hi::+bkquote-fg-color+ 2)
(defconstant hi::+unquote-fg-color+ 3)
(defconstant hi::+strings-fg-color+ 4)
(defconstant hi::+quote-fg-color+ 5)
(defconstant hi::+sharp-plus-fg-color+ 6)
(defconstant hi::+sharp-minus-fg-color+ 7)
(defconstant hi::+def-fg-color+ 8)
(defconstant hi::+name-fg-color+ 9)
(defconstant hi::+keyword-fg-color+ 10)
(defconstant hi::+integer-fg-color+ 11)


(defun lookup-text-color (key)
  (elt #(;;;fg
         #xffffffff
         #x019901ff     ;1 = comments

         #x00ffffff     ;2 = backquote
         #x009999ff     ;3 = unquote

         #xff9900ff     ;4 = strings
         #xffffffff     ;5 = quote
         #xff0000ff     ;6 = #+
         #x770000ff     ;7 = #-

         #x03ff03ff
         #xffff00ff
         #xff00ffff
         #xcc00ccff

         #x000000ff
         #x000000ff
         #x000000ff
         #x000000ff
         #x000000ff)
       (mod key 17)))

(defmethod add-buffer-line ((self buffer-view-text) (dl hi::window-dis-line))
  (let* ((window (the window))
	 (font (hi::window-font-family window))
	 (chars (hi::dis-line-chars dl))
	 (length (hi::dis-line-length dl))
	 (position (hi::dis-line-position dl))
	 (last 0)
	 (text-color-key 0))
    (do ((change (hi::dis-line-font-changes dl) (hi::font-change-next change)))
	((null change)
	 (buffer-view-add-string self last position font text-color-key chars last length))
      (let ((x (hi::font-change-x change)))
	(buffer-view-add-string self last position font text-color-key chars last x)
	(setq last x
	      text-color-key (hi::font-change-font change))))))

(defmethod buffer-view-add-string ((self buffer-view-text) x y font text-color-key string start end)
  (let* ((font-size (font-size font))
	 (font-data (font-data font))
	 (space-width (slot-value font-data '3b-bmfont-common::space-size))
	 (color (lookup-text-color text-color-key))
	 (xval (+ (* x space-width) (the superior x)))
	 (yval (+ (* y font-size) -2 (the superior y))))
    (scene-add-text-primitive (the scene) (the group) nil font color xval yval (subseq string start end) 0 (the layer))
    (values)))

(defmethod rm-draw-node ((self buffer-view-text))
  (let ((window (the window)))
    (hi::update-window-image window)
    (when (slot-boundp window 'hi::the-sentinel)
      (do ((dis-lines (hi::window-first-line window) (cdr dis-lines)))
	  ((eq dis-lines (slot-value window 'hi::the-sentinel)))
	(add-buffer-line self (car dis-lines))))))

(defmethod handle-character-input ((window text-editor-view-mixin) char p)
  (dotimes (n p)
    (hi::insert-character (hi::buffer-point (hi::window-buffer window)) char))
  (redraw-buffer window)
  (values))

(defun hi::view-buffer (view)
  (hi::window-buffer view))

(defun redraw-buffer (self)
  (rm-redraw-node (the buffer-view text)))
