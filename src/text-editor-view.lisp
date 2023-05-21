(in-package :krma.gui)

(defun hemlock::canonicalize-slave-package-name (str)
  (cl-ppcre:regex-replace "^SB!" (hi::canonical-case str) "SB-"))

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
  (let ((buffer (hi::window-buffer view)))
    (setf (hi::buffer-windows buffer) (remove view (hi::buffer-windows buffer)))
    (unless (hi::buffer-windows buffer)
      (hi::delete-buffer buffer *echo-area-buffer*)))
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
	      (unless (the iconified?)
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
			   0 (1+ (the buffer-view text layer)))))))))))))))

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

(defmethod rm-redraw-node ((text buffer-view-text))
  (let ((window (send text window)))
    (unless (send window iconified?)
      (let*((xp (send text superior x))
	    (yp (send text superior y))
	    (scene (send text scene))
	    (group (send text group))
	    (layer (float (send text layer)))
	    (font (hi::window-font-family window))
	    (font-size (font-size font))
	    (font-data (font-data font))
	    (space-width (slot-value font-data '3b-bmfont-common::space-size))
	    (gm (send text global-matrix)))
	(update-window-image window)
	(krma:rm-dispatch-to-render-thread (scene draw-data)
	  (krma::delete-groups-1 draw-data (list group) nil)
	  (when (slot-boundp window 'hi::the-sentinel)
	    (let ((last-line (car (hi::window-last-line window)))
		  (last-line-seen? nil))
	      (do ((dis-lines (hi::window-first-line window) (cdr dis-lines)))
		  ((or last-line-seen?
		       (null dis-lines)
		       (eq dis-lines (slot-value window 'hi::the-sentinel))))
		(let* ((dl (car dis-lines))
		       (chars (hi::dis-line-chars dl))
		       (length (hi::dis-line-length dl))
		       (position (hi::dis-line-position dl))
		       (last 0)
		       (text-color-key 0))
		  (setq last-line-seen? (eq dl last-line))
		  (flet ((buffer-view-add-string (x y font text-color-key string start end)
			   (unless end (setq end length))
			   (unless start (setq start 0))
			   (when (< start end)
			     (let* ((color (lookup-text-color (or text-color-key 0)))
				    (xval (float (+ (* x space-width) xp)))
				    (yval (float (+ (* y font-size) -2 yp))))
			       (krma::draw-data-add-text-primitive draw-data group nil font color
								   xval yval (subseq string start end)
								   0 layer)))))
		    (do ((change (hi::dis-line-font-changes dl) (hi::font-change-next change)))
			((null change)
			 (buffer-view-add-string last position font text-color-key chars last length))
		      (let ((x (hi::font-change-x change)))
			(buffer-view-add-string last position font text-color-key chars last x)
			(setq last x
			      text-color-key (hi::font-change-font change)))))))))
	  (krma::group-set-model-matrix-1 draw-data group gm)
	  (values))))))

(defmethod handle-character-input ((window text-editor-view-mixin) char p)
  (dotimes (n p)
    (hi::insert-character (hi::buffer-point (hi::window-buffer window)) char))
  (redraw-buffer window)
  (values))

(defun hi::view-buffer (view)
  (hi::window-buffer view))

(defun redraw-buffer (self)
  (unless (the iconified?)
    (rm-redraw-node (the buffer-view text))))
