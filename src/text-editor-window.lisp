(in-package :krma.gui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass text-editor-command-table-mixin (command-table-with-classes-mixin)
    ())

  (defmethod table-lookup ((table text-editor-command-table-mixin) (key character))
    #'(lambda (window p)
	(handle-character-input window key p)))

  (defclass text-editor-command-table (text-editor-command-table-mixin)
    ())

  (defclass text-editor-window-class (homemade-window-class)
    ((command-table :initform (make-instance 'text-editor-command-table))))
  
  (defmethod validate-superclass ((subclass text-editor-window-class) (superclass homemade-window-class))
    t))

(defobject buffer-view (group-mixin view-mixin)
  
  :components
  ((text :type 'buffer-view-text)))

(defobject buffer-view-text (group-owner-view-mixin))

(defobject text-editor-window-mixin (text-editor-view-mixin homemade-window-mixin)
  :metaclass text-editor-window-class
  
  :inputs
  ((title "hemlock"))
  
  :components
  ((buffer-view :type 'buffer-view
		x (+ (the client-rect x) (frame-padding-x (the style)))
		y (+ (the client-rect y) (frame-padding-y (the style)))
		width (- (the client-rect width) (* 2 (frame-padding-x (the style))))
		height (- (the client-rect height) (* 2 (frame-padding-y (the style)))))))

(defobject text-editor-window (text-editor-window-mixin))

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

(defun hi::internal-make-window (&rest args)
  (apply #'make-instance 'text-editor-window args))


(defmethod add-buffer-line ((self buffer-view-text) (dl hi::window-dis-line))
  (let* ((window (the window))
	 (font (hi::window-font-family window))
	 (chars (hi::dis-line-chars dl))
	 (length (hi::dis-line-length dl))
	 (position (hi::dis-line-position dl))
	 (last 0)
	 (text-color-key 0))
    (print position)
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
    (hi::%rt-update-window-image window)
    (do ((dis-lines (hi::window-first-line window) (cdr dis-lines)))
	((eq dis-lines (slot-value window 'hi::the-sentinel)))
      (add-buffer-line self (car dis-lines)))))

(defun make-hemlock-window (&optional (app *app*))
  (hi::make-editor-window (hi::current-buffer)
			  'parent (main-window app)
			  'translation (vec3 100 100 0)
			  'width 300
			  'height 200
			  'scene (application-scene app)
			  :width-chars 80
			  :height-lines 35))

(defmethod handle-character-input ((window krma.gui::text-editor-window-mixin) char p)
  (dotimes (n p)
    (hi::insert-character (hi::buffer-point (hi::window-buffer window)) char))
  (let ((text (send window buffer-view text)))
    (rm-redraw-node text))
  (values))

(in-package :hi)

(defun krma.gui::command-next-line (window p)
  (let ((w window))
    (when (typep w 'krma.gui::text-editor-window)
      (let ((b (hi::window-buffer w)))
	(let* ((target (with-buffer-globals (b)
			 (hemlock::.set-target-column. .point.))))
          (unless (with-buffer-globals (b) (line-offset .point. (or p 1)))
            (cond ((not p)
                   (with-buffer-globals (b)
                     (when (same-line-p .point. (buffer-end-mark .buffer.))
                       (line-end .point.)))
		   ;;
		   ;; The insert-character was probably intentional back when
		   ;; Hemlock was written, but it inserts bogus newlines at
		   ;; the end of the file, and that's a bad idea.
		   ;;
		   ;; (insert-character point #\newline)
                   (editor-error "No next line."))
                  ((minusp p)
                   (with-buffer-globals (b)
                     (buffer-start .point. .buffer.))
                   (editor-error "No previous line."))
                  (t
                   (with-buffer-globals (b)
                     (buffer-end .point. .buffer.))
                   (when p (editor-error "No next line.")))))
	  (with-buffer-globals (b)
            (unless (.move-to-column. .point. target) (line-end .point.)))
	  (setf (last-command-type) :line-motion)))))
  (krma.gui::redraw-buffer window))

(in-package :krma.gui)

(defun command-previous-line (window p)
  (command-next-line window (- (or p 1))))
  



(defun command-insert-newline (window p)
  (handle-character-input window #\Newline p))
  



(defun command-save-file (window p)
  (declare (ignore window))
  (dotimes (n p)
    (print 'hiya)))



(in-package :hi)

(defun krma.gui::command-beginning-of-line (window p)
  (with-buffer-globals ((window-buffer window))
    (let ((point .point.))
      (unless (line-offset point (if p p 0)) (editor-error "No such line."))
      (line-start point)))
  (krma.gui::redraw-buffer window))

(in-package :krma.gui)



(defun draw-cursor-closure-generator (window scene)
  #'(lambda ()
      (let ((time (get-internal-real-time)))
	(when (evenp (floor time (* 0.5 internal-time-units-per-second)))
	  (let ((self (focused-window window)))
	    (when self
	      (when (typep self 'text-editor-window-mixin)
		(multiple-value-bind (ox oy) (global-window-position (the buffer-view text))
		  (multiple-value-bind (x y) (hi::mark-to-cursorpos (hi::window-point self) self)
		    (let* ((font (the font))
			   (font-size (font-size font))
			   (font-data (font-data font))
			   (space-width (slot-value font-data '3b-bmfont-common::space-size))
			   (xval (+ (* x space-width) ox))
			   (yval (+ (* y font-size) oy)))
		      (scene-draw-filled-2d-rectangle-list
		       scene :cursor #xffff0077
		       (list xval yval (+ xval space-width) (+ yval font-size))
		       0 (1+ (the buffer-view text layer)))))))))))))

#+NIL
(setf (krma::immediate-mode-work-function-1 (clui::window-display (main-window *app*)))
      (draw-cursor-closure-generator (main-window *app*) (application-scene *app*)))

#+NIL
(setf (krma::immediate-mode-work-function-1 (clui::window-display (main-window *app*))) nil)

(defun hi::view-buffer (view)
  (hi::window-buffer view))

(defun redraw-buffer (self)
  (rm-redraw-node (the buffer-view text)))

(in-package :hi)

(defun krma.gui::command-forward-character (view p)
  (let ((p (or p 1)))
    (with-buffer-globals ((view-buffer view))
      (cond ((character-offset .point. p))
	    ((= p 1)
	     (editor-error "No next character."))
	    ((= p -1)
	     (editor-error "No previous character."))
	    (t (if (plusp p)
		   (buffer-end .point. .buffer.)
		   (buffer-start .point. .buffer.))
	       (editor-error "Not enough characters."))))
    (%rt-update-window-image view)))

(defun krma.gui::command-backward-character (view p)
  (krma.gui::command-forward-character view (- (or p 1))))

(defun krma.gui::command-delete-next-character (view p)
  (cond ((with-buffer-globals ((view-buffer view))
	   (hemlock::.kill-characters. .point. (or p 1))))
	((and p (minusp p))
	 (editor-error "Not enough previous characters."))
	(t (editor-error "Not enough next characters.")))
  (krma.gui::redraw-buffer view))

(defun krma.gui::command-delete-previous-character (view p)
  (krma.gui::command-delete-next-character view (- (or p 1))))

(defun krma.gui::command-transpose-characters (view p)
  (let ((buffer (view-buffer view)))
    (when buffer
      (with-buffer-globals (buffer)
	(let ((arg (or p 1))
	      (point (buffer-point buffer)))
	  (dotimes (i (abs arg))
	    (when (or (minusp arg) (end-line-p point))
	      (mark-before point))
	    (let ((prev nil)
		  (next nil))
	      (setq prev (hi::.previous-character. point))
	      (setq next (hi::.next-character. point))
	      (cond ((not prev) (editor-error "No previous character."))
		    ((not next) (editor-error "No next character."))
		    (t
		     (hi::.set-previous-character. next point)
		     (hi::.set-next-character. prev point)))
	      (when (plusp arg) (mark-after point))))))))
  (krma.gui::redraw-buffer view))

(defun krma.gui::command-forward-word (view p)
  (with-buffer-globals ((view-buffer view))
    (cond ((word-offset .point. (or p 1)))
          ((and p (minusp p))
           (buffer-start .point. .buffer.)
           (editor-error "No previous word."))
          (t
           (buffer-end .point. .buffer.)
           (editor-error "No next word."))))
  (krma.gui::redraw-buffer view))







(in-package :krma.gui)
	 
(define-gesture text-editor-window-mixin command-next-line #k"DownArrow")
(define-gesture text-editor-window-mixin command-next-line #k"Ctrl-n")
(define-gesture text-editor-window-mixin command-previous-line #k"UpArrow")
(define-gesture text-editor-window-mixin command-insert-newline #k"Enter")
(define-gesture text-editor-window-mixin command-save-file #k"C-x" #k"C-s")
(define-gesture text-editor-window-mixin command-beginning-of-line #k"Ctrl-a")
(define-gesture text-editor-window-mixin command-forward-character #k"RightArrow")
(define-gesture text-editor-window-mixin command-forward-character #k"Ctrl-f")
(define-gesture text-editor-window-mixin command-backward-character #k"LeftArrow")
(define-gesture text-editor-window-mixin command-backward-character #k"Ctrl-b")
(define-gesture text-editor-window-mixin command-transpose-characters #k"Ctrl-t")

(defobject echo-area-mixin (text-editor-window-mixin)
  :inputs
  ((parent (main-window *app*))
   (scene (application-scene *app*))
   (style *style*)
   (font (current-font (the style)))

   (window self))

  :attributes
  ((collapsed? nil)
   (resizable? nil)
   (menu-bar? nil)
   (title-bar? nil)
   (collapse-button? nil)
   (close-button? nil)
   (iconified? nil)
   (rounding? nil)
   (alpha 1.0)
   (border-size 0)
   (x :uncached (car (the window-pos)))
   (y :uncached (- (cadr (the window-pos)) (the height)))
   (width :uncached (car (the window-size)))
   (height (title-bar-height (the style)))
   (window-pos :uncached (multiple-value-list (window-position (the parent))))
   (window-size :uncached (multiple-value-list (window-framebuffer-size (the parent))))))
   
