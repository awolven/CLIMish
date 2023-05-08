(in-package :krma.gui)

(defobject echo-area-window-mixin (text-editor-window-mixin)
  :metaclass echo-area-window-class
  
  :inputs
  ((parent (main-window *app*))
   (collapsed? nil)
   (resizable? nil)
   (memu-bar? nil)
   (scrollbar-x? nil)
   (scrollbar-y? nil)
   (title-bar? nil)
   (modeline? nil)
   (collapse-button? nil)
   (close-button? nil)
   (iconified? nil)
   (rounding? nil))

  :attributes
  ((x 0)
   (y (- (second (the parent-window-size)) (the height)))
   (parent-window-size (multiple-value-list
			(clui:window-framebuffer-size (the parent))))
   (width (first (the parent-window-size)))
   (height (* 4 (font-size (the font))))))


(defobject echo-area-window (echo-area-window-mixin))

(defun make-echo-area-window (&key app)
  (setq hi::*echo-area-stream*
	(hemlock::make-hemlock-interactive-character-io-stream
	 hi::*echo-area-buffer*))
  (setq hi::*echo-area-window*
	(make-instance 'echo-area-window :buffer hi::*echo-area-buffer*
		       'scene (application-scene app)
		       'parent (main-window app)))
  (when (send hi::*echo-area-window* visible?)
    (rm-draw-node-recursively hi::*echo-area-window*))
  hi::*echo-area-window*)
  

   
