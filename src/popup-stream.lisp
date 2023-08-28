(in-package :krma.gui)

(defobject popup-stream-mixin (trivial-gray-streams::fundamental-character-output-stream homemade-window-mixin)

  :slots ((buffer-string :initform "" :accessor popup-stream-buffer-string))
  
  :inputs
  ((menu-bar? nil)
   (scrollbar-x? nil)
   (scrollbar-y? nil)
   (title-bar? nil)
   (iconify-button? nil)
   (close-button? nil)

   (width 475)
   (height 275)

   (color #(1.0f0 1.0f0 1.0f0 0.5f0))

   (fg-color #(0.0f0 0.3f0 1.0f0 0.5f0))
		    

   (point (vec2 10 10))))

(defmethod initialize-instance :after ((self popup-stream-mixin) &rest initargs)
  (declare (ignore initargs))
  (let ((parent (the parent)))
    (multiple-value-bind (dx dy) (window-framebuffer-size parent)
      (declare (ignore dy))
      (set-window-position self (- dx (the width) 100) 50))
    (rm-redraw-node-recursively self)
    (values)))

(defmethod rm-draw-node ((self popup-stream-mixin))
  (call-next-method)
  (let ((string (popup-stream-buffer-string self)))
    (loop
	with pos = nil
	with start = 0
	do (setq pos (position #\Newline string :start start))
	   (if pos
	       (let ((point (the point)))
		 (unless (= start pos)
		   (add-text (subseq string start pos) (vx point) (vy point)
			     :color (the fg-color)
			     :group (the group) :elevation (1+ (the elevation))))
		 (setf (the point) (let* ((y (vy (the point)))
					  (new-y (+ y (font-size (the style current-font)))))
				     (vec2 10 new-y))))
	       (let ((point (the point)))		 
		 (add-text (subseq string start) (vx point) (vy point)
			   :color (the fg-color)
			   :group (the group) :elevation (1+ (the elevation)))))
	   (unless pos (return))
	   (setq start (1+ pos)))
    (setf (the point) (vec2 10 10)))
  (values))

(defmethod trivial-gray-streams:stream-write-char ((stream popup-stream-mixin) character)
  (setf (popup-stream-buffer-string stream)
	(concatenate 'string (popup-stream-buffer-string stream) (list character)))
  (values))

(defmethod trivial-gray-streams:stream-finish-output ((stream popup-stream-mixin))
  (rm-redraw-node-recursively stream))

(defmethod trivial-gray-streams:stream-force-output ((stream popup-stream-mixin))
  (rm-redraw-node-recursively stream))

(defmethod trivial-gray-streams:stream-line-column ((stream popup-stream-mixin))
  (let* ((point (send stream point))
	 (font (send stream style current-font))
	 (font-data (font-data font))
	 (glyph-table (slot-value font-data '3b-bmfont-common::chars))
	 (glyph (gethash #\X glyph-table)))
    (round (/ (- (vx point) 10) (3b-bmfont:glyph-xadvance glyph)))))


(defmethod trivial-gray-streams:stream-advance-to-column ((stream popup-stream-mixin) column)
  (let* ((point (send stream point))
	 (font (send stream style current-font))
	 (font-data (font-data font))
	 (glyph-table (slot-value font-data '3b-bmfont-common::chars))
	 (glyph (gethash #\X glyph-table)))
    (setf (send stream point) (vec2 (+ 10 (* column (3b-bmfont:glyph-xadvance glyph)))
				    (vy point)))
    t))

(defmethod trivial-gray-streams:stream-write-string ((stream popup-stream-mixin) string &optional (start 0) (end (length string)))
  (setf (popup-stream-buffer-string stream)
	(concatenate 'string (popup-stream-buffer-string stream)
		     (with-output-to-string (st)
		       (princ (subseq string start end) st))))
  (values))

(defobject popup-stream (popup-stream-mixin))

(defun change-alpha (color alpha)
  (etypecase color
    (integer (logior (logand color #xffffff00) (max alpha 0)))
    (vector (let ((new-color (copy-seq color)))
	      (setf (aref new-color 3) (max (vk::clampf (/ alpha 255)) 0.0f0))
	      new-color))))
  
(defun set-popup-timer ()
  (let ((os-window (main-window *app*)))
    (bt:make-thread
     #'(lambda ()
	 (let ((popup-window (popup-window os-window))
	       (alpha #x99)
	       (count 0))
	   (unless popup-window
	     (setq popup-window
		   (setf (popup-window os-window) (make-instance 'popup-stream))))
	   (rm-redraw-node-recursively popup-window)
	   (loop
	     (when (> (incf count) 20)
	       (return))
	     (when (<= alpha #x05)
	       (return))
	     (sleep 0.5)
	     (setf (send popup-window color)
		   (change-alpha (send popup-window color) (decf alpha #x05)))
	     (setf (send popup-window fg-color)
		   (change-alpha (send popup-window fg-color) (decf alpha #x05)))
	     (rm-redraw-node-recursively popup-window))
	   (destroy-window popup-window)
	   (setf (popup-window os-window) nil))))))

(defun make-popup-window ()
  (let ((popup-window (popup-window (krma:main-window krma:*app*))))
    (unless popup-window
      (setq popup-window
	    (setf (popup-window (krma:main-window krma:*app*)) (make-instance 'popup-stream))))
    (set-popup-timer)
    popup-window))
	   
	     

				      
   
