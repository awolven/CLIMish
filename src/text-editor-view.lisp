(in-package :climish)



(defun hemlock::canonicalize-slave-package-name (str)
  (cl-ppcre:regex-replace "^SB!" (hi::canonical-case str) "SB-"))

(defobject buffer-view (group-mixin view-mixin)
  :inputs
  ((font :visual))

  :attributes
  ((elevation (+ 0.1 (the superior elevation))))
  
  :components
  ((text :type 'buffer-view-text
	 :eager t)))

(defobject buffer-view-text (group-owner-view-mixin)
  :attributes
  ((iconified? (the window iconified?))
   (elevation (+ 0.1 (the superior elevation)))))

(defobject text-editor-view-mixin (view-mixin textual-view hi::window)
  :metaclass text-editor-view-class

  :slots
  ((dragging-select-box? :initform nil)
   (maybe-drag :initform nil))

  :components
  ((buffer-view :type 'buffer-view
		:eager t
		font (the font)
		x (+ (the client-rect x) (the style frame-padding-x))
		y (+ (the client-rect y) (the style frame-padding-y))
		width (- (the client-rect width) (* 2 (the style frame-padding-x)))
		height (- (the client-rect height) (* 2 (the style frame-padding-y))))))

(defun resize-text-editor-window-before (view)
  (let ((font-family (send view font)))
    (setf (hi::window-width view) (floor (send view client-rect width) (slot-value (slot-value font-family 'krma::data) '3b-bmfont-common::space-size))
	  (hi::window-height view) (floor (send view client-rect height) (* 1.2 (font-size font-family))))
    
    (invalidate-hemlock-window-size view)
    
    (hi::update-window-image view)
    
    (values)))
  

(defmethod resize-window :before ((view text-editor-view-mixin) new-x new-y new-width new-height)
  (declare (ignore new-x new-y new-height new-width))
  (resize-text-editor-window-before view))

(defun invalidate-hemlock-window-size (view)
  (bt:with-recursive-lock-held ((hi::window-lock view))
    (when (and (> (hi::window-height view) 0)
	       (> (hi::window-height view) 0))
      (with-slots (hi::the-sentinel) view
	(unless (eq (cdr (hi::window-first-line view)) hi::the-sentinel)
	  (shiftf (cdr (hi::window-last-line view))
		  (hi::window-spare-lines view)
		  (cdr (hi::window-first-line view))
		  hi::the-sentinel))

	(do ((i (- (hi::window-height view)) (1+ i))
	     (res ()
		  (cons (hi::make-window-dis-line (make-string (hi::window-width view))) res)))
	    ((= i (hi::window-height view)) (setf (hi::window-spare-lines view) res)))))))

	

(defmethod initialize-instance ((view text-editor-view-mixin) &rest initargs
				&key (font-family nil)
				  (width-chars nil)
				  (height-lines nil)
				&allow-other-keys)
  (declare (ignore initargs))

  (call-next-method)

  (unless font-family
    (setq font-family (send view style current-font)))

  (unless width-chars
    (setq width-chars (floor (send view client-rect width) (slot-value (slot-value font-family 'krma::data) '3b-bmfont-common::space-size))))

  (unless height-lines
    (setq height-lines (floor (send view client-rect height) (* 1.2 (font-size font-family)))))
  
  (setf (hi::window-font-family view) font-family)
  (setf (hi::window-width view) width-chars)
  (setf (hi::window-height view) height-lines)

  (invalidate-hemlock-window-size view)

  ;;(hi::update-window-image view)
  
  view)

(defun draw-cursor-closure-generator (frame scene)
  #'(lambda ()
      (let ((time (get-internal-real-time)))
	(when (evenp (floor time (* 0.425 internal-time-units-per-second)))
	  (let ((window (focused-window frame)))
	    (when (not (eq window frame))
	      (unless (send window iconified?)
		(let ((self (focused-view window)))
		  (when (typep self 'text-editor-view-mixin)
		    (multiple-value-bind (ox oy) (global-window-position (the buffer-view text))
		      (multiple-value-bind (x y) (mark-to-cursorpos (hi::window-point self) self)
			(when x
			  (let* ((font (the font))
				 (font-size (font-size font))
				 (font-data (font-data font))
				 (space-width (slot-value font-data '3b-bmfont-common::space-size))
				 (xval (+ (* x space-width) ox))
				 (yval (+ (* y 1.2 font-size) oy)))
			    (scene-draw-filled-2d-rectangle-list
			     scene :cursor #xffff0077
			     (list xval yval (+ xval space-width) (+ yval (* 1.2 font-size)))
			     0 (+ (send window base-z)
				  (1+ (the buffer-view text elevation)))))))))))))))))


(defun cursorpos-to-mark (x y window)
  (with-buffer-globals ((window-buffer window))
    (if (or (minusp x) (minusp y))
      
	(copy-mark (window-display-start window))
    
	(let ((last-lines (hi::window-last-line window)))
	
	  (if (and (<= y (hi::dis-line-position (car last-lines)))
		   (<= x (hi::window-width window)))
	    
	      (with-slots (hi::the-sentinel) window

		(labels ((line-exists? (this)
			   (and this
				(not (eql this hi::the-sentinel))
				(not (eql this (hi::window-first-line window)))))
		       
			 (continuing-wrapped-line? (this prev)
			   (and (line-exists? prev)
				(eq (hi::dis-line-line (car prev))
				    (hi::dis-line-line (car this))))))

		  (let ((dis-lines (hi::window-first-line window))
			(line)
			(line-string)
			(charpos)
			(offset))

		    (unless (cdr dis-lines)
		      (return-from cursorpos-to-mark nil)) ;; window image is bogus

		    (when (eql (cdr dis-lines) hi::the-sentinel) ;; there are no lines
		      (return-from cursorpos-to-mark nil))

		    (flet ((actual-mark-position (position line-string)
			     (macrolet ((%fcwa (str start end)
					  `(hi::%sp-find-character-with-attribute
					    ,str ,start ,end hi::*losing-character-mask* hi::losing-char))
					(return-pos ()
					  `(if (< position total)
					       (return count)
					       (return (- position total)))))
			       (loop
				 with xpos = 0
				 with total = 0
				 with start = 0
				 with losing = nil
				 with last-losing = 0
				 with count = -1
				 when (>= count 1000)
				   do (return-pos)
				 when (>= start (length line-string))
				   do (return-pos)
				 do (setq losing (%fcwa line-string start (length line-string)))
				    (if losing
					(progn (incf count)
					       (when (>= losing (- position total))
						 (return-pos))
					       (setq start (1+ losing))
					       (incf xpos (- losing last-losing))
					       (incf xpos total)
					       (let ((str (hi::get-rep (char line-string losing))))
						 (setq last-losing losing)
						 (when (functionp str)
						   (setq str (funcall str (mod xpos (hi::window-width window)))))
						 (let ((len (hi::strlen str)))
						   (incf total len)
						   #+NIL
						   (if (zerop count)
						       (incf total len)
						       (incf total (1- len))
						       ))))
					(return-pos))))))

		      (setq line (hi::dis-line-line (cadr dis-lines)))
		      (setq line-string (line-string (window-buffer window) line))

		      (tagbody

		       check

			 (if (continuing-wrapped-line? (cdr dis-lines) dis-lines)
			     (progn (incf offset (1- (hi::window-width window))))
			     (setq offset 0))

			 (when (= y (hi::dis-line-position (cadr dis-lines)))
			   (setq charpos (actual-mark-position (+ offset x) line-string))
			   (go exit))
		       
			 (setq dis-lines (cdr dis-lines))

			 (unless (cdr dis-lines)
			   (go exit))

			 (when (eql (cdr dis-lines) hi::the-sentinel)
			   (go exit))
		       
			 (setq line (hi::dis-line-line (cadr dis-lines)))
			 (setq line-string (line-string (window-buffer window) line))
		       
			 (go check)
		       
		       exit)
		    
		      (hi::internal-make-mark line (min charpos (length line-string)) :temporary)))))
	    
	      (copy-mark (window-display-end window)))))))

(defun wrapped-line? (window dis-lines)
  (declare (ignorable window))
  (and dis-lines
       (cdr dis-lines)
       (eql (hi::dis-line-line (car dis-lines))
	    (hi::dis-line-line (cadr dis-lines)))))

(defun line-exists? (window this)
  (and this
       (not (eql this (slot-value window 'hi::the-sentinel)))
       (not (eql this (hi::window-first-line window)))))

(defun continuing-wrapped-line? (window this prev)
  (and (line-exists? window prev)
       (eq (hi::dis-line-line (car prev))
	   (hi::dis-line-line (car this)))))

(defun mark-to-cursorpos (mark window)
  (multiple-value-bind (dis-lines x) (find-dis-line window mark)
    (let ((y 0))
      (when dis-lines
	(setq y (hi::dis-line-position (car dis-lines))))
      (values x y))))  

(defun find-dis-line (window mark)
  ;; this function has to be resilient to broken windows
  ;; hence checking for non-null dis-lines and cdr dis-lines and such

  (with-buffer-globals ((window-buffer window))
    (unless (or (mark< mark (hi::window-display-start window))
		(mark> mark (hi::window-display-end window)))
  
      (let* ((dis-lines (hi::window-first-line window))
	     (the-sentinel (slot-value window 'hi::the-sentinel))
	     (line (hi::mark-line mark))
	     (charpos (hi::mark-charpos mark))
	     (offset 0)
	     (total 0)
	     (i 0))

	(unless dis-lines
	  (return-from find-dis-line (values nil 0))) ;; window image is corrupt
    
	(when (eql dis-lines hi::the-sentinel) ;; there are no lines, pathological case
	  (return-from find-dis-line (values nil 0)))

	(when (eql (cdr dis-lines) hi::the-sentinel) ;; there are no lines, normal case
	  (return-from find-dis-line (values nil 0)))

	;; find first dis-line with line as it's dis-line-line
	(tagbody
	 check
	   (when (or (null (cdr dis-lines)) ;; invalid window image
		     (eq (hi::dis-line-line (cadr dis-lines)) line) ;; end case
		     (eql (cdr dis-lines) the-sentinel)) ;; hard stop
	     (go exit))

	   (setq dis-lines (cdr dis-lines))
       
	   (go check)
       
	 exit)

	;; compute the offset into the line taking into account character print representation length
	(let ((line-string (line-string (window-buffer window) line)))
	  (setq charpos (min charpos (length line-string)))
	  (macrolet ((%fcwa (str start end)
		       `(hi::%sp-find-character-with-attribute
			 ,str ,start ,end hi::*losing-character-mask* hi::losing-char)))
	    (loop
	      with xpos = 0
	      with start = 0
	      with losing = nil
	      with last-losing = 0
	      with count = -1
	      when (>= count 1000)
		do (return)
	      when (>= start charpos)
		do (return)
	      do (setq losing (%fcwa line-string start charpos))
		 (if losing
		     (progn (incf count)
			    (setq start (1+ losing))
			    (incf xpos (- losing last-losing))
			    (setq last-losing losing)
			    (let ((str (hi::get-rep (char line-string losing))))
			      (when (functionp str)
				(setq str (funcall str (mod xpos (hi::window-width window)))))
			      (let ((len (hi::strlen str)))
				(incf xpos (1- len))
				(incf total (1- len))
				)))
		     (return)))))

	(incf charpos total)

	(setq offset charpos)

	(labels ((return-charpos ()
		   (return-from find-dis-line (values (cdr dis-lines) (+ offset i)))))

	  (if (wrapped-line? window (cdr dis-lines))
	    
	      (progn
		(loop with count = -1
		      while (cdr dis-lines)

		      when (> (incf count) 1000)
			;; runaway bug or runaway wrapped line
			do (return-charpos)
	  
		      when (null (cdr dis-lines))
			;; window image is bogus
			do (return-from find-dis-line (values dis-lines (+ offset i)))

		      when (< (+ offset i) (hi::dis-line-length (cadr dis-lines)))
			do (cond ((and (wrapped-line? window (cdr dis-lines))
				       (= (+ offset i) (1- (hi::window-width window))))
				  ;; special case for when cursor would
				  ;; be displayed on #\! line-wrap char
				  (setq dis-lines (cdr dis-lines))

				  (when (or (null (cdr dis-lines))
					    (eql (cdr dis-lines) hi::the-sentinel)) ;; this is unexpected
				    (return-from find-dis-line (values dis-lines offset)))
			 
				  (setq offset 0)
				  (setq i 0)))
		       
			   (return-charpos)

		      when (and (not (wrapped-line? window (cdr dis-lines)))
				(= (+ offset i) (hi::window-width window)))
			;; special case where end of line is exactly up to window width
			;; we want the cursor to display on the next line
			;; not out in space off the window
			do (setq dis-lines (cdr dis-lines))

			   (when (or (null (cdr dis-lines))
				     (eql (cdr dis-lines) hi::the-sentinel)) ;; this is unexpected
			     (return-from find-dis-line (values dis-lines offset)))
			 
			   (setq offset 0)
			   (setq i 0)
			   (return-charpos)

		      when (wrapped-line? window (cdr dis-lines))
			do (setq dis-lines (cdr dis-lines))
			   (incf i)
			   (decf offset (hi::window-width window))

		      when (null (cdr dis-lines))
			do (return-from find-dis-line (values dis-lines (+ offset i)))
	     
		      when (eql (cdr dis-lines) hi::the-sentinel)
		       		   
			do (return-charpos)))

	      (return-charpos)))))))

(defun draw-active-region-closure-generator (frame scene)
  #'(lambda ()
      (block draw-region
	(let ((window (focused-window frame)))
	  (when (and (not (eq window frame))
		     (send window visible?)
		     (not (send window iconified?)))
	    (let ((self (focused-view window)))
	      (when (typep self 'text-editor-view-mixin)
		(let ((buffer (window-buffer self)))
		  (when (hemlock::buffer-active-region-p buffer)
		    (with-buffer-globals (buffer)
		      (with-slots (hi::the-sentinel) self
			(let* ((font (the font))
			       (font-size (font-size font))
			       (font-data (font-data font))
			       (space-width (slot-value font-data '3b-bmfont-common::space-size)))
			  (multiple-value-bind (ox oy) (global-window-position (the buffer-view text))
			    (let* ((active-region (current-region buffer nil nil)))
			      (when active-region
				(let ((m0 (hi::region-start active-region))
				      (m1 (hi::region-end active-region)))
				  (when (and (hi::mark-line m0)
					     (hi::mark-line m1))
				    (unless (hi::mark= m0 m1)
				      (flet ((before-start? (mark start)
					       (let* ((line (hi::mark-line mark))
						      (number (hi::line-number line))
						      (charpos (hi::mark-charpos mark))
						      (offset (hi::mark-charpos start))
						      (start-number (hi::line-number (hi::mark-line start))))
						 (or (< number start-number)
						     (and (= number start-number) (< charpos offset)))))
					     (after-end? (mark end)
					       (let* ((line (hi::mark-line mark))
						      (number (hi::line-number line))
						      (charpos (hi::mark-charpos mark))
						      (offset (hi::mark-charpos end))
						      (end-number (hi::line-number (hi::mark-line end))))
						 (or (> number end-number)
						     (and (= number end-number) (> charpos offset))))))

					(let ((start-mark (hi::window-display-start self))
					      (end-mark (hi::window-display-end self)))

					  (when (or (before-start? m1 start-mark)
						    (after-end? m0 end-mark))
					    (return-from draw-region (values)))

					  (when (before-start? m0 start-mark)
					    (setq m0 (copy-mark start-mark :temporary)))

					  (when (after-end? m1 end-mark)
					    (setq m1 (copy-mark end-mark :temporary)))
				
					  (multiple-value-bind (dis-line-m0 offset-m0) (find-dis-line self m0)
					    (multiple-value-bind (dis-line-m1 offset-m1) (find-dis-line self m1)
					      ;; bug: this won't draw first line if first line is first line of window.
					      (when dis-line-m0
						(let ((ypos (hi::dis-line-position (car dis-line-m0))))

						  (flet ((draw (y x0 x1)
							   (scene-draw-filled-2d-rectangle-list
							    scene :cursor #xff770033
							    (list (+ (* x0 space-width) ox) (+ (* y 1.2 font-size) oy)
								  (+ (* x1 space-width) ox) (+ (* (1+ y) 1.2 font-size) oy))
							    0 (+ (send window base-z) (1+ (the buffer-view text elevation))))))

						    (when (eql dis-line-m0 dis-line-m1)
						      ;; it's just a partial line, draw and return
						      (draw ypos offset-m0 offset-m1)
						      (return-from draw-region (values)))

						    ;; draw the first line
						    (draw ypos offset-m0 (hi::dis-line-length (car dis-line-m0)))
					       
						    ;; draw the middle lines (if any)
						    (do ((dl (cdr dis-line-m0) (cdr dl)))
							((or (eql dl hi::the-sentinel)
							     (eql dl dis-line-m1)))
						      (draw (incf ypos) 0 (hi::dis-line-length (car dl))))

						    (draw (incf ypos) 0 offset-m1))
					  
						  ))))))))))))))))))))))))
  

(defmethod initialize-instance :around ((self text-editor-view-mixin) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (let ((application-frame (application-frame self)))
    (setf (krma::immediate-mode-work-function-1 (window-display application-frame))
	  (draw-cursor-closure-generator application-frame (the scene)))
    (setf (krma::immediate-mode-work-function-2 (window-display application-frame))
	  (draw-active-region-closure-generator application-frame (the scene))))
  self)

#+NIL
(setf (krma::immediate-mode-work-function-1 (clui::window-display (main-window clim:*default-frame-manager*))) nil)


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
  (declare (ignorable key))
  (elt
   (vector (canonicalize-color (send *style* text-color))
	   (canonicalize-color (send *style* lisp-comment-color))
	   #x000099ff
	   #x00999ff
	   
	   (canonicalize-color (send *style* lisp-strings-color))
	   (canonicalize-color (send *style* lisp-quote-color))
	   (canonicalize-color (send *style* lisp-keywords-color))
	   (canonicalize-color (send *style* lisp-comment-color))
	   

	   (canonicalize-color (send *style* lisp-defines-text-color))
	   (canonicalize-color (send *style* lisp-names-text-color))
	   (canonicalize-color (send *style* lisp-error-text-color))
	   #xcc00ccff
	   
	   #xffff
	   #xff00ff
	   #xff0000ff
	   #xffffffff
	   #xff)
   (mod key 17))
   )

(defmethod rm-erase-node ((text buffer-view-text))
  (delete-group (send text scene) (send text group) t))

(defmethod rm-draw-node ((text buffer-view-text))
  ;; todo: try to get rid of this
  (redraw-buffer (send text superior superior)))

(defun redraw-buffer (text-editor-view)
  (let ((buffer (window-buffer text-editor-view))
	(text (send text-editor-view buffer-view text)))
      (when buffer 
	(unless (string= "Random Typeout" (buffer-name buffer))
	  (let ((iconified? (send text iconified?)))
	    (let*((xp (send text superior x))
		  (yp (send text superior y))
		  (scene (send text scene))
		  (group (send text group))
		  (elevation (float (send text elevation)))
		  (font (send text-editor-view font))
		  (font-size (font-size font))
		  (font-data (font-data font))
		  (space-width (slot-value font-data '3b-bmfont-common::space-size))
		  (gm (send text global-matrix)))
	      (update-window-image text-editor-view)
	      (when group
		(krma:rm-dispatch-to-render-thread (scene draw-data)
		  (krma::delete-groups-1 draw-data (list group) nil)
		  (unless iconified?
		    (when (slot-boundp text-editor-view 'hi::the-sentinel)
		      (let ((last-line (car (hi::window-last-line text-editor-view)))
			    (last-line-seen? nil))
			(do ((dis-lines (hi::window-first-line text-editor-view) (cdr dis-lines)))
			    ((or last-line-seen?
				 (null dis-lines)
				 (eq dis-lines (slot-value text-editor-view 'hi::the-sentinel))))
			  (let* ((dl (car dis-lines))
				 (chars (hi::dis-line-chars dl))
				 (length (hi::dis-line-length dl))
				 (position (hi::dis-line-position dl))
				 (last 0)
				 (text-color-key 0))
			    (setq last-line-seen? (eq dl last-line))
			    (flet ((buffer-view-add-string (x y font text-color-key string start end)
				     (unless x (setq x 0))
				     (unless y (setq y 0))
				     (unless end (setq end length))
				     (unless start (setq start 0))
				     (when (< start end)
				       (let* ((color (lookup-text-color (or text-color-key 0)))
					      (xval (float (+ (* x space-width) xp)))
					      (yval (float (+ (* y 1.2 font-size) -2 yp))))
					 (krma::draw-data-add-text-primitive
					  draw-data group nil font color
					  xval yval (subseq string start end)
					  0 elevation)))))
			      (do ((change (hi::dis-line-font-changes dl) (hi::font-change-next change)))
				  ((null change)
				   (buffer-view-add-string last position font text-color-key chars last length))
				(let ((x (hi::font-change-x change)))
				  (buffer-view-add-string last position font text-color-key chars last x)
				  (setq last x
					text-color-key (hi::font-change-font change))))))))))
		  (krma::group-set-transform-1 draw-data group gm)
		  (values)))))))))



(defmethod handle-character-input ((window text-editor-view-mixin) char p)
  (let* ((buffer (hi::window-buffer window))
	 (point (hi::buffer-point buffer)))
    (dotimes (n p)
      (hi::insert-character buffer point char))
    (update-window-image window)
    (unless (displayed-p point window)
      (hi::bottom-window-on-mark window point))
    (redraw-buffer window)
    (values)))

(defun hi::view-buffer (view)
  (hi::window-buffer view))



(defmethod clim:handle-event :before ((self text-editor-view-mixin) (event clim:pointer-button-press-event))

  ;; this must be before the before method to homemade-window which does a set focus

  (when (eq clui::+pointer-left-button+ (clui::input-event-code event))
    (bt:with-recursive-lock-held ((hi::window-lock self))
      (let* ((pixel-x (clim:pointer-event-x event))
	     (pixel-y (clim:pointer-event-y event))
	     (buffer-view (the buffer-view))
	     (coord (untransform-global-coordinate
		     (vec3 pixel-x pixel-y 0) buffer-view)))
	(when (and (<= 0 (vx coord) (send buffer-view width))
		   (<= 0 (vy coord) (send buffer-view height)))
	  (let* ((font (hi::window-font-family self))
		 (x (round (vx coord)
			   (slot-value (slot-value font 'krma::data)
				       '3b-bmfont-common::space-size)))
		 (y (floor (vy coord) (* 1.2 (font-size font))))
		 (m (hi::cursorpos-to-mark x y self))
		 (buffer (window-buffer self)))			
	    (when m
	      (bt:with-recursive-lock-held ((hi::buffer-lock buffer))
		(move-mark (buffer-point buffer) m)
		(hi::update-window-image self))))))))

  (values))
  

(defmethod clim:handle-event ((self text-editor-view-mixin) (event clim:pointer-motion-event))

  (if (the maybe-drag)
	
      (let* ((start-event (the maybe-drag))
	     (start-x (clim:pointer-event-x start-event))
	     (start-y (clim:pointer-event-y start-event))
	     (monitor (window-monitor (application-frame self)))
	     (monitor-width-mm (if monitor (clui::monitor-width-mm monitor) 300))
	     (video-mode (when monitor (clui::get-monitor-video-mode monitor)))
	     (monitor-width-px (if video-mode (clui::video-mode-width video-mode) 1024))
	     (px-per-mm (/ monitor-width-px monitor-width-mm))
	     (curr-x (clim:pointer-event-x event))
	     (curr-y (clim:pointer-event-y event))
	     (delta-px (sqrt (+ (expt (- curr-x start-x) 2)
				(expt (- curr-y start-y) 2))))
	     (delta-mm (/ delta-px px-per-mm)))

	(when (> delta-mm 2)

	  (setf (the dragging-select-box?) (the maybe-drag))
	  (setf (the maybe-drag) nil)))
	
      (if (the dragging-select-box?)

	  (bt:with-recursive-lock-held ((hi::window-lock self))
	    (let* ((pixel-x (clim:pointer-event-x event))
		   (pixel-y (clim:pointer-event-y event))
		   (buffer-view (the buffer-view))
		   (coord (untransform-global-coordinate
			   (vec3 pixel-x pixel-y 0) buffer-view)))
	      (when (and (<= 0 (vx coord) (send buffer-view width))
			 (<= 0 (vy coord) (send buffer-view height)))
		(let* ((font (hi::window-font-family self))
		       (x (round (vx coord)
				 (slot-value (slot-value font 'krma::data)
					     '3b-bmfont-common::space-size)))
		       (y (floor (vy coord) (* 1.2 (font-size font))))
		       (m (hi::cursorpos-to-mark x y self))
		       (buffer (window-buffer self)))

		  (when m
		    (push-buffer-mark buffer m t))))))

	  (let ((last-event (climish::window-last-event (application-frame self))))
	    (when (and (typep last-event 'clui::pointer-button-press-event-mixin)
		       (eq clui::+pointer-left-button+ (clui::input-event-code last-event)))

	      (setf (the maybe-drag) last-event))))))

(defmethod clim:handle-event :after ((self text-editor-view-mixin)
				     (event clim:pointer-button-release-event))

  (when (the dragging-select-box?)
    
    (bt:with-recursive-lock-held ((hi::window-lock self))
      (let* ((pixel-x (clim:pointer-event-x event))
	     (pixel-y (clim:pointer-event-y event))
	     (buffer-view (the buffer-view))
	     (coord (untransform-global-coordinate
		     (vec3 pixel-x pixel-y 0) buffer-view)))
	(when (and (<= 0 (vx coord) (send buffer-view width))
		   (<= 0 (vy coord) (send buffer-view height)))

	  (let* ((font (hi::window-font-family self))
		 (x (round (vx coord)
			   (slot-value (slot-value font 'krma::data)
				       '3b-bmfont-common::space-size)))
		 (y (floor (vy coord) (* 1.2 (font-size font))))
		 (m (hi::cursorpos-to-mark x y self))
		 (buffer (hi::window-buffer self)))
	    (when m
	      (ring-push (current-region buffer nil) hemlock::*kill-ring*)
	      (move-mark (buffer-point buffer) m)
	      (hi::update-window-image self)))))))
  
  (setf (the dragging-select-box?) nil)
  (setf (the maybe-drag) nil)

  (values))
