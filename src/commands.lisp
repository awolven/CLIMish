(in-package :hemlock)

(defhcommand editor-error-test-command (window p)
  (:documentation "")
  (:pretty-name "Editor Error Test"))

(defmethod editor-error-test-command (window p)
  window p
  (editor-error "FOO!!"))

(defhcommand debugger-test-command (window p)
  (:documentation "")
  (:pretty-name "Debugger Test"))

(defmethod debugger-test-command (window p)
  window p
  (break))

(defhcommand next-line-command (window p)
  (:pretty-name  "Next Line")
  (:documentation 
   "Moves the point to the next line.
   With prefix argument, moves the point that many lines down (or up if
   the prefix is negative)."))

(defmethod next-line-command (window p)
  (when (typep window 'climish::text-editor-window)
    (let* ((buffer (window-buffer window))
	   (point (buffer-point buffer)))
      (let* ((target (set-target-column buffer point)))
	(unless (line-offset buffer point (or p 1))
	  (cond ((not p)
		 (when (same-line-p point (buffer-end-mark buffer))
		   (line-end buffer point))
		 ;;
		 ;; The insert-character was probably intentional back when
		 ;; Hemlock was written, but it inserts bogus newlines at
		 ;; the end of the file, and that's a bad idea.
		 ;;
		 ;; (insert-character point #\newline)
		 (editor-error "No next line."))
		((minusp p)
		 (buffer-start point buffer)
		 (editor-error "No previous line."))
		(t
		 (buffer-end point buffer)
		 (when p (editor-error "No next line.")))))
	(unless (move-to-column buffer point target) (line-end buffer point))
	(setf (last-command-type) :line-motion))
      (if (not (displayed-p point window))
	  (progn
	    (hi::center-window-on-mark window point)
	    (climish:redraw-buffer window))
	  (update-window-image window)))))


(defhcommand previous-line-command (window p)
  (:pretty-name "Previous Line")
  (:documentation 
   "Moves the point to the previous line.
  With prefix argument, moves the point that many lines up (or down if
  the prefix is negative)."))

(defmethod previous-line-command (window p)
  (next-line-command window (- (or p 1))))

(climish::define-everywhere-command enter-command (window buffer major-mode p)
  (:pretty-name "Enter")
  (:documentation "")
  (:help-string "")
  (:preparation (let* ((window (or (climish::focused-window clim:application-frame)
					    *echo-area-window*))
		       (buffer (and (typep window 'climish::text-editor-view-mixin)
				    (window-buffer window)))
		       (major-mode (and buffer (buffer-major-mode buffer)))
		       (universal-argument
			 (climish::universal-argument clim:application-frame)))
		  (list window buffer major-mode universal-argument))))

(defmethod enter-command (window buffer major-mode p)
  (if (buffer-active-region-p buffer)
      (ring-push (hi::current-region buffer) hemlock::*kill-ring*)
      (if (buffer-minor-mode buffer :interactive-character-stream)
	  (enter-command window buffer :interactive-character-stream nil)
	  (climish::handle-character-input window #\Newline (or p 1)))))

(defmethod enter-command (window buffer (major-mode (eql :lisp)) p)
  ;; indent-new-line
  (let* ((point (buffer-point buffer)))
    (let ((prefix (hi::value buffer fill-prefix)))
      (delete-horizontal-space buffer point)
      (enter-command window buffer nil p)
      (if prefix
          (insert-string buffer point prefix)
          (funcall (hi::value buffer indent-function) buffer point)))
    (climish::redraw-buffer window)))

(defmethod enter-command ((window climish::echo-area-window) buffer major-mode p)
  (declare (ignore p))
  (climish::echo-area-enter-command (clim:application-frame *echo-area-window*)))

(defmethod enter-command ((window climish::application-frame-mixin) buffer major-mode p)
  (declare (ignore buffer major-mode p))
  ;; do nothing currently
  (values))

(defmethod enter-command (window buffer (major-mode (eql :interactive-character-stream)) p)
  (declare (ignore p))
  (let* ((stream (value buffer buffer-stream))
	 (point (buffer-point buffer)))
    (insert-character buffer point #\Newline)
    (with-buffer-globals (buffer)
    (let* ((mark (hemlock-character-io-stream-mark stream))
	   (input-region (region mark (buffer-end-mark buffer)))
	   (string (region-to-string buffer input-region)))

      (move-mark
       (hemlock-character-io-stream-mark stream)
       (buffer-end-mark buffer))
      
      (with-slots (input-line input-line-position) stream
	(setf input-line-position 0
	      input-line (copy-seq string))))
    (buffer-end point buffer))
    (f-o stream)))

(defmethod enter-command (window buffer (major-mode (eql :dired)) p)
  (declare (ignore buffer))
  (dired-edit-file-command window p))

(defhcommand beginning-of-line-command (window p)
  (:pretty-name "Beginning of Line")
  (:documentation "Moves the point to the beginning of the current line.
  With prefix argument, moves the point to the beginning of the prefix'th
  next line."))

(defmethod beginning-of-line-command (window p)
  (let* ((buffer (window-buffer window))
	 (point (buffer-point buffer)))
    (unless (line-offset buffer point (if p p 0)) (editor-error "No such line."))
    (line-start point)
    (setf (last-command-type) nil)
    (climish:redraw-buffer window)))


(defhcommand end-of-line-command (window p)
  (:pretty-name "End of Line")
  (:documentation "Moves the point to the end of the current line.
  With prefix argument, moves the point to the end of the prefix'th next line."))

(defmethod end-of-line-command (window p)
  (let* ((buffer (window-buffer window))
	 (point (buffer-point buffer)))
    (unless (line-offset buffer point (if p p 0)) (editor-error "No such line."))
    (line-end buffer point)
    (setf (last-command-type) nil)
    (climish:redraw-buffer window)))

(defhcommand forward-character-command (view p)
    (:pretty-name "Forward Character")
    (:documentation "Move the point forward one character.
   With prefix argument move that many characters, with negative argument
   go backwards.")
  (:help-string "Move the point of the current buffer forward p characters."))

(defmethod forward-character-command (view p)
  (let ((p (or p 1)))
    (let* ((buffer (window-buffer view))
	   (point (buffer-point buffer)))
      (cond ((character-offset buffer point p))
	    ((= p 1)
	     (editor-error "No next character."))
	    ((= p -1)
	     (editor-error "No previous character."))
	    (t (if (plusp p)
		   (buffer-end point buffer)
		   (buffer-start point buffer))
	       (editor-error "Not enough characters."))))
    (setf (last-command-type) nil)
    (update-window-image view)))

(defhcommand backward-character-command (view p)
    (:pretty-name "Backward Character")
    (:documentation "Move the point backward one character.
  With prefix argument move that many characters backward.")
  (:help-string "Move the point p characters backward."))

(defmethod backward-character-command (view p)
  (forward-character-command view (- (or p 1))))

(defhcommand delete-next-character-command (view p)
    (:pretty-name "Delete Next Character")
    (:documentation "Deletes the character to the right of the point.
   With prefix argument, delete that many characters to the right
  (or left if prefix is negative).")
  (:help-string "Deletes p characters to the right of the point."))

(defmethod delete-next-character-command (view p)
  (let* ((buffer (window-buffer view))
	 (point (buffer-point buffer)))
    (cond ((kill-characters buffer point (or p 1)))
	  ((and p (minusp p))
	   (editor-error "Not enough previous characters."))
	  (t (editor-error "Not enough next characters.")))
    (climish:redraw-buffer view)))

(defhcommand delete-previous-character-command (view p)
    (:pretty-name "Delete Previous Character")
    (:documentation "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring.")
  (:help-string "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."))

(defmethod delete-previous-character-command (view p)
  (delete-next-character-command view (- (or p 1))))

(defhcommand transpose-characters-command (view p)
    (:pretty-name "Transpose Characters")
    (:documentation "Exchanges the characters on either side of the point and moves forward
  With prefix argument, does this that many times.  A negative prefix
  argument causes the point to be moved backwards instead of forwards.")
  (:help-string "Exchanges the characters on either side of the point and moves forward."))

(defmethod transpose-characters-command (view p)
  (let ((buffer (hi::view-buffer view)))
    (when buffer
      (let ((arg (or p 1))
	    (point (buffer-point buffer)))
	(dotimes (i (abs arg))
	  (when (or (minusp arg) (end-line-p buffer point))
	    (mark-before buffer point))
	  (let ((prev nil)
		(next nil))
	    (setq prev (previous-character buffer point))
	    (setq next (next-character buffer point))
	    (cond ((not prev) (editor-error "No previous character."))
		  ((not next) (editor-error "No next character."))
		  (t
		   (setf (previous-character buffer point) next)
		   (setf (next-character buffer point) prev)))
	      (when (plusp arg) (mark-after buffer point)))))))
  (setf (last-command-type) nil)
  (climish:redraw-buffer view))

(defhcommand forward-word-command (view p)
    (:pretty-name "Forward Word")
    (:documentation "Moves forward one word.
  With prefix argument, moves the point forward over that many words.")
  (:help-string "Moves the point forward p words."))

(defmethod forward-word-command (view p)
  (let* ((buffer (hi::view-buffer view))
	 (point (buffer-point buffer)))
    (cond ((word-offset buffer point (or p 1)))
          ((and p (minusp p))
           (buffer-start point buffer)
           (editor-error "No previous word."))
          (t
           (buffer-end point buffer)
           (editor-error "No next word.")))
    (setf (last-command-type) nil)
    (climish::redraw-buffer view)))

(defhcommand backward-word-command (view p)
    (:pretty-name "Backward Word")
    (:documentation "Moves forward backward word.
  With prefix argument, moves the point back over that many words.")
  (:help-string "Moves the point backward p words."))

(defmethod backward-word-command (view p)
  (forward-word-command view (- (or p 1))))


(defcommand find-file-command (view p &optional pathname)
  (:pretty-name "Find File")
  (:documentation "Visit a file in its own buffer.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it.")
  (:help-string "Make a buffer containing the file Pathname current, creating a buffer
   if necessary.  The buffer is returned.")
  (:applicability (climish::hemlock-applicability clim:application-frame))
  (:preparation (climish::hemlock-arguments clim:application-frame)))

(defmethod find-file-command (view p &optional pathname)
  (declare (ignore p))
  (let* ((pn (or pathname
                 (clim:accept 'file
			      :current-window view
			      :prompt "Find File: "
			      :must-exist nil
			      :help "Name of file to read into its own buffer."
			      :default (hemlock-ext:default-directory))))
         (buffer (find-file-buffer view pn)))
    (climish::find-or-create-text-editor-window-for-buffer buffer)
    buffer))

(climish::define-everywhere-command mousewheel-scroll-command (window p)
  (:pretty-name "Mousewheel Scroll")
  (:documentation "Use mousewheel to scroll.")
  (:help-string "Use mousewheel to scroll.")
  (:applicability (let* ((focused (climish:focused-window clim:application-frame)))
		    (and focused (typep focused 'climish::text-editor-view-mixin)
			 (not (eql focused *echo-area-window*)))))
  (:preparation (let* ((focused (climish:focused-window clim:application-frame)))
			   (list focused (climish::universal-argument clim:application-frame)))))
			   

(defmethod mousewheel-scroll-command (window p)
  (declare (ignore p))
  (let ((last-event (climish:window-last-event (clim:application-frame window))))
    (let* ((offset (clui::pointer-wheel-event-yoffset last-event)))
      (scroll-window window (- (* 2 (round offset))))
      (climish:redraw-buffer window))))

(defhcommand cycle-windows-command (window p)
    (:pretty-name "Cycle Windows")
    (:documentation "Focus next window")
  (:help-string "Focus next window"))

(defmethod cycle-windows-command (window p)
  (declare (ignore p))
  (let ((next (climish::next window)))
    (when (clui::window-iconified? next)
      (setf (clui::window-iconified? next) nil))
    (climish::focus-window next)))

(defhcommand reverse-cycle-windows-command (window p)
    (:pretty-name "Reverse Cycle Windows")
  (:documentation "Focus previous window")
  (:help-string "Focus previous window"))

(defmethod reverse-cycle-windows-command (window p)
  (declare (ignore p))
  (let ((prev (climish::prev window)))
    (when (clui::window-iconified? prev)
      (setf (clui::window-iconified? prev) nil))
    (climish::focus-window prev)))

(defhcommand iconify-window-command (window p)
  (:pretty-name "Iconify Window")
  (:help-string "Iconify Window")
  (:documentation "Iconify Window and focus on next window."))

(defmethod iconify-window-command (window p)
  (declare (ignore p))
  (let ((next (climish::next window)))
    (climish::focus-window next)
    (setf (clui::window-iconified? window) t)))

(defhcommand beginning-of-buffer-command (window p)
  (:pretty-name "Beginning of Buffer")
  (:help-string "Moves the point to the beginning of the current buffer.")
  (:documentation "Moves the point to the beginning of the current buffer."))

(defmethod beginning-of-buffer-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (buffer-start .point. .buffer.))
    (hi::maybe-recenter-window-on-mark window (buffer-point buffer))
    (climish::redraw-buffer window)))

(defhcommand end-of-buffer-command (window p)
  (:pretty-name "End of Buffer")
  (:documentation "Moves the point to the end of the current buffer.")
  (:help-string "Moves the point to the end of the current buffer."))

(defmethod end-of-buffer-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (buffer-end .point. .buffer.))
    (hi::maybe-rebottom-window-on-mark window (buffer-point buffer))
    (climish::redraw-buffer window)))

(defhcommand scroll-window-down-command (window p)
  (:pretty-name "Scroll Window Down")
  (:help-string
   "Move down one screenfull.
  With prefix argument scroll down that many lines.")
  (:documentation
   "If P is NIL then scroll Window, which defaults to the current
  window, down one screenfull.  If P is supplied then scroll that
  many lines."))

(defmethod scroll-window-down-command (window p)
  (let ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (if p
	  (hi::.scroll-window. window p)
	  (let ((height (window-height window))
		(overlap (value buffer hemlock::scroll-overlap)))
	    (hi::.scroll-window. window (if (<= height overlap)
					    height (- height overlap))))))
    (climish::redraw-buffer window)))

(defhcommand scroll-window-up-command (window p)
  (:pretty-name "Scroll Window Up")
  (:help-string "Move up one screenfull.
  With prefix argument scroll up that many lines.")
  (:documentation
   "If P is NIL then scroll Window, which defaults to the current
  window, up one screenfull.  If P is supplied then scroll that
  many lines."))

(defmethod scroll-window-up-command (window p)
  (let ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (if p
	  (hi::.scroll-window. window (- p))
	  (let ((height (- (window-height window)))
		(overlap (- (value buffer hemlock::scroll-overlap))))
	    (hi::.scroll-window. window (if (>= height overlap)
					    height (- height overlap))))))
    (climish::redraw-buffer window)))

(climish::define-everywhere-command options (frame applicable-commands)
  (:pretty-name "Options")
  (:help-string "Show the context menu.")
  (:documentation "Show the context menu.")
  (:applicability (not (typep (climish:focused-window clim:application-frame)
			      'climish::context-menu-mixin)))
  (:preparation (list climish::application-frame
		      (climish::applicable-commands clim:application-frame))))

(defmethod options (frame applicable-commands)
  (let* ((menu (make-instance 'climish::context-menu
			      'climish::applicable-commands
			      applicable-commands))
	 (event (climish::window-last-event frame)))
    (climish::set-window-position menu
				  (- (clim:pointer-event-x event) 70)
				  (- (clim:pointer-event-y event) 70))
    (let ((choice))
      (unwind-protect
	   (progn
	     (setq choice (clim:accept 'climish::not-everywhere-command
				       :current-window (climish:focused-window frame)
				       :view menu))
	     (climish::maybe-throw-presentation choice (climish::input-context frame) nil nil)
	     (climish::invoke-command choice frame))
	(climish::destroy-window menu)))))



    
    
