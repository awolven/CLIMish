(in-package :hi)

(defcommand "Next Line" (window p)
    "Moves the point to the next line.
   With prefix argument, moves the point that many lines down (or up if
   the prefix is negative)."
    "Moves the down p lines."
  (when (typep window 'krma.gui::text-editor-window)
    (with-buffer-globals ((window-buffer window))
      (let* ((target (hemlock::.set-target-column. .point.)))
	(unless (line-offset .point. (or p 1))
	  (cond ((not p)
		 (when (same-line-p .point. (buffer-end-mark .buffer.))
		   (line-end .point.))
		 ;;
		 ;; The insert-character was probably intentional back when
		 ;; Hemlock was written, but it inserts bogus newlines at
		 ;; the end of the file, and that's a bad idea.
		 ;;
		 ;; (insert-character point #\newline)
		 (editor-error "No next line."))
		((minusp p)
		 (buffer-start .point. .buffer.)
		 (editor-error "No previous line."))
		(t
		 (buffer-end .point. .buffer.)
		 (when p (editor-error "No next line.")))))
	(unless (.move-to-column. .point. target) (line-end .point.))
	(setf (last-command-type) :line-motion))
      (krma.gui::redraw-buffer window))))

(defcommand "Previous Line" (window p)
    "Moves the point to the previous line.
  With prefix argument, moves the point that many lines up (or down if
  the prefix is negative)."
    "Moves the point up p lines."
  (next-line-command window (- (or p 1))))

(defcommand "Insert Newline" (window p)
    ""
    ""
  (krma.gui::handle-character-input window #\Newline (or p 1)))

(defcommand "Beginning of Line" (window p)
    "Moves the point to the beginning of the current line.
  With prefix argument, moves the point to the beginning of the prefix'th
  next line."
    "Moves the point down p lines and then to the beginning of the line."
  (with-buffer-globals ((window-buffer window))
    (unless (line-offset .point. (if p p 0)) (editor-error "No such line."))
    (line-start .point.)
    (setf (last-command-type) nil)
    (krma.gui::redraw-buffer window)))


(defcommand "End of Line" (window p)
    "Moves the point to the end of the current line.
  With prefix argument, moves the point to the end of the prefix'th next line."
    "Moves the point down p lines and then to the end of the line."
  (with-buffer-globals ((window-buffer window))
    (unless (line-offset .point. (if p p 0)) (editor-error "No such line."))
    (line-end .point.)
    (setf (last-command-type) nil)
    (krma.gui::redraw-buffer window)))

(defcommand "Forward Character" (view p)
    "Move the point forward one character.
   With prefix argument move that many characters, with negative argument
   go backwards."
    "Move the point of the current buffer forward p characters."
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
    (setf (last-command-type) nil)
    (update-window-image view)))

(defcommand "Backward Character" (view p)
    "Move the point backward one character.
  With prefix argument move that many characters backward."
  "Move the point p characters backward."
  (forward-character-command view (- (or p 1))))

(defcommand "Delete Next Character" (view p)
    "Deletes the character to the right of the point.
   With prefix argument, delete that many characters to the right
  (or left if prefix is negative)."
    "Deletes p characters to the right of the point."
  (cond ((with-buffer-globals ((view-buffer view))
	   (hemlock::.kill-characters. .point. (or p 1))))
	((and p (minusp p))
	 (editor-error "Not enough previous characters."))
	(t (editor-error "Not enough next characters.")))
  (krma.gui::redraw-buffer view))

(defcommand "Delete Previous Character" (view p)
    "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."
    "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."
  (delete-next-character-command view (- (or p 1))))

(defcommand "Transpose Characters" (view p)
    "Exchanges the characters on either side of the point and moves forward
  With prefix argument, does this that many times.  A negative prefix
  argument causes the point to be moved backwards instead of forwards."
    "Exchanges the characters on either side of the point and moves forward."
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
    (setf (last-command-type) nil)
  (krma.gui::redraw-buffer view))

(defcommand "Forward Word" (view p)
    "Moves forward one word.
  With prefix argument, moves the point forward over that many words."
    "Moves the point forward p words."
  (with-buffer-globals ((view-buffer view))
    (cond ((word-offset .point. (or p 1)))
          ((and p (minusp p))
           (buffer-start .point. .buffer.)
           (editor-error "No previous word."))
          (t
           (buffer-end .point. .buffer.)
           (editor-error "No next word."))))
  (setf (last-command-type) nil)
  (krma.gui::redraw-buffer view))

(defcommand "Backward Word" (view p)
    "Moves forward backward word.
  With prefix argument, moves the point back over that many words."
    "Moves the point backward p words."
  (hi::forward-word-command view (- (or p 1))))

(defun find-or-create-text-editor-window-for-buffer (buffer)
  (or (first (buffer-windows buffer))
      (make-instance 'krma.gui::text-editor-window :buffer buffer)))

(defcommand "Find File" (view p &optional pathname)
  "Visit a file in its own buffer.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it."
  "Make a buffer containing the file Pathname current, creating a buffer
   if necessary.  The buffer is returned."
  (declare (ignore p))
  (let* ((pn (or pathname
                 (prompt-for-file
		  view
                  :prompt "Find File: "
                  :must-exist nil
                  :help "Name of file to read into its own buffer."
                  :default (hemlock-ext:default-directory))))
         (buffer (find-file-buffer view pn))
	 (window (find-or-create-text-editor-window-for-buffer buffer)))
    ;;(change-to-buffer buffer view)
    (krma.gui::focus-window window)
    buffer))

(defcommand "Mousewheel Scroll" (window p)
    "Use mousewheel to scroll."
    "Use mousewheel to scroll."
  (declare (ignore p))
  (let* ((last-event (krma.gui::window-last-event (clui::window-parent (adhoc:send window adhoc::root))))
	 (offset (clui::pointer-wheel-event-yoffset last-event)))
    (with-buffer-globals ((window-buffer window))
      (hi::.scroll-window. window (- (* 2 (round offset)))))
    (krma.gui::redraw-buffer window)))

(defcommand "Beginning of Buffer" (window p)
  "Moves the point to the beginning of the current buffer."
    "Moves the point to the beginning of the current buffer."
  (declare (ignore p))
  (let* ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (buffer-start .point. .buffer.))
    (maybe-recenter-window-on-mark window (buffer-point buffer))
    (krma.gui::redraw-buffer window)))

(defcommand "End of Buffer" (window p)
  "Moves the point to the end of the current buffer."
    "Moves the point to the end of the current buffer."
  (declare (ignore p))
  (let* ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (buffer-end .point. .buffer.))
    (maybe-rebottom-window-on-mark window (buffer-point buffer))
    (krma.gui::redraw-buffer window)))

(defcommand "Scroll Window Down" (window p)
  "Move down one screenfull.
  With prefix argument scroll down that many lines."
  "If P is NIL then scroll Window, which defaults to the current
  window, down one screenfull.  If P is supplied then scroll that
  many lines."
  (let ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (if p
	  (hi::.scroll-window. window p)
	  (let ((height (window-height window))
		(overlap (value hemlock::scroll-overlap)))
	    (hi::.scroll-window. window (if (<= height overlap)
					    height (- height overlap))))))
    (krma.gui::redraw-buffer window)))

(defcommand "Scroll Window Up" (window p)
    "Move up one screenfull.
  With prefix argument scroll up that many lines."
    "If P is NIL then scroll Window, which defaults to the current
  window, up one screenfull.  If P is supplied then scroll that
  many lines."
  (let ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (if p
	  (hi::.scroll-window. window (- p))
	  (let ((height (- (window-height window)))
		(overlap (- (value hemlock::scroll-overlap))))
	    (hi::.scroll-window. window (if (>= height overlap)
					    height (- height overlap))))))
    (krma.gui::redraw-buffer window)))


    
