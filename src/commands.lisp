(in-package :hi)

(defcommand "Next Line" (window p)
    ""
    ""
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
    ""
    ""
  (next-line-command window (- (or p 1))))

(defcommand "Insert Newline" (window p)
    ""
    ""
  (krma.gui::handle-character-input window #\Newline (or p 1)))

(defcommand "Beginning of Line" (window p)
    ""
    ""
  (with-buffer-globals ((window-buffer window))
    (unless (line-offset .point. (if p p 0)) (editor-error "No such line."))
    (line-start .point.)
    (setf (last-command-type) nil)
    (krma.gui::redraw-buffer window)))


(defcommand "End of Line" (window p)
    ""
    ""
  (with-buffer-globals ((window-buffer window))
    (unless (line-offset .point. (if p p 0)) (editor-error "No such line."))
    (line-end .point.)
    (setf (last-command-type) nil)
    (krma.gui::redraw-buffer window)))

(defcommand "Forward Character" (view p)
    ""
    ""
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
    ""
    ""
  (forward-character-command view (- (or p 1))))

(defcommand "Delete Next Character" (view p)
    ""
    ""
  (cond ((with-buffer-globals ((view-buffer view))
	   (hemlock::.kill-characters. .point. (or p 1))))
	((and p (minusp p))
	 (editor-error "Not enough previous characters."))
	(t (editor-error "Not enough next characters.")))
  (krma.gui::redraw-buffer view))

(defcommand "Delete Previous Character" (view p)
    ""
    ""
  (delete-next-character-command view (- (or p 1))))

(defcommand "Transpose Characters" (view p)
    ""
    ""
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
    ""
    ""
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
    ""
    ""
  (krma.gui::command-forward-word view (- (or p 1))))

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
