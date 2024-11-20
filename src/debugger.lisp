(in-package :climish)

(defobject debugger-window (text-editor-window)
  :inputs
  ((close-button? nil))
  :slots
  ((command-loop-thread :accessor debugger-window-command-loop-thread)))

(defmethod make-instance :after ((self debugger-window) &rest initargs)
  (declare (ignore initargs))
  
  (values))

(defmethod destroy-window :before ((self debugger-window))
  (bt:destroy-thread (debugger-window-command-loop-thread self))
  (values))

(defun gui-debug-condition (condition hook-value)
  (let* ((buffer (make-instance 'hi::buffer :name "debugger")))
    
    (setf (buffer-undo-p buffer) nil)
    (setf (buffer-minor-mode buffer *echo-area-buffer* :save) nil)
    (setf (buffer-minor-mode buffer *echo-area-buffer*
			     :interactive-character-stream) t)
    (setf (buffer-major-mode buffer *echo-area-buffer*) :lisp)

    (let ((window (make-instance 'debugger-window :buffer buffer)))
      ;; focus window has to come before debug-command-loop start
      ;; or else the focus will be on the previous window when
      ;; the first event comes
      (focus-window window)
      
      (setf (debugger-window-command-loop-thread window)
	    (let* ((main-window (krma::main-window clim::*default-frame-manager*)))
	      (bt:make-thread #'(lambda () (start-command-loop main-window))
			      :name "debug command loop")))
      (unwind-protect (my-debugger window buffer condition hook-value)
	(destroy-window window)))))

(in-package :hemlock)

(defun climish::my-debugger (window buffer condition hook-value)
  (declare (ignorable hook-value))
  (let* ((stream (hemlock::variable-value 'hemlock::buffer-stream :buffer buffer))
	 (*terminal-io* stream) ;; set up our streams
	 (*standard-output* stream)
	 (*standard-input* stream)
	 (*error-output* stream)
	 (*trace-output* #+sbcl sb-sys::*stdout* #-sbcl *trace-output*)
	 (*debug-io* stream)
	 (*query-io* stream))

    
    
    (let ((.form. nil))
	       (in-lisp buffer
		 (let ((hi::*debug-on-error* t)
		       (#+(or CCL ALLEGRO) *debugger-hook*
			  #+sbcl sb-ext:*invoke-debugger-hook*
			  #'(lambda (condition hook-value)
			      (climish::my-debugger
			       window
			       buffer
			       condition hook-value))))
		   (progn
		     (princ condition *debug-io*)
		     #+sbcl
		     (sb-debug:print-backtrace)
		     (terpri *debug-io*)
		     (terpri *debug-io*)
		     (when (typep climish::condition 'simple-condition)
		       (apply #'format *debug-io*
			      (simple-condition-format-control climish::condition)
			      (simple-condition-format-arguments climish::condition))
		       (fresh-line *debug-io*))
		     (format *debug-io* "condition of type: ~A" (type-of climish::condition))
		     (terpri *debug-io*)
		     (terpri *debug-io*)
		     (princ "RESTARTS:" *debug-io*)
		     (fresh-line *debug-io*)
		     (let ((restarts (butlast (compute-restarts climish::condition))))
		       (loop for restart in restarts
			     for i from 0
			     do (princ i *debug-io*)
				(princ " " *debug-io*)
				(princ (restart-name restart) *debug-io*)
				(fresh-line *debug-io*))
		       (loop
			 (princ "-> " *debug-io*)
			 (finish-output *debug-io*)
			 (restart-case
			     (let ()
			       (tagbody
				read
				  (setq .form. (read stream nil lispbuf-eof))
				  (when (eq .form. lispbuf-eof)
				    (go read)))

			       (when (and (integerp .form.)
					  (< .form. (length restarts)))
				 (invoke-restart (nth .form. restarts)))

			       (setq +++ ++ ++ + + - - .form.)
			       (unwind-protect
				    (let ((this-eval
					    (multiple-value-list
					     (eval .form.))))
				      (fresh-line)
				      (dolist (x this-eval) (prin1 x) (terpri))
				      (setq /// // // / / this-eval)
				      (setq *** ** ** * * (car this-eval)))
				 (fresh-line)))
			   (abort ()
			     :report (lambda (stream)
				       (format stream "Return to the top level."))
			     (values nil t)))))))))))
	 

    

