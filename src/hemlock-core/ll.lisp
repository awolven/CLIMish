(in-package :hi)

;; Lisp Listener and hemlock.vktk still suffer from a #'close-line bug in the rendering thread

;; todo: make backspace do the right thing
;; done: enabled minimal functionality by making input line-oriented
;; todo: now need a complete-lisp-form oriented mode
;; todo: once Line-Oriented Character Stream Mode is completely working, get shell process working.
;; maybe rename LO C S M to Interactive Character Stream Mode

;; todo: turn off undo in Lisp Listener.

;; get C-C C-C interrupt lisp listener process working during output

(in-package :hemlock)

(defun invoke-a-restart ()
  (let ((restarts (butlast (compute-restarts))))

     (do ((i 0 (+ i 1)) (r restarts (cdr r))) ((null r))
       (format t "~&~D: ~A~%" i (car r)))
     (let ((n nil) (k (length restarts)))
       (loop (when (and (typep n 'integer) (>= n 0) (< n k))
               (return t))
             (format t "~&Option: ")
             (setq n (read))
             (fresh-line))
       (invoke-restart-interactively (nth n restarts)))))

(defun repl (stream) ;; this function should only depend on stream being a character-io-stream
  ;; so that it is general to any character-io-stream, regardless of buffers/marks etc.
  (let* ((*terminal-io* stream) ;; set up our streams
         (*standard-output* stream)
         (*standard-input* stream)
         (*error-output* stream)
         (*trace-output* stream)
         (*debug-io* stream)
         (*query-io* stream)
         (.form. nil))
    (in-lisp (hemlock::hemlock-character-io-stream-buffer stream)
      (let ((hi::*debug-on-error* t)
            #+sbcl (sb-ext:*invoke-debugger-hook* nil)
            (*debugger-hook* nil))
        (with-nonbroken-debugger
	    (progn (eval (read-from-string "(in-package :krma)"))
	      (loop
		(princ (or (first (package-nicknames *package*)) (package-name *package*)) stream)
		(princ "> " stream)
		(finish-output)
		(restart-case
                    (let ()
                      (tagbody
                       read
			 (setq .form. (read stream nil lispbuf-eof))
			 (when (eq .form. lispbuf-eof)
                           (go read)))

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
                    (values nil t))))))))))




(defhcommand lisp-listener-command (window p)
  (:pretty-name "Lisp Listener")
  (:documentation "Start an REPL in a thread")
  (:help-string "Start an REPL in a thread"))

(defmethod lisp-listener-command (window p)
  (declare (ignore p))
  (let ((existing (find-window-named window "Lisp Listener")))
    (or (and existing (progn (climish::focus-window existing) existing))
	(let ((buffer (make-instance 'buffer :name "Lisp Listener"))
	      (current-buffer (hi::window-buffer window)))
	  (setf (buffer-undo-p buffer) nil)
	  (setf (buffer-minor-mode buffer current-buffer :save) nil)
	  (setf (buffer-minor-mode buffer current-buffer :interactive-character-stream) t)
	  (setf (buffer-minor-mode buffer current-buffer :lisp-listener) t)
	  (setf (buffer-major-mode buffer current-buffer) :lisp)
	  (climish::find-or-create-text-editor-window-for-buffer buffer)
	  (let* ((stream (variable-value 'hemlock::buffer-stream :buffer buffer))
		 (thread (bt:make-thread #'(lambda ()
					     (repl stream))
					 :name "Lisp Listener")))
	    (defhvar "Lisp Listener Thread" "Lisp Listener Thread" :buffer buffer
              :value thread)
	    (add-hook hemlock::delete-buffer-hook #'(lambda (buffer)
						      (when (buffer-minor-mode buffer :lisp-listener)
							(bt:destroy-thread (hemlock::variable-value 'hemlock::lisp-listener-thread :buffer buffer)))))
	    
	    (values))))))

(defmethod find-window-named ((w climish::application-frame-mixin) name)
  (find-window-named-1 (climish::focused-window w) name))

(defmethod find-window-named ((w climish::homemade-window-mixin) name)
  (find-window-named-1 w name))

(defun find-window-named-1 (w name)
  (flet ((is-named? (win)
	   (when (eq name (adhoc:send win climish::title))
	     (return-from find-window-named-1 win))))
    
    (is-named? w)
	   
    (do ((next (climish::next w) (climish::next next)))
	((eq next w))
      (is-named? next))))

(defhcommand interrupt-lisp-command (window p)
  (:pretty-name "Interrupt Lisp")
  (:documentation "Interrupt a running Lisp Listener Thread")
  (:help-string "Interrupt a running Lisp Listener Thread"))

(defmethod interrupt-lisp-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (thread (variable-value 'hemlock::lisp-listener-thread :buffer buffer)))
    (when thread
      (beep)
      (when buffer
	#+sbcl
        (sb-thread:release-mutex (buffer-lock buffer)))
      (bt:interrupt-thread thread #'break))))


