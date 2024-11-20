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

(defvar *last-lisp-listener-history-index* nil)
(defvar *lisp-listener-history* (make-array 100 :fill-pointer 0 :adjustable t))

(defun add-to-lisp-listener-history (string)
  (let ((length (length *lisp-listener-history*)))
    (loop for i from (1- length) downto 0
	  do (let ((item (aref *lisp-listener-history* i)))
	       (when (string= item string)
		 (unless (= 1 (length *lisp-listener-history*))
		   ;; replace entry at end
		   (loop for j from i below (1- (length *lisp-listener-history*))
			 do (setf (aref *lisp-listener-history* j)
				  (aref *lisp-listener-history* (1+ j)))
			 finally (setf (aref *lisp-listener-history* (1- (length *lisp-listener-history*)))
				       item)))
		 (return-from add-to-lisp-listener-history t)))
	  finally (vector-push-extend string *lisp-listener-history*)
		  (return t))))

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

(defun drop-prompt-mark (buffer)
  (setf (variable-value 'hemlock::prompt-mark :buffer buffer)
	(copy-mark (buffer-point buffer) :right-inserting))
  (setq *last-lisp-listener-history-index* nil)
  (values))
  

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
	    (progn (eval (read-from-string (format nil "(in-package ~S)"
						   (climish::lisp-listener-package
						    (clim:application-frame *echo-area-window*)))))
	      (loop
		(princ (or (first (package-nicknames *package*)) (package-name *package*)) stream)
		(princ "> " stream)
		(finish-output)
		(let ((buffer (hemlock::hemlock-character-io-stream-buffer stream)))
		  (drop-prompt-mark buffer))
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



(defcommand yank-from-lisp-listener-history (window p)
  (:pretty-name "Yank From Lisp Listener History")
  (:documentation "Yank From Lisp Listener History")
  (:help-string "Yank From Lisp Listener History")
  (:applicability (let ((focused (climish::focused-window climish::application-frame)))
		    (and focused
			 (let ((buffer (window-buffer focused)))
			   (and buffer
				(buffer-minor-mode buffer :lisp-listener))))))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defcommand yank-next-from-lisp-listener-history (window p)
  (:pretty-name "Yank Next From Lisp Listener History")
  (:documentation "Yank Next From Lisp Listener History")
  (:help-string "Yank Next From Lisp Listener History")
  (:applicability (let ((focused (climish::focused-window climish::application-frame)))
		    (and focused
			 (let ((buffer (window-buffer focused)))
			   (and buffer
				(buffer-minor-mode buffer :lisp-listener))))))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod yank-next-from-lisp-listener-history (window p)
  (yank-from-lisp-listener-history window t))

(defmethod yank-from-lisp-listener-history (window p)
  (when (> (length *lisp-listener-history*) 0)
    (let* ((buffer (window-buffer window))
	   (prompt (variable-value 'hemlock::prompt-mark :buffer buffer)))
      (flet ((reinit ()
	       (if *last-lisp-listener-history-index*
		   (if p
		       (if (= *last-lisp-listener-history-index* (1- (length *lisp-listener-history*)))
			   (setq *last-lisp-listener-history-index* 0)
			   (setq *last-lisp-listener-history-index* (1+ *last-lisp-listener-history-index*)))

		       (if (zerop *last-lisp-listener-history-index*)
			   (setq *last-lisp-listener-history-index* (1- (length *lisp-listener-history*)))
			   (setq *last-lisp-listener-history-index* (1- *last-lisp-listener-history-index*))))
		   (if p
		       (setq *last-lisp-listener-history-index* 0)
		       (setq *last-lisp-listener-history-index* (1- (length *lisp-listener-history*)))))))
	
	(cond ((mark= prompt (buffer-point buffer))
	       (reinit)
	       (insert-string buffer (buffer-point buffer) (aref *lisp-listener-history*
								 *last-lisp-listener-history-index*))
	       (update-window-image window)
	       (unless (displayed-p (buffer-point buffer) window)
		 (hi::bottom-window-on-mark window (buffer-point buffer)))
	       (climish::redraw-buffer window))
		   
	      ((mark> (buffer-point buffer) prompt)
	       (ignore-errors
		(delete-region buffer (region prompt (buffer-point buffer))))
	       (reinit)
	       (insert-string buffer (buffer-point buffer) (aref *lisp-listener-history*
								 *last-lisp-listener-history-index*))
	       (update-window-image window)
	       (unless (displayed-p (buffer-point buffer) window)
		 (hi::bottom-window-on-mark window (buffer-point buffer)))
	       (climish::redraw-buffer window)))))))
	      
	       
	     
    



(defcommand lisp-listener-command (window p)
  (:pretty-name "Lisp Listener")
  (:documentation "Start an REPL in a thread")
  (:help-string "Start an REPL in a thread")
  (:applicability (climish::hemlock-applicability climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod lisp-listener-command (window p)
  (declare (ignore p))
  (let ((frame (climish::application-frame window)))
    (let ((existing (find-window-named frame "Lisp Listener")))
      (or (and existing (progn (climish::focus-window existing) existing))
	  (let ((buffer (make-instance 'buffer :name "Lisp Listener"))
		(current-buffer (hi::window-buffer window)))
	  (setf (buffer-undo-p buffer) nil)
	  (setf (buffer-minor-mode buffer current-buffer :save) nil)
	  (setf (buffer-minor-mode buffer current-buffer :interactive-character-stream) t)
	  (setf (buffer-minor-mode buffer current-buffer :lisp-listener) t)
	  (setf (buffer-major-mode buffer current-buffer) :lisp)
	    (let ((w
		    (climish::find-or-create-window-of-class
		     'climish::lisp-listener-window :buffer buffer)))
	      (setf (window-buffer w) buffer)
	      (let* ((stream (variable-value 'hemlock::buffer-stream :buffer buffer))
		     (thread (bt:make-thread #'(lambda ()
						 (repl stream))
					     :name "Lisp Listener")))
		(defhvar "Lisp Listener Thread" "Lisp Listener Thread" :buffer buffer
		  :value thread)
		(defhvar "Prompt Mark" "Prompt Mark" :buffer buffer
		  :value nil)
		(add-hook buffer hemlock::delete-buffer-hook
			  #'(lambda (buffer)
			      (when (buffer-minor-mode buffer :lisp-listener)
				(let ((thread (hemlock::variable-value 'hemlock::lisp-listener-thread :buffer buffer)))
				  (when (and thread (bt:thread-alive-p thread))
				    (bt:destroy-thread thread))))))
		(climish::focus-window w)
	    
		(values))))))))

(defmethod find-window-named ((w climish::application-frame-mixin) name)
  (find-window-named-1 *echo-area-window* name))

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

(defcommand interrupt-lisp-command (window p)
  (:pretty-name "Interrupt Lisp")
  (:documentation "Interrupt a running Lisp Listener Thread")
  (:help-string "Interrupt a running Lisp Listener Thread")
  (:applicability (let ((focused (climish::focused-window climish::application-frame)))
		    (and focused
			 (let ((buffer (window-buffer focused)))
			   (and buffer
				(buffer-minor-mode buffer :lisp-listener))))))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))
		

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

#+SBCL
(sb-ext:without-package-locks

  (fmakunbound 'sb-debug::debug-prompt)
  
  (defmethod sb-debug::debug-prompt :after ((stream hemlock-interactive-character-io-stream))
    (let ((buffer (hemlock::hemlock-character-io-stream-buffer stream)))
      (drop-prompt-mark buffer)))
  
  (defmethod sb-debug::debug-prompt (stream)
    (sb-thread::get-foreground)
    (format stream
	    "~%~W~:[~;[~W~]] "
	    (sb-di::frame-number sb-debug::*current-frame*)
	    (> sb-debug::*debug-command-level* 1)
	    sb-debug::*debug-command-level*)))

