(in-package :climish)

(defvar *input-context* nil)

(defclass event-queue-mixin ()
  ((input-event-queue
    :initform (lparallel.queue:make-queue)
    :reader input-event-queue)))

(defun peek-next-event (window)
  (lparallel.queue:peek-queue (input-event-queue window)))

(defun get-next-event (window &optional (timeout nil))
  (if timeout
      (lparallel.queue:try-pop-queue (input-event-queue window) :timeout timeout)
      (lparallel.queue:pop-queue (input-event-queue window))))



(defun modifier-key-p (event-id)
  (or (eq (input-event-id-scancode event-id) clui::+key-left-alt+)
      (eq (input-event-id-scancode event-id) clui::+key-right-alt+)
      (eq (input-event-id-scancode event-id) clui::+key-left-GUI+)
      (eq (input-event-id-scancode event-id) clui::+key-right-GUI+)
      (eq (input-event-id-scancode event-id) clui::+key-left-ctrl+)
      (eq (input-event-id-scancode event-id) clui::+key-right-ctrl+)
      (eq (input-event-id-scancode event-id) clui::+key-left-shift+)
      (eq (input-event-id-scancode event-id) clui::+key-right-shift+)))



;; handle-event :after application-frame-mixin pointer-press-event and pointer-release-event
;; are in views.lisp

(defun message (format-string &rest args)
  (apply #'format *debug-io* format-string args)
  (finish-output *debug-io*))

(defmethod handle-character-input (window char p)
  (declare (ignore window char p)))

(defmacro maybe-provide (object expression)
  `(let ((tag (loop for (type tag) in *input-context*
		    when (typep ,object type)
		      do (return tag)
		    finally (return nil))))
     (when tag (throw tag ,expression))))

(defmethod accept :around (type &rest initargs &key stream)
  (declare (ignorable stream initargs))
  (let ((*input-context* (list* (list type '#:accept) *input-context*)))
    (catch (second (first *input-context*))
      (call-next-method))))
      
(defmethod accept (type &rest initargs &key stream)
  (declare (ignore initargs))
  (recursive-command-loop stream))



(defmethod accept ((type (eql 'climish::keyword))
		   &rest initargs &key &allow-other-keys)
  (apply #'hi::accept-keyword initargs))


		   

(defun recursive-command-loop (application-frame)
  (compute-applicable-commands application-frame)
  (let ((command))
    (tagbody
     again
       (setq command (read-gesture application-frame))
       (let ((type (first (first *input-context*)))
	     (tag (second (first *input-context*))))
	 (when (typep command type)
	   (throw tag command)))
       (invoke-command command application-frame)
       (go again))))

(defun toplevel-command-loop (application-frame)
  (compute-applicable-commands application-frame)
  (let (#+NOTYET(#+CCL *debugger-hook* #+SBCL sb-ext:*invoke-debugger-hook* #'gui-debug-condition))
    (restart-bind ((ignore #'(lambda (&optional c)
			       (declare (ignorable c))
			       (throw :reset nil))))
      
      (handler-bind
	  
	  ((editor-error #'(lambda (c)
			     (mapcar (lambda (buffer)
				       (bt:release-lock (buffer-lock buffer)))
				     hi::*buffer-list*)
			     (clear-echo-area)
			     (let ((string (editor-error-format-string c)))
			       (when string
				 (apply #'message string
					(editor-error-format-arguments c)))
			       (throw :reset nil)))))

	(let ((thing))
	  
	  (tagbody
	   restart
	     (setq thing (catch :reset (read-gesture application-frame)))
	   invoke
	     (catch :reset
	       (let ((*input-context* (list (list 'command '#:accept))))
		 ;; toplevel command loop only cares if commands are submitted by read gesture
		 ;; or thrown to :accept, it will invoke them
		 (setq thing (catch (second (first *input-context*))
			       (progn (invoke-command thing application-frame) nil)))
		 (when (typep thing 'command)
		   (go invoke))))
	     
	     (go restart)))))))
	     


(defun read-gesture (application-frame)  
  (let ((command-table (command-table application-frame))
	(event)
	(event-id)
	(table-chain)
	(event-chain)
	(result))

    (tagbody
       
     RESET 
       (setq event-chain ())
       (setq table-chain (list command-table))
       
     GET (setq event (get-next-event application-frame))
       (unless event (go GET))
       (setf (window-last-event application-frame) event)
       (setq event-id (input-event-id event))

     CHECK 
       
       (cond ((characterp event-id)
	      ;; we don't accept character events here
	      ;; without being first preceded by a key event
	      (go RESET))

	     ((eq event-id #k"C-g")

	      ;;(hi::invoke-hook hemlock::abort-hook)
			   
	      (message "Abort")
			   
	      (when table-chain
		;; go up one level
		(pop table-chain))

	      (compute-applicable-commands application-frame)
			   
	      (unless table-chain
		;; we've over aborted:
		(go RESET))

	      (throw :reset nil)
	      #+NIL
	      (go GET))
				
	     (t (push event-id event-chain)
		(setq result (gethash event-id (car table-chain)))

		(cond ((null result)

		       (let ((command (default-command application-frame)))
			 (when command
			   (return-from read-gesture command)))

		       (go RESET))
		       
		      ((hash-table-p result)
		       (push result table-chain)
		       (GO GET))
				   
		      (t (return-from read-gesture result))))))))

(defun start-command-loop (application-frame)
  (bt:make-thread #'(lambda ()
		      (bt:wait-on-semaphore (command-loop-go-semaphore application-frame))
		      (toplevel-command-loop application-frame)) :name "command loop")
  #+SBCL(sb-thread:release-foreground (bt:current-thread)))



