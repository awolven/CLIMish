(in-package :climish)

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


			   

(defun command-loop (application-frame)
  (compute-applicable-commands application-frame)
  (command-loop-1 application-frame (command-table application-frame)))

(defun command-loop-1 (application-frame command-table)  
  (let ((event)
	(event-id)
	(table-chain)
	(event-chain)
	(result)
	#+NIL
	(#+CCL *debugger-hook* #+SBCL sb-ext:*invoke-debugger-hook* #'gui-debug-condition))



    (restart-bind ((ignore #'(lambda (&optional c)
			       (declare (ignorable c))
			       (throw 'ignore nil))))
    
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
			       (throw 'hi::command-loop-catcher nil)))))


    
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

		    (hi::invoke-hook hemlock::abort-hook)
			   
		    (message "Abort")
			   
		    (when table-chain
		      ;; go up one level
		      (pop table-chain))
			   
		    (unless table-chain
		      ;; we've over aborted:
		      (go RESET))
	      
		    (go GET))
				
		   (t (push event-id event-chain)
		      (setq result (gethash event-id (car table-chain)))

		      (cond ((null result)

			     (catch :reset
			       (when (default-command application-frame)
				 (invoke-command (default-command application-frame) application-frame)))
			   
			     (go RESET))
		       
			    ((hash-table-p result)
			     (push result table-chain)
			     (GO GET))
				   
			    (t (catch 'ignore
				 (catch 'hi::command-loop-catcher
				   ;;(funcall hi::*invoke-hook* result w p)))
				   (invoke-command result application-frame)))
			       (go RESET))))))))))

(defun start-command-loop (application-frame)
  (bt:make-thread #'(lambda ()
		      (bt:wait-on-semaphore (command-loop-go-semaphore application-frame))
		      (command-loop application-frame)) :name "command loop")
  #+SBCL(sb-thread:release-foreground (bt:current-thread)))



