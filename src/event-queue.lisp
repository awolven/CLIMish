(in-package :krma.gui)

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

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::character-event-mixin))
  (let ((character (clui::input-event-character event)))
    (declare (ignorable character))
    (lparallel.queue:push-queue event (input-event-queue window))))

(defun modifier-key-p (event-id)
  (or (eq (input-event-id-scancode event-id) clui::+key-left-alt+)
      (eq (input-event-id-scancode event-id) clui::+key-right-alt+)
      (eq (input-event-id-scancode event-id) clui::+key-left-GUI+)
      (eq (input-event-id-scancode event-id) clui::+key-right-GUI+)
      (eq (input-event-id-scancode event-id) clui::+key-left-ctrl+)
      (eq (input-event-id-scancode event-id) clui::+key-right-ctrl+)
      (eq (input-event-id-scancode event-id) clui::+key-left-shift+)
      (eq (input-event-id-scancode event-id) clui::+key-right-shift+)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::key-press-event-mixin))
  (let ((event-id (input-event-id event)))
    (unless (modifier-key-p event-id)
      (lparallel.queue:push-queue event (input-event-queue window)))))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::pointer-button-press-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::pointer-wheel-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::spaceball-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::joystick-event-mixin))
  (lparallel.queue:push-queue event (input-event-queue window)))

;; handle-event :after krma-gui-window-mixin pointer-press-event and pointer-release-event
;; are in views.lisp

(defun message (format-string &rest args)
  (apply #'format *debug-io* format-string args)
  (finish-output *debug-io*))

(defmethod handle-character-input (window char p)
  (declare (ignore window char p)))

(defun command-loop (window)
  (let ((w)
	(event)
	(event-id)
	(table-chain)
	(event-chain)
	(result)
	(p nil)
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

	   (print (or (focused-window window) *echo-area-window*))
	   (finish-output)
       
	 RESET (setq w (or (focused-window window) *echo-area-window*))
	   (setq event-chain ())
	   (setq table-chain (list (command-table w)))

	   
       
	   ;; code will continue to wait until it either gets a gesture that matches or C-g is sent
       
	 GET (setq event (get-next-event window))
	   (setf (window-last-event window) event)
	   (setq event-id (input-event-id event))
       
	 CHECK 
       
	   (cond ((characterp event-id)
		  ;; we don't accept character events here without being first preceded by a key event
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
		    (setq result (get-table-entry w (car table-chain) event-id))

		    (cond ((null result)
			   ;; potential race condition:
			   ;; character-event has 1/100th of a second to show up in queue
			   ;; otherwse event is ignored
			   ;; so if computer is running very slowly character events will be dropped
			   (setq event (get-next-event window 0.01))
			   
			   (when event

			     (setf (window-last-event window) event)
			       (setq event-id (input-event-id event))
			     
			     (when (characterp event-id)			       
			       
			       ;; replace preceding key event with character event
			       (setf (car event-chain) event-id)
			       ;; we're still on the same table
			       (setq result (get-table-entry w (car table-chain) event-id))

			       (cond ((null result) (go GET))
			  
				     ((hash-table-p result)
				      (push result table-chain)
				      (go GET))
				 
				     (t (catch 'ignore
					  (catch 'hi::command-loop-catcher
					    (funcall result w p)))
					(go RESET)))))
			 
			   ;;(message "~%~{~A~} is not a defined gesture" (nreverse event-chain))
			   (go RESET))
		       
			  ((hash-table-p result)
			   (push result table-chain)
			   (GO GET))
				   
			  (t (catch 'ignore
			       (catch 'hi::command-loop-catcher
				 (funcall hi::*invoke-hook* result w p)))
			     (go RESET))))))))))

(defun start-command-loop (window)
  (bt:make-thread #'(lambda ()
		      (bt:wait-on-semaphore (command-loop-go-semaphore window))
		      (command-loop window)) :name "command loop")
  #+SBCL(sb-thread:release-foreground (bt:current-thread)))

(defmethod initialize-instance :after ((window krma-gui-window-mixin) &rest initargs)
  (declare (ignore initargs))
  (start-command-loop window)
  (values))

