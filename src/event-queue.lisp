(in-package :krma.gui)

(defun peek-next-event (window)
  (lparallel.queue:peek-queue (input-event-queue window)))

(defun get-next-event (window &optional (timeout nil))
  (if timeout
      (lparallel.queue:try-pop-queue (input-event-queue window) :timeout timeout)
      (lparallel.queue:pop-queue (input-event-queue window))))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::character-event-mixin))
  (let ((character (clui::input-event-character event)))
    (lparallel.queue:push-queue character (input-event-queue window))))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::key-press-event-mixin))
  (let ((event-id (clui::input-event-id event)))
    (lparallel.queue:push-queue event-id (input-event-queue window))))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::pointer-button-press-event-mixin))
  (lparallel.queue:push-queue (clui::input-event-id event) (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::pointer-wheel-event-mixin))
  (lparallel.queue:push-queue (clui::input-event-id event) (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::pointer-motion-event-mixin))
  (lparallel.queue:push-queue (clui::input-event-id event) (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::spaceball-event-mixin))
  (lparallel.queue:push-queue (clui::input-event-id event) (input-event-queue window)))

(defmethod clui:handle-event :after ((window krma-gui-window-mixin)
				     (event clui::joystick-event-mixin))
  (lparallel.queue:push-queue (clui::input-event-id event) (input-event-queue window)))

(defun message (format-string &rest args)
  (apply #'format *debug-io* format-string args)
  (finish-output *debug-io*))

(defmethod handle-character-input (window char p)
  (declare (ignore window char p)))

(defun ensure-gesture-1 (table function ids)
  (if (cdr ids)
      (ensure-gesture-1 (or (let ((maybe-ct (table-lookup table (car ids))))
			      (when (command-table-p maybe-ct)
				maybe-ct))
			    (setf (table-lookup table (car ids)) (make-instance 'command-table)))
			function
			(cdr ids))
      (setf (table-lookup table (car ids)) function)))

(defmacro define-gesture (class function &rest ids)
  `(ensure-gesture-1 (command-table (find-class ',class)) ',function (mapcar #'eval (list ,@ids))))

(defun command-loop (window)
  (let ((w)
	(event-id)
	(table-chain)
	(event-chain)
	(result)
	(p 1))
    (tagbody
       
     RESET (setq w (or (focused-window window) window))
       (setq event-chain ())
       (setq table-chain (list (command-table (class-of w))))
       
       ;; code will continue to wait until it either gets a gesture that matches or C-g is sent
       
     GET (setq event-id (get-next-event window))
       
     CHECK (print event-id)
       (finish-output)
       
       (cond ((eq event-id #k"C-g")
			   
	      (message "Abort")
			   
	      (when table-chain
		;; go up one level
		(pop table-chain))
			   
	      (unless table-chain
		;; we've over aborted:
		(go RESET))
	      
	      (go GET))
				
	     (t (push event-id event-chain)
		(setq result (table-lookup (car table-chain) event-id))

		(cond ((null result)
		       ;; potential race condition:
		       ;; character-event has 1/100th of a second to show up in queue
		       ;; otherwse event is ignored
		       ;; so if computer is running very slowly character events will be dropped
		       (setq event-id (get-next-event window 0.01))
				
		       (when event-id
			 (when (characterp event-id)
			   (print event-id)
			   (finish-output)
				    
			   ;; replace preceding key event with character event
			   (setf (car event-chain) event-id)
			   ;; we're still on the same table
			   (setq result (table-lookup (car table-chain) event-id))
				    
			   (cond ((null result) (go GET))
			  
				 ((command-table-p result)
				  (push result table-chain)
				  (go GET))
				 
				 (t (unwind-protect (funcall result w p)
				      (go RESET))))))

		       (message "~%~{~A~} is not a defined gesture" (nreverse event-chain))
		       (go RESET))
		       
		      ((command-table-p result)
		       (push result table-chain)
		       (GO GET))
				   
		      (t (unwind-protect (funcall result w p)
			   (go RESET)))))))))

(defun start-command-loop (window)
  (bt:make-thread #'(lambda ()
		      (command-loop window)) :name "command loop"))

(defmethod initialize-instance :after ((window krma-gui-window-mixin) &rest initargs)
  (declare (ignore initargs))
  (start-command-loop window)
  (values))


