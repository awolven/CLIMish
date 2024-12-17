(in-package :climish)

(defvar *application-commands* nil)

(defclass command (standard-generic-function)
  ((pretty-name :accessor hemlock::command-name)
   (applicability)
   (preparation)
   (help-string)
   (resolution))
  (:metaclass funcallable-standard-class))

(defclass everywhere-command (command)
  ()
  (:metaclass funcallable-standard-class))

(defmethod everywhere-command-p (command)
  nil)

(defmethod everywhere-command-p ((command everywhere-command))
  t)

(defclass not-everywhere-command (command)
  ()
  (:metaclass funcallable-standard-class))

(defclass text-editor-command (everywhere-command)
  ()
  (:metaclass funcallable-standard-class))

(defclass lisp-mode-command (text-editor-command)
  ()
  (:metaclass funcallable-standard-class))

(defmethod hemlock::lisp-mode-p (frame)
  (let* ((focused-window (focused-window frame))
	 (focused-view (focused-view focused-window)))
    (and (not (eq focused-view frame))
         (typep focused-view 'text-editor-window-mixin)
         (let ((buffer (hi::window-buffer focused-view)))
           (and buffer (eq (hi::buffer-major-mode buffer) :lisp))))))

(defvar *command-name->command-table*
  (make-hash-table :test #'equalp))

(defmacro defcommand (name arglist &rest initargs)
  (let ((fn (gensym)))
    `(let ((,fn
	     (cl:defgeneric ,name ,arglist
	       ,@(when (assoc :documentation initargs)
		   `((:documentation ,(second (assoc :documentation initargs)))))
	       ,@(if (assoc :generic-function-class initargs)
		     `((:generic-function-class ,(second (assoc :generic-function-class initargs))))
		     `((:generic-function-class command))))))
       ,@(when (assoc :pretty-name initargs)
	   `((setf (slot-value ,fn 'pretty-name)
		   ,(second (assoc :pretty-name initargs)))
	     (setf (hi::getstring ,(second (assoc :pretty-name initargs)) hi::*command-names*)
		   ,fn)))
       ,@(if (assoc :applicability initargs)
	     `((setf (slot-value ,fn 'applicability)
		   (lambda (application-frame)
		     (declare (ignorable application-frame))
		     ,(second (assoc :applicability initargs)))))
	     `((setf (slot-value ,fn 'applicability)
		   (lambda (application-frame)
		     (declare (ignorable application-frame))
		     t))))
       ,@(when (assoc :preparation initargs)
	   `((setf (slot-value ,fn 'preparation)
		   (lambda (application-frame)
		     (declare (ignorable application-frame))		     
		     ,(second (assoc :preparation initargs))))))
       ,@(when (assoc :help-string initargs)
	   `((setf (slot-value ,fn 'help-string)
		   (lambda (application-frame)
		     (declare (ignorable application-frame))		     
		     ,(second (assoc :help-string initargs))))))
       ,@(if (assoc :resolution initargs)
	     `((setf (slot-value ,fn 'resolution)
		   (lambda (application-frame results)
		     (declare (ignorable application-frame results))
		     ,(second (assoc :resolution initargs)))))
	     `((setf (slot-value ,fn 'resolution)
		     (lambda (application-frame results)
		       (declare (ignorable application-frame))
		       (if results
			   (hi:message "~A returned: ~S~{, ~S~}"
				       (slot-value ,fn 'pretty-name)
				       (first results)
				       (rest results))
			   (hi:message "~A returned no values."
				       (slot-value ,fn 'pretty-name)))))))
       (pushnew ,fn *application-commands*)
       ,fn)))

(defmacro define-everywhere-command (name arglist &rest initargs)
  `(defcommand ,name ,arglist
     ,@initargs
     ,@(when (not (assoc :preparation initargs))
	 `((:preparation (climish::hemlock-arguments climish::application-frame))))
     (:applicability t)
     (:resolution (values))
     (:generic-function-class everywhere-command)))

(defmacro hemlock::defhcommand (name arglist &rest initargs)
  `(defcommand ,name ,arglist
     ,@(when (not (assoc :preparation initargs))
	 `((:preparation (hemlock-arguments climish::application-frame))))
     ,@(when (not (assoc :applicability initargs))
	 `((:applicability (hemlock-applicability climish::application-frame))))
     (:resolution (values))
     ,@(when (not (assoc :generic-function-class initargs))
	 `((:generic-function-class text-editor-command)))
     ,@initargs))

(defmacro hemlock::define-lisp-mode-command (name arglist &rest initargs)
  `(defcommand ,name ,arglist
     (:preparation (hemlock-arguments climish::application-frame))
     (:applicability (hemlock::lisp-mode-p climish::application-frame))
     (:resolution (values))
     (:generic-function-class lisp-mode-command)
     ,@initargs))

(defmethod invoke-command (command application-frame)
  ;; the command stack is for computing applicability to commands
  ;; some commands are only available "inside" other commands.
  (push command (command-stack application-frame))
  (compute-applicable-commands application-frame)
  (unwind-protect
       (let ((results
	       (multiple-value-list
		(apply command
		       (funcall (slot-value command 'preparation) application-frame)))))
	 (prog1 (values-list results)
	   (funcall (slot-value command 'resolution) application-frame results)))
    
    (pop (command-stack application-frame))))

(defmethod invoke-command ((command everywhere-command) application-frame)
  (let ((results
	  (multiple-value-list
	   (apply command
		  (funcall (slot-value command 'preparation) application-frame)))))
    (prog1 (values-list results)
      (funcall (slot-value command 'resolution) application-frame results))))

(defun compute-applicable-commands (application-frame)
  (prog1 (setf (applicable-commands application-frame)
	       (remove-if #'null (mapcar #'(lambda (cgf)
					     (when (funcall (slot-value cgf 'applicability) application-frame)
					       cgf))
					 *application-commands*)))
    (rebuild-command-table application-frame)))



(defun hemlock-arguments (application-frame)
  (let* ((focused-window (focused-window application-frame))
	 (focused-view (focused-view focused-window)))
    (list focused-view nil)))

(defun toplevel-context-p (application-frame)
  (null (command-stack application-frame)))

(defun hemlock-applicability (application-frame)
  (let* ((focused-window (focused-window application-frame))
	 (focused-view (focused-view focused-window)))
    (typep focused-view 'text-editor-view-mixin)))

(defvar *accelerator-table*
  (make-hash-table :test #'eq))

(defmacro defaccelerator (command-name &rest gesture)
  (let ((command-var (gensym)))
    `(let ((,command-var (hi::getstring ,command-name hi::*command-names*)))
       (if (null ,command-var)
	   (warn "unknown command: ~S" ',command-name)
	   (setf (gethash ,command-var *accelerator-table*)
		 (list* ',(mapcar #'eval gesture) (gethash ,command-var *accelerator-table*)))))))

(defvar *command-table*
  ;; this is the command table the event-queue loop (aka input-editor) checks
  (make-hash-table :test #'eq))

(defun rebuild-command-table (application-frame)
  (let ((applicable-commands (applicable-commands application-frame))
	(command-table (command-table application-frame)))
    (clrhash command-table)	
    (loop for command in applicable-commands
	  do (let ((gestures (gethash command *accelerator-table*)))
	       (loop for gesture in gestures
		     do (let ((table command-table))
			  (when gesture
			    (loop for event-ids on gesture
				  do (if (null (rest event-ids))
					 (setf (gethash (first event-ids) table) command)
					 (let ((existing (gethash (first event-ids) table)))
					   (if (typep existing 'hash-table)
					       (setq table (setf (gethash (first event-ids) table) existing))
					       (setq table (setf (gethash (first event-ids) table) (make-hash-table :test #'eq))))))))))))
    command-table))
					     


