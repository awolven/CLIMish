(in-package :krma.gui)

(defvar *application-commands* nil)

(defclass command-generic-function (standard-generic-function)
  ((pretty-name :accessor hemlock::command-name)
   (applicability)
   (argument-computation)
   (help-string))
  (:metaclass funcallable-standard-class))

(defvar *command-name->command-table*
  (make-hash-table :test #'equalp))

(defmacro defcommand (name arglist &rest initargs)
  (let ((fn (gensym)))
    `(let ((,fn
	     (cl:defgeneric ,name ,arglist
	       ,@(when (assoc :documentation initargs)
		   `((:documentation ,(second (assoc :documentation initargs)))))
	       (:generic-function-class command-generic-function))))
       ,@(when (assoc :pretty-name initargs)
	   `((setf (slot-value ,fn 'pretty-name)
		   ,(second (assoc :pretty-name initargs)))
	     (setf (hi::getstring ,(second (assoc :pretty-name initargs)) hi::*command-names*)
		   ,fn)))
       ,@(if (assoc :applicability initargs)
	     `((setf (slot-value ,fn 'applicability)
		   (lambda (app)
		     (declare (ignorable app))
		     ,(second (assoc :applicability initargs)))))
	     `((setf (slot-value ,fn 'applicability)
		   (lambda (app)
		     (declare (ignorable app))
		     t))))
       ,@(when (assoc :argument-computation initargs)
	   `((setf (slot-value ,fn 'argument-computation)
		   (lambda (app)
		     (declare (ignorable app))		     
		     ,(second (assoc :argument-computation initargs))))))
       ,@(when (assoc :help-string initargs)
	   `((setf (slot-value ,fn 'help-string)
		   (lambda (app)
		     (declare (ignorable app))		     
		     ,(second (assoc :help-string initargs))))))
       (pushnew ,fn *application-commands*)
       ,fn)))

(defmacro hemlock::defhcommand (name arglist &rest initargs)
  `(defcommand ,name ,arglist
     (:argument-computation (krma.gui::hemlock-arguments krma.gui::app))
     (:applicability (krma.gui::hemlock-applicability krma.gui::app))
     ,@initargs))
    

(defmethod invoke-command (command-generic-function)
  (apply command-generic-function
	 (funcall (slot-value command-generic-function 'argument-computation) *app*)))

(defun compute-applicable-commands ()
  (remove-if #'null (mapcar #'(lambda (cgf)
					    (when (funcall (slot-value cgf 'applicability) *app*)
					      cgf))
					*application-commands*)))

(defun hemlock-arguments (app)
  (list (or (focused-window (main-window app)) *echo-area-window*) nil))

(defun hemlock-applicability (app)
  t
  #+NIL
  (typep (focused-window (main-window app)) 'text-editor-view-mixin))

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

(defun rebuild-command-table (applicable-commands)
  (clrhash *command-table*)
  (loop for command in applicable-commands
	do (let ((gestures (gethash command *accelerator-table*)))
	     (loop for gesture in gestures
		   do (let ((table *command-table*))
			(when gesture
			  (loop for event-ids on gesture
				do (if (null (rest event-ids))
				       (setf (gethash (first event-ids) table) command)
				       (let ((existing (gethash (first event-ids) table)))
					 (if (typep existing 'hash-table)
					     (setq table (setf (gethash (first event-ids) table) existing))
					     (setq table (setf (gethash (first event-ids) table) (make-hash-table :test #'eq)))))))))))))
					     


