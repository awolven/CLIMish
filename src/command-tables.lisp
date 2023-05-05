(in-package :krma.gui)

(defvar *global-command-table* (make-hash-table :test #'eq))

(defmethod get-command-table (class)
  (loop for class in (class-precedence-list class)
     when (slot-exists-p class 'command-table)
       do (return (slot-value class 'command-table))))

(defun hash-table-recursive-copy (old-hash-table new-hash-table)
  (maphash #'(lambda (k v)
	       (if (hash-table-p v)
		   (let ((existing (gethash k new-hash-table)))
		     (if (hash-table-p existing)
			 ;; copy into the new-hash-table's existing sub-table
			 (hash-table-recursive-copy v existing)
			 ;; otherwise create a new sub hash table for this key of old-hash-table,
			 ;; if new-hash-table already cames with existing value for this key (such as a command)
			 ;; shadow it by blowing it away with a new sub hash table
			 (setf (gethash k new-hash-table) (hash-table-recursive-copy v (make-hash-table :test #'eq)))))
		   (setf (gethash k new-hash-table) v)))
	   old-hash-table)
  new-hash-table)

(defun rebuild-command-table (text-editor-view)
  (let ((command-table (command-table text-editor-view)))
    (clrhash command-table) ;; start fresh from here
    (hash-table-recursive-copy *global-command-table* command-table)
    (let ((class-precedence-list (list (class-of text-editor-view))))
      (loop for class in class-precedence-list
	   with class-table = nil
	 when (setq class-table (get-command-table class))
	 do (hash-table-recursive-copy class-table command-table)))
    (let ((buffer (hi::window-buffer text-editor-view)))
      (when buffer
	(let ((reverse-modes (reverse (hi::buffer-mode-objects buffer))))
	  (loop for mode in reverse-modes
	     with mode-table = nil
	     when (setq mode-table (hi::mode-object-bindings mode))
	     do (hash-table-recursive-copy mode-table command-table)))
	(let ((buffer-bindings (hi::buffer-bindings buffer)))
	  (when buffer-bindings
	    (hash-table-recursive-copy buffer-bindings command-table))))
      
      t)))

(defmethod get-table-entry (object table key)
  (declare (ignore object))
  (gethash key table))

(defmethod get-table-entry ((object view-mixin) table (key character))
  #'(lambda (window p)
      (handle-character-input window key (or p 1))))

(defun set-table-entry (table key val)
  (dotimes (i (1- (length key)))
    (let* ((key-event (elt key i))
           (foo (gethash key-event table)))
      (if (hash-table-p foo)
          (setf table foo)
          (let ((new-table (make-hash-table)))
            (setf (gethash key-event table) new-table)
            (setf table new-table)))))
  (if (null val)
      (remhash (elt key (1- (length key))) table)
      (setf (gethash (elt key (1- (length key))) table) val)))

(defun define-gesture (command where &rest ids)
  (let ((cmd (hi::getstring command hi::*command-names*)))
    (unless cmd
      (error "Unknown command ~S while defining gesture ~{~A~}" command ids))
    (let ((table (cond ((and (symbolp where) (find-class where nil)) (get-command-table (find-class where)))
			((bufferp where) (hi::buffer-bindings where))
			((stringp where) (let ((mode (hi::getstring where hi::*mode-names*)))
					   (unless mode
					     (error "~S is an unknown mode while binding ~{~A~}" where ids))
					   (hi::mode-object-bindings mode)))
			
			((eq where :global) *global-command-table*)
			(t (error "Cannot find command table for ~S while binding ~{~A~}" where ids)))))
      (set-table-entry table ids cmd)
      (push (list ids where) (hi::command-%bindings cmd))
      (values))))
	   
	 
  
	










