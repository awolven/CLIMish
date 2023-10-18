(in-package :krma.gui)

(defmethod compute-command-table (view continued-command)
  (declare (ignore continued-command))
  (command-table view))

(defmethod compute-command-table ((view echo-area-window-mixin) (continued-command null))
  (command-table view))

(defmethod compute-command-table ((view echo-area-window-mixin) (continued-command hi::command))
  (let ((bindings (get-command-table continued-command))
	(table (make-hash-table :test #'eq)))
    (rebuild-command-table-1 view table)
    (hash-table-recursive-copy bindings table)
    table))    

(defvar *global-command-table* (make-hash-table :test #'eq))

(defmethod get-command-table (class)
  (loop for class in (compute-class-precedence-list class)
     when (slot-exists-p class 'command-table)
       do (return (slot-value class 'command-table))))

(defmethod get-command-table ((command hi::command))
  (or (hi::command-command-table command)
      (setf (hi::command-command-table command) (make-hash-table :test #'eq))))

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
    (rebuild-command-table-1 text-editor-view command-table)))

(defun rebuild-command-table-1 (text-editor-view command-table)
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
      
      t))

(defun command-bindings (view command)
  (let ((bindings (hi::command-%bindings command))
	(results ()))
    ;;(break "~S" *global-command-table*)
    ;;(break "~S" bindings)
    (flet ((test (bindings table)
	     (let ((result))
	       (tagbody
		again (when (null bindings)
			(go exit))
		  
		  (setq result (gethash (car bindings) table))
		  
		  (if result
		      (if (hash-table-p result)
			  
			  (progn
			    (setq table result)
			    (setq bindings (cdr bindings))
			    (go again))
			  
			  (go exit))
		      (go exit))
		  exit)
	       result)))

      (loop for binding in bindings
	    do
	       (when (test (car binding) *global-command-table*)
		 (push (list (car binding) :global (hi::view-buffer view)) results))
      
	       (when (test (car binding) (get-command-table (class-of view)))
		 (push (list (car binding) (list :class (class-of view)) (hi::view-buffer view)) results))
	       
	       (let ((buffer (hi::view-buffer view)))
		 (when buffer
		   (let ((reverse-modes (reverse (hi::buffer-mode-objects buffer))))
		     (loop for mode in reverse-modes
			   with mode-table = nil
			   when (test (car binding) (hi::mode-object-bindings mode))
			     do (push (list (car binding) (list :mode mode) (hi::view-buffer view)) results)))

		   (when (hi::buffer-bindings (hi::view-buffer view))
		     (when (test (car binding) (hi::buffer-bindings (hi::view-buffer view)))
		       (push (list (car binding) (list :buffer (hi::view-buffer view)) (hi::view-buffer view)) results))))))
      
      results)))

	  
      
	  

      

      
      

(defmethod get-table-entry (object (table hash-table) key)
  (declare (ignore object))
  (gethash key table))

(defmethod get-table-entry ((object view-mixin) (table hash-table) (key character))
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
					   (if mode
					       (hi::mode-object-bindings mode)
					       (let ((command (hi::getstring where hi::*command-names*)))
						 (if command
						     (get-command-table command)
						     (error "~S is an unknown mode while binding ~{~A~}" where ids))))))
			
			((eq where :global) *global-command-table*)
			(t (error "Cannot find command table for ~S while binding ~{~A~}" where ids)))))
      (set-table-entry table ids cmd)
      (push (list ids where) (hi::command-%bindings cmd))
      (values))))
	   
	 
  
	










