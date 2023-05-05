(in-package :krma.gui)

(defvar *style*)

(defvar *object-id-counter* 0)

(defun new-object-id ()
  (incf *object-id-counter*))

(defvar *object-id->object-table*
  #+ccl
  (make-hash-table :test #'eq :weak t)
  #+sbcl(make-hash-table :test #'eq :weakness :value))

(defun (setf krma::object-from-id) (object id)
  (setf (gethash id *object-id->object-table*) object))

(defun krma::object-from-id (id)
  (gethash id *object-id->object-table*))

(defvar *group-counter* 0)

(defun new-group ()
  (incf *group-counter*))

(defmacro regenerate! (object)
  `(eval (list* 'send ,object (send ,object root-path))))




