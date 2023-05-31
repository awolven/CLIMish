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




(in-package :hemlock-internals)

(defvar *echo-area-buffer* nil
  "Buffer used to hack text for the echo area.")
(defvar *echo-area-region* nil
  "Internal thing that's the *echo-area-buffer*'s region.")
(defvar *echo-area-stream* nil
  "Buffered stream that prints into the echo area.")
(defvar *echo-area-window* ()
  "Window used to display stuff in the echo area.")
(defvar *parse-starting-mark* nil
  "Mark that points to the beginning of the text that'll be parsed.")
(defvar *parse-input-region* nil
  "Region that contains the text typed in.")

(defvar *buffer-list* () "A list of all the buffer objects.")
(defvar *buffer-names* nil "A String Table of Buffer names and their corresponding objects.")
