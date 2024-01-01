(in-package :climish)

(defclass homemade-window-class (adhoc:adhoc-class)
  ((command-table :initform (make-hash-table :test #'eq))))

(defmethod validate-superclass ((subclass homemade-window-class) (superclass adhoc:adhoc-class))
  t)

(defclass text-editor-view-class (homemade-window-class)
  ())
  
(defmethod validate-superclass ((subclass text-editor-view-class) (superclass homemade-window-class))
  t)

(defclass echo-area-window-class (text-editor-view-class)
  ())
  
(defmethod validate-superclass ((subclass echo-area-window-class) (superclass text-editor-view-class))
  t)
