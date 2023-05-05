(in-package :krma.gui)

(defobject text-editor-window-mixin (text-editor-view-mixin homemade-window-mixin)
  :inputs
  ((title "hemlock")))

(defobject text-editor-window (text-editor-window-mixin))

(defun make-hemlock-window ()
  (make-instance 'text-editor-window 'x 100 'y 75))

(defun command-save-file (window p)
  (declare (ignore window))
  (dotimes (n p)
    (print 'hiya)))



