;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock)



(defcommand "Grep"
    (window p &optional (command (hi::prompt-for-string window
                           :prompt "Run grep (like this): "
                           :default "grep -nH -e "))
                 (directory (default-directory)))
  "" ""
  #+scl (setf directory (ext:unix-namestring directory nil))
  (setf (buffer-major-mode (shell-command-command window p command directory) (window-buffer window))
        "Grep"))

(defcommand "Grep Goto" (window p)
  "" ""
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (cl-ppcre:register-groups-bind (file line)
        ("^\([^:]+\):\([0-9]+\):" (line-string buffer (mark-line (buffer-point buffer))))
    (when file
      (change-to-buffer (find-file-buffer window (merge-pathnames file (default-directory))) window)
      (let ((point (buffer-point buffer)))
        (push-buffer-mark buffer (copy-mark point))
        (buffer-start point)
        (when line
          (line-offset buffer point (1- (parse-integer line)))))))))

#+NIL
(bind-key "Grep Goto" #k"return" :mode "Grep")
