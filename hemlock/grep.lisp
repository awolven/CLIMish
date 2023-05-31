;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock)



(defcommand "Grep"
    (window p &optional (command (hi::prompt-for-string window
                           :prompt "Run grep (like this): "
                           :default "grep -nH -e "))
                 (directory (default-directory)))
  "" ""
  #+scl (setf directory (ext:unix-namestring directory nil))
  (setf (buffer-major-mode (shell-command-command p command directory))
        "Grep"))

(defcommand "Grep Goto" (window p)
  "" ""
  (declare (ignore p))
  (cl-ppcre:register-groups-bind (file line)
      ("^\([^:]+\):\([0-9]+\):" (line-string (mark-line (current-point))))
    (when file
      (change-to-buffer (find-file-buffer (merge-pathnames file (default-directory))) window)
      (let ((point (current-point)))
        (push-buffer-mark (copy-mark point))
        (buffer-start point)
        (when line
          (line-offset point (1- (parse-integer line))))))))

#+NIL
(bind-key "Grep Goto" #k"return" :mode "Grep")
