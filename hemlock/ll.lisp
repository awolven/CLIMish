(in-package :hi)

;; Lisp Listener and hemlock.vktk still suffer from a #'close-line bug in the rendering thread

;; todo: make backspace do the right thing
;; done: enabled minimal functionality by making input line-oriented
;; todo: now need a complete-lisp-form oriented mode
;; todo: once Line-Oriented Character Stream Mode is completely working, get shell process working.
;; maybe rename LO C S M to Interactive Character Stream Mode

;; todo: turn off undo in Lisp Listener.

;; get C-C C-C interrupt lisp listener process working during output

(in-package :hemlock)

(defun invoke-a-restart ()
  (let ((restarts (butlast (compute-restarts))))

     (do ((i 0 (+ i 1)) (r restarts (cdr r))) ((null r))
       (format t "~&~D: ~A~%" i (car r)))
     (let ((n nil) (k (length restarts)))
       (loop (when (and (typep n 'integer) (>= n 0) (< n k))
               (return t))
             (format t "~&Option: ")
             (setq n (read))
             (fresh-line))
       (invoke-restart-interactively (nth n restarts)))))

(defun repl (stream) ;; this function should only depend on stream being a character-io-stream
  ;; so that it is general to any character-io-stream, regardless of buffers/marks etc.
  (let* ((*terminal-io* stream) ;; set up our streams
         (*standard-output* stream)
         (*standard-input* stream)
         (*error-output* stream)
         (*trace-output* #+sbcl sb-sys::*stdout* #-sbcl *trace-output*)
         (*debug-io* stream)
         (*query-io* stream)
         (.form. nil))
    (in-lisp (hemlock::hemlock-character-io-stream-buffer stream)
      (let ((hi::*debug-on-error* t)
            #+sbcl (sb-ext:*invoke-debugger-hook* nil)
            (*debugger-hook* nil))
        (with-nonbroken-debugger
            (loop
	      (show-prompt)
              (finish-output)
              (restart-case
                  (let ()
                    (tagbody
                     read
                       (setq .form. (read stream nil lispbuf-eof))
                       (when (eq .form. lispbuf-eof)
                         (go read)))

                    (setq +++ ++ ++ + + - - .form.)
                    (unwind-protect
                         (let ((this-eval
                                 (multiple-value-list
                                  (eval .form.))))
                           (fresh-line)
                           (dolist (x this-eval) (prin1 x) (terpri))
                           (setq /// // // / / this-eval)
                           (setq *** ** ** * * (car this-eval)))
                      (fresh-line)))
                (abort ()
                  :report (lambda (stream)
                            (format stream "Return to the top level."))
                  (values nil t)))))))))




(defcommand "Lisp Listener" (window p)
    "Start an REPL in a thread"
    "Start an REPL in a thread"
  (declare (ignore p))
  (let ((buffer (make-instance 'buffer :name "Lisp Listener"))
	(current-buffer (hi::window-buffer window)))
    (setf (buffer-undo-p buffer) nil)
    (setf (buffer-minor-mode buffer current-buffer "Save") nil)
    (setf (buffer-minor-mode buffer current-buffer "Interactive-Character-Stream") t)
    (setf (buffer-minor-mode buffer current-buffer "Lisp Listener") t)
    #+NOTYET(setf (buffer-major-mode buffer) "LISP")
    (let ((stream (variable-value 'hemlock::buffer-stream :buffer buffer)))
      (defhvar "Lisp Listener Thread" "Lisp Listener Thread" :buffer buffer
        :value (bt:make-thread #'(lambda ()
                                   (repl stream))
                               :name "Lisp Listener"))
      (krma.gui::focus-window (hi::find-or-create-text-editor-window-for-buffer buffer))
      (values))))

(defcommand "Interrupt Lisp" (p)
    "Interrupt a running Lisp Listener Thread"
    "Interrupt a running Lisp Listener Thread"
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (thread (variable-value 'hemlock::lisp-listener-thread :buffer buffer)))
    (when thread
      (beep)
      (when buffer
	#+sbcl
        (sb-thread:release-mutex (buffer-lock buffer)))
      (bt:interrupt-thread thread #'break))))


