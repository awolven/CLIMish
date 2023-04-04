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
    (in-lisp
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
                         ;;(sleep 0.001)
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


(defmode "Lisp Listener" :major-p nil)

(defcommand "Lisp Listener" (p)
    "Start an REPL in a thread"
    "Start an REPL in a thread"
  (declare (ignore p))
  (let ((buffer (make-buffer "Lisp Listener")))
    (setf (buffer-undo-p buffer) nil)
    (setf (buffer-minor-mode buffer "Save") nil)
    (setf (buffer-minor-mode buffer "Interactive-Character-Stream") t)
    (setf (buffer-minor-mode buffer "Lisp Listener") t)
    #+NOTYET(setf (buffer-major-mode buffer) "LISP")
    (let ((stream (variable-value 'hemlock::buffer-stream :buffer buffer)))
      (defhvar "Lisp Listener Thread" "Lisp Listener Thread" :buffer buffer
        :value (bt:make-thread #'(lambda ()
                                   (repl stream))
                               :name "Lisp Listener"))
      (change-to-buffer buffer))))

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





#+NOTYET
(defcommand "Confirm Eval Input" (p)
    "Evaluate Eval Mode input between point and last prompt."
    "Evaluate Eval Mode input between point and last prompt."
  (declare (ignore p))

  ;;(buffer-end (current-point))
  ;;(insert-string (current-point) (string #\newline))
  (block confirm-eval-input
    (let* ((input-region (get-interactive-input)))
      (let ((string (region-to-string input-region)))
        ;;(let ((standard-input (value eval-output-stream)))
        (let ((buffer (current-buffer)))
                      (move-mark
                       (variable-value 'buffer-input-mark :buffer buffer)
                       (buffer-point buffer)))
          (throw 'hi::lispbuf-editor-input
                string)
          ;;(if (hi::dont-preprocess-input standard-input)

              #+NOTYET
              (progn
                (when (and string
                           (> (length string) 0)
                           (char= (char string (1- (length string))) #\Newline))
                  (let ((breaks ()))
                    (with-input-from-string (s string)
                      (handler-case (let ((*read-eval* nil))
                                      (loop
                                        (if (peek-char t s nil nil) ;; the rest exists and is not whitespace
                                            (progn (read s) ;; without error means found a form
                                                   (push (file-position s) breaks)) ;; record the form breaks

                                            ;; we're done with this string, and possibly the rest is whitespace
                                            (return))))
                        (error () (return-from confirm-eval-input)))) ;; a read was bogus, start over

                    ;; break string into forms and submit to input for repl thread
                    (loop for break in (nreverse breaks) with last-break = 0
                          do (sb-concurrency:enqueue (subseq string last-break
                                                             (1- (setq last-break break)))
                                                     vktk::(read-queue *app*)))

                    ;; we need to move the region start beyond the input
                    ;; typing, so next read, if any, is clean
                    (let ((buffer (current-buffer)))
                      (move-mark
                       (variable-value 'buffer-input-mark :buffer buffer)
                       (buffer-point buffer)))

                    ;; the input is ready for the repl
                    (bt:signal-semaphore vktk::(before-read-semaphore *app*))

                    ;; don't do anything (such as process more input) until repl is done writing to the buffer
                    (bt:wait-on-semaphore vktk::(after-print-semaphore *app*))

                    ;; we need to move the region start beyond the output, so next read is clean
                    (let ((buffer (current-buffer)))
                      (move-mark
                       (variable-value 'buffer-input-mark :buffer buffer)
                       (buffer-point buffer))))))))))
