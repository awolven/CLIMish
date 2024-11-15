;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :hemlock)

;;;
;;; **********************************************************************
;;;
;;; Stuff to do a little lisp hacking in the editor's Lisp environment.
;;;

(defvar lispbuf-eof '(nil))

(defmacro in-lisp (buffer &body body)
  "Evaluates body inside HANDLE-LISP-ERRORS.  *package* is bound to the package
   named by \"Current Package\" if it is non-nil."
  (let ((current-package-name (gensym))
        (package-at-point-name (gensym))
        (package (gensym)))
    `;;(handle-lisp-errors
      (let* ((,current-package-name (value ,buffer current-package))
             (,package-at-point-name (package-at-point ,buffer (buffer-point ,buffer)))
             (,package (or (and ,current-package-name
                                (find-package ,current-package-name))
                           (and ,package-at-point-name
                                (find-package ,package-at-point-name)))))
        (progv (if ,package '(*package*)) (if ,package (list ,package))
          ,@body))));;)



(define-file-option "Package" (buffer value)
  (defhvar "Current Package"
    "The package used for evaluation of Lisp in this buffer."
    :buffer buffer
    :value
    (let* ((eof (list nil))
           (thing (read-from-string value nil eof)))
      (when (eq thing eof) (error "Bad package file option value."))
      (cond
       ((stringp thing)
        thing)
       ((symbolp thing)
        (symbol-name thing))
       ((characterp thing)
        (string thing))
       (t
        (message
         "Ignoring \"package\" file option -- cannot convert to a string."))))))


;;;; Eval Mode Interaction.

(defun get-prompt (buffer)
  (let* ((p (in-lisp buffer *package*))
         (short-nick
          (iterate:iter
           (iterate:for nick in (cons (package-name p) (package-nicknames p)))
           (iterate:finding nick minimizing (length nick)))))
    (format nil "~A> " short-nick)))

(defun show-prompt (&optional (stream *standard-output*))
  (princ (get-prompt (hemlock::hemlock-character-io-stream-buffer stream)) stream))


#+NIL
(defparameter *eval-welcome-message*
  "Welcome to the low-level Eval buffer.~@
   ~@
   This buffer is useful for debugging purposes, but be careful:~@
   You are running code directly in Hemlock's event loop.~%~%")

#+NIL
(defun setup-eval-mode (buffer)
  (let ((point (buffer-point buffer)))
    (setf (buffer-minor-mode buffer "Eval") t)
    (setf (buffer-minor-mode buffer "Editor") t)
    (setf (buffer-major-mode buffer) "Lisp")
    (buffer-end point)
    (defhvar "Current Package"
      "This variable holds the name of the package currently used for Lisp
       evaluation and compilation.  If it is Nil, the value of *Package* is used
       instead."
      :value nil
      :buffer buffer)
    (unless (hemlock-bound-p 'buffer-input-mark :buffer buffer)
      (defhvar "Buffer Input Mark"
        "Mark used for Eval Mode input."
        :buffer buffer
        :value (copy-mark point :right-inserting))
      (defhvar "Eval Output Stream"
        "Output stream used for Eval Mode output in this buffer."
        :buffer buffer
        :value (make-hemlock-output-stream point))
      (defhvar "Interactive History"
        "A ring of the regions input to an interactive mode (Eval or Typescript)."
        :buffer buffer
        :value (make-ring (value interactive-history-length)))
      (defhvar "Interactive Pointer"
        "Pointer into \"Interactive History\"."
        :buffer buffer
        :value 0)
      (defhvar "Searching Interactive Pointer"
        "Pointer into \"Interactive History\"."
        :buffer buffer
        :value 0))
    (let ((*standard-output*
           (variable-value 'eval-output-stream :buffer buffer)))
      (fresh-line)
      (format t *eval-welcome-message*)
      (show-prompt))
    (move-mark (variable-value 'buffer-input-mark :buffer buffer) point)))

#+NIL
(defmode "Eval" :major-p nil :setup-function 'setup-eval-mode)

#+NIL
(defun eval-mode-lisp-mode-hook (buffer on)
  "Turn on Lisp mode when we go into Eval Mode."
  (when on
    (setf (buffer-major-mode buffer) "Lisp")))
;;;
#+NIL
(add-hook eval-mode-hook 'eval-mode-lisp-mode-hook)

#+NIL
(defhvar "Editor Definition Info"
  "When this is non-nil, the editor Lisp is used to determine definition
   editing information; otherwise, the slave Lisp is used."
  :value t
  :mode "Eval")

#+NIL
(defvar *selected-eval-buffer* nil)

#+NIL
(defcommand "Select Eval Buffer" (p)
  "Goto buffer in \"Eval\" mode, creating one if necessary."
  "Goto buffer in \"Eval\" mode, creating one if necessary."
  (declare (ignore p))
  (unless *selected-eval-buffer*
    (when (getstring "Eval" *buffer-names*)
      (editor-error "There is already a buffer named \"Eval\"!"))
    (setf *selected-eval-buffer*
          (make-buffer "Eval"
                       :delete-hook
                       (list #'(lambda (buf)
                                 (declare (ignore buf))
                                 (setf *selected-eval-buffer* nil)))))
    (setf (buffer-minor-mode *selected-eval-buffer* "Eval") t))
  (change-to-buffer *selected-eval-buffer*))



#+NIL
(defhvar "Unwedge Interactive Input Confirm"
  "When set (the default), trying to confirm interactive input when the
   point is not after the input mark causes Hemlock to ask the user if he
   needs to be unwedged.  When not set, an editor error is signaled
   informing the user that the point is before the input mark."
  :value t)

#+NIL
(defun unwedge-eval-buffer ()
  (abort-eval-input-command nil))

#+NIL
(defhvar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-eval-buffer
  :mode "Eval")

#+NIL
(defhvar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Prompt again at the end of the buffer? "
  :mode "Eval")

(defmacro with-nonbroken-debugger (&rest forms)
  #+sbcl `(sb-thread:with-new-session () ,@forms)
  #-sbcl `(progn ,@forms))

#+NIL
(defcommand "Confirm Eval Input" (p)
    "Evaluate Eval Mode input between point and last prompt."
    "Evaluate Eval Mode input between point and last prompt."
  (declare (ignore p))
  (buffer-end (current-point))
  (insert-string (current-point) (string #\newline))
  (let ((input-region (get-interactive-input)))
    (when input-region
      (if hi::*reading-lispbuf-input*
          (throw 'hi::lispbuf-input
            (concatenate 'string
                         (region-to-string input-region)))
          (let* ((output (value eval-output-stream))
                 (*terminal-io* output)
                 (*standard-output* output)
                 (*standard-input* output)
                 (*error-output* output)
                 (*trace-output* output)
                 (*debug-io* output)
                 (*query-io* output))
            (fresh-line)
            (in-lisp
              (let ((copy (copy-region input-region))
                    (copy2 (copy-region input-region)))
                (with-input-from-region (stream copy)
                  (let ((string (region-to-string copy2)))
                    (when (and string
                               (> (length string) 0)
                               (char= (char string (1- (length string))) #\newline))
                      (let ((result nil))
                        (with-input-from-string (s string)
                          (handler-case (let ((*read-eval* nil))
                                          (loop
                                            do (if (peek-char t s nil nil)
                                                   (read s)
                                                   (return)))
                                          (setq result t))
                            (error () (setq result nil))))
                        (when result
                          (loop
                            (let ((form (read stream nil lispbuf-eof)))
                              (when (eq form lispbuf-eof)
                                ;; Move the buffer's input mark to the end of the buffer.
                                (move-mark (region-start input-region)
                                           (region-end input-region))
                                (return))
                              (setq +++ ++ ++ + + - - form)
                              (let ((buffer (current-buffer))
                                    (ok nil))
                                (move-mark
                                 (variable-value 'buffer-input-mark :buffer buffer)
                                 (buffer-point buffer))
                                (unwind-protect
                                     (let ((this-eval
                                             (multiple-value-list
                                              (let ((hi::*debug-on-error* t)
                                                    #+sbcl (sb-ext:*invoke-debugger-hook* nil)
                                                    (*debugger-hook* nil))
                                                (with-nonbroken-debugger
                                                    (eval form))))))
                                       (fresh-line)
                                       (dolist (x this-eval) (prin1 x) (terpri))
                                       (show-prompt)
                                       (setq /// // // / / this-eval)
                                       (setq *** ** ** * * (car this-eval))
                                       (setq ok t))
                                  (unless ok
                                    (fresh-line)
                                    (show-prompt)
                                    (move-mark
                                     (variable-value 'buffer-input-mark :buffer buffer)
                                     (buffer-point buffer)))))))))))))))))))

#+NIL
(defcommand "Abort Eval Input" (p)
  "Move to the end of the buffer and prompt."
  "Move to the end of the buffer and prompt."
  (declare (ignore p))
  (let ((point (current-point)))
    (buffer-end point)
    (insert-character point #\newline)
    (insert-string point "Aborted.")
    (insert-character point #\newline)
    (insert-string point (get-prompt))
    (move-mark (value buffer-input-mark) point)))



;;;; General interactive commands used in eval and typescript buffers.
#+NIL
(defun fresh-prompt (&aux (*standard-output* (value eval-output-stream)))
  (let ((buffer (current-buffer)))
    (show-prompt)
    (move-mark (variable-value 'buffer-input-mark :buffer buffer)
               (buffer-point buffer))))

#+NIL
(defcommand "Clear Eval Buffer" (p)
  "" ""
  (declare (ignore p))
  (let* ((input-region (get-interactive-input))
         (input (if input-region
                    (region-to-string input-region)
                    "")))
    (delete-region (buffer-region (current-buffer)))
    (fresh-prompt)
    (insert-string (buffer-point (current-buffer)) input)))

#+NIL
(defun region-chop-newline (region)
  (let ((end (region-end region)))
    (when (zerop (mark-charpos end))
      (let ((line (line-previous (mark-line end))))
        (move-to-position end (line-length line) line)))
    region))

#+NIL
(defun get-interactive-input (current-window)
  "Tries to return a region.  When the point is not past the input mark, and
   the user has \"Unwedge Interactive Input Confirm\" set, the buffer is
   optionally fixed up, and nil is returned.  Otherwise, an editor error is
   signalled.  When a region is returned, the start is the current buffer's
   input mark, and the end is the current point moved to the end of the buffer."
  (let ((point (current-point))
        (mark (value buffer-input-mark)))
    (cond
     ((mark>= point mark)
      (buffer-end point)
      (let* ((input-region (region mark point))
             (string (region-to-string input-region))
             (ring (value interactive-history)))
        (when (and (or (zerop (ring-length ring))
                       (string/= string (region-to-string (ring-ref ring 0))))
                   (> (length string) (value minimum-interactive-input-length)))
          (ring-push (region-chop-newline (copy-region input-region)) ring))
        input-region))
     ((value unwedge-interactive-input-confirm)
      (beep)
      (when (clim:accept 'y-or-n
                         :current-window current-window
             :prompt (concatenate 'simple-string
                                  "Point not past input mark.  "
                                  (value unwedge-interactive-input-string))
             :must-exist t :default t :default-string "yes")
        (funcall (value unwedge-interactive-input-fun))
        (message "Unwedged."))
      nil)
     (t
      (editor-error "Point not past input mark.")))))

;; fixme: this shouldn't be limited at all
#+NIL
(defhvar "Interactive History Length"
  "This is the length used for the history ring in interactive buffers.
   It must be set before turning on the mode."
  :value 1000)

#+NIL
(defhvar "Minimum Interactive Input Length"
  "When the number of characters in an interactive buffer exceeds this value,
   it is pushed onto the interactive history, otherwise it is lost forever."
  :value 2)

#+NIL
(defvar *previous-input-search-string* "ignore")

#+NIL
(defvar *previous-input-search-pattern*
  ;; Give it a bogus string since you can't give it the empty string.
  (new-search-pattern :string-insensitive :forward "ignore"))

#+NIL
(defun get-previous-input-search-pattern (string)
  (if (string= *previous-input-search-string* string)
      *previous-input-search-pattern*
      (new-search-pattern :string-insensitive :forward
                          (setf *previous-input-search-string* string)
                          *previous-input-search-pattern*)))

#+NIL
(defcommand "Search Previous Interactive Input" (p)
  "Search backward through the interactive history using the current input as
   a search string.  Consecutive invocations repeat the previous search."
  "Search backward through the interactive history using the current input as
   a search string.  Consecutive invocations repeat the previous search."
  (declare (ignore p))
  (let* ((mark (value buffer-input-mark))
         (ring (value interactive-history))
         (point (current-point))
         (just-invoked (eq (last-command-type) :searching-interactive-input)))
    (when (mark<= point mark)
      (editor-error "Point not past input mark."))
    (when (zerop (ring-length ring))
      (editor-error "No previous input in this buffer."))
    (unless just-invoked
      (get-previous-input-search-pattern (region-to-string (region mark point))))
    (let ((found-it (find-previous-input ring just-invoked)))
      (unless found-it
        (editor-error "Couldn't find ~a." *previous-input-search-string*))
      (delete-region (region mark point))
      (insert-region point (ring-ref ring found-it))
      (setf (value searching-interactive-pointer) found-it))
  (setf (last-command-type) :searching-interactive-input)))

#+NIL
(defun find-previous-input (ring againp)
  (let ((ring-length (ring-length ring))
        (base (if againp
                  (+ (value searching-interactive-pointer) 1)
                  0)))
      (loop
        (when (= base ring-length)
          (if againp
              (setf base 0)
              (return nil)))
        (with-mark ((m (region-start (ring-ref ring base))))
          (when (find-pattern buffer m *previous-input-search-pattern*)
            (return base)))
        (incf base))))

#+NIL
(defcommand "Previous Interactive Input" (p)
  "Insert the previous input in an interactive mode (Eval or Typescript).
   If repeated, keep rotating the history.  With prefix argument, rotate
   that many times."
  "Pop the *interactive-history* at the point."
  (let* ((point (current-point))
         (mark (value buffer-input-mark))
         (ring (value interactive-history))
         (length (ring-length ring))
         (p (or p 1)))
    (when (or (mark< point mark) (zerop length)) (editor-error))
    (cond
     ((eq (last-command-type) :interactive-history)
      (let ((base (mod (+ (value interactive-pointer) p) length)))
        (delete-region (region mark point))
        (insert-region point (ring-ref ring base))
        (setf (value interactive-pointer) base)))
     (t
      (let ((base (mod (if (minusp p) p (1- p)) length))
            (region (delete-and-save-region (region mark point))))
        (insert-region point (ring-ref ring base))
        (when (mark/= (region-start region) (region-end region))
          (ring-push region ring)
          (incf base))
        (setf (value interactive-pointer) base)))))
  (setf (last-command-type) :interactive-history))

#+NIL
(defcommand "Next Interactive Input" (p)
  "Rotate the interactive history backwards.  The region is left around the
   inserted text.  With prefix argument, rotate that many times."
  "Call previous-interactive-input-command with negated arg."
  (previous-interactive-input-command (- (or p 1))))
#+NIL
(defcommand "Kill Interactive Input" (p)
  "Kill any input to an interactive mode (Eval or Typescript)."
  "Kill any input to an interactive mode (Eval or Typescript)."
  (declare (ignore p))
  (let ((point (buffer-point (current-buffer)))
        (mark (value buffer-input-mark)))
    (when (mark< point mark) (editor-error))
    (hi::.kill-region. (region mark point) :kill-backward)))

#+NIL
(defcommand "Interactive Beginning of Line" (p)
  "If on line with current prompt, go to after it, otherwise do what
  \"Beginning of Line\" always does."
  "Go to after prompt when on prompt line."
  (let ((mark (value buffer-input-mark))
        (point (current-point)))
    (if (and (same-line-p point mark) (or (not p) (= p 1)))
        (move-mark point mark)
        (beginning-of-line-command p))))

#+NIL
(defcommand "Reenter Interactive Input" (p)
  "Copies the form to the left of point to be after the interactive buffer's
   input mark.  When the current region is active, it is copied instead."
  "Copies the form to the left of point to be after the interactive buffer's
   input mark.  When the current region is active, it is copied instead."
  (declare (ignore p))
  (unless (hemlock-bound-p 'buffer-input-mark)
    (editor-error "Not in an interactive buffer."))
  (let ((point (current-point)))
    (let ((region (if (region-active-p)
                      ;; Copy this, so moving point doesn't affect the region.
                      (copy-region (current-region (current-buffer)))
                      (with-mark ((start point)
                                  (end point))
                        (pre-command-parse-check start)
                        (unless (form-offset start -1)
                          (editor-error "Not after complete form."))
                        (region (copy-mark start) (copy-mark end))))))
      (buffer-end point)
      (push-buffer-mark (copy-mark point))
      (insert-region point region)
      (setf (last-command-type) :ephemerally-active))))



;;; Other stuff.



(defhcommand editor-mode-command (window p)
  (:pretty-name "Editor Mode")
  (:documentation
   "Turn on \"Editor\" mode in the current buffer.  If it is already on, turn it
  off.  When in editor mode, most lisp compilation and evaluation commands
  manipulate the editor process instead of the current eval server.")
  (:help-string "Toggle \"Editor\" mode in the current buffer."))

(defmethod editor-mode-command (window p)
  (declare (ignore p))
  (setf (buffer-minor-mode (window-buffer window) "Editor" (window-buffer window))
        (not (buffer-minor-mode (window-buffer window) "Editor"))))


(define-file-option "Editor" (buffer value)
  (declare (ignore value))
  (setf (buffer-minor-mode buffer "Editor" buffer) t))





(defhcommand editor-compile-defun-command (window p)
  (:pretty-name "Editor Compile Defun")
  (:documentation
   "Compiles the current or next top-level form in the editor Lisp.
   First the form is evaluated, then the result of this evaluation
   is passed to compile.  If the current region is active, this
   compiles the region.")
  (:help-string
   "Evaluates the current or next top-level form in the editor Lisp."))

(defmethod editor-compile-defun-command (window p)
  (let* ((buffer (window-buffer window)))
    (if buffer
        ;;(if (region-active-p buffer)
        ;;  (editor-compile-region window buffer (current-region buffer))
        (if (buffer-minor-mode buffer :lisp-listener)
            (hemlock::interrupt-lisp-command window p)
            (let ((region (defun-region buffer (buffer-point buffer)))
                  (saved (copy-mark (buffer-point buffer))))
              (unwind-protect (progn
                                (move-mark (buffer-point buffer) (region-start region))
                                (push-buffer-mark buffer (region-end region) t)
                                (update-window-image window)
                                (editor-compile-region window buffer region t)
                                (sleep 0.1))
                (move-mark (buffer-point buffer) saved)
                (pop-buffer-mark buffer)
                (update-window-image window)
                (adhoc::adhoc-redefinition-hook))))
        (editor-error "No current buffer"))))

#+NIL
(defcommand "Editor Compile Region" (p)
  "Compiles lisp forms between the point and the mark in the editor Lisp."
  "Compiles lisp forms between the point and the mark in the editor Lisp."
  (declare (ignore p))
  (editor-compile-region (current-region (current-buffer))))

(defun defun-region (buffer mark)
  "This returns a region around the current or next defun with respect to mark.
   Mark is not used to form the region.  If there is no appropriate top level
   form, this signals an editor-error.  This calls PRE-COMMAND-PARSE-CHECK."

    (with-mark ((start mark)
                (end mark))
      (pre-command-parse-check buffer start)
    (cond ((not (mark-top-level-form buffer start end))
           (editor-error "No current or next top level form."))
          (t (region start end)))))

(defun editor-compile-region (current-window buffer region &optional quiet)
  (unless quiet (message "Compiling region ..."))
  (let ((pathname (buffer-pathname buffer)))
    (in-lisp buffer
     (with-input-from-region (stream buffer region)
       (with-pop-up-display (*error-output* :height 19 :current-window current-window)
         ;; JDz: We don't record source locations and what not, but this
         ;; is portable.  CMUCL specific implementation removed because
         ;; it does not work on HEMLOCK-REGION-STREAM (but it can be
         ;; added back later if CMUCL starts using user-extensible
         ;; streams internally.)
         #-scl
         (funcall (compile nil `(lambda ()
                                  ,@(loop for form = (read stream nil stream)
                                      until (eq form stream)
                                      collect form))))
         #+scl
         ;; TODO: add position information to the source-info.
         (c::compile-from-stream stream :source-info pathname)
         )))))

#+NIL
(defcommand "Editor Evaluate Defun" (p)
  "Evaluates the current or next top-level form in the editor Lisp.
   If the current region is active, this evaluates the region."
  "Evaluates the current or next top-level form in the editor Lisp."
  (declare (ignore p))
  (if (region-active-p)
      (editor-evaluate-region-command nil)
      (with-input-from-region (stream (defun-region (current-point)))
        (clear-echo-area)
        (in-lisp
         (message "Editor Evaluation returned ~S"
                  (eval (read stream)))))))

#+NIL
(defcommand "Editor Evaluate Region" (p)
  "Evaluates lisp forms between the point and the mark in the editor Lisp."
  "Evaluates lisp forms between the point and the mark in the editor Lisp."
  (declare (ignore p))
  (with-input-from-region (stream (current-region (current-buffer)))
    (clear-echo-area)
    (write-string "Evaluating region in the editor ..." *echo-area-stream*)
    (finish-output *echo-area-stream*)
    (in-lisp
     (do ((object (read stream nil lispbuf-eof)
                  (read stream nil lispbuf-eof)))
         ((eq object lispbuf-eof))
       (eval object)))
    (message "Evaluation complete.")))

#+NIL
(defcommand "Editor Re-evaluate Defvar" (p)
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound.  This occurs in the editor Lisp."
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound.  This occurs in the editor Lisp."
  (declare (ignore p))
  (with-input-from-region (stream (defun-region (current-point)))
    (clear-echo-area)
    (in-lisp
     (let ((form (read stream)))
       (unless (eq (car form) 'defvar) (editor-error "Not a DEFVAR."))
       (makunbound (cadr form))
       (message "Evaluation returned ~S" (eval form))))))

#+NIL
(defcommand "Editor Macroexpand Expression" (p)
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  (let ((point (buffer-point (current-buffer))))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (with-mark ((end start))
        (unless (form-offset end 1) (editor-error))
        (in-lisp
         (with-pop-up-display (rts)
           (write-string (with-input-from-region (s (region start end))
                           (prin1-to-string (funcall (if p
                                                         'macroexpand
                                                         'macroexpand-1)
                                                     (read s))))
                         rts)))))))
#+NIL
(defcommand "Editor Evaluate Expression" (window p)
  "Prompt for an expression to evaluate in the editor Lisp."
  "Prompt for an expression to evaluate in the editor Lisp."
  (declare (ignore p))
  (in-lisp
   (multiple-value-call #'message "=> ~@{~#[~;~S~:;~S, ~]~}"
                        (eval (clim:accept 'lisp-expression
                                           :current-window window
            :prompt "Editor Eval: "
            :help "Expression to evaluate")))))

#+NIL
(defcommand "Editor Evaluate Buffer" (window p)
  "Evaluates the text in the current buffer in the editor Lisp."
  "Evaluates the text in the current buffer redirecting *Standard-Output* to
   the echo area.  This occurs in the editor Lisp.  The prefix argument is
   ignored."
  (declare (ignore p))
  (clear-echo-area)
  (write-string "Evaluating buffer in the editor ..." *echo-area-stream*)
  (finish-output *echo-area-stream*)
  (with-input-from-region (stream (buffer-region (current-buffer)))
    (let ((*standard-output* *echo-area-stream*))
      (in-lisp
       (do ((object (read stream nil lispbuf-eof)
                    (read stream nil lispbuf-eof)))
           ((eq object lispbuf-eof))
         (eval object))))
    (message "Evaluation complete.")))



;;; With-Output-To-Window  --  Internal
;;;
;;;
(defmacro with-output-to-window ((stream name) &body forms)
  "With-Output-To-Window (Stream Name) {Form}*
  Bind Stream to a stream that writes into the buffer named Name a la
  With-Output-To-Mark.  The buffer is created if it does not exist already
  and a window is created to display the buffer if it is not displayed.
  For the duration of the evaluation this window is made the current window."
  (let ((nam (gensym)) (buffer (gensym)) (point (gensym))
        (window (gensym)) (old-window (gensym)))
    `(let* ((,nam ,name)
            (,buffer (or (getstring ,nam *buffer-names*) (make-buffer ,nam)))
            (,point (buffer-end (buffer-point ,buffer)))
            (,window (or (car (buffer-windows ,buffer)) (make-window ,point)))
            (,old-window (current-window)))
       (unwind-protect
         (progn (setf (current-window) ,window)
                (buffer-end ,point)
                (with-output-to-mark (,stream ,buffer ,point) ,@forms))
         (setf (current-window) ,old-window)))))

#+MIL
(defcommand "Editor Compile File" (window p)
  "Prompts for file to compile in the editor Lisp.  Does not compare source
   and binary write dates.  Does not check any buffer for that file for
   whether the buffer needs to be saved."
  "Prompts for file to compile."
  (declare (ignore p))
  (let ((pn (clim:accept 'file
                         :current-window window :default
                             (buffer-default-pathname (current-buffer))
                             :prompt "File to compile: ")))
    (with-output-to-window (*error-output* "Compiler Warnings")
      (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil)))))


(defun older-or-non-existent-fasl-p (pathname &optional definitely)
  (let ((obj-pn (probe-file (compile-file-pathname pathname))))
    (or definitely
        (not obj-pn)
        (< (file-write-date obj-pn) (file-write-date pathname)))))


(defhcommand editor-compile-buffer-file-command (window p)
  (:pretty-name "Editor Compile Buffer File")
  (:documentation "Compile the file in the current buffer in the editor Lisp if its associated
   binary file (of type .fasl) is older than the source or doesn't exist.  When
   the binary file is up to date, the user is asked if the source should be
   compiled anyway.  When the prefix argument is supplied, compile the file
   without checking the binary file.  When \"Compile Buffer File Confirm\" is
   set, this command will ask for confirmation when it otherwise would not.")
  (:help-string
   "Compile the file in the current buffer in the editor Lisp if the fasl file
   isn't up to date.  When p, always do it."))

(defmethod editor-compile-buffer-file-command (window p)
  (let* ((buffer (window-buffer window))
         (buf buffer)
         (pn (buffer-pathname buf)))
    (macrolet ((with-visual-feedback (() &body body)
                 `(let ((region (buffer-region buffer))
                        (saved (copy-mark (buffer-point buffer))))
                    (unwind-protect (progn
                                      (move-mark (buffer-point buffer) (region-start region))
                                      (push-buffer-mark buffer (region-end region) t)
                                      (update-window-image window)
                                      ,@body
                                      (sleep 0.1))
                      (move-mark (buffer-point buffer) saved)
                      (pop-buffer-mark buffer)
                      (update-window-image window)
                      (adhoc::adhoc-redefinition-hook)))))
      (flet ((do-compile ()
               (with-visual-feedback ()
                 (bt:make-thread
                  #'(lambda ()
                      (let ((hi::*debug-on-error* t))
                        (let* ((original-error-output *error-output*)
                               (*error-output* (climish::make-popup-window)))
                          (let (#+SBCL(sb-ext:*invoke-debugger-hook*
                                        #'(lambda (condition hook-value)
                                            (declare (ignorable hook-value))
                                            (let ((*error-output* original-error-output))
                                              (invoke-debugger condition)))))
                            (format *error-output* "Compiling...~%")
                            (let ((fasl (compile-file (namestring pn))))
                              (finish-output *error-output*)
                              (when fasl
                                (load fasl)
                                (finish-output *error-output*)))))))))))
      (unless pn (editor-error "Buffer has no associated pathname."))
      (cond ((buffer-modified buf)
             (when (or (not (value buf compile-buffer-file-confirm))
                       (clim:accept 'y-or-n
                        window
                        :default t :default-string "Y"
                        :prompt (list "Save and compile file ~A? "
                                      (namestring pn))))
               (write-buffer-file window buf pn)
               (do-compile)
               (clear-echo-area)))
            ((older-or-non-existent-fasl-p pn p)
             (when (or (not (value buf compile-buffer-file-confirm))
                       (clim:accept 'y-or-n
                        window
                        :default t :default-string "Y"
                        :prompt (list "Compile file ~A? " (namestring pn))))
               (do-compile)
               (clear-echo-area)))
            (t (do-compile)
               (clear-echo-area)
               #+OLD(when (or p
                         (clim:accept 'y-or-n
                                      :current-window window
                          :default t :default-string "Y"
                          :prompt
                          "Fasl file up to date, compile source anyway? "))
                 )))))))
#+NIL
(defcommand "Editor Compile Group" (p)
  "Compile each file in the current group which needs it in the editor Lisp.
   If a file has type LISP and there is a curresponding file with type
   FASL which has been written less recently (or it doesn't exit), then
   the file is compiled, with error output directed to the \"Compiler Warnings\"
   buffer.  If a prefix argument is provided, then all the files are compiled.
   All modified files are saved beforehand."
  "Do a Compile-File in each file in the current group that seems to need it
   in the editor Lisp."
  (save-all-files-command ())
  (unless *active-file-group* (editor-error "No active file group."))
  (dolist (file *active-file-group*)
    (when (string-equal (pathname-type file) "lisp")
      (let ((tn (probe-file file)))
        (cond ((not tn)
               (message "File ~A not found." (namestring file)))
              ((older-or-non-existent-fasl-p tn p)
               (with-output-to-window (*error-output* "Compiler Warnings")
                 (in-lisp (compile-file (namestring tn) #+cmu :error-file #+cmu nil)))))))))
#+NIL
(defcommand "List Compile Group" (p)
  "List any files that would be compiled by \"Compile Group\".  All Modified
   files are saved before checking to generate a consistent list."
  "Do a Compile-File in each file in the current group that seems to need it."
  (declare (ignore p))
  (save-all-files-command ())
  (unless *active-file-group* (editor-error "No active file group."))
  (with-pop-up-display (s)
    (write-line "\"Compile Group\" would compile the following files:" s)
    (force-output s)
    (dolist (file *active-file-group*)
      (when (string-equal (pathname-type file) "lisp")
        (let ((tn (probe-file file)))
          (cond ((not tn)
                 (format s "File ~A not found.~%" (namestring file)))
                ((older-or-non-existent-fasl-p tn)
                 (write-line (namestring tn) s)))
          (force-output s))))))
#+NIL
(defhvar "Load Pathname Defaults"
  "The default pathname used by the load command.")

#+NIL
(defcommand "Editor Load File" (window p)
  "Prompt for a file to load into Editor Lisp."
  "Prompt for a file to load into the Editor Lisp."
  (declare (ignore p))
  (let ((name (truename (clim:accept 'file
                                     :current-window window
                         :default
                         (or (value buffer load-pathname-defaults)
                             (buffer-default-pathname (current-buffer)))
                         :prompt "Editor file to load: "
                         :help "The name of the file to load"))))
    (setv buffer load-pathname-defaults name)
    (in-lisp (load name))))



;;;; Lisp documentation stuff.

;;; FUNCTION-TO-DESCRIBE is used in "Editor Describe Function Call" and
;;; "Describe Function Call".
;;;
(defmacro function-to-describe (var error-name)
  `(cond ((not (symbolp ,var))
          (,error-name "~S is not a symbol." ,var))
         ((macro-function ,var))
         ((fboundp ,var)
          #+NIL ;; in ANSI-CL conses can't be functions
          (if (listp (symbol-function ,var))
              ,var
              (symbol-function ,var))
          #-NIL
          (symbol-function ,var) )
         (t
          (,error-name "~S is not a function." ,var))))
#+NIL
(defcommand "Editor Describe Function Call" (p)
  "Describe the most recently typed function name in the editor Lisp."
  "Describe the most recently typed function name in the editor Lisp."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
              (mark2 (current-point)))
    (pre-command-parse-check mark1)
    (unless (backward-up-list mark1) (editor-error))
    (form-offset (move-mark mark2 (mark-after mark1)) 1)
    (with-input-from-region (s (region mark1 mark2))
      (in-lisp
       (let* ((sym (read s))
              (fun (function-to-describe sym editor-error)))
         (with-pop-up-display (*standard-output*)
           (editor-describe-function fun sym)))))))

#+NIL
(defcommand "Editor Describe Symbol" (p)
  "Describe the previous s-expression if it is a symbol in the editor Lisp."
  "Describe the previous s-expression if it is a symbol in the editor Lisp."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
              (mark2 (current-point)))
    (mark-symbol mark1 mark2)
    (with-input-from-region (s (region mark1 mark2))
      (in-lisp
       (let ((thing (read s)))
         (if (symbolp thing)
             (with-pop-up-display (*standard-output*)
               (describe thing))
             (if (and (consp thing)
                      (or (eq (car thing) 'quote)
                          (eq (car thing) 'function))
                      (symbolp (cadr thing)))
                 (with-pop-up-display (*standard-output*)
                   (describe (cadr thing)))
                 (editor-error "~S is not a symbol, or 'symbol, or #'symbol."
                               thing))))))))

;;; MARK-SYMBOL moves mark1 and mark2 around the previous or current symbol.
;;; However, if the marks are immediately before the first constituent char
;;; of the symbol name, we use the next symbol since the marks probably
;;; correspond to the point, and Hemlock's cursor display makes it look like
;;; the point is within the symbol name.  This also tries to ignore :prefix
;;; characters such as quotes, commas, etc.
;;;

(defun mark-symbol (buffer mark1 mark2)
  (pre-command-parse-check buffer mark1)
  (with-mark ((tmark1 mark1)
              (tmark2 mark1))
    (cond ((and (form-offset buffer tmark1 1)
                (form-offset buffer (move-mark tmark2 tmark1) -1)
                (or (mark= mark1 tmark2)
                    (and (find-attribute buffer tmark2 :lisp-syntax
                                         #'(lambda (x) (not (eq x :prefix))))
                         (mark= mark1 tmark2))))
           (form-offset buffer mark2 1))
          (t
           (form-offset buffer mark1 -1)
           (find-attribute buffer mark1 :lisp-syntax
                           #'(lambda (x) (not (eq x :prefix))))
           (form-offset buffer (move-mark mark2 mark1) 1)))))


(defhcommand editor-describe-command (window p)
  (:pretty-name "Editor Describe")
  (:documentation "Call Describe on a Lisp object.
  Prompt for an expression which is evaluated to yield the object.")
  (:help-string "Prompt for an object to describe."))

(defmethod editor-describe-command (window p)
  (declare (ignore p))
  (in-lisp (window-buffer window)
    (let* ((exp (clim:accept 'lisp-expression
                             :current-window window
                             :prompt "Object: "
                             :help "Expression to evaluate to get object to describe."))
          (obj (eval exp)))
     (with-pop-up-display (*standard-output* :current-window window)
       (describe obj)
       #+NIL(climish::rm-redraw-node-recursively window)
       ))))

#+NIL
(defcommand "Filter Region" (window p)
  "Apply a Lisp function to each line of the region.
  An expression is prompted for which should evaluate to a Lisp function
  from a string to a string.  The function must neither modify its argument
  nor modify the return value after it is returned."
  "Call prompt for a function, then call Filter-Region with it and the region."
  (declare (ignore p))
  (let* ((exp (clim:accept 'lisp-expression
                           :current-window window
               :prompt "Function: "
               :help "Expression to evaluate to get function to use as filter."))
         (fun (in-lisp (eval exp)))
         (region (current-region (current-buffer))))
    (check-region-query-size region)
    (let* ((start (copy-mark (region-start region) :left-inserting))
           (end (copy-mark (region-end region) :left-inserting))
           (region (region start end))
           (undo-region (copy-region region)))
      (filter-region fun region)
      (make-region-undo (window-buffer window) :twiddle "Filter Region" region undo-region))))
