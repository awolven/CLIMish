(in-package :climish)

#+SBCL
(declaim (sb-ext:muffle-conditions SB-KERNEL:&OPTIONAL-AND-&KEY-IN-LAMBDA-LIST))

(defvar *style*)

(defclass textual-view () ())

(defmacro regenerate! (object)
  `(eval (list* 'send ,object (send ,object root-path))))


(defmacro maybe-provide (frame object expression)
  `(let ((tag (loop for (type tag) in (input-context ,frame)
		    when (typep ,object type)
		      do (return tag)
		    finally (return nil))))
     (when tag (throw tag ,expression))))

(defmacro maybe-provide-butlast (frame object expression)
  `(let ((tag (loop for (type tag) in (butlast (input-context ,frame))
		    when (typep ,object type)
		      do (return tag)
		    finally (return nil))))
     (when tag (throw tag ,expression))))

(defun make-input-context-clauses (pt-var clauses)
  (let ((new-clauses nil))
    (dolist (clause clauses (nreverse new-clauses))
      (let ((type (first clause))
            (body (rest clause)))
        (push `((presentation-subtypep-1 ,pt-var ',type)
                ,@body)
              new-clauses)))))

(defmacro with-input-context ((frame type &key override) 
                              (&optional object-var type-var event-var options-var)
                              form
                              &body clauses)
  #+Genera (declare (zwei:indentation 0 2 2 4 3 2))
  (let ((ignores nil))
    (when (null object-var)
      (setq object-var '#:object)
      (push object-var ignores))
    (when (null type-var)
      (setq type-var '#:presentation-type)
      (unless clauses (push type-var ignores)))
    (when (null event-var)
      (setq event-var '#:event)
      (push event-var ignores))
    (when (null options-var)
      (setq options-var '#:options)
      (push options-var ignores))
    `(flet ((body-continuation () ,form)
            (context-continuation (,object-var ,type-var ,event-var ,options-var)
              ,@(and ignores `((declare (ignore ,@ignores))))
              (cond ,@(make-input-context-clauses type-var clauses))))
       (declare (dynamic-extent #'body-continuation #'context-continuation))
       (invoke-with-input-context
	,frame
	(expand-presentation-type-abbreviation ,type) ,override
	#'body-continuation #'context-continuation))))

(defvar *default-input-editing-stream-class*
  'standard-input-editing-stream)

(defmacro default-input-stream (stream &optional must-be-variable-macro-name)
  `(cond ((member ,stream '(t nil))
          (setq ,stream '*standard-input*))
         ,@(when must-be-variable-macro-name
             `(((not (and (symbolp ,stream)
                          (not (keywordp ,stream))))
                (warn "The stream argument to ~S, ~S, is invalid.~@
                       This argument must be a variable that can be bound to a new stream."
                      ',must-be-variable-macro-name ,stream)
                (setq ,stream '*standard-input*))))))

(defmacro with-input-editing ((&optional stream
                               &key input-sensitizer initial-contents
                                    (class `*default-input-editing-stream-class*))
                              &body body)
  (declare (sb-ext:muffle-conditions SB-KERNEL:&OPTIONAL-AND-&KEY-IN-LAMBDA-LIST))
  (default-input-stream stream with-input-editing)
  `(flet ((with-input-editing-body (,stream) ,@body))
     (declare (dynamic-extent #'with-input-editing-body))
     (invoke-with-input-editing ,stream #'with-input-editing-body
                                ,class ,input-sensitizer ,initial-contents)))

;; Activation gestures terminate the entire input line.  They are usually
;; non-printing characters such as #\Newline or #\End.
(defvar *activation-gestures* nil)

(defvar *standard-activation-gestures* '(:newline :return :end))

(defmacro with-activation-gestures ((additional-gestures &key override)
                                    &body body &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (when (characterp additional-gestures)        ;yes, we mean CHARACTERP
    (setq additional-gestures `'(,additional-gestures)))
  `(with-stack-list*
       (*activation-gestures*
         ,additional-gestures
         ,(cond ((constantp override #+(or Genera Minima) env)
                 (if (null (eval override #+(or Genera Minima-Developer) env))
                     '*activation-gestures*
                     nil))
                (t
                 `(unless ,override *activation-gestures*))))
     ,@body))

(defun activation-gesture-p (gesture)
  (declare (ignorable gesture))
  #+NOTYET
  (and (not (typep gesture '(or pointer-event noise-string end-of-file-marker)))
       (dolist (set *activation-gestures*)
         (when (if (listp set)
                   (member gesture set
                           :test #'keyboard-event-matches-gesture-name-p)
                   (funcall set gesture))
           (return-from activation-gesture-p t)))))

;; Delimiter gestures terminate a field in an input line.  They are usually
;; printing characters such as #\Space or #\Tab
(defvar *delimiter-gestures* nil)

(defmacro with-delimiter-gestures ((additional-gestures &key override)
                                   &body body &environment env)
  #-(or Genera Minima) (declare (ignore env))
  (when (characterp additional-gestures)        ;yes, we mean CHARACTERP
    (setq additional-gestures `'(,additional-gestures)))
  `(with-stack-list*
       (*delimiter-gestures*
         ,additional-gestures
         ,(cond ((constantp override #+(or Genera Minima) env)
                 (if (null (eval override #+(or Genera Minima-Developer) env))
                     '*delimiter-gestures*
                     nil))
                (t
                 `(unless ,override *delimiter-gestures*))))
     ,@body))

(defun delimiter-gesture-p (gesture)
  (declare (ignorable gesture))
  #+NOTYET
  (and (not (typep gesture '(or pointer-event noise-string end-of-file-marker)))
       (dolist (set *delimiter-gestures*)
         (when (if (listp set)
                   (member gesture set
                           :test #'keyboard-event-matches-gesture-name-p)
                   (funcall set gesture))
           (return-from delimiter-gesture-p t)))))

(defun hemlock::file-kind (pathspec &key follow-symlinks)
  #+os-windows
  (clui::file-kind pathspec :follow-symlinks follow-symlinks)
  #-os-windows
  (osicat::file-kind pathspec :follow-symlinks follow-symlinks))

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
