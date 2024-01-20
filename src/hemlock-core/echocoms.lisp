;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Echo area commands.
;;;
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
(in-package :hemlock)




;;; Field separator characters separate fields for TOPS-20 ^F style
;;; completion.



;;; Find-All-Completions  --  Internal
;;;
;;;    Return as a list of all the possible completions of String in the
;;; list of string-tables Tables.
;;;
(defun find-all-completions (string tables)
  (do ((table tables (cdr table))
       (res ()
            (merge 'list (find-ambiguous string (car table))
                   res #'string-lessp)))
      ((null table) res)))

(defun echo-area-mode-p (frame)
  (or (null (climish::focused-window frame))
      (eq (climish::focused-window frame) *echo-area-window*)))

(defhcommand help-on-parse-command (window p)
  (:pretty-name "Help on Parse")
  (:documentation "Display help for parse in progress.
  If there are a limited number of options then display them.")
  (:help-string "Display the *Parse-Help* and any possibly completions of the current
  input.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod help-on-parse-command (window p)
  (declare (ignore p))
  (let ((help (typecase *parse-help*
                (list (unless *parse-help* (error "There is no parse help."))
                      (apply #'format nil *parse-help*))
                (string *parse-help*)
                (t (error "Parse help is not a string or list: ~S" *parse-help*))))
        (input (region-to-string *echo-area-buffer* *parse-input-region*)))
    (cond
     ((eq *parse-type* :keyword)
      (let ((strings (find-all-completions input *parse-string-tables*)))
        (with-pop-up-display (s :height (+ (length strings) 2) :current-window window)
          (write-line help s)
          (finish-output s)
          (cond (strings
                 (write-line "Possible completions of what you have typed:" s)
                 (dolist (string strings)
                   (write-line string s)))
                (t
                 (write-line
 "There are no possible completions of what you have typed." s))))))
     ((and (eq *parse-type* :file) (not (zerop (length input))))
      (let ((pns (ambiguous-files (region-to-string *echo-area-buffer* *parse-input-region*)
                                  *parse-default*)))
        (declare (list pns))
        (with-pop-up-display(s :height (+ (length pns) 2) :current-window window)
          (write-line help s)
          (cond (pns
                 (write-line "Possible completions of what you have typed:" s)
                 (let ((width (- (window-width (current-window)) 27)))
                   (dolist (pn pns)
                     (let* ((dir (directory-namestring pn))
                            (len (length dir)))
                       (unless (<= len width)
                         (let ((slash (position #\/ dir
                                                :start (+ (- len width) 3))))
                           (setf dir
                                 (if slash
                                     (concatenate 'string "..."
                                                  (subseq dir slash))
                                     "..."))))
                       (format s " ~A~25T ~A~%"
                               (file-namestring pn) dir)))))
                (t
                 (write-line
 "There are no possible completions of what you have typed." s))))))
     (t
      (with-mark ((m (buffer-start-mark *echo-area-buffer*) :left-inserting))
        (insert-string *echo-area-buffer* m help)
        (insert-character *echo-area-buffer* m #\newline))))))

(defun file-completion-action (typein)
  (declare (simple-string typein))
  (when (zerop (length typein)) (editor-error))
  (multiple-value-bind
      (result win)
      (hemlock-ext::complete-file typein
                                 :defaults (directory-namestring *parse-default*)
                                 :ignore-types (value *echo-area-buffer* ignore-file-types))
    (when result
      (delete-region *echo-area-buffer* *parse-input-region*)
      (insert-string *echo-area-buffer* (region-start *parse-input-region*)
                     (namestring result))
      (climish::redraw-buffer *echo-area-window*))
    #+NIL
    (when (and (not win) (value *echo-area-buffer* beep-on-ambiguity))
      (editor-error))))

(defhcommand complete-keyword-command (window p)
  (:pretty-name "Complete Keyword")
  (:documentation
   "Trys to complete the text being read in the echo area as a string in
  *parse-string-tables*")
  (:help-string
   "Complete the keyword being parsed as far as possible.
  If it is ambiguous and ``Beep On Ambiguity'' true beep.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod complete-keyword-command (window p)
  (declare (ignore p))
  (let ((typein (region-to-string *echo-area-buffer* *parse-input-region*)))
    (declare (simple-string typein))
    (case *parse-type*
      (:keyword
       (multiple-value-bind
           (prefix key value field ambig)
           (complete-string typein *parse-string-tables*)
         (declare (ignore value field))
         (when prefix
           (delete-region *echo-area-buffer* *parse-input-region*)
           (insert-string *echo-area-buffer* (region-start *parse-input-region*) prefix)
           (when (eq key :ambiguous)
             (let ((point (buffer-point *echo-area-buffer*)))
               (move-mark point (region-start *parse-input-region*))
               (unless (character-offset *echo-area-buffer* point ambig)
                 (buffer-end point *echo-area-buffer*)))))
         (when (and (or (eq key :ambiguous) (eq key :none))
                    (value *echo-area-buffer* beep-on-ambiguity))
           (editor-error))))
      (:file
       (file-completion-action typein))
      (t
       (editor-error "Cannot complete input for this prompt.")))))

(defun field-separator-p (x)
  (plusp (character-attribute :parse-field-separator x)))

(defhcommand complete-field-command (window p)
  (:pretty-name "Complete Field")
  (:documentation
   "Complete a field in a parse.
  Fields are defined by the :field separator attribute,
  the text being read in the echo area as a string in *parse-string-tables*")
  (:help-string
   "Complete a field in a keyword.
  If it is ambiguous and ``Beep On Ambiguity'' true beep.  Fields are
  separated by characters having a non-zero :parse-field-separator attribute,
  and this command should only be bound to characters having that attribute.")  
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod complete-field-command (window p)
  (let ((typein (region-to-string *echo-area-buffer* *parse-input-region*)))
    (declare (simple-string typein))
    (case *parse-type*
      (:string
       (self-insert-command window p))
      (:file
       (file-completion-action typein))
      (:keyword
       (let ((spacep
              ;; due to the use of spaces in command names, let's special
              ;; case on spaces here: The space key both completes and
              ;; self inserts, where other keys (like tab) only complete.
              (eql (hemlock-ext:key-event-char *last-key-event-typed*)
                   #\space)))
         (when spacep
           (let ((point (buffer-point *echo-area-buffer*)))
             (unless (blank-after-p *echo-area-buffer* point)
               (insert-character *echo-area-buffer* point #\space))))
         (multiple-value-bind
               (prefix key value field ambig)
             (complete-string typein *parse-string-tables*)
           (declare (ignore value ambig))
           (when (eq key :none) (editor-error "No possible completion."))
           (delete-region *echo-area-buffer* *parse-input-region*)
           (let ((new-typein (if (and spacep (or field (not (eq key :unique))))
                                 (concatenate 'string
                                              (subseq prefix 0 field)
                                              " ")
                                 (subseq prefix 0 field))))
             (insert-string *echo-area-buffer* (region-start *parse-input-region*) new-typein)))))
      (t
       (editor-error "Cannot complete input for this prompt.")))))




#+NOMORE-MOVED
(defhcommand confirm-parse-command (window p)
  (:pretty-name "Confirm Parse")
  (:documenation
   "Terminate echo-area input.
  If the input is invalid then an editor-error will signalled.")
  (:help-string
   "If no input has been given, exits the recursive edit with the default,
  otherwise calls the verification function."))

#+NOMORE-MOVED
(defmethod confirm-parse-command (window p)
  (declare (ignore p))
  (let* ((string (region-to-string *echo-area-buffer* *parse-input-region*))
         (empty (zerop (length string))))
    (declare (simple-string string))
    (if empty
        (when *parse-default* (setq string *parse-default*))
        (when (or (zerop (ring-length *echo-area-history*))
                  (string/= string (ring-ref *echo-area-history* 0)))
          (ring-push string *echo-area-history*)))
    (multiple-value-bind (res flag)
                         (funcall *parse-verification-function* string)
      (unless (or res flag) (editor-error))
      (exit-recursive-edit res))))

(defhcommand previous-parse-command (window p)
  (:pretty-name "Previous Parse")
  (:documentation
   "Rotate the echo-area history forward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input.")
  (:help-string
   "Pop the *echo-area-history* ring buffer.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod previous-parse-command (window p)
  (let ((length (ring-length *echo-area-history*))
        (p (or p 1)))
    (when (zerop length) (editor-error))
    (cond
     ((eq (last-command-type) :echo-history)
      (let ((base (mod (+ *echo-history-pointer* p) length)))
        (delete-region *echo-area-buffer* *parse-input-region*)
        (insert-string *echo-area-buffer* (region-end *parse-input-region*)
                       (ring-ref *echo-area-history* base))
        (setq *echo-history-pointer* base)))
     (t
      (let ((current (region-to-string *echo-area-buffer* *parse-input-region*))
            (base (mod (if (minusp p) p (1- p)) length)))
        (delete-region *echo-area-buffer* *parse-input-region*)
        (insert-string *echo-area-buffer* (region-end *parse-input-region*)
                       (ring-ref *echo-area-history* base))
        (when (and (plusp (length current))
                   (string/= (ring-ref *echo-area-history* 0) current))
          (ring-push current *echo-area-history*)
          (incf base))
        (setq *echo-history-pointer* base))))
    (setf (last-command-type) :echo-history)))

(defhcommand next-parse-command (window p)
  (:pretty-name "Next Parse")
  (:documentation
   "Rotate the echo-area history backward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input.")
  (:help-string "Push the *echo-area-history* ring buffer.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod next-parse-command (window p)
  (previous-parse-command window (- (or p 1))))

(defhcommand illegal-command (window p)
  (:pretty-name "Illegal")
  (:documentation "This signals an editor-error.
  It is useful for making commands locally unbound.")
  (:help-string "Just signals an editor-error.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod illegal-command (window p)
  (declare (ignore p))
  (editor-error))



(defhcommand beginning-of-parse-command (window p)
  (:pretty-name "Beginning Of Parse")
  (:documentation
   "Moves to immediately after the prompt when in the echo area.")
  (:help-string
   "Move the point of the echo area buffer to *parse-starting-mark*.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod beginning-of-parse-command (window p)
  (declare (ignore p))
  (move-mark (buffer-point *echo-area-buffer*) *parse-starting-mark*))

(defhcommand echo-area-delete-previous-character-command (window p)
  (:pretty-name "Echo Area Delete Previous Character")
  (:documentation "Delete the previous character.
  Don't let the luser rub out the prompt.")
  (:help-string
   "Signal an editor-error if we would nuke the prompt,
  otherwise do a normal delete.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod echo-area-delete-previous-character-command (window p)
  (with-mark ((tem (buffer-point *echo-area-buffer*)))
    (unless (character-offset *echo-area-buffer* tem (- (or p 1))) (editor-error))
    (when (mark< tem *parse-starting-mark*) (editor-error))
    (delete-previous-character-command window p)))

(defhcommand echo-area-kill-previous-word-command (window p)
  (:pretty-name "Echo Area Kill Previous Word")
  (:help-string "Kill the previous word.
  Don't let the luser rub out the prompt.")
  (:documentation
   "Signal an editor-error if we would mangle the prompt, otherwise
  do a normal kill-previous-word.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod echo-area-kill-previous-word-command (window p)
  (with-mark ((tem (buffer-point *echo-area-buffer*)))
    (unless (word-offset tem (- (or p 1))) (editor-error))
    (when (mark< tem *parse-starting-mark*) (editor-error))
    (kill-previous-word-command window p)))

(declaim (special *kill-ring*))

(defhcommand kill-parse-command (window p)
  (:pretty-name "Kill Parse")
  (:documenation "Kills any input so far.")
  (:help-string "Kills *parse-input-region*.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod kill-parse-command (window p)
  (declare (ignore p))
  (if (end-line-p *echo-area-buffer* (buffer-point *echo-area-buffer*))
      (kill-region *echo-area-buffer* *parse-input-region* :kill-backward)
      (ring-push (delete-and-save-region *echo-area-buffer* *parse-input-region*)
                 *kill-ring*)))

(defhcommand insert-default-parse-command (window p)
  (:pretty-name "Insert Parse Default")
  (:documentation
   "Inserts the default for the parse in progress.
  The text is inserted at the point.")
  (:help-string
   "Inserts *parse-default* at the point of the *echo-area-buffer*.
  If there is no default an editor-error is signalled.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod insert-default-parse-command (window p)
  (declare (ignore p))
  (unless *parse-default* (editor-error))
  (insert-string *echo-area-buffer* (buffer-point *echo-area-buffer*) *parse-default*))

(defhcommand echo-area-backward-character-command (window p)
  (:pretty-name "Echo Area Backward Character")
  (:documentation "Go back one character.
  Don't let the luser move into the prompt.")
  (:help-string
   "Signal an editor-error if we try to go into the prompt, otherwise
  do a backward-character command.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod echo-area-backward-character-command (window p)
  (backward-character-command window p)
  (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
    (beginning-of-parse-command window ())
    (editor-error)))

(defhcommand echo-area-backward-word-command (window p)
  (:pretty-name "Echo Area Backward Word")
  (:documentation "Go back one word.
  Don't let the luser move into the prompt.")
  (:help-string
   "Signal an editor-error if we try to go into the prompt, otherwise
  do a backward-word command.")
  (:applicability (echo-area-mode-p climish::application-frame)))

(defmethod echo-area-backward-word-command (window p)
  (backward-word-command window p)
  (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
    (beginning-of-parse-command window ())
    (editor-error)))
