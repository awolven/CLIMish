(in-package :climish)

;; Used for passing information from ACCEPT through the input editor to here
(defvar *presentation-type-for-yanking* nil)

(defvar *accept-help* nil)

;; OPTIONS is a list of a help type followed by a help string (or a function
;; of two arguments, a stream and the help string so far) A "help type" is
;; either a single keyword (either :TOP-LEVEL-HELP or :SUBHELP), or a list
;; consisting of the type and a suboption (:OVERRIDE, :APPEND, or
;; :ESTABLISH-UNLESS-OVERRIDDEN).
;; Specifying :SUBHELP means "Append to previous subhelp, unless an outer
;; context has established an :OVERRIDE".
;; Specifying (:SUBHELP :APPEND) means append no matter what.
;; Specifying (:SUBHELP :OVERRIDE) means "This is the subhelp, subject to
;; lower-level explicit :APPENDs, unless someone above has already :OVERRIDden us.
;; Specifying (<type> :ESTABLISH-UNLESS-OVERRIDDEN) means "Establish <type>
;; at this level, unless someone above has already established <type>."  It does
;; not imply :APPENDING.
(defmacro with-accept-help (options &body body)
  (check-type options list)
  (assert (every #'listp options))
  (dolist (option options)
    (let* ((option-name-spec (if (symbolp (first option))
                                `(,(first option) :normal)
                                (first option)))
           (option-name (first option-name-spec))
           (option-type (second option-name-spec))
           (option-args (rest option)))
      (check-type option-name (member :top-level-help :subhelp))
      (check-type option-type (member :normal :append :override :establish-unless-overridden))
      (setq body
            `((with-stack-list* (*accept-help*
                                  (list ',option-name-spec ,@option-args) *accept-help*)
                ,@(cond ((eq option-type :override)
                         `((if (assoc (caar *accept-help*) (rest *accept-help*)
                                      :test #'(lambda (a b)
                                                (and (eq (first a) (first b))
                                                     (member :override (rest b)))))
                               (pop *accept-help*)
                               (setq *accept-help*
                                     (cons (first *accept-help*)
                                           (delete ,option-name (rest *accept-help*)
                                                   :test #'(lambda (a b)
                                                             (eq (caar b) a))))))))
                        ((eq option-type :append)
                         nil)
                        ((eq option-type :establish-unless-overridden)
                         `((when (assoc (caaar *accept-help*) (rest *accept-help*)
                                        :key #'first)
                             (pop *accept-help*))))
                        (t
                         `((when (assoc (caar *accept-help*) (rest *accept-help*)
                                        :test #'(lambda (a b)
                                                  (and (eq (first a) (first b))
                                                       (member :override (rest b)))))
                             (pop *accept-help*)))))
                ,@body)))))
  `(progn ,@body))

(defun invoke-with-input-editing (stream continuation class input-sensitizer
                                  &optional initial-contents)
  (cond ((not (stream-supports-input-editing stream))
         (funcall continuation stream))
        ((input-editing-stream-p stream)
         (with-stack-list (context 'input-editor :stream stream)
           (with-input-context (stream context) ()
			       (funcall continuation stream)
	     (t (hi::bell)))))
        (t


         (let ((original-stream stream))
	   ;;(focus-window hi::*echo-area-window*)
	   
	   (funcall continuation stream)
	   #+NIL
           (letf-using-resource (stream input-editing-stream class stream)
             (setf (original-stream-recording-p stream) (stream-recording-p stream))
             (unwind-protect
                 (letf-globally (((stream-input-editor-stream original-stream) stream))
                   (with-output-recording-options (stream :record nil)
                     (initialize-position stream)
                     (cond ((stringp initial-contents)
                            (replace-input stream initial-contents))
                           ((consp initial-contents)
                            (presentation-replace-input
                             stream (first initial-contents) (second initial-contents)
                             +textual-view+)))
                     (with-stack-list (context 'input-editor :stream stream)
                       (loop
                         (catch 'rescan
                           (reset-scan-pointer stream)
                           (handler-bind 
                               ((parse-error
                                 #'(lambda (anerror)
                                     (display-input-editor-error stream anerror))))
                             (return
                               (let #+Genera ((sys:rubout-handler :read)) #-Genera ()
                                    (with-input-context (stream context) ()
                                                        (funcall continuation stream)
                                                        (t (hi::bell)))))))))))
               ;; Need to put the input buffer into the history, if it would
               ;; have gone in anyway.
               ;; What about making presentations out of the stuff in the input buffer?
               ;; The only guy that has *that* level of information is ACCEPT.
               ;; Don't bother to redraw if the buffer is empty.
               (unless (zerop (fill-pointer (input-editor-buffer stream)))
                 (with-output-recording-options (stream :draw nil)
                   (if input-sensitizer
                       (funcall input-sensitizer #'redraw-input-buffer stream)
                     (redraw-input-buffer stream))))))))))

(defun clim:accept (type &rest accept-args
		    &key
		      current-window
		      (view hi::*echo-area-window*)
		      (stream
		       (or (and current-window (application-frame current-window))
			   (krma::main-window clim:*default-frame-manager*)))
		      (default-type type)
		      (history type)
		      must-exist
		      (default nil default-supplied-p)
		      (provide-default nil)
		      default-string
		      prompt
		      help
		      parse-string-tables
		    &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (declare (ignorable current-window
		      view
		   must-exist
		   default
		   default-string
		   prompt
		   help
		   parse-string-tables))
  ;; Allow the arguments to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation type)
    (when expanded
      (when (eq default-type type)
	(setq default-type expansion))
      (when (eq history type)
	(setq history expansion))
      (setq type expansion)))
  (unless (eq default-type type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation default-type)
      (when expanded
	(setq default-type expansion)
	(setq accept-args `(:default-type ,default-type ,@accept-args)))))
  (unless (eq history type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation history)
      (when expanded
	(setq history expansion)
	(setq accept-args `(:history ,history ,@accept-args)))))
  (remf accept-args :stream)
  (let ((insert-default nil))
    (when (and provide-default (null default-supplied-p))
      ;; If the user wants a default, but provided none, go get it from the history
      (let ((history (if (typep history 'basic-history)
			 history
			 (presentation-type-history history))))
	(when history
	  #+NOTYET
	  (let ((element (yank-from-history
                          history
                          :test #'(lambda (element)
                                    (presentation-subtypep
                                     (presentation-history-element-type element)
                                     type)))))
	    (when element
	      (setq default (presentation-history-element-object element)
		    default-supplied-p t
		    insert-default t))))))
    (when default-supplied-p
      ;; Massage the default
      (multiple-value-bind (new-default new-type)
	  (presentation-default-preprocessor default type :default-type default-type)
	(when (or (not (eq default new-default))
		  (not (eq default-type new-type)))
	  (setq default new-default
		default-type (or new-type default-type)
		insert-default t))))
    (when insert-default
      (setq accept-args `(:default ,default :default-type ,default-type ,@accept-args))))
  (apply #'stream-accept stream type accept-args))

(defmethod prompt-for-accept ((stream input-protocol-mixin) type view
			      &rest accept-args
			      &key query-identifier prompt &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (when prompt
    (when (output-stream-p stream)
      (apply #'prompt-for-accept-1 stream type accept-args)))
  query-identifier)

(defun prompt-for-accept-1 (stream type
			    &key (default nil default-supplied-p) (default-type type)
			      prompt (prompt-mode ':normal)
			      (display-default prompt)
			    &allow-other-keys)
  (hi::clear-echo-area)
  (cond ((eq prompt t)
	 (write-string "Enter " stream)
	 (describe-presentation-type type stream 1))
	((not (eq prompt nil))
	 (write-string prompt stream)))
  (when (and display-default default-supplied-p)
    (when prompt (write-string " [" stream))
    (write-string "default " stream)
    (write-string (princ-to-string default) stream)
    #+NOTYET(present prompt default-type :stream stream)
    (when prompt (write-string "]" stream)))
  (when (eq prompt-mode ':normal)
    (when (or prompt (and display-default default-supplied-p))
      (write-string " " stream)))
  (hi::buffer-end (hi::region-start hi::*parse-input-region*) hi::*echo-area-buffer*)
  (redraw-buffer hi::*echo-area-window*))


;;; If the input from stream, starting at location, is a request to use the default
;;; then insert the default and return true
(defun check-for-default (stream location default default-type view)
  (setf (stream-scan-pointer stream) location)
  (loop
    (let ((char (read-event stream)))
      (when (or (not (characterp char))
		(delimiter-gesture-p char)
		(not (whitespace-char-p char)))
	;; If we got some kind of mouse gesture or a delimiter or a
	;; printing character, then put it back and check to see if
	;; we should insert the default.
	(unread-event char :stream stream)
	(setf (stream-scan-pointer stream) location)
	(when (or (null char)
		  (activation-gesture-p char)
		  (delimiter-gesture-p char))
	  ;; Insert the default if we got a character that will terminate
	  ;; the current call to ACCEPT
	  (unless (stream-rescanning-p stream)
	    #+NOTYET
	    (presentation-replace-input stream default default-type view
					:buffer-start location))
	  (return-from check-for-default t))
	(return-from check-for-default nil)))))

(defmethod stream-accept ((stream input-protocol-mixin) type &rest accept-args
			  &key view &allow-other-keys)
  (declare (dynamic-extent accept-args))
  (let* ((stream (encapsulating-stream stream))
	 (query-identifier (apply #'prompt-for-accept
				  stream type view accept-args)))
    (apply #'accept-1 stream type
	   :query-identifier query-identifier
	   accept-args)))

(defun accept-1 (stream type
		 &key
		   current-window
		   (view hi::*echo-area-window*)
		   (default nil default-supplied-p)
		   (default-type type)
		   ((:history history-type) type)
		   (insert-default nil)
		   (replace-input t replace-supplied-p)
		   (activation-gestures nil activation-gestures-p)
		   (additional-activation-gestures nil)
		   (delimiter-gestures nil delimiter-gestures-p)
		   (additional-delimiter-gestures nil)
		   (present-p nil)
		   must-exist
		   default-string
		   help
		   parse-string-tables
		   &allow-other-keys)
  
  ;; Set up the input editing environment
  (let ((the-object nil)
	(the-type nil)
	(the-options nil)
	(stuff nil)
	(activated t)
	(history nil))

    (cond ((typep history-type 'basic-history)
	   (setq history history-type
		 history-type type))
	  (history-type
	   (setq history (presentation-type-history history-type))))

    (let ((hi::*parse-default* default))

    ;; Inside ACCEPTING-VALUES, ACCEPT can turn into PRESENT
    (when present-p
      (return-from accept-1
	#+NOTYET
	(accept-present-default type stream view default default-supplied-p
				present-p query-identifier
				:prompt prompt :active-p active-p)))

    (block input-editing

      (flet ((input-sensitizer (continuation stream)
	       (declare (dynamic-extent continuation))
	       (if (stream-recording-p stream)
		   #+NOTYET
		   (with-output-as-presentation (stream the-object (or the-type type))
		     (funcall continuation stream))
		   (funcall continuation stream))))
	(declare (dynamic-extent #'input-sensitizer))

	(when (and current-window
		   (not (eq current-window view)))
	  (focus-window view))

	(unwind-protect
	     (with-input-editing (stream :input-sensitizer #'input-sensitizer
					 :initial-contents (and insert-default
								default-supplied-p
								(list default default-type)))
	       (let ((start-position (stream-scan-pointer stream)))
		 (with-input-context (stream type)
		     (object presentation-type nil options)
		     (with-activation-gestures ((if activation-gestures-p
						    activation-gestures
						    (or additional-activation-gestures
							*standard-activation-gestures*))
						:override activation-gestures-p)
		       (with-delimiter-gestures ((if delimiter-gestures-p
						     delimiter-gestures
						     additional-delimiter-gestures)
						 :override delimiter-gestures-p)
		  
			 (handler-bind
			     ((parse-error
				#'(lambda (anerror)
				    (declare (ignore anerror))
				    (when (and default-supplied-p
					       (check-for-default stream start-position
								  default default-type
								  view))
				      (setq the-object default
					    the-type default-type)
				      (return-from input-editing))
				    ;; Decline to handle the parse error
				    nil)))
			   (flet ((accept-help (stream action string-so-far)
				    (declare (ignore action string-so-far))
				    (write-string "You are being asked to enter " stream)
				    (describe-presentation-type type stream)
				    (write-char #\. stream)))
			     (declare (dynamic-extent #'accept-help))
			     (with-accept-help
				 (((:top-level-help :establish-unless-overridden)
				   ;; :ESTABLISH-... here because we want (SEQUENCE PATHNAME)'s
				   ;; help, not both (SEQUENCE PATHNAME) and PATHNAME.
				   #'accept-help))
			       ;; Call the presentation type's ACCEPT method
			       (multiple-value-setq (the-object the-type stuff)
				 (let ((*presentation-type-for-yanking* (and history history-type)))
				   (if default-supplied-p
				       (if history
					   (let ((default-element
						   (make-presentation-history-element
						    :object default :type default-type)))
					     (with-default-bound-in-history history default-element
					       (funcall-presentation-generic-function accept
						 type stream view
						 :default default :default-type default-type)))
					   (funcall-presentation-generic-function accept
					     type stream view
					     :default default :default-type default-type))
				       (funcall-presentation-generic-function accept
					 type stream view)))))))))

		   ;; A presentation translator was invoked
		   (t       	    
		    (setq the-object object
			  the-type presentation-type
			  the-options options
			  activated nil)
		    (when (if replace-supplied-p
			      replace-input
			      (getf the-options :echo t))
		      #+NOTYET
		      (presentation-replace-input stream the-object the-type view
						  :buffer-start start-position
						  :query-identifier query-identifier)
		      ;; spr25912 --PnC
		      ;; Windows seems to handle this slightly differently.
		      ;; As a result, when processing a command, if the user
		      ;; does a command-completion (from a menu), the focus
		      ;; doesn't come back to the window.  Furthermore, 
		      ;; trying to read the gesture goes into an infinite loop,
		      ;; because there's nothing there to read.		
		      #+mswindows
		      (when (eql (if (listp the-type)
				     (first the-type)
				     the-type) 
				 'clim:command-name)
			(clim:stream-set-input-focus (encapsulating-stream-stream stream))
			(stream-unread-gesture stream #\space))
		      )))))
	  
	  (when (and current-window
		     (not (eq current-window view)))
	    (focus-window current-window)))))

    ;; The input has been parsed, moused, or defaulted.
    ;; If we are still inside a WITH-INPUT-EDITING at an outer level, leave the
    ;; delimiter in the stream.	 But if this was the top level of input, eat
    ;; the activation gesture instead of leaving it in the stream. Don't eat
    ;; the activation gesture on streams that can't ever support input editing,
    ;; such as string streams.
    ;;--- This is really lousy.	 We need a coherent theory here.
    (when activated
      (when (and (not (input-editing-stream-p stream))
		 (stream-supports-input-editing stream))
	(let ((gesture (read-event stream :timeout 0)))
	  ;;--- For now, just ignore button release events
	  (when (typep gesture 'pointer-button-release-event)
	    (read-event stream :timeout 0)))))
    #+NOTYET
    (when (and history
	       (frame-maintain-presentation-histories *application-frame*)
	       (getf the-options :maintain-history t))
      ;;--- Should this only record stuff that was input via the keyboard?
      (push-history-element history (make-presentation-history-element
				      :object the-object :type (or the-type type))))

    ;; Ensure that there are no stale highlighting boxes lying around if
    ;; we are exiting via keyboard input
    #+NOTYET
    (when (output-recording-stream-p stream)
      (unhighlight-highlighted-presentation stream t))
    (values the-object (or the-type type) the-options))))

#+NOTYET
(with-input-context (stream type)
      ()
    (cond ((eq type 'hemlock::buffer)
	   (apply #'accept-buffer stream accept-args))
	  ((eq type 'hemlock::file)
	   (apply #'accept-file stream accept-args))
	  ((eq type 'hemlock::hemlock-variable)
	   (apply #'accept-hemlock-variable stream accept-args))
	  ((eq type 'hemlock::hemlock-keyword)
	   (apply #'accept-keyword stream accept-args))
	  ((eq type 'cl:integer)
	   (apply #'accept-integer stream accept-args))
	  ((eq type 'hemlock::lisp-expression)
	   (apply #'accept-lisp-expression stream accept-args))
	  ((eq type '3dm.d::vec3)
	   (apply #'accept-vec3 stream accept-args))
	  ((eq type 'cl:string)
	   (apply #'accept-string stream accept-args))
	  ((eq type 'hemlock::url)
	   (apply #'accept-url stream accept-args))
	  ((eq type 'hemlock::yes-or-no)
	   (apply #'accept-yes-or-no stream accept-args))	   
	  ((eq type 'hemlock::y-or-n)
	   (apply #'accept-y-or-n stream accept-args))
	  ((eq type 'hemlock::key)
	   (apply #'accept-key stream accept-args))
	  (t
	   (when current-window (focus-window hi::*echo-area-window*))
	   (let ((hi::*parse-verification-function*
		   #'(lambda (frame string)
		       (declare (ignore frame))
		       (let ((*read-eval* nil))
			 (let ((thing (eval (read-from-string string))))
			   (typep thing type))))))		       
	     (unwind-protect (accept-something stream current-window
					       :prompt (or prompt hi::*parse-prompt*))
	     (when current-window
	       (focus-window current-window)))))))
	   

(defun display-prompt (&optional (prompt hi::*parse-prompt*) foo)
  (declare (ignore foo))
  (hi::clear-echo-area)
  (let ((point (hi::buffer-point *echo-area-buffer*)))
    (cond ((listp prompt)
           (apply #'format hi::*echo-area-stream* prompt)
           (finish-output hi::*echo-area-stream*))
          (t
           (insert-string hi::*echo-area-buffer* point prompt)))
    (climish::redraw-buffer hi::*echo-area-window*)))

(defun buffer-verification-function (frame string)
  (declare (ignorable frame))
  (cond ((string= string "") nil)
        (;; fixme: this probably isn't the right place for this check,
         ;; but COMPLETE-STRING, called below, does something weird on
         ;; strings tokenized into words, and misses exact matches for
         ;; some strings.  So let's check for correct buffer names first,
         ;; before wandering off into the land of incorrect completion
         ;; functions.  The right place to fix things is _probably_ in
         ;; COMPLETE-STRING.
         (let ((buffer (hi::getstring string hi::*buffer-names*)))
           (when buffer
             (list buffer))))
        (hi::*parse-value-must-exist*
         (multiple-value-bind
	       (prefix key value field ambig)
             (hi::complete-string string hi::*parse-string-tables*)
           (declare (ignore field))
           (ecase key
             (:none nil)
             ((:unique :complete)
              (list value))
             (:ambiguous
              (hi::delete-region hi::*echo-area-buffer* hi::*parse-input-region*)
              (hi::insert-string hi::*echo-area-buffer* (hi::region-start hi::*parse-input-region*) prefix)
              (let ((point (buffer-point hi::*echo-area-buffer*)))
                (hi::move-mark point (hi::region-start hi::*parse-input-region*))
                (unless (hi::character-offset hi::*echo-area-buffer* point ambig)
                  (hi::buffer-end point hi::*echo-area-buffer*)))
              nil))))
        (t
         (list (or (hi::getstring string hi::*buffer-names*) string)))))

(defun file-verification-function (frame string)
  (declare (ignorable frame))
  (let ((pn (hi::pathname-or-lose string)))
    (if pn
        (let ((merge
		(cond ((not hi::*parse-default*) nil)
                     ((hi::directoryp pn)
                      (merge-pathnames pn hi::*parse-default*))
                     (t
                      (merge-pathnames pn
                                       (or (directory-namestring
                                            hi::*parse-default*)
                                           ""))))))
          (cond ((probe-file pn) (list pn))
                ((and merge (probe-file merge)) (list merge))
                ((not hi::*parse-value-must-exist*) (list (or merge pn)))
                (t nil))))))

(defun keyword-verification-function (frame string)
  (declare (ignorable frame))
  (multiple-value-bind
	(prefix key value field ambig)
      (hi::complete-string string hi::*parse-string-tables*)
    (declare (ignore field))
    (cond (hi::*parse-value-must-exist*
           (ecase key
             (:none nil)
             ((:unique :complete)
              (list prefix value))
             (:ambiguous
              (hi::delete-region hi::*echo-area-buffer* hi::*parse-input-region*)
              (hi::insert-string hi::*echo-area-buffer* (hi::region-start hi::*parse-input-region*) prefix)
              (let ((point (hi::buffer-point hi::*echo-area-buffer*)))
                (hi::move-mark point (hi::region-start hi::*parse-input-region*))
                (unless (hi::character-offset hi::*echo-area-buffer* point ambig)
                  (hi::buffer-end point hi::*echo-area-buffer*)))
              nil)))
          (t
           ;; HACK: If it doesn't have to exist, and the completion does not
           ;; add anything, then return the completion's capitalization,
           ;; instead of the user's input.
           (list (if (= (length string) (length prefix)) prefix string))))))

(defun echo-area-enter-command (frame)
  (let* ((string (hi::region-to-string hi::*echo-area-buffer* hi::*parse-input-region*))
         (empty (string= string "")))
    (if empty
        (when hi::*parse-default* (setq string hi::*parse-default*))
        (when (or (zerop (hi::ring-length hemlock::*echo-area-history*))
                  (string/= string (hi::ring-ref hemlock::*echo-area-history* 0)))
          (hi::ring-push string hemlock::*echo-area-history*)))
    (multiple-value-bind (res flag)
	(let ((hi::*parse-value-must-exist* t))
	  (funcall hi::*parse-verification-function* frame string))
      (declare (ignore flag))
      ;;(second (first (climish::input-context frame)))
      (unwind-protect (when res
			(throw :accept (first res)))
	(hi::clear-echo-area)))
    (editor-error "invalid input")))

(defun accept-buffer (frame &key (current-window (or (focused-window frame) hi::*echo-area-window*))
			      ((:must-exist hi::*parse-value-must-exist*) t)
			      default
			      ((:default-string hi::*parse-default-string*))
			      ((:prompt hi::*parse-prompt*) "Buffer: ")
			      ((:help hi::*parse-help*) "Type a buffer name."))
  "Prompts for a buffer name and returns the corresponding buffer.  If
   :must-exist is nil, then return the input string.  This refuses to accept
   the empty string as input when no default is supplied.  :default-string
   may be used to supply a default buffer name even when :default is nil, but
   when :must-exist is non-nil, :default-string must be the name of an existing
   buffer."
  (let ((hi::*parse-string-tables* (list hi::*buffer-names*))
	(hi::*parse-type* :keyword)
	(hi::*parse-default*
	  (cond (default (buffer-name default))
		(hi::*parse-default-string*
		 (when (and hi::*parse-value-must-exist*
			    (not (hi::getstring hi::*parse-default-string* hi::*buffer-names*)))
		   (error "Default-string must name an existing ~
buffer when must-exist is non-nil -- ~S."
			  hi::*parse-default-string*))
		 hi::*parse-default-string*)
		(t nil)))
	(hi::*parse-verification-function* #'buffer-verification-function))
    (accept-something frame current-window)))

(defun accept-file (frame &key
			    (current-window (or (focused-window frame) hi::*echo-area-window*))
			    ((:must-exist hi::*parse-value-must-exist*) t)
			    default
			    ((:default-string hi::*parse-default-string*))
			    ((:prompt hi::*parse-prompt*) "Filename: ")
			    ((:help hi::*parse-help*) "Type a file name."))
  "Prompts for a filename."
  (let ((hi::*parse-verification-function* #'file-verification-function)
        (hi::*parse-default* (if default (namestring default)))
        (hi::*parse-type* :file))
    (accept-something frame current-window)))

(defun accept-keyword (frame
		       &key 
			 (current-window (or (focused-window frame) hi::*echo-area-window*))
			 ((:parse-string-tables hi::*parse-string-tables*))
			 ((:must-exist hi::*parse-value-must-exist*) t)
			 ((:default hi::*parse-default*))
			 ((:default-string hi::*parse-default-string*))
			 ((:prompt hi::*parse-prompt*) "Keyword: ")
			 ((:help hi::*parse-help*) "Type a keyword."))
  "Prompts for a keyword using the String Tables."
  (let ((hi::*parse-verification-function*
	  #'keyword-verification-function)
        (hi::*parse-type* :keyword))
    (accept-something frame current-window)))

(defun accept-hemlock-variable (frame
				&key
				  (current-window (or (focused-window frame) hi::*echo-area-window*))
				  ((:must-exist hi::*parse-value-must-exist*) t)
				  ((:default hi::*parse-default*))
				  ((:default-string hi::*parse-default-string*))
				  ((:prompt hi::*parse-prompt*) "Variable: ")
				  ((:help hi::*parse-help*)
				   "Type the name of a variable."))
  "Prompts for a variable defined in the current scheme of things."
  (let ((hi::*parse-string-tables* (hi::current-variable-tables))
        (hi::*parse-verification-function* #'keyword-verification-function)
        (hi::*parse-type* :keyword))
    (accept-something frame current-window)))

(defun accept-integer (frame &key (current-window (or (focused-window frame) hi::*echo-area-window*))
			       ((:must-exist hi::*parse-value-must-exist*) t)
			       default
			       ((:default-string hi::*parse-default-string*))
			       ((:prompt hi::*parse-prompt*) "Integer: ")
			       ((:help hi::*parse-help*) "Type an integer."))
  "Prompt for an integer.  If :must-exist is Nil, then we return as a string
  whatever was input if it is not a valid integer."
  (let ((hi::*parse-verification-function*
         #'(lambda (string)
             (let ((number (parse-integer string  :junk-allowed t)))
               (if hi::*parse-value-must-exist*
                   (if number (list number))
                   (list (or number string))))))
        (hi::*parse-default* (if default (write-to-string default :base 10))))
    (accept-something frame current-window)))

(defun accept-lisp-expression (frame
			       &key (current-window (or (focused-window frame) hi::*echo-area-window*))
				 ((:must-exist hi::*parse-value-must-exist*) t)
				 (default nil defaultp)
				 ((:default-string hi::*parse-default-string*))
				 ((:prompt hi::*parse-prompt*) "Expression: ")
				 ((:help hi::*parse-help*)
				  "Type a Lisp expression."))
  "Prompts for a Lisp expression."
  (let ((hi::*parse-verification-function*
	  #'(lambda (frame string)
	      (declare (ignore frame))
	      (let ((expr (with-input-from-region (stream hi::*echo-area-buffer* hi:*parse-input-region*)
			    (handler-case (let ((*package* (find-package :climish)))
					    (read stream nil hi::hemlock-eof))
			      (error () hi::hemlock-eof)))))
               (if hi::*parse-value-must-exist*
                   (if (not (eq expr hi::hemlock-eof)) (values (list expr) t))
                   (if (eq expr hi::hemlock-eof)
                       (list string) (values (list expr) t))))))
        (hi::*parse-default* (if defaultp (prin1-to-string default))))
    (accept-something frame current-window)))

(defun accept-vec3 (frame
		    &key (current-window (or (focused-window frame) hi::*echo-area-window*))
		      ((:must-exist hi::*parse-value-must-exist*) t)
		      (default nil defaultp)
		      ((:default-string hi::*parse-default-string*))
		      ((:prompt hi::*parse-prompt*) "Expression: ")
		      ((:help hi::*parse-help*)
		       "Type a Lisp expression."))
  "Prompts for a Lisp expression."
  (let ((hi::*parse-verification-function*
	  #'(lambda (frame string)
	      (declare (ignore frame))
	      (let* ((expr (with-input-from-region (stream hi::*echo-area-buffer* hi:*parse-input-region*)
			     (handler-case (let ((*package* (find-package :climish)))
					     (read stream nil hi::hemlock-eof))
			       (error () hi::hemlock-eof))))
		     (thing (eval expr)))
		(if (typep thing '3dm.d::vec3)
		    (values (list thing) t)
		    (values string nil)))))
        (hi::*parse-default* (if defaultp (prin1-to-string default))))
    (accept-something frame current-window)))

(defun accept-string (frame
		      &key (current-window (or (focused-window frame) hi::*echo-area-window*))
			((:default hi::*parse-default*))
			((:default-string hi::*parse-default-string*))
			(trim ())
			((:prompt hi::*parse-prompt*) "String: ")
			((:help hi::*parse-help*) "Type a string."))
  "Prompts for a string.  If :trim is t, then leading and trailing whitespace
   is removed from input, otherwise it is interpreted as a Char-Bag argument
   to String-Trim."
  (let ((hi::*parse-verification-function*
	  #'(lambda (frame string)
	      (declare (ignore frame))
	      (list (string-trim (if (eq trim t)
				     '(#\space #\tab) trim)
				 string)))))
    (accept-something frame current-window)))

(defun accept-url (frame
		   &key (current-window (or (focused-window frame) hi::*echo-area-window*))
		     ((:default hi::*parse-default*))
		     ((:default-string hi::*parse-default-string*))
		     (trim ())
		     ((:prompt hi::*parse-prompt*) "URL: ")
		     ((:help hi::*parse-help*) "Type a URL."))
  "Prompts for a URL.  If :trim is t, then leading and trailing whitespace
   is removed from input, otherwise it is interpreted as a Char-Bag argument
   to String-Trim.  Since URL syntax is hard to validate, any input is
   accepted as if by PROMPT-FOR-STRING.  The prompt is different, however."
  (let ((hi::*parse-verification-function*
	  #'(lambda (frame string)
	      (declare (ignore frame))
	      (list (string-trim (if (eq trim t)
				     '(#\space #\tab) trim)
				 string)))))
    (accept-something frame current-window)))

(defun accept-yes-or-no (frame 
			 &key (current-window (or (focused-window frame) hi::*echo-area-window*))
			   ((:must-exist hi::*parse-value-must-exist*) t)
			   (default nil defaultp)
			   ((:default-string hi::*parse-default-string*))
			   ((:prompt hi::*parse-prompt*) "Yes or No? ")
			   ((:help hi::*parse-help*) "Type Yes or No."))
  "Prompts for Yes or No."
  (let* ((hi::*parse-string-tables* (list hi::*yes-or-no-string-table*))
         (hi::*parse-default* (if defaultp (if default "Yes" "No")))
         (hi::*parse-verification-function*
	   #'(lambda (frame string)
	       (declare (ignore frame))
	       (multiple-value-bind
		     (prefix key value field ambig)
		   (hi::complete-string string hi::*parse-string-tables*)
		 (declare (ignore prefix field ambig))
		 (let ((won (or (eq key :complete) (eq key :unique))))
		   (if hi::*parse-value-must-exist*
		       (if won (values (list value) t))
		       (list (if won (values value t) string)))))))
         (hi::*parse-type* :keyword))
    (accept-something frame current-window)))

(defun accept-y-or-n (frame
		      &key (current-window (or (focused-window frame) hi::*echo-area-window*))
			((:must-exist must-exist) t)
			(default nil defaultp)
			default-string
			((:prompt prompt) "Y or N? ")
			((:help hi::*parse-help*) "Type Y or N."))
  "Prompts for Y or N."
  (climish::focus-window hi::*echo-area-window*)
  (unwind-protect
       (progn
	 (display-prompt prompt (or default-string
				    (if defaultp (if default "Y" "N"))))
	 (loop
	   (let ((key-event (input-event-id (read-event frame))))
	     (unless (eq key-event #k"MouseMove")
	       (cond ((or (eq key-event #k"y")
			  (eq key-event #k"Y"))
		      (return t))
		     ((or (eq key-event #k"n")
			  (eq key-event #k"N"))
		      (return nil))
		     ((logical-key-event-p key-event :confirm)
		      (if defaultp
			  (return default)
			  (hi::bell)))
		     ((logical-key-event-p key-event :help)
		      (hemlock::help-on-parse-command current-window ()))
		     (t
		      (unless must-exist (return key-event))
		      (hi::bell)))))))
    (setf (climish::focused-window frame) current-window)))

(defun accept-key (frame &key
			   (current-window (or (focused-window frame) hi::*echo-area-window*))
			   ((:must-exist must-exist) t)
			   default default-string
			   (prompt "Key: ")
			   ((:help hi::*parse-help*) "Type a key."))
  ;; fix me.
  (let ((string (if default
                    (or default-string
                        (let ((l (coerce default 'list)))
                          (format nil "~:C~{ ~:C~}" (car l) (cdr l)))))))
    (focus-window hi::*echo-area-window*)
    (unwind-protect
         (progn
           (display-prompt prompt string)
           (setf (fill-pointer hi::*prompt-key*) 0)
           (prog ((key hi::*prompt-key*) key-event)
              (declare (vector key))
	    TOP
              (setf key-event (input-event-id (read-event frame)))
              (cond ((logical-key-event-p key-event :quote)
                     (setf key-event (input-event-id (read-event frame))))
                    ((logical-key-event-p key-event :confirm)
                     (cond ((and default (zerop (length key)))
                            (let ((res (gethash default (command-table frame))))
                              (unless (typep res 'command) (go FLAME))
                              (return (values default res))))
                           ((and (not must-exist) (plusp (length key)))
                            (return (copy-seq key)))
                           (t
                            (go FLAME))))
                    ((logical-key-event-p key-event :help)
                     (hemlock::help-on-parse-command current-window ())
                     (go TOP)))
              (vector-push-extend key-event key)
              (when must-exist
                (let ((res (gethash default (command-table frame))))
                  (cond ((typep res 'command)
                         (climish::print-pretty-input-event-id key-event hi::*echo-area-stream*)
                         (write-char #\space hi::*echo-area-stream*)
                         (force-output hi::*echo-area-stream*)
                         (return (values (copy-seq key) res)))
                        ((not (eq res :prefix))
                         (vector-pop key)
                         (go FLAME)))))
	      (climish::print-pretty-input-event-id key-event hi::*echo-area-stream*)
              (write-char #\space hi::*echo-area-stream*)
              (force-output hi::*echo-area-stream*)
              (go TOP)
              FLAME
              (hi::bell)
              (go TOP)))
      (force-output hi::*echo-area-stream*)
      (setf (climish::focused-window frame) current-window))))
