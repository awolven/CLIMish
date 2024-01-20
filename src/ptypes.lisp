;; ***********************************************************************
;;
;; Copyright (c) Franz, Inc.
;; Portions Copyright (c) 1990-1992 Symbolics, Inc.
;; Portions Copyright (c) 1988-1990 International Lisp Associates
;; Portions Copyright (c) 1991-1992 by Lucid, Inc.
;; Portions Copyright (c) 1989-1990 by Xerox Corporation
;; Portions Copyright (c) 1990-1992 by BBN Systems and Technologies
;; Portions Copyright (c) 1989-1991 Sun Microsystems, Inc.
;; Portions Copyright (c) 1985-1986 Bigelow & Holmes
;; Portions Copyright (c) 1986 Regents of the University of California
;; All rights reserved.

;; Redistribution and use in source and binary forms are permitted
;; provided that the above copyright notice and this paragraph are
;; duplicated in all such forms and that any documentation, advertising
;; materials, and other materials related to such distribution and use
;; acknowledge that the software was developed by the organizations
;; listed above. The name of the organizations may not be used to endorse
;; or promote products derived from this software without specific prior
;; written permission.  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE.

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(in-package :climish)



(defvar *keyword-package* (find-package :keyword))

(defclass presentation-type-class (standard-class) ())

(defmethod validate-superclass ((class presentation-type-class)
				(superclass standard-class))
  t)

(defmethod validate-superclass ((class presentation-type-class)
				(superclass funcallable-standard-class))
  t)

;;; Presentation generic functions have their own class just so we can define
;;; one method that aids in implementing presentation-method-combination
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass presentation-generic-function
    (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))
)

(defvar *presentation-type-being-defined* nil)

(defvar *presentation-type-class-table* (make-hash-table))

;;; Keyed by name, yields the function that translates one type specifier into another
(defvar *presentation-type-abbreviation-table* (make-hash-table))

;;; A list of names of presentation types whose parameters are themselves
;;; presentation types, for example, OR or SEQUENCE.
(defvar *presentation-type-parameters-are-types* nil)

;;; These hash tables are keyed by the presentation type class if it's a type
;;; or the name if it's an abbreviation, and yield up the indicated information.
;;; We don't bother putting an entry in the table if the information is nil.
;;; We use hash tables instead of property lists to improve virtual memory locality.
;;; This probably incurs no significant space cost.
(defvar *presentation-type-parameters-table* (make-hash-table))
(defvar *presentation-type-options-table* (make-hash-table))

;;; Keyed by the presentation type class, yield description or history if it
;;; was specified in the define-presentation-type rather than by a method
(defvar *presentation-type-description-table* (make-hash-table))
(defvar *presentation-type-history-table* (make-hash-table))

;;; This hash table is keyed by the class and yields the supertype names
;;; and the parameters and options massagers.
;;; The massagers are inlined in combined methods and map-over methods.
;;; One-element lists get condensed to save space.
(defvar *presentation-type-inheritance-table* (make-hash-table))

;;; This hash table is keyed by the presentation function name and yields
;;; a list (generic-function-name . lambda-list), where the symbols type,
;;; type-key, type-class, parameters, and options are special in lambda-list.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *presentation-generic-function-table* (make-hash-table))
)

;;; This list is not in the same form as the :options option,
;;; but is just a list of keywords.  Standard presentation options default
;;; to NIL, do not create variable bindings, and are not viewspec choices.
(defparameter *standard-presentation-options* '(:description))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun decode-once-only-arguments (variables)
  (let ((vars nil)
	(env nil)
	(vl variables))
    (loop
      (when (null vl)
	(return-from decode-once-only-arguments
	  (values (nreverse vars) env)))
      (let ((var (pop vl)))
	(if (eq var '&environment)
	    (setq env (pop vl))
	    (push var vars))))))
)

(defun ignore-arglist (arglist)
  (flet ((lambda-list-element-compare (element and-option)
	   (and (atom element)
		(string= element and-option))))
    `(progn ,@(let ((args nil))
		(dolist (arg arglist)
		  ;; These various &keys may be in some other package.
		  (cond ((member arg '(&rest &downward-rest &key &allow-other-keys &optional)
				 :test #'lambda-list-element-compare)
			 nil)
			(t (push (cond ((atom arg) arg)
				       ((atom (car arg)) (car arg))
				       (t (cadar arg)))
				 args)
			   (when (and (consp arg) (consp (cdr arg)) (consp (cddr arg)))
			     (push (third arg) args)))))
		(nreverse args))
	    nil)))

;;; Use a lambda-list to extract arguments from a list and bind variables.
;;; This "should" signal an error if the list doesn't match the lambda-list.
;;; In implementations that have DESTRUCTURING-BIND, the easiest thing is to use it.
;;; We're actually not using the destructuring part, just the ability to match
;;; a lambda-list to a list of arguments without making a closure and using APPLY.
;;; ANSI CL has DESTRUCTURING-BIND.  Cloe and Lucid have it.
;;; Genera has it in both SYMBOLICS-COMMON-LISP and FUTURE-COMMON-LISP,
;;; but the CLIM-LISP package doesn't use either of those.
;;; Last time I checked Franz did not have it
(defmacro bind-to-list (lambda-list list &body body)
  (cond ((not (constantp list))
	 #+Genera `(scl:destructuring-bind ,lambda-list ,list
		     ,(ignore-arglist lambda-list)
		     ,@body)
	 #+Cloe-Runtime `(cloe:destructuring-bind ,lambda-list ,list
			   ,(ignore-arglist lambda-list)
			   ,@body)
	 #+Minima `(destructuring-bind ,lambda-list ,list
		     ,(ignore-arglist lambda-list)
		     ,@body)
	 #+Lucid `(lucid-common-lisp:destructuring-bind ,lambda-list ,list
		    ,(ignore-arglist lambda-list)
		    ,@body)
 	 #+allegro `(destructuring-bind ,lambda-list ,list
 		      ,(ignore-arglist lambda-list)
 		      ,@body)
	 ;; For the other systems, I guess we'll just give up and do it the slow way
	 #-(or Genera Cloe-Runtime Minima Lucid allegro)
	 `(flet ((bind-to-list-body ,lambda-list
		   ,@(when (member '&rest lambda-list)
		       `((declare (dynamic-extent ,(second (member '&rest lambda-list))))))
		   ,(ignore-arglist lambda-list)
		   ,@body))
	    (declare (dynamic-extent #'bind-to-list-body))
	    (apply #'bind-to-list-body ,list)))
	(t
	 ;; This special case supposedly comes up a lot, but I think it never comes up
	 ;; This optimization plays fast and loose with order of evaluation issues
	 ;; for the default value forms in the lambda-list
	 (setq list (eval list))
	 `(symbol-macrolet
	    ,(do ((item)
		  (result nil)
		  (mode nil))
		 ((null lambda-list) (nreverse result))
	       (setq item (pop lambda-list))
	       (cond ((member item '(&optional &rest &key &aux))
		      (setq mode item))
		     ((member item lambda-list-keywords))
		     ((eq mode '&rest)
		      (push `(,item ',list) result))
		     ((eq mode '&key)
		      (multiple-value-bind (variable default supplied-p)
			  (if (atom item) (values item nil nil)
			    (values (if (atom (car item)) (car item) (cadar item))
				    (second item) (third item)))
			(do ((l list (cddr l))
			     (k (parameter-specifier-keyword item)))
			    ((null l)
			     (push `(,variable ,default) result)
			     (when supplied-p
			       (push `(,supplied-p 'nil) result)))
			  (when (eq (first l) k)
			    (push `(,variable ',(second l)) result)
			    (when supplied-p
			      (push `(,supplied-p 't) result))
			    (return)))))
		     (t
		      (multiple-value-bind (variable default supplied-p)
			  (if (atom item) (values item nil nil)
			    (values (first item) (second item) (third item)))
			(cond ((null list)
			       (push `(,variable ,default) result)
			       (when supplied-p
				 (push `(,supplied-p 'nil) result)))
			      (t
			       (push `(,variable ',(pop list)) result)
			       (when supplied-p
				 (push `(,supplied-p 't) result))))))))
	    ,@body))))

(defmethod presentation-type-class-p ((class presentation-type-class)) t)
(defmethod presentation-type-class-p ((class class)) nil)

(defmethod acceptable-presentation-type-class ((class presentation-type-class)) t)
(defmethod acceptable-presentation-type-class ((class class)) t)
(defmethod acceptable-presentation-type-class ((class built-in-class)) nil)
(defmethod acceptable-presentation-type-class ((class (eql (find-class 't)))) t)
(defmethod acceptable-presentation-type-class ((class t)) nil)

(defun find-class-prototype (class)
  ;; Finalization is necessary according to AMOP. -smh 18may93
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (class-prototype class))

(defun find-class-precedence-list (class)
  ;; Do this to ensure that the class is finalized.  This prevents us
  ;; from consing the CPL over and over again in degenerate cases that
  ;; can come up with structure classes
  #+Symbolics (find-class-prototype class)
  ;; Finalization is necessary according to AMOP. -smh 18may93
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (class-precedence-list class))

;;;; Operations on presentation type specifiers

;;; A presentation type specifier matches one of these three patterns:
;;;   name
;;;   (name parameters...)
;;;   ((name parameters...) options...)
;;; The name is a symbol or a class object (but not a built-in class)

(deftype presentation-type-specifier ()
  `(satisfies presentation-type-specifier-p))

(progn
(defmacro with-stack-list ((var &rest elements) &body body)
  `(let ((,var (list ,@elements)))
     (declare (ignorable ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  `(let ((,var (list* ,@elements)))
     (declare (ignorable ,var))
     ,@body))

;; Since with-stack-list does nothing, this doesn't either.
;; When stack-consing works for non-Genera/Cloe, make this do something.
(defmacro evacuate-list (list)
  `,list)
)

;;;; Presentation Generic Functions


(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Define a presentation generic function, for which presentation methods
;;; can be defined.
(defmacro define-presentation-generic-function (generic-function-name
                                                presentation-function-name
                                                lambda-list &rest options)
  (check-type presentation-function-name symbol)        ;SETFs don't work, I'm afraid
  (unless (member 'type lambda-list)
    (error "The parameter named ~S is missing from the lambda-list for ~S, ~S"
           'type generic-function-name lambda-list))
  ;; The first three parameters must be type-key or type-class, parameters, and options,
  ;; except that parameters and/or options can be omitted.  If the first parameter
  ;; is type-class, then presentation type methods don't inherit -- this allows not
  ;; defining a method to mean use the default (even if a supertype defines a method).
  (unless (member (first lambda-list) '(type-key type-class))
    (error "The first parameter in the lambda-list for ~S must be ~S or ~S, not ~S"
           generic-function-name 'type-key 'type-class (first lambda-list)))
  (when (member 'parameters (cddr lambda-list))
    (error "The parameter named ~S is misplaced in the lambda-list for ~S, ~S"
           'parameters generic-function-name lambda-list))
  (when (member 'options (nthcdr (if (eq (second lambda-list) 'parameters) 3 2) lambda-list))
    (error "The parameter named ~S is misplaced in the lambda-list for ~S, ~S"
           'options generic-function-name lambda-list))
  `(progn
     (setf (gethash ',presentation-function-name *presentation-generic-function-table*)
           `(,',generic-function-name ,@',lambda-list))
     (defgeneric ,generic-function-name ,lambda-list
       #-CCL-2
       (:generic-function-class presentation-generic-function)
       #+CLIM-extends-CLOS
       (:method-combination presentation-method-combination)
       ,@options)))

;;;; Definitions of Presentation Generic Functions

(define-presentation-generic-function map-over-presentation-type-supertypes-method
                                      map-over-presentation-type-supertypes
  (type-class parameters options type function))

(define-presentation-generic-function present-method
                                      present
  (type-key parameters options object type stream view
            &key &allow-other-keys)
            ;; &key &allow-other-keys allows methods to receive only the keyword arguments
            ;; that they are interested in.  We know the caller will never supply
  ;; misspelled keywords since only the CLIM system calls this.
  )

(define-presentation-generic-function accept-method
                                      accept
  (type-key parameters options type stream view
            &key &allow-other-keys)
            ;; &key &allow-other-keys allows methods to receive only the keyword arguments
            ;; that they are interested in.  We know the caller will never supply
  ;; misspelled keywords since only the CLIM system calls this.
  )

(define-presentation-generic-function describe-presentation-type-method
                                      describe-presentation-type
  (type-key parameters options type stream plural-count))

(define-presentation-generic-function presentation-typep-method
                                      presentation-typep
  (type-key parameters object type))

(define-presentation-generic-function presentation-subtypep-method
                                      presentation-subtypep
  (type-key type putative-supertype))

(define-presentation-generic-function presentation-type-history-method
                                      presentation-type-history
  (type-key parameters type))

(define-presentation-generic-function presentation-default-preprocessor-method
                                      presentation-default-preprocessor
  (type-key default type &key &allow-other-keys)
            ;; &key &allow-other-keys allows methods to receive only the keyword arguments
            ;; that they are interested in.  We know the caller will never supply
  ;; misspelled keywords since only the CLIM system calls this.
  )

(define-presentation-generic-function presentation-type-specifier-p-method
                                      presentation-type-specifier-p
  (type-key parameters options type))

(define-presentation-generic-function accept-present-default-method
                                      accept-present-default
  (type-key parameters options type
            stream view default default-supplied-p
            present-p query-identifier
            &key &allow-other-keys))

(define-presentation-generic-function gadget-includes-prompt-p-method
                                      gadget-includes-prompt-p
  (type-key parameters options type stream view))

(define-presentation-generic-function decode-indirect-view-method
                                      decode-indirect-view
  (type-key parameters options type view frame-manager &key &allow-other-keys))

(define-presentation-generic-function presentation-refined-position-test-method
                                      presentation-refined-position-test
  (type-key parameters options type
            record x y))

(define-presentation-generic-function highlight-presentation-method
                                      highlight-presentation
  (type-key parameters options type
            record stream state))
)

;;; ONCE-ONLY does the same thing as it does in zetalisp.  I should have just
;;; lifted it from there but I am honest.  Not only that but this one is
;;; written in Common Lisp.  I feel a lot like bootstrapping, or maybe more
;;; like rebuilding Rome.
;; Variables can have &ENVIRONMENT ENV in it
(defmacro once-only (variables &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (multiple-value-bind (variables #+(or Genera Minima) environment-var)
	(decode-once-only-arguments variables)
      (dolist (var variables)
	(push `(if (or (constantp ,var #+(or Genera Minima) ,environment-var)
		       (symbolp ,var))
		   ,var
		   (let ((,gensym-var (gensym)))
		     (push ,gensym-var ,run-time-vars)
		     (push ,var ,run-time-vals)
		     ,gensym-var))
	      expand-time-val-forms))
      `(let* (,run-time-vars
	      ,run-time-vals
	      (wrapped-body
		(let ,(mapcar #'list variables (reverse expand-time-val-forms))
		  ,@body)))
	 `(let ,(mapcar #'list (reverse ,run-time-vars)
			       (reverse ,run-time-vals))
	    ,wrapped-body)))))

;;; Decode a presentation type specifier into its name, parameters, and options
;;; Any of the three variables can be NIL meaning not to bother with that component
(defmacro with-presentation-type-decoded ((name-var &optional parameters-var options-var)
                                          type &body body)
  (once-only (type)
    `(multiple-value-bind (,@(and name-var `(,name-var))
                           ,@(and parameters-var `(,parameters-var))
                           ,@(and options-var `(,options-var)))
         (cond ((atom ,type)
                (values ,@(and name-var `(,type))
                        ,@(and parameters-var `(nil))
                        ,@(and options-var `(nil))))
               ((atom (car ,type))
                (values ,@(and name-var `((car ,type)))
                        ,@(and parameters-var `((cdr ,type)))
                        ,@(and options-var `(nil))))
               (t
                (values ,@(and name-var `((caar ,type)))
                        ,@(and parameters-var `((cdar ,type)))
                        ,@(and options-var `((cdr ,type))))))
       ,@body)))

;;; Bind the parameter variables of this presentation type to the
;;; parameter values in the given presentation type specifier
(defmacro with-presentation-type-parameters ((type-name type)
                                             &body body &environment env)
  (let ((lambda-list (asterisk-default (presentation-type-parameters type-name env)))
        (parameters '#:parameters))
    `(with-presentation-type-decoded (nil ,parameters) ,type
       (bind-to-list ,lambda-list ,parameters
         ,@body))))

;;; The call side of argument massaging
;;; Note that the presentation-type-specifier is evaluated multiple times
(defmacro call-presentation-generic-function (presentation-function-name &body arguments)
  (let* ((apply (when (eq presentation-function-name 'apply)
                  (setq presentation-function-name (pop arguments))
                  t))
         (info (or (gethash presentation-function-name *presentation-generic-function-table*)
                   (error "~S is not the name of a presentation function that supports presentation methods."
                          presentation-function-name)))
         (generic-function-name (car info))
         (generic-lambda-list (cdr info))
         (type-name-var '#:type-name)
         (parameters-var (and (member 'parameters generic-lambda-list) '#:parameters))
         (options-var (and (member 'options generic-lambda-list) '#:options))
         ;; Position in arguments of the type parameter
         (type-pos (- (position 'type generic-lambda-list)
                      (+ 1 (if parameters-var 1 0) (if options-var 1 0))))
         (presentation-type-specifier (nth type-pos arguments)))
    `(with-presentation-type-decoded (,type-name-var ,parameters-var ,options-var)
                                     ,presentation-type-specifier
       (,@(if apply
              `(apply #',generic-function-name)
              `(,generic-function-name))
        ,(ecase (second info)
           (type-key `(find-class-prototype (find-presentation-type-class ,type-name-var)))
           (type-class `(find-presentation-type-class ,type-name-var)))
        ,@(when parameters-var `(,parameters-var))
        ,@(when options-var `(,options-var))
        ,@arguments))))

(defmacro funcall-presentation-generic-function (presentation-function-name &body arguments)
  `(call-presentation-generic-function ,presentation-function-name ,@arguments))

(defmacro apply-presentation-generic-function (presentation-function-name &body arguments)
  `(call-presentation-generic-function apply ,presentation-function-name ,@arguments))



;;; True if object is a valid presentation type specifier
(defun presentation-type-specifier-p (object &optional environment)
  (with-presentation-type-decoded (type-name parameters-given options-given) object
    (when (typep type-name '(or symbol (satisfies acceptable-presentation-type-class)))
      (let ((class (find-presentation-type-class type-name nil environment)))
        (when class
          ;; The type-name is okay, validate the parameters and options
          (let ((parameters-allowed (presentation-type-parameters class environment))
                (options-allowed (presentation-type-options class environment)))
            (let ((parameters-allowed parameters-allowed)
                  (options-allowed options-allowed)
                  (parameters-given parameters-given))
              (do ((item (car parameters-allowed) (car parameters-allowed))
                   (optional nil))
                  ((null parameters-allowed)
                   (when parameters-given
                     ;; Too many parameters
                     (return-from presentation-type-specifier-p nil)))
                (cond ((member item lambda-list-keywords)
                       (case item
                         (&optional (setq optional t))
                         (&rest (return))
                         (&key
                           (pop parameters-allowed)
                           (unless (member '&allow-other-keys parameters-allowed)
                             (do* ((l parameters-given (cddr l))
                                   (kwd (car l) (car l)))
                                  ((null l))
                               (unless (member kwd parameters-allowed
                                               :key #'parameter-specifier-keyword)
                                 (return-from presentation-type-specifier-p nil))))
                           (return))))
                      (parameters-given
                       (pop parameters-given))
                      ((not optional)
                       ;; Too few parameters
                       (return-from presentation-type-specifier-p nil)))
                (pop parameters-allowed))
              (do* ((l options-given (cddr l))
                    (kwd (car l) (car l)))
                   ((null l))
                (or (member kwd *standard-presentation-options*)
                    (member kwd options-allowed :key #'parameter-specifier-keyword)
                    (return-from presentation-type-specifier-p nil))))
            (if (and (or parameters-allowed options-allowed)
                     ;; If the class exists but we can't get its prototype, which tends to
                     ;; happen during compile-file, then we can't call its method so we have
                     ;; to just assume that the parameters are correct.  I don't like it
                     ;; but there really is no other choice.
                     (handler-case
                         (class-prototype class)
                       (error () nil)))
                ;; Do any additional user-defined validation
                ;; Must trap errors because wrong number of parameters might signal
                ;; an error inside bind-to-list
                ;; Must use class rather than type-name so the right environment gets used
                (handler-case
                    (with-stack-list* (class-and-parameters class parameters-given)
                      (with-stack-list* (type-specifier class-and-parameters options-given)
                        (funcall-presentation-generic-function
                          presentation-type-specifier-p type-specifier)))
                  (error () nil))
                ;; If no parameters nor options, no need for user-defined validation
                t)))))))

;;; Find the options list of the presentation type or abbreviation named name
;;; This does not include the standard options unless the define-presentation-type
;;; mentioned them explicitly
(defun presentation-type-options (name &optional environment)
  (let ((class (if (symbolp name) (find-presentation-type-class name nil environment) name)))
    (cond ((and *presentation-type-being-defined*
                (or (eq name (first *presentation-type-being-defined*))
                    (eq class (second *presentation-type-being-defined*))))
           (fourth *presentation-type-being-defined*))
          ((and class
                (compile-file-environment-p environment)
                (compile-time-property class 'presentation-type-inheritance))
           (compile-time-property class 'presentation-type-options))
          (class (gethash class *presentation-type-options-table*))
          (t (gethash name *presentation-type-options-table*)))))



;;; Bind the option variables of this presentation type to the
;;; option values in the given presentation type specifier
(defmacro with-presentation-type-options ((type-name type)
                                          &body body &environment env)
  (let ((lambda-list (lambda-list-from-options (presentation-type-options type-name env)))
        (options '#:options))
    `(with-presentation-type-decoded (nil nil ,options) ,type
       (bind-to-list ,lambda-list ,options
		     ,@body))))

(defun presentation-type-name (type)
  (with-presentation-type-decoded (name) type
    name))




;;; True if value appears anywhere in tree, which is not really a tree because
;;; we only look in cars, not in cdrs, and we look inside vectors too.
;;; But I couldn't think of a better name.
(defun tree-member (value tree)
  (or (eq value tree)
      (and (typep tree 'sequence)
           (some #'(lambda (elt) (tree-member value elt)) tree))))

;;; The only portable way to figure out what a backquote form does is to
;;; evaluate it with "dummy" arguments, which of course places some
;;; restrictions on what backquote forms are valid to use.  They can't
;;; contain conditionals nor depend on the types of their arguments.
;;; The arguments are a form and some lambda-lists that define its inputs.
;;; The values are the result of evaluating the form with dummy bindings for
;;; the variables in the lambda-lists, and two alists from variables to dummy values,
;;; corresponding to the two lambda-lists.
;;; The second and third values only contain entries for variables whose dummy value
;;; actually was incorporated into the result.
;;; We use uninterned symbols as the dummy values.
(defun meta-evaluate-form (form lambda-list &optional another-lambda-list)
  (let ((bindings nil) (more-bindings nil))
    (labels ((do-lambda-list (lambda-list)
               (dolist (item lambda-list)
                 (cond ((member item lambda-list-keywords))
                       ((atom item) (do-variable item))
                       (t (do-variable (if (atom (car item)) (car item) (cadar item)))
                          (when (and (consp (cdr item)) (consp (cddr item)) (third item))
                            (do-variable (third item)))))))
             (do-variable (variable)
               (push (cons variable (make-symbol (symbol-name variable))) bindings)))
      (do-lambda-list another-lambda-list)
      (setq more-bindings bindings
            bindings nil)
      (do-lambda-list lambda-list))
    (let* ((x `(let ,(mapcar #'(lambda (binding) `(,(car binding) ',(cdr binding)))
                             (append bindings more-bindings))
                 ,form))
           (result (handler-case (eval x)
                     (error (anerror)
                       (warn "Error while meta-evaluating the form ~S:~%~A" form anerror)
                       nil))))
      (labels ((prune (bindings)
                 (delete-if-not #'(lambda (binding) (tree-member (cdr binding) result))
                                bindings)))
        (values result (prune bindings) (prune more-bindings))))))

;;;; Describe

;; Returns the plural for the specified string
(defun string-pluralize (string)
  (let ((length (length string))
        (pos (1+ (or (string-search-set '(#\Space #\Tab) string :from-end t) -1))))
    (when (zerop length)
      (return-from string-pluralize string))
    (let* ((flush nil)
           (suffix nil)
           (last-char (aref string (1- length)))
           (penult-char (if (> length 1) (aref string (- length 2)) #\*)))
      (cond ((and (char-equal last-char #\y)
                  (not (find penult-char '(#\a #\e #\i #\o #\u) :test #'char-equal)))
             (setq flush 1
                   suffix "ies"))
            ((or (string-equal string "ox"  :start1 pos)
                 (string-equal string "vax" :start1 pos))
             (setq suffix "en"))
            ((or (and (char-equal last-char #\h)
                      (find penult-char '(#\c #\s) :test #'char-equal))
                 (find last-char '(#\s #\z #\x) :test #'char-equal))
             (setq suffix "es"))
            ((and (>= length 3)
                  (string-equal string "man" :start1 (- length 3))
                  (not (string-equal string "human" :start1 pos)))
             (setq flush 2
                   suffix "en"))
            ((and (>= length 3)
                  (string-equal string "ife" :start1 (- length 3)))
             (setq flush 2
                   suffix "ves"))
            ((and (>= length 5)
                  (string-equal string "child" :start1 (- length 5)))
             (setq suffix "ren"))
            (t (setq suffix "s")))
      (concatenate 'string
        (if flush (subseq string 0 (- length flush)) string)
        suffix))))

;; Returns an article to be used with the specified string
(defun string-a-or-an (string)
  (let* ((length (length string))
         (char (and (not (zerop length))
                    (char string 0))))
    (when (zerop length)
      (return-from string-a-or-an ""))
    (if (digit-char-p char)
        ;; Pronounce leading digits number!
        (string-a-or-an (format nil "~:R" (parse-integer string :junk-allowed t)))
        (cond ((or (string-equal string "one")
                   (and (>= length 4)
                        (string-equal string "one " :end1 4)))
               "a ")
              ((or (= length 1)                        ;"an x", but "a xylophone"
                   ;; "an fff" but "a frog"
                   (not (string-search-set "aeiou" string))
                   ;; "an xl400" but "a xylophone"
                   (string-search-set "0123456789" string))
               (if (find char "aefhilmnorsx" :test #'char-equal) "an " "a "))
              (t
               (if (or (find char "aio" :test #'char-equal)
                       ;; "an egg", but "a eunich"
                       (and (char-equal char #\e)
                            (not (string-equal string "eu" :end1 2)))
                       ;; "an umbrella", but "a unicorn"
                       ;; "a uniform", but "an uninformed ..."
                       ;; And of course, "a unix".  We admittedly heuristicate
                       (and (char-equal char #\u)
                            (not (and (string-equal string "uni" :end1 3)
                                      (or (< length 5)
                                          (not (find (char string 4)
                                                     ;; Treat "y" as a vowel here.
                                                     ;; e.g., "unicycle"
                                                     "bcdfghjklmnpqrstvwxz"
                                                     :test #'char-equal)))))))
                   "an "
                   "a "))))))

(defun string-search-set (char-set string &key (start 0) end from-end (test #'char-equal))
  (let (#+(or Genera Minima) (string string))
    (declare (type vector string))
    (unless end
      (setq end (length string)))
    (if from-end
        (do ((i (1- end) (1- i)))
            ((< i start) nil)
          (when (find (aref string i) char-set :test test)
            (return i)))
        (do ((i start (1+ i)))
            ((>= i end) nil)
          (when (find (aref string i) char-set :test test)
            (return i))))))

;;; The default way of pluralizing a presentation type
;;; description is a string that names a single member of the type
(defun default-describe-presentation-type (description stream plural-count)
  (setq description (string description))        ;can be a symbol sometimes
  (etypecase plural-count
    ((member nil)
     (write-string description stream)
     (return-from default-describe-presentation-type nil))
    ((member 1)
     (write-string (string-a-or-an description) stream)
     (write-string description stream)
     (return-from default-describe-presentation-type nil))
    ((member t))
    ((integer 2 *)
     (format stream "~R " plural-count)))
  (write-string (string-pluralize description) stream))

;;; Like macroexpand-1
(defun expand-presentation-type-abbreviation-1 (type &optional environment)
  (with-presentation-type-decoded (name parameters options) type
    (let ((expander (or (and (compile-file-environment-p environment)
                             (compile-time-property name 'presentation-type-abbreviation))
                        (gethash name *presentation-type-abbreviation-table*))))
      (cond (expander
             (setq type (funcall expander type))
             ;; If it didn't pass the standard presentation options through, add them
             (with-presentation-type-decoded (new-name new-parameters new-options) type
               (dolist (keyword *standard-presentation-options*)
                 (do ((l options (cddr l)))
                     ((null l))
                   (when (eq (car l) keyword)
                     (unless (do ((l new-options (cddr l)))
                                 ((null l) nil)
                               (when (eq (car l) keyword)
                                 (return t)))
                       (setq new-options `(,keyword ,(cadr l) ,@new-options)
                             type nil)))))
               (values (or type
                           `((,new-name ,@new-parameters) ,@new-options))
                       t)))
            ((member name *presentation-type-parameters-are-types*)
             (let* ((any-expanded nil)
                    (expansions
                      (mapcar #'(lambda (type)
                                  (multiple-value-bind (expansion expanded)
                                      (expand-presentation-type-abbreviation-1
                                        type environment)
                                    (when expanded (setq any-expanded t))
                                    expansion))
                              parameters)))
               (if any-expanded
                   (values `((,name ,@expansions) ,@options) t)
                   (values type nil))))
            (t
             (values type nil))))))

;;; Like macroexpand
(defun expand-presentation-type-abbreviation (type &optional environment)
  (let ((flag nil))
    (loop
      (multiple-value-bind (expansion expanded)
          (expand-presentation-type-abbreviation-1 type environment)
        (unless expanded
          (unless (compile-file-environment-p environment)
            (check-type type presentation-type-specifier))
          (return-from expand-presentation-type-abbreviation
            (values type flag)))
        (setq type expansion
              flag t)))))

;;; Output a brief description of type to stream
(defun describe-presentation-type (presentation-type
                                   &optional (stream *standard-output*) (plural-count 1))
  (if (null stream)
      (with-output-to-string (stream)
        (describe-presentation-type presentation-type stream plural-count))
      (let ((type (expand-presentation-type-abbreviation presentation-type)))
        (with-presentation-type-decoded (nil nil options) type
          (let ((description (getf options :description)))
            (if description
                (default-describe-presentation-type description stream plural-count)
                (funcall-presentation-generic-function describe-presentation-type
                  type stream plural-count)))))))

(defun make-presentation-type-history (type &key (maximum-length *default-history-length*)
                                                 history-name)
  (let* ((type-name (presentation-type-name type))
         (history-name (or history-name
                           ;; check that type-name itself is a valid presentation type
                           ;; before calling describe-presentation-type (cim 11/17/95)
                           (and (presentation-type-specifier-p type-name)
                                (describe-presentation-type type-name nil nil)))))
    (make-instance 'presentation-history
      :name history-name :type type-name
      :maximum-length maximum-length)))

;;; Insert DEFTYPE-style defaulting of unsupplied arguments to *
(defun asterisk-default (lambda-list)
  (do* ((lambda-list lambda-list (cdr lambda-list))
        (item (first lambda-list) (first lambda-list))
        (result nil)
        (mode nil))
       ((null lambda-list) (nreverse result))
    (cond ((member item '(&optional &rest &key &aux))
           (setq mode item))
          ((member item lambda-list-keywords))
          ((not (member mode '(&optional &key))))
          ((atom item)
           (setq item `(,item '*)))
          ((null (cdr item))
           (setq item `(,(car item) '*))))
    (push item result)))

;;; Remove the extra stuff that isn't part of a lambda-list,
;;; and add &KEY and &ALLOW-OTHER-KEYS to make it a lambda-list.
;;; Do not add the standard presentation options as there is no need
;;; to bind variables to them, and no need to check the option keywords
;;; at all here, since PRESENTATION-TYPE-SPECIFIER-P does that.
(defun lambda-list-from-options (options)
  `(&key ,@(mapcar #'(lambda (item)
                       (if (and (consp item) (consp (cdr item)) (consp (cddr item)))
                           (subseq item 0 (if (null (third item)) 2 3))
                           item))
                   options)
	 
         &allow-other-keys))

(defun compile-file-environment-p (environment)
  (declare (ignore environment))
  nil)

(defun find-class-that-works (name &optional (errorp t) environment)
  (if (compile-file-environment-p environment)
      (find-class name nil environment)
      (find-class name errorp environment)))

(defvar *compile-time-property-table* (make-hash-table))

(defun compile-time-property (symbol indicator &optional default)
  (let ((table (gethash indicator *compile-time-property-table*)))
    (unless table
      (setf (gethash indicator *compile-time-property-table*)
	    (setq table (make-hash-table))))
    (values (gethash symbol table default))))

(defun set-compile-time-property (symbol indicator value)
  (let ((table (gethash indicator *compile-time-property-table*)))
    (unless table
      (setf (gethash indicator *compile-time-property-table*)
	    (setq table (make-hash-table))))
    (setf (gethash symbol table) value))
  value)

(defsetf compile-time-property set-compile-time-property)





(defun class-proper-name (class &optional environment)
  (let ((name (class-name class)))
    (if (and name (symbolp name)
             (or (eq (find-class-that-works name nil environment) class)
                 ;; If there is a forward-referenced compile-time class
                 ;; that corresponds to a class in the current load-time
                 ;; environment, just use the name of the load-time class.
                 ;; This often happens with STANDARD-OBJECT.
                 ;;--- The forces the assumption that the compile-time
                 ;;--- classes are not radically different from the load-time
                 ;;--- classes, in effect papering over that problem.
                 (eq (find-class-that-works name nil) class)))
        name
	class)))







;;; Analyze the :inherit-from backquote form to get the lists of direct supertype
;;; names, parameter massaging forms, and option massaging forms,
;;; and to check for violation of the separation of parameters and options.
;;; This requires that the backquote form be a simple substitution of variables
;;; into positions in a fixed structure, not conditional and not involve
;;; computation on the values of the variables.
;;; The values are lists of equal length and parallel contents, except that
;;; if the length is 1, each value is just the list element, to save space in
;;; compiled files.
(defun analyze-inherit-from (backquote parameters-ll options-ll)
  (multiple-value-bind (expansion parameters-bindings options-bindings)
      (meta-evaluate-form backquote parameters-ll options-ll)
    (with-presentation-type-decoded (expanded-name expanded-parameters expanded-options)
                                    expansion
      ;; Generate the "massager" that translates the parameters or options of a
      ;; type into the parameters or options of a direct supertype.  It is a form that
      ;; can be evaluated inside the bindings offered by parameters-ll or options-ll.
      ;; This also checks for violation of the separation of parameters and options.
      (flet ((generate-massager (expansion bindings bad-bindings
                                 good-word bad-word supertype-name)
               (let ((bad-variables nil))
                 (labels ((backquotify (object)
                            (cond ((car (rassoc object bindings)))
                                  ((rassoc object bad-bindings)
                                   (pushnew (car (rassoc object bad-bindings)) bad-variables)
                                   nil)
                                  ((consp object)
                                   (multiple-value-bind (forms tail)
                                       (do ((l object (cdr l))
                                            (forms nil (cons (backquotify (car l)) forms)))
                                           ((atom l)
                                            (when l (push (backquotify l) forms))
                                            (values (nreverse forms) l)))
                                     (if (every #'constantp forms)
                                         (list 'quote object)
                                         (cons (if tail 'list* 'list) forms))))
                                  ((and (vectorp object) (not (stringp object)))
                                   (let ((forms (map 'list #'backquotify object)))
                                     (if (every #'constantp forms)
                                         object
                                         (cons 'vector forms))))
                                  ((and (symbolp object) (not (keywordp object))
                                        (not (eq object nil)) (not (eq object t)))
                                   (list 'quote object))
                                  (t object))))
                   (prog1 (backquotify expansion)
                          (when bad-variables
                            (setq bad-variables (sort bad-variables #'string<))
                            (warn "The presentation type ~A~P ~{~S~^, ~} ~
                                   appear~A in the ~As of the supertype ~S."
                                  bad-word (length bad-variables) bad-variables
                                  (if (cdr bad-variables) "" "s") good-word supertype-name))
                          )))))
        (case expanded-name
          ((or)
           (warn "OR is not supported in :INHERIT-FROM")
           (values nil nil nil))
          ((and)
           ;; Multiple inheritance -- first get the direct supertype names
           (let ((direct-supertypes
                   (mapcar #'(lambda (type)
                               (with-presentation-type-decoded (expanded-name) type
                                 (when (member expanded-name '(and or not satisfies))
                                   (warn "AND in :INHERIT-FROM only supports classes, not ~S"
                                         type))
                                 expanded-name))
                           expanded-parameters)))
             (when expanded-options
               (warn "AND in :INHERIT-FROM does not support options."))
             ;; Generate the inheritance information for each supertype
             (values direct-supertypes
                     (mapcar #'(lambda (supertype)
                                 (with-presentation-type-decoded (name parameters) supertype
                                   (generate-massager parameters parameters-bindings
                                                      options-bindings
                                                      "parameter" "option" name)))
                             expanded-parameters)
                     (mapcar #'(lambda (supertype)
                                 (with-presentation-type-decoded (name nil options) supertype
                                   (generate-massager options options-bindings
                                                      parameters-bindings
                                                      "option" "parameter" name)))
                             expanded-parameters))))
          (otherwise
            ;; Single inheritance is a bit simpler
            (values expanded-name
                    (generate-massager expanded-parameters parameters-bindings
                                       options-bindings
                                       "parameter" "option" expanded-name)
                    (generate-massager expanded-options options-bindings
                                       parameters-bindings
                                       "option" "parameter" expanded-name))))))))

;;; Load-time and compile-time effect of define-presentation-type.
;;; The compile-time effects are dropped after compile-file.
(defun ensure-presentation-type (name parameters options direct-supertypes
                                 description history
                                 parameters-are-types
                                 parameter-massagers options-massagers
                                 &optional environment)
  (when (or (eq direct-supertypes 't)                ;In CLOS there's an intervening class
            (null direct-supertypes))                ;If the :inherit-from was erroneous
    (unless (eq name 't)                        ;Don't create a bootstrapping problem
      (typecase (find-class-that-works name nil environment)
        #+(or Symbolics LispWorks)                ;Symbolics CLOS, that is
        (clos:structure-class
          (setq direct-supertypes 'clos:structure-object))
        #+allegro
        (structure-class
          (setq direct-supertypes 'common-lisp:structure-object))
        #+CCL-2
        (structure-class
          (setq direct-supertypes 'structure-object))
        #+aclpc
        (cl:structure-class
          (setq direct-supertypes 'cl:structure-object))
        (t
          (setq direct-supertypes 'standard-object)))))

    (let* ((supertypes-list (if (listp direct-supertypes)
                                direct-supertypes
                                (list direct-supertypes)))
           (direct-superclasses (mapcar #'(lambda (name)
                                            (find-presentation-type-class name t environment))
                                        supertypes-list))
           old-inheritance new-inheritance
           (clos-class (find-class-that-works name nil environment))
           (class (find-presentation-type-class name nil environment))
           #-CLIM-extends-CLOS
           (old-direct-superclasses (if class
                                        (class-direct-superclasses class)
                                        direct-superclasses)))

      ;; If both a regular class and a presentation type class exist,
      ;; get rid of the presentation type class, with a warning
      (when (and (not (compile-file-environment-p environment))
                 class clos-class
                 (not (eq class clos-class))
                 (acceptable-presentation-type-class clos-class))
        (warn "The presentation type ~S is being converted to use ~S.~@
               If any presentation methods were previously defined for this type,~@
               they will no longer work until they are recompiled."
              name clos-class)
        (remhash name *presentation-type-class-table*)
        (setq class clos-class))
      ;; Create a presentation-type-class if one does not already exist
      (cond ((not class)
             (dolist (superclass direct-superclasses)
               (unless (or (presentation-type-class-p superclass)
                           (eq superclass
                               (find-presentation-type-class 'standard-object t environment)))
                 (error "The presentation type ~S is being defined with the class ~S as a direct supertype.  This is invalid because ~S cannot be used to create a new subclass of a CLOS class. ~@[~%Use ~S of ~S first to define the class, then use ~3:*~S to define how that class acts as a presentation type.~]"
                        name (class-proper-name superclass environment)
                        'define-presentation-type
                        (typecase superclass
                          (funcallable-standard-class 'defgeneric)
                          (standard-class 'defclass)
                          #+Symbolics                ;Symbolics CLOS, that is
                          (clos:structure-class 'defstruct)
                          #+CCL-2 (structure-class 'defstruct)
                          #+aclpc (cl:structure-class 'defstruct)
                          #+allegro (structure-class 'defstruct))
                        name)))
             (let ((class-name `(presentation-type ,name)))
               (setq class
                     (make-instance 'presentation-type-class
				    :direct-superclasses direct-superclasses
				    :direct-slots nil))
	       (setf (slot-value class 'sb-pcl::%class-precedence-list)
		     (compute-class-precedence-list class))

	       
               ;; Workaround for apparent MCL bug that otherwise causes
               ;; very bad things to happen
               #+CCL-2 (setf (slot-value class 'ccl::slots) (cons nil (vector)))
               ;; If the class name couldn't be set while making the class, set it now
               #-(or Genera Cloe-Runtime Lucid aclpc CCL-2) (setf (class-name class) class-name)))
            ((progn 
	       (setf (slot-value class 'sb-pcl::%class-precedence-list)
		     (compute-class-precedence-list class))
	       (not (or (equal (class-direct-superclasses class) direct-superclasses)
                      ;; The above equal would suffice if it were not for the fact that when
                      ;; a CLOS class in the compile-file environment has a superclass in the
                      ;; runtime environment, CLOS uses a forward-referenced-class instead
                      ;; of using the run-time class
                      (every #'(lambda (class type)
                                 (eq (class-proper-name class environment) type))
                             (class-direct-superclasses class) supertypes-list)
                      (presentation-type-class-p class))))
             ;; Inheritance must be consistent between CLOS and CLIM classes,
             ;; but only when CLOS and CLIM use the same class object.
             ;; When using different class objects, e.g. for built-in-classes, the
             ;; inheritance will necessarily be different since the metaclasses differ.
	     (unless (or (typep (find-class name) 'sb-pcl::built-in-class)
			 (typep (find-class name) 'sb-pcl::system-class))
	       (warn "The direct supertypes of presentation type ~S, ~S, do not match the direct superclasses of ~S, ~S."
		     name supertypes-list class
		     (mapcar #'(lambda (class)
				 (class-proper-name class environment))
			     (class-direct-superclasses class))))))

      ;; This used to be done only in the case where we were creating a class
      ;; "de novo".  However, CCL-2 currently doesn't record anything about
      ;; DEFCLASS at compile time, so we may write the new methods for this
      ;; presentation class on this "registered" name instead of on the
      ;; official class name.  The following line hooks up the class and the
      ;; registered name at load time.  -- rsl & York, 4 June 1991
      #+(or CCL-2 aclpc) (setf (gethash class *presentation-class-type-table*) registered-class-name
                    ;;--- Should the following be done at compile time?  It's unclear.
                    (find-class registered-class-name) class)
      #+aclpc (setf (gethash class *presentation-class-name-table*)
                    name)

      ;; Always put the class into the table, even if FIND-CLASS could find it, for better
      ;; virtual memory locality in systems where FIND-CLASS uses the property list.
      (when (symbolp name)
        (if (compile-file-environment-p environment)
            (setf (compile-time-property name 'presentation-type-class) class)
            (setf (gethash name *presentation-type-class-table*) class)))

      (setq old-inheritance (gethash class *presentation-type-inheritance-table*)
            new-inheritance (list direct-supertypes parameter-massagers options-massagers))

      ;; Store the information about the presentation type into the tables
      (cond ((compile-file-environment-p environment)
             (setf (compile-time-property class 'presentation-type-parameters) parameters))
            (parameters
             (setf (gethash class *presentation-type-parameters-table*) parameters))
            (t (remhash class *presentation-type-parameters-table*)))
      (cond ((compile-file-environment-p environment)
             (setf (compile-time-property class 'presentation-type-options) options))
            (options
             (setf (gethash class *presentation-type-options-table*) options))
            (t (remhash class *presentation-type-options-table*)))
      (if (compile-file-environment-p environment)
          (setf (compile-time-property class 'presentation-type-description) description)
          (setf (gethash class *presentation-type-description-table*) description))
      ;; We use the type's name instead of the class for speed during presentation
      ;; translator lookup
      (if (compile-file-environment-p environment)
          (setf (compile-time-property name 'parameters-are-types) parameters-are-types)
          (if parameters-are-types
              (pushnew name *presentation-type-parameters-are-types*)
              (setq *presentation-type-parameters-are-types*
                    (delete name *presentation-type-parameters-are-types*))))
      (cond ((compile-file-environment-p environment)
             (setf (compile-time-property name 'presentation-type-history) history))
            ((null history)
             (remhash name *presentation-type-history-table*))
            ((eq history 't)
             (setf (gethash name *presentation-type-history-table*)
                   (make-presentation-type-history name :history-name description)))
            (t
             (setf (gethash name *presentation-type-history-table*) history)))
      (if (compile-file-environment-p environment)
          (setf (compile-time-property class 'presentation-type-inheritance) new-inheritance)
          (setf (gethash class *presentation-type-inheritance-table*) new-inheritance))

      ;; If it used to be an abbreviation, undefine the abbreviation
      (cond ((compile-file-environment-p environment)
             (setf (compile-time-property name 'presentation-type-abbreviation) nil))
            (t
             #+Genera (sys:fundefine `(presentation-type-abbreviation ,name))
             #-Genera (remhash name *presentation-type-abbreviation-table*)))

      ;; If class already existed, make sure its inheritance is up to date.
      ;; This cannot be done until after the information is stored into the tables,
      ;; since this will recompute method combination, which accesses the tables.
      ;; This must be done even if class-direct-superclasses equals
      ;; direct-superclasses, since if there is both a defclass and a
      ;; define-presentation-type, when changing superclasses the defclass
      ;; will be redefined first, then when the define-presentation-type
      ;; is redefined, we need to make sure that methods are recombined based
      ;; on the new information in *presentation-type-inheritance-table*.
      ;; If a CLOS implementation optimizes out recombining methods when
      ;; reinitialize-instance doesn't appear to be changing anything, it will lose.
      ;; If we don't do method combination based on the presentation type class inheritance
      ;; tables, and the direct superclasses haven't changed, then don't call
      ;; reinitialize-instance.  This gets around a bug in some CLOS implementations.
      (unless (compile-file-environment-p environment)
        (unless (equal new-inheritance old-inheritance)
          (unless (or (equal direct-superclasses old-direct-superclasses)
		      (typep class 'sb-pcl::system-class))
	    (reinitialize-instance class :direct-superclasses direct-superclasses))))

      class))

;;;; Presentation Type Abbreviations

;;; Define a "macro" that expands one presentation type specifier into another
(defmacro define-presentation-type-abbreviation (name parameters expansion &key options
                                                 &environment env)
  (check-type name symbol)
  (check-type parameters list)
  (check-type options list)
  (let* ((parameters-var (and parameters '#:parameters))
         (options-var '#:options)
         (type-var '#:type)
         (parameters-ll (asterisk-default parameters))
         (options-ll (lambda-list-from-options options))
         (function
           `#'(lambda (,type-var)
                #+Genera (declare (sys:function-name (presentation-type-abbreviation ,name))
                                  (sys:function-parent ,name define-presentation-type))
                ;; Wrap the expansion in code to parse the incoming presentation type specifier
                (with-presentation-type-decoded (nil ,parameters-var ,options-var) ,type-var
                  (bind-to-list ,options-ll ,options-var
                    ,(if parameters
                         `(bind-to-list ,parameters-ll ,parameters-var ,expansion)
                         expansion))))))
    ;; If it's both an abbreviation and a class, warn
    (when (and (compile-file-environment-p env)        ;Avoid duplicate warning
               (find-class name nil #-allegro env))
      (warn "It is not valid to define a presentation type abbreviation with~@
               the same name as a CLOS class."))
    ;; Generate the expander function and pass it to load-presentation-type-abbreviation
    `(define-group ,name define-presentation-type-abbreviation
       (eval-when (:compile-toplevel)
         #+(or Genera Cloe-Runtime Minima CCL-2)
           (setf (compile-time-property ',name 'presentation-type-abbreviation)
                 ,function)
         #+(or Lucid allegro aclpc)
           ;; In these Lisps, compile-file-environment-p is always false, so
           ;; expand-presentation-type-abbreviation-1 is not going to look for
           ;; a compile-time-property.
           (load-presentation-type-abbreviation ',name ',parameters ',options ,function))
       (load-presentation-type-abbreviation ',name ',parameters ',options ,function))))

;;; Load-time support for define-presentation-type-abbreviation
(defun load-presentation-type-abbreviation (name parameters options function)
  (when #+Genera (sys:record-source-file-name name 'define-presentation-type)
        #-Genera t
    #-Genera (setf (gethash name *presentation-type-abbreviation-table*) function)
    #+Genera (sys:fdefine `(presentation-type-abbreviation ,name) function t t)
    (if parameters
        (setf (gethash name *presentation-type-parameters-table*) parameters)
        (remhash name *presentation-type-parameters-table*))
    (if options
        (setf (gethash name *presentation-type-options-table*) options)
        (remhash name *presentation-type-options-table*))
    ;; If it used to be a presentation type, undefine it
    (let ((class (gethash name *presentation-type-class-table*)))
      (when class
        (remhash name *presentation-type-class-table*)
        (remhash class *presentation-type-inheritance-table*)))
    ;; If it's both an abbreviation and a class, warn
    (when (find-class name nil)
      (warn "It is not valid to define a presentation type abbreviation with~@
               the same name as a CLOS class."))
    name))

;;; Wrap some let* bindings around some forms
;;; All the objects in the bindings have dynamic extent
;;; so use WITH-STACK-LIST as much as possible
(defun make-stack-list-bindings (bindings forms)
;  ;; The easy, ANSI Common Lisp way to do it would be thus:
;  `(let* ,bindings
;     ,@(when bindings `((declare (dynamic-extent ,@(mapcar #'first bindings)))))
;     ,@forms)
  (dolist (binding (reverse bindings))
    (labels ((translate (variable form)
               (if (and (consp form) (member (first form) '(list list* cons)))
                   (do* ((l (cdr form) (cdr l))
                         (element (car l) (car l))
                         (auxes nil)
                         (elements nil))
                        ((null l)
                         (setq forms `((,(if (eq (first form) 'list) 'with-stack-list
                                             'with-stack-list*)
                                         (,variable ,@(nreverse elements))
                                        ,@forms)))
                         (dolist (aux auxes)
                           (translate (first aux) (second aux))))
                     (if (and (consp element) (member (first element) '(list list* cons)))
                         (let ((temp (gensym)))
                           (push (list temp element) auxes)
                           (push temp elements))
                         (push element elements)))
                   (setq forms `((let ((,variable ,form)) ,@forms))))))
      (translate (first binding) (second binding))))
  forms)

;;; Given a presentation type class get its internal inheritance information
(defun presentation-type-inheritance (class &optional environment)
  (let ((list (cond ((eq class (second *presentation-type-being-defined*))
                     (cddddr *presentation-type-being-defined*))
                    ((and (compile-file-environment-p environment)
                          (compile-time-property class 'presentation-type-inheritance)))
                    (t
                     (gethash class *presentation-type-inheritance-table*)))))
    (if (listp (car list))
        (values (first list) (second list) (third list))
        ;; Expand condensed lists
        (values (list (first list)) (list (second list)) (list (third list))))))

;;; Find the parameters lambda-list of the presentation type or abbreviation named name
(defun presentation-type-parameters (name &optional environment)
  (let ((class (if (symbolp name) (find-presentation-type-class name nil environment) name)))
    (cond ((and *presentation-type-being-defined*
                (or (eq name (first *presentation-type-being-defined*))
                    (eq class (second *presentation-type-being-defined*))))
           (third *presentation-type-being-defined*))
          ((and class
                (compile-file-environment-p environment)
                (compile-time-property class 'presentation-type-inheritance))
           (compile-time-property class 'presentation-type-parameters))
          (class (gethash class *presentation-type-parameters-table*))
          (t (gethash name *presentation-type-parameters-table*)))))

;;; Optimization to not bother with destructuring bind if none of the variables
;;; will be used
(defun lambda-list-variables-used-in-body (lambda-list body)
  ;; First collect the variables bound by lambda-list
  (let ((variables nil))
    (do* ((lambda-list lambda-list (cdr lambda-list))
	  (item (first lambda-list) (first lambda-list)))
	 ((null lambda-list))
      (cond ((member item lambda-list-keywords))
	    ((atom item)
	     (push item variables))
	    (t
	     (push (if (atom (car item)) (car item) (cadar item)) variables)
	     (when (cddr item) (push (caddr item) variables)))))
    (when variables
      ;; We don't know how to do this correctly in other Lisps, since there isn't any
      ;; standardized code-walker, but as a first approximation we can assume that
      ;; if the symbols appear textually they are used as variables, and if they don't,
      ;; they aren't.  Tricky use of macros could defeat this, but it should work well enough.
      ;; Of course this can be fooled by a quoted constant with the same name as a variable
      ;; into producing an unnecessary binding, but that's only an efficiency issue.
      (labels ((analyze (tree)
		 (if (atom tree)
		     (member tree variables)
		     (some #'analyze tree))))
	(analyze body)))))

;;; Get the keyword argument name from an &KEY parameter specifier
(defun parameter-specifier-keyword (spec)
  (cond ((atom spec)
	 (intern (symbol-name spec) *keyword-package*))
	((atom (car spec))
	 (intern (symbol-name (car spec)) *keyword-package*))
	(t (caar spec))))

;;; The form that merely reconstructs this lambda-list's argument list
(defun lambda-list-pass-through (lambda-list)
  (do* ((lambda-list lambda-list (cdr lambda-list))
        (item (first lambda-list) (first lambda-list))
        (result nil)
        (mode nil))
       ((or (null lambda-list) (eq item '&aux))
        (and result
             (cons 'list (nreverse result))))
    (cond ((member item '(&optional &rest &key))
           (setq mode item))
          ((member item lambda-list-keywords))
          ((eq mode '&rest)
           (return (if result `(list* ,@(nreverse result) ,item) item)))
          (t
           (let ((keyword (and (eq mode '&key) (parameter-specifier-keyword item))))
             ;; Parameter specifiers that were only put in to ignore standard
             ;; presentation options don't count
             (unless (and keyword (consp item) (consp (car item))
                          (eq (caar item) keyword)
                          (eq (cadar item) (get keyword 'standard-presentation-option)))
               (when keyword
                 (push (if (keywordp keyword) keyword `',keyword) result))
               (push (cond ((atom item) item)
                           ((atom (car item)) (car item))
                           (t (cadar item)))
                     result)))))))



;;; Generate efficient code to massage presentation type specifiers for class
;;; into presentation type specifiers for each class in superclasses.
;;; A class can appear multiple times in superclasses if its presentation
;;; type specifier will be used more than once.
;;; If accuratep is false, the parameters/options for presentation types
;;; that don't have any will be ignored, so we can pass whatever is most
;;; convenient.  This speeds up a common case of method combination.
;;; The first value is a list of serial bindings of temporary variables.
;;; The second value is a list of elements (class parameters-form options-form)
(defun generate-type-massagers (class superclasses parameters-var options-var accuratep
                                &optional environment)
  (let ((paths nil)
        (classes-seen nil)
        (class-bindings-used nil)        ;((class parameters-count options-count)...)
        (bindings nil)
        alist)
    (labels ((map-over-superclasses (function class &rest trail)
               (declare (dynamic-extent trail))
               (funcall function class trail)
               (dolist (superclass (class-direct-superclasses class))
                 (apply #'map-over-superclasses function superclass class trail)))
             (note-path (class trail)
               (unless (assoc class paths)
                 (note-class class)
                 (dolist (subclass trail)
                   (note-class subclass))
                 (push (copy-list (cons class trail)) paths)))
             (note-class (class)
               (pushnew class classes-seen))
             (note-binding-used (class type)
               (let ((item (assoc class class-bindings-used)))
                 (unless item
                   (push (setq item (list class 0 0)) class-bindings-used))
                 (ecase type
                   (parameters (incf (second item)))
                   (options (incf (third item)))))))
      (declare (dynamic-extent #'note-path))

      ;; Find the direct inheritance path to each class
      (map-over-superclasses #'note-path class)
      (dolist (superclass superclasses)
        (unless (assoc superclass paths)
          (error "~S is not a superclass of ~S" superclass class)))

      ;; Remember how many times the parameters and options of each terminal class
      ;; will be used
      (dolist (class superclasses)
        (note-binding-used class 'parameters)
        (note-binding-used class 'options))

      ;; Build up the alist of massaging forms
      ;; This traverses the classes in most-specific-first order
      ;; The first pass identifies common subexpressions, the second builds the real alist
      (dolist (pass '(first second))
        (setq alist `((,class ,parameters-var ,options-var)))
        (dolist (to-class classes-seen)
          (do* ((path (reverse (assoc to-class paths)) (cdr path))
                (from-class (first path) (first path))
                (to-class (second path) (second path)))
               ((null to-class))
            (unless (assoc to-class alist)
              (let ((parameters-ll (asterisk-default
                                     (presentation-type-parameters from-class environment)))
                    (options-ll (lambda-list-from-options
                                  (presentation-type-options from-class environment)))
                    (supertype (class-name to-class))
                    (from-data (assoc from-class alist)))
                (assert (not (null from-data)) ()
                        "INTERNAL ERROR: classes-seen is out of order")
                (when (and (consp supertype) (eq (first supertype) 'presentation-type))
                  (setq supertype (second supertype)))
                (multiple-value-bind (direct-supertypes parameters-massager options-massager)
                    (presentation-type-inheritance from-class environment)
                  (when (listp direct-supertypes)
                    (do ()        ;Find correct supertype when multiple inheritance
                        ((or (null direct-supertypes)
                             (eq (car direct-supertypes) supertype)
                             #+(or aclpc acl86win32)
			     (let ((suclass
                                     (gethash
                                       (car direct-supertypes)
                                       *presentation-type-class-table*)))
                               (and suclass
                                    (eq supertype (class-name suclass)))))
                         (setq parameters-massager (car parameters-massager)
                               options-massager (car options-massager)))
                      (setq direct-supertypes (cdr direct-supertypes)
                            parameters-massager (cdr parameters-massager)
                            options-massager (cdr options-massager))))

                  ;; Make the massaging forms that go from from-class to to-class
                  (setq parameters-massager
                        (cond ((and (not accuratep) (null (presentation-type-parameters
                                                            to-class environment)))
                               ;; Pass inaccurate, but faster value
                               ;; since it's going to be ignored anyway
                               parameters-var)
                              ((constantp parameters-massager)
                               parameters-massager)
                              ((and (equal parameters-massager
                                           (lambda-list-pass-through parameters-ll))
                                    (equal (presentation-type-parameters from-class
                                                                         environment)
                                           (presentation-type-parameters to-class
                                                                         environment)))
                               ;; The subtype's parameters are passed through to the supertype
                               (when (eq pass 'first)
                                 (note-binding-used from-class 'parameters))
                               (second from-data))
                              ((lambda-list-variables-used-in-body
                                 parameters-ll (list parameters-massager))
                               ;; The massager is parameterized, wrap bindings around it
                               (when (eq pass 'first)
                                 (note-binding-used from-class 'parameters))
                               `(bind-to-list ,parameters-ll ,(second from-data)
                                  ,parameters-massager))
                              (t parameters-massager)))
                  (setq options-massager
                        (cond ((and (not accuratep)
                                    (null options-massager)
                                    (equal (presentation-type-options to-class environment)
                                           (presentation-type-options class environment))
                                    (= (length (presentation-type-options to-class
                                                                          environment))
                                       (length *standard-presentation-options*)))
                               ;; Pass inaccurate, but faster value
                               ;; since it's going to be ignored anyway
                               options-var)
                              ((constantp options-massager)
                               options-massager)
                              ((and (equal options-massager
                                           (lambda-list-pass-through options-ll))
                                    (equal (presentation-type-options from-class environment)
                                           (presentation-type-options to-class environment)))
                               ;; The subtype's options are passed through to the supertype
                               (when (eq pass 'first)
                                 (note-binding-used from-class 'options))
                               (third from-data))
                              ((lambda-list-variables-used-in-body
                                 options-ll (list options-massager))
                               ;; The massager is parameterized, wrap bindings around it
                               (when (eq pass 'first)
                                 (note-binding-used from-class 'options))
                               `(bind-to-list ,options-ll ,(third from-data)
                                  ,options-massager))
                              (t options-massager)))

                  ;; Remove common subexpressions referenced more than once
                  (when (eq pass 'second)
                    (unless (or (constantp parameters-massager) (symbolp parameters-massager)
                                (member (second (assoc to-class class-bindings-used))
                                        '(0 1 nil)))
                      (let ((temp (make-symbol (format nil "~A-~A" supertype 'parameters))))
                        (push (list temp parameters-massager) bindings)
                        (setq parameters-massager temp)))
                    (unless (or (constantp options-massager) (symbolp options-massager)
                                (member (third (assoc to-class class-bindings-used))
                                        '(0 1 nil)))
                      (let ((temp (make-symbol (format nil "~A-~A" supertype 'options))))
                        (push (list temp options-massager) bindings)
                        (setq options-massager temp))))
                  (push (list to-class parameters-massager options-massager) alist)))))))
      (values (nreverse bindings) alist))))

;;; Find the class corresponding to the presentation type named NAME
(defun find-presentation-type-class (name &optional (errorp t) environment)
  #+Genera (declare (inline compile-file-environment-p))
  #+allegro (setq environment (compile-file-environment-p environment))
  (macrolet ((not-found (name)
               `(if (gethash name *presentation-type-abbreviation-table*)
                    (error "~S is a presentation type abbreviation, not the name of a presentation type" ,name)
                    (error "~S is not the name of a presentation type" ,name))))
    (typecase name
      (symbol
        (let ((compile-file-environment-p (compile-file-environment-p environment)))
          (or (and (eq name (first *presentation-type-being-defined*))
                   (second *presentation-type-being-defined*))
              (if compile-file-environment-p
                  (compile-time-property name 'presentation-type-class)
                  (gethash name *presentation-type-class-table*))
              (let ((class (find-class name nil environment)))
                (and (acceptable-presentation-type-class class)
                     class))
              (when compile-file-environment-p
                ;; compile-file environment inherits from the run-time environment
                (or (gethash name *presentation-type-class-table*)
                    (let ((class (find-class name nil nil)))
                      (and (acceptable-presentation-type-class class)
                           class))))
              (and errorp (not-found name)))))
      ((satisfies acceptable-presentation-type-class)
       name)
      (otherwise        ;a type error should complain even if errorp is nil
        (not-found name)))))

;;; Return the presentation type name corresponding to a class
;;; This is essentially the inverse of find-presentation-type-class
(defmethod class-presentation-type-name ((class presentation-type-class) &optional environment)
  (declare (ignore environment))
  (second (class-name class)))

(defmethod class-presentation-type-name ((class class) &optional environment)
  (class-proper-name class environment))



(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *declarations-may-be-exposed-by-macro-expansion* nil)
  
(defun extract-declarations (body &optional environment)
   (let ((declarations nil)
	(documentation nil))
    (block process-declarations
      (loop
	(when (null body) (return-from process-declarations))
	(let ((form (first body)))
	  (cond ((stringp form)
		 (setf documentation (or documentation form)
		       body (cdr body)))
		((atom form) (return-from process-declarations))
		;; X3J13 says this nonsense is not required any more:
		(*declarations-may-be-exposed-by-macro-expansion*
		 (block expand-macros
		   (loop
		     (when (eq (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		       (return-from expand-macros))
		     (multiple-value-bind (new-form macro-expanded-p)
			 (macroexpand-1 form environment)
		       (unless macro-expanded-p (return-from process-declarations))
		       (setf form new-form)))))
		(t (if (eq (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		     (return-from process-declarations)))))))
     (values documentation `((declare ,@declarations)) body)))
)

;;;; Presentation Methods

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Common subroutine of the three forms of define-presentation-method
(defun define-presentation-method-1 (presentation-function-name body kind environment)
  ;;--- Remodularize this some time, because presentation-function-name isn't
  ;; really right, but it's too hard to compute the actual name of this definition
  ;; until we've parsed a bunch of stuff.
  (let* ((qualifiers (do ((qualifiers nil (cons (pop body) qualifiers)))
                           ((listp (car body)) (nreverse qualifiers))))
           (specialized-lambda-list (copy-list (pop body)))
           (info (or (gethash presentation-function-name *presentation-generic-function-table*)
                     (error "~S is not the name of a presentation~@
                             function that supports presentation methods."
                            presentation-function-name)))
           (generic-lambda-list (cdr info))
           (parameters (and kind (member 'parameters generic-lambda-list)))
           (options (and kind (member 'options generic-lambda-list)))
           ;; Position in specialized-lambda-list of the type parameter
           (type-pos (- (position 'type generic-lambda-list)
                        (+ 1 (if parameters 1 0) (if options 1 0))))
           type-var        ;name of the user's variable
           ;; Uninterned symbols for "magic" parameters added behind the user's back
           (type-key-var '#:type-key)
           (type-class-var '#:type-class)
           (parameters-var '#:parameters)
           (options-var '#:options)
           presentation-type-name class)
      (multiple-value-bind (documentation declarations body)
          (extract-declarations body environment)

        (unless (eq kind ':default)
          ;; Extract the presentation-type-name from the specialized-lambda-list
          ;; and make the type parameter unspecialized, since that argument is just
          ;; a presentation type specifier
          (setq presentation-type-name (nth type-pos specialized-lambda-list))
          (unless (and (consp presentation-type-name)
                       (consp (cdr presentation-type-name))
                       (null (cddr presentation-type-name))
                       (atom (second presentation-type-name)))        ;symbol or class
            (error "The TYPE parameter specifier, ~S, is invalid." presentation-type-name))
          (setq type-var (first presentation-type-name))
          (setq presentation-type-name (second presentation-type-name))
          (setf (nth type-pos specialized-lambda-list) type-var)
          (push type-var body)        ;specialized parameters don't get unused variable warnings

          ;; Find the presentation type class, the key to all the information we need
          (setq class (find-presentation-type-class presentation-type-name t environment))

          ;; Wrap the body in lexical bindings of the presentation type parameters and options
          (when parameters
            (let ((parameters-ll (asterisk-default
                                   (presentation-type-parameters class environment))))
              (when (lambda-list-variables-used-in-body parameters-ll body)
                (setq body `((bind-to-list
                               ,parameters-ll
                               ;; If massaging the parameters during
                               ;; method inheritance
                               #+CLIM-extends-CLOS ,parameters-var
                               ;; If doing it the slow way
                               #-CLIM-extends-CLOS (inherited-presentation-type-parameters
                                                     ',presentation-type-name
                                                     ,type-var
                                                     ,parameters-var)
                               ,@body))))))
          (when options
            (let ((options-ll (lambda-list-from-options
                                (presentation-type-options class environment))))
              (when (lambda-list-variables-used-in-body options-ll body)
                (setq body `((bind-to-list
                               ,options-ll
                               ;; If massaging the options during
                               ;; method inheritance
                               #+CLIM-extends-CLOS ,options-var
                               ;; If doing it the slow way
                               #-CLIM-extends-CLOS (inherited-presentation-type-options
                                                     ',presentation-type-name
                                                     ,type-var
                                                     ,options-var)
                               ,@body)))))))

        ;; Add the "magic" parameters to the front of the lambda list
        ;; and specialize this method to the presentation type class
        ;; It's okay not to use the "magic" parameters so add dummy uses to the body
        (when options
          (push options-var specialized-lambda-list)
          (push options-var body))
        (when parameters
          (push parameters-var specialized-lambda-list)
          (push parameters-var body))
        (push (ecase (first generic-lambda-list)
                (type-key `(,type-key-var
                            ,(cond ((eq kind ':default) `t)
                                   ((eq (class-name class) presentation-type-name)
                                    presentation-type-name)
                                   (t
                                    #+(or CCL-2 aclpc) (gethash class *presentation-class-type-table*)
                                    #-(or CCL-2 aclpc) class))))
                (type-class `(,type-class-var
                              ,(cond ((eq kind ':default) `t)
                                     ((symbolp presentation-type-name)
                                      `(eql (find-presentation-type-class
                                              ',presentation-type-name)))
                                     (t `(eql ,class))))))
              specialized-lambda-list)

        #+Genera
        (when (null kind)
          (push `(declare (sys:function-parent ,presentation-type-name define-presentation-type))
                declarations))

        ;;--- Note that DEFMETHOD doesn't accept class objects, according to 88-002R.
        ;;--- We will have to do something for that (like give them gensym names?  Ick!)

        ;; Expand into a defmethod
        `(defmethod ,(car info) ,@qualifiers ,specialized-lambda-list
           ,@(and documentation (list documentation))
           ,@declarations
           (block ,presentation-function-name
             ,@body)))))
)

;;; Define a presentation method, which involves translating the generic function name,
;;; translating the specializer of the type argument, and wrapping the body in
;;; lexical bindings of the presentation type parameters and options.
(defmacro define-presentation-method (presentation-function-name &rest body
                                      &environment environment)
  (define-presentation-method-1 presentation-function-name body ':massage environment))

;;; The next level down from define-presentation-method, this doesn't create
;;; lexical bindings of the presentation type parameters and options and hence
;;; doesn't insert those parameters into the lambda-list.  It still inserts
;;; a type-key or type-class parameter.
(defmacro define-presentation-method-without-massaging (presentation-function-name &rest body
                                                        &environment environment)
  (define-presentation-method-1 presentation-function-name body nil environment))

;;; Define a default method for a presentation generic function.
;;; It will be invoked if the presentation type doesn't have a specialized method.
;;; There is no parameters/options processing, this macro just exists for abstraction.
(defmacro define-default-presentation-method (presentation-function-name &rest body
                                              &environment environment)
  (define-presentation-method-1 presentation-function-name body ':default environment))


;;; Generate a map-over-presentation-type-supertypes method if there is any
;;; parameterized inheritance.  Otherwise the default method will work.
;;; If there is parameterized inheritance, compile code to do it efficiently
;;; rather than interpreting the inheritance information at run time.
(defun generate-map-over-presentation-type-supertypes-method-if-needed
       (name class function-var parameters-var options-var type-var
        &optional environment)
  (setf (slot-value class 'sb-pcl::%class-precedence-list)
	(compute-class-precedence-list class))
  (let ((superclasses (cdr (class-precedence-list class))))
    (multiple-value-bind (bindings alist)
        (generate-type-massagers class superclasses parameters-var options-var t environment)
      (unless (every #'(lambda (class)
                         (let ((massage-forms (cdr (assoc class alist))))
                           (and (null (first massage-forms))                ;no parameters
                                (null (second massage-forms)))))        ;no options
                     superclasses)
        `((define-presentation-method-without-massaging
                map-over-presentation-type-supertypes
                (,parameters-var ,options-var (,type-var ,name) ,function-var)
            ,parameters-var ,options-var        ;might not be used
            (funcall ,function-var ',name ,type-var)
            ,@(make-stack-list-bindings bindings
                (mapcan #'(lambda (class)
                            (let* ((massage-forms (cdr (assoc class alist)))
                                   (parameters (first massage-forms))
                                   (options (second massage-forms))
                                   (type-name (class-presentation-type-name class)))
                              (if (or options parameters)
                                  (make-stack-list-bindings
                                    `((type ,(if options
                                                 (if (constantp parameters)
                                                     (if (constantp options)
                                                         `',(cons (cons type-name
                                                                        (eval parameters))
                                                                  (eval options))
                                                         `(cons ',(cons type-name
                                                                        (eval parameters))
                                                                ,options))
                                                     `(cons (cons ',type-name ,parameters)
                                                            ,options))
                                                 (if (constantp parameters)
                                                     `',(cons type-name (eval parameters))
                                                     `(cons ',type-name ,parameters)))))
                                    `((funcall ,function-var ',type-name type)))
                                  `((funcall ,function-var ',type-name ',type-name)))))
                        superclasses))
            nil))))))

#-CLIM-extends-CLOS
(defun generate-presentation-type-inheritance-methods
    (name class parameters-var options-var &optional environment)
  (let ((superclasses
         #-(or allegro aclpc) (cdr (class-precedence-list class))
         #+aclpc
          (progn
            (unless (acl:class-finalized-p class)
              (acl:finalize-inheritance class))
            (cdr (class-precedence-list class)))
         #+allegro ;; Work around bug in CLOS compilation environments...
         (multiple-value-bind (no-errorp result)
             (excl:errorset (progn
                              ;; Finalization is necessary according to AMOP. -smh 18may93
                              (unless (clos:class-finalized-p class)
                                (clos:finalize-inheritance class))
                              (cdr (class-precedence-list class)))
                            nil)
           (if no-errorp
               result
             (return-from generate-presentation-type-inheritance-methods))))
        (to-type-name-var '#:to-type-name)
        (from-type-name-var '#:from-type-name))
    #+Minima (setq superclasses (elide-nonessential-superclasses superclasses))
    (mapcan #'(lambda (superclass)
                (multiple-value-bind (bindings alist)
                    (generate-type-massagers class (list superclass)
                                             parameters-var options-var t environment)
                  `(,@(when (presentation-type-parameters superclass environment)
                        `((defmethod inherited-presentation-type-parameters-method
                              ((,to-type-name-var
                                (eql ',(class-presentation-type-name superclass)))
                               (,from-type-name-var (eql ',name))
                               ,parameters-var)
                            ,parameters-var ;might not be used
                            (let* ,bindings
                              ,@(when bindings
                                  `((declare (dynamic-extent ,@(mapcar #'first bindings)))
                                    ,@(mapcar #'first bindings)))
                              ,(second (assoc superclass alist))))))
                      ,@(when (presentation-type-options superclass environment)
                          `((defmethod inherited-presentation-type-options-method
                                ((,to-type-name-var
                                  (eql ',(class-presentation-type-name superclass)))
                                 (,from-type-name-var (eql ',name))
                                 ,options-var)
                              ,options-var ;might not be used
                              (let* ,bindings
                                ,@(when bindings
                                    `((declare (dynamic-extent ,@(mapcar #'first bindings)))
                                      ,@(mapcar #'first bindings)))
                                ,(third (assoc superclass alist)))))))))
            superclasses)))

#-CLIM-extends-CLOS
(defmethod inherited-presentation-type-parameters-method
    ((to-type-name t) (from-type-name t) from-parameters)
  (declare (ignore from-parameters))
  nil)

#-CLIM-extends-CLOS
(defun inherited-presentation-type-parameters (to-type-name from-type from-parameters)
  (with-presentation-type-decoded (from-type-name) from-type
    (unless (symbolp from-type-name)
      (setq from-type-name (class-presentation-type-name from-type-name)))
    (if (eq from-type-name to-type-name)
        from-parameters
        (inherited-presentation-type-parameters-method to-type-name from-type-name
                                                       from-parameters))))

#-CLIM-extends-CLOS
(defmethod inherited-presentation-type-options-method
    ((to-type-name t) (from-type-name t) from-options)
  (declare (ignore from-options))
  nil)

#-CLIM-extends-CLOS
(defun inherited-presentation-type-options (to-type-name from-type from-options)
  (with-presentation-type-decoded (from-type-name) from-type
    (unless (symbolp from-type-name)
      (setq from-type-name (class-presentation-type-name from-type-name)))
    (if (eq from-type-name to-type-name)
        from-options
        (inherited-presentation-type-options-method to-type-name from-type-name
                                                    from-options))))

(defmacro define-presentation-type (name parameters
				    &key options inherit-from description history
				      parameters-are-types
				      ((omit-map-over-ptype-supertypes-p omit-map-over-ptype-supertypes-p)
				       nil)
				    &environment environment)
  (check-type name (or symbol (satisfies acceptable-presentation-type-class))
              "a symbol or a non-built-in class")
  (check-type parameters list)
  (check-type options list)
  (check-type history (or symbol (satisfies acceptable-presentation-type-class))
              "T, NIL, or the name of another presentation type")
  (check-type description (or string null))
  (check-type parameters-are-types boolean)

  (let* ((parameters-var '#:parameters)
	 (options-var '#:options)
	 (type-var '#:type)
	 (function-var '#:function)
	 (parameters-ll (asterisk-default parameters))
	 (options-ll (lambda-list-from-options options))
	 (class (if (symbolp name) (find-class-that-works name nil environment) name))
	 (direct-superclasses (and class
				   (mapcar #'(lambda (class)
					       (class-proper-name class environment))
					   (class-direct-superclasses class))))
	 ;; Default the supertype if unsupplied or nil
	 ;; This assumes consistency of the compile-time and load-time environments.  Okay?
	 (inherit-from (cond (inherit-from)
			     ((null class) `'standard-object)
			     ((cdr direct-superclasses) `'(and ,@direct-superclasses))
			     (t `',(first direct-superclasses)))))
    
    ;; Default the :description option
    (unless description
      (setq description (substitute #\space #\- (string-downcase
						 (if (symbolp name) name
						     (class-name name))))))
    ;; Convert :inherit-from into what we need to inherit methods and map over supertypes
    (multiple-value-bind (direct-supertypes parameter-massagers options-massagers)
        (analyze-inherit-from inherit-from parameters-ll options-ll)

      ;; Make sure we have a class adequate to use at macro expansion time.
      ;; It has to have the right class-precedence-list, as well as serving
      ;; as a key for the second position of *presentation-type-being-defined*.
      ;; If not compiling to a file, it has to be EQ to the class that will
      ;; be used for real when methods are invoked.  If compiling to a file, it
      ;; has to be similar as a constant to the class that will be used for real.
      ;;--- Might have to think this through a bit better
      ;;--- In particular, merely macroexpanding should not define the p.t. class
      ;;--- if it was not already defined, and also it's undesirable to change the
      ;;--- direct supertypes during macroexpansion, although we have to do that
      ;;--- to get the CPL right.  Maybe the CPL has to be specially kludged somehow
      ;;--- in *presentation-type-being-defined* ???

      (unless (acceptable-presentation-type-class class)
	(setq class (ensure-presentation-type name parameters options direct-supertypes
					      description history
					      parameters-are-types
					      parameter-massagers options-massagers
					      environment)))
              ;; Establish the information needed at macro expansion time
        (let ((*presentation-type-being-defined*
                (list name class parameters options
                      direct-supertypes parameter-massagers options-massagers)))

	  ;; Generate the form that stores all the information and defines the
          ;; automatically-defined presentation methods
          `(progn
             (eval-when (:compile-toplevel)
               (ensure-presentation-type ',name ',parameters ',options ',direct-supertypes
                                         ',description ',history
                                         ',parameters-are-types
                                         ',parameter-massagers ',options-massagers
                                         ;; Can't just put the environment here, since macro
                                         ;; expansion environments have dynamic extent
                                         ',(and (compile-file-environment-p environment)
						#-(and allegro (version>= 7 0)) 'compile-file
						#+(and allegro (version>= 7 0))
						(sys:augment-environment environment))))
             (define-group ,name define-presentation-type
               (ensure-presentation-type ',name ',parameters ',options ',direct-supertypes
                                         ',description ',history
                                         ',parameters-are-types
                                         ',parameter-massagers ',options-massagers)
               ,@(unless omit-map-over-ptype-supertypes-p
                   (generate-map-over-presentation-type-supertypes-method-if-needed
                    name class function-var parameters-var options-var type-var environment))
               #-CLIM-extends-CLOS
               ,@(generate-presentation-type-inheritance-methods
                   name class parameters-var options-var environment)
               ',name))))))

;;; Called by MAKE-LOAD-FORM forms
(defun load-reference-to-presentation-type-class (name parameters options direct-supertypes
                                                  description history
                                                  parameters-are-types
                                                  parameters-massagers options-massagers)
  (or (find-presentation-type-class name nil)
      (ensure-presentation-type name parameters options direct-supertypes
                                description history
                                parameters-are-types
                                parameters-massagers options-massagers)))

;;; These classes get into compiled files as method parameter specializers
(defmethod make-load-form ((object presentation-type-class) &optional environment)
  (declare (ignore environment))
   ;; The following would be "the right thing," but it doesn't work because some compilers
  ;; evaluate the MAKE-LOAD-FORM forms out of order with the top-level forms, so the
  ;; presentation type class might not be defined yet when this is evaluated.
  #+++ignore `(find-presentation-type-class ',(class-presentation-type-name object))
  ;; So do this instead
  `(load-reference-to-presentation-type-class
     ',(class-presentation-type-name object #+Symbolics 'compile-file)
     ',(presentation-type-parameters object)
     ',(presentation-type-options object)
     ',(let ((superclasses (class-direct-superclasses object)))
         (if (cdr superclasses)
             (mapcar #'(lambda (class)
                         (class-presentation-type-name class #+Symbolics 'compile-file))
                     (class-direct-superclasses object))
             ;; Use an atom instead of a one-element list to work around a Lucid bug
             ;; where it blows up if anything here is a list that gets consed freshly
             ;; each time make-load-form is called, because make-load-form gets called
             ;; twice and the results of evaluating the subforms have to be EQ.  Jeez!
             (class-presentation-type-name (first superclasses) #+Symbolics 'compile-file)))
     nil                        ;description doesn't matter
     ',(let* ((type-name (class-presentation-type-name object #+Symbolics 'compile-file))
              (history (gethash type-name *presentation-type-history-table*)))
          ;; The (AND HISTORY ...) is here to get around a compile/load
          ;; bootstrapping problem in HISTORIES.LISP
          (cond ((and history (typep history 'basic-history))
                (or (eq (slot-value history 'name) type-name)
                    (slot-value history 'name)))
               ((symbolp history) history)
               (t nil)))
     ',(not (null (member (class-name object) *presentation-type-parameters-are-types*)))
     nil                        ;parameter-massagers don't matter
     nil))                        ;options-massagers don't matter


;;; Is OBJECT of type TYPE?
(defun presentation-typep (object type)
  (with-presentation-type-decoded (type-name parameters) type
    (case type-name
      ((t) t)                ;the type of all objects
      ((nil) nil)        ;the type of no objects
      (otherwise
        ;; If this type is a Common Lisp type, first call TYPEP and only use the
        ;; method to check the parameters.  Otherwise the method does it all.
        ;; This actually applies not to all Common Lisp types, but just to the
        ;; ones that have corresponding classes, since the others either are not
        ;; presentation types or have to have explicit presentation-typep methods.
        (let ((class (if (symbolp type-name) (find-class type-name nil) type-name)))
          (unless (or (null class) (presentation-type-class-p class))
            (unless #+Genera (clos-internals:typep-class object class)
                    #+CCL-2  (ccl::class-typep object class)
                    #-(or Genera CCL-2) (typep object class)
              (return-from presentation-typep nil))
            (unless parameters
              (return-from presentation-typep t))))
        ;; Call the method, either to check the parameters after determining
        ;; that object is a member of the correct class, or to do it all.
       (funcall-presentation-generic-function presentation-typep object type)))))

;; The "real" default method is inlined above in PRESENTATION-TYPEP.  What
;; it does is to call TYPEP on anything that is a class or built-in type.
;; If that did not get executed, then this type must be a user-defined type
;; that does not correspond to a class.  Let him know.
(define-default-presentation-method presentation-typep (object type)
  (declare (ignore object))
  (error "You must supply a ~S method for the presentation type ~S"
         'presentation-typep (presentation-type-name type)))

;;;; Map Over Supertypes

;;; Successively calls FUNCTION on the type and all its supertypes, in order.
;;; FUNCTION is called with two arguments, the new type name and the new
;;; presentation type specifier with parameters and options.
#+Genera (zwei:defindentation (map-over-presentation-type-supertypes 1 2))
(defun map-over-presentation-type-supertypes (presentation-type function)
  (declare (dynamic-extent function))
  (funcall-presentation-generic-function map-over-presentation-type-supertypes
    presentation-type function))

;;; Map over unparameterized presentation types, including some mappings
;;; generated by ADDITIONAL-FORMS.
(defmacro map-over-presentation-type-supertypes-augmented
          (presentation-type function &body additional-forms)
  (let ((name (gensymbol 'name))
        (class (gensymbol 'class))
        (type-name (gensymbol 'type-name)))
    `(with-presentation-type-decoded (,name) ,presentation-type
       (funcall ,function ,name ,presentation-type)
       ,@additional-forms
       (dolist (,class (cdr (find-class-precedence-list
                              (find-presentation-type-class ,name))))
         (let ((,type-name (class-presentation-type-name ,class)))
           (funcall function ,type-name ,type-name))))))

;;; The default method that works for mapping over unparameterized presentation types
(define-default-presentation-method map-over-presentation-type-supertypes
                                    (presentation-type function)
  (map-over-presentation-type-supertypes-augmented presentation-type function))

;;; SUBTYPEP

;; SWM wanted this to have a cache, but let's hold off since it looks
;; like MAP-OVER-PRESENTATION-TYPE-SUPERTYPES is faster than caching.
;; Here is some metering information (Genera 8.0, MacIvory model 2):
;; subtype        supertype       Common Lisp    CLIM    Dynamic Windows (cached)
;; integer        integer               9          10         387
;; integer        rational            907         307         382
;; (integer 1 2)  (integer 3 5)       342         213         698
;; (integer 1 2)  (rational 3 5)     2296         522         729
;; integer        float              3180         224         450
;; integer        number              897         336         419
;; Current numbers for CLIM will be a few microseconds slower, since some code was added

;; Is TYPE a subtype of PUTATIVE-SUPERTYPE?
;; Should not accept abbreviations, you gotta expand them before calling this
;; Possible returned values are:
;;  T,T     => TYPE is definitely a subtype of PUTATIVE-SUPERTYPE
;;  NIL,T   => TYPE is definitely *not* a subtype of PUTATIVE-SUPERTYPE
;;  NIL,NIL => Don't know, the answer cannot definitively be determined
;;  T,NIL   => Can't ever happen
(defun presentation-subtypep (type putative-supertype)
  (multiple-value-bind (subtype-p known-p)
      (presentation-subtypep-1 type putative-supertype)
    (when (and (eq subtype-p nil)
               (eq known-p t)
               (gethash (presentation-type-name putative-supertype)
                        *presentation-type-abbreviation-table*))
      ;; One reason we might have gotten this result is that the supertype
      ;; was an abbreviation, so it never was reached during the type walk.
      ;; Check this out now.
      (error "~S is a presentation type abbreviation, not the name of a presentation type"
             (presentation-type-name putative-supertype)))
    (values subtype-p known-p)))

;; The guts of the above, but faster since we don't worry about the case
;; where a type abbreviation was passed in.
(defun presentation-subtypep-1 (type putative-supertype)
  (with-presentation-type-decoded (type-name type-parameters) type
    (with-presentation-type-decoded (supertype-name supertype-parameters) putative-supertype

      ;; Work even if classes are used as presentation type specifiers.
      ;; Canonicalize to symbols rather than to classes on the theory that
      ;; this will be faster because symbols are more commonly used here.
      (unless (symbolp type-name)
        (setq type-name (class-presentation-type-name type-name)))
      (unless (symbolp supertype-name)
        (setq supertype-name (class-presentation-type-name supertype-name)))

      ;; Do some quick tests first
      (cond ((eq type-name supertype-name)        ;No inheritance involved
             (cond ((null supertype-parameters)
                    (values t t))                ;Parameters can only restrict
                   ((equal supertype-parameters type-parameters)
                    (values t t))                ;Everything is its own subtype
                   (t
                    (multiple-value-bind (subtype-p known-p)
                        (funcall-presentation-generic-function presentation-subtypep
                          type putative-supertype)
                      ;; Catch a common mistake in PRESENTATION-SUBTYPEP methods
                      (when (and subtype-p (not known-p))
                        (cerror "Return the two values T and T"
                                "The call to ~S on type ~S only returned a single value of T"
                                'presentation-subtypep type)
                        (setq known-p t))
                      (values subtype-p known-p)))))
            ((eq type-name 't)
             (values nil t))                        ;Nothing has T as a subtype
            ((eq type-name 'nil)
             (values t t))                        ;NIL is a subtype of everything
            ((eq supertype-name 't)
             (values t t))                        ;T is a supertype of everything
            ((eq supertype-name 'nil)
             (values nil t))                        ;Nothing has NIL as a supertype
            ((eq type-name 'and)
             ;; Yes, we really call SOME, not EVERY.  For example,
             ;; (AND INTEGER (SATISFIES ODDP)) is a subtype of INTEGER
             (if (some #'(lambda (type)
                           (with-presentation-type-decoded (type-name) type
                             (unless (member type-name '(not satisfies))
                               (presentation-subtypep-1 type putative-supertype))))
                       type-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq type-name 'or)
             (if (every #'(lambda (type)
                            (presentation-subtypep-1 type putative-supertype))
                        type-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq supertype-name 'and)
             (if (every #'(lambda (supertype)
                            (with-presentation-type-decoded (type-name) supertype
                              (unless (member type-name '(not satisfies))
                                (presentation-subtypep-1 type supertype))))
                        supertype-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq supertype-name 'or)
             (if (some #'(lambda (supertype) (presentation-subtypep-1 type supertype))
                       supertype-parameters)
                 (values t t)
                 (values nil nil)))
            (t
             ;; Must resort to inheritance to figure it out
             (flet ((psubtypep (ntype-name ntype)
                      (when (eq ntype-name supertype-name)
                        ;; Aside from parameters, type is a subtype of putative-supertype
			(format t  "~%nytpe-name: ~S, supertype-name:~S" ntype-name supertype-name)
			(finish-output)
                        (return-from presentation-subtypep-1
                          (cond ((null supertype-parameters)
                                 (values t t))        ;Parameters can only restrict
                                ((equal supertype-parameters
                                        (with-presentation-type-decoded (nil parameters) ntype
                                          parameters))
                                 (values t t))        ;Everything is its own subtype
                                (t                ;Call method to figure out the parameters

                                 (multiple-value-bind (subtype-p known-p)
                                     (funcall-presentation-generic-function
                                       presentation-subtypep
                                       ntype putative-supertype)
                                   (when (and subtype-p (not known-p))
                                     (cerror "Return the two values T and T"
                                             "The call to ~S on type ~S only returned a single value of T"
                                             'presentation-subtypep ntype)
                                     (setq known-p t))
                                   (values subtype-p known-p))))))))
               (declare (dynamic-extent #'psubtypep))
               (map-over-presentation-type-supertypes type #'psubtypep))
             ;; TYPE is not even a subclass of PUTATIVE-SUPERTYPE
             (values nil t))))))

;;;; Histories

(defun presentation-type-history (presentation-type)
  (let ((type (expand-presentation-type-abbreviation presentation-type)))
    (loop
      (let ((history
              (funcall-presentation-generic-function presentation-type-history type)))
        (cond ((null history)
               (return-from presentation-type-history nil))
              ((typep history 'basic-history)
               (return-from presentation-type-history history))
              (t
               (setq type history)))))))

(define-default-presentation-method presentation-type-history (presentation-type)
  (let ((name (presentation-type-name presentation-type)))
    (values (gethash name *presentation-type-history-table*))))

;; Some histories are maintained for each instance of an application frame
(defun presentation-type-history-for-frame (type frame)
  (when (frame-maintain-presentation-histories frame)
    (let* ((type-name (presentation-type-name type))
           (history (find type-name (slot-value frame 'histories)
                          :key #'presentation-history-type)))
      (unless history
        (setq history (make-presentation-type-history type-name))
        (push history (slot-value frame 'histories)))
      history)))

(defun presentation-default-preprocessor (default presentation-type &key default-type)
  (let ((type (expand-presentation-type-abbreviation presentation-type)))
    (funcall-presentation-generic-function presentation-default-preprocessor
      default type :default-type default-type)))

(define-default-presentation-method presentation-default-preprocessor
                                    (default presentation-type &key default-type)
  (declare (ignore presentation-type))
  (values default default-type))

;;;; Conditions for Parsing Exceptions

(define-condition simple-parse-error (parse-error)
  ((format-string :initarg :format-string
                  :reader parse-error-format-string )
   (format-arguments :initarg :format-arguments :initform nil
                     :reader parse-error-format-arguments))
  (:report (lambda (condition stream)
             (apply #'format stream (parse-error-format-string condition)
                                    (parse-error-format-arguments condition)))))

(define-condition input-not-of-required-type (parse-error)
  ((string :reader input-not-of-required-type-string :initarg :string)
   (type :reader input-not-of-required-type-type :initarg :type))
  (:report (lambda (condition stream)
             (let ((string (input-not-of-required-type-string condition)))
               (format stream "The input read, ~A, was not "
                       (if (equal string "") '|""| string))
               (describe-presentation-type (input-not-of-required-type-type condition)
                                           stream 1)
               (write-char #\. stream)))))

(defun simple-parse-error (format-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (signal 'simple-parse-error
          :format-string (evacuate-temporary-string format-string)
          :format-arguments (copy-list format-arguments))
  (error "~S signalled outside of ~S."
         'simple-parse-error 'accept))

(defun input-not-of-required-type (object type)
  (signal 'input-not-of-required-type
          :string object
          :type type)
  (error "~S signalled outside of ~S."
         'input-not-of-required-type 'accept))
