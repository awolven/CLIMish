(in-package :climish)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *gensymbol* 0)
  
(defun gensymbol (&rest parts)
  (declare (dynamic-extent parts))
  (when (null parts) (setf parts '(gensymbol)))
  (make-symbol (format nil "~{~A-~}~D" parts (incf *gensymbol*))))
)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC (a pathname designator) does not
designate a directory, PATHSPEC otherwise.  It is irrelevant
whether file or directory designated by PATHSPEC does actually
exist."
  (and (not (component-present-p (pathname-name pathspec)))
       (not (component-present-p (pathname-type pathspec)))
       pathspec))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p pathspec)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname :directory (butlast directory)
                         :name (pathname-name name-and-type)
                         :type (pathname-type name-and-type)
                         :defaults pathname))
        pathname)))

(defmacro define-group (name type &body body)
  (declare (ignore name type))
  `(progn ,@body))

;;; Characters that are ordinary text rather than potential input editor commands.
;;; Note that GRAPHIC-CHAR-P is true of #\Space
(defun ordinary-char-p (char)
  (and (eql char (code-char (char-code char))) ; false for #\control-c
       (or (graphic-char-p char)
	   ;; For characters, CHAR= and EQL are the same.  Not true of EQ!
	   (eql char #\Newline)
	   (eql char #\Return)
	   (eql char #\Tab))))

(defun whitespace-char-p (char)
  (and (characterp char)
       (or (char-equal char #\Space)
	   (eql char #\Tab))))

(defmacro dorest ((var list &optional (by 'cdr)) &body body)
  `(do ((,var ,list (,by ,var)))
       ((null ,var) nil)
     ,@body))

;;; Interning. There are three places where the package matters here:
;;; the current package, the package that is current when FORMAT is
;;; called (matters for ~S &c), and the package into which the symbol
;;; is interned.  The aim is that the first two should always be the
;;; current packag, but you get to choose where the symbol is interned
;;; for PACKAGE-FINTERN.
;;;
;;; I am not sure if this stuff is really worth the cost, but...

;;; spr24505, carefully extract symbol-names, so
;;; case-sensitity works better.
(defun package-fintern (package format-string &rest format-args)
  ;; this argument order is unfortunate.
  (declare (dynamic-extent format-args))
  (intern (let ((pkg *package*))
	    (let ((*package* pkg))
		  (apply #'format () format-string
			 (mapcar #'(lambda (x)
				     (if (symbolp x)
					 (symbol-name x)
					 x))
				 format-args))))
	  package))

(defun fintern (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply #'package-fintern *package* format-string format-args))

(defmacro writing-clauses (&body body)
  (let ((clauses-var (gensymbol 'clauses)))
    `(let ((,clauses-var nil))
       (macrolet ((clause (clause)
		    `(push ,clause ,',clauses-var)))
	 ,@body)
       (nreverse ,clauses-var))))

;#-Genera
;;; can't hack return-from in macro for aclpc +++pr
(defmacro letf-globally (places-and-vals &body body)
  ;; I don't want to use LETF-globally, mind you, but I can't easily implement
  ;; LETF{-not-globally} without something like sys:%bind-location.
  ;; Of course, this one is really LETF*-GLOBALLY, but don't tell anyone.
  ;; A minor optimization: when you bind something to itself or don't
  ;;  say what to bind it to, it doesn't get SETF'd, since it isn't
  ;;  being changed.
 (if (null places-and-vals)
  `(progn ,@body)
  (let ((let-forms nil)
	(set-forms nil)
	(unwind-forms nil))
    ;; remember that we can't use SCL:LOOP
    (map nil #'(lambda (place-and-val)
		 (let* ((place (pop place-and-val))
			(val-p (not (null place-and-val)))
			(val (and val-p (pop place-and-val)))
			(temp-var (gensymbol 'letf-globally-temp)))
		   (when (and val-p (equal place val)) (setf val-p nil))   ;bind to itself?
		   (push `(,temp-var ,place) let-forms)
		   (when val-p (push place set-forms) (push val set-forms))
		   (push temp-var unwind-forms) (push place unwind-forms)))
	 places-and-vals)
    `(let ,(nreverse let-forms)
       (unwind-protect
	   (progn (setf ,@(nreverse set-forms)) ,@body)
	 (setf ,@unwind-forms))))))

;; Define this so we don't have to deal with stupid warnings about
;; the iteration variable being used, or not used, or what not
(defmacro repeat (n &body body)
  (let ((i '#:i))
    `(dotimes (,i ,n)
       ,@body)))

;; rfe5546 - evaluate the from-end argument at run-time
;;   This is a deviation from the old definition which evaluated from-end 
;;   (twice) at compile-time.
;;   All CLIM uses are compile-time constants. 
;;   Any non-contant usage would have generaed a compiler error or very
;;   incorrect code.
(defmacro dovector ((var vector &key (start 0) end from-end simple-p) 
		    &body body)
  (unless (constantp simple-p)
    (setq simple-p nil)
    (warn "SIMPLE-P should be a constant, ignoring it"))
  (when (and simple-p (null end))
    (warn "When SIMPLE-P is T, you must supply :END"))
  (let ((fvector '#:vector)
	(startd  '#:start)
	(endd    '#:end)
	(limit   '#:limit)
	(fromend '#:from-end)
	(variable (if (atom var) var (first var)))
	(index    (if (atom var) '#:index (second var)))
	(aref (if simple-p 'svref 'aref)))
    `(block nil
       (let* ((,fvector ,vector)
	      (,startd ,start)
	      (,endd ,(if simple-p 
			  `,end 
			`(or ,end (length ,fvector))))
	      (,fromend ,from-end)
	      (,index (if ,fromend (1- ,endd) ,startd))
	      (,limit (if ,fromend (1- ,startd) ,endd)))
	 (declare (type fixnum ,endd ,index ,limit)
		  ;; Turn on the afterburners...
		  (optimize (speed 3) (safety 0))
		  ,(if simple-p
		       `(type simple-vector ,fvector)
		     `(type vector ,fvector)))
	 (loop
	   (when (= ,index ,limit) (return))
	   (let ((,variable (,aref ,fvector ,index)))
	     ,@body)
	   (if ,fromend (decf ,index) (incf ,index)))))))

(defmacro doseq ((var sequence) &body body)
  (let ((fcn (gensymbol 'doseq)))
    `(flet ((,fcn (,var) ,@body))
       (etypecase ,sequence
	 (list (dolist (thing ,sequence) (,fcn thing)))
	 (vector (dovector (thing ,sequence) (,fcn thing)))))))
;;; Arglist tools

;; (flatten-arglist '(foo bar &optional baz (quux)
;; 		          &rest mumble
;; 		          &key frotz (trouble) ((:izzy the-cat))))
;; (FOO BAR &OPTIONAL BAZ QUUX &REST MUMBLE &KEY FROTZ TROUBLE IZZY)
;; make-pass-on-arglist is the only caller
(defun flatten-arglist (arglist)
  (let ((new-arglist nil)
	(mode :positional))
    (dolist (arg-spec arglist)
      (cond ((listp arg-spec)
	     (case mode
	       (&optional
		 (push (first arg-spec) new-arglist))
	       (&key
		 ;; deal with "(... &key ((:keyword var) default))" syntax
		 (let ((thing (first arg-spec)))
		   (push (if (listp thing)
			     (let ((name (first thing)))
			       (if (eq (symbol-package name) *keyword-package*)
				   (intern (symbol-name name))
				   name))
			     thing)
			 new-arglist)))))
	    ((member arg-spec '(&key &rest &optional))
	     (setq mode arg-spec)
	     (push arg-spec new-arglist))
	    (t (push arg-spec new-arglist))))
    (nreverse new-arglist)))

;; (make-pass-on-arglist '(foo bar &optional baz (quux)
;; 		          &rest mumble
;; 		          &key frotz (trouble) ((:izzy the-cat))))
;; (FOO BAR BAZ QUUX MUMBLE :FROTZ FROTZ :TROUBLE TROUBLE ':IZZY THE-CAT)
;; --- It looks like &REST and &KEY don't get along well here.  A big
;; question in such a circumstane is who should be doing the defaulting?
;; I.e., should this do the defaulting, by making the above arglist be
;; 	:FROTZ FROTZ :TROUBLE TROUBLE ':IZZY THE-CAT MUMBLE
;; for apply, or should it let the eventual caller do the defaulting by
;; punting the defaulting here and just doing
;;	MUMBLE
;; for apply?  There are also interactions with &allow-other-keys that
;; aren't done here.  If this is for pass-on, then &allow-other-keys
;; implies an &rest beforehand, otherwise it is impossible to get all
;; the keys that are being passed on.  There's also the question of
;; whether &allow-other-keys here should put :allow-other-keys in the
;; passed on call list.
(defun make-pass-on-arglist (arglist)
  (let ((new-arglist nil)
	(fa (flatten-arglist arglist))
	(mode :positional)
	(apply-p nil))
    (do ((args fa (cdr args))
	 (original-args arglist (cdr original-args)))
	((null args) nil)
      (let ((arg (first args))
	    (arg-spec (first original-args)))
	(cond ((member arg '(&key &optional &rest))
	       (setq mode arg))
	      ((eq arg '&allow-other-keys)
	       (unless (eq mode '&key)
		 (error "~&&ALLOW-OTHER-KEYS must follow &KEY")))
	      (t (case mode
		   (&key
		     (let ((arg-name arg) (arg-var arg))
		       (cond ((and (listp arg-spec)
				   (listp (first arg-spec)))
			      (setq arg-name `',(first (first arg-spec)))
			      (setq arg-var (second (first arg-spec))))
			     (t (setq arg-name (intern (symbol-name arg) *keyword-package*))))
		       (push arg-name new-arglist)
		       (push arg-var new-arglist)))
		   (&rest (setq apply-p t)
			  (push arg new-arglist))
		   (t (push arg new-arglist)))))))
    (values
      (nreverse new-arglist)
      apply-p)))

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


(defmacro with-collection (&body body)
  `(let (($with-collection-result$ nil)
         $with-collection-tail$)
     (macrolet
         ((collect (form)
            ;;  The FORM is evaluated first so that COLLECT nests
            ;; properly, i.e., The test to determine if this is
            ;; the first value collected should be done after the
            ;; value itself is generated in case it does
            ;; collection as well.
            `(let (($collectable$ ,form))
               (if $with-collection-tail$
                   (rplacd $with-collection-tail$
                           (setq $with-collection-tail$
                                 (list $collectable$)))
                   (setq $with-collection-result$
                         (setq $with-collection-tail$
                               (list $collectable$))))
               $with-collection-tail$)))
       ,@body $with-collection-result$)))

;;; Why PUSHNEW doesn't do this is beyond me.
(defmacro push-unique (item reference &rest args &key test test-not key)
  (declare (ignore test test-not))
  `(let* ((evaled-item ,item)
	  (evaled-reference ,reference)
	  (element (find ,(if key `(funcall ,key evaled-item) 'evaled-item)
			 evaled-reference ,@args)))
     (setf ,reference
	   (if element
	       (substitute evaled-item element evaled-reference)
	       (cons evaled-item evaled-reference)))))

(defmacro defun-inline (name lambda-list &body body)
  `(progn ;; define-group ,name defun-inline
;;;; don't use define-group, because it does a excl::record-source-file,
;;;; which will be also done by defun!  This causes duplicate definition in
;;;; file warnings.
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (proclaim '(inline ,name)))
     (defun ,name ,lambda-list
       ,@body)))

(defmacro with-warnings-for-definition (name type &body body)
  (declare (ignore name type)) ;-- Why?
  `(let () ,@body))

(defmacro with-keywords-removed ((new-list list keywords-to-remove) &body body)
  `(let ((,new-list (remove-keywords ,list ,keywords-to-remove)))
     ,@body))

(defun remove-keywords (list keywords)
  (macrolet ((remove-keywords-1 (name-var predicate-form)
	       `(let ((head nil)
		      (tail nil))
		  (do ()
		      ((null list))
		    (let ((,name-var (pop list))
			  (value (pop list)))
		      (unless ,predicate-form
			(setq tail (setq head (list ,name-var value)))
			(return))))
		  (do ()
		      ((null list) head)
		    (let ((,name-var (pop list))
			  (value (pop list)))
		      (unless ,predicate-form
			(setq tail (setf (cddr tail) (list ,name-var value)))))))))
    (cond ((null list) nil)
	  ((null keywords) list)
	  ;; Special case: use EQ instead of MEMBER when only one keyword is supplied.
	  ((null (cdr keywords))
	   (let ((keyword (car keywords)))
	     (remove-keywords-1 name (eq name keyword))))
	  (t
	   (remove-keywords-1 name (member name keywords))))))

(defun canonicalize-and-match-lambda-lists (canonical-order user-specified
					    &optional allow-user-keys)
  (check-type canonical-order list)
  (check-type user-specified list)
  (let* ((new-lambda-list nil)
	 (ignores nil)
	 (rest-pos (or (position '&rest user-specified) (length user-specified)))
	 (key-pos (or (position '&key user-specified) rest-pos))
	 (rest-and-key (nthcdr (min rest-pos key-pos) user-specified)))
    (when allow-user-keys
      (setq user-specified (subseq user-specified 0 (min rest-pos key-pos))))
    (flet ((user-var-symbol (entry)
	     ;; FOO | (FOO NIL) | ((:FOO BAR) NIL) | (FOO NIL FOO-P) | ((:FOO BAR) FOO-P)
	     ;;--- We don't support the FOO-P syntax yet.
	     (cond ((atom entry)
		    entry)
		   ((atom (setq entry (first entry)))
		    entry)
		   (t (second entry))))
	   (user-var-name (entry)
	     ;; FOO | (FOO NIL) | ((:FOO BAR) NIL) | (FOO NIL FOO-P) | ((:FOO BAR) FOO-P)
	     ;;--- We don't support the FOO-P syntax yet.
	     (cond ((atom entry)
		    entry)
		   ((atom (setq entry (first entry)))
		    entry)
		   (t (first entry)))))
      (declare (dynamic-extent #'user-var-symbol #'user-var-name))
      (dolist (canonical-var canonical-order)
	(let ((user-entry (first (member canonical-var user-specified
					 :test #'string-equal
					 :key #'user-var-name))))
	(cond (user-entry
	       (push (user-var-symbol user-entry) new-lambda-list)
	       (setq user-specified (remove user-entry user-specified)))
	      (t (let ((canonical-gensym (get canonical-var 'canonical-gensym)))
		   (unless canonical-gensym
		     (setq canonical-gensym (make-symbol (symbol-name canonical-var)))
		     (setf (get canonical-var 'canonical-gensym) canonical-gensym))
		   (push canonical-gensym new-lambda-list)
		   (push canonical-gensym ignores))))))
      (when (set-difference user-specified '(&key &allow-other-keys))
	(error "The arguments ~S aren't valid for this lambda list."
	       user-specified))
      (values (if allow-user-keys
		  (append (nreverse new-lambda-list) rest-and-key)
		  (nreverse new-lambda-list))
	      (nreverse ignores)))))
