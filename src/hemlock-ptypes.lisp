(in-package :climish)

(defun accept-something (frame)
  (catch :accept
    (recursive-command-loop frame)))

(define-presentation-type hemlock::command-name ()
  :inherit-from 'string
  :history nil)

(define-presentation-method accept ((type hemlock::command-name)
				    (stream application-frame-mixin) view &key)
  (let ((hi::*parse-string-tables* (list hi::*command-names*))
	(hi::*parse-value-must-exist* t)
	(hi::*parse-verification-function* #'keyword-verification-function)
	(hi::*parse-type* :keyword))
    (accept-something stream)))

(define-presentation-type hemlock::file ()
  :inherit-from 'string
  :history nil)

(define-presentation-method accept ((type hemlock::file)
				    (stream application-frame-mixin) view &key)
  (let ((hi::*parse-verification-function* #'file-verification-function)
	(hi::*parse-type* :file))
    (accept-something stream)))

(define-presentation-type hemlock::buffer-name ()
  :inherit-from 'string
  :history nil)

(define-presentation-method accept ((type hemlock::buffer-name)
				    (stream application-frame-mixin) view &key)
  (let ((hi::*parse-string-tables* (list hi::*buffer-names*))
	(hi::*parse-value-must-exist* t)
	(hi::*parse-verification-function* #'buffer-verification-function)
	(hi::*parse-type* :keyword))
    (accept-something stream)))


(define-presentation-type hemlock::buffer ())

(define-presentation-method accept ((type hemlock::buffer)
				    (stream application-frame-mixin) view &key)
  (let ((buffer-name (clim:accept 'hemlock::buffer-name :stream stream)))

    (hi::getstring buffer-name hi::*buffer-names*)))

(define-presentation-method accept ((type string)
				    (stream application-frame-mixin) view &key)
  (let ((hi::*parse-verification-function* #'(lambda (frame string)
					       (declare (ignore frame))
					       (list string))))
    (accept-something stream)))

(define-presentation-method accept ((type 3dm.d::vec3)
				  (stream application-frame-mixin) view &key)
  (let ((hi::*parse-verification-function* #'(lambda (frame string)
					       (declare (ignore frame))
					       (block test
						 (let ((*read-eval* nil)
						       (thing))
						   (handler-bind ((error (lambda (c)
									   (declare (ignore c))
									   (return-from test nil))))
						     (setq thing (read-from-string string))
						     (setq thing (eval thing))
						     (when (typep thing '3dm.d::vec3)
						       (list thing))))))))
    (accept-something stream)))
							 


  
