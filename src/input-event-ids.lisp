(in-package :climish)

(defstruct (input-event-id (:print-function %print-input-event-id)
			       (:constructor %make-input-event-id (scancode modifier-bits action-bit)))
  (modifier-bits 0 :type fixnum)
  (scancode 0 :type fixnum)
  (action-bit 0 :type fixnum))

(defvar *input-event-id-high-bytes* (make-array 256 :initial-element nil))

(defun get-input-event-id* (input-code modifier-bits action-bit)
  (let* ((high-byte (ash input-code -8))
         (low-byte-vector (svref *input-event-id-high-bytes* high-byte)))
    
    (unless low-byte-vector
      (let ((new-vector (make-array 256 :initial-element nil)))
        (setf (svref *input-event-id-high-bytes* high-byte) new-vector)
        (setf low-byte-vector new-vector)))
    
    (let* ((low-byte (ldb (byte 8 0) input-code))
           (modifier-bit-vector (svref low-byte-vector low-byte)))
      
      (unless modifier-bit-vector
        (let ((new-vector (make-array 256 :initial-element nil)))
          (setf (svref low-byte-vector low-byte) new-vector)
          (setf modifier-bit-vector new-vector)))
      
      (let* ((action-bit-vector (svref modifier-bit-vector modifier-bits)))
	
	(unless action-bit-vector
	  (let ((new-vector (make-array 2 :initial-element nil)))
	    (setf (svref modifier-bit-vector modifier-bits) new-vector)
	    (setf action-bit-vector new-vector)))
	
	(let ((input-event-id (svref action-bit-vector action-bit)))
	  (if input-event-id
	      input-event-id
	      (setf (svref action-bit-vector action-bit) (%make-input-event-id input-code modifier-bits action-bit))))))))

(defun key-character-class (char)
  (case char
    (#\- :modifier-terminator)
    (#\_ :key-terminator)
    (#\Space :event-terminator)
    (#\" :eof)
    (t :unicode)))

(defconstant input-event-id-escape-char #\\
  "The escape character that #k uses.")

(defun get-key-char (stream)
  (let ((char (read-char stream t nil t)))
    (cond ((char= char input-event-id-escape-char)
           (let ((char (read-char stream t nil t)))
             (values char :escaped)))
          (t (values char (key-character-class char))))))

(defun skip-whitespace (&optional (stream *standard-input*))
  (peek-char t stream))

(defun input-event-id-modifier-mask (modifier-name)
  "This function returns a mask for modifier-name.  This mask is suitable
   for use with INPUT-EVENT-ID-BITS.  If modifier-name is undefined, this signals
   an error."
  (let ((res (cdr (assoc modifier-name clui::*modifiers-to-internal-masks*
                         :test #'string-equal))))
    (unless res (error "Undefined input-event-id modifier -- ~S." modifier-name))
    res))

(defmethod make-load-form ((self input-event-id) &optional environment)
  (declare (ignore environment))
  `(intern-input-event-id ,(input-event-id-scancode self)
			  ,(input-event-id-modifier-bits self)
			  ,(input-event-id-action-bit self)))

(defun intern-input-event-id (object modifier-bits action)
  "This returns a input-event-id described by object with bits.  Object is one of
   keysym, string, or input-event-id.  When object is a input-event-id, this uses
   INPUT-EVENT-ID-KEYSYM.  You can form bits with MAKE-INPUT-EVENT-ID-BITS or
   INPUT-EVENT-ID-MODIFIER-MASK."
  (let ((action-bit (if (numberp action)
			action
			(ecase action
			  (:press 0)
			  (:release 1)))))
    (etypecase object
      (integer
       (unless (aref clui::*keysyms* object)
	 (error "~S is an undefined input-code." object))
       (get-input-event-id* object modifier-bits action-bit))
      #|(character
      (let* ((name (char-name object))	;
      (keysym (name-keysym (or name (string object))))) ;
      (unless keysym			;
      (error "~S is an undefined keysym." object)) ;
      (get-input-event-id* keysym bits)))|#
      (string
       (let ((input-code (clui::keysym-input-code object)))
	 (unless input-code
           (error "~S is an undefined keysym." object))
	 (get-input-event-id* input-code modifier-bits action-bit)))
      (input-event-id
       (get-input-event-id* (input-event-id-scancode object) modifier-bits action-bit)))))

(defun %print-input-event-id (object stream ignore)
  (declare (ignore ignore))
  (write-string "#k\"" stream)
  (print-pretty-input-event-id object stream)
  (write-char #\" stream))

(defun print-pretty-input-event-id (input-event-id &optional (stream *standard-output*)
							     long-names-p)
  "This prints input-event-id to stream.  Long-names-p indicates whether modifier
   names should appear using the long name or short name."
  (do ((map (if long-names-p
                (cdr clui::*modifiers-to-internal-masks*)
                clui::*modifiers-to-internal-masks*)
            (cddr map)))
      ((null map))
    (when (not (zerop (logand (cdar map) (input-event-id-modifier-bits input-event-id))))
      (write-string (caar map) stream)
      (write-char #\- stream)))
  (let* ((name (aref clui::*keysyms* (input-event-id-scancode input-event-id))))
    (write-string name stream)
    (write-string (aref #("" "_Up") (input-event-id-action-bit input-event-id)) stream)))

(defun make-input-event-id-bits (&rest modifier-names)
  "This returns bits suitable for INTERN-INPUT-EVENT-ID from the supplied modifier
   names.  If any name is undefined, this signals an error."
  (let ((mask 0))
    (dolist (mod modifier-names mask)
      (let ((this-mask (cdr (assoc mod clui::*modifiers-to-internal-masks*
                                   :test #'string-equal))))
        (unless this-mask (error "~S is an undefined modifier name." mod))
        (setf mask (logior mask this-mask))))))

(defun make-input-event-id-action-bit (name)
  (if (string= name "Up")
      1
      (if (string= name "Dn")
	  0
	  (error "Unrecognized action ~S" name))))

(defun input-event-id-bits-modifiers (bits)
  "This returns a list of input-event-id modifier names, one for each modifier
   set in bits."
  (let ((res nil))
    (do ((map (cdr clui::*modifiers-to-internal-masks*) (cddr map)))
        ((null map) res)
      (when (logtest bits (cdar map))
        (push (caar map) res)))))

(defun parse-input-event-id (stream sub-char count)
  (declare (ignore sub-char count))
  (let ((id-namestring
	  (make-array 30 :adjustable t :fill-pointer 0 :element-type 'character)))
    
    (prog ((bits 0)
           (input-event-ids ())
	   (action :press)
           char class)
       (unless (char= (read-char stream) #\")
         (error "Keys must be delimited by ~S." #\"))
       ;; Skip any leading spaces in the string.
       (skip-whitespace stream)
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:ISO-start (go ISOCHAR))
         (:ISO-end (error "Angle brackets must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (error "No key to read.")))
     ID
       ;;(print "ID")
       (vector-push-extend char id-namestring)
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
	 (:key-terminator (go GOT-KEY))
         (:event-terminator (go GOT-CHAR))
         (:modifier-terminator (go GOT-MODIFIER))
         ((:ISO-start :ISO-end) (error "Angle brackets must be escaped."))
         (:EOF (go GET-LAST-CHAR)))
     GOT-CHAR
       ;;(print "GOT-CHAR")
       (push `(intern-input-event-id ,(copy-seq id-namestring) ,bits ,action)
             input-event-ids)
       (setf (fill-pointer id-namestring) 0)
       (setf bits 0)
       ;; Skip any whitespace between characters.
       (skip-whitespace stream)
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:ISO-start (go ISOCHAR))
         (:ISO-end (error "Angle brackets must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (go FINAL)))
     GOT-MODIFIER
       ;;(print "GOT-MODIFIER")
       (let ((modifier-name (car (assoc id-namestring
                                        clui::*modifiers-to-internal-masks*
                                        :test #'string-equal))))
         (unless modifier-name
           (error "~S is not a defined modifier." id-namestring))
         (setf (fill-pointer id-namestring) 0)
         (setf bits (logior bits (input-event-id-modifier-mask modifier-name))))
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:ISO-start (go ISOCHAR))
         (:ISO-end (error "Angle brackets must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (error "Expected something naming a input-event-id, got EOF.")))
     ISOCHAR
       ;;(print "ISOCHAR")
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :event-terminator :other :escaped)
          (vector-push-extend char id-namestring)
          (go ISOCHAR))
         (:ISO-start (error "Open Angle must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (error "Bad syntax in key specification, hit eof."))
         (:ISO-end (go GOT-CHAR)))
     GET-LAST-CHAR
       ;;(print "GET-LAST-CHAR")
       (push `(intern-input-event-id ,(copy-seq id-namestring) ,bits ,action)
             input-event-ids)
     GOT-KEY
       ;;(PRINT "GOT-KEY")
       (unless (char= char #\")
	 (setq action (make-input-event-id-action-bit
		       (loop with name = (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)
			     repeat 3
			     do (setq char (read-char stream))
			     when (eq char #\")
			       do (return name)
			     do (vector-push-extend char name))))
	 (push `(intern-input-event-id ,(copy-seq id-namestring) ,bits ,action)
	       input-event-ids))
       
       (go FINAL)
      
     FINAL
       ;;(print "FINAL")
       (return (if (cdr input-event-ids)
                   `(vector ,@(nreverse input-event-ids))
                   `,(car input-event-ids))))))

(set-dispatch-macro-character #\# #\k 'parse-input-event-id)

(defmethod input-event-id ((event clui::input-event-mixin))
  (intern-input-event-id (clui::input-event-code event)
			 (clui::event-modifier-state event)
			 (typecase event
			   (clui::button-release-event-mixin :release)
			   (t :press))))

(defmethod input-event-id ((event clui::character-event-mixin))
  (clui::input-event-character event))

(defmethod input-event-id ((event clim:pointer-enter-event))
  :pointer-enter)

(defmethod input-event-id ((event clim:pointer-exit-event))
  :pointer-exit)





