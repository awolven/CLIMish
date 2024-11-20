(in-package :climish)

;; NIL means that the default history has unbounded length.
;;--- Should the default be bigger?  unbounded?
(defparameter *default-history-length* 40)

(defclass basic-history
          ()
    ((name :initform "Unnamed" :initarg :name
           :reader history-name)
     (elements :initform nil                        ;the contents of the history
               :reader history-elements)
     (current-length :initform 0)                ;current length of the history
     (maximum-length :initform *default-history-length*
                     :initarg :maximum-length        ;NIL means no bound on length
                     :reader history-maximum-length)
     (rotation :initform 0)                        ;state for m-Y
     (yank-position :initform 0)                ;c-Y/c-m-Y to m-Y communication
     (temporary-element :initform nil)))        ;can be bound to temporary front of list

(defmethod print-object ((history basic-history) stream)
  (print-unreadable-object (history stream :type t :identity t)
    (write-string (slot-value history 'name) stream)))

;; On strings, EQUAL does case-sensitive comparisons.
(defmethod history-elements-equal ((history basic-history) element1 element2)
  (equal element1 element2))

;; Default method for matching an element.  Assumes a string representation.
(defmethod history-match-element ((history basic-history) element string)
  (search string element))

;; Is this element visible in the history?  The default answer is yes.
(defmethod history-element-visible-p ((history basic-history) element)
  (declare (ignore element))
  t)

;; Default method for printing an element.  Assumes a string representation.
(defmethod print-history-element ((history basic-history) element stream)
  (write-string element stream))

(defmethod history-and-element ((history basic-history) element)
  (cons history element))

(defmethod history-element-type ((history basic-history))
  'basic-history-element)

#+NIL
(defmethod display-history-menu-element ((history basic-history) stream number index
                                         &optional element-type)
  (when (null element-type)
    (setq element-type (history-element-type history)))
  (with-slots (rotation) history
    (let ((element (history-element history index)))
      (with-output-as-presentation
          (stream (history-and-element history element) element-type)
        (cond ((zerop rotation)
               (format stream "~3D: " number))
              ((= index rotation)
               (format stream "*~3D: " number))
              (t
               (format stream " ~3D: " number)))
        (print-history-element history element stream))
      (terpri stream))))

;; Number of elements in the history, including TEMPORARY-ELEMENT.
(defmethod history-length ((history basic-history))
  (with-slots (current-length temporary-element) history
    (if temporary-element (1+ current-length) current-length)))

(defmethod (setf history-maximum-length) (length (history basic-history))
  (with-slots (current-length elements maximum-length) history
    (when (and (not (null length))
               (< length current-length))
      (let ((new-elements
              (let ((new nil)
                    (elts elements))
                (repeat length
                  (push (pop elts) new))
                (nreverse new))))
         (setq elements new-elements
               current-length length)))
    (setq maximum-length length)))

;; Get the element from HISTORY indexed by INDEX.
(defmethod history-element ((history basic-history) index &optional fixup-p)
  (declare (ignorable fixup-p))
  (let ((delta 0))
    (with-slots (elements current-length temporary-element) history
      (#+SBCL sb-assem:without-scheduling #+SBCL () #-SBCL without-scheduling
        (when temporary-element
          (when (or (zerop index) (and fixup-p (minusp index)))
            (return-from history-element (values temporary-element 0)))
          (decf index)
          (incf delta))
        (cond ((minusp index)
               (values (and fixup-p (first elements)) delta))
              ((< index current-length)
               (values (nth index elements) (+ index delta)))
              (fixup-p
               (values (car (last elements)) (+ current-length -1 delta)))
              (t nil))))))

;;; Presentation histories

(defclass presentation-history (basic-history)
    ((type :initarg :type :reader presentation-history-type)))

(defstruct (presentation-history-element (:type list))
  object
  type
  ;; Cache the printed representation for a particular view
  (string nil)
  (view nil))

(defmacro with-default-bound-in-history (history default-element &body body)
  `(flet ((with-default-bound-in-history-body () ,@body))
     (declare (dynamic-extent #'with-default-bound-in-history-body))
     (invoke-with-default-bound-in-history ,history ,default-element
                                           #'with-default-bound-in-history-body)))

(defmethod invoke-with-default-bound-in-history
           ((history basic-history) default-element continuation)
  (with-slots (yank-position rotation temporary-element) history
    ;; Reset both pieces of state so that multiple calls to ACCEPT start fresh
    (setq yank-position 0
          rotation 0)
    (letf-globally ((temporary-element default-element))
      (funcall continuation))))
