;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Rob MacLachlan
;;;
;;;  This file defines a ring-buffer type and access functions.
;;;
(in-package :hemlock-internals)

(defun %print-hkill-ring (obj stream depth)
  (declare (ignore depth obj))
  (write-string "#<Hemlock Kill Ring>" stream))

;;;; The kill-ring data structures:
;;;
(defclass kill-ring-element-mixin (krma.gui::ring-element-mixin)
  ())

(defclass kill-ring-head (kill-ring-element-mixin)
  ((display :initarg :display
            :accessor kill-ring-display)
   (max-size :initarg :max-size
             :accessor kill-ring-max-size)))

(defmethod kill-ring-element-value ((ring kill-ring-head))
  (clui::copy-string-from-clipboard (kill-ring-display ring)))

(defmethod (setf kill-ring-element-value) ((value string)
                                           (ring kill-ring-head))
  (clui::copy-string-to-clipboard (kill-ring-display ring) value))
  
  

(defmethod kill-ring-element-index ((ring kill-ring-head))
  0)

(defclass kill-ring-element (kill-ring-element-mixin)
  ((value :accessor kill-ring-element-value)
   (index :accessor kill-ring-element-index :initform 1)))

(defun make-kill-ring (display size)
  (assert (> size 0))
  (let ((head (make-instance 'kill-ring-head
                             :display display
                             :max-size size)))
    (krma.gui::ring-initialize head)
    head))

(defgeneric ring-push (object ring)
  (:documentation "Push an object into a ring, deleting an element if necessary."))

(defmethod ring-push (region (ring kill-ring-head))
  (let ((window (krma.gui::focused-window (krma::main-window krma::*app*))))
    (when window
      (with-buffer-globals ((window-buffer window))
        (let* ((string (.region-to-string. region))
               (new (make-instance 'kill-ring-element))
               (prev (krma.gui::prev ring)))
    
          
    
          (when (= (kill-ring-max-size ring)
                   (1+ (kill-ring-element-index prev)))
            ;; drop off the oldest one
            (setf (krma.gui::next prev) ring
                  (krma.gui::prev ring) (krma.gui::prev prev)))

          (setf (kill-ring-element-value new)
                (kill-ring-element-value ring))

          (setf (krma.gui::next new) (krma.gui::next ring))
          (setf (krma.gui::prev new) ring)
          (setf (krma.gui::next ring) new)
          (setf (kill-ring-element-value ring) string)
          (unless (eq (krma.gui::prev ring) ring)
            (setf (kill-ring-element-index (krma.gui::prev ring))
                  (1+ (kill-ring-element-index
                       (krma.gui::prev (krma.gui::prev ring)))))))))
    (values)))


;;; ring-pop  --  Public
;;;
;;;    Increment first modulo the maximum size.
;;;
(defgeneric ring-pop (ring)
  (:documentation "Pop an object from a ring and return it."))

(defmethod ring-pop ((ring kill-ring-head))
  (unless (eq (krma.gui::next ring) ring)
    (let* ((next (krma.gui::next ring))
           (value (kill-ring-element-value next)))
      (clui::copy-string-to-clipboard
       (kill-ring-display ring)
       (kill-ring-element-value next))
      (setf (krma.gui::prev next) (krma.gui::prev ring))
      (change-class next 'kill-ring-head
                    :display (kill-ring-display ring)
                    :max-size (kill-ring-max-size ring))
      (setf (kill-ring-element-value next) value)))
  ring)
      


;;; ring-length  --  Public
;;;
;;;    Return the current and maximum size.
;;;
(defgeneric ring-length (ring)
  (:documentation "Return as values the current and maximum size of a ring."))

(defmethod ring-length ((ring kill-ring-head))
  (values (1+ (kill-ring-element-index (krma.gui::prev ring)))
          (kill-ring-max-size ring)))
  

;;; ring-ref  --  Public
;;;
;;;    Do modulo arithmetic to find the correct element.
;;;
(defgeneric ring-ref (ring index)
  (:documentation "Return the index'th element of a ring.  This can be set with Setf."))

(defmethod ring-ref ((ring kill-ring-head) index)
  (declare (fixnum index))
  
  (block nil
    (let ((real-index
            (mod index
                 (1+
                  (kill-ring-element-index (krma.gui::prev ring))))))
      
      (do ((i 0 (1+ i))
           (current ring (krma.gui::next current)))
           (nil)
          (when (= i real-index)
            (return (kill-ring-element-value current)))))))


;;; (SETF RING-REF)  --  Internal
;;;
;;;    Setf form for ring-ref, set a ring element.
;;;
(defgeneric (setf ring-ref) (value ring index)
  (:documentation "Set an element in a ring."))

(defmethod (setf ring-ref) (value (ring kill-ring-head) index)
  (block nil
    (let ((real-index
            (mod index
                 (1+
                  (kill-ring-element-index (krma.gui::prev ring))))))
      
      (do ((i 0 (1+ i))
           (current ring (krma.gui::next current)))
          (nil)
          (when (= i real-index)
            (return (setf (kill-ring-element-value current) value)))))))

;;; rotate-ring  --  Public
;;;
;;;    Rotate a ring, blt'ing elements as necessary.
;;;
(defun kill-ring-shift-right (element)
  (let ((old-value))
    (dotimes (i (kill-ring-element-index (krma.gui::prev element)))
      (setq old-value (kill-ring-element-value (krma.gui::next element)))
      (setf (kill-ring-element-value (krma.gui::next element))
            (kill-ring-element-value element))
      (setq element (krma.gui::next element)))
    
    (setf (kill-ring-element-value element) old-value))
  (values))

(defun kill-ring-shift-left (element)
  (let ((old-value (kill-ring-element-value element)))
    (dotimes (i (kill-ring-element-index (krma.gui::prev element)))
      (setf (kill-ring-element-value element)
            (kill-ring-element-value (krma.gui::next element)))
      (setq element (krma.gui::next element)))
    
    (setf (kill-ring-element-value element) old-value))
  (values))

(defgeneric rotate-ring (ring offset)
  (:documentation
   "Rotate a ring forward, i.e. second -> first, with positive offset,
  or backwards with negative offset."))

(defmethod rotate-ring (ring offset)
  (if (zerop offset)
      ring
      (if (minusp offset)
          (loop for i from 0 below (- offset)
                do (kill-ring-shift-right ring)
                finally (return ring))
          (loop for i from 0 below offset
                do (kill-ring-shift-left ring)
                finally (return ring)))))
            
