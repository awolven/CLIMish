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
(defclass kill-ring-element-mixin (climish::ring-element-mixin)
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
    (climish::ring-initialize head)
    head))

#+REDUNDANT
(defgeneric ring-push (object ring)
  (:documentation "Push an object into a ring, deleting an element if necessary."))

(defmethod ring-push ((region region) (ring kill-ring-head))
  (let* ((window (climish::focused-window (krma::main-window clim:*default-frame-manager*)))
         (view (climish::focused-view window)))
    (when window
      (let* ((string (region-to-string (window-buffer view) region))
             (new (make-instance 'kill-ring-element))
             (prev (climish::prev ring)))
    
        (when (= (kill-ring-max-size ring)
                 (1+ (kill-ring-element-index prev)))
          ;; drop off the oldest one
          (setf (climish::next prev) ring
                (climish::prev ring) (climish::prev prev)))

        (setf (kill-ring-element-value new)
              (kill-ring-element-value ring))

        (setf (climish::next new) (climish::next ring))
        (setf (climish::prev new) ring)
        (setf (climish::next ring) new)
        (setf (kill-ring-element-value ring) string)
        (unless (eq (climish::prev ring) ring)
          (setf (kill-ring-element-index (climish::prev ring))
                (1+ (kill-ring-element-index
                     (climish::prev (climish::prev ring))))))))
    (values)))

(defmethod ring-push ((string string) (ring kill-ring-head))
  (let* ((new (make-instance 'kill-ring-element))
         (prev (climish::prev ring)))
    
    (when (= (kill-ring-max-size ring)
             (1+ (kill-ring-element-index prev)))
      ;; drop off the oldest one
      (setf (climish::next prev) ring
            (climish::prev ring) (climish::prev prev)))
    
    (setf (kill-ring-element-value new)
          (kill-ring-element-value ring))

    (setf (climish::next new) (climish::next ring))
    (setf (climish::prev new) ring)
    (setf (climish::next ring) new)
    (setf (kill-ring-element-value ring) string)
    (unless (eq (climish::prev ring) ring)
      (setf (kill-ring-element-index (climish::prev ring))
            (1+ (kill-ring-element-index
                 (climish::prev (climish::prev ring))))))))


;;; ring-pop  --  Public
;;;
;;;    Increment first modulo the maximum size.
;;;
#+REDUNDANT
(defgeneric ring-pop (ring)
  (:documentation "Pop an object from a ring and return it."))

(defmethod ring-pop ((ring kill-ring-head))
  (unless (eq (climish::next ring) ring)
    (let* ((next (climish::next ring))
           (value (kill-ring-element-value next)))
      (clui::copy-string-to-clipboard
       (kill-ring-display ring)
       (kill-ring-element-value next))
      (setf (climish::prev next) (climish::prev ring))
      (change-class next 'kill-ring-head
                    :display (kill-ring-display ring)
                    :max-size (kill-ring-max-size ring))
      (setf (kill-ring-element-value next) value)))
  ring)
      


;;; ring-length  --  Public
;;;
;;;    Return the current and maximum size.
;;;
#+REDUNDANT
(defgeneric ring-length (ring)
  (:documentation "Return as values the current and maximum size of a ring."))

(defmethod ring-length ((ring kill-ring-head))
  (values (1+ (kill-ring-element-index (climish::prev ring)))
          (kill-ring-max-size ring)))
  

;;; ring-ref  --  Public
;;;
;;;    Do modulo arithmetic to find the correct element.
;;;
#+REDUNDANT
(defgeneric ring-ref (ring index)
  (:documentation "Return the index'th element of a ring.  This can be set with Setf."))

(defmethod ring-ref ((ring kill-ring-head) index)
  (declare (fixnum index))
  
  (block nil
    (let ((real-index
            (mod index
                 (1+
                  (kill-ring-element-index (climish::prev ring))))))
      
      (do ((i 0 (1+ i))
           (current ring (climish::next current)))
           (nil)
          (when (= i real-index)
            (return (kill-ring-element-value current)))))))


;;; (SETF RING-REF)  --  Internal
;;;
;;;    Setf form for ring-ref, set a ring element.
;;;
#+REDUNDANT
(defgeneric (setf ring-ref) (value ring index)
  (:documentation "Set an element in a ring."))

(defmethod (setf ring-ref) (value (ring kill-ring-head) index)
  (block nil
    (let ((real-index
            (mod index
                 (1+
                  (kill-ring-element-index (climish::prev ring))))))
      
      (do ((i 0 (1+ i))
           (current ring (climish::next current)))
          (nil)
          (when (= i real-index)
            (return (setf (kill-ring-element-value current) value)))))))

;;; rotate-ring  --  Public
;;;
;;;    Rotate a ring, blt'ing elements as necessary.
;;;
(defun kill-ring-shift-right (element)
  (let ((old-value))
    (dotimes (i (kill-ring-element-index (climish::prev element)))
      (setq old-value (kill-ring-element-value (climish::next element)))
      (setf (kill-ring-element-value (climish::next element))
            (kill-ring-element-value element))
      (setq element (climish::next element)))
    
    (setf (kill-ring-element-value element) old-value))
  (values))

(defun kill-ring-shift-left (element)
  (let ((old-value (kill-ring-element-value element)))
    (dotimes (i (kill-ring-element-index (climish::prev element)))
      (setf (kill-ring-element-value element)
            (kill-ring-element-value (climish::next element)))
      (setq element (climish::next element)))
    
    (setf (kill-ring-element-value element) old-value))
  (values))

#+REDUNDANT
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
            
