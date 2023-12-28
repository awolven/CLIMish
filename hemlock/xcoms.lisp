;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains commands and support specifically for X related features.
;;;
;;; Written by Bill Chiles.
;;;

(in-package :hemlock)


(defhcommand region-to-cut-buffer-command (window p)
  (:pretty-name "Region to Cut Buffer")
  (:documentation "Place the current region into the X cut buffer.")
  (:help-string "Place the current region into the X cut buffer."))

(defmethod region-to-cut-buffer-command (window p)  
  (declare (ignore p))
  (store-cut-string (hi::bitmap-device-display
                     (hi::device-hunk-device (hi::window-hunk window)))
                    (region-to-string (current-region))))

(defhcommand insert-cut-buffer-command (window p)
  (:pretty-name "Insert Cut Buffer")
  (:documentation "Insert the X cut buffer at current point.")
  (:help-string "Insert the X cut buffer at current point.  Returns nil when it is empty."))

(defmethod insert-cut-buffer-command (window p)
  (declare (ignore p))
  (let ((str (fetch-cut-string (hi::bitmap-device-display
                                (hi::device-hunk-device
                                 (hi::window-hunk (current-window)))))))
    (if str
        (let ((point (current-point)))
          (push-buffer-mark (copy-mark point))
          (insert-string (current-point) str))
        (editor-error "X cut buffer empty.")))
  (setf (last-command-type) :ephemerally-active))
