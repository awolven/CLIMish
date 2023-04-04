;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles.
;;;

(in-package :hemlock)

(defmode "Overwrite")

(defcommand "Overwrite Mode" (p)
  "Printing characters overwrite characters instead of pushing them to the right.
   A positive argument turns Overwrite mode on, while zero or a negative
   argument turns it off.  With no arguments, it is toggled.  Use C-Q to
   insert characters normally."
    "Determine if in Overwrite mode or not and set the mode accordingly."
  (with-buffer-globals ((current-buffer))
    (setf (buffer-minor-mode .buffer. "Overwrite")
          (if p
              (plusp p)
              (not (buffer-minor-mode .buffer. "Overwrite"))))))


(defcommand "Self Overwrite" (p)
  "Replace the next character with the last character typed,
   but insert at end of line.  With prefix argument, do it that many times."
    "Implements ``Self Overwrite'', calling this function is not meaningful."
  (with-buffer-globals ((current-buffer))
    (let ((char (hemlock-ext:key-event-char *last-key-event-typed*))
          (point .point.))
      (unless char (editor-error "Can't insert that character."))
      (do ((n (or p 1) (1- n)))
          ((zerop n))
        (case (next-character point)
          (#\tab
           (let ((col1 (.mark-column. point))
                 (col2 (.mark-column. (mark-after point))))
             (if (= (- col2 col1) 1)
                 (setf (previous-character point) char)
                 (insert-character (mark-before point) char))))
          ((#\newline nil) (insert-character point char))
          (t (setf (next-character point) char)
           (mark-after point)))))))


(defcommand "Overwrite Delete Previous Character" (p)
  "Replaces previous character with space, but tabs and newlines are deleted.
   With prefix argument, do it that many times."
    "Replaces previous character with space, but tabs and newlines are deleted."
  (with-buffer-globals ((current-buffer))
    (do ((point .point.)
         (n (or p 1) (1- n)))
        ((zerop n))
      (case (previous-character point)
        ((#\newline #\tab) (delete-characters point -1))
        ((nil) (editor-error))
        (t (setf (previous-character point) #\space)
         (mark-before point))))))
