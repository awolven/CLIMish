;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Slave Apropos (as opposed to "hemlock com>mand name apropos" aka Apropos)

(in-package :hemlock)



(defvar *apropos-entries* nil)
(defvar *apropos-entries-end* nil)
;;;

(defstruct (apropos-entry
             (:constructor internal-make-apropos-entry
                           (slavesym kind docstring)))
  slavesym
  kind
  docstring)

(defun parse-apropos-entry (slot-list)
  (destructuring-bind (slavesym &optional (kind :unbound) docstring)
                      slot-list
    (internal-make-apropos-entry
     slavesym
     kind
     (if (eq docstring :not-documented) nil docstring))))

;;; This is the apropos buffer if it exists.
;;;
(defvar *apropos-buffer* nil)

;;; This is the cleanup method for deleting *apropos-buffer*.
;;;
(defun delete-apropos-buffers (buffer)
  (when (eq buffer *apropos-buffer*)
    (setf *apropos-buffer* nil)
    (setf *apropos-entries* nil)))


;;;; Commands.



(defhcommand apropos-quit-command (window p)
  (:pretty-name "Apropos Quit")
  (:documentation "Kill the apropos buffer.")
  (:help-string ""))

(defmethod apropos-quit-command (window p)
  (declare (ignore p))
  (when *apropos-buffer* (delete-buffer-if-possible *apropos-buffer* (window-buffer window))))

(defun apropos-entry-from-mark (mark)
  )

(defhcommand apropos-find-definition-command (window p)
  (:pretty-name "Apropos Find Definition")
  (:documentation "")
  (:help-string ""))

(defmethod apropos-find-definition-command (window p)
  (declare (ignore p))
  (let ((entry (apropos-entry-from-mark (buffer-point (window-buffer window)))))
    (when entry
      (change-to-definition entry))))

(defhcommand apropos-describe-command (window p)
  (:pretty-name "Apropos Describe")
  (:documentation "")
  (:help-string ""))

(defmethod apropos-describe-command (window p)
  (declare (ignore p))
  (let ((entry (apropos-entry-from-mark (buffer-point (window-buffer window)))))
    (when entry
      (change-to-definition entry))))

(defun refresh-apropos (buf entries)
  (with-writable-buffer (buf)
    (delete-region buf (buffer-region buf))
    (setf *apropos-entries-end* (length entries))
    (setf *apropos-entries* (coerce entries 'vector))
    (with-output-to-mark (s buf (buffer-point buf))
      (dolist (entry entries)
        (apropos-write-line entry s)))))

(defun make-apropos-buffer (current-window entries)
  (let ((buf (or *apropos-buffer*
                 (make-buffer "*Slave Apropos*" :modes '("Apropos")))))
    (setf *apropos-buffer* buf)
    (refresh-apropos buf entries)
    (let ((fields (buffer-modeline-fields *apropos-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :apropos-cmds)
                      (make-modeline-field
                       :name :apropos-cmds :width 18
                       :function
                       #'(lambda (buffer window)
                           (declare (ignore buffer window))
                           "  Type ? for help.")))))
      (setf (buffer-modeline-fields *apropos-buffer*) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf current-window)))

(defun apropos-write-line (entry s)
  (format s "~A~%  ~:(~A~)~:[~;: ~:*~A~]~%~%"
          (apropos-entry-slavesym entry)
          (apropos-entry-kind entry)
          (let ((docstring (apropos-entry-docstring entry)))
            (when docstring
              (first-line-of-string docstring)))))

(defun first-line-of-string (str)
  (with-input-from-string (s str) (read-line s nil "")))

(defhcommand apropos-help-command (window p)
  (:pretty-name "Apropos Help")
  (:documentation "Show this help.")
  (:help-string "Show this help."))

(defmethod apropos-help-command (window p)
  (declare (ignore p))
  (describe-mode-command window nil "Apropos"))

(defhcommand slave-apropos-ignoring-point-command
            (window p &optional str)
  (:pretty-name "Slave Apropos Ignoring Point")
  (:documentation "")
  (:help-string ""))

(defmethod slave-apropos-ignoring-point-command
    (window p &optional (str
                         (hemlock-interface::prompt-for-string window
                                                               :prompt "Apropos string: ")))
  (declare (ignore p))
  (slave-apropos str))

(defhcommand slave-apropos-command (window p)
  (:pretty-name "Slave Apropos")
  (:documentation "")
  (:help-string ""))

(defmethod slave-apropos-command (window p)
  (declare (ignore p))
  (let ((default (hemlock::symbol-string-at-point)))
    ;; Fixme: MARK-SYMBOL isn't very good, meaning that often we
    ;; will get random forms rather than a symbol.  Let's at least
    ;; catch the case where the result is more than a line long,
    ;; and give up.
    (when (find #\newline default)
      (setf default nil))
    (slave-apropos
     (hemlock-interface::prompt-for-string
      window
      :prompt "Apropos string: "
      :default default))))

(defun slave-apropos (str)
  (hemlock::eval-in-slave `(%apropos ',str)))

(defun %apropos (str)
  (let ((data
         (mapcar (lambda (sym)
                   (cons (make-slave-symbol sym)
                         (conium:describe-symbol-for-emacs sym)))
                 (apropos-list str))))
    (hemlock::eval-in-master `(%apropos-results ',data ',str))))

(defun %apropos-results (current-window data str)
  (let ((entries (mapcar #'parse-apropos-entry data)))
    (cond
     ((null data)
      (message "No apropos results for: ~A" str))
     (t
      (make-apropos-buffer (krma.gui::focused-window (clui::window-parent *echo-area-window*)) entries)))))
