;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;
;;; This file contains Commands useful when running on a Unix box.  Hopefully
;;; there are no CMU Unix dependencies though there are probably CMU Common
;;; Lisp dependencies, such as RUN-PROGRAM.
;;;
;;; Written by Christopher Hoover.

(in-package :hemlock)



;;;; Region and File printing commands.

(defhvar "Print Utility"
  "UNIX(tm) program to invoke (via EXT:RUN-PROGRAM) to do printing.
   The program should act like lpr: if a filename is given as an argument,
   it should print that file, and if no name appears, standard input should
   be assumed."
  :value "lpr")

(defhvar "Print Utility Switches"
  "Switches to pass to the \"Print Utility\" program.  This should be a list
   of strings."
  :value ())


;;; PRINT-SOMETHING calls RUN-PROGRAM on the utility-name and args.  Output
;;; and error output are done to the echo area, and errors are ignored for
;;; now.  Run-program-keys are other keywords to pass to RUN-PROGRAM in
;;; addition to :wait, :output, and :error.
;;;
(defmacro print-something (&optional (run-program-keys)
                                     (utility-name '(value print-utility))
                                     (args '(value print-utility-switches)))
  (let ((pid (gensym))
        (error-code (gensym)))
    `(multiple-value-bind (,pid ,error-code)
                          (ext:run-program ,utility-name ,args
                                           ,@run-program-keys
                                           :wait t
                                           :output *echo-area-stream*
                                           :error *echo-area-stream*)
       (declare (ignore ,pid ,error-code))
       (force-output *echo-area-stream*)
       ;; Keep the echo area from being cleared at the top of the command loop.
       (setf (buffer-modified *echo-area-buffer*) nil))))


;;; PRINT-REGION -- Interface
;;;
;;; Takes a region and outputs the text to the program defined by
;;; the hvar "Print Utility" with options form the hvar "Print
;;; Utility Options" using PRINT-SOMETHING.
;;;
(defun print-region (region)
  #+NIL
  (with-input-from-region (s region)
    (print-something (:input s))))


(defhcommand print-buffer-command (window p)
  (:pretty-name "Print Buffer")
  (:documentation
   "Prints the current buffer using the program defined by the hvar
   \"Print Utility\" with the options from the hvar \"Print Utility
   Options\".   Errors appear in the echo area.")
  (:help-string "Prints the contents of the buffer."))

(defmethod print-buffer-command (window p)
  (declare (ignore p))
  (message "Printing buffer...~%")
  (print-region (buffer-region (current-buffer))))

(defhcommand print-region-command (window p)
  (:pretty-name "Print Region")
  (:documentation
   "Prints the current region using the program defined by the hvar
   \"Print Utility\" with the options from the hvar \"Print Utility
   Options\".  Errors appear in the echo area.")
  (:help-string "Prints the current region."))

(defmethod print-region-command (window p)
  (declare (ignore p))
  (message "Printing region...~%")
  (print-region (current-region)))

(defhcommand print-file-command (window p)
  (:pretty-name "Print File")
  (:documentation
   "Prompts for a file and prints it usings the program defined by
   the hvar \"Print Utility\" with the options from the hvar \"Print
   Utility Options\".  Errors appear in the echo area.")
  (:help-string "Prints a file."))

(defmethod print-file-command (window p)
  (declare (ignore p))
  (let* ((pn (prompt-for-file window :prompt "File to print: "
                              :help "Name of file to print."
                              :default (buffer-default-pathname (current-buffer))
                              :must-exist t))
         (ns (namestring (truename pn))))
    (message "Printing file...~%")
    (print-something () (value print-utility)
                     (append (value print-utility-switches) (list ns)))))


;;;; Scribe.

(defhcommand scribe-file-command (window p)
  (:pretty-name "Scribe File")
  (:documentation
   "Scribe a file with the default directory set to the directory of the
   specified file.  The output from running Scribe is sent to the
   \"Scribe Warnings\" buffer.  See \"Scribe Utility\" and \"Scribe Utility
   Switches\".")
  (:help-string
   "Scribe a file with the default directory set to the directory of the
   specified file."))

(defmethod scribe-file-command (window p)
  (declare (ignore p))
  (scribe-file (prompt-for-file window :prompt "Scribe file: "
                                :default
                                (buffer-default-pathname (current-buffer)))))

(defhvar "Scribe Buffer File Confirm"
  "When set, \"Scribe Buffer File\" prompts for confirmation before doing
   anything."
  :value t)

(defhcommand scribe-buffer-file-command (window p)
  (:pretty-name "Scribe Buffer File")
  (:documentation
   "Scribe the file associated with the current buffer.  The default directory
   set to the directory of the file.  The output from running Scribe is sent to
   the \"Scribe Warnings\" buffer.  See \"Scribe Utility\" and \"Scribe Utility
   Switches\".  Before doing anything the user is asked to confirm saving and
   Scribe'ing the file.  This prompting can be inhibited by with \"Scribe Buffer
   File Confirm\".")
  (:help-string
   "Scribe a file with the default directory set to the directory of the
   specified file."))

(defmethod scribe-buffer-file-command (window p)
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (pathname (buffer-pathname buffer))
         (modified (buffer-modified buffer)))
    (when (or (not (value scribe-buffer-file-confirm))
              (prompt-for-y-or-n
               window
               :default t :default-string "Y"
               :prompt (list "~:[S~;Save and s~]cribe file ~A? "
                             modified (namestring pathname))))
      (when modified (write-buffer-file buffer pathname))
      (scribe-file pathname))))

(defhvar "Scribe Utility"
  "Program name to invoke (via EXT:RUN-PROGRAM) to do text formatting."
  :value "scribe")

(defhvar "Scribe Utility Switches"
  "Switches to pass to the \"Scribe Utility\" program.  This should be a list
   of strings."
  :value ())

(defun scribe-file (pathname)
  (let* ((pathname (truename pathname))
         (out-buffer (or (getstring "Scribe Warnings" *buffer-names*)
                         (make-buffer "Scribe Warnings")))
         (out-point (buffer-end (buffer-point out-buffer)))
         (stream (make-hemlock-output-stream out-point :line))
         (orig-cwd (default-directory)))
    (buffer-end out-point)
    (insert-character out-point #\newline)
    (insert-character out-point #\newline)
    (unwind-protect
        (progn
          (setf (default-directory) (directory-namestring pathname))
          (ext:run-program (namestring (value scribe-utility))
                           (list* (namestring pathname)
                                  (value scribe-utility-switches))
                           :output stream :error stream
                           :wait nil))
      (setf (default-directory) orig-cwd))))


;;;; UNIX Filter Region

(defhcommand unix-filter-region-command (window p)
  (:pretty-name "Unix Filter Region")
  (:documentation "Unix Filter Region prompts for a UNIX program and then passes the current
  region to the program as standard input.  The standard output from the
  program is used to replace the region.  This command is undo-able.")
  (:help-string
   "UNIX-FILTER-REGION-COMMAND is not intended to be called from normal
  Hemlock commands; use UNIX-FILTER-REGION instead."))

(defmethod unix-filter-region-command (window p)
  (declare (ignore p))
  (let* ((region (current-region))
         (filter-and-args (prompt-for-string
                           window
                           :prompt "Filter: "
                           :help "Unix program to filter the region through."))
         (filter-and-args-list (listify-unix-filter-string filter-and-args))
         (filter (car filter-and-args-list))
         (args (cdr filter-and-args-list))
         (new-region (unix-filter-region (window-buffer window) region filter args))
         (start (copy-mark (region-start region) :right-inserting))
         (end (copy-mark (region-end region) :left-inserting))
         (old-region (region start end))
         (undo-region (delete-and-save-region old-region)))
    (ninsert-region end new-region)
    (make-region-undo (window-buffer window) :twiddle "Unix Filter Region" old-region undo-region)))

(defun unix-filter-region (buffer region command args)
  "Passes the region REGION as standard input to the program COMMAND
  with arguments ARGS and returns the standard output as a freshly
  cons'ed region."
  (let ((new-region (make-empty-region)))
    (with-input-from-region (input buffer region)
      (with-output-to-mark (output buffer (region-end new-region) :full)
        (ext:run-program command args
                         :input input
                         :output output
                         :error output)))
    new-region))

(defun listify-unix-filter-string (str)
  (declare (simple-string str))
  (let ((result nil)
        (lastpos 0))
    (loop
      (let ((pos (position #\Space str :start lastpos :test #'char=)))
        (push (subseq str lastpos pos) result)
        (unless pos
          (return))
        (setf lastpos (1+ pos))))
    (nreverse result)))



;;;; Man pages.

(defhcommand manual-page-command (window p)
  (:pretty-name "Manual Page")
  (:documentation "Read the Unix manual pages in a View buffer.
   If given an argument, this will put the man page in a Pop-up display.")
  (:help-string
   "Read the Unix manual pages in a View buffer.
   If given an argument, this will put the man page in a Pop-up display."))

(defmethod manual-page-command (window p)
  (let ((topic (prompt-for-string window :prompt "Man topic: ")))
    (if p
        (with-pop-up-display (stream)
          (execute-man topic stream))
        (let* ((buf-name (format nil "Man Page ~a" topic))
               (new-buffer (make-buffer buf-name :modes '("Fundamental" "View")))
               (buffer (or new-buffer (getstring buf-name *buffer-names*)))
               (point (buffer-point buffer)))
          (change-to-buffer buffer)
          (when new-buffer
            (setf (value view-return-function) #'(lambda ()))
            (with-writable-buffer (buffer)
              (with-output-to-mark (s (window-buffer window) point :full)
                (execute-man topic s))))
          (buffer-start point buffer)))))

(defun execute-man (topic stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
         (format nil "man ~a| ul -t adm3" topic))
   :output stream))
