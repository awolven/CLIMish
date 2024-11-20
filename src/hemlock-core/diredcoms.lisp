;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Simple directory editing support.
;;; This file contains site dependent calls.
;;;
;;; Written by Blaine Burks and Bill Chiles.
;;;

(in-package :hemlock)





(defstruct (dired-information (:print-function print-dired-information)
                              (:conc-name dired-info-))
  pathname              ; Pathname of directory.
  pattern               ; FILE-NAMESTRING with wildcard possibly.
  dot-files-p           ; Whether to include UNIX dot files.
  write-date            ; Write date of directory.
  files                 ; Simple-vector of dired-file structures.
  file-list)            ; List of pathnames for files, excluding directories.

(defun print-dired-information (obj str n)
  (declare (ignore n))
  (format str "#<Dired Info ~S>" (namestring (dired-info-pathname obj))))


(defstruct (dired-file (:print-function print-dired-file)
                       (:constructor make-dired-file (pathname)))
  pathname
  (deleted-p nil)
  (write-date nil))

(defun print-dired-file (obj str n)
  (declare (ignore n))
  (format str "#<Dired-file ~A>" (namestring (dired-file-pathname obj))))



;;;; "Dired" command.

;;; *pathnames-to-dired-buffers* is an a-list mapping directory namestrings to
;;; buffers that display their contents.
;;;
(defvar *pathnames-to-dired-buffers* ())

(make-modeline-field
 :name :dired-cmds :width 20
 :function
 #'(lambda (buffer window)
     (declare (ignore buffer window))
     "  Type ? for help.  "))

(defun dired-context? (application-frame)
  (let* ((homemade-window (climish::focused-window application-frame))
         (buffer (window-buffer homemade-window))
         (major-mode (and buffer (buffer-major-mode buffer))))
    (eq major-mode :dired)))

(defun view-context? (application-frame)
  (let* ((homemade-window (climish::focused-window application-frame))
         (buffer (window-buffer homemade-window))
         (major-mode (and buffer (buffer-major-mode buffer))))
    (eq major-mode :view)))

(defcommand dired-command (window p &optional directory)
  (:pretty-name "Dired")
  (:documentation
   "Prompts for a directory and edits it.  If a dired for that directory already
   exists, go to that buffer, otherwise create one.  With an argument, include
   UNIX dot files.")
  (:help-string "Prompts for a directory and edits it.  If a dired for that directory already
   exists, go to that buffer, otherwise create one.  With an argument, include
   UNIX dot files.")
  (:applicability (climish::hemlock-applicability climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-command (window p &optional (directory #p"~/"))
  (let* ((window (climish::find-or-create-window-of-class 'climish::dired-window))
         (info (if (hemlock-bound-p 'dired-information)
                   (if (eq window *echo-area-window*)
                       nil
                       (variable-value 'dired-information :buffer (window-buffer window)))))
         (buffer (dired-guts window nil
                             ;; Propagate dot-files property to subdirectory edits.
                             (or (and info (dired-info-dot-files-p info))
                                 p)
                             directory)))
      
    (setf (window-buffer window) buffer)
    (climish::focus-window window)
    (adhoc-scene-graph::rm-redraw-node (adhoc:send window climish::buffer-view climish::text))
    window))

(defhcommand dired-with-pattern-command (window p)
  (:pretty-name "Dired with Pattern")
  (:documentation
   "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dit files.")
  (:help-string
   "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dit files.")
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-with-pattern-command (window p)
  (dired-guts window t p nil))

(defun dired-guts (current-window patternp dot-files-p directory)
  (let* ((dpn (value (window-buffer current-window) pathname-defaults))
         (directory (or directory
                        (clim:accept 'file
                                     :current-window current-window
                                     :prompt "Edit Directory: "
                                     :help "Pathname to edit."
                                     :default (make-pathname
                                               :device (pathname-device dpn)
                                               :directory (pathname-directory dpn))
                                     :must-exist nil)))
         (pattern (if patternp
                      (clim:accept 'string
                                   :current-window current-window
                                   :prompt "Filename pattern: "
                                   :help "Type a filename with a single asterisk."
                                   :trim t)))
         (full-name (namestring (if pattern
                                    (merge-pathnames directory pattern)
                                    directory)))
         (name (concatenate 'simple-string "Dired " full-name))
         (buffer (cdr (assoc full-name *pathnames-to-dired-buffers*
                             :test #-win32 #'string=
                             #+win32 #'string-equal))))
    (declare (simple-string full-name))
    (setf (value (window-buffer current-window) pathname-defaults) (merge-pathnames directory dpn))
    (let ((b
            (cond (buffer
                   (when (and dot-files-p
                              (not (dired-info-dot-files-p
                                    (variable-value 'dired-information
                                                    :buffer buffer))))
                     (setf (dired-info-dot-files-p (variable-value 'dired-information
                                                                   :buffer buffer))
                           t)
                     (update-dired-buffer current-window directory pattern buffer))
                   buffer)
                  (t
                   (let ((buffer (make-instance 'buffer
                                                :name name :modes '(:dired)
                                                :modeline-fields
                                                (append (value (window-buffer current-window)
                                                               default-modeline-fields)
                                                        (list (modeline-field :dired-cmds)))
                                                :delete-hook (list 'dired-buffer-delete-hook))))
                     (unless (initialize-dired-buffer current-window directory pattern
                                                      dot-files-p buffer)
                       ;;(delete-buffer-if-possible buffer (window-buffer current-window))
                       (editor-error "No entries for ~A." full-name))
                     (push (cons full-name buffer) *pathnames-to-dired-buffers*)
                     buffer)))))
      
      b)))

;;; INITIALIZE-DIRED-BUFFER gets a dired in the buffer and defines some
;;; variables to make it usable as a dired buffer.  If there are no file
;;; satisfying directory, then this returns nil, otherwise t.
;;;
(defun initialize-dired-buffer (current-window directory pattern dot-files-p buffer)
  (setf (window-buffer current-window) buffer)
  (multiple-value-bind (pathnames dired-files)
                       (dired-in-buffer current-window directory pattern dot-files-p buffer)
    (if (zerop (length dired-files))
        nil
        (defhvar "Dired Information"
          "Contains the information neccessary to manipulate dired buffers."
          :buffer buffer
          :value (make-dired-information :pathname directory
                                         :pattern pattern
                                         :dot-files-p dot-files-p
                                         :write-date (file-write-date directory)
                                         :files dired-files
                                         :file-list pathnames)))))

;;; CALL-PRINT-DIRECTORY gives us a nice way to report PRINT-DIRECTORY errors
;;; to the user and to clean up the dired buffer.
;;;
(defun call-print-directory (current-window directory mark dot-files-p)
  (handler-case (with-output-to-mark (s (window-buffer current-window) mark :full)
                  (print-directory directory s
                                   :all dot-files-p
                                   :verbose t
                                   :return-list t))
    (error (condx)
      (delete-buffer-if-possible (window-buffer current-window) (window-buffer current-window))
      (editor-error "~A" condx))))

;;; DIRED-BUFFER-DELETE-HOOK is called on dired buffers upon deletion.  This
;;; removes the buffer from the pathnames mapping, and it deletes and buffer
;;; local variables referring to it.
;;;
(defun dired-buffer-delete-hook (buffer)
  (setf *pathnames-to-dired-buffers*
        (delete buffer *pathnames-to-dired-buffers* :test #'eq :key #'cdr)))



;;;; Dired deletion and undeletion.

(defcommand dired-delete-file-command (window p)
  (:pretty-name "Dired Delete File")
  (:documentation
   "Marks a file for deletion; signals an error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be flagged
   for deletion.")
  (:help-string
   "Marks a file for deletion; signals an error if not in a dired buffer.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-delete-file-command (window p)
  (dired-frob-deletion window p t))

(defcommand dired-undelete-file-command (window p)
  (:pretty-name "Dired Undelete File")
  (:documentation
   "Removes a mark for deletion; signals and error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be unflagged
   for deletion.")
  (:help-string
   "Removes a mark for deletion; signals and error if not in a dired buffer.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-undelete-file-command (window p)
  (dired-frob-deletion window p nil))

(defcommand dired-delete-file-and-down-line-command (window p)
  (:pretty-name "Dired Delete File and Down Line")
  (:documentation "Marks file for deletion and moves down a line.
   See \"Dired Delete File\".")
  (:help-string "Marks file for deletion and moves down a line.
   See \"Dired Delete File\".")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-delete-file-and-down-line-command (window p)
  (declare (ignore p))
  (dired-frob-deletion window nil t)
  (let ((buffer (window-buffer window)))
    (dired-down-line buffer (buffer-point buffer))))

(defcommand dired-undelete-file-and-down-line-command (window p)
  (:pretty-name "Dired Undelete File and Down Line")
  (:documentation "Marks file undeleted and moves down a line.
   See \"Dired Delete File\".")
  (:help-string "Marks file undeleted and moves down a line.
   See \"Dired Delete File\".")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))
  

(defmethod dired-undelete-file-and-down-line-command (window p)
  (declare (ignore p))
  (dired-frob-deletion window nil nil)
  (let ((buffer (window-buffer window)))
    (dired-down-line buffer (buffer-point buffer))))

(defcommand dired-delete-file-with-pattern-command (window p)
  (:pretty-name "Dired Delete File with Pattern")
  (:documentation "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\".")
  (:help-string "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\".")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-delete-file-with-pattern-command (window p)
  (declare (ignore p))
  (dired-frob-deletion window t t)
  (let ((buffer (window-buffer window)))
    (dired-down-line buffer (buffer-point buffer))))

(defcommand dired-undelete-file-with-pattern-command (window p)
  (:pretty-name "Dired Undelete File with Pattern")
  (:documentation "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\".")
  (:help-string "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\".")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-undelete-file-with-pattern-command (window p)
  (declare (ignore p))
  (dired-frob-deletion window t nil)
  (let ((buffer (window-buffer window)))
    (dired-down-line buffer (buffer-point buffer))))

;;; DIRED-FROB-DELETION takes arguments indicating whether to prompt for a
;;; pattern and whether to mark the file deleted or undeleted.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER, and if not in a dired buffer, signal
;;; an error.
;;;
(defun dired-frob-deletion (current-window patternp deletep)
  (unless (hemlock-bound-p 'dired-information :buffer (window-buffer current-window))
    (editor-error "Not in Dired buffer."))
  (let ((current-buffer (window-buffer current-window)))
    (with-mark ((mark (buffer-point current-buffer) :left-inserting))
      (let* ((dir-info (variable-value 'dired-information :buffer (window-buffer current-window)))
             (files (dired-info-files dir-info))
             (del-files
               (if patternp
                   (dired:pathnames-from-pattern
                    (clim:accept 'string
                                 :current-window current-window
                                 :prompt "Filename pattern: "
                                 :help "Type a filename with a single asterisk."
                                 :trim t)
                    (dired-info-file-list dir-info))
                   (list (dired-file-pathname
                          (array-element-from-mark current-buffer mark files)))))
             (note-char (if deletep #\D #\space)))
        (with-writable-buffer (current-buffer)
          (dolist (f del-files)
            (let* ((pos (position f files :test #'equal
                                          :key #'dired-file-pathname))
                   (dired-file (svref files pos)))
              (buffer-start mark current-buffer)
              (line-offset current-buffer mark pos 0)
              (setf (dired-file-deleted-p dired-file) deletep)
              (if deletep
                  (setf (dired-file-write-date dired-file)
                        (file-write-date (dired-file-pathname dired-file)))
                  (setf (dired-file-write-date dired-file) nil))
              (setf (next-character current-buffer mark) note-char))))))
    (climish:redraw-buffer current-window)))

(defun dired-down-line (buffer point)
  (line-offset buffer point 1)
  (when (blank-line-p buffer (mark-line point))
    (line-offset buffer point -1)))



;;;; Dired file finding and going to dired buffers.

(defcommand dired-edit-file-command (window p)
  (:pretty-name "Dired Edit File")
  (:documentation "Read in file or recursively \"Dired\" a directory.")
  (:help-string "Read in file or recursively \"Dired\" a directory.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-edit-file-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (when (blank-line-p buffer (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
                     (array-element-from-mark
                      buffer
                      point (dired-info-files (variable-value 'dired-information :buffer (window-buffer window)))))))
      (if (directoryp pathname)
          (dired-command window nil (hemlock-ext::directory-namestring2 pathname))
          (change-to-buffer (find-file-buffer window pathname) window)))
    #+NIL(climish::rm-redraw-node-recursively window)))

(defcommand dired-view-file-command (window p)
  (:pretty-name "Dired View File")
  (:documentation
   "Read in file as if by \"View File\" or recursively \"Dired\" a directory.
   This associates the file's buffer with the dired buffer.")
  (:help-string
   "Read in file as if by \"View File\".
   This associates the file's buffer with the dired buffer.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-view-file-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (when (blank-line-p buffer (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
                     (array-element-from-mark
                      buffer
                      point (dired-info-files (variable-value 'dired-information :buffer buffer))))))
      (if (directoryp pathname)
          (dired-command window nil (hemlock-ext::directory-namestring2 pathname))
          (let* ((dired-buf (window-buffer window))
                 (buffer (view-file-command window nil pathname)))
            (push #'(lambda (buffer)
                      (declare (ignore buffer))
                      (setf dired-buf nil))
                  (buffer-delete-hook dired-buf))
            (setf (variable-value 'view-return-function :buffer buffer)
                  #'(lambda ()
                      (if dired-buf
                          (change-to-buffer dired-buf window)
                          (dired-from-buffer-pathname-command window nil)))))))))

(defhcommand dired-from-buffer-pathname-command (window p)
  (:pretty-name "Dired from Buffer Pathname")
  (:documentation
   "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory.")
  (:help-string
   "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory."))

(defmethod dired-from-buffer-pathname-command (window p)
  (let* ((buffer (window-buffer window))
         (pathname (buffer-pathname buffer)))
    (if pathname
        (dired-command window p (hemlock-ext::directory-namestring2 pathname))
        (editor-error "No pathname associated with buffer."))))

(defcommand dired-up-directory-command (window p)
  (:pretty-name "Dired Up Directory")
  (:documentation "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer.")
  (:help-string "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-up-directory-command (window p)
  (declare (ignore p))
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((dirs (or (pathname-directory
                   (dired-info-pathname (variable-value 'dired-information :buffer (window-buffer window))))
                  '(:relative))))
    (dired-command window nil
                   (truename (make-pathname :directory (nconc dirs '(:up)))))))



;;;; Dired misc. commands -- update, help, line motion.

(defcommand dired-update-buffer-command (window p)
  (:pretty-name "Dired Update Buffer")
  (:documentation
   "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified.")
  (:help-string
   "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-update-buffer-command (window p)
  (declare (ignore p))
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let* ((buffer (window-buffer window))
        (dir-info (variable-value 'dired-information :buffer buffer)))
    (update-dired-buffer window (dired-info-pathname dir-info)
                         (dired-info-pattern dir-info)
                         buffer)))

;;; UPDATE-DIRED-BUFFER updates buffer with a dired of directory, deleting
;;; whatever is in the buffer already.  This assumes buffer was previously
;;; used as a dired buffer having necessary variables bound.  The new files
;;; are compared to the old ones propagating any deleted flags if the name
;;; and the write date is the same for both specifications.
;;;
(defun update-dired-buffer (current-window directory pattern buffer)
  (with-writable-buffer (buffer)
    (delete-region buffer (buffer-region buffer))
    (let ((dir-info (variable-value 'dired-information :buffer buffer)))
      (multiple-value-bind (pathnames new-dired-files)
                           (dired-in-buffer current-window directory pattern
                                            (dired-info-dot-files-p dir-info)
                                            buffer)
        (let ((point (buffer-point buffer))
              (old-dired-files (dired-info-files dir-info)))
          (declare (simple-vector old-dired-files))
          (dotimes (i (length old-dired-files))
            (let ((old-file (svref old-dired-files i)))
              (when (dired-file-deleted-p old-file)
                (let ((pos (position (dired-file-pathname old-file)
                                     new-dired-files :test #'equal
                                     :key #'dired-file-pathname)))
                  (when pos
                    (let* ((new-file (svref new-dired-files pos))
                           (write-date (file-write-date
                                        (dired-file-pathname new-file))))
                      (when (= (dired-file-write-date old-file) write-date)
                        (setf (dired-file-deleted-p new-file) t)
                        (setf (dired-file-write-date new-file) write-date)
                        (setf (next-character
                               (line-offset (buffer-start point) pos 0)
                               buffer)
                              #\D))))))))
          (setf (dired-info-files dir-info) new-dired-files)
          (setf (dired-info-file-list dir-info) pathnames)
          (setf (dired-info-write-date dir-info)
                (file-write-date directory))
          (move-mark point (buffer-start-mark buffer)))))))

;;; DIRED-IN-BUFFER inserts a dired listing of directory in buffer returning
;;; two values: a list of pathnames of files only, and an array of dired-file
;;; structures.  This uses FILTER-REGION to insert a space for the indication
;;; of whether the file is flagged for deletion.  Then we clean up extra header
;;; and trailing lines known to be in the output (into every code a little
;;; slime must fall).
;;;
(defun dired-in-buffer (current-window directory pattern dot-files-p buffer)
  (with-buffer-globals (buffer)
    (let ((point (buffer-point buffer)))

      (with-writable-buffer (buffer)
        (let* ((pathnames (call-print-directory
                           current-window
			   (if pattern
			       (merge-pathnames directory pattern)
                               (pathname-as-directory directory)
			       #+NOTYET(merge-pathnames directory "*.*.~*~"))
                           point
                           dot-files-p))
               (dired-files (make-array (length pathnames))))
          (declare (list pathnames) (simple-vector dired-files))

          (filter-region buffer #'(lambda (str)
                                    (if (integerp str)
                                        (break))
                                    (concatenate 'simple-string "  " str))
                         (buffer-region buffer))
          (delete-characters buffer point -2)
          (delete-region buffer (line-to-region buffer (mark-line (buffer-start point buffer))))
          (delete-characters buffer point)
          (do ((p pathnames (cdr p))
               (i 0 (1+ i)))
              ((null p))
            (setf (svref dired-files i) (make-dired-file (car p))))
          (values (delete-if #'directoryp pathnames) dired-files))))))

#+NOTYET
(defcommand dired-help-command (window p)
  (:pretty-name "Dired Help")
  (:documentation "How to use dired.")
  (:help-string "How to use dired.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

#+NOTYET
(defmethod dired-help-command (window p)
  (declare (ignore p))
  (describe-mode-command window nil :dired))

(defcommand dired-next-file-command (window p)
  (:pretty-name "Dired Next File")
  (:documentation "Moves to next undeleted file.")
  (:help-string "Moves to next undeleted file.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-next-file-command (window p)
  (let ((buffer (window-buffer window)))
    (unless (dired-line-offset buffer (buffer-point buffer) (or p 1))
      (editor-error "Not enough lines."))))

(defcommand dired-previous-file-command (window p)
  (:pretty-name "Dired Previous File")
  (:documentation "Moves to previous undeleted file.")
  (:help-string "Moves to next undeleted file.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-previous-file-command (window p)
  (let ((buffer (window-buffer window)))
    (unless (dired-line-offset buffer (buffer-point buffer) (or p -1))
      (editor-error "Not enough lines."))))

;;; DIRED-LINE-OFFSET moves mark n undeleted file lines, returning mark.  If
;;; there are not enough lines, mark remains unmoved, this returns nil.
;;;
(defun dired-line-offset (buffer mark n)
  (with-mark ((m mark))
    (let ((step (if (plusp n) 1 -1)))
      (dotimes (i (abs n) (move-mark mark m))
        (loop
          (unless (line-offset m step 0)
            (return-from dired-line-offset nil))
          (when (blank-line-p buffer (mark-line m))
            (return-from dired-line-offset nil))
          (when (char= (next-character buffer m) #\space)
            (return)))))))



;;;; Dired user interaction functions.

(defun dired-error-function (string &rest args)
  (apply #'editor-error string args))

(defun dired-report-function (string &rest args)
  (clear-echo-area)
  (apply #'message string args))

(defun dired-yesp-function (current-window string &rest args)
  (clim:accept 'y-or-n :current-window current-window :prompt (cons string args) :default t))



;;;; Dired expunging and quitting.

(defcommand dired-expunge-files-command (window p)
  (:pretty-name "Dired Expunge Files")
  (:documentation
   "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\".")
  (:help-string
   "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\".")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-expunge-files-command (window p)
  (declare (ignore p))
  (when (expunge-dired-files window)
    (dired-update-buffer-command window nil))
  (maintain-dired-consistency window))

(defcommand dired-quit-command (window p)
  (:pretty-name "Dired Quit")
  (:documentation "Expunges the files in a dired buffer and then exits.")
  (:help-string "Expunges the files in a dired buffer and then exits.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-quit-command (window p)
  (declare (ignore p))
  (expunge-dired-files window)
  (delete-buffer-if-possible (window-buffer window) (window-buffer window)))



(defun expunge-dired-files (current-window)
  (multiple-value-bind (marked-files marked-dirs) (get-marked-dired-files (window-buffer current-window))
    (let ((dired:*error-function* #'dired-error-function)
          (dired:*report-function* #'dired-report-function)
          (dired:*yesp-function* #'dired-yesp-function)
          (we-did-something nil))
      (when (and marked-files
                 (or (not (variable-value 'dired-file-expunge-confirm :buffer (window-buffer current-window)))
                     (clim:accept 'y-or-n
                                  :current-window current-window
                                  :prompt "Really delete files? "
                                  :default t
                                  :must-exist t
                                  :default-string "Y")))
        (setf we-did-something t)
        (dolist (file-info marked-files)
          (let ((pathname (car file-info))
                (write-date (cdr file-info)))
            (if (= write-date (file-write-date pathname))
                (dired:delete-file (namestring pathname) :clobber t
                                   :recursive nil)
                (message "~A has been modified, it remains unchanged."
                         (namestring pathname))))))
      (when marked-dirs
        (dolist (dir-info marked-dirs)
          (let ((dir (car dir-info))
                (write-date (cdr dir-info)))
            (if (= write-date (file-write-date dir))
                (when (or (not (variable-value 'dired-directory-expunge-confirm :buffer (window-buffer current-window)))
                          (clim:accept 'y-or-n
                                       :current-window current-window
                                       :prompt (list "~a is a directory. Delete it? "
                                                     (hemlock-ext::directory-namestring2 dir))
                                       :default t
                                       :must-exist t
                                       :default-string "Y"))
                  (dired:delete-file (hemlock-ext::directory-namestring2 dir) :clobber t
                                     :recursive t)
                  (setf we-did-something t))
                (message "~A has been modified, it remains unchanged.")))))
      we-did-something)))





(defcommand dired-copy-file-command (window p)
  (:pretty-name "Dired Copy File")
  (:documentation "Copy the file under the point")
  (:help-string "Copy the file under the point")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-copy-file-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (confirm (variable-value 'dired-copy-file-confirm :buffer (window-buffer window)))
         (source (dired-file-pathname
                  (array-element-from-mark
                   buffer
                   point (dired-info-files (variable-value 'dired-information :buffer (window-buffer window))))))
         (dest (clim:accept 'file
                            :current-window window
                            :prompt (if (directoryp source)
                                        "Destination Directory Name: "
                                        "Destination Filename: ")
                            :help "Name of new file."
                            :default source
                            :must-exist nil))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
                                 :clobber (not confirm)))
  (maintain-dired-consistency window))

(defcommand dired-rename-file-command (window p)
  (:pretty-name "Dired Rename File")
  (:documentation "Rename the file or directory under the point")
  (:help-string "Rename the file or directory under the point")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-rename-file-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (source (dired-namify (dired-file-pathname
                                (array-element-from-mark
                                 buffer
                                 point
                                 (dired-info-files (variable-value 'dired-information :buffer buffer))))))
         (dest (clim:accept 'file
                            :current-window window
                            :prompt "New Filename: "
                            :help "The new name for this file."
                            :default source
                            :must-exist nil))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    ;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
    (dired:rename-file source dest :clobber (variable-value 'dired-rename-file-confirm :buffer buffer)))
  (maintain-dired-consistency window))

(defcommand dired-copy-with-wildcard-command (window p)
  (:pretty-name "Dired Copy with Wildcard")
  (:documentation "Copy files that match a pattern containing ONE wildcard.")
  (:help-string "Copy files that match a pattern containing ONE wildcard.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-copy-with-wildcard-command (window p)
  (declare (ignore p))
  (let* ((dir-info (variable-value 'dired-information :buffer (window-buffer window)))
         (confirm (variable-value 'dired-copy-file-confirm  :buffer (window-buffer window)))
         (pattern (clim:accept 'string
                               :current-window window
                               :prompt "Filename pattern: "
                               :help "Type a filename with a single asterisk."
                               :trim t))
         (destination (namestring
                       (clim:accept 'file
                                    :current-window window
                                    :prompt "Destination Spec: "
                                    :help "Destination spec.  May contain ONE asterisk."
                                    :default (dired-info-pathname dir-info)
                                    :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*yesp-function* #'dired-yesp-function)
         (dired:*report-function* #'dired-report-function))
    (dired:copy-file pattern destination :update (if (eq confirm :update) t nil)
                     :clobber (not confirm)
                     :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency window))

(defcommand dired-rename-with-wildcard-command (window p)
  (:pretty-name "Dired Rename with Wildcard")
  (:documentation "Rename files that match a pattern containing ONE wildcard.")
  (:help-string "Rename files that match a pattern containing ONE wildcard.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod dired-rename-with-wildcard-command (window p)
  (declare (ignore p))
  (let* ((dir-info (variable-value 'dired-information :buffer (window-buffer window)))
         (pattern (clim:accept 'string
                               :current-window window
                               :prompt "Filename pattern: "
                               :help "Type a filename with a single asterisk."
                               :trim t))
         (destination (namestring
                       (clim:accept 'file
                                    :current-window window
                                    :prompt "Destination Spec: "
                                    :help "Destination spec.  May contain ONE asterisk."
                                    :default (dired-info-pathname dir-info)
                                    :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*yesp-function* #'dired-yesp-function)
         (dired:*report-function* #'dired-report-function))
    (dired:rename-file pattern destination
                       :clobber (not (variable-value 'dired-rename-file-confirm :buffer (window-buffer window)))
                       :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency window))

(defcommand delete-file-command (window p)
  (:pretty-name "Delete File")
  (:documentation "Delete a file.  Specify directories with a trailing slash.")
  (:help-string "Delete a file.  Specify directories with a trailing slash.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod delete-file-command (window p)
  (declare (ignore p))
  (let* ((spec (namestring
                (clim:accept 'file
                             :current-window window
                             :prompt "Delete File: "
                             :help '("Name of File or Directory to delete.  ~
                          One wildcard is permitted.")
                             :must-exist nil)))
         (directoryp (directoryp spec))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (when (or (not directoryp)
              (not (variable-value 'dired-directory-expunge-confirm :buffer (window-buffer window)))
              (clim:accept 'y-or-n
                           :current-window window
               :prompt (list "~A is a directory. Delete it? "
                             (hemlock-ext::directory-namestring2 spec))
               :default t :must-exist t :default-string "Y")))
    (dired:delete-file spec :recursive t
                            :clobber (or directoryp
                                         (variable-value 'dired-file-expunge-confirm :buffer (window-buffer window)))))
  (maintain-dired-consistency window))

(defcommand copy-file-command (window p)
  (:pretty-name "Copy File")
  (:documentation "Copy a file, allowing ONE wildcard.")
  (:help-string "Copy a file, allowing ONE wildcard.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod copy-file-command (window p)
  (declare (ignore p))
  (let* ((confirm (variable-value 'dired-copy-file-confirm :buffer (window-buffer window)))
         (source (namestring
                  (clim:accept 'file
                               :current-window window
                               :prompt "Source Filename: "
                               :help "Name of File to copy.  One wildcard is permitted."
                               :must-exist nil)))
         (dest (namestring
                (clim:accept 'file
                             :current-window window
                             :prompt (if (directoryp source)
                                         "Destination Directory Name: "
                                         "Destination Filename: ")
                             :help "Name of new file."
                             :default source
                             :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
                                 :clobber (not confirm)))
  (maintain-dired-consistency window))

(defcommand rename-file-command (window p)
  (:pretty-name "Rename File")
  (:documentation "Rename a file, allowing ONE wildcard.")
  (:help-string "Rename a file, allowing ONE wildcard.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod rename-file-command (window p)
  (declare (ignore p))
  (let* ((source (namestring
                  (clim:accept 'file
                               :current-window window
                               :prompt "Source Filename: "
                               :help "Name of file to rename.  One wildcard is permitted."
                               :must-exist nil)))
         (dest (namestring
                (clim:accept 'file
                             :current-window window
                             :prompt (if (directoryp source)
                                         "Destination Directory Name: "
                                         "Destination Filename: ")
                             :help "Name of new file."
                             :default source
                             :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (dired:rename-file source dest
                       :clobber (not (variable-value 'dired-rename-file-confirm :buffer (window-buffer window)))))
  (maintain-dired-consistency window))

(defun maintain-dired-consistency (current-window)
  (dolist (info *pathnames-to-dired-buffers*)
    (let* ((directory (hemlock-ext::directory-namestring2 (car info)))
           (buffer (cdr info))
           (dir-info (variable-value 'dired-information :buffer buffer))
           (write-date (file-write-date directory)))
      (unless (= (dired-info-write-date dir-info) write-date)
        (update-dired-buffer current-window directory (dired-info-pattern dir-info) buffer)))))



;;;; Dired utilities.

;;; GET-MARKED-DIRED-FILES returns as multiple values a list of file specs
;;; and a list of directory specs that have been marked for deletion.  This
;;; assumes the current buffer is a "Dired" buffer.
;;;
(defun get-marked-dired-files (window)
  (let* ((files (dired-info-files (variable-value 'dired-information :buffer (window-buffer window))))
         (length (length files))
         (marked-files ())
         (marked-dirs ()))
    (unless files (editor-error "Not in Dired buffer."))
    (do ((i 0 (1+ i)))
        ((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let* ((thing (svref files i))
             (pathname (dired-file-pathname thing)))
        (when (and (dired-file-deleted-p thing) ; file marked for delete
                   (probe-file pathname))       ; file still exists
          (if (directoryp pathname)
              (push (cons pathname (file-write-date pathname)) marked-dirs)
              (push (cons pathname (file-write-date pathname))
                    marked-files)))))))

;;; ARRAY-ELEMENT-FROM-MARK -- Internal Interface.
;;;
;;; This counts the lines between it and the beginning of the buffer.  The
;;; number is used to index vector as if each line mapped to an element
;;; starting with the zero'th element (lines are numbered starting at 1).
;;; This must use AREF since some modes use this with extendable vectors.
;;;
(defun array-element-from-mark (buffer mark vector
                                &optional (error-msg "Invalid line."))
  (when (blank-line-p buffer (mark-line mark)) (editor-error error-msg))
  (aref vector
         (1- (count-lines (region
                           (buffer-start-mark buffer)
                           mark)))))

;;; DIRED-NAMIFY and DIRED-DIRECTORIFY are implementation dependent slime.
;;;
(defun dired-namify (pathname)
  (let* ((string (namestring pathname))
         (last (1- (length string))))
    (if (char= (schar string last) #\/)
        (subseq string 0 last)
        string)))

;;;; View Mode.



(defun setup-view-mode (buffer)
  (defhvar "View Return Function"
    "Function that gets called when quitting or returning from view mode."
    :value nil
    :buffer buffer)
  (setf (buffer-writable buffer) nil))
;;;
(defun cleanup-view-mode (buffer current-buffer)
  (delete-variable 'view-return-function current-buffer :buffer buffer)
  (setf (buffer-writable buffer) t))

(defcommand view-file-command (window p &optional pathname)
  (:pretty-name "View File")
  (:documentation "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience.")
  (:help-string "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience.")
  (:applicability (dired-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod view-file-command (window p &optional pathname)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (pn (or pathname
                 (clim:accept 'file
                              :current-window window
                              :prompt "View File: " :must-exist t
                              :help "Name of existing file to read into its own buffer."
                              :default (buffer-default-pathname buffer))))
         (buffer (make-instance 'buffer :name (format nil "View File ~A" (gensym)))))
    (visit-file-command nil pn buffer)
    (setf (buffer-minor-mode buffer "View" buffer) t)
    (change-to-buffer buffer window)
    buffer))

(defcommand view-return-command (window p)
  (:pretty-name "View Return")
  (:documentation "Return to a parent buffer, if it exists.")
  (:help-string "Return to a parent buffer, if it exists.")
  (:applicability (view-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod view-return-command (window p)
  (declare (ignore p))
  (unless (call-view-return-fun (window-buffer window))
    (editor-error "No View return method for this buffer.")))

(defcommand view-quit-command (window p)
  (:pretty-name "View Quit")
  (:documentation "Delete a buffer in view mode.")
  (:help-string "Delete a buffer in view mode, invoking VIEW-RETURN-FUNCTION if it exists for
   this buffer.")
  (:applicability (view-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod view-quit-command (window p)
  (declare (ignore p))
  (let* ((buf (window-buffer window))
         (funp (call-view-return-fun (window-buffer window))))
    (delete-buffer-if-possible buf buf)
    (unless funp (editor-error "No View return method for this buffer."))))

;;; CALL-VIEW-RETURN-FUN returns nil if there is no current
;;; view-return-function.  If there is one, it calls it and returns t.
;;;
(defun call-view-return-fun (window)
  (if (hemlock-bound-p 'view-return-function :buffer (window-buffer window))
      (let ((fun (variable-value 'view-return-function :buffer (window-buffer window))))
        (cond (fun
               (funcall fun)
               t)))))



(defcommand view-scroll-down-command (window p)
  (:pretty-name "View Scroll Down")
  (:documentation
   "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer.")
  (:help-string
   "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer.")
  (:applicability (view-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod view-scroll-down-command (window p)
  (if (and (not p)
           (displayed-p (buffer-end-mark (window-buffer window))
                        window)
           (variable-value 'view-scroll-deleting-buffer :buffer (window-buffer window)))
      (view-quit-command window nil)
      (scroll-window-down-command window p)))

(defcommand view-edit-file-command (window p)
  (:pretty-name "View Edit File")
  (:documentation "Turn off \"View\" mode in this buffer.")
  (:help-string "Turn off \"View\" mode in this buffer.")
  (:applicability (view-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

(defmethod view-edit-file-command (window p)
  (declare (ignore p))
  (let ((buf (window-buffer window)))
    (setf (buffer-minor-mode buf "View" (window-buffer window)) nil)
    (warn-about-visit-file-buffers buf)))

#+NOTYET
(defcommand view-help-command (window p)
  (:pretty-name "View Help")
  (:documentation "Shows \"View\" mode help message.")
  (:help-string "Shows \"View\" mode help message.")
  (:applicability (view-context? climish::application-frame))
  (:preparation (climish::hemlock-arguments climish::application-frame))
  (:generic-function-class climish::not-everywhere-command))

#+NOTYET
(defmethod view-help-command (window p) 
  (declare (ignore p))
  (describe-mode-command nil "View"))
