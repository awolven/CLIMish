;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains file/buffer manipulating commands.
;;;

(in-package :hemlock)

(defvar *buffer-history* ()
  "A list of buffers, in order from most recently to least recently selected.")

(defun change-to-buffer (buffer current-window)
  "Switches to buffer in the current window maintaining *buffer-history*."
  (let* ((cb (window-buffer current-window)))
    (setq *buffer-history* (cons cb (delq cb *buffer-history*)))
    (setf (current-buffer cb) buffer)
    (setf (window-buffer current-window) buffer)))

;;;; PROCESS-FILE-OPTIONS.

(defvar *mode-option-handlers* ()
  "Do not modify this; use Define-File-Option instead.")

(defvar *file-type-hooks* ()
  "Do not modify this; use Define-File-Type-Hook instead.")

(defun trim-subseq (string start end)
  (declare (simple-string string))
  (string-trim '(#\Space #\Tab) (subseq string start end)))

;;; PROCESS-FILE-OPTIONS checks the first line of buffer for the file options
;;; indicator "-*-".  IF it finds this, then it enters a do-file-options block.
;;; If any parsing errors occur while picking out options, we return from this
;;; block.  Staying inside this function at this point, allows us to still set
;;; a major mode if no file option specified one.
;;;
;;; We also cater to old style mode comments:
;;;    -*- Lisp -*-
;;;    -*- Text -*-
;;; This kicks in if we find no colon on the file options line.
;;;
(defun process-file-options (buffer &optional
                                    (pathname (buffer-pathname buffer)))
  "Checks for file options and invokes handlers if there are any.  If no
   \"Mode\" mode option is specified, then this tries to invoke the appropriate
   file type hook."
  (let* ((string
          (line-string buffer (mark-line (buffer-start-mark buffer))))
         (found (search "-*-" string))
         (no-major-mode t)
         (type (if pathname (pathname-type pathname))))
    (declare (simple-string string))
    (when found
      (block do-file-options
        (let* ((start (+ found 3))
               (end (search "-*-" string :start2 start)))
          (unless end
            (loud-message "No closing \"-*-\".  Aborting file options.")
            (return-from do-file-options))
          (cond
           ((find #\: string :start start :end end)
            (do ((opt-start start (1+ semi)) colon semi)
                (nil)
              (setq colon (position #\: string :start opt-start :end end))
              (unless colon
                (loud-message "Missing \":\".  Aborting file options.")
                (return-from do-file-options))
              (setq semi (or (position #\; string :start colon :end end) end))
              (let* ((option (nstring-downcase
                              (trim-subseq string opt-start colon)))
                     (handler (assoc option *mode-option-handlers*
                                     :test #'string=)))
                (declare (simple-string option))
                (cond
                 (handler
                  (let ((result (funcall (cdr handler) buffer
                                         (trim-subseq string (1+ colon) semi))))
                    (when (string= option "mode")
                      (setq no-major-mode (not result)))))
                 (t (message "Unknown file option: ~S" option)))
                (when (= semi end) (return nil)))))
           (t
            ;; Old style mode comment.
            (setq no-major-mode nil)
            (funcall (cdr (assoc "mode" *mode-option-handlers* :test #'string=))
                     buffer (trim-subseq string start end)))))))
    (when (and no-major-mode type)
      (let ((hook (assoc (string-downcase type) *file-type-hooks*
                         :test #'string=)))
        (when hook (funcall (cdr hook) buffer type))))))



;;;; File options and file type hooks.

(defmacro define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
   Defines a new file option to be user in the -*- line at the top of a file.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name *mode-option-handlers*  :test #'string=)
                    (car (push (cons ,name nil) *mode-option-handlers*))))
           #'(lambda ,lambda-list ,@body))))

(define-file-option "Mode" (buffer str)
  (let ((seen-major-mode-p nil)
        (lastpos 0))
    (loop
      (let* ((pos (position #\, str :start lastpos))
             (keyword (intern (string-upcase (trim-subseq str lastpos pos)) :keyword)))
        (cond ((gethash keyword *mode-names*)
               (cond ((mode-major-p keyword)
                      (when seen-major-mode-p
                        (loud-message
                         "Major mode already processed. Using ~S now."
                         keyword))
                      (setf seen-major-mode-p t)
                      (setf (buffer-major-mode buffer nil) keyword))
                     (t
                      (setf (buffer-minor-mode buffer keyword nil) t))))
              (t
               (loud-message "~S is not a defined mode -- ignored." keyword)))
        (unless pos
          (return seen-major-mode-p))
        (setf lastpos (1+ pos))))))


(defmacro define-file-type-hook (type-list (buffer type) &body body)
  "Define-File-Type-Hook ({Type}*) (Buffer Type) {Form}*
  Define some code to be evaluated when a file having one of the specified
  Types is read by a file command.  Buffer is bound to the buffer the
  file is in, and Type is the actual type read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,type) ,@body))
       (dolist (,str ',(mapcar #'string-downcase type-list))
         (setf (cdr (or (assoc ,str *file-type-hooks*  :test #'string=)
                        (car (push (cons ,str nil) *file-type-hooks*))))
               #',fun)))))





;;;; Support for file hacking commands:



(defun buffer-default-pathname (buffer)
  "Returns \"Buffer Pathname\" if it is bound.  If it is not, and buffer's name
   is composed solely of alphnumeric characters, then return a pathname formed
   from the buffer's name.  If the buffer's name has other characters in it,
   then return the value of \"Last Resort Pathname Defaults Function\" called
   on buffer."
  (or (buffer-pathname buffer)
      (if (every #'alphanumericp (the simple-string (buffer-name buffer)))
          (merge-pathnames (make-pathname :name (buffer-name buffer))
                           (value pathname-defaults))
          (funcall (value last-resort-pathname-defaults-function) buffer))))

(defun buffer-default-directory (buffer)
  "Like buffer-default-pathname, but prefer a directory."
  (let ((p
         (or (buffer-pathname buffer)
             (if (every #'alphanumericp (buffer-name buffer))
                 (merge-pathnames (make-pathname :name (buffer-name buffer))
                                  (value pathname-defaults))
                 (funcall (value last-resort-pathname-defaults-function)
                          buffer)))))
    (make-pathname :name nil :type nil :defaults p)))

(defun pathname-to-buffer-name (pathname)
  "Returns a simple-string using components from pathname."
  (let ((pathname (pathname pathname)))
    (concatenate 'simple-string
                 (file-namestring pathname)
                 " "
                 (directory-namestring pathname))))



;;;; File hacking commands.

(defhcommand process-file-options-command (window p)
  (:pretty-name "Process File Options")
  (:documentation "Reprocess this buffer's file options.")
  (:help-string "Reprocess this buffer's file options."))

(defmethod process-file-options-command (window p)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (process-file-options buffer)))

(defhcommand insert-file-command (window p &optional pathname buffer)
  (:pretty-name "Insert File")
  (:documentation "Inserts a file which is prompted for into the current buffer at the point.
  The prefix argument is ignored.")
  (:help-string "Inserts the file named by Pathname into Buffer at the point."))

(defmethod insert-file-command (window p &optional pathname (buffer (window-buffer window)))
  (declare (ignore p))
  (let* ((pn (or pathname
                 (prompt-for-file window :default (buffer-default-pathname buffer)
                                  :prompt "Insert File: "
                                  :help "Name of file to insert")))
         (point (buffer-point buffer))
         ;; start and end will be deleted by undo stuff
         (start (copy-mark point :right-inserting))
         (end (copy-mark point :left-inserting))
         (region (region start end)))
    (setv pathname-defaults pn)
    (push-buffer-mark buffer (copy-mark end))
    (read-file buffer pn end)
    (make-region-undo buffer :delete "Insert File" region)))

(defhcommand write-region-command (window p &optional pathname)
  (:pretty-name "Write Region")
  (:documentation "Writes the current region to a file. ")
  (:help-string "Writes the current region to a file. "))

(defmethod write-region-command (window p &optional pathname)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (let ((region (current-region buffer))
          (pn (or pathname
                  (prompt-for-file window :prompt "File to Write: "
                                   :help "The name of the file to write the region to. "
                                   :default (buffer-default-pathname buffer)
                                   :must-exist nil))))
      (write-file buffer region pn)
      (message "~A written." (namestring (truename pn))))))



;;;; Visiting and reverting files.

(defhcommand visit-file-command (window p &optional pathname buffer)
  (:pretty-name "Visit File")
  (:documentation
   "Replaces the contents of Buffer with the file Pathname.  The prefix
   argument is ignored.  The buffer is set to be writable, so its region
   can be deleted.")
  (:help-string
   "Replaces the contents of the current buffer with the text in the file
   which is prompted for.  The prefix argument is, of course, ignored p times."))

(defmethod visit-file-command (window p &optional pathname (buffer (window-buffer window)))
  (declare (ignore p))
  (when (and (buffer-modified buffer)
             (prompt-for-y-or-n window :prompt "Buffer is modified, save it? "))
    (save-file-command () buffer))
  (let ((pn (or pathname
                (prompt-for-file window :prompt "Visit File: "
                                 :must-exist nil
                                 :help "Name of file to visit."
                                 :default (buffer-default-pathname buffer)))))
    (setf (buffer-writable buffer) t)
    (read-buffer-file pn buffer)
    (let ((n (pathname-to-buffer-name (buffer-pathname buffer))))
      (unless (getstring n *buffer-names*)
        (setf (buffer-name buffer) n))
      (warn-about-visit-file-buffers buffer))))

(defun warn-about-visit-file-buffers (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (dolist (b *buffer-list*)
      (unless (eq b buffer)
        (let ((bpn (buffer-pathname b)))
          (when (equal bpn buffer-pn)
            (loud-message "Buffer ~A also contains ~A."
                          (buffer-name b) (namestring buffer-pn))
            (return)))))))




(defhcommand revert-file-command (window p)
  (:pretty-name "Revert File")
  (:documentation
   "Unless in Save Mode, reads in the last saved version of the file in
   the current buffer. When in Save Mode, reads in the last checkpoint or
   the last saved version, whichever is more recent. An argument will always
   force Revert File to use the last saved version. In either case, if the
   buffer has been modified and \"Revert File Confirm\" is true, then Revert
   File will ask for confirmation beforehand. An attempt is made to maintain
   the point's relative position.")
  (:help-string
   "With an argument reverts to the last saved version of the file in the
   current buffer. Without, reverts to the last checkpoint or last saved
   version, whichever is more recent."))

(defmethod revert-file-command (window p)
  (let* ((buffer (window-buffer window))
         (buffer-pn (buffer-pathname buffer))
         (point (buffer-point buffer))
         (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (multiple-value-bind (revert-pn used-checkpoint)
        (if p buffer-pn (revert-pathname buffer))
      (unless revert-pn
        (editor-error "No file associated with buffer to revert to!"))
      (when (or (not (value revert-file-confirm))
                (not (buffer-modified buffer))
                (prompt-for-y-or-n
                 window
                 :prompt
                 "Buffer contains changes, are you sure you want to revert? "
                 :help (list
                        "Reverting the file will undo any changes by reading in the last ~
 ~:[saved version~;checkpoint file~]." used-checkpoint)
                 :default t))
        (read-buffer-file revert-pn buffer)
        (when used-checkpoint
          (setf (buffer-modified buffer) t)
          (setf (buffer-pathname buffer) buffer-pn)
          (message "Reverted to checkpoint file ~A." (namestring revert-pn)))
        (unless (line-offset buffer point lines)
          (buffer-end point))))))

;;; REVERT-PATHNAME -- Internal
;;;
;;; If in Save Mode, return either the checkpoint pathname or the buffer
;;; pathname whichever is more recent. Otherwise return the buffer-pathname
;;; if it exists. If neither file exists, return NIL.
;;;
(defun revert-pathname (buffer)
  (let* ((buffer-pn (buffer-pathname buffer))
         (buffer-pn-date (file-write-date buffer-pn))
         (checkpoint-pn (get-checkpoint-pathname buffer))
         (checkpoint-pn-date (and checkpoint-pn
                                  (file-write-date checkpoint-pn))))
    (cond (checkpoint-pn-date
           (if (> checkpoint-pn-date (or buffer-pn-date 0))
               (values checkpoint-pn t)
               (values buffer-pn nil)))
          (buffer-pn-date (values buffer-pn nil))
          (t (values nil nil)))))



;;;; Find file.



(defun find-file-buffer (current-window pathname)
  "Return a buffer assoicated with the file Pathname, reading the file into a
   new buffer if necessary.  The second value is T if we created a buffer, NIL
   otherwise.  If the file has already been read, we check to see if the file
   has been modified on disk since it was read, giving the user various
   recovery options."
  (let* ((pathname (pathname pathname))
         (trial-pathname (or (probe-file pathname)
                             (merge-pathnames pathname (hemlock-ext:default-directory))))
         (found (find trial-pathname (the list *buffer-list*)
                      :key #'buffer-pathname :test #'equal)))
    (cond ((not found)
           (let ((kind (osicat::file-kind trial-pathname :follow-symlinks t)))
             (case kind
               (:directory
                (dired-guts current-window nil nil trial-pathname))
               (:regular-file
                (let* ((name (pathname-to-buffer-name trial-pathname))
                       (found (getstring name *buffer-names*))
                       (use (if found
                                (prompt-for-buffer
                                 current-window
                                 :prompt "Buffer to use: "
                                 :help
                                 "Buffer name in use; give another buffer name, or confirm to reuse."
                                 :default found
                                 :must-exist nil)
                                (make-instance 'buffer :name name)))
                       (buffer (if (stringp use) (make-instance 'buffer :name use) use)))
                  (when (and (buffer-modified buffer)
                             (prompt-for-y-or-n current-window :prompt
                                                "Buffer is modified, save it? "))
                    (save-file-command () buffer))
                  (read-buffer-file pathname buffer)
                  (values buffer (stringp use))))
               (t (if (probe-file pathname)
                      (editor-error "cannot open a file of type ~A" kind)
                      (editor-error "~A does not exist." pathname))))))
          ((check-disk-version-consistent current-window pathname found)
           (when (buffer-windows found)
             (krma::focus-window (first (buffer-windows found))))
           (values found nil))
          (t
           (read-buffer-file pathname found)
           (when (buffer-windows found)
             (krma::focus-window (first (buffer-windows found))))
           (values found nil)))))


;;; Check-Disk-Version-Consistent  --  Internal
;;;
;;;    Check that Buffer contains a valid version of the file Pathname,
;;; harrassing the user if not.  We return true if the buffer is O.K., and
;;; false if the file should be read.
;;;
(defun check-disk-version-consistent (current-window pathname buffer)
  (let ((ndate (file-write-date pathname))
        (odate (buffer-write-date buffer)))
    (cond ((not (and ndate odate (/= ndate odate)))
           t)
          ((buffer-modified buffer)
           (beep)
           (clear-input)
           (command-case (:prompt (list
 "File has been changed on disk since it was read and you have made changes too!~
 ~%Read in the disk version of ~A? [Y] " (namestring pathname))
                          :help
 "The file in disk has been changed since Hemlock last saved it, meaning that
 someone else has probably overwritten it.  Since the version read into Hemlock
 has been changed as well, the two versions may have inconsistent changes.  If
 this is the case, it would be a good idea to save your changes in another file
 and compare the two versions.

 Type one of the following commands:")
             ((:confirm :yes)
 "Prompt for a file to write the buffer out to, then read in the disk version."
              (write-buffer-file
               current-window
               buffer
               (prompt-for-file
                current-window
                :prompt "File to save changes in: "
                :help (list "Save buffer ~S to this file before reading ~A."
                            (buffer-name buffer) (namestring pathname))
                :must-exist nil
                :default (buffer-default-pathname buffer)))
              nil)
             (:no
              "Change to the buffer without reading the new version."
              t)
             (#\r
              "Read in the new version, clobbering the changes in the buffer."
              nil)))
           (t
            (not (prompt-for-yes-or-no current-window :prompt
                                       (list
 "File has been changed on disk since it was read.~
 ~%Read in the disk version of ~A? "
                                        (namestring pathname))
                                       :help
 "Type Y to read in the new version or N to just switch to the buffer."
                                       :default t))))))




(defun read-buffer-file (pathname buffer)
  "Delete the buffer's region, and uses READ-FILE to read pathname into it.
   If the file exists, set the buffer's write date to the file's; otherwise,
   MESSAGE that this is a new file and set the buffer's write date to nil.
   Move buffer's point to the beginning, set the buffer unmodified.  If the
   file exists, set the buffer's pathname to the probed pathname; else, set it
   to pathname merged with DEFAULT-DIRECTORY.  Set \"Pathname Defaults\" to the
   same thing.  Process the file options, and then invoke \"Read File Hook\"."
  (setf (buffer-writable buffer) t)
  (delete-region buffer (buffer-region buffer))
  (let* ((point (buffer-point buffer))
         (pathname (pathname pathname))
         (probed-pathname (probe-file pathname)))
    (cond (probed-pathname
           (read-file buffer probed-pathname point)
           (setf (buffer-write-date buffer) (file-write-date probed-pathname)))
          (t
           (message "(New File)")
           (setf (buffer-write-date buffer) nil)))
    (buffer-start point buffer)
    (setf (buffer-modified buffer) nil)
    (let ((stored-pathname (or probed-pathname
                               (merge-pathnames pathname (hemlock-ext:default-directory)))))
      (setf (buffer-pathname buffer) stored-pathname)
      (setf (value pathname-defaults) stored-pathname)
      (process-file-options buffer stored-pathname)
      (invoke-hook read-file-hook buffer probed-pathname))))



;;;; File writing.



(defun write-buffer-file (current-window buffer pathname)
  "Write's buffer to pathname.  This assumes pathname is somehow related to
   the buffer's pathname, and if the buffer's write date is not the same as
   pathname's, then this prompts the user for confirmation before overwriting
   the file.  This consults \"Add Newline at EOF on Writing File\" and
   interacts with the user if necessary.  This sets \"Pathname Defaults\", and
   the buffer is marked unmodified.  The buffer's pathname and write date are
   updated, and the buffer is renamed according to the new pathname if possible.
   This invokes \"Write File Hook\"."
  (let ((buffer-pn (buffer-pathname buffer)))
    (let ((date (buffer-write-date buffer))
          (file-date (when (probe-file pathname) (file-write-date pathname))))
      (when (and buffer-pn date file-date
                 (equal (make-pathname :version nil :defaults buffer-pn)
                        (make-pathname :version nil :defaults pathname))
                 (/= date file-date))
        (unless (prompt-for-yes-or-no current-window :prompt (list
 "File has been changed on disk since it was read.~%Overwrite ~A anyway? "
 (namestring buffer-pn))
                                      :help
                                      "Type No to abort writing the file or Yes to overwrite the disk version."
                                      :default nil)
          (editor-error "Write aborted."))))
    (let ((val (value add-newline-at-eof-on-writing-file)))
      (when val
        (let ((end (buffer-end-mark buffer)))
          (unless (start-line-p end)
            (when (if (eq val :ask-user)
                      (prompt-for-y-or-n
                       current-window
                       :prompt
                       (list "~A~%File does not have a newline at EOF, add one? "
                             (buffer-name buffer))
                       :default t)
                      t)
              (insert-character buffer end #\newline)
              (message "Added newline at EOF."))))))
    (setv pathname-defaults pathname)
    (write-file buffer (buffer-region buffer) pathname)
    (let ((tn (truename pathname)))
      (message "~A written." (namestring tn))
      (setf (buffer-modified buffer) nil)
      (unless (equal tn buffer-pn)
        (setf (buffer-pathname buffer) tn))
      (setf (buffer-write-date buffer) (file-write-date tn))
      (let ((name (pathname-to-buffer-name tn)))
        (unless (getstring name *buffer-names*)
          (setf (buffer-name buffer) name)))))
  (invoke-hook write-file-hook buffer))

(defhcommand write-file-command (window p &optional pathname buffer)
  (:pretty-name "Write File")
  (:documentation
   "Writes the contents of Buffer, which defaults to the current buffer to
  the file named by Pathname.  The prefix argument is ignored.")
  (:help-string "Prompts for a file to write the contents of the current Buffer to.
  The prefix argument is ignored."))

(defmethod write-file-command (window p &optional pathname (buffer (window-buffer window)))
  (declare (ignore p))
  (write-buffer-file
   window
   buffer
   (or pathname
       (prompt-for-file window :prompt "Write File: "
                        :must-exist nil
                        :help "Name of file to write to"
                        :default (buffer-default-pathname buffer)))))

(defhcommand save-file-command (window p &optional buffer)
  (:pretty-name "Save File")
  (:documentation "Writes the contents of the current buffer to the associated file.  If there
  is no associated file, once is prompted for.")
  (:help-string "Writes the contents of the current buffer to the associated file."))

(defmethod save-file-command (window p &optional (buffer (window-buffer window)))
  (declare (ignore p))
  (when (or (buffer-modified buffer)
            (prompt-for-y-or-n
             window
             :prompt "Buffer is unmodified, write it anyway? "
             :default t))
    (write-buffer-file
     window
     buffer
     (or (buffer-pathname buffer)
         (prompt-for-file
          window :prompt "Save File: "
                          :help "Name of file to write to"
                          :default (buffer-default-pathname buffer)
                          :must-exist nil)))))


(defhcommand save-all-files-command (window p)
  (:pretty-name "Save All Files")
  (:documentation
   "Saves all modified buffers in their associated files.
  If a buffer has no associated file it is ignored even if it is modified..")
  (:help-string "Saves each modified buffer that has a file."))

(defmethod save-all-files-command (window p)
  (declare (ignore p))
  (let ((saved-count 0))
    (dolist (b *buffer-list*)
      (let ((pn (buffer-pathname b))
            (name (buffer-name b)))
        (when
            (and (buffer-modified b)
                 pn
                 (or (not (value save-all-files-confirm))
                     (prompt-for-y-or-n
                      window
                      :prompt (list
                               "Write ~:[buffer ~A as file ~S~;file ~*~S~], ~
                               Y or N: "
                               (string= (pathname-to-buffer-name pn) name)
                               name (namestring pn))
                      :default t)))
          (write-buffer-file window b pn)
          (incf saved-count))))
    (if (zerop saved-count)
        (message "No files were saved.")
        (message "Saved ~S file~:P." saved-count))))

(defhcommand save-all-files-and-exit-command (window p)
  (:pretty-name "Save All Files and Exit")
  (:documentation
   "Save all modified buffers in their associated files and exit;
  a combination of \"Save All Files\" and \"Exit Hemlock\".")
  (:help-string
   "Do a save-all-files-command and then an exit-hemlock."))

(defmethod save-all-files-and-exit-command (window p)
  (declare (ignore p))
  (save-all-files-command window ())
  (exit-hemlock))

(defhcommand backup-file-command (window p)
  (:pretty-name "Backup File")
  (:documentation "Write the buffer to a file without changing the associated name.")
  (:help-string "Write the buffer to a file without changing the associated name."))

(defmethod backup-file-command (window p)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (let ((file (prompt-for-file window :prompt "Backup to File: "
                                 :help
                                 "Name of a file to backup the current buffer in."
                                 :default (buffer-default-pathname buffer)
                                 :must-exist nil)))
      (write-file buffer (buffer-region buffer) file)
      (message "~A written." (namestring (truename file))))))



;;;; Buffer hacking commands:



(defun previous-buffer (current-window)
  "Returns some previously selected buffer that is not the current buffer.
   Returns nil if no such buffer exists."
  (let ((b (car *buffer-history*)))
    (let ((cb (window-buffer current-window)))
      (or (if (eq b cb) (cadr *buffer-history*) b)
          (find-if-not #'(lambda (x)
                           (or (eq x cb)
                               (eq x *echo-area-buffer*)))
                       (the list *buffer-list*))))))

;;; ADD-BUFFER-HISTORY-HOOK makes sure every buffer will be visited by
;;; "Circulate Buffers" even if it has never been before.
;;;
(defun add-buffer-history-hook (buffer)
  (let ((ele (last *buffer-history*))
        (new-stuff (list buffer)))
    (if ele
        (setf (cdr ele) new-stuff)
        (setf *buffer-history* new-stuff))))
;;;


;;; DELETE-BUFFER-HISTORY-HOOK makes sure we never end up in a dead buffer.
;;;
(defun delete-buffer-history-hook (buffer)
  (setq *buffer-history* (delq buffer *buffer-history*)))
;;;




(defun delete-buffer-if-possible (buffer current-buffer)
  "Deletes a buffer if at all possible.  If buffer is the only buffer, other
   than the echo area, signals an error.  Otherwise, find some recently current
   buffer, and make all of buffer's windows display this recent buffer.  If
   buffer is current, set the current buffer to be this recently current
   buffer."
  (let ((new-buf (flet ((frob (b)
                          (or (eq b buffer) (eq b *echo-area-buffer*))))
                   (or (find-if-not #'frob (the list *buffer-history*))
                       (find-if-not #'frob (the list *buffer-list*))))))
    (unless new-buf
      (error "Cannot delete only buffer ~S." buffer))
    (dolist (w (buffer-windows buffer))
      (setf (window-buffer w) new-buf))
    (when (eq buffer current-buffer)
      (setf (current-buffer (window-buffer (climish::focused-window (krma::main-window clim:*default-frame-manager*)))) new-buf)))
  (delete-buffer buffer current-buffer))


(defvar *create-buffer-count* 0)

(defhcommand create-buffer-command (window p &optional buffer-name)
  (:pretty-name "Create Buffer")
  (:documentation "Create a new buffer.  If a buffer with the specified name already exists,
   then go to it.")
  (:help-string "Create or go to the buffer with the specifed name."))

(defmethod create-buffer-command (window p &optional buffer-name)
  (declare (ignore p))
  (let ((name (or buffer-name
                  (prompt-for-buffer window :prompt "Create Buffer: "
                                     :default-string
                                     (format nil "Buffer ~D"
                                             (incf *create-buffer-count*))
                                     :must-exist nil))))
    (if (bufferp name)
        (change-to-buffer name window)
        (change-to-buffer (or (getstring name *buffer-names*)
                              (make-instance 'buffer :name name))
                          window))))

(defhcommand select-buffer-command (window p)
  (:pretty-name "Select Buffer")
  (:documentation
   "Select a different buffer.
   The buffer to go to is prompted for.")
  (:help-string "Select a different buffer.
   The buffer to go to is prompted for."))

(defmethod select-buffer-command (window p)
  (declare (ignore p))
  ;; what to do about (previous-buffer) ?
  ;; (previous-window-buffer (current-window)) ?
  ;; or some global previous buffer?
  (let ((buf (prompt-for-buffer window :prompt "Select Buffer: "
                                :default (previous-buffer window))))
    (when (eq buf *echo-area-buffer*)
      (editor-error "Cannot select Echo Area buffer."))
    (change-to-buffer buf window)
    (climish::rm-redraw-node-recursively window)))


(defvar *buffer-history-ptr* ()
  "The successively previous buffer to the current buffer.")

(defhcommand select-previous-buffer-command (window p)
  (:pretty-name "Select Previous Buffer")
  (:documentation "Select the buffer selected before this one.  If called repeatedly
   with an argument, select the successively previous buffer to the
   current one leaving the buffer history as it is.")
  (:help-string
   "Select the buffer selected before this one."))

(defmethod select-previous-buffer-command (window p)
  (if p
      (circulate-buffers-command window nil)
      (let ((b (previous-buffer window)))
        (unless b (editor-error "No previous buffer."))
        (change-to-buffer b window)
        ;;
        ;; If the pointer goes to nil, then "Circulate Buffers" will keep doing
        ;; "Select Previous Buffer".
        (setf *buffer-history-ptr* (cddr *buffer-history*))
        (setf (last-command-type) :previous-buffer)))
  (climish::redraw-buffer window))

(defhcommand circulate-buffers-command (window p)
  (:pretty-name "Circulate Buffers")
  (:documenation "Advance through buffer history, selecting successively previous buffer.")
  (:help-string "Advance through buffer history, selecting successively previous buffer."))

(defmethod circulate-buffers-command (window p)
  (declare (ignore p))
  (let* ((cb (window-buffer window)))
    (if (and (eq (last-command-type) :previous-buffer)
             *buffer-history-ptr*)      ;Possibly nil if never CHANGE-TO-BUFFER.
        (let ((b (pop *buffer-history-ptr*)))
          (when (eq b cb)
            (setf b (pop *buffer-history-ptr*)))
          (unless b
            (setf *buffer-history-ptr*
                  (or (cdr *buffer-history*) *buffer-history*))
            (setf b (car *buffer-history*)))
          (setf (current-buffer (window-buffer (climish::focused-window (krma::main-window clim:*default-frame-manager*)))) b)
          (setf (window-buffer window) b) ;; setf window buffer happens in render thread
          (setf (last-command-type) :previous-buffer))
        (select-previous-buffer-command window nil))
  (climish::redraw-buffer window)))


(defhcommand buffer-not-modified-command (window p)
  (:pretty-name "Buffer Not Modified")
  (:documentation "Make the current buffer not modified.")
  (:help-string "Make the current buffer not modified."))

(defmethod buffer-not-modified-command (window p)
  (declare (ignore p))
  (setf (buffer-modified (window-buffer window)) nil)
  (message "Buffer marked as unmodified."))

(defhcommand check-buffer-modified-command (window p)
  (:pretty-name "Check Buffer Modified")
  (:documentation "Say whether the buffer is modified or not.")
  (:help-string "Say whether the current buffer is modified or not."))

(defmethod check-buffer-modified-command (window p)
  (declare (ignore p))
  (clear-echo-area)
  (let ((cb (window-buffer window)))
    (message "Buffer ~S ~:[is not~;is~] modified."
             (buffer-name cb) (buffer-modified cb))))

(defhcommand set-buffer-read-only-command (window p)
  (:pretty-name "Set Buffer Read-Only")
  (:documentation "Toggles the read-only flag for the current buffer.")
  (:help-string "Toggles the read-only flag for the current buffer."))

(defmethod set-buffer-read-only-command (window p)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (message "Buffer ~S is now ~:[read-only~;writable~]."
             (buffer-name buffer)
             (setf (buffer-writable buffer) (not (buffer-writable buffer))))))

(defhcommand set-buffer-writable-command (window p)
  (:pretty-name "Set Buffer Writable")
  (:documentation "Make the current buffer modifiable.")
  (:help-string "Make the current buffer modifiable."))

(defmethod set-buffer-writable-command (window p)
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (setf (buffer-writable buffer) t)
    (message "Buffer ~S is now writable." (buffer-name buffer))))


;;; TODO: If this is true, it is possible to make Hemlock unusable by
;;; killing last buffer and selecting Echo Area as the new buffer.



(defhcommand kill-buffer-command (window p &optional buffer-name)
  (:pretty-name "Kill Buffer")
  (:documentation
   "Prompts for a buffer to delete.
  If the buffer is modified, then let the user save the file before doing so.
  When deleting the current buffer, prompts for a new buffer to select.  If
  a buffer other than the current one is deleted then any windows into it
  are deleted.")
  (:help-string
   "Delete buffer Buffer-Name, doing sensible things if the buffer is displayed
  or current."))

(defmethod kill-buffer-command (window p &optional buffer-name)
  (declare (ignore p))
  (let ((wb (window-buffer window)))
    
    (let ((buffer (if buffer-name
                      (getstring buffer-name *buffer-names*)
                      (prompt-for-buffer window :prompt "Kill Buffer: "
                                                :default wb))))
      (unless buffer
        (editor-error "No buffer named ~S" buffer-name))



      (when (and (buffer-modified buffer)
                 (buffer-pathname buffer)
                 (prompt-for-y-or-n window :prompt "Save it first? "))
        (save-file-command window nil buffer))

      (if (eq buffer wb)
          (progn 
          (let* ((previous (or (previous-buffer window)
                               (editor-error "Cannot kill last buffer.")))
                 (new (if (value ask-for-new-buffer)
                          (prompt-for-buffer
                           window
                           :prompt "New Buffer: "
                           :default previous
                           :help "Buffer to change to after the current one is killed.")
                          previous)))
            (when (eq new buffer)
              (editor-error "You must select a different buffer."))
            (dolist (w (buffer-windows buffer))
              (setf (window-buffer w) new))
            (setf (current-buffer (window-buffer (climish::focused-window (clui::window-parent *echo-area-window*)))) new)))
          (dolist (w (buffer-windows buffer))
            (climish::destroy-window w)))
      (delete-buffer buffer *echo-area-window*)
      (climish::rm-redraw-node-recursively window))))


(defhcommand rename-buffer-command (window p)
  (:pretty-name "Rename Buffer")
  (:documentation "Change the current buffer's name.
  The name, which is prompted for, defaults to the name of the associated
  file.")
  (:help-string "Change the name of the current buffer."))

(defmethod rename-buffer-command (window p)
  (declare (ignore p))
  (let* ((buf (window-buffer window))
         (pn (buffer-pathname buf))
         (name (if pn (pathname-to-buffer-name pn) (buffer-name buf)))
         (new (prompt-for-string window :prompt "New Name: "
                                 :help "Give a new name for the current buffer"
                                 :default name)))
    (multiple-value-bind (entry foundp) (getstring new *buffer-names*)
      (cond ((or (not foundp) (eq entry buf))
             (setf (buffer-name buf) new))
            (t (editor-error "Name ~S already in use." new))))
    (climish::rm-redraw-node-recursively window)))


(defhcommand insert-buffer-command (window p)
  (:pretty-name "Insert Buffer")
  (:documentation
   "Insert the contents of a buffer.
  The name of the buffer to insert is prompted for.")
  (:help-string "Prompt for a buffer to insert at the point."))

(defmethod insert-buffer-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (region (buffer-region (prompt-for-buffer
                                window
                                ;; previous buffer needs to kinda be some global thing
                                :default (previous-buffer window)
                                :help
                                "Type the name of a buffer to insert."))))

      ;;
      ;; start and end will be deleted by undo stuff
      (let ((save (region (copy-mark point :right-inserting)
                          (copy-mark point :left-inserting))))

        (push-buffer-mark buffer (copy-mark point))
        (insert-region buffer point region)
        (make-region-undo buffer :delete "Insert Buffer" save))))



;;;; Printing directories.

;;; PRINT-DIRECTORY is exported from the EXTENSIONS package.
;;;
#-(or cmu scl)
(defmacro out-synonym-of (stream &optional check-type)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-output*)
             ((eq ,svar t) *terminal-io*)
             (t ,@(if check-type `((check-type ,svar ,check-type)))
                ,svar)))))

#-(or cmu scl)
(defun print-directory (pathname stream &key all verbose return-list)
  "Like Directory, but prints a terse, multi-column directory listing
   instead of returning a list of pathnames.  When :all is supplied and
   non-nil, then Unix dot files are included too (as ls -a).  When :verbose
   is supplied and non-nil, then a long listing of miscellaneous
   information is output one file per line."
  (let ((*standard-output* (out-synonym-of stream))
        (pathname pathname))
    (if verbose
        (print-directory-verbose pathname all return-list)
        (print-directory-formatted pathname all return-list))))

(defun pathname-as-directory (pathname)
  (let* ((namestring (namestring pathname)))
    (pathname (concatenate 'string namestring "/"))))


#-(or cmu scl)
(defun %directory (directory &optional all)
  (declare (ignorable all))
  (setf directory (namestring (pathname-as-directory directory)))
  ;; TODO: handle patterns in the file, type, and version.
  (let ((list ()))
    (loop for file in (directory (concatenate 'string directory "*.*"))
          do (push (namestring (osicat::pathname-as-file file)) list))
    (sort list #'string<)))

#-(or cmu scl)
(defun write-file-mode (mode)
  (macrolet ((frob (bit name &optional sbit sname negate)
               `(if ,(if negate
                         `(not (logbitp ,bit mode))
                         `(logbitp ,bit mode))
                    ,(if sbit
                         `(if (logbitp ,sbit mode)
                              (write-char ,sname)
                              (write-char ,name))
                         `(write-char ,name))
                    (write-char #\-))))
    (frob 15 #\d nil nil t)
    (frob 8 #\r)
    (frob 7 #\w)
    (frob 6 #\x 11 #\s)
    (frob 5 #\r)
    (frob 4 #\w)
    (frob 3 #\x 10 #\s)
    (frob 2 #\r)
    (frob 1 #\w)
    (frob 0 #\x)))

(cffi:defcenum _SE_OBJECT_TYPE
  (SE_UNKNOWN_OBJECT_TYPE)
  (SE_FILE_OBJECT)
  (SE_SERVICE)
  (SE_PRINTER)
  (SE_REGISTRY_KEY)
  (SE_LMSHARE)
  (SE_KERNEL_OBJECT)
  (SE_WINDOW_OBJECT)
  (SE_DS_OBJECT)
  (SE_DS_OBJECT_ALL)
  (SE_PROVIDER_DEFINED_OBJECT)
  (SE_WMIGUID_OBJECT)
  (SE_REGISTRY_WOW64_32KEY)
  (SE_REGISTRY_WOW64_64KEY))

(cffi:defcenum _SID_NAME_USE
  (SidTypeUser 1)
  (SidTypeGroup)
  (SidTypeDomain)
  (SidTypeAlias)
  (SidTypeWellKnownGroup)
  (SidTypeDeletedAccount)
  (SidTypeInvalid)
  (SidTypeUnknown)
  (SidTypeComputer)
  (SidTypeLabel)
  (SidTypeLogonSession))

(defconstant OWNER_SECURITY_INFORMATION 1)

(cffi:defcfun (GetSecurityInfo "GetSecurityInfo") :int
  (handle :int)
  (ObjectType :int)
  (SecurityInfo :int)
  (ppsidOwner :pointer)
  (ppsidGroup :pointer)
  (ppDacl :pointer)
  (ppSacl :pointer)
  (ppSecurityDescriptor :pointer))

(cffi:defcfun (CreateFileW "CreateFileW") :int
  (lpFileName :string)
  (dwDesiredAccess :int64)
  (dwShareMode :int64)
  (lpSecurityAttributes :pointer)
  (dwCreationDisposition :int)
  (dwFlagsAndAttributes :int)
  (hTemplateFile :int))

(cffi:defcfun (LookupAccountSid "LookupAccountSidA") :bool
  (lpSystemName :string)
  (pSid :pointer)
  (Name :string)
  (cchName :pointer)
  (ReferencedDomainName :string)
  (cchReferencedDomainName :pointer)
  (peUse :pointer))

(defconstant GENERIC_READ    #b10000000000000000000000000000000)
(defconstant GENERIC_WRITE   #b01000000000000000000000000000000)
(defconstant GENERIC_EXECUTE #b00100000000000000000000000000000)
(defconstant GENERIC_ALL     #b00010000000000000000000000000000)

(defconstant FILE_SHARE_DELETE #x00000004)
(defconstant FILE_SHARE_READ   #x00000001)
(defconstant FILE_SHARE_WRITE  #x00000002)

(defconstant OPEN_EXISTING 3)
(defconstant FILE_ATTRIBUTE_NORMAL #x80)

(defconstant INVALID_HANDLE_VALUE #xffffffff)

(defconstant ERROR_SUCCESS 0)
(defconstant ERROR_NONE_MAPPED #x534)

(cffi:defcfun (GetLastError "GetLastError") :int)

(cffi:defcfun (CloseHandle "CloseHandle") :bool
  (hObject :int))

(defun get-file-owner-1 (pathname)
  (multiple-value-bind (user domain) (get-file-owner-2 pathname)
    (concatenate 'string user "\\" domain)))

(defun get-file-owner-2 (pathname)
  (setq pathname (concatenate 'string "\\\\?\\" (namestring pathname)))
  (let ((return-code 0)
        (pSidOwner vk:+nullptr+)
        (return-bool t)
        (eUse SidTypeUnknown)
        (handle)
        (error-code))

    (setq handle
          (CreateFileW pathname
                       GENERIC_READ
                       (logior FILE_SHARE_DELETE FILE_SHARE_READ FILE_SHARE_WRITE)
                       vk:+nullptr+
                       OPEN_EXISTING
                       FILE_ATTRIBUTE_NORMAL
                       0))

    #+NIL(when (eq handle INVALID_HANDLE_VALUE)
      (error "CreateFile Error = ~S" (GetLastError)))

    (unwind-protect
         (progn

           (cffi:with-foreign-objects ((ppSidOwner :pointer))

             (setf (cffi:mem-aref ppSidOwner :pointer) pSidOwner)

             (setq return-code (GetSecurityInfo handle
                                                SE_FILE_OBJECT
                                                OWNER_SECURITY_INFORMATION
                                                ppSidOwner
                                                vk:+nullptr+
                                                vk:+nullptr+
                                                vk:+nullptr+
                                                vk:+nullptr+))

             (unless (eq return-code ERROR_SUCCESS)
               (error "GetSecurityInfo Error = ~S" (GetLastError)))

             (cffi:with-foreign-objects ((pAccountNameSize :int)
                                         (pDomainNameSize :int)
                                         (peUse :int))

               (setf (cffi:mem-aref pAccountNameSize :int) 1
                     (cffi:mem-aref pDomainNameSize :int) 1
                     (cffi:mem-aref peUse :int) eUse)

               (setq return-bool (LookupAccountSid vk:+nullptr+
                                                   (cffi:mem-aref ppSidOwner :pointer)
                                                   vk:+nullptr+
                                                   pAccountNameSize
                                                   vk:+nullptr+
                                                   pDomainNameSize
                                                   peUse))
               ;;(break "~S ~S" (cffi:mem-aref pAccountNameSize :int) (cffi:mem-aref pDomainNameSize :int))

               (cffi:with-foreign-objects ((p-account-name :char (cffi:mem-aref pAccountNameSize :int))
                                           (p-domain-name  :char (cffi:mem-aref pDomainNameSize :int)))

                 (setq return-bool (LookupAccountSid vk:+nullptr+
                                                     (cffi:mem-aref ppSidOwner :pointer)
                                                     p-account-name
                                                     pAccountNameSize
                                                     p-domain-name
                                                     pDomainNameSize
                                                     peUse))

                 (unless return-bool
                   (setq error-code (GetLastError))
                   (cond ((eq error-code ERROR_NONE_MAPPED)
                          (error "Account owner not found for specified SID."))
                         (t (error "Error in LookupAccountSID: ~S" error-code))))

                 (values (cffi:foreign-string-to-lisp p-account-name :count (cffi:mem-aref pAccountNameSize :int))
                         (cffi:foreign-string-to-lisp p-domain-name  :count (cffi:mem-aref pDomainNameSize :int)))))))

      (CloseHandle handle))))

(cffi:defcfun (GetFileAttributes "GetFileAttributesA") :int
  (lpFileName :string))

(defconstant FILE_ATTRIBUTE_DIRECTORY #x10)

(defun file-directory-p (pathname)
  (logtest (GetFileAttributes (namestring pathname)) FILE_ATTRIBUTE_DIRECTORY))




#-(or cmu scl ccl)
(defun print-directory-verbose (pathname all return-list)
  (let* ((contents (%directory pathname all))
         (result nil)
         (n (length contents)))
    (format t "Directory of ~A:~%" (namestring pathname))
    (iter:iter (iter:for file in contents)
               (iter:for i from 0)
               (when (zerop (mod i 100))
                 (message "Dired: reading files (~D/~D)" i n))
      (let* ((namestring (namestring file))
             (tail (subseq namestring
                           (1+ (or (position #\/ namestring
                                             :from-end t
                                             :test #'char=)
                                   -1)))))
        (handler-case
            (sb-posix::lstat (if (file-directory-p namestring)
                                (pathname-as-directory namestring)
                                namestring))
          (sb-posix::syscall-error (c)
            (format t "Couldn't stat ~A -- ~A.~%" tail c))
          (:no-error (stat)
            (let ((mode (sb-posix::stat-mode stat))
                  (uid (sb-posix::stat-uid stat))
                  (nlink (sb-posix::stat-nlink stat))
                  (size (sb-posix::stat-size stat))
                  (mtime (sb-posix::stat-mtime stat)))
              ;;
              ;; Print characters for file modes.
              (write-file-mode mode)

              ;;
              ;; Print the rest.
              (multiple-value-bind (sec min hour date month year)
                  (get-decoded-time)
                (declare (ignore sec min hour date month))
                (let ((name (get-file-owner-1 namestring)))
                  (format t "~2D ~8A ~8D ~12A ~A~@[/~]~%"
                          nlink
                          (or name uid)
                          size
                          (decode-universal-time-for-files mtime year)
                          tail
                          (= (logand mode sb-posix::s-ifmt)
                             sb-posix::s-ifdir))))
              ;;
              ;; return
              (when return-list
                (push (if (= (logand mode sb-posix::s-ifmt)
                             sb-posix::s-ifdir)
                          (pathname (concatenate 'string namestring "/"))
                          ;; let's return a namestring rather than an
                          ;; IOLIB pathname for now, to insulate the rest
                          ;; of the code base (which still expects standard
                          ;; Common Lisp pathnames) from the new iolib world.
                          namestring)
                      result)))))))
    (clear-echo-area)
    (message "Dired: ~D files read" n)
    (nreverse result)))

#-(or cmu scl)
(defconstant unix-to-universal-time 2208988800)

#-(or cmu scl)
(defun decode-universal-time-for-files (time current-year)
  (multiple-value-bind (sec min hour day month year)
                       (decode-universal-time (+ time unix-to-universal-time))
    (declare (ignore sec))
    (format nil "~A ~2,' D ~:[ ~D~;~*~2,'0D:~2,'0D~]"
            (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                      "Sep" "Oct" "Nov" "Dec")
                   (1- month))
            day (= current-year year) year hour min)))

#-(or cmu scl)
(defun print-directory-formatted (pathname all return-list)
  (let ((width (or #+NIL(hi::stream-line-length *standard-output*) 80))
        (names ())
        (cnt 0)
        (max-len 0)
        (result (%directory pathname all)))
    (declare (list names) (fixnum max-len cnt))
    ;;
    ;; Get the data.
    (dolist (file result)
      (let* ((name (namestring (osicat::pathname-as-file file)))
             (length (length name))
             (end (if (and (plusp length)
                           (char= (schar name (1- length)) #\/))
                      (1- length)
                      length))
             (slash-name (subseq name
                                 (1+ (or (position #\/ name
                                                   :from-end t
                                                   :end end
                                                   :test #'char=)
                                         -1))))
             (len (length slash-name)))
        (declare (simple-string slash-name)
                 (fixnum len))
        (if (> len max-len) (setq max-len len))
        (incf cnt)
        (push slash-name names)))
    (setq names (nreverse names))
    ;;
    ;; Do the output.
    (let* ((col-width (1+ max-len))
           (cols (max (truncate width col-width) 1))
           (lines (ceiling cnt cols)))
      (declare (fixnum cols lines))
      (format t "Directory of ~A:~%" (namestring pathname))
      (dotimes (i lines)
        (declare (fixnum i))
        (dotimes (j cols)
          (declare (fixnum j))
          (let ((name (nth (+ i (the fixnum (* j lines))) names)))
            (when name
              (write-string name)
              (unless (eql j (1- cols))
                (dotimes (i (- col-width (length (the simple-string name))))
                  (write-char #\space))))))
        (terpri)))
    (when return-list
      result)))


;;;; File utility commands:

(defhcommand directory-command (window p)
  (:pretty-name "Directory")
  (:documentation
   "Do a directory into a pop-up window.  If an argument is supplied, then
   dot files are listed too (as with ls -a).  Prompts for a pathname which
   may contain wildcards in the name and type.")
  (:help-string "Do a directory into a pop-up window."))

(defmethod directory-command (window p)
  (let* ((dpn (value pathname-defaults))
         (pn (prompt-for-file
              window
              :prompt "Directory: "
              :help "Pathname to do directory on."
              :default (make-pathname :device (pathname-device dpn)
                                      :directory (pathname-directory dpn))
              :must-exist nil)))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (with-pop-up-display (s :current-window window)
      (print-directory pn s :all p))))

(defhcommand verbose-directory-command (window p)
  (:pretty-name "Verbose Directory")
  (:documentation 
   "Do a directory into a pop-up window.  If an argument is supplied, then
   dot files are listed too (as with ls -a).  Prompts for a pathname which
   may contain wildcards in the name and type.")
  (:help-string
   "Do a directory into a pop-up window."))

(defmethod verbose-directory-command (window p)
  (let* ((dpn (value pathname-defaults))
         (pn (prompt-for-file
              window
              :prompt "Verbose Directory: "
              :help "Pathname to do directory on."
              :default (make-pathname :device (pathname-device dpn)
                                      :directory (pathname-directory dpn))
              :must-exist nil)))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (with-pop-up-display (s :current-window window)
      (print-directory pn s :verbose t :all p))))



;;;; Change log stuff:



(defun universal-time-to-string (ut)
  (multiple-value-bind (sec min hour day month year)
                       (decode-universal-time ut)
    (format nil "~2,'0D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            day (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                          "Sep" "Oct" "Nov" "Dec")
                       (1- month))
            (rem year 100)
            hour min sec)))

(defvar *back-to-@-pattern* (new-search-pattern :character :backward #\@))
(defhcommand log-change-command (window p)
  (:pretty-name "Log Change")
  (:documenation
   "Make an entry in the change-log file for this buffer.
  Saves the file in the current buffer if it is modified, then finds the file
  specified in the \"Log\" file option, adds the template for a change-log
  entry at the beginning, then does a recursive edit, saving the log file on
  exit.")
  (:help-string
   "Find the change-log file as specified by \"Log File Name\" and edit it."))

(defmethod log-change-command (window p)
  (declare (ignore p))
  (unless (hemlock-bound-p 'log-file-name)
    (editor-error "No log file defined."))
  (let* ((buffer (window-buffer window))
         (pathname (buffer-pathname buffer)))
    (when (or (buffer-modified buffer) (null pathname))
      (save-file-command window ()))
    (unwind-protect
        (progn
          (find-file-command nil (merge-pathnames
                                  (value log-file-name)
                                  (buffer-default-pathname buffer)))
          (let ((point (buffer-point buffer)))
            (buffer-start point)
            (with-output-to-mark (s buffer point :full)
              (format s (value log-entry-template)
                      (namestring pathname)
                      (universal-time-to-string
                       (or (file-write-date pathname)
                           (get-universal-time)))
                      (file-author pathname)))
            (when (find-pattern buffer point *back-to-@-pattern*)
              (delete-characters buffer point 1)))
          (do-recursive-edit)
          (when (buffer-modified buffer) (save-file-command window ())))
      (if (member buffer *buffer-list* :test #'eq)
          (change-to-buffer buffer window)
          (editor-error "Old buffer has been deleted.")))))



;;;; Window hacking commands:

(defhcommand next-window-command (window p)
  (:pretty-name "Next Window")
  (:documentation
   "Change the current window to be the next window and the current buffer
  to be it's buffer.")
  (:help-string
   "Go to the next window.
  If the next window is the bottom window then wrap around to the top window."))

(defmethod next-window-command (window p)
  (declare (ignore p))
  (let* ((next (climish::next window)))
    (setf (current-buffer (window-buffer (climish::focused-window (krma::main-window clim:*default-frame-manager*)))) (window-buffer next))
    (climish::focus-window next)))

(defhcommand previous-window-command (window p)
  (:pretty-name "Previous Window")
  (:documentation
   "Change the current window to be the previous window and the current buffer
  to be it's buffer.")
  (:help-string
   "Go to the previous window.
  If the Previous window is the top window then wrap around to the bottom."))

(defmethod previous-window-command (window p)
  (declare (ignore p))
  (let* ((previous (climish::prev window)))
    (setf (current-buffer (window-buffer (climish::focused-window (krma::main-window clim:*default-frame-manager*)))) (window-buffer previous))
    (climish::focus-window previous)))

(defhcommand split-window-command (window p)
  (:pretty-name "Split Window")
  (:documentation
   "Make a new window by splitting the current window.
   The new window is made the current window and displays starting at
   the same place as the current window.")
  (:help-string
   "Create a new window which displays starting at the same place
   as the current window."))

(defmethod split-window-command (window p)
  (declare (ignore p))
  (let ((new (make-instance 'climish::text-editor-window :display-start (window-display-start window))))
    (unless new (editor-error "Could not make a new window."))
    (climish::focus-window new)))

(defun hi::enlarge-window (window offset)
  "offset in lines.  Can be negative."
  (declare (ignorable window offset))
  #+NIL
  (hi::device-enlarge-window (device-hunk-device (window-hunk window))
                             window
                             offset))

(defhcommand enlarge-window-command (window p)
  (:pretty-name "Enlarge Window")
  (:documentation "")
  (:help-string ""))

(defmethod enlarge-window-command (window p)
  (hi::enlarge-window window (or p 1)))

(defhcommand new-window-command (window p)
  (:pretty-name "New Window")
  (:documenation "Make a new window and go to it.
   The window will display the same buffer as the current one.")
  (:help-string
   "Create a new window which displays starting at the same place
   as the current window."))

(defmethod new-window-command (window p)
  (declare (ignore p))
  (let ((new (make-instance 'climish::text-editor-window :display-start (window-display-start window)
                          :ask-user t)))
    (unless new (editor-error "Could not make a new window."))
    (climish::focus-window new)))

(defhcommand delete-window-command (window p)
  (:pretty-name "Delete Window")
  (:documenation
   "Delete the current window, going to the previous window.")
  (:help-string "Delete the window we are in, going to the previous window."))

(defmethod delete-window-command (window p)
  (declare (ignore p))
  (when (= (length *window-list*) 2)
    (editor-error "Cannot delete only window."))
  (let ((window window))
    (previous-window-command window nil)
    (climish::destroy-window window)))

(defhcommand line-to-top-of-window-command (window p)
  (:pretty-name "Line to Top of Window")
  (:documentation "Move current line to top of window.")
  (:help-string "Move current line to top of window."))

(defmethod line-to-top-of-window-command (window p)
  (declare (ignore p))
  (let ()
    (with-mark ((mark (buffer-point (window-buffer window))))
      (move-mark (window-display-start window) (line-start mark)))))

(defhcommand delete-next-window-command (window p)
  (:pretty-name "Delete Next Window")
  (:documentation "Deletes the next window on display.")
  (:help-string "Deletes then next window on display."))

(defmethod delete-next-window-command (window p)
  (declare (ignore p))
  (if (<= (length *window-list*) 2)
      (editor-error "Cannot delete only window")
      (climish::destroy-window (climish::next window))))

(defhcommand go-to-one-window-command (window p)
  (:pretty-name "Go to One Window")
  (:documentation
   "Deletes all windows leaving one with the \"Default Initial Window X\",
   \"Default Initial Window Y\", \"Default Initial Window Width\", and
   \"Default Initial Window Height\".")
  (:help-string
   "Deletes all windows leaving one with the \"Default Initial Window X\",
   \"Default Initial Window Y\", \"Default Initial Window Width\", and
   \"Default Initial Window Height\"."))

(defmethod go-to-one-window-command (window p)
  (declare (ignore p))
  (let ((win (make-instance 'climish::text-editor-window
                            :display-start (window-display-start window)
                          :ask-user t
                          :x (value default-initial-window-x)
                          :y (value default-initial-window-y)
                          :width (value default-initial-window-width)
                          :height (value default-initial-window-height))))
    (climish::focus-window win)
    (dolist (w *window-list*)
      (unless (or (eq w win)
                  (eq w *echo-area-window*))
        (climish::destroy-window w)))))

(defhcommand line-to-center-of-window (window p)
  (:pretty-name "Line to Center of Window")
  (:documentation "Moves current line to the center of the window.")
  (:help-string "Moves current line to the center of the window."))

(defmethod line-to-center-of-window (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (hi::center-window-on-mark window point)))

#+NOTNOW
(defun quickselect-slave-repl (&optional *)
  (let ((info (variable-value 'current-eval-server :global)))
    (when info
      (change-to-buffer (server-info-slave-buffer info)))))

(defun quickselect-help (current-window &optional *)
  (with-pop-up-display (s :current-window current-window)
    (format s "Quickselect:~%")
    (terpri s)
    (format s "  h  this help~%")
    (terpri s)
    (format s "  S  List Slaves (also c for slime compatibility)~%")
    (format s "  r  Slave REPL~%")
    (terpri s)
    (format s "  b  Bufed~%")
    (format s "  C  Coned~%"))
  (message "Press q to dismiss this help."))

(defhcommand quickselect-command (window p)
  (:pretty-name "Quickselect")
  (:documentation "")
  (:help-string ""))

(defmethod quickselect-command (window p)
  p
  (funcall (let ((old-window window))
             (unwind-protect
                  (progn
                    (setf (climish::focused-window (clui::window-parent *echo-area-window*)) *echo-area-window*)
                    (hi::display-prompt-nicely "Quickselect? [?hrCcSb]")
                    (hi::key-event-case
                     ((#k"r") 'quickselect-slave-repl)
                     ((#k"?" #k"h") #'(lambda ()
                                        (quickselect-help *echo-area-window*)))
                     ((#k"C") 'coned-command)
                     ((#k"c" #k"S") 'list-slaves-command)
                     ((#k"b") 'bufed-command)
                     (t (lambda ()))))
               (climish::focus-window old-window)
               (setf (climish::focused-window (clui::window-parent *echo-area-window*)) old-window)))
           nil))
