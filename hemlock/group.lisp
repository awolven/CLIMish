;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; File group stuff for Hemlock.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
;;;    The "Compile Group" and "List Compile Group" commands in lispeval
;;;    also know about groups.
;;;
;;; This file provides Hemlock commands for manipulating groups of files
;;; that make up a larger system.  A file group is a set of files whose
;;; names are listed in some other file.  At any given time one group of
;;; files is the Active group.  The Select Group command makes a group the
;;; Active group, prompting for the name of a definition file if the group
;;; has not been selected before.  Once a group has been selected once, the
;;; name of the definition file associated with that group is retained.  If
;;; one wishes to change the name of the definition file after a group has
;;; been selected, one should call Select Group with a prefix argument.

(in-package :hemlock)

(defvar *file-groups* (make-string-table)
  "A string table of file groups.")

(defvar *active-file-group* ()
  "The list of files in the currently active group.")

(defvar *active-file-group-name* ()
  "The name of the currently active group.")



;;;; Selecting the active group.

(defcommand "Select Group" (window p)
  "Makes a group the active group.  With a prefix argument, changes the
  definition file associated with the group."
  "Makes a group the active group."
  (let* ((group-name
          (prompt-for-keyword
           window
           (list *file-groups*)
           :must-exist nil
           :prompt "Select Group: "
           :help
           "Type the name of the file group you wish to become the active group."))
         (old (getstring group-name *file-groups*))
         (pathname
          (if (and old (not p))
              old
              (prompt-for-file
               window :must-exist t
                               :prompt "From File: "
                               :default (merge-pathnames
                                         (make-pathname
                                          :name group-name
                                          :type "upd")
                                         (value pathname-defaults))))))
    (setq *active-file-group-name* group-name)
    (setq *active-file-group* (nreverse (read-file-group pathname nil)))
    (setf (getstring group-name *file-groups*) pathname)))


;;; READ-FILE-GROUP reads an Update format file and returns a list of pathnames
;;; of the files named in that file.  This guy knows about @@ indirection and
;;; ignores empty lines and lines that begin with @ but not @@.  A simpler
;;; scheme could be used for non-Spice implementations, but all this hair is
;;; probably useful, so Update format may as well be a standard for this sort
;;; of thing.
;;;
(defun read-file-group (pathname tail)
  (with-open-file (file pathname)
    (do* ((name (read-line file nil nil) (read-line file nil nil))
          (length (if name (length name)) (if name (length name))))
         ((null name) tail)
      (declare (type (or simple-string null) name))
      (cond ((zerop length))
            ((char= (char name 0) #\@)
             (when (and (> length 1) (char= (char name 1) #\@))
               (setq tail (read-file-group
                           (merge-pathnames (subseq name 2)
                                            pathname)
                           tail))))
            (t
             (push (merge-pathnames (pathname name) pathname) tail))))))



;;;; DO-ACTIVE-GROUP.



(defmacro do-active-group (current-window &rest forms)
  "This iterates over the active file group executing forms once for each
   file.  When forms are executed, the file will be in the current buffer,
   and the point will be at the start of the file."
  (let ((n-buf (gensym))
        (n-start-buf (gensym))
        (n-save (gensym)))
    `(progn
       (unless *active-file-group*
         (editor-error "There is no active file group."))

       (let ((,n-start-buf (window-buffer ,current-window))
             (,n-buf nil))
         (unwind-protect
             (dolist (file *active-file-group*)
               (catch 'file-not-found
                 (setq ,n-buf (group-read-file ,current-window file ,n-buf))
                 (with-mark ((,n-save (buffer-point (window-buffer ,current-window)) :right-inserting))
                   (unwind-protect
                       (progn
                         (buffer-start (buffer-point (window-buffer ,current-window)))
                         ,@forms)
                     (move-mark (buffer-point (window-buffer ,current-window)) ,n-save)))
           (group-save-file ,current-window)))
       (if (member ,n-start-buf *buffer-list*)
           (setf ;;(current-buffer) ,n-start-buf
            (window-buffer ,current-window) ,n-start-buf)
           (editor-error "Original buffer deleted!")))))))

;;; GROUP-READ-FILE reads in files for the group commands via DO-ACTIVE-GROUP.
;;; We use FIND-FILE-BUFFER, which creates a new buffer when the file hasn't
;;; already been read, to get files in, and then we delete the buffer if it is
;;; newly created and "Group Find File" is false.  This lets FIND-FILE-BUFFER
;;; do all the work.  We don't actually use the "Find File" command, so the
;;; buffer history isn't affected.
;;;
;;; Search-Buffer is any temporary search buffer left over from the last file
;;; that we want deleted.  We don't do the deletion if the buffer is modified.
;;;
(defun group-read-file (current-window name search-buffer)
  (unless (probe-file name)
    (message "File ~A not found." name)
    (throw 'file-not-found nil))
  (multiple-value-bind (buffer created-p)
                       (find-file-buffer current-window name)
    (setf (window-buffer current-window) buffer)
    ;;(setf (window-buffer (current-window)) buffer)

    (when (and search-buffer (not (buffer-modified search-buffer)))
      (dolist (w (buffer-windows search-buffer))
        (setf (window-buffer w) (window-buffer current-window)))
      (delete-buffer search-buffer (window-buffer current-window)))

    (if (and created-p (not (value group-find-file)))
        (window-buffer current-window) nil)))

;;; GROUP-SAVE-FILE is used by DO-ACTIVE-GROUP.
;;;
(defun group-save-file (current-window)
  (let* ((buffer (window-buffer current-window))
         (pn (buffer-pathname buffer))
         (name (namestring pn)))
    (when (and (buffer-modified buffer)
               (or (not (value group-save-file-confirm))
                   (prompt-for-y-or-n
                    current-window
                    :prompt (list "Save changes in ~A? " name)
                    :default t)))
      (save-file-command current-window ()))))



;;;; Searching and Replacing commands.

(defcommand "Group Search" (window p)
    "Searches the active group for a specified string, which is prompted for."
    "Searches the active group for a specified string."
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (let ((string (prompt-for-string window :prompt "Group Search: "
                                            :help "String to search for in active file group"
                                            :default *last-search-string*)))
      (get-search-pattern string :forward)
      (do-active-group
        window
        (do ((won (find-pattern buffer (buffer-point buffer) *last-search-pattern*)
                  (find-pattern buffer (buffer-point buffer) *last-search-pattern*)))
            ((not won))
          (character-offset buffer (buffer-point buffer) won)
          (command-case
              (:prompt "Group Search: "
               :help "Type a character indicating the action to perform."
               :change-window nil
               :current-window window)
            (:no "Search for the next occurrence.")
            (:do-all "Go on to the next file in the group."
              (return nil))
            ((:exit :yes) "Exit the search."
             (return-from group-search-command))
            (:recursive-edit "Enter a recursive edit."
                             (do-recursive-edit)
                             (get-search-pattern string :forward)))))
      (message "All files in group ~S searched." *active-file-group-name*))))

(defcommand "Group Replace" (window p)
  "Replaces one string with another in the active file group."
  "Replaces one string with another in the active file group."
  (declare (ignore p))
  (let* ((target (prompt-for-string window :prompt "Group Replace: "
                                    :help "Target string"
                                    :default *last-search-string*))
         (replacement (prompt-for-string window :prompt "With: "
                                         :help "Replacement string")))
    (do-active-group
        window
     (query-replace-function nil target replacement
                             "Group Replace on previous file" t))
    (message "Replacement done in all files in group ~S."
             *active-file-group-name*)))

(defcommand "Group Query Replace" (window p)
  "Query Replace for the active file group."
  "Query Replace for the active file group."
  (declare (ignore p))
  (let ((target (prompt-for-string window :prompt "Group Query Replace: "
                                   :help "Target string"
                                   :default *last-search-string*)))
    (let ((replacement (prompt-for-string window :prompt "With: "
                                          :help "Replacement string")))
      (do-active-group
          window
       (unless (query-replace-function window
                nil target replacement "Group Query Replace on previous file")
         (return nil)))
      (message "Replacement done in all files in group ~S."
               *active-file-group-name*))))
