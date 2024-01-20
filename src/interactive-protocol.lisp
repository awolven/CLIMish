(in-package :climish)

(defclass input-editing-stream () ())

(defmethod input-editing-stream-p ((stream input-editing-stream))
  t)

(defmethod input-editing-stream-p (stream)
  nil)

(defgeneric stream-scan-pointer (stream)
  #+Genera (:selector :read-location))

(defgeneric stream-rescanning-p (stream)
  #+Genera (:selector :rescanning-p))

;;; Fake methods to keep things like COMPLETE-INPUT from blowing up on
;;; non-input-editing streams like string streams.
(defmethod stream-scan-pointer ((stream t))
  (file-position stream))

(defmethod (setf stream-scan-pointer) (position (stream t))
  (file-position stream position))

(defmethod stream-rescanning-p ((object t)) nil)

(defmethod replace-input
           ((stream t) new-input &key (start 0) end rescan buffer-start)
  (declare (ignore new-input start end buffer-start rescan))
  nil)

(defmethod presentation-replace-input
           ((stream t) object type view
            &key rescan buffer-start query-identifier for-context-type)
  (declare (ignore object type view rescan buffer-start query-identifier for-context-type))
  nil)

;;; Specific implementation of input editing protocol mixin
(defclass input-editing-stream-mixin
          (standard-encapsulating-stream input-editing-stream)
      ;; The fill-pointer of INPUT-BUFFER is the "high water" mark,
      ;; that is, it points past the last thing in the buffer.
     ((input-buffer :initform (make-array 100 :fill-pointer 0 :adjustable t)
                    :accessor input-editor-buffer)
      ;; STREAM-SCAN-POINTER  is the point at which parsing takes place.
      (scan-pointer :initform 0 :accessor stream-scan-pointer)
      ;; STREAM-INSERTION-POINTER is where the input-editing cursor is,
      ;; that is, input editing commands and insertions take place
      ;; at the insertion pointer.
      (insertion-pointer :initform 0 :accessor stream-insertion-pointer)
      ;; STREAM-RESCANNING-P is now part of the input editing stream protocol.
      ;; If T, it means that the input editor is "rescanning", that is, re-processing
      ;; input that was already typed in response to some editing command.
      (rescanning-p :accessor stream-rescanning-p :initform nil)
      (rescan-queued :initform nil)
      ;; A buffer for an activation gesture to process.  Conceptually,
      ;; the activation gesture lives at the end of the input buffer.
      (activation-gesture :initform nil)
      ;; top level command state
      (command-mode :initform nil)
      ;; State for prefixed commands, holds a command aarray
      (command-state :initform nil)
      ;; Numeric argument for input editing commands
      (numeric-argument :initform nil)
      ;; State for passing information from c-Y/c-m-Y to m-Y
      (previous-history :initform nil)
      (previous-insertion-pointer :initform nil)
      ;; For deciding whether to do kill-ring merging, etc.
      (last-command-type :initform nil)
      ;; A mark that we can set
      (mark :initform nil)))

(defclass standard-input-editing-stream
          (input-editing-stream-mixin)
     ((start-x-position)
      (start-y-position)
      ;; all I want is :writer, but PCL barfs
      (original-stream-recording-p :accessor original-stream-recording-p)))

(defmethod initialize-position ((istream standard-input-editing-stream))
  (with-slots (stream start-x-position start-y-position) istream
    (multiple-value-bind (x y)
        ;; in this specific implementation it's ok for
        ;; us to know that our encapsulated stream will
        ;; support the extended output protocol.
        (stream-cursor-position stream)
      (setf start-x-position x
            start-y-position y))))

;; Do a rescan if one is pending.
;; If it's an "activation" rescan, that means that the user typed an activation
;; character at some point, so this rescan should terminate all input.  That's
;; why we set the insertion pointer to the end of the buffer...
;; In the cases where the program is maintaining another input buffer besides
;; the input editor's buffer, that buffer can get out of synch (for instance,
;; it could appear to be empty) because the user yanked something in and then
;; immediately hit <End>.  In that case, the yank commands have left a rescan
;; pending which we can take care of now.  In general, any piece of code that
;; is maintaining an input buffer separate from the input editor's buffer should
;; call RESCAN-IF-NECESSARY before parsing any input.
(defmethod rescan-if-necessary ((istream input-editing-stream-mixin)
                                &optional inhibit-activation)
  (with-slots (rescan-queued input-buffer insertion-pointer) istream
    (when rescan-queued
      (when (and (not inhibit-activation)
                 (eql rescan-queued ':activation))
        (setq insertion-pointer (fill-pointer input-buffer)))
      (setf rescan-queued nil)
      (throw 'rescan (values)))))
