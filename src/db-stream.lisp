;;;"Copyright (c) 1990 by International Lisp Associates.  All rights reserved.
;;; Portions copyright (c) 1991, 1992 Franz, Inc.  All rights reserved.
;;; Portions copyright (c) 1989, 1990 Symbolics, Inc.  All rights reserved."

(in-package :climish)

;;; CLIM stream sheets and panes

;;--- How to keep PANE-BACKGROUND/FOREGROUND in sync with the medium?
;;--- I'm not convinced that including WINDOW-STREAM here is right...
(defclass clim-stream-sheet
          (window-stream                        ;includes output recording
           sheet-permanently-enabled-mixin
           sheet-mute-input-mixin
           sheet-multiple-child-mixin
           space-requirement-mixin
           space-requirement-cache-mixin
           permanent-medium-sheet-output-mixin
           basic-pane)
    ((input-editor-stream :initform nil :accessor stream-input-editor-stream))
  (:default-initargs
    :medium t
    :transformation +identity-transformation+))

(defmethod stream-input-editor-stream ((stream sheet)) nil)
(defmethod (setf stream-input-editor-stream) (value (stream sheet))
  value)

(defmethod stream-input-editor-stream ((stream standard-encapsulating-stream))
  (stream-input-editor-stream (encapsulating-stream-stream stream)))

(defmethod (setf stream-input-editor-stream) (value (stream standard-encapsulating-stream))
  (setf (stream-input-editor-stream (encapsulating-stream-stream stream)) value))
