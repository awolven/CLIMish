(in-package :climish)

;;; "Normal" output streams are never recording.
(defmethod stream-recording-p ((stream t)) nil)

;;; It is an error to set record-p on non-recording streams to non-NIL.
(defmethod (setf stream-recording-p) (new-value (stream t))
  (when new-value (error "Attempt to set RECORD-P for stream ~S" stream)))
