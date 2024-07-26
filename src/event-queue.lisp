(in-package :climish)

(defun beep (stream)
  (declare (ignore stream))
  (hi::bell))

(defmethod stream-supports-input-editing ((stream trivial-gray-streams:fundamental-stream)) t)

(defmethod stream-supports-input-editing ((stream t)) nil)

(defconstant *end-of-file-marker* :eof)
(deftype end-of-file-marker ()
  '(member :eof))


(defclass encapsulating-stream () ())

(defclass standard-encapsulating-stream (encapsulating-stream)
  ((stream :initarg :stream
	   :reader encapsulating-stream-stream)))

(defmethod stream-encapsulates-stream-p
    ((encapsulator standard-encapsulating-stream) stream)
  (let ((encapsulated-stream (encapsulating-stream-stream encapsulator)))
    (or (eq encapsulated-stream stream)
        (stream-encapsulates-stream-p encapsulated-stream stream))))

(defmethod stream-encapsulates-stream-p ((stream1 stream) (stream2 stream))
  nil)

(defun encapsulating-stream (stream)
  (declare (special *original-stream*)) ;;-- fwd reference
  (if (and *original-stream*
           (stream-encapsulates-stream-p *original-stream* stream))
      *original-stream*
    stream))

(defclass basic-extended-input-protocol
    (trivial-gray-streams:fundamental-character-input-stream)
  ())

(define-stream-protocol basic-extended-input-protocol
  stream-input-buffer
  stream-pointers
  stream-primary-pointer
  stream-text-cursor
  stream-read-gesture-cursor-state
  )

(defmethod extended-input-stream-p ((stream basic-extended-input-protocol))
  t)

(defmethod extended-input-stream-p (stream)
  nil)

(defclass input-protocol-mixin (basic-extended-input-protocol)
  ((input-event-queue
    :initform (lparallel.queue:make-queue)
    :reader input-event-queue)))


(defmethod stream-peek-event ((stream input-protocol-mixin))
  (lparallel.queue:peek-queue (input-event-queue stream)))

(defun peek-event (stream)
  (stream-peek-event stream))

(defmethod stream-read-event ((stream input-protocol-mixin)
			      &key (timeout nil)
				peek-p input-wait-test input-wait-handler
				pointer-button-press-handler)
  (declare (ignorable peek-p input-wait-test input-wait-handler pointer-button-press-handler))
  (if timeout
      (lparallel.queue:try-pop-queue (input-event-queue stream) :timeout timeout)
      (lparallel.queue:pop-queue (input-event-queue stream))))

(defun read-event (stream &key (timeout nil))
  (stream-read-event stream :timeout timeout))

(defmethod stream-unread-event ((stream input-protocol-mixin) event)
  (queue-unget (input-event-queue stream) event))

(defun unread-event (event &key stream)
  (stream-unread-event stream event))

(defmethod stream-read-char ((stream input-protocol-mixin))
  (let ((event nil))
    (loop
      (setq event (stream-read-event (encapsulating-stream stream)))
      (if (characterp event)
	  (return-from stream-read-char event)
	  (when (typep event 'clui::character-event-mixin)
	    (return-from stream-read-char (clui::input-event-character event)))))))

(defmethod stream-unread-char ((stream input-protocol-mixin) character)
  (stream-unread-event (encapsulating-stream stream) character))

(defmethod stream-read-char-no-hang ((stream input-protocol-mixin))
  (let ((event nil))
    (loop
      (setq event (stream-read-event (encapsulating-stream stream) 0))
      (if (characterp event)
	  (return-from stream-read-char-no-hang event)
	  (when (typep event 'clui::character-event-mixin)
	    (return-from stream-read-char-no-hang (clui::input-event-character event)))))))

(defmethod stream-peek-char ((stream input-protocol-mixin))
  (let ((event (stream-read-event (encapsulating-stream stream))))
    (prog1 (when (typep event 'clui::character-event-mixin)
	     (clui::input-event-character event))
      (stream-unread-event (encapsulating-stream stream) event))))

(defmethod stream-listen ((stream input-protocol-mixin))
  (map-over-queue #'(lambda (thing)
		      (if (characterp thing)
			  (return-from stream-listen t)
			  (when (typep thing 'clui::character-event-mixin)
			    (return-from stream-listen t))))
		  (input-event-queue stream))
  nil)
  
(defmethod stream-read-line ((stream input-protocol-mixin))
  (with-temporary-string (result :length 100 :adjustable t)
    ;; Read the first char separately, since we are supposed to react differently
    ;; to EOF on the first char than we do when in the middle of a line.
    (let ((ch (stream-read-char stream)))
      ;;--- Reconcile various error cases.  When CH is NIL then that probably means
      ;; that the caller supplied error-p nil, and we may have to return the eof-val.
      ;; Of course, we aren't dealing with EOF on our window streams at all.
      (unless (eq ch *end-of-file-marker*)
        (loop
          ;; Process the character
          (cond ((or (eql ch #\Newline)
                     (eq ch *end-of-file-marker*))
                 (return-from stream-read-line
                   (evacuate-temporary-string result)))
                (t
                 ;; Be robust against weird characters
                 (if (characterp ch)
                     (vector-push-extend ch result)
                     (hi::bell))))
          (setq ch (stream-read-char stream)))))))		  

(defmethod stream-clear-input ((stream input-protocol-mixin))
  (queue-flush (input-event-queue stream)))


(defun modifier-key-p (event-id)
  (or (eq (input-event-id-scancode event-id) clui::+key-left-alt+)
      (eq (input-event-id-scancode event-id) clui::+key-right-alt+)
      (eq (input-event-id-scancode event-id) clui::+key-left-GUI+)
      (eq (input-event-id-scancode event-id) clui::+key-right-GUI+)
      (eq (input-event-id-scancode event-id) clui::+key-left-ctrl+)
      (eq (input-event-id-scancode event-id) clui::+key-right-ctrl+)
      (eq (input-event-id-scancode event-id) clui::+key-left-shift+)
      (eq (input-event-id-scancode event-id) clui::+key-right-shift+)))



;; handle-event :after application-frame-mixin pointer-press-event and pointer-release-event
;; are in views.lisp

(defun message (format-string &rest args)
  (apply #'format *debug-io* format-string args)
  (finish-output *debug-io*))

(defmethod handle-character-input (window char p)
  (declare (ignore window char p)))

;;; Reading and writing of tokens

(defparameter *quotation-character* #\")

;; READ-TOKEN reads characters until it encounters an activation gesture,
;; a delimiter character, or something else (like a mouse click).
(defun read-token (stream &key #+NOTYET #+NOTYET input-wait-handler pointer-button-press-handler
                               click-only timeout)
  (with-temporary-string (string :length 50 :adjustable t)
    (let* ((event nil)
           (event-type nil)
           (quote-seen nil)
           (old-delimiters *delimiter-gestures*)
           (*delimiter-gestures* *delimiter-gestures*))
      (flet ((return-token (&optional unread)
               (when unread
                 (unread-event unread :stream stream))
               (when (and (activation-gesture-p unread)
                          (input-editing-stream-p stream))
                 (rescan-if-necessary stream))
               (return-from read-token
                 (values (evacuate-temporary-string string)))))
        (loop
          (multiple-value-setq (event event-type)
            (stream-read-event stream
			       #+NOTYET #+NOTYET #+NOTYET #+NOTYET
				 :input-wait-handler
				 (or input-wait-handler
				     *input-wait-handler*)
				 :pointer-button-press-handler
				 (or pointer-button-press-handler
				     *pointer-button-press-handler*)
				 #+NOTYET #+NOTYET
			       :timeout (and click-only timeout)))

	  (cond ((typep event 'clim:pointer-motion-event)
		   ;; do nothing
		 )
		((and (typep event 'clim:key-press-event)
		      (eq (input-event-id event) #k"Enter"))
		 (return-token))

		  #+NOTYET((eq event-type :timeout)
                   (return-from read-token :timeout))
                  #+NOTYET((and click-only (eq event :eof))
                   (error "Got an EOF when waiting for mouse input"))
                  #+NOTYET((eq event :eof)
                   (return-token event))
                  ((and click-only
			(not (typep event 'clim:pointer-button-event)))
                   (beep stream))
                  ((typep event 'clim:pointer-button-event)
		   ;; No need to do anything, since this should have been handled
		   ;; in the presentation type system already
                   )
                  ((typep event 'clui::character-event-mixin)
		   (let ((character (input-event-id event)))
		     (cond ((and (zerop (fill-pointer string))
				 (eql character *quotation-character*))
			    (setq quote-seen t)
			    (setq *delimiter-gestures* nil))
			   ((and quote-seen
				 (eql character *quotation-character*))
			    (setq quote-seen nil)
			    (setq *delimiter-gestures* old-delimiters))
			   ((activation-gesture-p event)
			    (return-token character))
			   ((delimiter-gesture-p event)
			    ;; ditto?
                            (return-token character))
			   ((ordinary-char-p character)
                            (vector-push-extend character string)
			    ;;--- haven't updated WRITE-CHAR yet
                            #+++ignore (write-char gesture stream))
			   (t (beep stream)))))
	    
                  #+NOTYET((activation-gesture-p event)
                   (return-token event))
                  #+NOTYET((delimiter-gesture-p event)
                   (return-token event))
                  (t (beep stream))))))))







		   





