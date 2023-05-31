(in-package :hemlock)



(defcommand "Character-Stream Mode" (window p)
    "Put buffer in Character-Stream Mode"
    "Put buffer in Character-Stream Mode"
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (setf (buffer-minor-mode buffer "Character-Stream" buffer) t)))

(defcommand "Interactive-Character-Stream Mode" (window p)
    "Put buffer in Interactive-Character-Stream Mode"
    "Put buffer in Interactive-Character-Stream Mode"
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (setf (buffer-minor-mode buffer "Interactive-Character-Stream" buffer) t)))

(defclass hemlock-character-io-stream (fundamental-character-output-stream
                                       fundamental-character-input-stream)
  ((buffer :initform nil :accessor hemlock-character-io-stream-buffer
           :initarg :buffer)
   (mark :initform nil :accessor hemlock-character-io-stream-mark
         :initarg :mark)
   (line-count :initform 0 :accessor hemlock-character-io-stream-line-count)
   (timestamp-since-last-intermediate-ui-update
    :initform (get-internal-real-time) :accessor stream-timestamp)))

(defclass hemlock-interactive-character-io-stream (hemlock-character-io-stream)
  ((input-line :initform "" :accessor hemlock-interactive-character-io-stream-input-line)
   (input-line-position :initform 0 :accessor hemlock-interactive-character-io-stream-input-line-position)))

(defun make-hemlock-character-io-stream (buffer)
  (make-instance 'hemlock-character-io-stream
                 :buffer buffer
                 :mark (copy-mark (buffer-end-mark buffer) :right-inserting)))

(defun make-hemlock-interactive-character-io-stream (buffer)
  (make-instance 'hemlock-interactive-character-io-stream
                 :buffer buffer
                 :mark (copy-mark (buffer-end-mark buffer) :right-inserting)))

(defun setup-character-stream-mode (buffer)
  (unless (hemlock-bound-p 'buffer-stream :buffer buffer)
    (defhvar "Buffer Stream"
      "Character IO Stream used for Character Stream Mode"
      :buffer buffer
      :value (make-hemlock-character-io-stream buffer)))
  t)

(defun setup-interactive-character-stream-mode (buffer)
  (unless (hemlock-bound-p 'buffer-stream :buffer buffer)
    (defhvar "Buffer Stream"
      "interactive Character IO Stream used for Interactive-Character-Stream Mode"
      :buffer buffer
      :value (make-hemlock-interactive-character-io-stream buffer)))
  t)

(defmethod stream-write-char ((stream hemlock-character-io-stream) character)
  ;; maybe this stuff needs to be protected by without-interrupts.
  (let ((buffer (hemlock-character-io-stream-buffer stream)))
    (bt:with-recursive-lock-held ((buffer-lock buffer))
      (insert-character buffer (buffer-end-mark buffer) character) ;;only seems to work this way
      (move-mark (hemlock-character-io-stream-mark stream) (buffer-end-mark buffer)))
    (when (> (get-internal-real-time)
             (+ (stream-timestamp stream) (/ internal-time-units-per-second 20.0)))
      (f-o stream)
      (setf (stream-timestamp stream) (get-internal-real-time))))
  (values))

(defmethod stream-listen ((stream hemlock-character-io-stream))
  (and (next-character (hemlock-character-io-stream-buffer stream)
		       (hemlock-character-io-stream-mark stream))
       t))

(defmethod stream-listen ((stream hemlock-interactive-character-io-stream))
  (with-slots (input-line input-line-position) stream
    (when (and input-line input-line-position)
      (when (< -1 input-line-position (length input-line))
        (when (char= (char input-line (1- (length input-line))) #\Newline)
          t)))))

(defmethod stream-read-char ((stream hemlock-character-io-stream))
  (let ((maybe-character nil))
    (tagbody
     maybe-get-char 
       (setq maybe-character (next-character (hemlock-character-io-stream-buffer stream)
					     (hemlock-character-io-stream-mark stream)))
     check
       (unless maybe-character
         (sleep 0.1) ;; for in-process maybe some kind of (%command-loop) thingy?
         (go maybe-get-char))
     exit)
    (character-offset (hemlock-character-io-stream-mark stream) 1)
    maybe-character))

(defmethod stream-read-char ((stream hemlock-interactive-character-io-stream))
  ;; if eq "Hemlock Main" current thread name
  ;; (%command-loop :iterations x) [ or %command-loop :timeout 0.001 ]
  ;; else sleep 0.001
  (let ((char nil))
    (loop until (setq char (when (stream-listen stream)
                             (with-slots (input-line input-line-position) stream
                               (prog1 (char input-line input-line-position)
                                 (incf input-line-position)))))
          do (sleep 0.1)  ;; why this only works with sleep is a connundrum.

             )
    char))

(defmethod stream-read-char-no-hang ((stream hemlock-character-io-stream))
  (let ((maybe-character (next-character (hemlock-character-io-stream-buffer stream)
					 (hemlock-character-io-stream-mark stream))))
    (when maybe-character
      (character-offset (hemlock-character-io-stream-mark stream) 1))
    maybe-character))

(defmethod stream-read-char-no-hang ((stream hemlock-interactive-character-io-stream))
  (when (stream-listen stream)
    (with-slots (input-line input-line-position) stream
      (prog1 (char input-line input-line-position)
        (incf input-line-position)))))


(defun f-o (stream)
  (let ((buffer-windows (hi::buffer-windows (hemlock-character-io-stream-buffer stream))))
    (mapcar #'(lambda (window)
		(hi::maybe-rebottom-window-on-mark window (buffer-point (window-buffer window)))
		(krma.gui::redraw-buffer window))
	    buffer-windows)))

(defmethod stream-finish-output ((stream hemlock-character-io-stream))
  (f-o stream))

(defmethod stream-force-output ((stream hemlock-character-io-stream))
  (f-o stream))

(defmethod stream-unread-char ((stream hemlock-character-io-stream) character)
  (declare (ignore character))
  (if (eql (hi::mark-position (hemlock-character-io-stream-mark stream))
           (hi::mark-position (buffer-start-mark (hemlock-character-io-stream-buffer stream))))
      (error "Nothing to unread")
        (character-offset (hemlock-character-io-stream-mark stream) -1))
  (values))

(defmethod stream-unread-char ((stream hemlock-interactive-character-io-stream) character)
  (declare (ignore character))
  (with-slots (input-line input-line-position) stream
    (when (and input-line input-line-position)
      (if (<= 1 input-line-position (length input-line))
          (decf input-line-position)
          ;; will only work until the next "Confirm line of input" event.
          (error "Nothing to unread"))))
  (values))

(defmethod stream-peek-char ((stream hemlock-character-io-stream))
  (when (stream-listen stream)
    (next-character (hemlock-character-io-stream-buffer stream) (hemlock-character-io-stream-mark stream)) :eof))

(defmethod stream-peek-char ((stream hemlock-interactive-character-io-stream))
  ;; once again, a candidate for without-interrupts, all these gray stream methods are.
  (when (stream-listen stream)
    (with-slots (input-line input-line-position) stream
      (char input-line input-line-position))))

(defmethod stream-clear-input ((stream hemlock-character-io-stream))
  (let ((buffer (hemlock-character-io-stream-buffer stream)))
    (delete-region (hemlock-character-io-stream-buffer stream)
		   (region (hemlock-character-io-stream-mark stream)
                           (buffer-end-mark (hemlock-character-io-stream-buffer stream))))
    (mapcar #'krma.gui::redraw-buffer (buffer-windows buffer))
    (values)))

(defmethod stream-clear-input ((stream hemlock-character-io-stream))
  (with-slots (input-line input-line-position) stream
    (setf input-line-position 0 input-line ""))
  (call-next-method)
  (values))

(defmethod stream-start-line-p ((stream hemlock-character-io-stream))
  ;; maybe needs previous-character buffer-end-mark #\newline
  (let ((buffer (hemlock-character-io-stream-buffer stream)))
    (or (eql (hi::mark-position (hemlock-character-io-stream-mark stream))
             (hi::mark-position (buffer-start-mark buffer)))
        (eq (previous-character (hemlock-character-io-stream-buffer stream)
				(hemlock-character-io-stream-mark stream)) #\Newline))))

(defmethod stream-line-column ((stream hemlock-character-io-stream))
  (mark-charpos (hemlock-character-io-stream-mark stream)))

(defmethod stream-advance-to-column ((stream hemlock-character-io-stream) column)
  (let ((delta (- column (mark-charpos (hemlock-character-io-stream-mark stream)))))
    (if (minusp delta)
        nil
        (progn
          (dotimes (n delta)
            (stream-write-char stream #\Space))
          t))))

(defmethod stream-write-string ((stream hemlock-character-io-stream) string
                                        &optional (start 0) (end (length string)))
  (let ((buffer (hemlock-character-io-stream-buffer stream)))
    (bt:with-recursive-lock-held ((buffer-lock buffer))
      (insert-string buffer (buffer-end-mark buffer) string start end)
      ;; could only get this move-mark to work both here and in stream-write-char
      ;; without the buffer lock around these two forms,
      ;; the mark< type functions were finding that the line-%buffers of
      ;; the mark-lines were not the same (race condition w/ (setf window-buffer))
      (move-mark (hemlock-character-io-stream-mark stream) (buffer-end-mark buffer)))
    (when (> (get-internal-real-time)
             (+ (stream-timestamp stream) (/ internal-time-units-per-second 20.0)))
      (f-o stream)
      (setf (stream-timestamp stream) (get-internal-real-time))))
  ;; note: you can't type input and expect it to work during printing after printing is done, bmmr.
  (values))


(defcommand "Confirm Line of Input" (window p)
    "Absorb input from the buffer, a line at a time. For use with Interactive-Character-Stream buffers."
    "Absorb input from the buffer, a line at a time. For use with Interactive-Character-Stream buffers."
  (declare (ignore p))
  (let* ((cw window)
         (buffer (window-buffer cw))
         (stream (variable-value 'buffer-stream :buffer buffer))
	 (point (buffer-point buffer)))
    (insert-character buffer point #\Newline)
    (with-buffer-globals (buffer)
    (let* ((mark (hemlock-character-io-stream-mark stream))
	   (input-region (region mark (buffer-end-mark buffer)))
	   (string (region-to-string buffer input-region)))

      (move-mark
       (hemlock-character-io-stream-mark stream)
       (buffer-end-mark buffer))
      
      (with-slots (input-line input-line-position) stream
	(setf input-line-position 0
	      input-line (copy-seq string))))
    (buffer-end point buffer))
    (f-o stream)))



