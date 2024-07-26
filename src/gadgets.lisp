(in-package :climish)

(defobject gadget ())

(defobject basic-gadget (view gadget)
  :inputs
  ((id nil)
   (active? t)
   (client (the superior))
   (armed-callback nil)
   (disarmed-callback nil)
   (help-callback nil)))

(defmethod armed-callback ((gadget basic-gadget) (client t) (id t))
  nil)

(defmethod disarmed-callback ((gadget basic-gadget) (client t) (id t))
  nil)

(defmethod armed-callback :around ((gadget basic-gadget) (client t) (id t))
  (let ((callback (send gadget armed-callback)))
    (if callback
	(invoke-callback-function callback gadget)
	(call-next-method))))

(defmethod disarmed-callback :around ((gadget basic-gadget) (client t) (id t))
  (let ((callback (send gadget disarmed-callback)))
    (if callback
	(invoke-callback-function callback gadget)
	(call-next-method))))

(defmethod activate-gadget ((gadget basic-gadget))
  (unless (send gadget active?)
    (setf (send gadget active?) t)
    (note-gadget-activated (send gadget client) gadget)))

(defmethod note-gadget-activated ((client t) (gadget basic-gadget))
  nil)

(defmethod deactivate-gadget ((gadget basic-gadget))
  (when (send gadget active?)
    (setf (send gadget active?) nil)
    (note-gadget-deactivated (send gadget client) gadget)))

(defmethod note-gadget-deactivated ((client t) (gadget basic-gadget))
  nil)

(defobject value-gadget (basic-gadget)
  :inputs
  ((value (:noticer (value-changed-callback self (the client) (the id) value)))
   value-changed-callback))

(defmethod value-changed-callback ((gadget value-gadget) (client t) (id t) value)
  (declare (ignore value))
  nil)

(defmethod value-changed-callback :around ((gadget value-gadget) (client t) (id t) value)
  (let ((callback (send gadget value-changed-callback)))
    (when callback
      (invoke-callback-function callback gadget value))
    (call-next-method)))


(defobject action-gadget (basic-gadget)
  :inputs
  ((activate-callback nil)))

(defmethod activate-callback ((gadget action-gadget) (client t) (id t))
  nil)

(defmethod activate-callback :around ((gadget action-gadget) (client t) (id t))
  (let ((callback (send gadget activate-callback)))
    (when callback
      (invoke-callback-function callback gadget))
    (call-next-method)))

(defobject focus-gadget (basic-gadget)
  :inputs
  ((focus-out-callback nil)
   (focus-in-callback nil)))

(defmethod focus-out-callback ((gadget focus-gadget) (client t) (id t))
  nil)

(defmethod focus-in-callback ((gadget focus-gadget) (client t) (id t))
  nil)

(defmethod focus-out-callback :around ((gadget focus-gadget) (client t) (id t))
  (let ((callback (send gadget focus-out-callback)))
    (when callback
      (invoke-callback-function callback gadget))
    (call-next-method)))

(defmethod focus-in-callback :around ((gadget focus-gadget) (client t) (id t))
  (let ((callback (send gadget focus-in-callback)))
    (when callback
      (invoke-callback-function callback gadget))
    (call-next-method)))

(defobject oriented-gadget-mixin ()
  :inputs
  ((orientation :horizontal)))

(defobject labelled-gadget-mixin ()
  :inputs
  ((label "")
   (alignment :center)))

(defmethod print-object ((object labelled-gadget-mixin) stream)
  (if (and (slot-boundp object 'label)
	   (stringp (send object label)))
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~A" (send object label)))
      (call-next-method)))

(defobject range-gadget-mixin ()
  :inputs
  ((min-value 0.0)
   (max-value 1.0))

  :attributes
  ((range (- (the max-value) (the min-value)))))

(defmethod gadget-range* ((gadget range-gadget-mixin))
  (values (send gadget min-value)
	  (send gadget max-value)))

(defpackage climish-gadgets)

(defobject climish-gadgets::slider (value-gadget oriented-gadget-mixin range-gadget-mixin labelled-gadget-mixin)
  :inputs
  ((drag-callback nil)
   (decimal-places (cond ((<= (the range) 1) 2)
			 ((<= (the range) 10) 1)
			 (t 0)))
   (show-value? nil)
   (min-label nil)
   (max-label nil)
   (number-of-tick-marks 0)
   (number-of-quanta nil)
   (editable? t)))

(defmethod drag-callback ((gadget climish-gadgets::slider) (client t) (id t) value)
  (declare (ignore value))
  nil)

(defmethod drag-callback :around ((gadget slider) (client t) (id t) value)
  (let ((callback (send gadget drag-callback)))
    (when callback
      (invoke-callback-function callback gadget value))
    (call-next-method)))


