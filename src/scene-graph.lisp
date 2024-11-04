(in-package :climish)

(defun safe-euclid (vec4)
  (if (<= (abs (vw vec4)) single-float-epsilon)
      (vec3 (vx vec4) (vy vec4) (vz vec4))
      (vec3 (/ (vx vec4) (vw vec4))
	    (/ (vy vec4) (vw vec4))
	    (/ (vz vec4) (vw vec4)))))

(defgeneric rm-erase-node (node)
  (:documentation "Erases retained mode portion of a node and it's descendants.  Default method does nothing.")
  (:method (node) (declare (ignore node)) (values)))

(defgeneric rm-draw-node (node)
  (:documentation "Redraws retained mode portion of a node and it's descendants.  Does not delete old geometry. Default method does nothing.")
  (:method (node) (declare (ignore node)) (values)))

(defmethod rm-erase-node-recursively (self)
  (values))



(defun rm-draw-node-recursively (self)
  "Draws a node and all of it's descendants."
  (rm-draw-node self)
  (mapcar #'rm-draw-node-recursively (the node-children))
  (values))

(defmethod rm-redraw-node (node)
  (rm-erase-node node)
  (rm-draw-node node)
  (values))

(defmethod rm-redraw-node-recursively (self)
  (values))

(defobject group-mixin ()
  :inputs
  ((group :defaulting)))

(defobject selectable-mixin ()
  :slots
  ((object-id :initform (new-object-id)))

  :inputs
  ((hovered? nil)))

(defmethod initialize-instance :after ((self selectable-mixin) &rest initargs)
  (declare (ignorable initargs))
  (setf (krma::object-from-id (the object-id)) self)
  (values))

(defmethod forget-object (self)
  (values))

(defmethod forget-object ((self selectable-mixin))
  (remhash (the object-id) *object-id->object-table*)
  (values))

(defmethod forget-object-recursively (thing)
  (declare (ignore thing))
  (values))	   

(defun node-set-model-matrix (self)
  (let ((scene (the scene))
	(matrix (the global-matrix)))
    (mapcar #'(lambda (handle)
		(primitive-set-transform scene handle matrix))
	    (the primitive-handles))))

(defmethod clim:handle-event :around ((self selectable-mixin) (event clim:pointer-enter-event))
  (setf (the hovered?) t)
  (when (next-method-p)
    (call-next-method)))

(defmethod clim:handle-event :around ((self selectable-mixin) (event clim:pointer-exit-event))
  (setf (the hovered?) nil)
  (when (next-method-p)
    (call-next-method)))

(defobject node-mixin ()
  :slots
  ((primitive-handles :initform ()))
  
  :inputs
  ((visible? t)
   (origin
    (:noticer (krma-set-model-matrix-recursively self))
    (vec3 0 0 0))
   (rotation
    (:noticer (krma-set-model-matrix-recursively self))
    (quat 0 0 0 1))
   (scene :defaulting))

  :default-initargs (scene (krma:default-scene))
  
  :attributes
  ((rotation-matrix
    (matrix-from-quaternion (the rotation)))

   (translation-matrix
    (let* ((origin (the origin))
	   (x (vx origin))
	   (y (vy origin))
	   (z (vz origin)))
      (mat4 1 0 0 x
	    0 1 0 y
	    0 0 1 z
	    0 0 0 1)))


   (matrix (m* (the translation-matrix) (the rotation-matrix)))
   
   (global-matrix (if (null (the superior))
		      (the matrix)
		      (m* (the superior global-matrix) (the matrix))))

   (inverse-matrix (minv (the matrix)))

   (inverse-global-matrix (minv (the global-matrix)))
		    
   (node-children (remove-if-not #'(lambda (child)
				     (typep child 'node-mixin))
				 (the children)))))

(defmethod add-to-scene (node &optional (scene (krma:default-scene)))
  (push node (krma::node-children scene))
  (rm-redraw-node-recursively node)
  (values))  

(defun remove-from-scene (node &optional (scene (krma:default-scene)))
  (rm-erase-node-recursively node)
  (setf (krma::node-children scene)
	(remove node (krma::node-children scene)))
  (values))

(defmethod rm-erase-node-recursively ((self node-mixin))
  (rm-erase-node self)
  (mapcar #'rm-erase-node-recursively (the node-children))
  (values))

(defmethod rm-redraw-node-recursively ((self node-mixin))
  (rm-redraw-node self)
  (mapcar #'rm-redraw-node-recursively (the node-children))
  (values))

(defmethod forget-object-recursively ((self node-mixin))
  (forget-object self)
  (mapcar #'forget-object-recursively (the node-children))
  (values))

(defobject group-owner-mixin (group-mixin)
  :slots
  ((group :initform (new-group) :accessor object-group)))

(defmethod initialize-instance ((self group-owner-mixin) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (krma::scene-ensure-group (the scene) (the group)))

(defmethod node-parent ((self node-mixin))
  (the superior))

(defmethod rm-erase-node ((self node-mixin))
  (delete-primitives (the scene) (the primitive-handles))
  (setf (the primitive-handles) nil)
  (values))

(defmethod rm-erase-node ((self group-owner-mixin))
  (delete-group (the scene) (the group) nil)
  (values))

(defmethod rm-draw-node :after ((self node-mixin))
  (if (typep self 'group-owner-mixin)
      (group-set-transform (the scene) (the group) (the global-matrix))
      (node-set-model-matrix self))
  (values))

(defmethod krma-set-model-matrix-recursively ((self node-mixin))
  (node-set-model-matrix self)
  (mapcar #'krma-set-model-matrix-recursively (the node-children))
  (values))

(defmethod krma-set-model-matrix-recursively ((self group-owner-mixin))
  (group-set-transform (the scene) (the group) (the global-matrix))
  (mapcar #'krma-set-model-matrix-recursively (the node-children)))  

(defmethod adhoc::unbind-this-variable :before ((variable adhoc::component-variable))
  (when (slot-boundp variable 'adhoc::value) 
    (let ((object (adhoc::variable-value variable)))
      (forget-object-recursively object)
      (rm-erase-node-recursively object)
      (values))))

(defmethod adhoc::unbind-this-variable :before ((variable adhoc::array-aggregate-mixin))
  (when (slot-boundp variable 'adhoc::value) 
    (let ((value (adhoc::variable-value variable)))
      (loop for element across value
	 when element
	 do (forget-object-recursively element)
	   (rm-erase-node-recursively element))
      (values))))

(defmethod adhoc::unbind-this-variable :before ((variable adhoc::table-aggregate-mixin))
  (when (slot-boundp variable 'adhoc::value) 
    (let ((value (adhoc::variable-value variable)))
      (maphash #'(lambda (k v)
		   (declare (ignore k))
		   (forget-object-recursively v)
		   (rm-erase-node-recursively v))
	       value)
      (values))))

(defmethod translate-node ((self node-mixin) (vec vec3))
  (setf (the origin) (v+ (the origin) vec))
  (values))


(defun adhoc::adhoc-redefinition-hook ()
  #+NIL
  (when (boundp 'krma::*media*)
    (loop for node in (krma::node-children (krma:default-scene))
	  do (rm-redraw-node-recursively node))))

(defun touch-instance-recursively (self)
  (the superior)
  (mapcar #'touch-instance-recursively (the children))
  (values))

#+NOTNOW
(defmethod update-instance-for-redefined-class :after ((self node-mixin)
						       added-slots discarded-slots plist &rest initargs)
  (declare (ignore added-slots discarded-slots plist initargs))
  ;; when an adhoc class is recompiled, it blasts all the generative data including the children
  ;; but the instances themselves remain with normal slots having their old values
  ;; so group and primitive-handles and object-id remain in the instance which was redefined
  ;; the first level children are already recursively erased when the children are blasted
  ;; so an rm-draw-recursively will generate the new children and draw them

  ;; retained mode has it's advantages wrt lower cpu load, but drawing/erasing/redrawing is more complicated
  ;;(rm-redraw-node self)
  ;;(mapcar #'rm-draw-node-recursively (the node-children))
  (values))

(defgeneric transform-coordinate (coordinate node)
  (:documentation "Given a vector and a node, return a new vector which is the transformation of the original vector into the parent coordinate system of the node."))

(defmethod transform-coordinate ((vec 3dm.d:vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the matrix))))
    (safe-euclid vec4)))

(defmethod transform-coordinate ((vec 3dm.f:vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the matrix))))
    (safe-euclid vec4)))

(defgeneric global-transform-coordinate (coordinate node)
  (:documentation "Given a vector and a node, return a new vector which is the transformation of the original vector into the global coordinate system."))

(defmethod global-transform-coordinate ((vec 3dm.d::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the global-matrix))))
    (safe-euclid vec4)))

(defmethod global-transform-coordinate ((vec 3dm.f::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the global-matrix))))
    (safe-euclid vec4)))

(defgeneric untransform-coordinate (coordinate node)
  (:documentation "Given a vector and a node, where the vector is represented in the parent coordinate system of the node, return a new vector which is represented in the coordinate system of the node itself."))

(defmethod untransform-coordinate ((vec 3dm.d::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod untransform-coordinate ((vec 3dm.f::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod transform-coordinate-to ((vec 3dm.d::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defmethod transform-coordinate-to ((vec 3dm.f::vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-matrix))))
    (safe-euclid vec4)))

(defgeneric untransform-global-coordinate (global-coordinate node)
  (:documentation "Given a global coordinate, return a new vector which is the the coordinate as represented in the node's coordinate system."))

(defmethod untransform-global-coordinate ((vec vec3) (self node-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (the inverse-global-matrix))))
    (safe-euclid vec4)))








