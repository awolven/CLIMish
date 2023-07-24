(adhoc:defpackage :krma.gui
  (:use :cl :clui :krma :3dm
	:hemlock-interface)

  (:import-from :3dm.f
		   #:vec2
		   #:vec3
		   #:vec4
		   #:mat2
		   #:mat3
		   #:mat4
		   #:matn
		   #:quat
		   #:meye
		   #:with-vec2
		   #:with-vec3
		   #:with-vec4
		   #:with-vec)

  (:export #:redraw-buffer))



