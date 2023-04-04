(in-package :vktk)

(defun load-fonts (&optional (fonts-list
			      (list
			       #+windows (list "C:/Windows/Fonts/DejaVuSansMono.ttf" 19.0f0)
			       #+windows #+NIL (list "C:/Windows/Fonts/SourceCodePro-Medium.ttf" 20.0f0)
			       #+windows #+NIL (list "C:/Windows/Fonts/SourceCodePro-MediumIt.ttf" 20.0f0
					       :italics t)
			       #+windows #+NIL (list "C:/Windows/Fonts/courbd.ttf" 15.0f0)
			       #+linux (list "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf" 19.0f0)
			       (list (concatenate
				      'string
				      (namestring (asdf/system:system-relative-pathname :vktk ""))
				      "/cimgui/imgui/misc/fonts/ProggyClean.ttf")
				     20.0f0))))
			 
  (let* ((io (ig::igGetIO))
	 (fonts (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::fonts)))
    (ig:ImFontAtlas_AddFontDefault fonts +nullptr+)
    (loop for font in fonts-list
       do (destructuring-bind (pathname size &rest plist) font
	    (push (make-instance 'ig::imgui-font
				 :handle (ig:ImFontAtlas_AddFontFromFileTTF
					  fonts pathname size +nullptr+ +nullptr+)
				 :pathname pathname
				 :size size
				 :plist plist)
		  (application-fonts *app*))))
    
    ;; // use ImGui::PushFont()/ImGui::PopFont() to change the font at runtime
    (values)))

;;(= (ig::font-size font) size)


(defun find-font (app &rest properties)
  (labels ((size-test (properties font)
	         (let ((size (getf properties :size)))
	           (when size
		         (= (ig::font-size font) size))))
	       (italics-test (properties font)
	         (let ((italics (getf properties :italics)))
	           (when italics
		         (search "It" (namestring (ig::font-pathname font)) :test #'char=))))
	       (not-italics-test (properties font)
	         (declare (ignore properties))
	         (not (search "It" (namestring (ig::font-pathname font)) :test #'char=)))
	       (bold-test (properties font)
	         (let ((bold (getf properties :bold)))
	           (when bold
		         (search "Bold" (namestring (ig::font-pathname font)) :test #'char=))))
	       (medium-test (properties font)
	         (let ((medium (getf properties :medium)))
	           (when medium
		         (search "Medium" (namestring (ig::font-pathname font)) :test #'char=))))
	       (pathname-test (properties font)
	         (let ((pathname (getf properties :pathname)))
	           (when pathname
		         (search (namestring pathname) (namestring (ig::font-pathname font)) :test #'char=)))))
    (or (find properties (application-fonts app)
		      :test (lambda (properties font)
			          (and (size-test properties font)
				           (pathname-test properties font)
				           (italics-test properties font)
				           #+N(print 0))))


	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and (size-test properties font)
			               (pathname-test properties font)
			               (bold-test properties font)
			               (not-italics-test properties font)
			               #+n(print 1))))

	    (find properties (application-fonts app)
		      :test (lambda (properties font)
			          (and (size-test properties font)
				           (pathname-test properties font)
				           (medium-test properties font)
				           (not-italics-test properties font)
				           #+N(print 2))))
	
	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and (size-test properties font)
			               (pathname-test properties font)
			               (not-italics-test properties font)
			               #+N(print 3))))

	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and
		               (pathname-test properties font)
		               (bold-test properties font)
		               #+N(print 4))))

	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and
		               (pathname-test properties font)
		               (medium-test properties font)
		               (not-italics-test properties font)
		               #+N(print 5))))

	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and (size-test properties font)
			               (italics-test properties font)
			               #+N(print 6))))

	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and (size-test properties font)
			               (bold-test properties font)
			               #+N(print 7))))
	
	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and (size-test properties font)
			               (medium-test properties font)
			               (not-italics-test properties font)
			               #+N(print 8))))
	
	    (find properties (application-fonts app)
	          :test (lambda (properties font)
		              (and (pathname-test properties font)
			               (not-italics-test properties font)
			               #+N(print 9))))

	    (find properties (application-fonts app)
		      :test (lambda (properties font)
			          (and (size-test properties font)
				           (not-italics-test properties font)
				           #+N(print 10))))

	    (find properties (application-fonts app)
		      :test (lambda (properties font)
			          (and
			           (pathname-test properties font)
			           (italics-test properties font)
			           #+N(print 11))))
	
	
	    )))
      


		    

(defmethod set-default-font (app)
  (when (application-fonts app)
    (let ((font (find-font app :pathname (preferred-font app) :size 20.0f0)))
      (setf (foreign-slot-value (ig:igGetIO) '(:struct ig::ImGuiIO) 'ig::FontDefault)
            (h (or font (first (application-fonts app)))))
      font)))
