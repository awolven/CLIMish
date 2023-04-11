(in-package :hi)

(defclass hi::window ()())
(defclass hi::buffer ()())


(defclass krma.gui::text-editor-view-mixin (hi::window)
  ((width-in-chars
    :accessor hi::window-width
    :initform nil
    :initarg :width-chars
    :documentation "Width of the window in characters.")

   (height-in-lines
    :accessor hi::window-height
    :initform nil
    :initarg :height-lines
    :documentation "Height of the window in lines.")

   (tick
    :accessor hi::window-tick
    :initform nil
    :initarg :tick
    :documentation "The last time this window was updated.")

   (%buffer
    :accessor hi::window-%buffer
    :initform nil
    :initarg :%buffer
    :documentation "buffer displayed in this window.")

   (old-start
    :accessor hi::window-old-start
    :initform nil
    :initarg :old-start
    :documentation "The charpos of the first char displayed.")

   (first-line
    :accessor hi::window-first-line
    :initform nil
    :initarg :first-line
    :documentation "The head of the list of dis-lines.")

   (last-line
    :accessor hi::window-last-line
    :initform nil
    :initarg :last-line
    :documentation "The last dis-line displayed.")

   (first-changed
    :accessor hi::window-first-changed
    :initform nil
    :initarg :first-changed
    :documentation "The first changed dis-line on last update.")

   (last-changed
    :accessor hi::window-last-changed
    :initform nil
    :initarg :last-changed
    :documentation "The last changed dis-line.")

   (spare-lines
    :accessor hi::window-spare-lines
    :initform nil
    :initarg :spare-lines
    :documentation "The head of the list of unused dis-lines")

   (old-lines
    :accessor hi::window-old-lines
    :initform 0
    :initarg :old-lines
    :documentation "Slot used by display to keep state info")

   (display-start
    :accessor hi::window-display-start
    :initform nil
    :initarg :display-start
    :documentation "first character position displayed")

   (display-end
    :accessor hi::window-display-end
    :initform nil
    :initarg :display-end
    :documentation "last character displayed")

   (point
    :accessor hi::window-point
    :initform nil
    :initarg :point
    :documentation "Where the cursor is in this window.")

   (modeline-dis-line
    :accessor hi::window-modeline-dis-line
    :initform nil
    :initarg :modeline-dis-line
    :documentation "Dis-line for modeline display.")

   (modeline-buffer
    :accessor hi::window-modeline-buffer
    :initform nil
    :initarg :modeline-buffer
    :documentation "Complete string of all modeline data.")

   (modeline-buffer-len
    :accessor hi::window-modeline-buffer-len
    :initform nil
    :initarg :modeline-buffer-len
    :documentation "Valid chars in modeline-buffer.")

   (display-recentering
    :accessor hi::window-display-recentering
    :initform nil
    :initarg :display-recentering
    :documentation "Tells whether redisplay recenters window, regardless of whether it is current.")

   (hi::the-sentinel ;; the sentinels are specific to each window 'cause they get line-%buffer set
    :accessor hi::window-the-sentinel
    :initarg :the-sentinel
    :documentation "This dis-line, which has several interesting properties, is used to end
  lists of dis-lines.")

   (font-family
    :accessor hi::window-font-family
    :initform nil
    :initarg :font-family
    :documentation "The default font for a hemlock window.")))

(defmethod initialize-instance :after ((view krma.gui::text-editor-view-mixin)
				       &rest args
				       &key
					 (buffer (make-instance 'krma.gui::text-editor-buffer))
					 (width-chars 80)
					 (height-lines 35)     
					 (start (hi::buffer-start-mark buffer))
					 (display-start (hi::copy-mark start :right-inserting))
					 (old-start (hi::copy-mark start :temporary))
					 (display-end (hi::copy-mark start :right-inserting))
					 (point (hi::copy-mark (hi::buffer-point buffer)))
					 (tick -1)
					 (the-sentinel (hi::make-the-sentinel))
					 (first-line (cons hi::dummy-line the-sentinel))
					 (last-line the-sentinel)
					 (first-changed the-sentinel)
					 (last-changed first-line)
					 (font-family (krma::default-system-font (clui::default-display)))
				       &allow-other-keys)
  (declare (ignore args))
  (setf (hi::window-%buffer view) buffer)
  (setf (hi::window-display-start view) display-start)
  (setf (hi::window-old-start view) old-start)
  (setf (hi::window-display-end view) display-end)
  (setf (hi::window-tick view) tick)
  (setf (hi::window-point view) point)
  (setf (hi::window-first-line view) first-line)
  (setf (hi::window-last-line view) last-line)
  (setf (hi::window-first-changed view) first-changed)
  (setf (hi::window-last-changed view) last-changed)
  (setf (hi::window-the-sentinel view) the-sentinel)
  (setf (hi::window-font-family view) font-family)
  (setf (hi::window-width view) width-chars)
  (setf (hi::window-height view) height-lines)
  

  (do ((i (- height-lines) (1+ i))
       (res ()
	    (cons (hi::make-window-dis-line (make-string width-chars)) res)))
      ((= i height-lines) (setf (hi::window-spare-lines view) res)))

  (hi::invoke-hook hemlock::make-window-hook view)
  (values))

(defclass krma.gui::text-editor-buffer-mixin (hi::buffer)
  ((lock
    :reader hi::buffer-lock
    :initform (bt:make-recursive-lock "buffer lock"))
   
   (hi::open-line
    :accessor hi::buffer-open-line
    :documentation "Line open for hacking on."
    :initform nil)
   
   (hi::open-chars
    :accessor hi::buffer-open-chars
    :documentation "A Vector of characters for hacking on."
    :initform (make-string 200))
   
   (hi::left-open-pos
    :accessor hi::buffer-left-open-pos
    :documentation "Index to first free character to left of mark in Open-Chars."
    :initform 0)
   
   (hi::right-open-pos
    :accessor hi::buffer-right-open-pos
    :documentation "Index to first used character to right of mark in Open-Chars."
    :initform 0)
   
   (hi::line-cache-length
    :accessor buffer-line-cache-length
    :initform 200
    :documentation "Length of Open-Chars.")

   (hi::*internal-temp-region*
    :initform (hi::make-empty-region))

   (hi::*internal-temp-mark*
    :initform (hi::internal-make-mark nil nil :temporary))

   (hi::last-was-undo-p
    :initform nil)
   
   (hi::this-is-undo-p
    :initform nil)
   
   (hi::undoing-undo-list
    :initform nil)

   (hi::%name
    :accessor hi::buffer-%name
    :initform nil
    :initarg :%name)
   
   (hi::%region
    :accessor hi::buffer-%region
    :initform nil
    :initarg :%region)
   
   (hi::%pathname
    :accessor hi::buffer-%pathname
    :initform nil
    :initarg :%pathname)
   
   (hi::modes
    :accessor hi::buffer-modes
    :initform nil
    :initarg :modes)
   
   (hi::mode-objects
    :accessor hi::buffer-mode-objects
    :initform nil
    :initarg :mode-objects)
   
   (hi::bindings :accessor hi::buffer-bindings
	     :initform nil
	     :initarg :bindings)
   
   (hi::.point.
    :accessor hi::buffer-point
    :initform nil
    :initarg :point)
   
   (hi::%writable
    :accessor hi::buffer-%writable
    :initform t
    :initarg :%writable)
   
   (hi::modified-tick
    :accessor hi::buffer-modified-tick
    :initform -2
    :initarg :modified-tick)
   
   (hi::unmodified-tick
    :accessor hi::buffer-unmodified-tick
    :initform -1
    :initarg :unmodified-tick)
   
   (hi::windows
    :accessor hi::buffer-windows
    :initform nil
    :initarg :windows)
   
   (hi::var-values
    :accessor hi::buffer-var-values
    :initform nil
    :initarg :var-values)
   
   (hi::variables
    :accessor hi::buffer-variables
    :initform nil
    :initarg :variables)
   
   (hi::write-date
    :accessor hi::buffer-write-date
    :initform nil
    :initarg :write-date)
   
   (hi::active-region-p
    :accessor hi::buffer-active-region-p
    :initform nil
    :initarg :active-region-p)
   
   (hi::display-start
    :accessor hi::buffer-display-start
    :initform nil
    :initarg :display-start)
   
   (hi::%modeline-fields
    :accessor hi::buffer-%modeline-fields
    :initform nil
    :initarg :%modeline-fields)
   
   (hi::delete-hook
    :accessor hi::buffer-delete-hook
    :initform nil
    :initarg :delete-hook)
   
   (hi::widget
    :accessor hi::buffer-widget
    :initform nil
    :initarg :widget)
   
   (hi::tag-line-number
    :accessor hi::buffer-tag-line-number
    :initform -1
    :initarg :tag-line-number)
   
   (hi::undo-p
    :accessor hi::buffer-undo-p
    :initform t
    :initarg :undo-p)
   
   (hi::undo-list
    :accessor hi::buffer-undo-list
    :initform nil
    :initarg :undo-list)

   (hi::plist
    :accessor hi::buffer-plist
    :initform nil)))

(defmethod hi::bufferp (thing)
  (declare (ignore thing))
  nil)

(defmethod hi::bufferp ((thing krma.gui::text-editor-buffer-mixin))
  t)

(defmethod print-object ((object krma.gui::text-editor-buffer-mixin) stream)
  (hi::%print-hbuffer object stream nil))

(defmethod initialize-instance :after ((buffer krma.gui::text-editor-buffer-mixin)
				       &rest args
				       &key
					 (name "*scratch*")
					 (modes (hi::value hemlock::default-modes))
				       (modeline-fields
					(hi::value hemlock::default-modeline-fields))
				       delete-hook
				       &allow-other-keys)
  (declare (ignore args))
  (unless (listp delete-hook)
    (error ":delete-hook is a list of functions -- ~S." delete-hook))

  (let* ((region (hi::make-empty-region))
	 (object (hi::getstring "Fundamental" hi::*mode-names*)))

    (setf (hi::buffer-%name buffer) name)
    (setf (hi::buffer-%region buffer) region)
    (setf (hi::buffer-modes buffer) (list (hi::mode-object-name object)))
    (setf (hi::buffer-mode-objects buffer) (list object))
    (setf (hi::buffer-point buffer) (hi::copy-mark (hi::region-end region)))
    (setf (hi::buffer-display-start buffer) (hi::copy-mark (hi::region-start region)))
    (setf (hi::buffer-delete-hook buffer) delete-hook)
    (setf (hi::buffer-variables buffer) (hi::make-string-table))

    (hi::sub-set-buffer-modeline-fields buffer modeline-fields)

    (setf (hi::line-%buffer (hi::mark-line (hi::region-start region))) buffer)

    (push buffer hi::*buffer-list*)

    (setf (hi::getstring name hi::*buffer-names*) buffer)

    (unless (equalp modes '("Fundamental"))
      (setf (hi::buffer-major-mode buffer) (car modes))
      (dolist (m (cdr modes))
	(setf (hi::buffer-minor-mode buffer m) t)))

    #+NOTYET
    (hi::invoke-hook hemlock::make-buffer-hook buffer)

    (values)))
	  
	  
(defclass krma.gui::text-editor-buffer (krma.gui::text-editor-buffer-mixin) ())
