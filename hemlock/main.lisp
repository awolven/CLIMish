(in-package :vktk)

(defvar *buffer-modifying-threads* nil)
(defvar *waiting-for* nil)

(defvar *imgui-padding-y* 28)

(defvar *main-window* nil)

(defvar *error-msg* nil)

(defvar *backtrace* nil)

(defun record-backtrace ()
  (setf (backtrace-string *app*)
        (with-output-to-string (*debug-io*)
          (sb-debug:print-backtrace))))

(defun record-error-msg (c)
  (let ((*print-escape* nil))
    (setf (error-message *app*)
          (format nil "~W" c))))

(defmacro reroute-debug (&body body)
  `(restart-bind ((ignore (lambda (&optional c)
			                (declare (ignorable c))
			                (throw :ignore nil))))
     (catch :ignore
       (handler-bind ((error (lambda (c)
			                   (record-error-msg c)
			                   (record-backtrace))))
         ,@body))))

(defclass text-editor-mixin ()
  ((preferred-font :accessor preferred-font :initform "DejaVuSansMono")
   (default-font :accessor vktk::default-font)
   (error-message :accessor vktk::error-message :initform nil)
   (backtrace-string :accessor vktk::backtrace-string :initform nil)

   (hemlock-windows-created-p :accessor vktk::hemlock-windows-created-p
                              :initform nil)

   (imgui-window-created-semaphore-signalled-once
    :accessor vktk::imgui-window-created-semaphore-signalled-once
    :initform nil)

   (imgui-window-created-semaphore
    :accessor vktk::imgui-window-created-semaphore
    :initform (bt:make-semaphore :name "imgui-window-created-semaphore" :count 0))

   (window-update-complete-semaphore
    :accessor vktk::window-update-complete-semaphore
    :initform (bt:make-semaphore :name "window-update-complete-semaphore" :count 0))

   (render-command-queue :accessor vktk::render-command-queue
                         :initform (sb-concurrency:make-queue))

   (application-fonts :accessor vktk::application-fonts :initform nil)

   (cached-width :accessor cached-width :initform 0)
   (cached-height :accessor cached-height :initform 0)
   (cached-width-echo-area :accessor cached-width-echo-area :initform 0)))

(defclass text-editor (text-editor-mixin imgui-enabled-application)
  ((current-frame :accessor current-frame :initform 0)
   (exit? :accessor exit? :initform nil)))

(defmethod set-default-font ((app text-editor-mixin))
  (setf (default-font app) (call-next-method)))

(defclass glfw-editor-input (hi::editor-input)
  ())

(progn
  hi::(defmethod listen-editor-input ((stream vktk::glfw-editor-input))
        (hi::input-event-next (hi::editor-input-head stream)))
)


(defun debug-message (app)
  (when (error-message app)
    (multiple-value-bind (dx dy) (get-window-size (main-window app))
      (multiple-value-bind (x1 y1) (get-window-pos (main-window app))
        (let ((x x1)
              (y (+ y1 (round (/ dy 2)))))
          (ig:set-next-window-pos (+ x 450) y)
          (ig:set-next-window-size (- dx 450) (round (/ dy 2)))
          (ig:begin "###debug-msg"
                    :p-open t :open t
                    :no-title-bar t
                    :no-resize t
                    :no-move t
                    :no-scrollbar t)
          (unwind-protect
               (progn
                 (ig::with-text-color (#(0.0f0 0.0f0 0.0f0 1.0f0))
                   (ig:text "~A" (error-message app)))

                 (when (backtrace-string app)
                   (ig::with-text-color (#(0.0f0 0.0f0 0.0f0 1.0f0))
                     (ig:text "~A" (backtrace-string app))))

                 (when (button "dismiss")
                   (setf (error-message app) nil
                         (backtrace-string app) nil)))
            (ig:end)))))))

(defun menu-bar (app)
  (ig:igBeginMainMenuBar)
  (unwind-protect
       (progn
         (when (ig:begin-menu "File")
           (ig:menu-item "New")
           (ig:menu-item "Open")
           (when (ig:menu-item "Exit")
             (setf (exit? app) t))
           (ig:end-menu))

         (when (ig:begin-menu "Edit")
           (ig:menu-item "Create")
           (ig:menu-item "Move")
           (ig:menu-item "Delete")
           (ig:end-menu))

         (when (ig:begin-menu "View")
           #+NIL
           (when (ig:menu-item "Reset Camera"))
           (ig:end-menu)))

    (ig:igEndMainMenuBar)))

(progn
  hi::(defmethod %init-screen-manager ((backend (eql :vktk)) dummy)
        (declare (ignore dummy))
        (let ((display 0))
          (hi::site-init)
          (let ((device (make-default-bitmap-device display)))
            (vktk::vktk-create-initial-windows-hook device))))
  )

(defun hi::make-default-bitmap-device (display)
  (hemlock.x11::make-bitmap-device
   :name "Windowed Bitmap Device"
   :display display))

(defmethod hi::device-init ((device hemlock.x11::bitmap-device))
  t)

(progn
  hi::(defun vktk::vktk-create-initial-windows-hook (device)
        (bt:wait-on-semaphore (vktk::imgui-window-created-semaphore vk::*app*))
        (let ((imgui-window (ig::igfindwindowbyname "editor##editor")))
          (when (vktk::null-pointer-p imgui-window)
            (error "imgui window doesn't exist"))
          (when *current-buffer*
            (let ((main-win (make-window (buffer-start-mark *current-buffer*)
                                         :id "editor##editor"
                                         :device device
                                         :window imgui-window
                                         :x 0.0f0
                                         :y 0.0f0
                                         ;;:width (max 10 w)
                                         ;;:height (max 10 (- h 56))
                                         )))
              (%rt-update-modeline-fields *current-buffer* main-win)
              (multiple-value-bind (w h) (vk::get-window-size (vk::main-window vk::*app*))
                (declare (ignore h))
                (when *echo-area-buffer*
                  (setf *echo-area-window*
                        (make-window (buffer-start-mark *echo-area-buffer*)
                                     :id "echo-area##echo-area"
                                     :x 0.0f0
                                     :y 0.0f0
                                     :width (round (- w (* 2 hemlock.x11::hunk-left-border)))
                                     :device device :modelinep nil
                                     :proportion :fixed-height)))

              (setf *current-window* main-win)
              (setf vktk::*main-window* main-win)
              (setf (vktk::hemlock-windows-created-p vk::*app*) t))))))
)

(defun render-editor (app)
  (multiple-value-bind (x y) (get-window-pos (main-window app))
    (let ((initial-width 950)
          (initial-height 550))
      (with-window ((format nil "editor##editor")
                  :position (vec2 (+ x 500) (+ y 500))
                  :size (vec2 initial-width initial-height)
                  :condition :first-use-ever
                  :no-scrollbar t)
        (let ((active? (ig:igIsWindowFocused ig::ImGuiFocusedFlags_RootAndChildWindows)))
          ;; seems like this is making hemlock slow
          (when (hemlock-windows-created-p app)
            (mapcar #'(lambda (w)
                        (setf (hi::active? w) active?)
                        (hemlock.x11::dumb-window-redisplay w))
                  (remove-if #'(lambda (window)
                                 (eq window hi::*echo-area-window*))
                             hi::*window-list*))))
        (unless (imgui-window-created-semaphore-signalled-once app)
          (bt:signal-semaphore (imgui-window-created-semaphore app))
          (setf (imgui-window-created-semaphore-signalled-once app) t)))))
  (values))


(defclass vktk-hunk (hemlock.x11::bitmap-hunk) ())

(progn
  hemlock.x11::
  (defun hunk-write-line (hunk dl &optional (position nil))
    (let* ((font-family (bitmap-hunk-font-family hunk)))
      (if (typep dl 'hi::window-dis-line)
          (let ((chars (dis-line-chars dl))
                (length (dis-line-length dl)))
            (unless position
              (setq position (dis-line-position dl)))
            (let ((last 0)
                  (last-font 0))
              (do ((change (dis-line-font-changes dl) (font-change-next change)))
                  ((null change)
                   (hunk-put-string* hunk last position font-family last-font chars last length))
                (let ((x (font-change-x change)))
                  (hunk-put-string* hunk last position font-family last-font chars last x)
                  (setq last x
                        last-font (font-change-font change))))))
          (hunk-put-string* hunk 0 0 font-family 0 "BORKEM" 0 6))))
  )

(defun offset-coordinates-i (hemlock-window x y)
  (declare (ignore hemlock-window))
  (let ((imgui-padding-y 28.0f0))
    (values (round (- x (+ hemlock.x11::hunk-left-border)))
            (round (- y (+ imgui-padding-y hemlock.x11::hunk-top-border y))))))

(defun offset-coordinates-f (hemlock-window x y)
  (declare (ignore hemlock-window))
  (let ((imgui-padding-y 28.0f0))
    (values (clampf (- x (+ hemlock.x11::hunk-left-border)))
            (clampf (- y (+ imgui-padding-y hemlock.x11::hunk-top-border))))))

(progn
  hemlock.x11::
  (defmethod hunk-put-string* ((hunk vktk::vktk-hunk) x y font-family font-color string start end)
    (let* ((font (vktk::default-font vk::*app*))
           (font-size (slot-value font 'ig::size))
           (font-width 10.0f0)
           (window (hi::bitmap-hunk-window hunk))
           (imgui-padding-y 28.0f0)
           (ox (hi::window-x window))
           (oy (hi::window-y window))
           (xval (vk:clampf (+ hunk-left-border (* x font-width) ox)))
           (baseline -2.0f0)
           (yval (vk:clampf (+ imgui-padding-y hunk-top-border (* y font-size) baseline oy)))
           (color (vktk::find-color font-color))
           (red (/ (ash color -24) 255.0f0))
           (green (/ (ash (logand #x00ff0000 color) -16) 255.0f0))
           (blue (/ (ash (logand #x0000ff00 color) -8) 255.0f0))
           (alpha (/ (logand #x000000ff color) 255.0f0)))

      (if (eq window *echo-area-window*)
          (progn
          (ig::render-text string
                         :start start
                         :end end
                         :font font
                         :color (vector red green blue alpha)
                         :x (vk:clampf (+ (* x 10.0f0) (* 2 hunk-left-border)))
                         :y (vk:clampf (+ (* y 19.0f0) hunk-top-border -4))))
          (when (and ox oy)
            (ig::render-text string
                             :start start
                         :end end
                             :font font
                             :color (vector red green blue alpha)
                             :x xval
                             :y yval)))))
  )


(defconstant +default-fg-color+ 0)
(defconstant +comment-fg-color+ 1)
(defconstant +bkquote-fg-color+ 2)
(defconstant +unquote-fg-color+ 3)
(defconstant +strings-fg-color+ 4)
(defconstant +quote-fg-color+ 5)
(defconstant +sharp-plus-fg-color+ 6)
(defconstant +sharp-minus-fg-color+ 7)
(defconstant +def-fg-color+ 8)
(defconstant +name-fg-color+ 9)
(defconstant +keyword-fg-color+ 10)
(defconstant +integer-fg-color+ 11)


(defun find-color (font-color)
  (elt #(;;;fg
         #xffffffff
         #x019901ff     ;1 = comments

         #x00ffffff     ;2 = backquote
         #x009999ff     ;3 = unquote

         #xff9900ff     ;4 = strings
         #xffffffff     ;5 = quote
         #xff0000ff     ;6 = #+
         #x770000ff     ;7 = #-

         #x03ff03ff
         #xffff00ff
         #xff00ffff
         #xcc00ccff

         #x000000ff
         #x000000ff
         #x000000ff
         #x000000ff
         #x000000ff)
        (mod font-color 17)))

(progn
  hemlock.x11::
  (defmethod hemlock.x11::hunk-replace-modeline ((hunk vktk::vktk-hunk))
    (let* (
           (window (hi::bitmap-hunk-window hunk))
           (dl (window-modeline-dis-line window))
           (ox (hi::window-x window))
           (oy (hi::window-y window))
           (default-font (vktk::default-font vk::*app*))
           (modeline-pos (vk:clampf (bitmap-hunk-modeline-pos hunk)))
           (baseline -2.0f0))

      (when (and ox oy)
        (ig::igRenderFrame (+ ox hunk-left-border)
                           modeline-pos
                           (+ ox (bitmap-hunk-width hunk))
                           (+ modeline-pos hunk-modeline-top hunk-modeline-bottom
                              (slot-value default-font 'ig::size))
                           #xff777777
                           t
                           8.0f0)
        (ig::render-text (dis-line-chars dl)
                       :end (dis-line-length dl)
                       :x  (vk:clampf (+ ox hunk-left-border))
                       :y (vk:clampf (+ oy modeline-pos hunk-modeline-top baseline))
                       :font default-font
                       :color #(1.0f0 1.0f0 1.0f0 1.0f0)))))
  )

#+windows
(defcfun (ff-Beep "Beep") :bool
  (dwFreq :int)
  (dwDuration :int))

(defun bell ()
  (ff-Beep 650 40))

(defun hi::beep ()
  (bell))

(defmethod main-loop-one-iteration (app)

  (reroute-debug
    (menu-bar app))

  (reroute-debug
    (debug-message app))

  (reroute-debug
    (render-editor app))

  (reroute-debug
    (render-echo-area app))
  )

(defmethod render-echo-area (app)
  (when (hemlock-windows-created-p app)
    (multiple-value-bind (w h) (vk:get-window-size (main-window app))
      (multiple-value-bind (x y) (vk:get-window-pos (main-window app))
        (let ((echo-area-height (* 1 19.0 (hi::window-height hi::*echo-area-window*))))
          (setq y (+ y (- h echo-area-height)))
          (with-window ("echo-area##echo-area"
                        :position (vec2 x y)
                        :size (vec2 (+ w) echo-area-height)
                        :no-title-bar t
                        :no-resize t
                        :no-scrollbar t
                        )
            (let ((active? (ig:igIsWindowFocused ig::ImGuiFocusedFlags_RootAndChildWindows)))
              (setf (hi::active? hi::*echo-area-window*) active?)
              (hemlock.x11::dumb-window-redisplay hi::*echo-area-window*))))))))

(defmethod on-scroll ((app text-editor-mixin) xoffset yoffset)
  (flet ((apply-mods (string)
           (when (shift-key-down? app)
             (setq string (concatenate 'string "Shift\-" string)))
           (when (ctrl-key-down? app)
             (setq string (concatenate 'string "C\-" string)))
           (when (meta-key-down? app)
             (setq string (concatenate 'string "M\-" string)))
           string))
    (let ((event-string (format nil "~S" (apply-mods "Mousewheel"))))
      (with-input-from-string (stream event-string)
        (hi::q-event hi::*editor-input*
                     (eval (hi::parse-key-fun stream nil nil))
                     xoffset yoffset
                     (hemlock.x11::window-hunk vktk::*main-window*)))))
  (values))

(defmethod on-mouse-click ((app text-editor-mixin) buttons-pressed action mods)
  (flet ((q-event (key-event)
           (multiple-value-bind (x y) (get-window-pos (main-window app))
             (format t "~&A ~S ~S" x y)
             (let ((hemlock-window-pointer (ig::igfindwindowbyname "editor##editor")))
               (unless (null-pointer-p hemlock-window-pointer)
                 (multiple-value-bind (xw yw) (ig::get-window-pos hemlock-window-pointer)
                   (format t "~&B ~S ~S" xw yw)
                   (multiple-value-bind (xc yc) (get-cursor-pos (main-window app))
                     (format t "~&C ~S ~S" xc yc)
                     (let ((relative-cursor-position-x (- (+ x xc) xw hemlock.x11::hunk-left-border))
                           (relative-cursor-position-y (- (+ y yc) yw *imgui-padding-y*)))

                       (format t "~&D ~S ~S"
                               (round relative-cursor-position-x)
                               (round relative-cursor-position-y))

                       (let ((character-position-x (floor (/ relative-cursor-position-x 10.0f0)))
                             (character-position-y (floor (/ relative-cursor-position-y 19.0f0))))
                         (format t "~&E ~S ~S" character-position-x character-position-y)
                         (finish-output)
                         (unless (or (minusp character-position-x)
                                     (minusp character-position-y)
                                     (> character-position-x (hi::window-width vktk::*main-window*))
                                     (> character-position-y (hi::window-height vktk::*main-window*)))
                           (hi::q-event hi::*editor-input* key-event
                                        character-position-x
                                        character-position-y
                                        (hemlock.x11::window-hunk vktk::*main-window*)))))))))))
         (apply-mods (string)
           (when (logtest 1 mods)
             (setq string (concatenate 'string "Shift\-" string)))
           (when (logtest 2 mods)
             (setq string (concatenate 'string "C\-" string)))
           (when (logtest 4 mods)
             (setq string (concatenate 'string "M\-" string)))
           string))
    (let ((events ()))
      (when (elt buttons-pressed 0)
        (when (eq action %glfw:GLFW_PRESS)
          (push (apply-mods "Leftdown") events))
        (when (eq action %glfw:GLFW_RELEASE)
          (push (apply-mods "Leftup") events)))
      (when (elt buttons-pressed 1)
        (when (eq action %glfw:GLFW_PRESS)
          (push (apply-mods "Rightdown") events))
        (when (eq action %glfw:GLFW_RELEASE)
          (push (apply-mods "Rightup") events)))
      (when (elt buttons-pressed 2)
        (when (eq action %glfw:GLFW_PRESS)
          (push (apply-mods "Middledown") events))
        (when (eq action %glfw:GLFW_RELEASE)
          (push (apply-mods "Middleup") events)))
      (loop for event in events
            do (let ((event-string (format nil "~S" event)))
                 (with-input-from-string (stream event-string)
                   (q-event (eval (hi::parse-key-fun stream nil nil)))))))
    (values)))


hemlock.x11::(defun hunk-reset (hunk) hunk)


(defmethod on-key-event ((app text-editor-mixin) key action)
  (flet ((any? (function list)
           (mapcar #'(lambda (item)
                       (when (funcall function item)
                         (return-from any? t)))
                   list)
           nil))

    (reroute-debug
      (block on-key-event

        (unless (any? #'(lambda (w)
                          (hi::active? w))
                      hi::*window-list*)
          (return-from on-key-event nil))

        (when (or (eq key %glfw:GLFW_KEY_LEFT_SHIFT)
                  (eq key %glfw:GLFW_KEY_RIGHT_SHIFT))
          (when (eq action %glfw:GLFW_PRESS)
            (setf (shift-key-down? app) t))
          (when (eq action %glfw:GLFW_RELEASE)
            (setf (shift-key-down? app) nil)))

        (when (or (eq key %glfw:GLFW_KEY_LEFT_CONTROL)
                  (eq key %glfw:GLFW_KEY_RIGHT_CONTROL))
          (when (eq action %glfw:GLFW_PRESS)
            (setf (ctrl-key-down? app) t))
          (when (eq action %glfw:GLFW_RELEASE)
            (setf (ctrl-key-down? app) nil)))

        (when (or (eq key %glfw:GLFW_KEY_LEFT_ALT)
                  (eq key %glfw:GLFW_KEY_RIGHT_ALT))
          (when (eq action %glfw:GLFW_PRESS)
            (setf (meta-key-down? app) t))
          (when (eq action %glfw:GLFW_RELEASE)
            (setf (meta-key-down? app) nil)))

        (when (or (eq key %glfw:GLFW_KEY_LEFT_SUPER)
                  (eq key %glfw:GLFW_KEY_RIGHT_SUPER))
          (when (eq action %glfw:GLFW_PRESS)
            (setf (super-key-down? app) t))
          (when (eq action %glfw:GLFW_RELEASE)
            (setf (super-key-down? app) nil)))


        (unless (or (eq key %glfw:GLFW_KEY_LEFT_SHIFT)
                    (eq key %glfw:GLFW_KEY_RIGHT_SHIFT)
                    (eq key %glfw:GLFW_KEY_LEFT_CONTROL)
                    (eq key %glfw:GLFW_KEY_RIGHT_CONTROL)
                    (eq key %glfw:GLFW_KEY_LEFT_ALT)
                    (eq key %glfw:GLFW_KEY_RIGHT_ALT)
                    (eq key %glfw:GLFW_KEY_LEFT_SUPER)
                    (eq key %glfw:GLFW_KEY_RIGHT_SUPER))

          (when (or (eq action %glfw:GLFW_PRESS)
                    (eq action %glfw:GLFW_REPEAT))
            (multiple-value-bind (x y) (get-cursor-pos (main-window app))
              (hi::q-event hi::*editor-input*
                           (translate-glfw-key key (shift-key-down? app)
                                               (ctrl-key-down? app)
                                               (meta-key-down? app)
                                               nil)
                           (round x) (round y)))

            (return-from on-key-event t))))

      nil)))

(defmethod on-char-event ((app text-editor-mixin) char)
  (declare (ignore char))
  (values))

(defun handle-queued-hemlock-ui-events (app)
  (let ((window-pointer (ig::igfindwindowbyname "editor##editor")))
    (unless (null-pointer-p window-pointer)
      (when vktk::*main-window*
        (multiple-value-bind (w h) (ig::get-window-size window-pointer)
          (unless (and (= (cached-width app) w)
                       (= (cached-height app) h))
            (setf (cached-width app) w
                  (cached-height app) h)

            (let* ((main-hunk (hi::window-hunk vktk::*main-window*)))

              (hemlock.x11::set-hunk-size main-hunk (- w (* 0 hemlock.x11::hunk-left-border))
                                          h t)

              (hemlock.x11::window-changed main-hunk)
              (hi::%rt-update-window-image vktk::*main-window*)
              ))))

      (when hi::*echo-area-window*
        (multiple-value-bind (w h) (vk:get-window-size (main-window app))
          (declare (ignore h))
          (unless (= (cached-width-echo-area app) w)
            (setf (cached-width-echo-area app) w)

            (let* ((echo-hunk (hi::window-hunk hi::*echo-area-window*)))

              (hemlock.x11::set-hunk-size
               echo-hunk (- w 0.0f0) (round (* 19.0 (hi::window-height hi::*echo-area-window*)))
               nil)

              (hemlock.x11::window-changed echo-hunk))
            )))))

  (loop with command = nil with lastcmd = nil
        while (setq command (sb-concurrency:dequeue (render-command-queue app)))
          do (funcall command))

  )


(defmethod main ((app text-editor) &rest args)
  (declare (ignore args))

  (let*  ((device (default-logical-device app))
          (main-window (first (last (vktk::window-registry app))))
          (index (queue-family-index (render-surface main-window)))
          (queue (find-queue device index))
          (command-pool (find-command-pool device index))
          (command-buffer (elt (command-buffers command-pool) 0)))

    (device-wait-idle device)

    (reset-command-pool device command-pool)

    (begin-command-buffer command-buffer)

    (load-fonts)

    (imgui-vulkan-create-fonts-texture (imgui-module app) command-buffer)

    (end-command-buffer command-buffer)

    (queue-submit1 queue command-buffer)

    (device-wait-idle device)

    (imgui-vulkan-destroy-font-upload-objects (imgui-module app))

    (set-default-font app)

    (setq hi::*main-event-base*
          (bt:make-thread #'(lambda ()
                              (reroute-debug
                                socket-rpc::
                                (unless *rpc-server*
                                  (setq *rpc-server* (start-local-rpc-server
                                                      :port nil
                                                      :max-connections *max-connections*)))
                                (bt:make-thread
                                 #'(lambda ()
                                     socket-rpc::
                                     (accept-rpc-connection *rpc-server*))
                                 :name "accept incoming connection")
                                (hi::hemlock nil :backend-type :vktk :display 0)))
                          :name "Hemlock Main"))

    (glfwInit)

    (let* ((image-index))

      (loop while (zerop (glfwWindowShouldClose (h main-window)))
            until (exit? app)
            do
               (ig::igCaptureMouseFromApp t)
               (ig::igCaptureKeyboardFromApp t)
               (glfwPollEvents)
               (ig::igCaptureKeyboardFromApp nil)
               (ig::igCaptureMouseFromApp nil)

               (handle-queued-hemlock-ui-events app)

               (when (recreate-swapchain? main-window)
                 (device-wait-idle device) ;; hack
                 (multiple-value-bind (width height) (get-framebuffer-size main-window)
                   (vulkan-create-window main-window width height)
                   (setf (recreate-swapchain? main-window) nil)))

               (let* ((swapchain (swapchain main-window))
                      (command-buffer (frame-command-buffer (elt (frame-resources swapchain)
                                                                 (current-frame app)))))

                 (imgui-glfw-new-frame (imgui-module app))

                 (ig::igNewFrame)

                 (setq image-index
                       (frame-begin swapchain (render-pass swapchain) (current-frame app)
                                    (clear-value main-window) command-pool))

                 (imgui::show-demo-window)
                 (main-loop-one-iteration app)

                 (ig:igRender)

                 (imgui-vulkan-render-draw-data
                  (imgui-module app) (ig::igGetMainViewport) command-buffer (current-frame app))

                 (frame-end swapchain queue (current-frame app))

                 (when (not (zerop (logand (foreign-slot-value (ig::igGetIO) '(:struct ig::ImguiIO)
                                                               'ig::ConfigFlags)
                                           ig::ImGuiConfigFlags_ViewportsEnable)))
                   (ig::igUpdatePlatformWindows)
                   (ig::igRenderPlatformWindowsDefault))

                 (frame-present swapchain queue (current-frame app) image-index main-window)
                 (setf (current-frame app) (mod (1+ (current-frame app)) (number-of-images swapchain)))
                 (when (= (current-frame app) 2)
                   (bt:signal-semaphore (vktk::imgui-window-created-semaphore vk::*app*)))))

      (shutdown-application app))))

(in-package :vktk.hemlock)

#-scl
(defun delq (item list)
  (delete item list))

#-scl
(defun memq (item list)
  (member item list))

#-scl
(defun assq (item alist)
  (assoc item alist))

(defun concat (&rest args)
  (apply #'concatenate 'string args))

(in-package :hi)

;;;; PRINT-PRETTY-KEY and PRINT-PRETTY-KEY-EVENT.

;;; PRINT-PRETTY-KEY -- Public.
;;;
(defun print-pretty-key (key &optional (stream *standard-output*) long-names-p)
  "This prints key, a key-event or vector of key-events, to stream in a
   user-expected fashion.  Long-names-p indicates whether modifiers should
   print with their long or short name."
  (declare (type (or vector key-event) key) (type stream stream))
  (etypecase key
    (key-event (print-pretty-key-event key stream long-names-p))
    (vector
     (let ((length-1 (1- (length key))))
       (dotimes (i (length key))
         (let ((key-event (aref key i)))
           (print-pretty-key-event key-event stream long-names-p)
           (unless (= i length-1) (write-char #\space stream))))))))

;;; PRINT-PRETTY-KEY-EVENT -- Public.
;;;
;;; Note, this makes use of the ordering in the a-list
;;; *modifiers-to-internal-masks* by CDDR'ing down it by starting on a short
;;; name or a long name.
;;;
(defun print-pretty-key-event (key-event &optional (stream *standard-output*)
                                         long-names-p)
  "This prints key-event to stream.  Long-names-p indicates whether modifier
   names should appear using the long name or short name."
  (do ((map (if long-names-p
                (cdr *modifiers-to-internal-masks*)
                *modifiers-to-internal-masks*)
            (cddr map)))
      ((null map))
    (when (not (zerop (logand (cdar map) (key-event-bits key-event))))
      (write-string (caar map) stream)
      (write-char #\- stream)))
  (let* ((name (keysym-preferred-name (key-event-keysym key-event)))
         (spacep (position #\space (the simple-string name))))
    (when spacep (write-char #\< stream))
    (write-string name stream)
    (when spacep (write-char #\> stream))))

(defmacro with-editor
    ((&key (load-user-init t) backend-type display) &body body)
  `(call-with-editor (lambda () ,@body)
                     :load-user-init ,load-user-init
                     :backend-type ,backend-type
                     :display ,display))

;;; This function is the user-visible entry point to the Hemlock editor.
;;; It calls out to CALL-WITH-EDITOR and COMMAND-LOOP to do the hard work.
;;;
;;; In addition to those, it runs entry and exit hooks.
;;;
;;; This function may also be when already in the editor, or when in a slave,
;;; and merely processes the command line argument in that case, allowing ED
;;; to work when already in Hemlock.
;;;
(defun hemlock (x
                &key (load-user-init t)
                  backend-type
                  (display (sb-posix:getenv "DISPLAY")))
  "Invokes the editor, Hemlock.  If X is supplied and is a symbol, the
   definition of X is put into a buffer, and that buffer is selected.  If X is
   a pathname, the file specified by X is visited in a new buffer.  If X is not
   supplied or Nil, the editor is entered in the same state as when last
   exited.  When :init is supplied as t (the default), the file
   \"hemlock-init.lisp\", or \".hemlock-init.lisp\" is loaded from the home
   directory, but the Lisp command line switch -hinit can be used to specify a
   different name.  Any compiled version of the source is preferred when
   choosing the file to load.  If the argument is non-nil and not t, then it
   should be a pathname that will be merged with the home directory."
  (cond
    (*in-the-editor*
     (process-command-line-argument x))
    (*in-hemlock-slave-p*
     (hemlock.wire:remote-value (hemlock::ts-stream-wire *terminal-io*)
                                (process-command-line-argument x)))
    (t
     (with-editor (:load-user-init load-user-init
                   :backend-type backend-type
                   :display display)
       (process-command-line-argument x)
       (invoke-hook hemlock::entry-hook)
       (loop
            (command-loop))))))

(defun command-loop ()
  (let ((*standard-input* *illegal-read-stream*)
        (*query-io* *illegal-read-stream*))
    (loop
       (catch 'command-loop-catcher     ;fixme: isn't this redundant?
         (catch 'editor-top-level-catcher
           (handler-bind
               ((error #'(lambda (condition)
                           (lisp-error-error-handler condition :internal))))
             (invoke-hook hemlock::abort-hook)
             (%command-loop)))))))

(defvar *main-event-base* nil)

;;; This function does all the hard work for Hemlock initialization, in
;;; particular backend initialization.  It differs from the main function
;;; in that it lets the caller take control once initialization is done,
;;; instead of entering the command loop.
;;;
(defun call-with-editor
    (fun
     &key (load-user-init t) backend-type (display (sb-posix:getenv "DISPLAY")))
  (when *in-the-editor*
    (error "already in the editor"))
  (when (and (not backend-type) (not (validate-backend-type backend-type)))
    (error "Specified backend ~A not loaded" backend-type))
  ;; fixme: pass DISPLAY to WITH-EVENT-LOOP, so that Qt can pick it up
  ;; in case the user wants a DISPLAY != $DISPLAY
  (let* ((backend-type (or backend-type (choose-backend-type display)))
         (*default-backend* backend-type))
    (setf *connection-backend*
          (ecase backend-type
            (:qt :qt)
            (:vktk :vktk)
            ((:tty :clx :mini) :iolib)))
    (with-existing-event-loop
        (or *main-event-base*
            (setf *main-event-base*
                  (make-event-loop *connection-backend*)))
      (let* ((*in-the-editor* t)
             (display (unless *editor-has-been-entered*
                        (maybe-load-hemlock-init load-user-init)
                        ;; Device dependent initializaiton.
                        (init-raw-io backend-type display))))
        (catch 'editor-top-level-catcher
          (site-wrapper-macro
            (unless *editor-has-been-entered*
              ;; Make an initial window, and set up redisplay's internal
              ;; data structures.
              (%init-redisplay backend-type display)
              (setq *editor-has-been-entered* t)
              ;; Pick up user initializations to be done after initialization.
              (invoke-hook (reverse *after-editor-initializations-funs*)))
            (catch 'hemlock-exit
              (funcall fun))))))))

(defun process-command-line-argument (x)
  (catch 'editor-top-level-catcher
    (process-command-line-argument/2 x)))

(defun process-command-line-argument/2 (x)
  (typecase x
    (list
     (mapc #'process-command-line-argument/2 x))
    (symbol
     (let* ((name (nstring-capitalize
                   (concatenate 'simple-string "Edit " (string x))))
            (buffer (or (getstring name *buffer-names*)
                        (make-buffer name)))
            (*print-case* :downcase))
       (delete-region (buffer-region buffer))
       (with-output-to-mark
           (*standard-output* (buffer-point buffer))
         (eval `(grindef ,x))   ; hackish, I know...
         (terpri)
         (hemlock::change-to-buffer buffer)
         (buffer-start (buffer-point buffer) buffer))))
    ((or string pathname)
     (hemlock::find-file-command () x))
    (t
     (error
      "~S is not a symbol or pathname.  I can't edit it!" x))))

(defvar *in-hemlock-slave-p* nil)

(defun hemlock-ed-function (&optional x)
  (hemlock x)
  t)

#+sbcl
(pushnew 'hemlock-ed-function sb-ext:*ed-functions*)

#+ccl
(unless (eq ccl:*resident-editor-hook* 'hemlock-ed-function)
  (if ccl:*resident-editor-hook*
      (warn "*resident-editor-hook* already bound, not installing hemlock")
      (setf ccl:*resident-editor-hook* 'hemlock-ed-function)))

#+allegro
(unless (eq scm::*ed-hook* 'hemlock-ed-function)
  (if scm::*ed-hook*
      (warn "*ed-hook* already bound, not installing hemlock")
      (setf scm::*ed-hook* 'hemlock-ed-function)))

(defun maybe-load-hemlock-init (init)
  (when init
    (let ((names
           (if (typep init '(or string pathname))
               (list init)
               (let ((home (user-homedir-pathname)))
                 (list (merge-pathnames ".hemlock.lisp" home)
                       (merge-pathnames ".hemlock/hemlock.lisp" home)
                       ;; Also support one of the traditional pathnames for
                       ;; CMUCL compatibility:
                       (merge-pathnames ".hemlock-init.lisp" home))))))
      (dolist (name names)
        (when (probe-file name)
          (load name :verbose t)
          (return))))))

;;;; SAVE-ALL-BUFFERS.

;;; SAVE-ALL-BUFFERS -- Public.
;;;
(defun save-all-buffers (&optional (list-unmodified-buffers nil))
  "This prompts users with each modified buffer as to whether they want to
   write it out.  If the buffer has no associated file, this will also prompt
   for a file name.  Supplying the optional argument non-nil causes this
   to prompt for every buffer."
  (dolist (buffer *buffer-list*)
    (when (or list-unmodified-buffers (buffer-modified buffer))
      (maybe-save-buffer buffer))))

(defun maybe-save-buffer (buffer)
  (let* ((modified (buffer-modified buffer))
         (pathname (buffer-pathname buffer))
         (name (buffer-name buffer))
         (string (if pathname (namestring pathname))))
    (format t "Buffer ~S is ~:[UNmodified~;modified~], Save it? "
            name modified)
    (force-output)
    (when (y-or-n-p)
      (let ((name (read-line-default "File to write" string)))
        (format t "Writing file ~A..." name)
        (force-output)
        (write-file (buffer-region buffer) name)
        (write-line "write WON")))))

(defun read-line-default (prompt default)
  (format t "~A:~@[ [~A]~] " prompt default)
  (force-output)
  (do ((result (read-line) (read-line)))
      (())
    (declare (simple-string result))
    (when (plusp (length result)) (return result))
    (when default (return default))
    (format t "~A:~@[ [~A]~] " prompt default)
    (force-output)))

;; Hmm, what is this?
;;
;; This is highly bogus, but moving this to runtime fails also since
;; DEFMODE winds up wanting to refer to *current-buffer*. I don't know
;; why, and I don't know if we can repair it.
;; --GB

;;(unless *hemlock-initialized*
  ;;(%init-hemlock))

(defvar *installation-directory* nil)

(defun installation-directory ()
  (or *installation-directory*
      (asdf:component-pathname (asdf:find-system :hemlock.base))))



(defun schedule-window-operation (function)
  (when (boundp 'vktk::*app*)
    (sb-concurrency:enqueue
     function
     (vktk::render-command-queue vktk::*app*))
    nil))

(defmethod make-event-loop ((backend (eql :vktk)))
  nil)

(defmethod invoke-with-new-event-loop ((backend (eql :vktk)) fun)
  (funcall fun))

(defmethod invoke-with-existing-event-loop ((backend (eql :vktk)) loop fun)
  (let ((*event-base* loop))
    (funcall fun)))

(defmethod backend-init-raw-io ((backend (eql :vktk)) display)
  (declare (ignore display))
  (setf *editor-input* (make-instance 'vktk::glfw-editor-input))
  (setf *real-editor-input* *editor-input*))

(defmethod get-key-event ((stream vktk::glfw-editor-input) &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream vktk::glfw-editor-input))
  (un-event key-event stream))

(defun object-set-event-handler (display &optional (timeout 0))
  (declare (ignore display timeout))
  (values))

hemlock.x11::(defmethod device-after-redisplay ((device bitmap-device))
  (let ((display (bitmap-device-display device)))
    (loop (unless (hi::object-set-event-handler display) (return)))))





(progn
  hemlock::
  (defun rep-one-input (app input-string)
    (sb-concurrency:enqueue input-string (vktk::rep-in-queue app))
    (princ (sb-concurrency:dequeue (vktk::rep-out-queue app)))
    (fresh-line))

)


(defun vktk::dbg (num)
  (when nil
    (princ num sb-sys::*stdout*)
    (terpri sb-sys::*stdout*)
    (finish-output sb-sys::*stdout*)))

;;Error: There is no applicable method for the generic function
;;         #<standard-generic-function (common-lisp:setf vktk.hemlock.internals::line-chars) (1)>
;;       when called with arguments
;;         (-12469 nil).

;; window image mutation functions:
;; setup-window-image
;; maybe-update-window-image
;; update-window-image
;; maybe-recenter-window
;; center-window
