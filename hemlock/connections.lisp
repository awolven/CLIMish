;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)

(defvar *all-connections* nil)

(defvar *in-device-read?* nil)

(defun list-all-connections ()
  (copy-seq *all-connections*))

(defvar *foreground-connection* nil)
(defvar *background-connection* nil)

(defvar *event-base*)

(defmethod invoke-later ((backend (eql :vktk)) fun)
  (sb-ext:schedule-timer (sb-ext:make-timer fun) 0))

(defun dispatch-events ()
  (dispatch-events-with-backend *connection-backend*))

(defun dispatch-events-no-hang ()
  (prog1 (dispatch-events-no-hang-with-backend *connection-backend*)
    #+NIL(vktk::dbg "C")))


;;;;
;;;; CONNECTION
;;;;

(defparameter +input-buffer-size+ #x2000)

(defclass connection ()
  ((name :initarg :name
         :accessor connection-name)
   (buffer :initarg :buffer
           :initform nil
           :accessor connection-buffer)
   (stream :initarg :stream
           :initform nil
           :accessor connection-stream)
   (connection-sentinel :initarg :sentinel
                        :initform nil
                        :accessor connection-sentinel)))

(defmethod print-object ((instance connection) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "~A" (connection-name instance))))

(defun make-buffer-with-unique-name (name &rest keys)
  (or (apply #'make-buffer name keys)
      (iter:iter
       (iter:for i from 2)
       (let ((buffer (apply #'make-buffer
                            (format nil "~A<~D>" name i)
                            keys)))
         (when buffer
           (return buffer))))))

(defmethod initialize-instance :after
    ((instance connection) &key buffer)
  (flet ((delete-hook (buffer)
           (when (eq buffer (connection-buffer instance))
             (setf (connection-buffer instance) nil))))
    (typecase buffer
      ((eql t)
       (setf (connection-buffer instance)
             (make-buffer-with-unique-name
              ;; note the space in the buffer name
              (format nil " *Connection ~A*" (connection-name instance))
              :delete-hook (list #'delete-hook))))
      (hi::buffer
       (push #'delete-hook (buffer-delete-hook buffer)))
      (null)
      (t
       (error "expected NIL, T, or a buffer, but found ~A" buffer))))
  (setf (connection-name instance)
        (unique-connection-name (connection-name instance)))
  (push instance *all-connections*))

(defun unique-connection-name (base)
  (let ((name base))
    (iter:iter (iter:for i from 1)
               (iter:while (find name
                                 *all-connections*
                                 :test #'equal
                                 :key #'connection-name))
               (setf name (format nil "~A<~D>" base i)))
    name))

(defun delete-connection-buffer (connection)
  (when (connection-buffer connection)
    (delete-buffer (connection-buffer connection))
    (setf (connection-buffer connection) nil)))

(defmethod delete-connection ((connection connection))
  (delete-connection-buffer connection)
  (setf *all-connections* (remove connection *all-connections*)))

(defun connection-note-event (connection event)
  (let ((sentinel (connection-sentinel connection)))
    (when sentinel
      (funcall sentinel connection event))))


;;;;
;;;; IO-CONNECTION
;;;;

(defclass io-connection (connection)
  ((connection-filter :initarg :filter
                      :initform nil
                      :accessor connection-filter)
   (input-buffer :initform (make-array +input-buffer-size+
                                       :element-type '(unsigned-byte 8))
                 :accessor connection-input-buffer)
   (encoding :initform :utf-8
             :initarg :encoding
             :accessor connection-encoding)))

(defmethod initialize-instance :after
    ((instance io-connection) &key)
  (let ((enc (connection-encoding instance)))
    (when (symbolp enc)
      (setf (connection-encoding instance)
            (babel-encodings:get-character-encoding enc)))))

(defun filter-connection-output (connection data)
  (etypecase data
    (string
     (babel:string-to-octets data :encoding (connection-encoding connection)))
    ((array (unsigned-byte 8) (*))
     data)))

(defun note-connected (connection)
  (connection-note-event connection :connected))

(defun format-to-connection-buffer-or-stream (connection fmt &rest args)
  (let ((buffer (connection-buffer connection))
        (stream (connection-stream connection)))
    (when buffer
      (with-writable-buffer (buffer)
        (insert-string (buffer-point buffer)
                       (apply #'format nil fmt args))))
    (when stream
      (apply #'format stream fmt args))))

(defun insert-into-connection-buffer-or-stream (connection str)
  (let ((buffer (connection-buffer connection))
        (stream (connection-stream connection)))
    (when buffer
      (with-writable-buffer (buffer)
        (insert-string (buffer-point buffer) str)))

    (when stream
      (write-string str stream)
      (finish-output stream))))

(defun note-disconnected (connection)
  (connection-note-event connection :disconnected)
  (format-to-connection-buffer-or-stream connection
                                        "~&* Connection ~S disconnected."
                                        connection))

(defun note-error (connection)
  (connection-note-event connection :error)
  (format-to-connection-buffer-or-stream
   connection
   "~&* Error on connection ~S."
   connection))

(defun filter-incoming-data (connection bytes)
  (funcall (or (connection-filter connection) #'default-filter)
           connection
           bytes))

(defun default-filter (connection bytes)
  ;; fixme: what about multibyte characters that got split between two
  ;; input events data?
  (babel:octets-to-string bytes :encoding (connection-encoding connection)))


;;;;
;;;; PROCESS-CONNECTION-MIXIN
;;;;

(defclass process-connection-mixin ()
  ((command :initarg :command
            :accessor connection-command)
   (exit-code :initform nil
              :initarg :exit-code
              :accessor connection-exit-code)
   (exit-status :initform nil
                :initarg :exit-status
                :accessor connection-exit-status)
   (slave-pty-name :initform nil
                   :initarg :slave-pty-name
                   :accessor connection-slave-pty-name)
   (directory :initform nil
              :initarg :directory
              :accessor connection-directory)))

(defmethod class-for
    ((backend (eql :iolib)) (type (eql 'process-connection-mixin)))
  'process-connection/iolib)

(defmethod class-for
    ((backend (eql :qt)) (type (eql 'process-connection-mixin)))
  'process-connection/qt)

(defun make-process-connection
       (command
        &rest args
        &key name buffer stream filter sentinel slave-pty-name directory)
  (declare (ignore buffer stream filter sentinel slave-pty-name directory))
  (apply #'make-instance
         (class-for *connection-backend* 'process-connection-mixin)
         :name (or name (princ-to-string command))
         :command command
         args))

(defvar *request-queue*)

(defun maybe-serve-requests (connection)
  (declare (ignore connection))
  (let ((request nil))
    (loop while (setq request (sb-concurrency::dequeue *request-queue*))
          do (funcall request) ;; potentially calls hemlock.wire::device-read
          )))

(defmethod hemlock.wire:device-read ((device connection-device) buffer)
  (declare (ignore buffer))
  (let ((connection *foreground-connection*)
        (*in-device-read?* t))
    (maybe-serve-requests connection)
    (let ((previous-counter (device-filter-counter device)))
      (let ((retval 0))
        (loop while (eql previous-counter (device-filter-counter device))
              do
                 (when connection
                   (let ((ready-read? nil)
                         (num-bytes nil))
                     (tagbody
                      check
                        (multiple-value-setq (ready-read? num-bytes) (connection-listen-2 connection t))
                        (unless ready-read? (go check))
                      process
                        (incf retval (process-incoming-data connection num-bytes))))))
        0))))

(defmethod dispatch-events-with-backend ((backend (eql :vktk)))
  (let ((connection *foreground-connection*))
    (when connection
      (hemlock.wire::device-read (connection-device connection) nil))))

(defmethod dispatch-events-no-hang-with-backend ((backend (eql :vktk)))
  (let ((connection *foreground-connection*))
    (when connection
      (connection-read-no-hang connection))))

(defun connection-read-no-hang (connection)
  (let ((ready-read? nil)
        (num-bytes nil)
        (retval nil))
    (maybe-serve-requests connection)
    (multiple-value-setq (ready-read? num-bytes) (connection-listen-2 connection nil))
    (if ready-read?
        (progn (setq retval t)
               (process-incoming-data connection num-bytes)
               (vktk::dbg "K")
               retval)
        retval)))

;;;;
;;;; TCP-CONNECTION-MIXIN
;;;;

(defclass tcp-connection-mixin ()
  ((host :initarg :host
         :accessor connection-host)
   (port :initarg :port
         :accessor connection-port)))

(defmethod print-object ((instance tcp-connection-mixin) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "~A, connected to ~A:~D"
            (connection-name instance)
            (connection-host instance)
            (connection-port instance))))

(defmethod class-for
    ((backend (eql :iolib)) (type (eql 'tcp-connection-mixin)))
  'tcp-connection/iolib)

(defmethod class-for
    ((backend (eql :qt)) (type (eql 'tcp-connection-mixin)))
  'tcp-connection/qt)

(defmethod class-for
    ((backend (eql :vktk)) (type (eql 'tcp-connection-mixin)))
  'tcp-connection/vktk)

(defclass tcp-connection/vktk (tcp-connection-mixin io-connection)
  ((device :initarg :device
           :initform nil
           :accessor connection-device)
   (io-device :initarg :io-device
              :initform nil
              :accessor connection-io-device)
   (client :initarg :client
           :initform nil
           :accessor connection-client)))

(defmethod initialize-instance :after ((instance tcp-connection/vktk) &key (background nil))
  (unless (connection-client instance)
    (let ((client (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream
                                 :protocol :tcp)))
      (setf (connection-client instance) client)
      (connection-note-event instance :initialized)
      (sb-bsd-sockets:socket-connect
       (connection-client instance)
       (nslookup (machine-instance)#+NIL(connection-host instance))
       (connection-port instance))

      (if background
          (setf *background-connection* instance)
          (setf *foreground-connection* instance))
      (note-connected instance)))
  (setf (sb-bsd-sockets:non-blocking-mode (connection-client instance)) t)
  (values))

(defun connection-listen-2 (connection &optional (block? nil))
  (multiple-value-bind (err bytes)
      (sb-bsd-sockets-internal::ioctl
       (sb-bsd-sockets::socket-file-descriptor
                (connection-client connection))
               1074030207 ;; fionread
               (if block? 0 1))
    (when (not (zerop err))
      (error "ioctl returned code ~S" err))
    (values (not (zerop bytes))
            bytes)))

(defmethod %read ((connection tcp-connection/vktk) num-bytes)
  (if (and num-bytes (zerop num-bytes))
      :eof
      (let* ((buf-size 4096)
             (data (make-array (* 2 (or num-bytes 65536)) :element-type '(unsigned-byte 8)
                                               :adjustable t :fill-pointer 0)) ;; todo: make this a resource
             (buffer (make-array buf-size :element-type '(unsigned-byte 8)))
             (length))
        (declare (dynamic-extent buffer))
        (tagbody
         read
           ;;(vktk::dbg 12)
           (setq length (socket-receive (connection-client connection) buffer buf-size))
         check
           ;;(vktk::dbg 13)
           (when (zerop length)
             (go exit))
         load
           ;;(vktk::dbg 14)
           (loop for i from 0 below length
                 do (vector-push-extend (aref buffer i) data))
         check2
           ;;(vktk::dbg 15)
           (when (= length buf-size)
             (go read))

         exit
           #+NIL(vktk::dbg 16))

        data)))

(defun process-incoming-data (connection num-bytes)
  ;;(vktk::dbg 10)
  (let ((bytes (%read connection num-bytes)))
    (case bytes
      (:eof
       (note-disconnected connection)
       :eof)
      (t
       ;;(vktk::dbg 11)
       (let* ((characters (filter-incoming-data connection bytes))
              (buffer (connection-buffer connection))
              (stream (connection-stream connection)))
         (when (and characters (or buffer stream))
           (insert-into-connection-buffer-or-stream connection
                                                    characters)))))
    (length bytes)))

(defmethod connection-write (data (connection tcp-connection/vktk))
  (let ((bytes (filter-connection-output connection data)))
    (sb-bsd-sockets:socket-send (connection-client connection)
                                bytes nil)))

(defun make-tcp-connection
    (name host port &rest args &key buffer stream filter sentinel client background)
  (declare (ignore buffer stream filter sentinel client background))
  (apply #'make-instance
         (class-for *connection-backend* 'tcp-connection-mixin)
         :name name
         :host host
         :port port
         args))


;;;;
;;;; FILE-CONNECTION
;;;;

;;; not needed at the moment

;; (progn
;;   (defclass file-connection (qiodevice-connection)
;;     ((filename :initarg :filename
;;                :accessor connection-filename)))

;;   (defmethod initialize-instance :after ((instance file-connection) &key)
;;     (let ((socket (#_new QFile (connection-filename instance))))
;;       (setf (connection-io-device instance) socket)
;;       (connection-note-event instance :initialized)
;;       (#_open socket (#_QIODevice::ReadWrite))))

;;   #+(or)
;;   (defmethod (setf connection-io-device)
;;       :after
;;       (newval (connection file-connection))
;;     )

;;   (defun make-file-connection
;;       (filename &rest args &key name buffer stream filter sentinel)
;;     (declare (ignore buffer stream filter sentinel))
;;     (apply #'make-instance
;;            'file-connection
;;            :filename filename
;;            :name (or name filename)
;;            args)))


;;;;
;;;; PIPELIKE-CONNECTION
;;;;

#|
HRESULT WINAPI CreatePseudoConsole(
    _In_ COORD size,
    _In_ HANDLE hInput,
    _In_ HANDLE hOutput,
    _In_ DWORD dwFlags,
    _Out_ HPCON* phPC
                                   );
|#

#+WIN32
(cffi:defcfun ("GetLastError" GetLastError) :int)

#+WIN32
(defconstant PROC_THREAD_ATTRIBUTE_INPUT #x00020000)

#+WIN32
(defconstant PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE (logior 22 0 PROC_THREAD_ATTRIBUTE_INPUT 0))

#+WIN32
(defconstant S_OK 0)

#+WIN32
(defconstant EXTENDED_STARTUPINFO_PRESENT #x80000)

#+WIN32
(defconstant STATUS_PENDING #x103)

#+WIN32
(defconstant CREATE_NO_WINDOW #x08000000)

#+WIN32
(defconstant STARTF_USESTDHANDLES #x100)

#+WIN32
(cffi:defctype HRESULT :int)

#+WIN32
(cffi:defctype HANDLE :pointer)

#+WIN32
(cffi:defctype HPCON (:array HANDLE 3))

#+WIN32
(defclass win32-named-pipe ()
  ((pipe-read-handle :reader pipe-read-handle :initarg :read-handle)
   (pipe-write-handle :reader pipe-write-handle :initarg :write-handle)))

#+WIN32
(defclass win32-anonymous-pipe (win32-named-pipe)
  ())

#|
BOOL CreatePipe(
  [out]          PHANDLE               hReadPipe,
  [out]          PHANDLE               hWritePipe,
  [in, optional] LPSECURITY_ATTRIBUTES lpPipeAttributes,
  [in]           DWORD                 nSize
                );
|#

#+WIN32
(cffi:defcfun (CreatePipe "CreatePipe") :bool
  (phReadPipe :pointer)
  (phWritePipe :pointer)
  (lpPipeAttributes :pointer)
  (nSize :int))

#+WIN32
(defun create-pipe-0 (&key (size 0) (attributes nil))
  (cffi:with-foreign-objects ((&readPipe :pointer)
                              (&writePipe :pointer))
    (when (CreatePipe &readPipe &writePipe (if (null attributes) 0 attributes) size)
      (make-instance 'win32-anonymous-pipe
                     :read-handle (cffi:mem-aref &readPipe :pointer)
                     :write-handle (cffi:mem-aref &writePipe :pointer)))))

#+WIN32
(cffi:defcstruct COORD
  (X :short)
  (Y :short))

#+WIN32
(cffi:defcfun (CreatePseudoConsole "CreatePseudoConsole") HRESULT
  (size :unsigned-int)
  (hInput HANDLE)
  (hOutput HANDLE)
  (dwFlags :int)
  (phPC :pointer))

#+WIN32
(cffi:defcfun (ConptyCreatePseudoConsoleAsUser "ConptyCreatePseudoConsoleAsUser") HRESULT
  (hToken HANDLE)
  (size :unsigned-int)
  (hInput HANDLE)
  (hOutput HANDLE)
  (dwFlags :int)
  (phPC :pointer))

#+WIN32
(cffi:defcfun (InitializeProcThreadAttributeList "InitializeProcThreadAttributeList") :bool
  (lpAttributeList :pointer)
  (dwAttributeCount :int)
  (dwFlags :int)
  (lpSize :pointer))

#+WIN32
(cffi:defcfun (UpdateProcThreadAttribute "UpdateProcThreadAttribute") :bool
  (lpAttributeList :pointer)
  (dwFlags :int)
  (pAttribute :int64)
  (lpValue :pointer)
  (cbSize :int64)
  (lpPreviousValue :pointer)
  (lpReturnSize :pointer))

#+WIN32
(cffi:defcstruct STARTUPINFOA
  (cb :int)
  (lpReserved :string)
  (lpDesktop :string)
  (lpTitle :string)
  (dwX :int)
  (dwY :int)
  (dwXSize :int)
  (dwYSize :int)
  (dwXCountChars :int)
  (dwYCountChars :int)
  (dwFillAttribute :int)
  (dwFlags :int)
  (wShowWindow :short)
  (cbReserved2 :short)
  (lpReserved2 :pointer)
  (hStdInput HANDLE)
  (hStdOutput HANDLE)
  (hStdError HANDLE))

#+WIN32
(cffi:defcstruct STARTUPINFOEXA
  (StartupInfo (:struct STARTUPINFOA))
  (lpAttributeList :pointer))

#+WIN32
(cffi:defcstruct PROCESS_INFORMATION
  (hProcess HANDLE)
  (hThread HANDLE)
  (dwProcessId :int)
  (dwThreadId :int))

#+WIN32
(defclass win32-process ()
  ((thread-id :initarg :thread-id)
   (process-id :initarg :process-id :reader process-id)
   (thread-handle :initarg :thread-handle)
   (handle :initarg :h :reader h)
   (command-line :initarg :command-line)))

#+WIN32
(defclass win32-console-process (win32-process)
  ((stdout :accessor process-stdout :initarg :stdout)
   (stdin :accessor process-stdin :initarg :stdin)
   (input-queue :initform (sb-concurrency:make-queue) :reader process-input-queue)
   (writer-thread :accessor process-writer-thread)
   (output-queue :initform (sb-concurrency:make-queue) :reader process-output-queue)
   (reader-thread :accessor process-reader-thread)))

#+WIN32
(cffi:defcstruct SECURITY_ATTRIBUTES
  (nLength :int)
  (lpSecurityDescriptor :pointer)
  (bInheritHandle :bool))

#|
BOOL CreateProcessW(
  [in, optional]      LPCWSTR               lpApplicationName,
  [in, out, optional] LPWSTR                lpCommandLine,
  [in, optional]      LPSECURITY_ATTRIBUTES lpProcessAttributes,
  [in, optional]      LPSECURITY_ATTRIBUTES lpThreadAttributes,
  [in]                BOOL                  bInheritHandles,
  [in]                DWORD                 dwCreationFlags,
  [in, optional]      LPVOID                lpEnvironment,
  [in, optional]      LPCWSTR               lpCurrentDirectory,
  [in]                LPSTARTUPINFOW        lpStartupInfo,
  [out]               LPPROCESS_INFORMATION lpProcessInformation
);
|#

#+WIN32
(cffi:defcfun (CreateProcessA "CreateProcessA") :bool
  (lpApplicationName :string)
  (lpCommandLine :string)
  (lpProcessAttributes :pointer)
  (lpThreadAttributes :pointer)
  (bInheritHandles :bool)
  (dwCreationFlags :int)
  (lpEnvironment :pointer)
  (lpCurrentDirectory :string)
  (lpStartupInfo :pointer)
  (lpProcessInformation :pointer))

#+WIN32
(cffi:defcfun (GetExitCodeProcess "GetExitCodeProcess") :bool
  (hProcess HANDLE)
  (lpExitCode :pointer))

#+WIN32
(defmethod get-exit-code ((process win32-process))
  (cffi:with-foreign-objects ((&exit-code :uint))
    (when (GetExitCodeProcess (h process) &exit-code)
      (cffi:mem-aref &exit-code :uint))))

#+WIN32
(defmethod process-running? ((process win32-process))
  (eq (get-exit-code process) STATUS_PENDING))

(defun zero-struct (pointer size)
  (loop for i from 0 below size
        do (setf (cffi:mem-aref pointer :char i) 0))
  (values))

#+WIN32
(defun %initialize-startup-info-attached-to-conpty (p-si-ex hpc hStdInput hStdOutput)
  (block nil

    (let ((&si (cffi:foreign-slot-pointer p-si-ex '(:struct STARTUPINFOEXA) 'StartupInfo)))

      (setf (cffi:foreign-slot-value &si '(:struct STARTUPINFOA) 'cb)
            (load-time-value (cffi:foreign-type-size '(:struct STARTUPINFOEXA)))

            (cffi:foreign-slot-value &si '(:struct STARTUPINFOA) 'dwFlags)
            (logior STARTF_USESTDHANDLES #x01)

            (cffi:foreign-slot-value &si '(:struct STARTUPINFOA) 'hStdInput) hStdInput
            (cffi:foreign-slot-value &si '(:struct STARTUPINFOA) 'hStdOutput) hStdOutput
            (cffi:foreign-slot-value &si '(:struct STARTUPINFOA) 'hStdError) hStdOutput
         )
      )

    (cffi:with-foreign-objects ((&size :int64))

      (setf (cffi:mem-aref &size :int64) 0)

      (InitializeProcThreadAttributeList vk::+nullptr+ 1 0 &size)

      (let ((pAttrList (cffi:foreign-alloc :char :count (cffi:mem-aref &size :int64))))

        (zero-struct pAttrList (cffi:mem-aref &size :int64))

        (setf (cffi:foreign-slot-value p-si-ex '(:struct STARTUPINFOEXA) 'lpAttributeList)
              pAttrList)

        (if (InitializeProcThreadAttributeList
             (cffi:foreign-slot-value p-si-ex '(:struct STARTUPINFOEXA) 'lpAttributeList) 1 0 &size)

            ;;(cffi:with-foreign-objects ((&hPC 'HANDLE 3))

              ;;(setf (cffi:mem-aref &hPC 'HANDLE 0) hpc)

              (let ((success? (UpdateProcThreadAttribute
                               (cffi:foreign-slot-value p-si-ex '(:struct STARTUPINFOEXA) 'lpAttributeList)
                               0
                               PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE
                               hpc
                               (load-time-value (cffi:foreign-type-size 'HANDLE))
                               vk::+nullptr+
                               vk::+nullptr+)))

                (if success?
                    (return success?)
                    (error "UpdateProcThreadAttribute failed: ~S" (GetLastError))))

            (error "InitializeProcThreadAttributeList returned error code: ~S" (GetLastError)))))))

#+WIN32
(defconstant DETACHED_PROCESS #x8)

#+WIN32
(cffi:defcfun (SetHandleInformation "SetHandleInformation") :bool
  (hObject HANDLE)
  (dwMask :int)
  (dwFlags :int))

#+WIN32
(defconstant HANDLE_FLAG_INHERIT #x1)

#+WIN32
(defun create-pseudo-console (&key (command-line "C:\\Windows\\System32\\cmd.exe")
                                   (width 80) (height 32)
                                (current-directory vk::+nullptr+)
                                (application-name vk::+nullptr+))
  (let ((psec-attr (cffi:foreign-alloc '(:struct SECURITY_ATTRIBUTES))))

    (zero-struct psec-attr (load-time-value (cffi:foreign-type-size '(:struct SECURITY_ATTRIBUTES))))
    (setf (cffi:foreign-slot-value psec-attr '(:struct SECURITY_ATTRIBUTES) 'nLength)
          (cffi:foreign-type-size '(:struct SECURITY_ATTRIBUTES))
          (cffi:foreign-slot-value psec-attr '(:struct SECURITY_ATTRIBUTES) 'bInheritHandle)
          t)

  (let ((input-pipe (create-pipe-0 :attributes psec-attr)))
    (when (null input-pipe) (error "Could not create pipe for input."))

    (let ((output-pipe (create-pipe-0 :attributes psec-attr)))
      (when (null output-pipe) (error "Could not create pipe for output."))

      (with-slots ((in-pipe-pseudo-console-side pipe-read-handle)
                   (in-pipe-our-side pipe-write-handle)) input-pipe

        (SetHandleInformation in-pipe-our-side HANDLE_FLAG_INHERIT 0)

        (with-slots ((out-pipe-our-side pipe-read-handle)
                     (out-pipe-pseudo-console-side pipe-write-handle)) output-pipe

          (SetHandleInformation out-pipe-our-side HANDLE_FLAG_INHERIT 0)

          (cffi:with-foreign-objects (#+NO(&ConsoleSize '(:struct COORD)) (&hPC 'HPCON))

            (let ((console-size (logior width (ash height 16))))
              #+NIL
              (setf (cffi:foreign-slot-value &ConsoleSize '(:struct COORD) 'X) width
                    (cffi:foreign-slot-value &ConsoleSize '(:struct COORD) 'Y) height)

              (setf (cffi:mem-aref &hPC 'HANDLE 0) vk::+nullptr+)
              (setf (cffi:mem-aref &hPC 'HANDLE 1) vk::+nullptr+)
              (setf (cffi:mem-aref &hPC 'HANDLE 2) vk::+nullptr+)

              (let ((hr S_OK #+NOTYET(CreatePseudoConsole
                         console-size in-pipe-pseudo-console-side out-pipe-pseudo-console-side 0 &hPC)))

                (unless (eq hr S_OK)
                  (error "Could not create pseudo-console: ~S" (GetLastError)))

                ;;(print (cffi:mem-aref &hPC 'HANDLE 1))
                ;;(finish-output)

                (cffi:with-foreign-objects ((&siEx '(:struct STARTUPINFOEXA)))

                  (zero-struct &siEx (load-time-value (cffi:foreign-type-size '(:struct STARTUPINFOEXA))))

                  (%initialize-startup-info-attached-to-conpty &siEx (cffi:mem-aref &hPC 'HANDLE)
                                                               in-pipe-pseudo-console-side out-pipe-pseudo-console-side)

                  (cffi:with-foreign-objects ((&piClient '(:struct PROCESS_INFORMATION)))

                    (zero-struct &piClient (load-time-value (cffi:foreign-type-size '(:struct PROCESS_INFORMATION))))

                    (let ((pSec (cffi:foreign-alloc '(:struct SECURITY_ATTRIBUTES)))
                          (tSec (cffi:foreign-alloc '(:struct SECURITY_ATTRIBUTES))))

                      (zero-struct pSec (load-time-value (cffi:foreign-type-size '(:struct SECURITY_ATTRIBUTES))))
                      (zero-struct tSec (load-time-value (cffi:foreign-type-size '(:struct SECURITY_ATTRIBUTES))))

                      (setf (cffi:foreign-slot-value pSec '(:struct SECURITY_ATTRIBUTES) 'nLength) (cffi:foreign-type-size '(:struct SECURITY_ATTRIBUTES))
                            (cffi:foreign-slot-value tSec '(:struct SECURITY_ATTRIBUTES) 'nLength) (cffi:foreign-type-size '(:struct SECURITY_ATTRIBUTES)))

                      (if (CreateProcessA application-name
                                          command-line
                                          pSec
                                          tSec
                                          t ;; nil
                                          (logior #+NIL EXTENDED_STARTUPINFO_PRESENT CREATE_NO_WINDOW)
                                          vk::+nullptr+
                                          current-directory
                                          (cffi:foreign-slot-pointer &siEx '(:struct STARTUPINFOEXA) 'StartupInfo)
                                          &piClient)

                          (progn ;;(print (cffi:mem-aref &siEx '(:struct STARTUPINFOEXA)))
                                 (CloseHandle in-pipe-pseudo-console-side)
                                 (CloseHandle out-pipe-pseudo-console-side)
                                 (make-instance
                                  'win32-console-process
                                  :command-line command-line
                                  :thread-id (cffi:foreign-slot-value &piClient '(:struct PROCESS_INFORMATION) 'dwThreadId)
                                  :process-id (cffi:foreign-slot-value &piClient '(:struct PROCESS_INFORMATION) 'dwProcessId)
                                  :thread-handle (cffi:foreign-slot-value &piClient '(:struct PROCESS_INFORMATION) 'hThread)
                                  :h (cffi:foreign-slot-value &piClient '(:struct PROCESS_INFORMATION) 'hProcess)
                                  :stdin in-pipe-our-side
                                  :stdout out-pipe-our-side))

                          (progn
                            (print (cffi:mem-aref &siEx '(:struct STARTUPINFOEXA)))
                            (error "Could not create process ~S, error code: ~S" command-line
                                   (GetLastError))))))))))))))))

#+WIN32
(cffi:defcfun (TransactNamedPipe "TransactNamedPipe") :bool
  (hNamedPipe HANDLE)
  (lpInBuffer :pointer)
  (nInBufferSize :int)
  (lpOutBuffer :pointer)
  (nOutBufferSize :int)
  (lpBytesRead :pointer)
  (lpOverlapped :pointer))

#+WIN32
(defun transact-named-pipe (pipe-handle in-buffer &key (in-buffer-size (length in-buffer)))
  (let* ((out-buffer (make-array 2048 :element-type '(unsigned-byte 8)))
         (out-buffer-size (length out-buffer)))
    (sb-sys:with-pinned-objects (in-buffer out-buffer)
      (cffi:with-foreign-objects ((&bytes-read :int))
        (values (TransactNamedPipe pipe-handle ;; which pipe-handle??
                                   (sb-sys:vector-sap in-buffer)
                                   in-buffer-size
                                   (sb-sys:vector-sap out-buffer)
                                   out-buffer-size
                                   &bytes-read
                                   vk::+nullptr+)
                (cffi:mem-aref &bytes-read :int))))))

#+WIN32
(cffi:defcfun (ReadFile "ReadFile") :bool
  (hFile HANDLE)
  (lpBuffer :pointer)
  (nNumberOfBytesToRead :int)
  (lpNumberOfBytesRead :pointer)
  (lpOverlapped :pointer))

#+WIN32
(defun read-file-1 (handle buffer size)
  (cffi:with-foreign-objects ((&bytes-read :int))
    (values (ReadFile handle buffer size &bytes-read vk::+nullptr+)
            (cffi:mem-aref &bytes-read :int))))

(defun read-pipe (handle)
  (let ((buf-size 4096)
        (num-bytes-read nil)
        (retval nil)
        (octets (make-array 4096 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))

    (cffi:with-foreign-objects ((buf :unsigned-char buf-size))
      (tagbody
       read-1
         (multiple-value-setq (retval num-bytes-read) (read-file-1 handle buf buf-size))
       check
         (unless retval
           (go exit))
       check-2
         (when (<= num-bytes-read buf-size)
           (loop for i from 0 below num-bytes-read
                 do (let ((byte (cffi:mem-aref buf :unsigned-char i)))
                      (unless (= byte 13)
                        (vector-push-extend byte octets)))))
       check-3
         (when (= num-bytes-read buf-size)
           (go read-1))
       exit)
      (babel:octets-to-string octets))))

#+WIN32
(cffi:defcfun (WriteFile "WriteFile") :bool
  (hFile HANDLE)
  (lpBuffer :pointer)
  (nNumberOfBytesToWrite :int)
  (lpNumberOfBytesWritten :pointer)
  (lpOverlapped :pointer))

#+WIN32
(defun write-file-1 (handle buffer &optional (length (length buffer)))
  (sb-sys:with-pinned-objects (buffer)
    (cffi:with-foreign-objects ((&bytes-written :int))
      (values (WriteFile handle (sb-sys:vector-sap buffer) length &bytes-written vk::+nullptr+)
              (cffi:mem-aref &bytes-written :int)))))

(defun write-pipe (handle string &optional (start 0) (end (length string)))
  (let ((buffer (babel:string-to-octets string :start start :end end)))
    (write-file-1 handle buffer)))

(defun read-one-input-line (input-stream)
  (let ((input (make-array 100 :element-type 'character :adjustable t :fill-pointer 0))
        (char nil))
    (loop while (setq char (read-char input-stream nil nil))
          do (vector-push-extend char input)
          when (char= char #\Newline)
            do (return input))))

(defun start-pipe-maintenance-thread (process)
  (let ((stdin (process-stdin process))
        (stdout (process-stdout process)))
    (setf (process-reader-thread process)
          (bt:make-thread
           #'(lambda ()
               (loop
                 (sb-concurrency:enqueue (read-pipe stdout) (process-output-queue process))))
           :name (format nil "reader thread ~S" (process-id process))))
    (setf (process-writer-thread process)
          (bt:make-thread
           #'(lambda ()
               (loop
                 (let ((input (sb-concurrency:dequeue (process-input-queue process))))
                   (if input
                       (write-pipe stdin input)
                       (sleep 0.01)))))
           :name (format nil "writer thread ~S" (process-id process))))
    t))

(defun cmdline (&optional (io-stream *terminal-io*))
  (let ((process (create-pseudo-console)))
    (start-pipe-maintenance-thread process)
    (let ((output nil))
      (tagbody

       start
         (loop while (sb-concurrency:queue-empty-p (process-output-queue process))
               do (sleep 0.01))
         (loop
           (setq output (sb-concurrency:dequeue (process-output-queue process)))
           (if output
               (progn
                 (princ output io-stream)
                 (finish-output io-stream))
               (go read)))

       read
         (let ((input (read-one-input-line io-stream)))
           (sb-concurrency:enqueue input (process-input-queue process)))

         (loop while (sb-concurrency:queue-empty-p (process-output-queue process))
               do (sleep 0.01))

       write
         (loop
           (setq output (sb-concurrency:dequeue (process-output-queue process)))
           (when (string= output "dir
") (break))
           (if output
               (progn
                 (princ output io-stream)
                 (finish-output io-stream))
               (go read)))))))
(progn
hemlock::
(defcommand "CMD" (p)
    "Start a CMD.EXE shell"
    "Start a CMD.EXE shell"
  (declare (ignore p))
  (let ((buffer (make-buffer "CMD")))
    (change-to-buffer buffer)
    (setf (buffer-minor-mode buffer "Interactive-Character-Stream") t)
    (let ((stream (variable-value 'hemlock::buffer-stream :buffer buffer)))
      (bt:make-thread #'(lambda ()
                          (hi::cmdline stream)
                          :name "CMD")))))
)

#+WIN32
(cffi:defcfun (PeekNamedPipe "PeekNamedPipe") :bool
  (hNamedPipe HANDLE)
  (lpBuffer :pointer)
  (nBufferSize :int)
  (lpBytesRead :pointer)
  (lpTotalBytesAvail :pointer)
  (lpBytesLeftThisMessage :pointer))

#+WIN32
(defun peek-named-pipe (handle)
  (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
    (cffi:with-foreign-objects ((&bytes-read :int)
                                (&total-bytes-avail :int)
                                (&bytes-left-this-message :int))
      (sb-sys:with-pinned-objects (buffer)
        (values (PeekNamedPipe handle (sb-sys:vector-sap buffer) 2048
                               &bytes-read
                               &total-bytes-avail
                               &bytes-left-this-message)
                buffer
                (cffi:mem-aref &bytes-read :int)
                (cffi:mem-aref &total-bytes-avail :int)
                (cffi:mem-aref &bytes-left-this-message :int))))))

#+WIN32
(cffi:defcfun (CloseHandle "CloseHandle") :bool
  (hObject HANDLE))



    
   
#-(and scl linux)
(defun find-a-pty ()
  (block t
    (dolist (char '(#\p #\q) (error "no pty found"))
      (dotimes (digit 16)
        #+linux
        (handler-case
           (isys:open (format nil "/dev/pty~C~X" char digit)
                                      isys:o-rdwr)
          ((or isys:enoent
               isys:enxio
               isys:eio)
           ())
          (:no-error (master-fd)
            (let ((slave-name (format nil "/dev/tty~C~X" char digit)))
              (handler-case
                  (isys:open slave-name isys:o-rdwr)
                ((or isys:enoent
                     isys:enxio
                     isys:eio)
                 ()
                 (isys:close master-fd))
                (:no-error (slave-fd)
                  (return-from t
                    (values master-fd
                            slave-fd
                            slave-name)))))))))))

#+(and scl linux)
(defun find-a-pty ()
  (multiple-value-bind (master errno)
      (unix:unix-getpt)
    (unless master
      (error "~@<Getpt failed: ~A~@:>" (unix:get-unix-error-msg errno)))
    (multiple-value-bind (winp errno)
        (unix:unix-grantpt master)
      (unless winp
        (unix:unix-close master)
        (error "~@<Grantpt failed: ~A~@:>" (unix:get-unix-error-msg errno))))
    (multiple-value-bind (winp errno)
        (unix:unix-unlockpt master)
      (unless winp
        (unix:unix-close master)
        (error "~@<unlockpt failed: ~A~@:>" (unix:get-unix-error-msg errno))))
    (multiple-value-bind (name errno)
        (unix:unix-ptsname master)
      (unless name
        (unix:unix-close master)
        (error "~@<ptsname failed: ~A~@:>" (unix:get-unix-error-msg errno)))
      (multiple-value-bind (slave errno)
          (unix:unix-open name unix:o_rdwr 0)
        (unless slave
          (unix:unix-close master)
          (error "~@<Error opening slave pty: ~A~@:>" (unix:get-unix-error-msg errno)))
        (values master slave name)))))

(defun make-process-with-pty-connection
    (command &key name (buffer nil bufferp) stream)
  (multiple-value-bind (master slave slave-name)
      (find-a-pty)
    (declare (ignorable slave))
    (let ((pc (make-process-connection command :slave-pty-name slave-name)))
      ()#+NIL(isys:close slave)
      (make-pipelike-connection master
                                master
                                :name (or name (princ-to-string command))
                                :process-connection pc
                                :buffer (if bufferp
                                            buffer
                                            (null stream))
                                :stream stream))))

(defclass pipelike-connection-mixin ()
  ())

(defclass process-with-pty-connection-mixin (pipelike-connection-mixin)
  ((process-connection :initarg :process-connection
                       :accessor connection-process-connection)))

(macrolet ((defproxy (name)
             `(defmethod ,name ((connection process-with-pty-connection-mixin))
                (,name (connection-process-connection connection)))))
  (defproxy connection-command)
  (defproxy connection-exit-code)
  (defproxy connection-exit-status))

(defmethod delete-connection
    :before
    ((connection process-with-pty-connection-mixin))
  (delete-connection (connection-process-connection connection)))

(defmethod class-for
    ((backend (eql :iolib)) (type (eql 'process-with-pty-connection-mixin)))
  'process-with-pty-connection/iolib)

(defmethod class-for
    ((backend (eql :qt)) (type (eql 'process-with-pty-connection-mixin)))
  'process-with-pty-connection/qt)

(defmethod class-for
    ((backend (eql :iolib)) (type (eql 'pipelike-connection-mixin)))
  'pipelike-connection/iolib)

(defmethod class-for
    ((backend (eql :qt)) (type (eql 'pipelike-connection-mixin)))
  'pipelike-connection/qt)

(defun make-pipelike-connection
    (read-fd
     write-fd
     &rest args
     &key name buffer stream filter sentinel process-connection encoding)
  (declare (ignore buffer stream sentinel))
  (apply #'make-instance
         (class-for *connection-backend*
                    (if process-connection
                        'process-with-pty-connection-mixin
                        'pipelike-connection-mixin))
         :read-fd read-fd
         :write-fd write-fd
         :name (or name (format nil "descriptor ~D/~D" read-fd write-fd))
         :filter (or filter
                     (lambda (connection bytes)
                       (default-filter connection bytes)))
         :encoding (or encoding :utf-8)
         args))

(defgeneric stream-fd (stream))
(defmethod stream-fd (stream) stream)

#+sbcl
(defmethod stream-fd ((stream sb-sys:fd-stream))
  (sb-sys:fd-stream-fd stream))

#+cmu
(defmethod stream-fd ((stream system:fd-stream))
  (system:fd-stream-fd stream))

#+scl
(defmethod stream-fd ((stream stream))
  (system:fd-stream-fd stream))

#+openmcl
(defmethod stream-fd ((stream ccl::basic-stream))
  (ccl::ioblock-device (ccl::stream-ioblock stream t)))
#+openmcl
(defmethod stream-fd ((stream ccl::fd-stream))
  (ccl::ioblock-device (ccl::stream-ioblock stream t)))

#+clisp
(defmethod stream-fd ((stream stream))
  ;; sockets appear to be direct instances of STREAM
  (ignore-errors (socket:stream-handles stream)))

#+allegro
(defmethod stream-fd ((stream stream))
  (slot-value stream 'excl::input-handle))

(defmethod stream-fd ((stream integer))
  stream)


;;;;
;;;; LISTENING-CONNECTION
;;;;

(defclass listening-connection (connection)
  ((server :initarg :server
           :initform nil
           :accessor connection-server)
   (acceptor :initarg :acceptor
             :initform nil
             :accessor connection-acceptor)
   (initargs :initarg :initargs
             :initform nil
             :accessor connection-initargs)))

(defgeneric convert-pending-connection (listener &optional background))

(defun process-incoming-connection (listener &optional background)
  (let ((connection (convert-pending-connection listener background)))
    (format-to-connection-buffer-or-stream
     connection "~&* ~A." connection)
    (when (connection-acceptor listener)
      (funcall (connection-acceptor listener) connection))))




;;;;
;;;; TCP-LISTENER-MIXIN
;;;;

(defclass tcp-listener-mixin ()
  ((host :initarg :host
         :accessor connection-host)
   (port :initarg :port
         :accessor connection-port)))

(defmethod initialize-instance :after ((instance tcp-listener-mixin) &key)
  (check-type (connection-host instance) string)
  (check-type (connection-port instance)
              (or null (unsigned-byte 16))))

(defmethod print-object ((instance tcp-listener-mixin) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "~A ~A:~D"
            (connection-name instance)
            (connection-host instance)
            (connection-port instance))))

(defun make-tcp-listener
    (name host port &rest args &key buffer stream acceptor sentinel initargs)
  (declare (ignore buffer stream acceptor sentinel initargs))
  (apply #'make-instance
                 (class-for *connection-backend* 'tcp-listener-mixin)
                 :name name
                 :host host
                 :port port
                 args))

(defmethod class-for
    ((backend (eql :iolib)) (type (eql 'tcp-listener-mixin)))
  'tcp-listener/iolib)

(defmethod class-for
    ((backend (eql :qt)) (type (eql 'tcp-listener-mixin)))
  'tcp-listener/qt)

(defclass tcp-listener/vktk (tcp-listener-mixin listening-connection)
  ((buffer :initarg :buffer
           :accessor connection-buffer)
   (name :initarg :name
         :accessor connection-name)))

(defmethod convert-pending-connection ((server tcp-listener/vktk) &optional (background nil))
  (let* ((socket (connection-server server))
         (new-socket (sb-bsd-sockets:socket-accept socket)))
    (multiple-value-bind (host port) (sb-bsd-sockets:socket-name new-socket)
      (declare (ignore host))
      (let ((c
              (apply #'make-tcp-connection
                     (format nil "Accepted for: ~A" (connection-name server))
                     (machine-instance)
                     port
                     :client new-socket
                     :background background
                     (connection-initargs server))))
        (if background
            (setf *background-connection* c)
            (setf *foreground-connection* c))))))

(defmethod class-for ((backend (eql :vktk)) (type (eql 'tcp-listener-mixin)))
  'tcp-listener/vktk)

(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
four element array, suitable for socket-connect. If HOSTNAME is
not found, a host-not-found-error condition is thrown."
  (if hostname
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
      nil))

(defun ip-addr-vector-to-string (vector)
  (format nil "~A.~A.~A.~A" (elt vector 0) (elt vector 1) (elt vector 2) (elt vector 3)))

(defmethod initialize-instance :after ((instance tcp-listener/vktk) &key)
  (let* ((hostname (connection-host instance))
         (server (handler-case (let ((server (make-instance 'sb-bsd-sockets:inet-socket
                                                            :type :stream
                                                            :protocol :tcp)))
                                 (sb-bsd-sockets:socket-bind server (nslookup hostname) 0)
                                 (sb-bsd-sockets:socket-listen server 100)
                                 server)
                   (error () nil))))
    (multiple-value-bind (address port) (sb-bsd-sockets:socket-name server)
      (setf (connection-port instance) port
            (connection-host instance) (ip-addr-vector-to-string address)
            (connection-server instance) server))))

;;; wire interaction

(defstruct (connection-device
             (:include hemlock.wire:device)
           (:conc-name device-)
           (:constructor %make-connection-device (connection)))
  (connection (error "missing argument") :type connection)
  (reading 0 :type integer)
  (filter-counter 0 :type integer)
  (original-sentinel nil))

(defmethod print-object ((object connection-device) stream)
  (print-unreadable-object (object stream)
    (format stream "~A" (device-connection object))))

(defun make-connection-device (connection)
  (let ((device (%make-connection-device connection)))
    (setf (connection-device connection) device)
    (setf (connection-filter connection)
          (lambda (connection bytes)
            (connection-device-filter device connection bytes)))
    (setf (device-original-sentinel device)
          (connection-sentinel connection))
    (setf (connection-sentinel connection)
          (lambda (connection event)
            (connection-device-sentinel device connection event)))
    device))

#+NIL
(progn
cl-user::(defun foo () (setq sl (third (bt:all-threads)) hl (second (bt:all-threads)))
           (defun num-resp (thread) (sb-thread::symbol-value-in-thread 'hemlock.wire::*num-responded* thread))
           (defun num-sent (thread) (sb-thread::symbol-value-in-thread 'hemlock.wire::*num-sent* thread))
           (defun num-sent-hl () hemlock.wire::*num-sent*)
           (defun num-resp-hl () hemlock.wire::*num-responded*)
           (defun num-sent-sl () (num-sent sl))
  (defun num-resp-sl () (num-resp sl)))
)


(defvar *request-queue* (sb-concurrency:make-queue))

(defvar cl-user::hl nil)
(defvar cl-user::sl nil)

(defun connection-device-filter (device connection bytes)
  (declare (ignore connection))
  (setf bytes (copy-seq bytes))
  (sb-concurrency::enqueue
   #'(lambda ()
       (incf (device-filter-counter device))
       (hemlock.wire:device-append-to-input-buffer device bytes)
       (unless *in-device-read?*
         (hemlock.wire:device-serve-requests device t)))
   *request-queue*)
  nil)

(defun connection-device-sentinel (device connection event)
  (when (device-original-sentinel device)
    (funcall (device-original-sentinel device) connection event))
  ;; fixme: anything else to do here?
  )

(defmethod hemlock.wire:device-listen
    ((device connection-device))
  (connection-listen-2 (device-connection device)))

(defmethod hemlock.wire:device-write
    ((device connection-device) buffer &optional (end (length buffer)))
  (connection-write (subseq buffer 0 end) (device-connection device)))

#+WIN32
(cffi:defctype SOCKET :uint64)

(cffi:defcfun ("recv" recv) :int
  (socket SOCKET)
  (buf :pointer)
  (len :int)
  (flags :int))

(cffi:defcfun ("WSAGetLastError" WSAGetLastError) :int)

;; winsock error codes returned by WSAGetLastError:
(defconstant WSA_INVALID_HANDLE 6)
(defconstant WSA_NOT_ENOUGH_MEMORY 8)
(defconstant WSA_INVALID_PARAMETER 87)
(defconstant WSA_OPERATION_ABORTED 995)
(defconstant WSA_IO_INCOMPLETE 996)
(defconstant WSA_IO_PENDING 997)
(defconstant WSAEINT 10004)
(defconstant WSAEBADF 10009)
(defconstant WSAEACCES 10013)
(defconstant WSAEFAULT 10014)
(defconstant WSAEINVAL 10022)
(defconstant WSAEMFILE 10024)
(defconstant WSAEWOULDBLOCK 10035)
(defconstant WSAEINPROGRESS 10036)
(defconstant WSAEALREADY 10037)
(defconstant WSAENOTSOCK 10038)
(defconstant WSAEDESTADDRREQ 10039)
(defconstant WSAEMSGSIZE 10040)
(defconstant WSAEPROTOTYPE 10041)
(defconstant WSAENOPROTOOPT 10042)
(defconstant WSAEPROTONOSUPPORT 10044)
(defconstant WSAEOPNOTSUPPORT 10045)
(defconstant WSAEPFNOSUPPORT 10046)
(defconstant WSAEAFNOSUPPORT 10047)
(defconstant WSAEADDRINUSE 10048)
(defconstant WSAEADDRNOTAVAIL 10049)
(defconstant WSAENETDOWN 10050)
(defconstant WSAENETUNREACH 10051)
(defconstant WSAENETRESET 10052)
(defconstant WSACONNABORTED 10053)
(defconstant WSAECONNRESET 10054)
(defconstant WSANOBUFS 10055)
(defconstant WSAEISCONN 10056)
(defconstant WSAENOTCONN 10057)
(defconstant WSAESHUTDOWN 10058)
(defconstant WSAETOOMANYREFS 10059)
(defconstant WSAETIMEDOUT 10060)
(defconstant WSAECONNREFUSED 10061)
(defconstant WSAELOOP 10062)
(defconstant WSAENAMETOOLONG 10063)
(defconstant WSAEHOSTDOWN 10064)
(defconstant WSAEHOSTUNREACH 10065)
(defconstant WSAENOTEMPTY 10066)
(defconstant WSAEPROCLIM 10067)
(defconstant WSAEUSERS 10068)
(defconstant WSAEDQUOT 10069)
(defconstant WSAESTALE 10070)
(defconstant WSAEREMOTE 10071)
(defconstant WSASYSNOTREADY 10091)
(defconstant WSAVERNOTSUPPORTED 10092)
(defconstant WSANOTINITIALIZED 10093)
(defconstant WSAEDISCON 10101)
(defconstant WSAENOMORE 10102)
(defconstant WSAECANCELLED 10103)
(defconstant WSAEINVALIDPROCTABLE 10104)
(defconstant WSAEINVALIDPROVIDER 10105)
(defconstant WSAEPROVIDERFAILEDINIT 10106)
(defconstant WSASYSCALLFAILURE 10107)
(defconstant WSASERVICE_NOT_FOUND 10108)
(defconstant WSATYPE_NOT_FOUND 10109)
(defconstant WSA_E_NO_MORE 10110)
(defconstant WSA_E_CANCELLED 10111)
(defconstant WSAEREFUSED 10112)
(defconstant WSAHOST_NOT_FOUND 11001)
(defconstant WSATRY_AGAIN 11002)
(defconstant WSANO_RECOVERY 11003)
(defconstant WSANO_DATA 11004)
(defconstant WSA_QOS_RECEIVERS 11005)
(defconstant WSA_QOS_SENDERS 11006)
(defconstant WSA_QOS_NO_SENDERS 11007)
(defconstant WSA_QOS_NO_RECEIVERS 11008)
(defconstant WSA_QOS_REQUEST_CONFIRMED 11009)
(defconstant WSA_QOS_ADMISSION_FAILURE 11010)
(defconstant WAS_QOS_POLICY_FAILURE 11011)
(defconstant WSA_QOS_BAD_STYLE 11012)
(defconstant WSA_QOS_BAD_OBJECT 11013)
(defconstant WSA_QOS_TRAFFIC_CTRL_ERROR 11014)
(defconstant WSA_QOS_GENERIC_ERROR 11015)
(defconstant WSA_QOS_ESERVICETYPE 11016)
(defconstant WSA_QOS_EFLOWSPEC 11017)
(defconstant WSA_QOS_EPROVSPECBUF 10018)
(defconstant WSA_QOS_EFILTERSTYLE 11019)
(defconstant WSA_QOS_EFILTERTYPE 11020)
(defconstant WSA_QOS_EFILTERCOUNT 10021)
(defconstant WSA_QOS_EOBJLENGTH 11022)
(defconstant WSA_QOS_EFLOWCOUNT 11023)
(defconstant WSA_QOS_EUNKNOWNPSOBJ 11024)
(defconstant WSA_QOS_EPOLICYOBJ 10025)
(defconstant WSA_QOS_EFLOWDESC 11026)
(defconstant WSA_QOS_EPSFLOWSPEC 11027)
(defconstant WSA_QOS_EPSFILTERSPEC 11028)
(defconstant WSA_QOS_ESDMODEOBJ 11029)
(defconstant WSA_QOS_ESHAPERATEOBJ 11030)
(defconstant WSA_QOS_RESERVED_PETYPE 11031)

(defconstant INVALID_SOCKET (lognot 0))


(defun socket-receive (socket buffer length &optional (flags 0))
  (let ((s (sb-bsd-sockets::socket-file-descriptor socket)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((result (recv s (sb-sys:vector-sap buffer) length flags)))
        (cond ((plusp result) result)
              ((zerop result) 0)
              (t (error "recv got error code: ~A" (WSAGetLastError))))))))
