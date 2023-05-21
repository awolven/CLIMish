(in-package :hi)

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
   (input-queue :initform #+sbcl (sb-concurrency:make-queue) #-sbcl nil :reader process-input-queue)
   (writer-thread :accessor process-writer-thread)
   (output-queue :initform #+sbcl (sb-concurrency:make-queue) #-sbcl nil :reader process-output-queue)
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
            (cffi:foreign-slot-value &si '(:struct STARTUPINFOA) 'hStdError) hStdOutput))

    (cffi:with-foreign-objects ((&size :int64))

      (setf (cffi:mem-aref &size :int64) 0)

      (InitializeProcThreadAttributeList (cffi:null-pointer) 1 0 &size)

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
                               (cffi:null-pointer)
                               (cffi:null-pointer))))

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
                                (current-directory (cffi:null-pointer))
                                (application-name (cffi:null-pointer)))
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

              (setf (cffi:mem-aref &hPC 'HANDLE 0) (cffi:null-pointer))
              (setf (cffi:mem-aref &hPC 'HANDLE 1) (cffi:null-pointer))
              (setf (cffi:mem-aref &hPC 'HANDLE 2) (cffi:null-pointer))

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
                                          (cffi:null-pointer)
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
    #+SBCL
    (sb-sys:with-pinned-objects (in-buffer out-buffer)
      (cffi:with-foreign-objects ((&bytes-read :int))
        (values (TransactNamedPipe pipe-handle ;; which pipe-handle??
                                   (sb-sys:vector-sap in-buffer)
                                   in-buffer-size
                                   (sb-sys:vector-sap out-buffer)
                                   out-buffer-size
                                   &bytes-read
                                   (cffi:null-pointer))
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
    (values (ReadFile handle buffer size &bytes-read (cffi:null-pointer))
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
                      (unless (= byte 13) ;; strip CR
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
  #+SBCl
  (sb-sys:with-pinned-objects (buffer)
    (cffi:with-foreign-objects ((&bytes-written :int))
      (values (WriteFile handle (sb-sys:vector-sap buffer) length &bytes-written (cffi:null-pointer))
              (cffi:mem-aref &bytes-written :int)))))

(defun write-pipe (handle string &optional (start 0) (end (length string)))
  (let ((buffer (babel:string-to-octets string :start start :end end)))
    (write-file-1 handle buffer)))

(defmethod read-one-input-line ((stream hemlock::hemlock-interactive-character-io-stream)
                                &key (block? t))
  (with-slots (hemlock::input-line hemlock::input-line-position) stream
    (if block?
        (loop when (minusp hemlock::input-line-position)
                do (sleep 0.00001)
              finally (return (prog1 (copy-seq hemlock::input-line)
                                (setf hemlock::input-line "")
                                (setf  hemlock::input-line-position -1))))
        (unless (minusp hemlock::input-line-position)
          (prog1 (copy-seq hemlock::input-line)
            (setf hemlock::input-line "")
            (setf  hemlock::input-line-position -1))))))

(defun start-pipe-maintenance-thread (process)
  (let ((stdin (process-stdin process))
        (stdout (process-stdout process)))
    (setf (process-reader-thread process)
          (bt:make-thread
           #'(lambda ()
	       #+SBCl
               (loop
                 (sb-concurrency:enqueue (read-pipe stdout) (process-output-queue process))))
           :name (format nil "CMD reader thread ~S" (process-id process))))
    t))

(defun cmdline (&optional (io-stream *terminal-io*))
  (let ((process (create-pseudo-console)))
    (start-pipe-maintenance-thread process)
    (let ((candidate-input nil)
          (output nil)
          (input nil))
      (tagbody

       maybe-output
         ;; by using the output-queue and maintenance thread, pipe-reads don't block
	 #+sbcl
         (setq output (sb-concurrency:dequeue (process-output-queue process)))

         (when (and output input)
           (when (string= input output)
             ;; don't echo output, continue outputting
             (setq output nil input nil)
             (go maybe-output)))

         #+NIL
         (progn
           (princ "--------------------" sb-sys::*stdout*)
           (print candidate-input sb-sys::*stdout*)
           (finish-output sb-sys::*stdout*)

           (print output sb-sys::*stdout*)
           (finish-output sb-sys::*stdout*)
           (princ "--------------------" sb-sys::*stdout*))

         (when output
           (princ output io-stream)
           (finish-output io-stream)
           ;; once we have actual output, input is no longer relevant.
           ;; make sure it doesn't create false positives.
           (setq input nil))

       maybe-input
         ;; could be input could be output.
         (setq candidate-input (read-one-input-line io-stream :block? nil))
         (unless candidate-input
           (sleep 0.00001)
           (go maybe-output)) ;; keep outputting shit until input shows up

         (setq input candidate-input)

         (write-pipe (process-stdin process) input)

         (go maybe-output)))))


(progn

(hemlock::defcommand "CMD" (window p)
    "Start a CMD.EXE shell"
    "Start a CMD.EXE shell"
  (declare (ignore p))
  (let ((buffer (make-buffer "CMD"))
	(current-buffer (window-buffer window)))
    (if (eq window *echo-area-window*)
	(change-to-buffer buffer (setq window (krma.gui::find-or-create-text-editor-window-for-buffer buffer)))
	(change-to-buffer buffer window))
    (setf (buffer-undo-p buffer) nil)
    (setf (buffer-minor-mode buffer current-buffer "Interactive-Character-Stream") t)
    (setf (buffer-minor-mode buffer current-buffer "Save") nil)
    (let ((stream (variable-value 'hemlock::buffer-stream :buffer buffer)))
      (bt:make-thread #'(lambda ()
                          (hi::cmdline stream))
                      :name "CMD"))
    (krma.gui::focus-window window)))
  
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
      #+sbcl
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
