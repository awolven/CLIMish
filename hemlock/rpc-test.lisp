(in-package :socket-rpc)

(defvar *ok-to-send* nil)
(defvar *responding* nil)

(defvar *num-responded* 0)
(defvar *num-sent* 0)

(defvar *rpc-server* nil)

(defvar *max-connections* 1)

(defvar *rpc-server-bind-address*)
(defvar *rpc-server-port*)

(defvar *server-thread-count* 0)

(defvar *server-exit?* nil)

(defstruct (socket-device
            (:include hemlock.wire::device)
            (:conc-name hemlock.wire::device-)
            (:constructor make-socket-device (socket))
            (:print-function
             (lambda (object stream depth)
               (declare (ignore depth))
               (print-unreadable-object (object stream)
                 (format stream "socket-device ~A" (device-socket object))))))
  (socket (error "missing argument") :type sb-bsd-sockets:inet-socket))

(defun start-rpc-server (bind-address &key (port nil) (max-incoming 100))
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream
                                :protocol :tcp))
         (server (make-socket-device socket)))
    (sb-bsd-sockets:socket-bind socket bind-address (if port port 0))
    (sb-bsd-sockets:socket-listen socket max-incoming)
    (multiple-value-setq (*rpc-server-bind-address*
                          *rpc-server-port*)
      (sb-bsd-sockets:socket-name socket))
    (setq *rpc-server* server)))

(defun start-local-rpc-server (&key (port nil) (max-connections *max-connections*))
  (start-rpc-server #(127 0 0 1) :port port :max-incoming max-connections))

(defvar *test*)

(defun accept-rpc-connection (server)
  (let ((new (make-socket-device (sb-bsd-sockets:socket-accept (device-socket server))))) ;; blocking
    (setq *test* (setq hemlock.wire:*current-wire* (hemlock.wire:make-wire new)))))

(defun create-request-server (on-connected &key (connection-timeout nil))
  (unless *rpc-server*
    (setq *rpc-server* (start-local-rpc-server :port nil
                                               :max-connections *max-connections*)))
  (create-request-server-thread on-connected :connection-timeout connection-timeout))

(defun create-request-server-thread (on-connected &key (connection-timeout nil))
  (declare (ignore connection-timeout))
  (bt:make-thread
   #'(lambda ()
       (let ((hemlock.wire:*current-wire* nil))
         (accept-rpc-connection *rpc-server*)
         (funcall on-connected hemlock.wire:*current-wire*)))
   :name (format nil "RPC connection to ~A:~A ~A"
                 *rpc-server-bind-address*
                 *rpc-server-port*
                 (incf *server-thread-count*))))

(defun test ()
  (create-request-server #'(lambda (wire) (declare (ignore wire)))))

(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
four element array, suitable for socket-connect. If HOSTNAME is
not found, a host-not-found-error condition is thrown."
  (if hostname
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
      nil))

(defun maybe-vectorize-ip-address (ip-address-or-hostname)
  (let ((ip-address (nslookup ip-address-or-hostname)))
    (if (stringp ip-address)
        (coerce (mapcar #'parse-integer (uiop/utility:split-string ip-address :separator "."))
                'vector)
        (if (vectorp ip-address)
            ip-address
            (error "could not canonicalize ip-address-or-hostname ~S" ip-address-or-hostname)))))

(defun connect-to-remote-server (ip-address port on-connected &optional (on-death nil))
  (declare (ignore on-death))
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream
                                :protocol :tcp))
         (device (make-socket-device socket)))
    (sb-bsd-sockets:socket-connect socket (maybe-vectorize-ip-address ip-address) port)
    (setq hemlock.wire:*current-wire* (hemlock.wire:make-wire device))
    (funcall on-connected (setq hemlock.wire:*current-wire* (hemlock.wire:make-wire device)))
    hemlock.wire:*current-wire*))

(defun serve-request (wire)
  (let ((*responding* t))
    (prog1 (hemlock.wire::wire-get-object wire)
      (incf *num-responded*))))

(defun maybe-serve-request ()
  (let ((*responding* t))
    (let ((wire hemlock.wire::*current-wire*))
      (when wire
        (if (hemlock.wire::wire-listen wire)
            (serve-request wire)
            nil)))))

(defmethod hemlock.wire:device-read ((device socket-device) buffer)
  ;; todo: make this a resource
  (let ((buf-size 512))
    (unless buffer
      (setq buffer
            (make-array buf-size :element-type '(unsigned-byte 8))))
    (let* ((length (socket-receive (device-socket device) buffer (length buffer)))
           (buf (make-array length :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
      (loop for i from 0 below length
            do (vector-push-extend (aref buffer i) buf))
      (hemlock.wire:device-append-to-input-buffer device buf)
      0)))

(defmethod hemlock.wire:device-write ((device socket-device) buffer &optional (end (length buffer)))
  (sb-bsd-sockets:socket-send (device-socket device) buffer end))

(defun hemlock.wire::wire-listen (wire)
  (or (<= (hemlock.wire::wire-ibuf-end wire)
          (hemlock.wire::wire-ibuf-offset wire))
      (hemlock.wire:device-listen (hemlock.wire::wire-device wire))))

(defmethod hemlock.wire:device-listen ((device socket-device))
  (multiple-value-bind (err bytes)
      (sb-bsd-sockets-internal::ioctl
       (sb-bsd-sockets::socket-file-descriptor (device-socket device))
       1074030207 ;; fionread
       1) ;; don't block.
    (when (not (zerop err))
      (error "ioctl returned code ~S" err))
    (values (not (zerop bytes))
            bytes)))

(cffi:defctype SOCKET :uint64)

(cffi:defcfun ("recv" recv) :int
  (socket SOCKET)
  (buf :pointer)
  (len :int)
  (flags :int))

(defun socket-receive (socket buffer length &optional (flags 0))
  (let ((s (sb-bsd-sockets::socket-file-descriptor socket)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((result (recv s (sb-sys:vector-sap buffer) length flags)))
        (cond ((plusp result) result)
              ((zerop result) 0)
              (t (error "recv got error code: ~A" (WSAGetLastError))))))))

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




(defun ok-to-send ()
  (setq *ok-to-send* t)
  (values))
