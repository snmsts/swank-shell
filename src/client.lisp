(cl:in-package :cl-user)

(defpackage :swank-shell.client
  (:use :cl))

(in-package :swank-shell.client)

(defvar *list* ())
(defvar *lock* (load-time-value (bt:make-lock)))
(defvar *sockets* '())
(defvar *count* 0)

(defclass swank-client ()
  ((buffer
    :initform '()
    :documentation "")
   (lock
    :initform (bt:make-lock)
    :documentation "")
   (sockets
    :initform '()
    :documentation "")
   (count
    :initform 0)
   (thread :initform nil)
   (queue :initform nil))
  (:documentation ""))

(defun conn-thread (cl)
  (lambda ()
    (loop :while t :do
       (dolist (socket (usocket:wait-for-input (slot-value cl 'sockets) :timeout 1 :ready-only t))
         (ignore-errors
           (let* ((swank t)
                  (in (usocket:socket-stream socket))
                  (buff (loop :repeat 6
                           :for c := (read-byte in)
                           :do (setq swank (and swank (digit-char-p (code-char c) 16)))
                           :collect c))
                  (length (parse-integer (coerce (mapcar #'code-char buff) 'string) :radix 16))
                  (body (trivial-utf-8:read-utf-8-string (usocket:socket-stream socket) :byte-length length)))
             (format t "recv:~A~%" body)
             (force-output))))
       (let* ((lock (slot-value cl 'lock))
              (list (bt:with-lock-held (lock)
                     (prog1 (slot-value cl 'queue)
                       (setf (slot-value cl 'queue) nil))))
             (socket (first (slot-value cl 'sockets))))
         (loop :for l :in (nreverse list)
            :for body := (format nil "~S" `(:emacs-rex (swank:interactive-eval ,l) "COMMON-LISP-USER" t ,(incf (slot-value cl 'count))))
            :for length := (length body)
            :do
            (write-sequence (babel:string-to-octets (format nil "~6,'0x" length)) (usocket:socket-stream socket))
            (write-sequence (babel:string-to-octets body :encoding :utf-8) (usocket:socket-stream socket))
            (force-output (usocket:socket-stream socket)))))))

(defun send-message (cl message)
  (let ((lock (slot-value cl 'lock)))
    (bt:with-lock-held (lock)
      (push message (slot-value cl 'queue)))))

(defun connect-swank (host port)
  (let ((cl (make-instance 'swank-client)))
    (push (usocket:socket-connect "localhost" 4050 :element-type '(unsigned-byte 8)) (slot-value cl 'sockets))
    (bt:make-thread (conn-thread cl))
    cl))
