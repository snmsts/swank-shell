(cl:in-package :cl-user)

(defpackage :swank-shell.client
  (:use :cl))

(in-package :swank-shell.client)

(defvar *list* ())
(defvar *lock* (load-time-value (bt:make-lock)))
(defvar *sockets* '())
(defvar *count* 0)

(defun conn-thread ()
  (loop :while t :do
     (dolist (socket (usocket:wait-for-input *sockets* :timeout 1 :ready-only t))
       (ignore-errors
         (let* ((swank t)
                (in (usocket:socket-stream socket))
                (buff (loop :repeat 6
                         :for c := (read-byte in)
                         :do (setq swank (and swank (digit-char-p (code-char c) 16)))
                         :collect c)))
           (let* ((length (parse-integer (coerce (mapcar #'code-char buff) 'string) :radix 16))
                  (body (trivial-utf-8:read-utf-8-string (usocket:socket-stream socket) :byte-length length)))
             (format t "recv:~A~%" body)
             (force-output)))))
     (let ((list (bt:with-lock-held (*lock*)
                   (prog1 *list*
                     (setf *list* nil))))
           (socket (first *sockets*)))
       (loop :for l :in list
          :for body := (format nil "~S" `(:emacs-rex (swank:interactive-eval ,l) "COMMON-LISP-USER" t ,(incf *count*)))
          :for length := (length body)
          :do
          (write-sequence (babel:string-to-octets (format nil "~6,'0x" length)) (usocket:socket-stream socket))
          (write-sequence (babel:string-to-octets body :encoding :utf-8) (usocket:socket-stream socket))
          (force-output (usocket:socket-stream socket))))))

