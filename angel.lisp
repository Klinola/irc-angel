 (require 'usocket)

 (defpackage :ampws.irc.angel
   (:use :cl :usocket))

 (in-package :ampws.irc.angel)

 (defclass irc-servers ()
	  ((host
	    :initarg :host
 	    :initform (error "Must reply a host name")
	    :accessor host)
	   (port
	    :initarg :port
	    :initform "6667"
	    :accessor port)
	   (nick
	    :initarg :nick
	    :initform nil
	    :accessor nick)))

   (defclass angels (irc-servers)
     ((irc-name
       :initarg :irc-name
       :initform nil
       :accessor angel-irc)
      (angel-name
       :initarg :angel-name
       :initform "Angel"
       :accessor angel-name)
      (irc-chan
       :initarg :irc-chan
       :initform "#anonymousliar"
       :accessor irc-chan)
      host
      port
      nick))

 (defmethod initialize-instance :after ((server angels) &key)
	    (unless (nick server)
	      (with-accessors ((nick nick) (host host)) server
		(setf nick
		      (subseq host
			      (1+ (position #\. host))
			      (position #\. host :from-end t)))))
	    (call-next-method))

(defgeneric arc-light (angel length stay-time &optional path)
  (:documentation "Operation Arc Light."))

(defmethod arc-light ((angel angels) length stay-time &optional (path "~/IRC/logs/"))
  (with-accessors ((host host) (port port)
		   (name angel-name) (channel irc-chan)(nick nick)) angel
    (let ((socket (socket-connect host port))
	  (start  (get-universal-time)))
      (socket-send socket
		   (progn
		     (format nil "/nick ~a~&" name)
		     (format nil "/user ~a:~a~&" name "Ampws")
		     (format nil "/join ~a~&" channel))
		   length)
      (with-open-file (out (format nil "~a~a:~a:~a" path name nick channel)
			   :direction :output )
	(do ((end (+ start stay-time)))
	    ((eql #'get-universal-time end) (socket-close socket))
	  (socket-receive socket out nil))))))


(defun vega (host port nick name channel length stay-time &optional (path "~/IRC/logs/"))
    (let ((socket (socket-connect host port :timeout stay-time :local-port length))
	  (start  (get-universal-time)))
      (socket-send socket
		   (progn
		     (format nil "/nick ~a~&" name)
		     (format nil "/user ~a:~a~&" name "Ampws")
		     (format nil "/join ~a~&" channel))
		   length)
      (with-open-file (out (concatenate 'string path name nick channel)
			   :direction :output )
	(do ((end (+ start stay-time)))
	    ((eql #'get-universal-time end) (socket-close socket))
	 (socket-receive socket out nil)))))
