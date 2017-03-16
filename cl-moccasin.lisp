;;;; cl-moccasin.lisp

;;;; Copyright (C) 2017 Darren W. Ringer

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR
;;;; ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

;;;; Except as contained in this notice, the name of the author shall
;;;; not be used in advertising or otherwise to promote the sale, use
;;;; or other dealings in this Software without prior written
;;;; authorization from the author.

(in-package :common-lisp-user)

;;; CL-MOCCASIN [SBCL-only]

;;; This package provides a way to manage multiple subordinate
;;; interactive console applications running in separate processes on
;;; Microsoft Windows.  The primary motivation is to provide a simple
;;; way to interact with interpreters for other high-level languages
;;; like Python.

;;; Currently this has only been tested with Python 2.7

;;; Example:

;;; CL-MOC> (start :identifier nil)      ; Launch default instance.
;;; ; <OR>
;;; CL-MOC> (defparameter *p1* (start))  ; Launch unique instance as *p1*.
;;; NIL
;;; CL-MOC> (wait)  ; Use (wait *p1*) to target *p1* instead of default.
;;; Python 2.7.10 (default, ...   ; [abbreviated for example]
;;; Type "help", "copyright", ... ; " " "
;;; NIL
;;; CL-MOC> (send "from my_module import MyClass")  ; [ ... *p1*)]
;;; NIL
;;; CL-MOC> (wait)
;;; NIL  ; this implies success, otherwise we would see an error or be blocked.
;;; CL-MOC> (send "MyClass().do_something()")  ; [ ... *p1*)]
;;; NIL
;;; CL-MOC> (recv)  ; [ ... *p1*)]
;;; ; <<Finished lines of output from Python>>
;;; CL-MOC> (peek)  ; [ ... *p1*)]
;;; ; <<If there was an incomplete line during (recv), it is displayed here>>
;;; CL-MOC> (wait)  ; [ ... *p1*)]
;;; ; <<Blocks until line(s) terminated in Python and printed to REPL>>
;;; NIL
;;; CL-MOC> (kill)  ; [ ... *p1*)]
;;; NIL  ; Process was successfully terminated.

(defpackage :cl-moccasin
  (:use :common-lisp)
  (:nicknames :cl-moc)
  (:export :start  ; Launch a new subordinate process
	   :send   ; Send to the process input stream
	   :recv   ; Receive complete lines from process output stream
	   :peek   ; Preview incomplete line from process output stream
	   :wait   ; Read lines from process output until control returns
	   :kill   ; Forcibly terminate the process
	   :set-prompt  ; Set the string used by the interpreter as a prompt
	   :set-default-executable))  ; Change default used by (start)

(in-package :cl-moccasin)

;;; Set *default-executable* to the full path of the default program
;;; to be run with (cl-moc:start) [start may also be called with the
;;; :path keyword, specifying an override to this default].  Set
;;; *arguments* to be a list of strings to append to the program call
;;; that will ensure that it launches in interactive mode using
;;; standard console I/O streams.

(defparameter *default-executable* #P"C:/DWR/Main/pob/Scripts/python.exe")
(defparameter *arguments* '("-i"))  ; Start executable in interpreter mode
(defparameter *prompt* ">>> ")      ; Trimmed from output; REQUIRED
(defparameter *prompts* (make-hash-table :test 'equal))

(defparameter *stream* nil)
(defparameter *process* nil)
(defparameter *buffer* nil)
(defparameter *streams* (make-hash-table :test 'equal))
(defparameter *processes* (make-hash-table :test 'equal))
(defparameter *buffers* (make-hash-table :test 'equal))


(defun set-default-executable (path)
  "Assign a new value to the *default-executable* parameter."
  (setf *default-executable* path))


(defun set-prompt (prompt &optional (identifier nil))
  "Assign a new PROMPT specification for specified process."
  (if (null identifier)
      (setf *prompt* prompt)
      (setf (gethash identifier *prompts*) prompt)))


;; Thanks to |3b| from #lisp on irc.freenode.net:
(sb-alien:define-alien-routine ("GetProcessId" get-process-id) sb-win32:dword
  (handle sb-win32:handle))


(defun program-stream (program &optional args)
  "Create a two-way stream for a new instance of program [w/arguments]."
  (let ((process (sb-ext:run-program program args
				     :input :stream
				     :output :stream
				     :wait nil
				     :search t)))
    (when process
      (values
       (make-two-way-stream (sb-ext:process-output process)
			    (sb-ext:process-input process))
       process))))


(defun print-strings (string-list
		      &optional (separator (format nil "~C" #\newline)))
  "Print a joined list of strings to the REPL [with optional separator]."
  (when (> (length string-list) 0)
    (let ((acc (elt string-list 0)))
      (dotimes (i (- (length string-list) 1))
	(setf acc (concatenate 'string acc separator
			       (elt string-list (+ 1 i)))))
      (format t acc))))


(defun trim-eol (string)
  "Remove trailing <Return> character from string (i.e. Windows text line)."
  (string-right-trim (format nil "~C" #\return) string))


(defun trim-prompt (string &optional (prompt *prompt*))
  "Return a copy of the given string, sans preceding prompt when present."
  (let ((slen (length string))
	(plen (length prompt)))
    (if (and (> slen (- plen 1))
	     (string-equal prompt (subseq string 0 plen)))
	(subseq string plen slen)
	string)))
  
  
(defun read-line-no-hang (stream)
  "Read any buffered line from the stream without blocking."
  (let ((acc (make-array 0
			 :element-type 'character
			 :fill-pointer 0
			 :adjustable t))
	(char (read-char-no-hang stream)))
    (do ((i 0 (+ 1 i)))
	((or (null char) (equal char #\newline))
	 (values (trim-eol acc) (null char)))
      (vector-push-extend char acc)
      (setf char (read-char-no-hang stream)))))


(defun read-lines-no-hang (stream)
  "Get all available buffered lines in the stream, without blocking."
  (let ((acc nil)
	(line nil)
	(isnil nil)
	(remain nil))
    (do ((i 0 (+ 1 i)))
	(isnil (values (reverse acc) remain))
      (multiple-value-setq (line isnil) (read-line-no-hang stream))
      (if (not isnil)
	  (push line acc)
	  (setf remain line)))))


(defun write-finished-line (string stream)
  "Write a string to the given stream and call finish-output."
  (write-line string stream)
  (finish-output stream))


(defun start (&key
		(identifier (gensym "cl-moc-iostream-"))
		(path *default-executable*)
		(args *arguments*)
		(prompt *prompt*))
  "Instantiate two-way stream for interpreter process and keep reference."
  (when (not (null identifier))
    (setf (gethash identifier *prompts*) prompt))
  (multiple-value-bind (stream process) (program-stream path args)
    (if (null identifier)
	(progn
	  (setf *stream* stream)
	  (setf *process* process))
	(progn
	  (setf (gethash identifier *streams*) stream)
	  (setf (gethash identifier *processes*) process))))
  identifier)


(defun send (string &optional (identifier nil))
  "Send the given string to stream, then finish-output."
  (write-finished-line string
		       (if (null identifier)
			   *stream*
			   (gethash identifier *streams*))))


(defun lines (&optional (identifier nil))
  "Retrieve current lines of output buffer from stream."
  (let ((previous-remains (if (null identifier)
			      *buffer*
			      (gethash identifier *buffers*)))
	(prompt (if (null identifier)
		    *prompt*
		    (gethash identifier *prompts*))))
    (multiple-value-bind (lines remains)
	(read-lines-no-hang (if (null identifier)
				*stream*
				(gethash identifier *streams*)))
      (if (> (length lines) 0)
	  (progn
	    (if (null identifier)
		(setf *buffer* nil)
		(setf (gethash identifier *buffers*) nil))
	    (if (not (null previous-remains))
		(setf lines (cons (concatenate 'string
					       previous-remains
					       (car lines))
				  (cdr lines)))))
	  (setf remains (concatenate 'string previous-remains remains)))
      (setf remains (trim-prompt remains prompt))
      (when (equal (length remains) 0) (setf remains nil))
      (if (null identifier)
	  (setf *buffer* remains)
	  (setf (gethash identifier *buffers*) remains))
      (mapcar #'(lambda (x) (trim-prompt x prompt)) lines))))


(defun recv (&optional (identifier nil))
  "Print lines of buffered output from stream"
  (print-strings (lines identifier)))


(defun wait (&optional (identifier nil))
  "Read/print lines from stream, blocking until control is restored."
  (let ((lines (lines identifier))
	(finished nil))
    (send "()" identifier)
    (do ((i 0 (+ 1 i)))
	(finished (print-strings lines))
      (let ((newlines (lines identifier)))
	(when (not (null newlines))
	  (when (string-equal (elt newlines (- (length newlines) 1)) "()")
	    (setf newlines (subseq newlines 0 (- (length newlines) 1)))
	    (setf finished t))
	  (when (> (length newlines) 0)
	    (print-strings lines)
	    (setf lines newlines)))))))  


(defun pid (&optional (identifier nil))
  "Get the Windows PID for the running process."
  (get-process-id (sb-ext:process-pid
		   (if (null identifier)
		       *process*
		       (gethash identifier *processes*)))))


(defun peek (&optional (identifier nil))
  "Peek at the contents of last-seen unterminated line in buffer (if any)."
  (let ((buffer (if (null identifier)
		       *buffer*
		       (gethash identifier *buffers*))))
    (when (not (null buffer))
      (format t "~A" buffer))))


(defun kill (&optional (identifier nil))
  "Forcibly kill the running process."
  (sb-ext:run-program "cmd"
		      (list (format nil "/C taskkill /f /pid ~A"
				    (pid identifier))) :search t)
  (let ((process (if (null identifier)
			*process*
			(gethash identifier *processes*))))
    (sb-ext:process-wait process)
    (sb-ext:process-close process)
    (sb-ext:process-exit-code process)))
