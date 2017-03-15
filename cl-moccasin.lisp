(in-package :common-lisp-user)

(defpackage :cl-moccasin
  (:use :common-lisp)
  (:export :py-start
	   :py-send
	   :py-recv
	   :py-peek
	   :py-wait
	   :py-kill
	   :set-python-executable))

(in-package :cl-moccasin)

(defparameter *python-executable* #P"C:/DWR/Main/pob/Scripts/python.exe")	      

(defparameter *python-stream* nil)
(defparameter *python-process* nil)
(defparameter *python-buffer* nil)
(defparameter *python-streams* (make-hash-table :test 'equal))
(defparameter *python-processes* (make-hash-table :test 'equal))
(defparameter *python-buffers* (make-hash-table :test 'equal))


(defun set-python-executable (path)
  "Assign a new value to the *python-executable* parameter."
  (setf *python-executable* path))


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


(defun trim-python-prompt (string)
  "Return a copy of the given string, sans preceding '>>> ' if present."
  (if (and (> (length string) 3)
	     (string-equal ">>> " (subseq string 0 4)))
      (subseq string 4 (length string))
      string))


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

(defun py-start (&key
		   (identifier (gensym "py-iostream-"))
		   (path *python-executable*))
  "Instantiate two-way stream for a Python process and keep local reference."
  (declare (sb-ext:unmuffle-conditions style-warning))
  (multiple-value-bind (py-stream py-process) (program-stream path '("-i"))
    (if (null identifier)
	(progn
	  (setf *python-stream* py-stream)
	  (setf *python-process* py-process))
	(progn
	  (setf (gethash identifier *python-streams*) py-stream)
	  (setf (gethash identifier *python-processes*) py-process))))
  identifier)


(defun py-send (string &optional (identifier nil))
  "Send the given string to python stream, then finish-output."
  (write-finished-line string
		       (if (null identifier)
			   *python-stream*
			   (gethash identifier *python-streams*))))


(defun py-lines (&optional (identifier nil))
  "Retrieve current lines of output buffer from python stream."
  (let ((previous-remains (if (null identifier)
			      *python-buffer*
			      (gethash identifier *python-buffers*))))
    (multiple-value-bind (lines remains)
	(read-lines-no-hang (if (null identifier)
				*python-stream*
				(gethash identifier *python-streams*)))
      (if (> (length lines) 0)
	  (progn
	    (if (null identifier)
		(setf *python-buffer* nil)
		(setf (gethash identifier *python-buffers*) nil))
	    (if (not (null previous-remains))
		(setf lines (cons (concatenate 'string
					       previous-remains
					       (car lines))
				  (cdr lines)))))
	  (setf remains (concatenate 'string previous-remains remains)))
      (setf remains (trim-python-prompt remains))
      (when (equal (length remains) 0) (setf remains nil))
      (if (null identifier)
	  (setf *python-buffer* remains)
	  (setf (gethash identifier *python-buffers*) remains))
      (mapcar #'trim-python-prompt lines))))


(defun py-recv (&optional (identifier nil))
  "Print lines of buffered output from python stream"
  (print-strings (py-lines identifier)))


(defun py-wait (&optional (identifier nil))
  "Read/print lines from python stream, blocking until control is restored."
  (let ((lines (py-lines identifier))
	(finished nil))
    (py-send "()" identifier)
    (do ((i 0 (+ 1 i)))
	(finished (print-strings lines))
      (let ((newlines (py-lines identifier)))
	(when (not (null newlines))
	  (when (string-equal (elt newlines (- (length newlines) 1)) "()")
	    (setf newlines (subseq newlines 0 (- (length newlines) 1)))
	    (setf finished t))
	  (when (> (length newlines) 0)
	    (print-strings lines)
	    (setf lines newlines)))))))  


(defun py-pid (&optional (identifier nil))
  "Get the Windows PID for the running python process."
  (get-process-id (sb-ext:process-pid
		   (if (null identifier)
		       *python-process*
		       (gethash identifier *python-processes*)))))


(defun py-peek (&optional (identifier nil))
  "Peek at the contents of last-seen unterminated line in buffer (if any)."
  (let ((py-buffer (if (null identifier)
		       *python-buffer*
		       (gethash identifier *python-buffers*))))
    (when (not (null py-buffer))
      (format t "~A" py-buffer))))


(defun py-kill (&optional (identifier nil))
  "Forcibly kill the running python process."
  (sb-ext:run-program "cmd"
		      (list (format nil "/C taskkill /f /pid ~A"
				    (py-pid identifier))) :search t)
  (let ((py-process (if (null identifier)
			*python-process*
			(gethash identifier *python-processes*))))
    (sb-ext:process-wait py-process)
    (sb-ext:process-close py-process)
    (sb-ext:process-exit-code py-process)))
