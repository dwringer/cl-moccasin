(in-package :common-lisp-user)

(defpackage :cl-moccasin
  (:use :common-lisp
	:common-lisp-user)
  (:export :py-start
	   :py-send
	   :py-recv
	   :py-wait))

(in-package :cl-moccasin)

(defparameter *python-program* nil)
(defparameter *python-programs* (make-hash-table :test 'equal))

(defun program-stream (program &optional args)
  "Create a two-way stream for a new instance of program [w/arguments]"
  (let ((process (sb-ext:run-program program args
				     :input :stream
				     :output :stream
				     :wait nil
				     :search t)))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
			   (sb-ext:process-input process)))))


(defun print-strings (string-list
		      &optional (separator (format nil "~C" #\newline)))
  "Print a joined list of strings to the REPL [with optional separator]"
  (when (> (length string-list) 0)
    (let ((acc (elt string-list 0)))
      (dotimes (i (- (length string-list) 1))
	(setf acc (concatenate 'string acc separator
			       (elt string-list (+ 1 i)))))
      (format t acc))))


(defun trim-eol (string)
  "Remove trailing <Return> character from string (i.e. Windows text line)"
  (string-right-trim (format nil "~C" #\return) string))


(defun trim-python-prompt (string)
  "Return a copy of the given string, sans preceding '>>> ' if present."
  (if (and (> (length string) 3)
	     (string-equal ">>> " (subseq string 0 4)))
      (subseq string 4 (length string))
      string))


(defun read-line-no-hang (stream)
  "Read any buffered line from the stream without blocking"
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
	(isnil nil))
    (do ((i 0 (+ 1 i)))
	(isnil (reverse acc))
      (multiple-value-setq (line isnil) (read-line-no-hang stream))
      (when (not isnil) (push line acc)))))


(defun write-finished-line (string stream)
  "Write a string to the given stream and call finish-output."
  (write-line string stream)
  (finish-output stream))


(defun py-start (&key
		   (path "C:/DWR/Main/pob/Scripts/python.exe")
		   (identifier (gensym "py-iostream-")))
  "Instantiate two-way stream for a Python process and keep global reference."
  (if (null identifier)
      (setf *python-program* (program-stream path '("-i")))
      (setf (gethash identifier *python-programs*)
	    (program-stream path '("-i"))))
  identifier)


(defun py-send (string &optional (identifier nil))
  "Send the given string to *python-program*, then finish-output."
  (if (null identifier)
      (write-finished-line string *python-program*)
      (write-finished-line string (gethash identifier *python-programs*))))


(defun py-lines (&optional (identifier nil))
  "Retrieve current lines of output buffer from *python-program*"
  (mapcar #'trim-python-prompt
	  (read-lines-no-hang
	   (if (null identifier)
	       *python-program*
	       (gethash identifier *python-programs*)))))	      


(defun py-recv (&optional (identifier nil))
  "Print lines of buffered output from *python-program*"
  (print-strings (py-lines identifier)))


(defun py-wait (&optional (identifier nil))
  "Read/print lines from *python-program*, blocking until control is restored."
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
