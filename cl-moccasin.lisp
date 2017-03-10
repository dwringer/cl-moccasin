(in-package :common-lisp-user)

(defpackage :cl-moccasin
  (:use :common-lisp))

(in-package :cl-moccasin)

(defparameter *python-program* nil)


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


(defun print-strings (string-list &optional (separator ""))
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


(defun py-start (&optional (path "C:/DWR/Main/pob/Scripts/python.exe"))
  "Instantiate two-way stream *python-program* for an inferior Python process."
  (defparameter *python-program* (program-stream path '("-i"))))


(defun py-send (string)
  "Send the given string to *python-program*, then finish-output."
  (write-finished-line string *python-program*))


(defun py-recv ()
  "Retrieve current lines of output buffer from *python-program*"
  (print-strings (mapcar #'trim-python-prompt
			 (read-lines-no-hang *python-program*))
		 (format nil "~C" #\newline)))


(defun py-wait ()
  "Read lines from *python-program*, blocking until control is restored."
  (let ((lines (mapcar #'trim-python-prompt
		       (read-lines-no-hang *python-program*)))
	(finished nil))
    (py-send "()")
    (do ((i 0 (+ 1 i)))
	(finished (print-strings lines (format nil "~C" #\newline)))
      (let ((newlines (mapcar #'trim-python-prompt
			      (read-lines-no-hang *python-program*))))
	(when (not (null newlines))
	  (when (string-equal (elt newlines (- (length newlines) 1)) "()")
	    (setf newlines (subseq newlines 0 (- (length newlines) 1)))
	    (setf finished t))
	  (when (> (length newlines) 0)
	    (print-strings lines (format nil "~C" #\newline))
	    (setf lines newlines)))))))  
