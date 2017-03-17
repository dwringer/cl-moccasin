;;;; cl-moccasin.lisp
;;;; by <dwringer@gmail.com>

(in-package :common-lisp-user)

;;; CL-MOCCASIN [SBCL-only]

;;; This package provides a way to manage multiple subordinate
;;; interactive console applications running in separate processes on
;;; Microsoft Windows.  The primary motivation is to provide a simple
;;; way to interact with interpreters for other high-level languages
;;; like Python.

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
	   :set-test-string  ; Set self-evaluating string used for control test
	   :set-default-executable   ; Change default used by (start)
	   :set-default-arguments))  ; Change default used by (start)

(in-package :cl-moccasin)

;;; Set *default-executable* to the full path of the default program
;;; to be run with (cl-moc:start) [start may also be called with the
;;; :path keyword, specifying an override to this default].  Set
;;; *default-arguments* to be a list of strings to append to the
;;; program call that will ensure that it launches in interactive mode
;;; using standard console I/O streams.  Specifying *prompt* and
;;; *test-string* is required for proper inference of
;;; return-of-control when using (cl-moc:wait).

(defparameter *default-executable* #P"python.exe")
(defparameter *default-arguments* '("-i"))  ; Start in interactive mode
(defparameter *prompt* ">>> ")              ; Trimmed from output lines
(defparameter *test-string* "()")           ; String that evaluates to itself
(defparameter *stream* nil)
(defparameter *process* nil)
(defparameter *buffer* nil)
(defparameter *prompts* (make-hash-table :test 'equal))
(defparameter *test-strings* (make-hash-table :test 'equal))
(defparameter *streams* (make-hash-table :test 'equal))
(defparameter *processes* (make-hash-table :test 'equal))
(defparameter *buffers* (make-hash-table :test 'equal))


(defun set-default-executable (path)
  "Assign a new value to the *default-executable* parameter."
  (setf *default-executable* path))


(defun set-default-arguments (args)
  "Assign a new value to the *default-arguments* parameter."
  (setf *default-arguments* args))


(defun set-prompt (prompt &optional (identifier nil))
  "Assign a new PROMPT specification for specified process."
  (if (null identifier)
      (setf *prompt* prompt)
      (setf (gethash identifier *prompts*) prompt)))


(defun set-test-string (test &optional (identifier nil))
  "Assign a new TEST-STRING specification for specified process."
  (if (null identifier)
      (setf *test-string* test)
      (setf (gethash identifier *test-strings*) test)))


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
      (format t "~A~%" acc))))


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
		(args *default-arguments*)
		(prompt *prompt*)
		(test *test-string*))
  "Instantiate two-way stream for interpreter process and keep reference."
  (if (null identifier)
      (progn
	(setf *prompt* prompt)
	(setf *test-string* test))
      (progn
	(setf (gethash identifier *prompts*) prompt)
	(setf (gethash identifier *test-strings*) test)))
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
	(finished nil)
	(test (if (null identifier)
		  *test-string*
		  (gethash identifier *test-strings*))))
    (send test identifier)
    (do ((i 0 (+ 1 i)))
	(finished (print-strings lines))
      (let ((newlines (lines identifier)))
	(when (not (null newlines))
	  (when (string-equal (elt newlines (- (length newlines) 1)) test)
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


;;; PYTHON-SPECIFIC FUNCTIONS:

(defun python-send-function (string &optional (identifier nil))
  "Push a function definition to the Python interpreter and await control."
  (send (format nil "~A~%" string) identifier)
  (let ((stream (if (null identifier)
		    *stream*
		    (gethash identifier *streams*))))
    (do ((done nil))
	((not (null done)) nil)
      (sleep .02)
      (setf done (listen stream))))
  (recv identifier)
  (if (null identifier)
      (setf *buffer* nil)
      (setf (gethash identifier *buffers*) nil))
  (wait identifier))


(defun python-monitor-interrupts (&optional (identifier nil))
  "Prepare and start a python monitor thread for KeyboardInterrupt requests"
  (python-send-function (format nil "
def watch_for_keyboard_interrupt():
    from os import remove
    from six.moves._thread import interrupt_main
    from time import sleep
    filename = '~A_access.lock'
    while True:
        try:
            _interrupt = False
            with open(filename, 'rb') as inf:
                if len(inf.readlines()) > 0:
                    _interrupt = True
            if _interrupt:
                interrupt_main()
                with open(filename, 'wb') as outf:
                    _nil = outf
                remove(filename)
        except Exception:
            pass
        sleep(0.5)" (string identifier)) identifier)
  (send "from six.moves._thread import start_new_thread" identifier)
  (wait identifier)
  (send "_CL_INTERRUPT = start_new_thread(watch_for_keyboard_interrupt, ())"
	identifier)
  (wait identifier))


(defun python-interrupt (&optional (identifier nil))
  "Signal python monitor thread to raise KeyboardInterrupt, via *_access.lock"
  (let ((filename (format nil "~A_access.lock" (string identifier))))
    (with-open-file (outf filename :direction :output :if-exists :supersede)
      (format outf "X"))))
  
