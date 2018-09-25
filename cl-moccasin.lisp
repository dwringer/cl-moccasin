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
  (:use :common-lisp :sb-ext)
  (:nicknames :cl-moc)
  (:export :start  ; Launch a new subordinate process
	   :send   ; Send to the process input stream
	   :recv   ; Receive complete lines from process output stream
	   :ping   ; Check process for interpreter control (w/timeout)
	   :pong   ; A response symbol from ping (not defined)
	   :peek   ; Preview incomplete line from process output stream
	   :wait   ; Read lines from process output until control returns
	   :kill   ; Forcibly terminate the process
	   :psw    ; (progn (send ... ) (wait ...))
	   :set-prompt  ; Set the string used by the interpreter as a prompt
	   :set-test-string  ; Set self-evaluating string used for control test
	   :set-default-executable  ; Change default used by (start)
	   :set-default-arguments   ; Change default used by (start)
	   :python-send-function    ; Coordinate sending function def to Python
	   :python-monitor-interrupts  ; Start Python interrupt monitor thread
	   :python-interrupt        ; Signal monitor to raise KeyboardInterrupt
	   :python-import           ; Import name from module
	   :create-python-timer     ; Timer functions creation macro
	   :init-python-timers))    ; Batch timer blocking initializer


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
(defparameter *active-ping* nil)
(defparameter *prompts* (make-hash-table :test 'equal))
(defparameter *test-strings* (make-hash-table :test 'equal))
(defparameter *streams* (make-hash-table :test 'equal))
(defparameter *processes* (make-hash-table :test 'equal))
(defparameter *buffers* (make-hash-table :test 'equal))
(defparameter *active-pings* (make-hash-table :test 'equal))


(defun get-active-ping (id)
  "Retrieve appropriate value of ACTIVE-PING for identified process stream."
  (if (null id)
      *active-ping*
      (gethash id *active-pings*)))

(defun get-buffer (id)
  "Retrieve appropriate value of BUFFER for identified process stream."
  (if (null id)
      *buffer*
      (gethash id *buffers*)))

(defun get-process (id)
  "Retrieve appropriate value of PROCESS for identified process stream."
  (if (null id)
      *process*
      (gethash id *processes*)))

(defun get-prompt (id)
  "Retrieve appropriate value of PROMPT for identified process stream."
  (if (null id)
      *prompt*
      (gethash id *prompts*)))

(defun get-stream (id)
  "Retrieve appropriate value of STREAM for identified process stream."
  (if (null id)
      *stream*
      (gethash id *streams*)))

(defun get-test-string (id)
  "Retrieve appropriate value of TEST-STRING for identified process stream."
  (if (null id)
      *test-string*
      (gethash id *test-strings*)))


(defun set-active-ping (id value)
  "Assign a new ACTIVE-PING specification for specified process."
  (if (null id)
      (setf *active-ping* value)
      (setf (gethash id *active-pings*) value)))

(defun set-buffer (id value)
  "Assign a new BUFFER specification for specified process."
  (if (null id)
      (setf *buffer* value)
      (setf (gethash id *buffers*) value)))

(defun set-process (id value)
  "Assign a new PROCESS specification for specified process."
  (if (null id)
      (setf *process* value)
      (setf (gethash id *processes*) value)))

(defun set-prompt (id prompt)
  "Assign a new PROMPT specification for specified process."
  (if (null id)
      (setf *prompt* prompt)
      (setf (gethash id *prompts*) prompt)))

(defun set-stream (id value)
  "Assign a new STREAM specification for specified process."
  (if (null id)
      (setf *stream* value)
      (setf (gethash id *streams*) value)))

(defun set-test-string (id test)
  "Assign a new TEST-STRING specification for specified process."
  (if (null id)
      (setf *test-string* test)
      (setf (gethash id *test-strings*) test)))

(defun set-default-executable (path)
  "Assign a new value to the *default-executable* parameter."
  (setf *default-executable* path))

(defun set-default-arguments (args)
  "Assign a new value to the *default-arguments* parameter."
  (setf *default-arguments* args))


;; Thanks to |3b| from #lisp on irc.freenode.net:
;;(sb-alien:define-alien-routine ("GetProcessId" get-process-id) sb-win32:dword
;;  (handle sb-win32:handle))


;; Thanks to danlei from stackoverflow.com/questions/15988870/:
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
  (set-prompt identifier prompt)
  (set-test-string identifier test)
  (multiple-value-bind (stream process) (program-stream path args)
    (set-stream identifier stream)
    (set-process identifier process))
  identifier)


(defun send (string &optional (identifier nil))
  "Send the given string to stream, then finish-output."
  (write-finished-line string (get-stream identifier)))


(defun lines (&optional (identifier nil))
  "Retrieve current lines of output buffer from stream."
  (let ((previous-remains (get-buffer identifier))
	(prompt (get-prompt identifier)))
    (multiple-value-bind (lines remains)
	(read-lines-no-hang (get-stream identifier))
      (if (> (length lines) 0)
	  (progn
	    (set-buffer identifier nil)
	    (if (not (null previous-remains))
		(setf lines (cons (concatenate 'string
					       previous-remains
					       (car lines))
				  (cdr lines)))))
	  (setf remains (concatenate 'string previous-remains remains)))
      (setf remains (trim-prompt remains prompt))
      (when (equal (length remains) 0) (setf remains nil))
      (set-buffer identifier remains)
      (mapcar #'(lambda (x) (trim-prompt x prompt)) lines))))


(defun recv (&optional (identifier nil) return-lines)
  "Print lines of buffered output from stream"
  (let* ((lines (lines identifier))
	 (length (length lines))
	 (test (get-test-string identifier))
	 (prompt (get-prompt identifier)))
    (when (and (get-active-ping identifier)
	       (> length 0)
	       (or (string-equal test (elt lines (- length 1)))
		   (string-equal test
				 (trim-prompt (elt lines (- length 1))
					      prompt))))
      (setf lines (subseq lines 0 (- length 1)))
      (set-active-ping identifier nil))
    (print-strings lines)
    (when return-lines (setf return-lines lines)))
  return-lines)


(defun ping (&optional (identifier nil) (persist-for 2) (force-reset nil))
  "Check for a response from the interpreter stream"
  (when force-reset (set-active-ping identifier nil))
  (let ((test (get-test-string identifier))
	(ping (get-active-ping identifier)))
    (when (null ping)
      (send test identifier)
      (set-active-ping identifier t))
    (let ((tau (get-internal-real-time))
	  (delta (* persist-for internal-time-units-per-second)))
      (do ((return nil)
	   (theta 0))
	  ((or (>= theta (+ tau delta)) (not (null return))) return)
	(setf theta (get-internal-real-time))
	(let* ((lines (lines identifier))
	       (length (length lines))
	       (prompt (get-prompt identifier)))
	  (when (> length 0)
	    (when (or (string-equal test (elt lines (- length 1)))
		      (string-equal test
				    (trim-prompt (elt lines (- length 1))
						 prompt)))
	      (setf lines (subseq lines 0 (- length 1)))
	      (setf return 'pong)
	      (set-active-ping identifier nil))
	    (print-strings lines)))))))


(defun wait (&optional (identifier nil) (return-lines nil))
  "Read/print lines from stream, blocking until control is restored."
  (let ((results nil)
	(lines (lines identifier))
	(finished nil)
	(test (get-test-string identifier)))
    (send test identifier)
    (do ((i 0 (+ 1 i)))
	(finished (progn
		    (when return-lines (setf results (append results lines)))
		    (print-strings lines)
		    (when return-lines results)))
      (let ((newlines (lines identifier)))
	(when (not (null newlines))
	  (when (string-equal (elt newlines (- (length newlines) 1)) test)
	    (setf newlines (subseq newlines 0 (- (length newlines) 1)))
	    (setf finished t))
	  (when (> (length newlines) 0)
	    (when return-lines (setf results (append results lines)))
	    (print-strings lines)
	    (setf lines newlines)))))))  


;;(defun pid (&optional (identifier nil))
;;  "Get the Windows PID for the running process."
;;  (get-process-id (sb-ext:process-pid (get-process identifier))))


(defun peek (&optional (identifier nil))
  "Peek at the contents of last-seen unterminated line in buffer (if any)."
  (let ((buffer (get-buffer identifier)))
    (when (not (null buffer))
      (format t "~A" buffer))))


;; (defun kill (&optional (identifier nil))
;;   "Forcibly kill the running process."
;;   (sb-ext:run-program "cmd"
;; 		      (list (format nil "/C taskkill /f /pid ~A"
;; 				    (pid identifier))) :search t)
;;   (let ((process (get-process identifier)))
;;     (sb-ext:process-wait process)
;;     (sb-ext:process-close process)
;;     (sb-ext:process-exit-code process)
;;     (set-stream identifier nil)))

(defun kill (&optional (identifier nil))
  "Forcibly kill the running process."
  (let ((process (get-process identifier)))
    (sb-ext:run-program "cmd"
			(list (format nil "/C taskkill /f /pid ~A"
				      (sb-ext:process-pid process))) :search t)
    (sb-ext:process-wait process)
    (sb-ext:process-close process)
    (sb-ext:process-exit-code process)
    (set-stream identifier nil)))


(defun psw (string &optional (identifier nil))
  "Equivalent to (Progn (Send string identifier) (Wait identifier))"
  (send string identifier)
  (wait identifier))


;;; TIMER FUNCTIONS:

(defun start-cycle (timer repeat-interval
		    &key
		      (delay-seconds 10)
		      (start-time (get-universal-time)))
  (schedule-timer timer
		  (+ start-time delay-seconds)
		  :repeat-interval repeat-interval
		  :absolute-p t))


(defun run-step (id code name)
  (if (equal (ping id) 'pong)
      (progn
	(send code id)
	(format t "<RUNNING ~A>~%" name)
	t)
      (progn
	(format t "<WAITING ~A>~%" name)
	nil)))


(defparameter *timer-process-wait-counts* nil)


(defun aget (key alist &optional (test #'eql))
  "Shortcut to directly obtain value from an alist by key"
  (cdr (assoc key alist :test test)))


(defun set-timer-waits (id &optional (value-or-fn 0))
  (labels ((const-f-ignore-x (const)
	     #'(lambda (x) (declare (ignore x)) const)))
    (do ((fn (if (equal (type-of value-or-fn) 'function)
		 value-or-fn
		 (const-f-ignore-x value-or-fn)))
	 (i 0 (+ i 1)) fin)
	((or fin (= i (length *timer-process-wait-counts*)))
	 (when (not fin)
	   (push (cons id (funcall fn 0)) *timer-process-wait-counts*)))
      (destructuring-bind (timer . waits) (elt *timer-process-wait-counts* i)
	(when (equal timer id)
	  (setf (cdr (elt *timer-process-wait-counts* i)) (funcall fn waits))
	  (setf fin t))))))


(defun increment-timer-waits (id &optional (value 1))
  (set-timer-waits id #'(lambda (x) (+ x value))))


;;; PYTHON-SPECIFIC FUNCTIONS:

(defun python-send-function (string &optional (identifier nil))
  "Push a function definition to the Python interpreter and await control."
  (send (format nil "~A~%" string) identifier)
  (let ((stream (get-stream identifier)))
    (do ((done nil))
	((not (null done)) nil)
      (sleep .02)
      (setf done (listen stream))))
  (recv identifier)
  (set-buffer identifier nil)
  (wait identifier))


(defun python-monitor-interrupts (&optional (identifier nil))
  "Prepare and start a python monitor thread for KeyboardInterrupt requests"
  (wait identifier)
  (python-send-function (format nil "
def watch_for_keyboard_interrupt():
    from os import remove
    from sys import version_info
    from time import sleep
    if version_info.major >= 3:
        from _thread import interrupt_main
    else:
        from thread import interrupt_main
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
      (format outf "X")))
  (set-active-ping identifier nil))


(defun python-import (from what &optional (identifier nil))
  (psw (format nil "from ~A import ~A" from what) identifier))


;;; PYTHON TIMER FUNCTIONS:

(defparameter *python-timers* nil)
(defparameter *python-initialization* (make-hash-table :test #'equal))


(defun timer-init-p (name) (gethash name *python-initialization*))

(defparameter *scheduled-intervals* nil)
(defparameter *scheduled-interval-mutex* (sb-thread:make-mutex))

(defun get-scheduled-interval (name)
  (sb-thread:with-mutex (*scheduled-interval-mutex*)
    (aget name *scheduled-intervals* #'equal)))

(defun set-scheduled-interval (name interval)
  (sb-thread:with-mutex (*scheduled-interval-mutex*)
    (let ((current (assoc name *scheduled-intervals* :test #'equal)))
      (if current
	  (setf (cdr current) interval)
	  (push (cons name interval) *scheduled-intervals*)))))

(defmacro create-python-timer (name init-fn step-expr)
  (let ((instance-name
	 (intern (string-upcase (format nil "*~A-python*" name))))
	(timer-name (intern (string-upcase (format nil "*~A-timer*" name))))
	(init-fn-name (intern (string-upcase (format nil "init-~A" name))))
	(run-fn-name (intern (string-upcase (format nil "run-~A" name))))
	(schedule-fn-name
	 (intern (string-upcase (format nil "schedule-~A" name))))
	(cancel-fn-name
	 (intern (string-upcase (format nil "cancel-~A" name)))))
    `(progn
       (push ,name *python-timers*)
       (setf (gethash ,name *python-initialization*) nil)
       (defparameter ,instance-name nil)  ; (start))
       (defun ,init-fn-name (&optional (id ,instance-name))
	 (if (null id)
	     (progn
	       (setf ,instance-name (start))
	       (ping ,instance-name)
	       (wait ,instance-name)
	       (funcall ,init-fn ,instance-name)
	       (spawn-python-timer ,name))
	     (funcall ,init-fn id))
	 (setf (gethash ,name *python-initialization*) t))
       (defun ,run-fn-name (&optional (id ,instance-name))
	 (cl-moc::run-step id ,step-expr ,name))
;;	 (when (not (cl-moc::run-step id ,step-expr ,name))
;;	   (eval '(increment-timer-waits ,timer-name))
;;	   (when (>= (eval '(aget ,timer-name *timer-process-wait-counts*
;;			    #'equal)) 5)
;;	     (eval '(funcall (set-timer-waits ,timer-name)))
;;	     (eval '(funcall ,cancel-fn-name))
;;	     (eval '(funcall ,init-fn-name))
;;	     (eval '(funcall ,schedule-fn-name
;;		     (get-scheduled-interval ,name))))))
       (defparameter ,timer-name nil)
       (defun ,schedule-fn-name (repeat-interval)
	 (set-scheduled-interval ,name repeat-interval)
	 (set-timer-waits ,timer-name)
	 (schedule-python-name ,name repeat-interval :delay-seconds 5))
       (defun ,cancel-fn-name () (cancel-python-timer ,name)))))


(defun spawn-python-timer (name)
  (eval (read-from-string
	 (format nil "(setf *~A-timer* ~A)"
		 name
		 (format nil "(make-timer #'(lambda () (run-~A *~A-python*)))"
			 name name)))))


(defun cancel-python-timer (name)
  (setf (gethash name *python-initialization*) nil)
  (eval (read-from-string (format nil "(unschedule-timer *~A-timer*)" name)))
  (eval (read-from-string (format nil "(kill *~A-python*)" name)))
  (eval (read-from-string (format nil "(setf *~A-python* nil)" name)))
  (eval (read-from-string (format nil "(setf *~A-timer* nil)" name))))


(defun ping-python-name (n &optional (persist-for 2))
  (eval (read-from-string (format nil "(ping *~A-python* ~A)" n persist-for))))


(defun init-python-timers (&optional (names nil))  ; *python-timers*))
  (when (null names)
    (setf names (remove-if #'timer-init-p *python-timers*)))
  (let ((init-functions nil))
    (map nil #'(lambda (n)
		 (push (intern (string-upcase (format nil "init-~A" n)))
		       init-functions))
	 names)
    (map nil #'(lambda (f) (funcall f)) init-functions)
    (do ((fin nil))
    	(fin nil)
      (let ((acc 0)
    	    (pings (mapcar #'ping-python-name names)))
    	(do ((i 0 (+ i 1)))
    	    ((= i (length pings)) nil)
    	  (when (equal 'pong (elt pings i))
    	    (incf acc)))
    	(when (= acc (length pings))
    	  (setf fin t))))))


(defun schedule-python-name (n repeat-interval &key (delay-seconds 10))
  (when (not (timer-init-p n))
    (init-python-timers (list n)))
  (eval (read-from-string (format nil "(wait *~A-python*)" n)))
  (eval (read-from-string
	 (format nil "(cl-moc::start-cycle *~A-timer* ~A :delay-seconds ~A)"
		 n repeat-interval delay-seconds))))
