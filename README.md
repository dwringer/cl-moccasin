# CL-MOCCASIN [SBCL-only]

This package provides a way to manage multiple subordinate
interactive console applications running in separate processes on
Microsoft Windows.  The primary motivation is to provide a simple
way to interact with interpreters for other high-level languages
like Python.

Currently this has only been tested with Python 2.7

Example:
```
CL-MOC> (start :identifier nil)      ; Launch default instance.
; <OR>
CL-MOC> (defparameter *p1* (start))  ; Launch unique instance as *p1*.
NIL
CL-MOC> (wait)  ; Use (wait *p1*) to target *p1* instead of default.
Python 2.7.10 (default, ...   ; [abbreviated for example]
Type "help", "copyright", ... ; " " "
NIL
CL-MOC> (send "from my_module import MyClass")  ; [ ... *p1*)]
NIL
CL-MOC> (wait)
NIL  ; this implies success, otherwise we would see an error or be blocked.
CL-MOC> (send "MyClass().do_something()")  ; [ ... *p1*)]
NIL
CL-MOC> (recv)  ; [ ... *p1*)]
; <<Finished lines of output from Python>>
CL-MOC> (peek)  ; [ ... *p1*)]
; <<If there was an incomplete line during (recv), it is displayed here>>
CL-MOC> (wait)  ; [ ... *p1*)]
; <<Blocks until line(s) terminated in Python and printed to REPL>>
NIL
CL-MOC> (kill)  ; [ ... *p1*)]
NIL  ; Process was successfully terminated.
```