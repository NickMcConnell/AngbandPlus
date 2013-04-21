;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: game.lisp - simple load of the game
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file just contains simple init and loading of the game

|#

;; add features we need
(eval-when (:execute :load-toplevel :compile-toplevel)
  
;;  (push :xp-testing *features*)

  )
  
#+(or cmu clisp allegro lispworks)
(progn
  (unless (find-package :make)
    (load "tools/defsystem" :verbose nil)))

(eval-when (:execute :load-toplevel :compile-toplevel) 
(proclaim '(optimize
	    #+cmu (ext:inhibit-warnings 2)
            ;;    (speed 3)
            ;;(speed 1)
	    (speed 0)
;;            (compilation-speed 0)
	    (compilation-speed 2)
;;            (safety 1)
	    (safety 2)
            ;;    (debug 1)
            (debug 3)
            )))

#||
;;#+(or cmu allegro)
(progn
  (with-open-file (o-str (pathname "/tmp/langband-compile-error.log")
			 :direction :output
			 :if-exists :new-version
			 :if-does-not-exist :create)
    ))
||#

#+allegro
(progn
  (setf *load-local-names-info* t
	;;(sys:gsgc-switch :print) t
	))
  
(defun compile-in-environment (func)
  (let (
	#+cmu (*compile-print* nil)
	      #+cmu (*load-verbose* nil)
	      (*load-print* nil)
	      ;;#+cmu (*error-output* o-str)
	      #+cmu (extensions:*gc-verbose* nil)
	      )
    (funcall func)))
 

(defun load-game ()
    #+cmu
    (progn
      (alien:load-foreign "./lib/zterm/liblang_ui.so")
      (pushnew :already-loaded *features*))

    
    ;;  (push :langband-debug *features*)
    (load "langband.system")
    (mk:operate-on-system 'langband 'compile :verbose nil)
    (format t "~&System loaded...~%"))

(compile-in-environment #'load-game)

#||
(defun load-variant (key)
  (let ((var-obj 
  (load "lib/var-vanilla.system")
  (mk:operate-on-system 'vanilla-variant 'compile :verbose nil)
  (format t "~&Variant loaded...~%"))
||#

#+xp-testing	
(defun load-tests ()
  (load "tools/XPTest.system")
  (mk:operate-on-system 'XPTest 'compile :verbose nil)
  
  (load "tests/tests.system")
  (mk:operate-on-system 'lb-test 'compile :verbose nil)
  (format t "~&Tests loaded.~%"))
	
#+xp-testing
(compile-in-environment #'load-tests)


;;(compile-in-environment #'load-vanilla-variant)

(in-package :langband)

#+xp-testing
(defun do-a-test (stage)
  (lb-test::run-lb-test stage :verbose t))

(defun a ()
  ;; to make sure dumps look pretty
  (let ((*package* (find-package :langband))
	#+cmu (extensions:*gc-verbose* nil)
	#+cmu (*compile-print* nil)
	)
    (game-init&)))

#+xp-testing
(do-a-test :pre)
