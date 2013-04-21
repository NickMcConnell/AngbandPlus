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
(setq *load-local-names-info* t)

(defun compile-in-environment (func)
  (let (#+cmu (*compile-print* nil)
	      ;;(*load-verbose* nil)
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
    

(defun load-vanilla-variant ()
  (load "lib/var-vanilla.system")
  (mk:operate-on-system 'vanilla-variant 'compile :verbose nil)
  (format t "~&Variant loaded...~%"))

(compile-in-environment #'load-game)
(compile-in-environment #'load-vanilla-variant)

(in-package :langband)

(defun a ()
  ;; to make sure dumps look pretty
  (let ((*package* (find-package :langband))
	#+cmu (extensions:*gc-verbose* nil)
	#+cmu (*compile-print* nil)
	)
    (game-init&)))
