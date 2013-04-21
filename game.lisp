;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

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

(in-package :cl-user)

;; we want to know where we are
(defun %get-default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (ext:default-directory)
  #+cmucl (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #-(or allegro clisp cmucl cormanlisp lispworks lucid) (truename "."))


(defvar *current-dir* (namestring (%get-default-directory)))

;; add features we need
(eval-when (:execute :load-toplevel :compile-toplevel)

;;  (pushnew :xp-testing *features*)
  (pushnew :langband-development *features*)
;;  #-clisp
;;  (push :using-sound *features*)
  ;; this one should be turned on in releases and in curses
  (pushnew :hide-warnings *features*)

;;  (pushnew :maintainer-mode *features*)
  
  )

(defconstant +defsystem-file+ #+ecl "tools/defsystem.lisp"
	     #-ecl "tools/defsystem")

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

(defun assign-log-path& (log-path name)
  (setf (logical-pathname-translations name)
	(list (list ";**;*.*.*"  (strcat log-path "**/*.*"))
	      (list "**;*.*.*"  (strcat log-path "**/*.*"))
;;	      (list "**;*"     (strcat log-path "**/*")) 
	      (list ";*.*.*"  (strcat log-path "*.*"))
	      (list "*.*.*"  (strcat log-path "*.*")))))

(assign-log-path& *current-dir* "langband")
(assign-log-path& (strcat *current-dir* "tools/") "langband-tools")
(assign-log-path& (strcat *current-dir* "tests/") "langband-tests")
(assign-log-path& (strcat *current-dir* "variants/vanilla/") "langband-vanilla")
;;(assign-log-path& (strcat *current-dir* "lib/foreign/") "langband-foreign")


;;#+(or cmu clisp allegro lispworks)
(progn
  (unless (find-package :make)
    (load +defsystem-file+ :verbose nil)
    ))

(defvar *normal-opt* '(optimize
		       #+cmu (ext:inhibit-warnings 3)
		       (speed 3)
		       (compilation-speed 0)
		       (safety 1)
		       (debug 1)
		       ))

(defvar *dev-opt* '(optimize
		    #+cmu (ext:inhibit-warnings 2)
		    (speed 1)
		    (compilation-speed 2)
		    (safety 2)
		    (debug 3)
		    ))


(proclaim *dev-opt*)
;;(proclaim *normal-opt*)
 
(defun compile-in-environment (func)
  (let (
	#+cmu (*compile-print* nil)
	      #+cmu (*load-verbose* nil)
	      (*load-print* nil)
	      ;;#+cmu (*error-output* o-str)
	      #+cmu (extensions:*gc-verbose* nil)
	      )
    (funcall func)))

(defun progress-msg (msg)
  (format t "~&~a~%" msg))

(defun load-game ()
  "Tries to load the game."
  ;;  (push :langband-debug *features*)

  (load "langband-engine.system")
;;  (print (mk:get-file-order-system 'langband-engine))
;;  (load "lib/foreign/langband-foreign.system")
;;  (mk:operate-on-system 'langband-foreign 'compile :verbose nil)
  (mk:operate-on-system 'langband-engine 'compile :verbose nil)
  (progress-msg "Base engine loaded...")
  
  (load "variants/vanilla/langband-vanilla.system")
  (mk:operate-on-system 'langband-vanilla 'compile :verbose nil)
  (progress-msg "Variant loaded...")
  t)
  
(compile-in-environment #'load-game)

#+xp-testing	
(defun load-tests ()
  (load "tools/XPTest.system")
  (mk:operate-on-system 'XPTest 'compile :verbose nil)
  
  (load "tests/langband-tests.system")
  (mk:operate-on-system 'langband-tests 'compile :verbose nil)
  (progress-msg "Tests loaded."))
	
#+xp-testing
(compile-in-environment #'load-tests)

(in-package :org.langband.engine)

#+xp-testing
(do-a-test :pre)

