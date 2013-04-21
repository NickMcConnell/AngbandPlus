;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: game.lisp - simple load of the game
Copyright (c) 2000-2003 - Stig Erik Sandø

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
  #+cmu (ext:default-directory)
;;  #+sbcl (sb-ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #+sbcl (truename ".")
  #-(or allegro sbcl clisp cmu cormanlisp lispworks lucid) (truename "."))


(defvar *current-dir* (namestring (%get-default-directory)))

;;(defvar *variant-to-load* :contraband)
(defvar *variant-to-load* :vanilla)
;;(defvar *variant-to-load* :both)

;; add features we need
(eval-when (:execute :load-toplevel :compile-toplevel)

;;  (pushnew :xp-testing *features*)
  (pushnew :langband-development *features*)

  ;; should be on in a released source
  (pushnew :langband-release *features*)

;;  (pushnew :maintainer-mode *features*)
  #+(or cmu allegro sbcl lispworks clisp openmcl)
  (pushnew :use-asdf *features*)
  
;;  #+(or clisp)
;;  (pushnew :use-mkdefsystem *features*)
  )

#-use-asdf
(error "Your lisp-system is not supported, make it use ASDF and edit game.lisp.")

(defvar *normal-opt* '(optimize
		       #+cmu (ext:inhibit-warnings 3)
		       (speed 3)
		       (compilation-speed 0)
		       (safety 1)
		       (debug 1)
		       #+lispworks (fixnum-safety 3)
		       ))

(defvar *dev-opt* '(optimize
		    #+cmu (ext:inhibit-warnings 2)
		    #+sbcl (sb-ext:inhibit-warnings 2)
		    (speed 1)
		    (compilation-speed 2)
		    (safety 3)
		    (debug 3)
		    #+lispworks (fixnum-safety 3)
		    ))

#+langband-release
(proclaim *normal-opt*)
#-langband-release
(proclaim *dev-opt*)

(defvar *asdf-file*
  #+(or ecl cormanlisp) "tools/asdf.lisp"
  #-(or ecl cormanlisp) "tools/asdf")

#+use-asdf
(unless (find-package :asdf)
  #+lispworks
  (compile-file *asdf-file* :verbose nil)
  (load *asdf-file* :verbose nil)
  )


;; hack!
#+(and cmu use-asdf)
(setf *default-pathname-defaults* (%get-default-directory))
#+(and clisp use-asdf)
(setf *default-pathname-defaults* (%get-default-directory))

 
(defun compile-in-environment (func)
  (let (
	#+(or cmu lispworks sbcl) (*compile-print* nil)
	  #+lispworks (*compile-verbose* nil)
	  #+(or cmu lispworks) (*load-verbose* nil)
	  (*load-print* nil)
	  ;; #+cmu (*error-output* o-str)
	  #+cmu (extensions:*gc-verbose* nil)
	  ;; #+sbcl (sb-ext:*gc-verbose* nil)
	  )
    (funcall func)))

(defun progress-msg (msg)
  (format t "~&~a~%" msg))

#+use-asdf
(defun load-game ()
  "Tries to load the game asdf-style."
  (let ((asdf:*central-registry* (list *default-pathname-defaults* "variants/vanilla/"
				       "variants/contraband/"))
	;;#+lispworks ;; possibly others too
	(asdf::*compile-file-failure-behaviour* :ignore)
	(asdf::*compile-file-warnings-behaviour* :ignore)
	(var *variant-to-load*)
	;;(var :vanilla)
	;;(var :both)
	)
    
    (load "langband-engine.asd")

    (when (or (eq var :contraband) (eq var :both))
      (load "modules/dialogue/dialogue.asd")
      (load "variants/contraband/contraband.asd")
      (asdf:oos 'asdf:load-op :contraband))
    
    (when (or (eq var :vanilla) (eq var :both))
      (load "variants/vanilla/langband-vanilla.asd")
      (asdf:oos 'asdf:load-op :langband-vanilla))

    (progress-msg "Variants loaded...")
    t))


(compile-in-environment #'load-game)


#||
(defvar *defsystem-file*
  #+(or ecl cormanlisp) "tools/defsystem.lisp"
  #-(or ecl cormanlisp) "tools/defsystem")

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

#+use-mkdefsystem
(defun assign-log-path& (log-path name)

  (setf (logical-pathname-translations name)
	(list (list ";**;*.*.*"  (strcat log-path "**/*.*"))
	      (list "**;*.*.*"  (strcat log-path "**/*.*"))
;;	      (list "**;*"     (strcat log-path "**/*")) 
	      (list ";*.*.*"  (strcat log-path "*.*"))
	      (list "*.*.*"  (strcat log-path "*.*"))))
  )

#+use-mkdefsystem
(progn
  ;; make some logical paths for later loading
  (assign-log-path& *current-dir* "langband")
  (assign-log-path& (strcat *current-dir* "tools/") "langband-tools")
  (assign-log-path& (strcat *current-dir* "tests/") "langband-tests")
  (assign-log-path& (strcat *current-dir* "variants/vanilla/") "langband-vanilla")
  (assign-log-path& (strcat *current-dir* "variants/vanilla/tests/") "vanilla-tests")
  ;;(assign-log-path& (strcat *current-dir* "lib/foreign/") "langband-foreign")
  )

#+use-mkdefsystem
(unless (find-package :make)
  (load *defsystem-file* :verbose nil))



#+use-mkdefsystem
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




#+xp-testing	
(defun load-tests ()
  
  #+use-asdf
  (let (;;(asdf:*central-registry* (list *default-pathname-defaults* "tools/" "tests/"))
	(asdf:*compile-file-warnings-behaviour* :ignore))
    (load "tools/xp-test.asd")
    (load "tests/langband-tests.asd")
    (load "variants/vanilla/tests/vanilla-tests.asd")
    (asdf:oos 'asdf:load-op :vanilla-tests)
    )

  #+use-mkdefsystem
  (progn
    (load "tools/XPTest.system")
    (mk:operate-on-system 'XPTest 'compile :verbose nil)
    
    (load "tests/langband-tests.system")
    (mk:operate-on-system 'langband-tests 'compile :verbose nil)
    
    (load "variants/vanilla/tests/vanilla-tests.system")
    (mk:operate-on-system 'vanilla-tests 'compile :verbose nil))
  
  #-(or use-asdf use-mkdefsystem)
  (progn
    (warn "Tests cannot be loaded without ASDF or MK:DEFSYSTEM"))
  
  (progress-msg "Tests loaded."))
	
#+xp-testing
(compile-in-environment #'load-tests)

(in-package :org.langband.engine)

#+xp-testing
(do-a-test :pre)

;; hidden thing to print out lists
(defun fil (lst tp)
  (loop for i in lst do
	(when (typep i tp) (format t "~&~a~%" (object.name i)))))
||#
