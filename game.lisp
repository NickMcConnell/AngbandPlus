;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: game.lisp - simple load of the game
Copyright (c) 2000-2002 - Stig Erik Sandø

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

;; add features we need
(eval-when (:execute :load-toplevel :compile-toplevel)

;;  (pushnew :xp-testing *features*)
  (pushnew :langband-development *features*)

  ;; should be on in a released source
  (pushnew :langband-release *features*)
  
  ;; this one should be turned on in releases and in curses
  #+(or win32 langband-release)
  (pushnew :hide-warnings *features*)

;;  (pushnew :maintainer-mode *features*)
  #+(or cmu allegro sbcl lispworks)
  (pushnew :use-asdf *features*)
  
  #+(or clisp)
  (pushnew :use-mkdefsystem *features*)
  )

(defvar *defsystem-file*
  #+(or ecl cormanlisp) "tools/defsystem.lisp"
  #-(or ecl cormanlisp) "tools/defsystem")

(defvar *asdf-file*
  #+(or ecl cormanlisp) "tools/asdf.lisp"
  #-(or ecl cormanlisp) "tools/asdf")

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

#+use-asdf
(unless (find-package :asdf)
  (load *asdf-file* :verbose nil))

;; hack!
#+(and cmu use-asdf)
(setf *default-pathname-defaults* (%get-default-directory))

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
  (let ((asdf:*central-registry* (list *default-pathname-defaults* "variants/vanilla/"))
	;;#+lispworks ;; possibly others too
	(asdf::*compile-file-failure-behaviour* :ignore)
	(asdf::*compile-file-warnings-behaviour* :ignore))
    (load "langband-engine.asd")
    (load "variants/vanilla/langband-vanilla.asd")
    ;;(load "variants/contraband/contraband.asd")
    (asdf:oos 'asdf:load-op :langband-vanilla)
    ;;(asdf:oos 'asdf:load-op :contraband)
    (progress-msg "Variants loaded...")
    t))

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

#-(or use-mkdefsystem use-asdf)
(defun load-game ()
  (labels ((%cl (x)
	     (let* ((fname (strcat x ".lisp"))
		    (cfname (compile-file-pathname fname))
		    (*load-print* nil))
	       (if (probe-file cfname)
		   (load cfname)
		   (progn
		     (compile-file fname)
		     (load cfname)))))
	   (c-files (&rest files)
	     (dolist (i files)
	       (%cl i))))
    
    (%cl "pre-build")
    (%cl "binary-types/binary-types")
    (%cl "package")
    (%cl "ffi/ffi-load")
    (%cl "ffi/ffi-sys")
    #+cormanlisp
    (%cl "ffi/ffi-corman")
    #+lispworks
    (%cl "ffi/ffi-lw")

    (c-files "memoize" "base" "constants" "generics"
	     "sys" "classes" "parameters" "global" 
	     "sound" "character" "object"
	     "equipment" "player" "monster"
	     "dungeon" "building" "stores" "allocate"
	     "generate" "print" "util"
	     "combat" "keys" "actions" "view"
	     "project" "save" "load" "death"
	     "birth" "ai" "loop" "dump" "init" "verify")
    (progress-msg "Base engine loaded...")

    (map nil #'(lambda (x)
		 (%cl (strcat "variants/vanilla/" x)))
	 (list
	  "base" "quirks" "various" "rooms"
	  "levels" "spells" "wizard" "keys"
	  "verify"))

    (progress-msg "Variant loaded...")
    t))


(compile-in-environment #'load-game)


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
