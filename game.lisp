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
(defun default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (lisp:default-directory)
  #+cmucl (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #-(or allegro clisp cmucl cormanlisp lispworks lucid) (truename "."))


(defvar *current-dir* (namestring (default-directory)))

;; add features we need
(eval-when (:execute :load-toplevel :compile-toplevel)
  
;;  (push :xp-testing *features*)
;;  #-clisp
;;  (push :using-sound *features*)
;;  (push :use-common-ffi *features*)
  #+(or allegro cmu)
  (push :use-callback-from-c *features*)

  #+(or cmu clisp)
  (push :handle-char-as-num *features*)

  #+cmu
  (push :compiler-that-inlines *features*)
  
  )



(defconstant +defsystem-file+ #+ecl "tools/defsystem.lisp"
	     #-ecl "tools/defsystem")

#+clisp
(progn
  (format t "Removing some clisp-warnings.. we hope~%")
  ;;(push (pathname "@lisppath@/") *load-paths*)        
  (setq 
   clos::*gf-warn-on-removing-all-methods* nil
   clos::*warn-if-gf-already-called* nil
   clos::*gf-warn-on-replacing-method* nil
   system::*SOURCE-FILE-TYPES* '(".lisp" ".lsp")))
  

#+ecl
(setq sys:*gc-verbose* nil)

(let ((log-path (concatenate 'string *current-dir* "/**/*")))
  (setf (logical-pathname-translations "langband")
	(list (list "**;*" log-path))))


;;#+(or cmu clisp allegro lispworks)
(progn
  (unless (find-package :make)
    (load +defsystem-file+ :verbose nil)))

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

#+cmu
(setq ext:*gc-verbose* nil
      ext:*byte-compile-default* nil
      *compile-print* nil)

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

(defun quit-game& ()
  "Tries to quit game.."
  #+cmu
  (cl-user::quit)
  #-cmu
  (warn "Can't quit yet.. fix me..")
  (values))


(defun load-shared-lib (&optional (lib "./lib/zterm/liblang_ui.so"))
  "Loads the necessary shared-lib."

  (let ((is-there (probe-file lib)))
    (unless is-there
      (warn "Unable to locate dynamic library ~a, please run 'make'."
	    lib)
      (quit-game&)))
      
  
  #+allegro
  (load lib)
  #+cmu
  (alien:load-foreign lib)
  #+clisp
  nil
  #-(or cmu allegro clisp)
  (warn "Did not load shared-library.."))

(defun load-game ()
  "Tries to load the game."
  (load-shared-lib)
  ;;  (push :langband-debug *features*)
  (load "langband.system")
  (mk:operate-on-system 'langband 'compile :verbose t)
  (format t "~&Base engine loaded...~%"))

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

#||
(trace lb::pqsetdblogin)

(lb::pqsetdblogin "" nil nil nil "s14" "stig"
		  "heihei")
||#

#+xp-testing
(do-a-test :pre)

;;(trace c-term-queue-char!)
