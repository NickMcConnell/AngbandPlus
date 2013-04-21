;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: langband-engine.system - another system-def for vanilla
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

;; we need certain flags
(eval-when (:execute :load-toplevel :compile-toplevel)
  #+(or allegro cmu sbcl lispworks)
  (pushnew :use-callback-from-c *features*)

;;  #+(or cmu clisp sbcl)
  (pushnew :handle-char-as-num *features*) ;; always

  #+(or cmu allegro lispworks sbcl)
  (pushnew :enough-support-for-langband *features*)
  #+(and clisp langband-development) ;; clisp not ready in debian yet
  (pushnew :enough-support-for-langband *features*)

  ;; this one should be turned on when maintainer is debugging
;;  (pushnew :langband-extra-checks *features*)
  
  #-langband-development
  (pushnew :hide-warnings *features*)
  
  #+(or cmu sbcl lispworks)
  (pushnew :compiler-that-inlines *features*)
  
  )

(defpackage :langband-engine-system 
  (:use :cl :asdf))

(in-package :langband-engine-system)
  
(asdf:defsystem :langband-engine
    :version "0.1.0"
    :components
    ((:module settings
	      :pathname ""
	      :components ((:file "pre-build")))
     
     (:module binary-types
	      :pathname "binary-types/"
	      :components ((:file "binary-types"))
	      :depends-on (settings))
     
     (:module decl
              :pathname ""
              :components ((:file "package"))
	      :depends-on (binary-types))

     (:module foreign
	      :pathname "ffi/"
	      :components ((:file "ffi-load")
			   (:file "ffi-sys")
			   #+cmu
			   (:file "ffi-cmu")
			   #+sbcl
			   (:file "ffi-sbcl")
			   #+allegro
			   (:file "ffi-acl")
			   #+lispworks
			   (:file "ffi-lw")
			   #+clisp
			   (:file "ffi-clisp"))
	      :depends-on (decl))

     ;; fix remaining dependency-problems as they show up
     (:module basic
              :pathname ""
              :components ((:file "memoize")
			   (:file "base" :depends-on ("memoize"))
			   (:file "constants" :depends-on ("base"))
			   (:file "generics")
			   (:file "sys")
			   (:file "classes" :depends-on ("generics" "constants"))
			   (:file "global" :depends-on ("classes" "generics" "base" "constants" "sys"))
			   (:file "sound")
			   (:file "character" :depends-on ("classes" "global"))
			   (:file "object" :depends-on ("classes" "generics" "global"))
			   (:file "equipment" :depends-on ("global" "object"))
			   (:file "player" :depends-on ("classes" "global" "character" "equipment"))
			   (:file "monster" :depends-on ("classes" "global"))
			   (:file "dungeon" :depends-on ("base" "monster" "classes" "constants"))
			   (:file "building" :depends-on ("generics" "base" "global" "dungeon"))
			   (:file "stores" :depends-on ("building" "generics" "equipment"))
			   (:file "allocate" :depends-on ("generics" "dungeon" "constants"))
			   (:file "generate" :depends-on ("dungeon" "allocate" "classes" "object" "equipment" "generics"))
			   (:file "print" :depends-on ("generics" "player"))
			   (:file "util" :depends-on ("dungeon" "classes" "global" "generics" "generate"))
			   (:file "combat" :depends-on ("generics" "base" "sound" "global" "classes"))
			   (:file "keys" :depends-on ("base" "dungeon" "constants"))
			   (:file "actions" :depends-on ("generics" "util" "keys" "generate" "combat"))
			   (:file "view" :depends-on ("dungeon" "generics" "constants"))
			   (:file "project" :depends-on ("base" "generics" "player" "object" "dungeon" "combat"))
			   (:file "save" :depends-on ("player" "dungeon" "classes" "global" "generics"))
			   (:file "load" :depends-on ("save"))
			   (:file "death" :depends-on ("global" "player" "sys" "character"))
			   (:file "ai" :depends-on ("generics" "monster" "project"))
			   (:file "loop" :depends-on ("classes" "player" "sys" "death" "ai" "print" "view"))
			   (:file "birth" :depends-on ("generics" "constants" "classes" "player" "loop"))
			   (:file "dump" :depends-on ("monster" "classes" "object" "character" "global" "building" "equipment"))
			   (:file "init" :depends-on ("classes" "monster" "object" "loop"))
			   (:file "verify" :depends-on ("player" "global" "base" "dungeon" "monster" "character"))
			   			   
			   )
	      :depends-on (foreign))
     
     ;; this can safely be commented all out, or parts of it may be commented out
     #+never
     (:module compat
	      :pathname "lib/compat/"
	      :components (
			   #+never
			   (:file "ego")

			   (:file "savefiles")
			   )
	      :depends-on (basic))
     ))


#-enough-support-for-langband
(warn "Langband-Engine has not been tested with '~a ~a', skips compilation."
      (lisp-implementation-type)
      (lisp-implementation-version))
