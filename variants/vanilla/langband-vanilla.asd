;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: variants/vanilla/langband-vanilla.asdm - another system-def for vanilla
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

;; we need certain flags
(eval-when (:execute :load-toplevel :compile-toplevel)
  #+(or cmu allegro sbcl lispworks)
  (pushnew :enough-support-for-langband *features*)
  #+(and clisp langband-development) ;; clisp not ready in debian yet
  (pushnew :enough-support-for-langband *features*)
  )

(defpackage :langband-vanilla-system 
  (:use :cl :asdf))

(in-package :langband-vanilla-system)

(asdf:defsystem :langband-vanilla
    :version "0.0.19"
    :components ((:file "base")
		 (:file "quirks" :depends-on ("base"))
		 (:file "various" :depends-on ("base"))
		 (:file "rooms" :depends-on ("base"))
		 (:file "levels" :depends-on ("base"))
		 (:file "spells" :depends-on ("base" "various" "quirks"))
		 (:file "wizard")
		 (:file "keys" :depends-on ("wizard" "spells"))
	       )
  :depends-on (langband-engine))

#-enough-support-for-langband
(warn "Langband-Vanilla has not been tested with '~a ~a', skips compilation."
      (lisp-implementation-type)
      (lisp-implementation-version))

