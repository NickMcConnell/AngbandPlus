;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: variants/vanilla/langband-vanilla.asd - another system-def for vanilla
Copyright (c) 2001-2003 - Stig Erik Sandø

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
    :version "0.1.5"
    :components ((:file "package")
		 (:file "constants" :depends-on ("package"))
		 (:file "base" :depends-on ("constants"))
		 (:file "quirks" :depends-on ("base"))
		 (:file "skills" :depends-on ("quirks"))
		 (:file "player" :depends-on ("skills"))
		 (:file "save" :depends-on ("player"))
		 (:file "levels" :depends-on ("base"))
		 (:file "various" :depends-on ("base" "quirks" "player" "levels"))
		 (:file "rooms" :depends-on ("base"))
		 (:file "combat" :depends-on ("player"))
		 (:file "spells" :depends-on ("base" "various" "quirks"))
		 (:file "effects" :depends-on ("spells"))
		 (:file "objects" :depends-on ("base" "spells"))
		 (:file "ego" :depends-on ("objects"))
		 (:file "monsters" :depends-on ("base" "spells" "effects"))
		 (:file "print" :depends-on ("base" "player" "spells"))
		 (:file "wizard" :depends-on ("spells"))
		 (:file "keys" :depends-on ("wizard" "combat" "spells" "ego" "monsters"))
		 (:file "verify" :depends-on ("various"))
	       )
  :depends-on (langband-engine))

#-enough-support-for-langband
(warn "Langband-Vanilla has not been tested with '~a ~a', skips compilation."
      (lisp-implementation-type)
      (lisp-implementation-version))

