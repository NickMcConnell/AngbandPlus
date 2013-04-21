;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/tests/tests.asd - testing module
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

(defpackage :lb-xptest-system 
  (:use :cl :asdf))

(in-package :lb-xptest-system)

;;; System definition

(asdf:defsystem lb-xp-test
    :version "0.1"
    :components ((:file "xp-package")
		 (:file "xp-testsuite" :depends-on ("xp-package"))
		 ))



(defpackage :langband-tests-system 
  (:use :cl :asdf))

(in-package :langband-tests-system)

;;; System definition

(asdf:defsystem :lbmodule-tests
    :version "0.0.20"
    :components ((:file "package")
		 (:file "base" :depends-on ("package"))
		 (:file "equal")
		 (:file "core")
		 (:file "stat")
		 (:file "checking")
		 (:file "player")
		 (:file "building")
		 (:file "save")
		 #+never
		 (:file "ffi")
		 (:file "format")
		 (:file "monster"))
    :depends-on (lb-xp-test))
