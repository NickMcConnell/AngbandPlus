;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/compat/compat.asd - compatibility module
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

(defpackage :langband-compat-system 
  (:use :cl :asdf))

(in-package :langband-compat-system)

(asdf:defsystem :lbmodule-compat
    :version "0.1.4"
    :components ((:file "ego")
		 (:file "savefiles")
		 (:file "flavours")
		 (:file "floor")
		 (:file "monster")
		 (:file "obj-kind"))
    :depends-on (langband-engine))

