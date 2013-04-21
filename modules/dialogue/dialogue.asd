;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/dialogue/dialogue.asd - system-def for dialogue module
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

(defpackage :langband-dialogue-system 
  (:use :cl :asdf))

(in-package :langband-dialogue-system)

(asdf:defsystem :lbmodule-dialogue
    :version "0.1.4"
    :components ((:file "base")
		 (:file "conversation" :depends-on ("base")))
    :depends-on (langband-engine))

