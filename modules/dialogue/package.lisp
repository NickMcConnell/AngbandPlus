;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/dialogue/package.lisp - package def for dialogue module

|#

(in-package :cl-user)

(defpackage :org.langband.dialogue
  (:nicknames :lb-dialogue :dialogue)
  (:use :common-lisp :org.langband.engine)
  (:export #:interactive-start-conversation
	   #:define-conversation
	   ))
