;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: tests/package.lisp - package def for langband-test

|#

(in-package :cl-user)

(defpackage :langband-test
  (:nicknames :lb-test)
  (:use :lisp :langband :xp-test)
;;  (:export #:game-init&)
  #+lisp2csf
  (:documentation "This is the testing of the Langband code."))
