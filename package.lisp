;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: package.lisp - package def for langband

|#

(in-package :cl-user)


(defpackage :langband-ffi
  (:use :common-lisp #+clisp :ffi))

(defpackage :langband
  (:nicknames :lb)
  (:use :common-lisp :langband-ffi)
  (:export #:game-init&)
  #+lisp2csf
  (:documentation "This is the Langband game package."))

 

