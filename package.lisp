;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: package.lisp - package def for langband

|#

(in-package :cl-user)


(defpackage :org.langband.ffi
  (:use :common-lisp #+clisp :ffi)
  (:nicknames :lb-ffi :langband-ffi)
  (:export #:+c-null-value+
           #:loc-char-format
           #:to-arr
           #:to-str
           ;; #:str-to-arr
           ))


(defpackage :org.langband.engine
  (:nicknames :lb :lb-engine :langband :org.langband.vanilla) ;; fix later
  (:use :common-lisp
	#-building-ffi-defs :binary-types
	:org.langband.ffi)
  (:export #:game-init&)
  #+lisp2csf
  (:documentation "This is the Langband game package."))

