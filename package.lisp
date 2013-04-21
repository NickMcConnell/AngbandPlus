;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: package.lisp - package def for langband

|#

(in-package :cl-user)


(defpackage :org.langband.ffi
  (:use :common-lisp #+clisp :ffi #+sbcl :sb-alien)
  (:nicknames :lb-ffi :langband-ffi)
  (:export #:+c-null-value+
           #:loc-char-format
           #:to-arr
           #:to-str
           ;; #:str-to-arr
           ))


(defpackage :org.langband.engine
  (:nicknames :lb :lb-engine :engine :langband :org.langband.vanilla) ;; fix later
  (:use :common-lisp
	#-building-ffi-defs :binary-types
	:org.langband.ffi)
  (:export #:game-init&
	   #:get-loadable-form
	   #:get-monster-list

	   ;; unsorted added on need
	   #:gval.dmg-modifier
	   #:gval.ac-modifier
	   #:aobj.game-values
	   #:aobj.number
	   #:write-obj-description
	   #:create-aobj-from-id
	   #:get-object-kind
	   #:get-monster-kind
	   #:produce-active-monster
	   #:produce-active-object
	   #:active-monster
	   #:active-object
	   #:report-equal
	   #:*level*
	   #:*variant*
	   )
  #+lisp2csf
  (:documentation "This is the Langband game package."))

