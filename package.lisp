;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: package.lisp - package def for langband

|#

(in-package :cl-user)


(defpackage :org.langband.ffi
  (:use :common-lisp #+clisp :ffi #+sbcl :sb-alien)
  (:nicknames :lb-ffi :langband-ffi))


(defpackage :org.langband.engine
  (:nicknames :lb :langband-engine :lb-engine :engine :langband
	      :org.langband.vanilla :org.langband.contraband) ;; fix later
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
	   #:*engine-version*
	   #:*engine-num-version*

	   #:text-attr
	   #:text-char
	   #:x-attr
	   #:x-char
	   #:text-sym
	   #:gfx-sym
	   
	   #:lbsys/get-current-directory
	   #:lbsys/ensure-dir-name
	   )
  #+lisp2csf
  (:documentation "This is the Langband game package."))


(defpackage :org.langband.datastructures
  (:nicknames :lb-ds)
  (:use :common-lisp)
  (:export #:make-heap
	   #:heap-front
	   #:heap-remove
	   #:heap-insert
	   #:heap-build
	   #:heap-sort

	   #:make-priority-queue
	   #:pq-front
	   #:pq-remove
	   #:pq-insert
	   #:pq-size
	   #:init-pq-pool

	   #:make-queue
	   #:queue-as-list
	   #:enqueue
	   #:dequeue
	   ))
