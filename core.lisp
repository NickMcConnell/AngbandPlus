;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: core.lisp - core classes, generics and functions
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass variant ()
    (
     (id        :accessor variant.id
		:initform :lithping
		:initarg :id)
   
     (name      :accessor variant.name
		:initform "lithping"
		:initarg :name)
   
     (pre-init  :accessor variant.pre-init
		:initform nil
		:initarg :pre-init)
   
     (post-init :accessor variant.post-init
		:initform nil
		:initarg :post-init)
   
     (races     :accessor variant.races
		:initform (make-hash-table :test #'equal)
		:initarg :races)
   
     (classes   :accessor variant.classes
		:initform (make-hash-table :test #'equal)
		:initarg :classes)

     (file-path :accessor variant.file-path
		:initform nil
		:initarg :file-path)

     (floor-features :accessor variant.floor-features
		     :initform (make-hash-table :test #'eql)
		     :initarg :floor-features)

     (room-builders  :accessor variant.room-builders
		     :initform (make-hash-table :test #'eql)
		     :initarg :room-builders)

     (max-charlevel  :accessor variant.max-charlevel
		     :initform 50
		     :initarg :max-charlevel)
     
     (xp-table  :accessor variant.xp-table
		:initarg :xp-table
		;; maybe have a default? or maybe not
		;; it should be an array of size max-charlevel
		:initform nil)
   
     )))


(defun register-variant& (var-obj)
  "Registers a variant-object."

;;  (warn "Trying to run variant ~a" (variant.name var-obj))

  (setf *variant* var-obj))

(defun load-variant-data& (var-obj data-file)
  "Loads variant-data from appropriate directory."

  (let* ((file-path (variant.file-path var-obj))
	 (full-fname (if file-path
			 (concatenate 'string file-path "/" data-file)
			 data-file)))
    (load full-fname)
;;	(error "Unable to find variant data-file ~a" full-fname))
    ))
      