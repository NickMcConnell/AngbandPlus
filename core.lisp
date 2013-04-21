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
  (defstruct (game-obj-table (:conc-name gobj-table.))
    (obj-table nil)
    (alloc-table nil)
    (obj-table-by-lvl nil)
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass variant ()
    (
     (id        :accessor variant.id
		:initform :lithping
		:initarg :id)
   
     (name      :accessor variant.name
		:initform "lithping"
		:initarg :name)

     (sys-file  :accessor variant.sys-file
		:initform nil
		:initarg :sys-file)

     (file-path :accessor variant.file-path
		:initform nil
		:initarg :file-path)

     ;; the rest can be done lazily
     
     (races     :accessor variant.races
		:initform (make-hash-table :test #'equal)
		:initarg :races)
   
     (classes   :accessor variant.classes
		:initform (make-hash-table :test #'equal)
		:initarg :classes)

     (turn      :accessor variant.turn
		:initform 0
		:initarg :turn)

     (turn-events :accessor variant.turn-events
		  :initform (make-hash-table :test #'equal)
		  :initarg :turn-events)



     (floor-features :accessor variant.floor-features
		     :initform (make-hash-table :test #'eql)
		     :initarg :floor-features)

     (room-builders  :accessor variant.room-builders
		     :initform (make-hash-table :test #'eql)
		     :initarg :room-builders)

     (sort-values    :accessor variant.sort-values
		     :initform (make-hash-table :test #'eql)
		     :initarg :sort-values)

     (max-depth      :accessor variant.max-depth
		     :initform 128
		     :initarg :max-depth)
     
     (max-charlevel  :accessor variant.max-charlevel
		     :initform 50
		     :initarg :max-charlevel)
     
     (xp-table  :accessor variant.xp-table
		:initarg :xp-table
		;; maybe have a default? or maybe not
		;; it should be an array of size max-charlevel
		:initform nil)

     ;; these are just types.. not actual monsters
     (monsters :accessor variant.monsters
	       :initform (make-hash-table :test #'eq)
	       :initarg :monsters)

     (objects :accessor variant.objects
	      :initform (make-hash-table :test #'eq)
	      :initarg :objects)
     
     (filters :accessor variant.filters
	      :initform (make-hash-table :test #'eq)
	      :initarg :filters)
		   
		   
#||     
     (alloc-table-monsters :accessor variant.alloc-table-monsters
			   :initarg :alloc-table-monsters
			   :initform nil)

     (alloc-table-objects  :accessor variant.alloc-table-objects
			   :initarg :alloc-table-objects
			   :initform nil)

     
     ;; monster kind
     (mkind-table :accessor variant.mkind-table
		  :initarg :mkind-table
		  :initform (make-hash-table :test #'equal))

     ;; numeric version.. remove later
     (mkind-table-num :accessor variant.mkind-table-num
		      :initarg :mkind-table-num
		      :initform (make-hash-table :test #'equal))

     (mkind-by-lvl :accessor variant.mkind-by-lvl
		   :initarg :mkind-by-lvl
		   :initform nil)

     (okind-by-lvl :accessor variant.okind-by-lvl
		   :initarg :okind-by-lvl
		   :initform nil)

     ;; object kind
     (okind-table :accessor variant.okind-table
		  :initarg :okind-table
		  :initform (make-hash-table :test #'equal))

     ;; numeric version.. remove later
     (okind-table-num :accessor variant.okind-table-num
		      :initarg :okind-table-num
		      :initform (make-hash-table :test #'equal))
     ||#
     (day-length      :accessor variant.day-length
		      :initarg :day-length
		      :initform 10000)

   
     )))

(defun register-variant& (var-obj)
  "Registers a variant-object."

;;  (warn "Trying to run variant ~a" (variant.name var-obj))
  (setf (get 'variants (variant.id var-obj)) var-obj))


(defun load-variant-data& (var-obj data-file)
  "Loads variant-data from appropriate directory."

  (let* ((file-path (variant.file-path var-obj))
	 (full-fname (if file-path
			 (concatenate 'string file-path "/" data-file)
			 data-file)))
    (load full-fname)
;;	(error "Unable to find variant data-file ~a" full-fname))
    ))

(defun load-variant& (id &key (verbose t))
  "Tries to load a variant."

  (let ((var-obj (get 'variants id)))
    (when (and var-obj (typep var-obj 'variant))
      (let ((sys-file (variant.sys-file var-obj)))
	(when verbose
	  (format t "~&Will try to load variant '~a' in file ~a~%" id sys-file))
	(compile-in-environment
	 #'(lambda ()
	     (load sys-file)
	     (mk:operate-on-system id 'compile :verbose nil)
	     (when verbose
	       (format t "~&Variant '~a' compiled and loaded.~%" id))))
	var-obj))))

	     

(defun get-sort-value (key)
  "Returns a number for the key, or NIL."
  (let ((table (variant.sort-values *variant*)))
    (gethash key table)))

(defun execute-turn-events (var-obj)
  "Executes any turn-events."
  (let* ((turn (variant.turn var-obj))
	 (turn-table (variant.turn-events var-obj))
	 (turn-ev (gethash turn turn-table)))

    (when turn-ev
      (warn "Executing events ~a" turn-ev)
      (remhash turn turn-table))))

(defun add-turn-event (var-obj wanted-turn event)
  "Adds a turn-event."

  (push event (gethash wanted-turn (variant.turn-events var-obj))))


(defun register-monster-filter! (id filter var-obj)
  (push (cons id filter)
	(gethash :monsters (variant.filters var-obj))))

(defun register-object-filter! (id filter var-obj)
  (push (cons id filter)
	(gethash :objects (variant.filters var-obj))))


;;(defun get-monster-filters (type var-obj)
;;  (gethash type (variant.filters var-obj)))


(defun apply-filters-on-obj (type var-obj obj)
  (let ((filters (gethash type (variant.filters var-obj))))
    (dolist (i filters)
      (funcall (cdr i) var-obj obj))))

