;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: core.lisp - core classes, generics and functions
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)
  
  (defstruct (game-obj-table (:conc-name gobj-table.))
    (obj-table nil)
    (alloc-table nil)
    (obj-table-by-lvl nil))


  (defclass variant (activatable)
    ((id        :accessor variant.id
		:initform :lithping
		:initarg :id)
   
     (name      :accessor variant.name
		:initform "lithping"
		:initarg :name)

     (sys-file  :accessor variant.sys-file ;; used for?
		:initform nil
		:initarg :sys-file)

     (config-path :accessor variant.config-path ;; where is the configuration-files?
		  :initform nil
		  :initarg :config-path)

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


     ;; a level builder is a constructor that must be funcalled
     ;; the key is the level-id
     (level-builders :accessor variant.level-builders
		     :initform (make-hash-table :test #'equal)
		     :initarg :level-builders)

     (floor-features :accessor variant.floor-features
		     :initform (make-hash-table :test #'eql)
		     :initarg :floor-features)

     (room-builders  :accessor variant.room-builders
		     :initform (make-hash-table :test #'equal)
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
     
     (flavour-types :accessor variant.flavour-types
		    :initform nil
		    :initarg :flavour-types)

     (house-types :accessor variant.house-types
		  :initform (make-hash-table)
		  :initarg :store-types)
     
     (house-owners :accessor variant.house-owners
		   :initform (make-hash-table)
		   :initarg :store-owners)

     (skill-translations :accessor variant.skill-translations
			 :initform nil
			 :initarg :skill-translations)

     (attk-descs :accessor variant.attk-descs
		 :initform (make-hash-table :test #'eq)
		 :initarg :attk-descs)

     (day-length      :accessor variant.day-length
		      :initarg :day-length
		      :initform 10000)
     
     ))

  (defgeneric initialise-monsters& (variant &key &allow-other-keys)
    (:documentation "Initialises monsters for the given variant."))
  
  (defgeneric initialise-features& (variant &key &allow-other-keys)
    (:documentation "Initialises features for the given variant."))
  
  (defgeneric initialise-objects& (variant &key &allow-other-keys)
    (:documentation "Initialises objects for the given variant."))

  (defgeneric calculate-score (variant player)
    (:documentation "Calculates the score for the player based on the variant's
scoring-system."))

 
(defmethod initialise-monsters& (variant &key)
  (error "No INIT-MONSTERS for ~s" (type-of variant)))
  
(defmethod initialise-features& (variant &key)
  (error "No INIT-FEATURES for ~s" (type-of variant)))

(defmethod initialise-objects& (variant &key)
  (error "No INIT-OBJECTS for ~s" (type-of variant)))


(defun register-variant& (var-obj)
  "Registers a variant-object."

;;  (warn "Trying to run variant ~a" (variant.name var-obj))
  (setf (get 'variants (variant.id var-obj)) var-obj))

(defun variant-data-fname (var-obj data-fname)
  "Returns a full pathname for data."
  (let ((file-path (variant.config-path var-obj)))
    (if file-path
	(concatenate 'string file-path "/" data-fname)
	data-fname)))


(defun load-variant-data& (var-obj data-file)
  "Loads variant-data from appropriate directory."

  (let ((fname (variant-data-fname var-obj data-file)))
    (load fname)))


(defun load-variant& (id &key (verbose t))
  "Tries to load a variant."
  (declare (ignore verbose))
  (let ((var-obj (get 'variants id)))
    (when (and var-obj (typep var-obj 'variant))
      var-obj)))
#||
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
||#
	     

(defun get-sort-value (key)
  "Returns a number for the key, or NIL."
  (let ((table (variant.sort-values *variant*)))
    (gethash key table)))

(defun execute-turn-events! (var-obj)
  "Executes any turn-events."
  (let* ((turn (variant.turn var-obj))
	 (turn-table (variant.turn-events var-obj))
	 (turn-ev (gethash turn turn-table)))

    (when turn-ev
      (warn "Executing events ~a" turn-ev)
      (remhash turn turn-table))))

(defun register-turn-event! (var-obj wanted-turn event)
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

(defun register-sorting-values& (var-obj sort-values)
  "The SORT-VALUES are a list where the CARTS are CONSes. In
each such CONS the CAR is an appropriate key and the CDR is
the sorting-value which is a positive integer, lowest numbers
are sorted first.  Returns nothing."

  (let ((table (variant.sort-values var-obj)))
    (dolist (i sort-values)
      (let ((key (car i))
	    (sort-val (cdr i)))
	(setf (gethash key table) sort-val)))
    (values)))

(defun get-level-builder (id &optional (var-obj *variant*))
  "Returns a level-builder or nil."
  (assert (or (symbolp id) (stringp id)))
  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (gethash key table)))

(defun register-level-builder! (id builder &optional (var-obj *variant*))
  "Registers a level-builder which must be a function."
  (assert (or (symbolp id) (stringp id)))
  (assert (functionp builder))

  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) builder)))

