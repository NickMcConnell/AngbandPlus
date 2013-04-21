;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: level.lisp - describing a level
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defclass level (activatable)
  ((id      :accessor level.id      :initarg :id      :initform "level")
   (symbol  :accessor level.symbol  :initarg :symbol  :initform 'level)
   (dungeon :accessor level.dungeon :initarg :dungeon :initform nil)
   (rating  :accessor level.rating  :initarg :rating  :initform 0)
   (depth   :accessor level.depth   :initarg :depth   :initform 0))
  (:documentation "A representation of a level.  Meant to be subclassed."))


(defclass random-level (level)
  ((id     :initform "random-level")
   (symbol :initform 'random-level)))
   


(defclass themed-level (level)
  ((id :initform "themed-level")
   (symbol :initform 'themed-level)))
   
 
;; a simple builder, register it in your variant as 'random-level
(defun make-random-level-obj ()
  (make-instance 'random-level :depth 0 :rating 0))

(defmethod level-ready? ((level random-level))
  (when (level.dungeon level)
    t))


(defmethod register-level! ((var-obj variant) (id string) &key object-filter monster-filter &allow-other-keys)
;;  (assert (not (eq nil var-obj)))
;;  (assert (symbolp id))
;;  #+langband-extra-checks
;;  (assert (ok-object? var-obj))

  (let ((mon-table (make-game-obj-table))
	(obj-table (make-game-obj-table)))
    
    (setf (gobj-table.obj-table mon-table) (make-hash-table :test #'equal)
	  (gobj-table.obj-table obj-table) (make-hash-table :test #'equal))

    (setf (gethash id (variant.monsters-by-level var-obj)) mon-table
	  (gethash id (variant.objects-by-level var-obj))  obj-table)

    ;; fix
    (when object-filter
      (if (not (functionp object-filter))
	  (lang-warn "Object-filter ~s for level ~s is not a function." object-filter id)
	  (pushnew (cons id object-filter)
		   (gethash :objects (variant.filters var-obj))
		   :key #'car)))
    
    (when monster-filter
      (if (not (functionp monster-filter))
	  (lang-warn "Monster-filter ~s for level ~s is not a function." monster-filter id)
	  (pushnew (cons id monster-filter)
		   (gethash :monsters (variant.filters var-obj))
		   :key #'car)))
    
    ))

(defun %get-var-table (var-obj key slot)
  ""
  (let ((id (etypecase key
	      (level (level.id key))
	      (string key))))
    (let ((mon-table (slot-value var-obj slot)))
      (when mon-table
	(gethash id mon-table)))))
