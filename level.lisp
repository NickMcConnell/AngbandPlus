;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: level.lisp - describing a level
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(eval-when (:compile-toplevel :load-toplevel :execute)
 
  (defclass level (activatable)
    (
     (id      :accessor level.id      :initarg :id      :initform 'level)
     (dungeon :accessor level.dungeon :initarg :dungeon :initform nil)
     (rating  :accessor level.rating  :initarg :rating  :initform 0)
     (depth   :accessor level.depth   :initarg :depth   :initform 0)
     ))


  (defclass random-level (level)
    ((id :initform 'random-level)))


  (defclass themed-level (level)
    ((id :initform 'themed-level)))
     

  (defgeneric generate-level! (level player)
    (:documentation "Returns the level-object."))
  
  (defgeneric create-appropriate-level (variant old-level player depth)
    (:documentation "Returns an appropriate level for the given
variant and player."))
  
  (defgeneric level-ready? (level)
    (:documentation "Returns T if the level is ready for use, returns NIL otherwise."))
  
  (defgeneric get-otype-table (level var-obj)
    (:documentation "hack, may be updated later."))
  
  (defgeneric get-mtype-table (level var-obj)
    (:documentation "hack, may be updated later."))

  )

(defmethod generate-level! (level player)
  (declare (ignore level player))
  (warn "The basic GENERATE-LEVEL is not implemented, please pass
a proper LEVEL object.")
  nil)

;; see generate.lisp and variants



(defmethod create-appropriate-level (variant old-level player depth)
  (declare (ignore old-level player depth))
  (error "CREATE-APPROPRIATE-LEVEL not implemented for variant ~a"
	 (type-of variant)))



(defmethod level-ready? (level)
  (declare (ignore level))
  (error "pass a proper level to LEVEL-READY?"))


;;; random levels (see also generate.lisp)

;; a simple builder, register it in your variant as 'random-level
(defun make-random-level-obj ()
  (make-instance 'random-level :depth 0 :rating 0))

(defmethod level-ready? ((level random-level))
  (when (level.dungeon level)
    t))


(defun register-level! (id var-obj)
  (let ((mon-table (make-game-obj-table))
	(obj-table (make-game-obj-table)))
    
    (setf (gobj-table.obj-table mon-table) (make-hash-table :test #'equal))
    (setf (gobj-table.obj-table obj-table) (make-hash-table :test #'equal))
  
    (setf (gethash id (variant.monsters var-obj)) mon-table)
    (setf (gethash id (variant.objects var-obj))  obj-table)
    ))

(defun %get-var-table (var-obj key slot)
  ""
  (let ((id (etypecase key
	      (level (level.id key))
	      (symbol key))))
    (let ((mon-table (slot-value var-obj slot)))
      (when mon-table
	(gethash id mon-table)))))



(defmethod get-otype-table ((level level) var-obj)
  (%get-var-table var-obj level 'objects))

(defmethod get-otype-table ((level (eql 'level)) var-obj)
  (%get-var-table var-obj level 'objects))



(defmethod get-mtype-table ((level level) var-obj)
  (declare (ignore var-obj))
  (error "WRONG MTYPE"))

(defmethod get-mtype-table ((level (eql 'level)) var-obj)
  (declare (ignore var-obj))
  (error "WRONG MTYPE"))


