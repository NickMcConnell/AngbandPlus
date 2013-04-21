;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: building.lisp - code which deals with buildings
Copyright (c) 2000-2001 - Stig Erik Sand�

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass house (activatable)
    ((id     :accessor house.id     :initform nil :initarg :id)
     (name   :accessor house.name   :initform nil :initarg :name)
     (x-attr :accessor house.x-attr :initform nil :initarg :x-attr)
     (x-char :accessor house.x-char :initform nil :initarg :x-char)

     (owner  :accessor house.owner :initform nil :initarg :owner)
     ;; the current items
     (items  :accessor house.items :initform nil :initarg :items)
     ))

  (defclass owner ()
    ((id         :accessor owner.id         :initform nil :initarg :id)
     (name       :accessor owner.name       :initform nil :initarg :name))
    ))


(defgeneric build-house! (level house topleft-x topleft-y &key)
  (:documentation "Builds a house on the given level at given coord."))

(defgeneric find-owner-for-house (level house &key)
  (:documentation "Tries to find an appropriate owner for the house."))

(defmethod find-owner-for-house (level house &key)
  nil)

(defmethod activate-object ((obj house) &key (owner nil))
  "Wakes a house(-type) from slumber and returns a usable house."

  (when (and owner (typep owner 'owner))
    (setf (house.owner obj) owner))
    
;;  (warn "activating object ~a" obj)
  
  obj)


(defun establish-house& (var-obj house &key (house-key nil))
  "Establish a house as available type.  Returns NIL
on failure, and the house on success."

  (when (and var-obj house
	     (typep var-obj 'variant)
	     (typep house 'house))
    
    (let ((table (variant.house-types var-obj))
	  (house-id (if house-key
			house-key
			(house.id house))))

      (setf (gethash house-id table) house)
      
      house)))


(defun establish-owner& (var-obj owner)
  "Establish an owner as available. Returns NIL on
failure and owner on success."
  
  (when (and var-obj owner
	     (typep var-obj 'variant)
	     (typep owner 'owner))
    
    (let ((table (variant.house-owners var-obj))
	  (owner-id (owner.id owner)))
      
      (setf (gethash owner-id table) owner)
      
      owner)))


(defun get-house (id &optional (var-obj *variant*))
  "Returns a house-type (non-activated) or NIL."

  (when (and var-obj id
	     (typep var-obj 'variant))
    
    (let ((table (variant.house-types var-obj)))
      
      (gethash id table))))


(defun get-owner (id &optional (var-obj *variant*))
  "Returns an owner (non-activated) or NIL."
  
  (when (and var-obj id
	     (typep var-obj 'variant))
    
    (let ((table (variant.house-owners var-obj)))
      
      (gethash id table))))



(defmethod print-object ((inst house) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A ~A]" (class-name (class-of inst))
	   (slot-value inst 'name)
	   (slot-value inst 'id)
	   (slot-value inst 'owner)
	   ))
	   
  inst)


(defmethod print-object ((inst owner) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A]" (class-name (class-of inst))
	   (slot-value inst 'name)))
	   
  inst)

(defmethod build-house! (level (house house) topleft-x topleft-y
			       &key
			       (door-feature nil)
			       (door-trigger nil)
			       )

  (when level
;;    (warn "building house ~a on level ~a at [~a,~a]" house level topleft-x topleft-y)

    (let* ((dungeon (level.dungeon level))
	   (y0 topleft-y)
	   (x0 topleft-x)
	   (y1 (- y0 (randint 3)))
	   (y2 (+ y0 (randint 3)))
	   (x1 (- x0 (randint 5)))
	   (x2 (+ x0 (randint 5))))

      (loop for y from y1 to y2 do
	    (loop for x from x1 to x2 do
		  (setf (cave-feature dungeon x y) +feature-perm-extra+)))
      

     ;; add doors
     (let ((tmp (random 4))
	   (x 0)
	   (y 0))

       ;; skip relocating annoying doors
       
       (case tmp
	 ;; bottom
	 (0 (setq y y2
		  x (rand-range x1 x2)))
	 ;; top
	 (1 (setq y y1
		  x (rand-range x1 x2)))
	 ;; right
	 (2 (setq y (rand-range y1 y2)
		  x x2))
	 ;; left
	 (3 (setq y (rand-range y1 y2)
		  x x1))
	 
	 (t
	  (warn "Fall-through in door placement")
	  (setq y y2
		x x2)))

       ;; time to place house number
       (when door-feature
	 (setf (cave-feature dungeon x y) door-feature))
       
       (when door-trigger
	 (setf (get-coord-trigger dungeon x y) door-trigger))
       
       ))

    house))