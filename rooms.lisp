;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: rooms.lisp - contains room-builders
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with generation of rooms

|#

(in-package :org.langband.engine)


  (defclass room-type ()
    ((id        :accessor room-type.id
		:initarg :id
		:initform nil)
     
     (name      :accessor room-type.name
		:initarg :name
		:initform "room")
     
     (size-mod  :accessor room-type.size-mod
		:initarg :size-mod
		:initform #1A(0 0 0 0 0))
     
     (min-level :accessor room-type.min-level
		:initarg :min-level
		:initform 1)
     ))

  (defclass active-room (activatable)
    ((type      :accessor room.type
		:initarg :type
		:initform nil)
     (loc-x     :accessor location-x
		:initarg :loc-x
		:initform +illegal-loc-x+)
     (loc-y     :accessor location-y
		:initarg :loc-y
		:initform +illegal-loc-y+)))
     
		

  (defgeneric build-room! (room dungeon player where-x where-y)
    (:documentation "Builds given room in the dungeon at [where-x, where-y]."))
  
  (defgeneric find-appropriate-room (variant level player)
    (:documentation "Tries to find an appropriate room-type for given
dungeon."))
  

(defun define-room (id constructor)
  "First argument should be an integer.. fix this later.."
  (assert (or (stringp id) (symbolp id)))
  (assert (functionp constructor))

  (let ((table (variant.room-builders *variant*))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) constructor)))

(defun get-room (id)
  "Returns the constructor to build the given room, or NIL."

  (assert (or (stringp id) (symbolp id)))
  
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.room-builders *variant*)))
    (gethash key table)))

(defmethod find-appropriate-room (variant level player)
  (declare (ignore variant level player))
  (error "find-appropriate-room not implemented."))

(defun construct-room! (room-type dungeon player bx0 by0)
  "Constructs and returns an active-room."
  
  ;;  (declare (ignore player))
  ;;  (warn "Build room ~a ~a" by0 bx0)

  (assert (and (>= bx0 0) (>= by0 0) (< bx0 18) (< by0 6)))

  (let ((returned-room nil))
  
    (block room-construction
      
      (let* (;;(room-builder (get-room-builder num))
	     (room-info (room-type.size-mod room-type))
	     (room-map (dun-data.room-map *cur-dun*))
	     (by1 (+ by0 (svref room-info 0)))
	     (by2 (+ by0 (svref room-info 1)))
	     (bx1 (+ bx0 (svref room-info 2)))
	     (bx2 (+ bx0 (svref room-info 3))))
	
	(when (or (< by1 0)
		  (< bx1 0)
		  (>= by2 (dun-data.row-rooms *cur-dun*))
		  (>= bx2 (dun-data.col-rooms *cur-dun*)))
	  (warn "off the screen...")
	  (return-from room-construction nil))
	
	;; verify open space
	(loop for i from by1 to by2
	      do
	      (loop for j from bx1 to bx2
		    do
		    (when (aref room-map j i)
		      (return-from room-construction nil))))

    
	(let (;;(fun (cdr room-builder))
	      (y (int-/ (* (+ by1 by2 1) +block-height+) 2))
	      (x (int-/ (* (+ bx1 bx2 1) +block-width+) 2)))

	  (build-room! room-type dungeon player x y)

	  (let ((aroom (make-instance 'active-room :type room-type
				      :loc-x x
				      :loc-y y)))
	    (add-room-to-dungeon! dungeon aroom)
	    (setq returned-room aroom))


	  (push (cons x y) (dun-data.room-centres *cur-dun*))

	  ;; reserve space in the room map
      
	  (loop for i from by1 to by2
		do
		(loop for j from bx1 to bx2
		      do
		      (setf (aref room-map j i) t)))

	  ;; skip crowd


	  returned-room)))))
