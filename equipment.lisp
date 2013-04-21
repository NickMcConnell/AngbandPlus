;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: equipment.lisp - code for any equipment in all containers.
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass item-table ()
    ((cur-size :accessor items.cur-size :initarg :cur-size :initform 0))
    (:documentation "abstract interface for all item-tables."))

  (defclass items-on-floor (item-table)
    ((obj-list :accessor items.objs
	       :initform nil)
     (dungeon  :accessor items.dun
	       :initarg :dungeon
	       :initform nil)
     (loc-x    :accessor location-x
	       :initarg :loc-x
	       :initform +illegal-loc-x+) ;; invalid value
     (loc-y    :accessor location-y
	       :initarg :loc-y
	       :initform +illegal-loc-y+))
    
    (:documentation "Represents the items on the floor."))

  (defclass items-in-container (item-table)
    ((obj-arr  :accessor items.objs     :initarg :objs     :initform nil)
     (max-size :accessor items.max-size :initarg :max-size :initform 5))
    (:documentation "A container for other objects, ie a backpack."))

  (defclass items-worn (item-table)
    ((obj-arr  :accessor items.objs     :initarg :objs     :initform nil))
    (:documentation "What is worn."))  

  (defclass items-in-house (items-in-container)
    ((max-size :initform 24))
    (:documentation "What is in a house."))
  
  (defclass items-in-store (items-in-house)
    ()
    (:documentation "What is in a store."))

  )


(defgeneric item-table-add!       (table obj &optional key))  
(defgeneric item-table-remove!    (table key))
(defgeneric item-table-clean!     (table))
(defgeneric item-table-find       (table key))
(defgeneric item-table-sort!      (table sorter))
(defgeneric item-table-iterate!   (table function)
  (:documentation "Function should take three arguments in order:
the table, the key and the object itself."))

(defgeneric item-table-verify-key (table key)
  (:documentation "Returns T when key is OK, and NIL when it is not."))

(defgeneric item-table-print (table &key show-pause start-x start-y)
  (:documentation "Returns T when key is OK, and NIL when it is not."))

;;; -----------------------------

(defmethod item-table-add! (table obj &optional key)
  (declare (ignore obj key))
  (error "add not implemented for ~a" (type-of table)))

(defmethod item-table-remove! (table key)
  (declare (ignore key))
  (warn "remove not implemented for ~a" (type-of table)))

(defmethod item-table-clean! (table)
  (warn "clean not implemented for ~a" (type-of table)))

(defmethod item-table-find (table key)
  (declare (ignore key))
  (warn "find not implemented for ~a" (type-of table)))

(defmethod item-table-sort! (table sorter)
    (declare (ignore sorter))
  (warn "sort not implemented for ~a" (type-of table)))

(defmethod item-table-iterate! (table function)
    (declare (ignore function))
  (warn "iterate not implemented for ~a" (type-of table)))

(defmethod item-table-verify-key (table key)
  (typecase key
    (character
       (< (a2i key) (items.cur-size table)))
    
    (number
     (< key (items.cur-size table)))
    
    (t
     nil)))


(defmethod item-table-print (table &key show-pause start-x start-y)
  (declare (ignore table show-pause start-x start-y))
  (warn "[Printing not implemented]"))




;;; ----------------------------
;; backpack
    
(defmethod item-table-add! ((table items-in-container) obj &optional key)
  (declare (ignore key))
  (let ((retval (add-object-to-array! (items.objs table)
				      (items.cur-size table)
				      (items.max-size table) obj)))
    (when retval
      (incf (items.cur-size table)))

    (item-table-sort! table #'<)
    
    retval))


(defmethod item-table-remove! ((table items-in-container) key)
  (cond ((item-table-verify-key table key)
	 (let ((key-as-num (typecase key
			     (character (a2i key))
			     (number key)
			     (otherwise nil))))
	   (when key-as-num
	     (let ((obj (aref (items.objs table) key-as-num)))
	       (setf (aref (items.objs table) key-as-num) nil)
	       (shrink-array! (items.objs table))
	       (decf (items.cur-size table))
	       obj))))
	(t
	 (warn "illegal key ~a" key)
	 nil)))

(defmethod item-table-clean! ((table items-in-container))
  (when (next-method-p)
    (call-next-method table))
  (loop for i from 0 to (1- (items.max-size table))
	do
	(setf (aref (items.objs table) i) nil))
  nil)

(defmethod item-table-find ((table items-in-container) key)
  (when (item-table-verify-key table key)
    (let ((key-as-num (typecase key
			(character (a2i key))
			(number key)
			(otherwise nil))))
      (when key-as-num
	(aref (items.objs table) key-as-num)))))

(defun stackable? (obj-a obj-b)
  "checks if two objects are stackable.. hackish still."
  (equal (object.sort-value (aobj.kind obj-a))
	 (object.sort-value (aobj.kind obj-b))))

(defmethod item-table-sort! ((table items-in-container) sorter)
  (declare (ignore sorter))
;;  (warn "sorting..")

  (setf (items.objs table) (stable-sort (items.objs table)
				      #'>
				      :key #'(lambda (x)
					       (if x
						   (object.sort-value (aobj.kind x))
						   0))
				      )))
  #||
  (loop for i from 0
	for x across (items.objs table)
	do
	(when (and x (< i (1- (items.max-size table))))
	  (let ((next (aref (items.objs table) (1+ i))))
	    (when (and next (stackable? x next))
	      (incf (aobj.number x))
	      (item-table-remove! table (1+ i))))))
  ||#

  

(defmethod item-table-iterate! ((table items-in-container) function)
  (loop for i from 0 to (1- (items.cur-size table))
	for x = (aref (items.objs table) i)
	do
	(funcall function table i x)))


(defmethod item-table-print ((table items-in-container)
			     &key show-pause start-x start-y)
  
  (let ((x (if start-x start-x 25))
	(y (if start-y start-y 1))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table key))
	     (c-prt! "" (+ i y) (- x 2))
	     (c-put-str! (format nil "~a) ~a" (i2a i)
				(object-description val))
			(+ i y) x)
	   
	     (incf i)))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (c-pause-line! *last-console-line*))

    )))


(defmethod print-object ((inst items-in-container) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (items.cur-size inst)
	   (items.max-size inst)))
  inst)


;;; ----------------------------
;; equipment slots


(defvar *equip-slot-order* nil)
(defvar *allowed-types-for-slots* nil)

(defun register-slot-order& (order)
  "Takes a list of lists (symbol description types-allowed)
and adds settings to various places.  Must be FIXed and moved
to variant obj."
  
;;  (warn "Registering slot order.")

  (let ((len (length order)))

    (setq *equip-slot-order* (make-array len :initial-element nil))
    (setq *allowed-types-for-slots* (make-array len :initial-element nil))
    
    (loop for i from 0
	  for elm in order
	  do
	  (let ((types (if (listp (caddr elm)) (caddr elm) (list (caddr elm)))))
	    
	    (setf (get (car elm) 'arr-place) i
		  (get (car elm) 'description) (cadr elm)
		  (get (car elm) 'types-allowed) types)
	    
	    (dolist (j types)
	      (setf (get j 'equip-slot) i))

	    (setf (aref *equip-slot-order* i) (car elm))
	    (setf (aref *allowed-types-for-slots* i) types)
	    ))
    ))

(defun %get-equip-key-as-num (key obj)
  "Returns the key as the appropriate number or NIL."
  (let ((real-key key))
    (unless real-key
      (let ((its-types (object.obj-type obj)))
	(dolist (i its-types)
	  (let ((poss-key (get i 'equip-slot)))
	    (when (and poss-key (numberp poss-key))
	      (setq real-key poss-key))))))

    (if (not real-key)
	nil
	(typecase real-key
	  (character (a2i real-key))
	  (number real-key)
	  (symbol (get key 'arr-place))
	  (t
	   nil)))))

;;(trace %get-equip-key-as-num)


(defmethod item-table-add! ((table items-worn) obj &optional key)

  (let ((real-key (%get-equip-key-as-num key obj)))

    (if (not real-key)
	nil
	;; now let's check if we're allowed to put the item there
	(let ((type-list (aref *allowed-types-for-slots* real-key)))
	  (if (obj-is-in? obj type-list)
	      (let ((old-obj (aref (items.objs table) real-key)))
		(setf (aref (items.objs table) real-key) obj)
		;;(update-player! *player*) ;; hack.. remove later
		(if old-obj old-obj t)) ;; return T if succesful or object
	      (progn
		(warn "Object ~a cannot be equipped.." obj)
		nil))))
    ))


    

(defmethod item-table-remove! ((table items-worn) key)
  (let ((real-key (%get-equip-key-as-num key nil)))
    (when real-key
      (let ((old-obj (aref (items.objs table) real-key)))
	(setf (aref (items.objs table) real-key) nil)
	old-obj))))
  

(defmethod item-table-clean! ((table items-worn))
  ;; do nothing
  nil)

(defmethod item-table-find ((table items-worn) key)
  (let ((real-key (%get-equip-key-as-num key nil)))
    (when real-key
      (aref (items.objs table) real-key))))


(defmethod item-table-sort! ((table items-worn) sorter)
  (declare (ignore sorter))
  nil)


(defmethod item-table-iterate! ((table items-worn) function)
  (loop for i from 0 to (1- (length (items.objs table)))
	for x = (aref (items.objs table) i)
	do
	(funcall function table i x))
  )

(defmethod item-table-print ((table items-worn) &key show-pause start-x start-y)
  
  (let ((x (if start-x start-x 25))
	(y (if start-y start-y 1))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table key))
	     (c-prt! "" (+ i y) (- x 2))
	     (c-put-str! (format nil "~a) ~13a : ~a" (i2a i)
				 (get (aref *equip-slot-order* i) 'description)
				 (if val
				     (object-description val)
				     "(nothing)"))
			 (+ i y) x)
	     (incf i)))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (c-pause-line! *last-console-line*))

    )))

;;; ----------------------------


(defun make-equipment-slots ()
  "Returns appropriate equipment object."
  (let ((len (length *allowed-types-for-slots*)))

    (make-instance 'items-worn
		   :objs (make-array len :initial-element nil)
		   :cur-size len)))


(defun make-container (size &optional (type 'items-in-container))
  "Returns appropriate container.."
  (make-instance type :max-size size
		 :cur-size 0
		 :objs (make-array size :initial-element nil)))

(defun make-floor-container (dun loc-x loc-y)
  "Returns a container for floor-objects."
  (make-instance 'items-on-floor :dungeon dun :loc-x loc-x :loc-y loc-y))

;;(trace make-container)
;;(trace make-floor-container)
