;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: equipment.lisp - code for any equipment in all containers.
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


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
	     :initform +illegal-loc-x+);; invalid value
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


;;; -----------------------------

(defmethod item-table-add! (table obj &optional key)
  (declare (ignore obj key))
  (error "add not implemented for ~a" (type-of table)))

(defmethod item-table-remove! (table key &key only-single-items)
  (declare (ignore key only-single-items))
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
  (declare (ignore show-pause start-x start-y))
  (warn "[Printing not implemented for table ~s]" table))

(defmethod item-table-more-room? (table &optional obj)
  (declare (ignore obj))
  (warn "[MORE-ROOM? isn't implemented for table ~s]" table)
  nil)

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


(defmethod item-table-remove! ((table items-in-container) key &key only-single-items)
  (cond ((item-table-verify-key table key)
	 (let ((key-as-num (typecase key
			     (character (a2i key))
			     (number key)
			     (otherwise nil))))
	   (when key-as-num
	     (let ((old-obj (aref (items.objs table) key-as-num)))
	       (cond ((and only-single-items (> (aobj.number old-obj) 1))
		      (let ((ret-obj (create-aobj-from-kind (aobj.kind old-obj))))
			(decf (aobj.number old-obj))
			ret-obj))
		     (t
		      (setf (aref (items.objs table) key-as-num) nil)
		      (shrink-array! (items.objs table))
		      (decf (items.cur-size table))
		      old-obj))))
	   ))
	((typep key 'active-object)
	 (loop for i from 0
	       for x across (items.objs table)
	       do
	       (when (eq key x)
		 (return-from item-table-remove!
		   (item-table-remove! table i
				       :only-single-items only-single-items))))
	 (warn "[Object ~a not found when removing from container]"
	       key)
	 nil)
	 
	(t
	 (warn "[illegal key ~a when removing from container]" key)
	 nil)))

(defmethod item-table-clean! ((table items-in-container))
  (when (next-method-p)
    (call-next-method table))
  (loop for i from 0 to (1- (items.max-size table))
	do
	(setf (aref (items.objs table) i) nil))
  nil)

(defmethod item-table-find ((table items-in-container) key)
  (cond ((item-table-verify-key table key)
	 (let ((key-as-num (typecase key
			     (character (a2i key))
			     (number key)
			     (otherwise nil))))
	   (when key-as-num
	     (aref (items.objs table) key-as-num))))
	((typep key 'active-object)
	 (find key (items.objs table)))
	(t nil)))

(defun stackable? (obj-a obj-b)
  "checks if two objects are stackable.. hackish still."
  (and obj-a obj-b
       (equal (object.sort-value (aobj.kind obj-a))
	      (object.sort-value (aobj.kind obj-b)))))

(defun %equip-stacking (table)
  (loop for i from 0
	for x across (items.objs table)
	with prev = nil
	do
;;	(warn "comparing ~s ~s -> ~s" x prev (stackable? x prev))
	(when (stackable? x prev)
	  (incf (aobj.number prev) (aobj.number x))
	  (item-table-remove! table i)
	  (return-from %equip-stacking nil))
	(setf prev x))
  t)

(defmethod item-table-sort! ((table items-in-container) sorter)
  (declare (ignore sorter))
;;  (warn "sorting..")

  (setf (items.objs table) (stable-sort (items.objs table)
				      #'>
				      :key #'(lambda (x)
					       (if x
						   (object.sort-value (aobj.kind x))
						   0))
				      ))
  
  (loop
   (let ((stacking-done (%equip-stacking table)))
     (when stacking-done
       (return-from item-table-sort! nil)))))
  

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
	     (let ((attr (get-attribute val))
		   (desc (with-output-to-string (s)
			   (write-obj-description *variant* val s))))
	       (c-prt! "" (+ i y) (- x 2))
	       (c-col-put-str! +term-white+ (format nil "~a) " (i2a i)) (+ i y) x)
	       (c-col-put-str! attr desc (+ i y) (+ x 4))
	       (incf i))))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (c-pause-line! *last-console-line*))

    )))

(defmethod item-table-more-room? ((table items-in-container) &optional obj)
  (declare (ignore obj))
  (< (items.cur-size table) (items.max-size table)))


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
;;	(warn "Checking types ~a" its-types)
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
;;    (warn "RK: ~a" real-key)
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


    

(defmethod item-table-remove! ((table items-worn) key &key only-single-items)
  (declare (ignore only-single-items)) ;; already just single items
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
	     (let ((attr (if val (get-attribute val) +term-white+))
		   (desc (if val (with-output-to-string (s)
				   (write-obj-description *variant* val s))
			     "(nothing)")))
	       (c-prt! "" (+ i y) (- x 2))
	       (c-col-put-str! +term-white+ (format nil "~a) ~13a : " (i2a i)
						    (get (aref *equip-slot-order* i) 'description))
			       (+ i y) x)
	       (c-col-put-str! attr desc (+ i y) (+ x 20))
	       (incf i))))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (c-pause-line! *last-console-line*))

    )))

(defmethod item-table-more-room? ((table items-worn) &optional obj)
  (declare (ignore obj))
  ;; this one is sortof complex, postpone
  nil)



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

