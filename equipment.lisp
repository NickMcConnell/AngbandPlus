;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: equipment.lisp - code for any equipment in all containers.
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

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

    (active-object
     (find key (items.objs table)))
    
    (t
     nil)))


(defmethod item-table-print (table &key show-pause start-x start-y print-selection)
  (declare (ignore show-pause start-x start-y print-selection))
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
			     (active-object (position key (items.objs table)))
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
			     &key show-pause start-x start-y
			     print-selection)
  
  (let ((x (if start-x start-x 25))
	(y (if start-y start-y 1))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table))
	     (when (and (functionp print-selection) (eq nil (funcall print-selection val))) ;; should it be printed?
	       ;;(warn "obj ~s is not to be printed, cur-key ~s" val key)
	       (return-from iterator-fun nil))

	     (assert (integerp key))
	     (let ((attr (get-colour val))
		   (desc (with-output-to-string (s)
			   (write-obj-description *variant* val s))))
	       (c-prt! "" (- x 2) (+ i y))
	       (put-coloured-str! +term-white+ (format nil "~a) " (i2a key)) x (+ i y))
	       (put-coloured-str! attr desc (+ x 4) (+ i y))
	       (incf i))))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (pause-last-line!))

    )))

(defmethod item-table-more-room? ((table items-in-container) &optional obj)
  (declare (ignore obj))
  (< (items.cur-size table) (items.max-size table)))


;;; ----------------------------
;; equipment slots


(defun register-slot-order& (variant order)
  "Takes a list of lists (symbol description types-allowed)
and adds settings to various places.  Must be FIXed and moved
to variant obj."
  
;;  (warn "Registering slot order.")

  (let* ((len (length order))
	 (slot-arr (make-array len :initial-element nil)))
        
    (loop for i from 0
	  for elm in order
	  do
	  (let* ((types (if (listp (third elm))
			   (third elm)
			   (list (third elm))))
		 (item-slot (make-worn-item-slot :key (first elm)
						 :desc (second elm)
						 :types types)))
	    (setf (aref slot-arr i) item-slot)))
    (setf (variant.worn-item-slots variant) slot-arr)
    ))


(defun %get-equip-keys (obj key)
  (let* ((var-obj *variant*)
	 (slot-arr (variant.worn-item-slots var-obj)))
	 
    (flet ((%get-slot-numbers-from-obj (slot-arr obj)
	     (let ((ret-val '()))
	       (when obj
		 (loop for i from 0
		       for x across slot-arr
		       do
		       (dolist (j (worn-item-slot-types x))
			 (when (typep obj j)
			   (push i ret-val)))))
	       ret-val))
	   
	   (%get-slot-numbers-from-key (slot-arr key)
	     (loop for i from 0
		   for x across slot-arr
		   do
		   (when (eq key (worn-item-slot-key x))
		     (return-from %get-slot-numbers-from-key (cons i '()))))
	     nil))
      
      (cond ((eq key nil)
	     (%get-slot-numbers-from-obj slot-arr obj))
	    ((symbolp key)
	     (%get-slot-numbers-from-key slot-arr key))
	    ((characterp key)
	     (cons (a2i key) '()))
	    ((integerp key)
	     (cons key '()))
	    (t
	     (warn "Odd key ~s when adding ~s to worn-items" key obj)))
      )))

(defmethod item-table-add! ((table items-worn) obj &optional key)

  (let* ((obj-arr (items.objs table))
	 (num-keys (%get-equip-keys obj key)))

    
    (when (eq nil num-keys)
;;      (print-message! (format nil "Can't wear ~a" (object.name obj)))
      (warn "Can't wear ~a" (object.name obj))
      (return-from item-table-add! nil))

    ;; check for empty slots
    (dolist (i num-keys)
      (let ((an-obj (aref obj-arr i)))
	(unless an-obj
	  (setf (aref obj-arr i) obj)
	  (return-from item-table-add! t))))
    
    ;; if not, use the first slot
    (let* ((num-key (first num-keys))
	   (old-obj (aref obj-arr num-key)))
      (setf (aref obj-arr num-key) obj)
      
      old-obj)))
    

;; not tested
(defmethod item-table-remove! ((table items-worn) key &key only-single-items)
  
  (declare (ignore only-single-items)) ;; already just single items
  (let ((keys (%get-equip-keys nil key)))
    (when (consp keys)
      (let* ((num (first keys))
	     (old-obj (aref (items.objs table) num)))
	(setf (aref (items.objs table) num) nil)
	old-obj))))
  

(defmethod item-table-clean! ((table items-worn))
  ;; do nothing
  nil)

(defmethod item-table-find ((table items-worn) key)
  
  (let ((keys (%get-equip-keys nil key)))
    (when (consp keys)
      (aref (items.objs table) (first keys)))))


(defmethod item-table-sort! ((table items-worn) sorter)
  (declare (ignore sorter))
  nil)


(defmethod item-table-iterate! ((table items-worn) function)
  (loop for i from 0 to (1- (length (items.objs table)))
	for x = (aref (items.objs table) i)
	do
	(funcall function table i x))
  )

(defmethod item-table-print ((table items-worn)
			     &key show-pause start-x start-y print-selection)
  
  (let* ((x (if start-x start-x 25))
	 (y (if start-y start-y 1))
	 (var-obj *variant*)
	 (slot-info (variant.worn-item-slots var-obj))
	 (i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table))
	     (when (and (functionp print-selection) (eq nil (funcall print-selection val))) ;; should it be printed?
	       (return-from iterator-fun nil))

	     (let ((attr (if val (get-colour val) +term-white+))
		   (desc (if val (with-output-to-string (s)
				   (write-obj-description *variant* val s))
			     "(nothing)")))
	       (c-prt! "" (- x 2) (+ i y))
	       (put-coloured-str! +term-white+ (format nil "~a) ~13a : " (i2a key)
						       (worn-item-slot-desc (aref slot-info i)))
				  x (+ i y))
	       (put-coloured-str! attr desc (+ x 20) (+ i y))
	       (incf i))))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (pause-last-line!))

    )))

(defmethod item-table-more-room? ((table items-worn) &optional obj)
  (declare (ignore obj))
  ;; this one is sortof complex, postpone
  nil)



;;; ----------------------------


(defun make-equipment-slots (variant)
  "Returns appropriate equipment object."
  (let* ((item-slots (variant.worn-item-slots variant))
	 (len (length item-slots)))

    (make-instance 'items-worn
		   :objs (make-array len :initial-element nil)
		   :cur-size len)))


(defun make-container (size &optional (type 'items-in-container))
  "Returns appropriate container.."
  (make-instance type :max-size size
		 :cur-size 0
		 :objs (make-array size :initial-element nil)))

(defun make-floor-container (dungeon loc-x loc-y)
  "Returns a container for floor-objects."
  (make-instance 'items-on-floor :dungeon dungeon :loc-x loc-x :loc-y loc-y))

;;(trace make-container)
;;(trace make-floor-container)

