;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: util.lisp - utility-code dependant on other code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Convenient utilities which is based on several
ADD_DESC: classes and must be loaded late.

|#

(in-package :org.langband.engine)

(defun get-item-table (dungeon player which-table)
  "Returns item-table or NIL."
  
  (ecase which-table
    (:floor
     (let* ((px (location-x player))
	    (py (location-y player))
	    (cur-objs (cave-objects dungeon px py)))
       (unless cur-objs
	 (setf cur-objs (make-floor-container dungeon px py))
	 (setf (cave-objects dungeon px py) cur-objs))
       cur-objs))
    (:backpack (aobj.contains (player.inventory player)))
    (:equip (player.eq player))))


;;; === Equipment-implementation for floors ===

(defmethod item-table-add! ((table items-on-floor) obj &optional key)
  (declare (ignore key))
;;  (lang-warn "Pushing ~a [~a,~a] onto floor [~a,~a]"
;;	    obj (location-x obj) (location-y obj)
;;	    (location-x table) (location-y table))
  (setf (location-x obj) (location-x table)
	(location-y obj) (location-y table))
  (push obj (dungeon.objects (items.dun table)))
  (push obj (items.objs table))
  (incf (items.cur-size table))
  t)

(defmethod item-table-remove! ((table items-on-floor) key &key only-single-items)
  (cond ((item-table-verify-key table key)
	 (let ((ret-obj nil)
	       (num-key (typecase key
			  (character (a2i key))
			  (number key)
			  (t nil))))
	   (when (numberp num-key)
	     (let ((old-obj (elt (items.objs table) num-key)))

	       ;; if only one, else remove
	       (cond ((and only-single-items (> (aobj.number old-obj) 1))
		      (setf ret-obj (create-aobj-from-kind (aobj.kind old-obj)))
		      (decf (aobj.number old-obj)))
		     (t
		      (setf (items.objs table) (delete old-obj (items.objs table)))
		      (remove-item-from-dungeon! (items.dun table) old-obj)
		      (decf (items.cur-size table))
		      (setf ret-obj old-obj)))))
	   
	   ret-obj))
	(t
	 (warn "illegal key ~a" key)
	 nil)))

(defmethod item-table-clean! ((table items-on-floor))
  (when (next-method-p)
    (call-next-method table))
  (let ((dun (items.dun table)))
    (dolist (i (items.objs table))
      (remove-item-from-dungeon! dun i)))

  (setf (items.objs table) nil))

(defmethod item-table-find ((table items-on-floor) key)
  (when (item-table-verify-key table key)
    (typecase key
      (character (elt (items.objs table) (a2i key)))
      (number (elt (items.objs table) key))
      (t
       (warn "unknown type ~a of key" (type-of key))
       nil))))


(defmethod item-table-sort! ((table items-on-floor) sorter)
  (declare (ignore sorter))
  ;; the floor is never sorted
  nil)

(defmethod item-table-iterate! ((table items-on-floor) function)
  (loop for i from 0
	for obj in (items.objs table)
	do
	(funcall function table i obj)))

(defmethod calculate-score (variant player)
  (declare (ignore variant))
  (+ (player.max-xp player) (* 100 (player.depth player))))
