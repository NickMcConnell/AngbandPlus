;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: util.lisp - utility-code dependant on other code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Convenient utilities which is based on several
ADD_DESC: classes and must be loaded late.

||#

(in-package :org.langband.engine)

    
(defmethod select-item ((dungeon dungeon) (player player) allow-from
			&key prompt (where :backpack)
			selection-function)
  "Selects and returns a CONS (where . which) when succesful or
NIL.  Where is a keyword :floor, :backpack or :equip and which is either
a number or a symbol identifying the place."

  (let ((allow-floor (if (or (eq allow-from :floor)
			     (find :floor allow-from))
			 t
			 nil))
	
	(allow-equip (if (or (eq allow-from :equip)
			     (find :equip allow-from))
			 t
			 nil))
	
	(allow-backpack (if (or (eq allow-from :backpack)
				(find :backpack allow-from))
			    t
			    nil))
	(the-prompt (if prompt prompt "Inventory command:"))

	(show-mode nil)
	(the-place where)
	(printed-prompt nil)
	)

    (block read-loop
      (loop
       (when selection-function
	 (warn "selection function not implemented."))
       
       (setq printed-prompt (format nil "~a " the-prompt))
       (c-prt! printed-prompt 0 0)
       
       (when show-mode
	 (item-table-print (get-item-table dungeon player the-place)
			   :show-pause nil))

       ;; add setting of cursor.

       ;; how to do graceful exit of loop?
       (let ((read-char (read-one-character)))
	 ;;(warn "read-char ~s" read-char)
	 (cond ((eql read-char +escape+)
		(return-from select-item nil))
	       
	       ((eql read-char #\*)
		(setq show-mode t))
	       
	       ((eql read-char #\/)
		(when (or (and allow-equip    (eq the-place :backpack))
			  (and allow-backpack (eq the-place :equip)))
		  (setq the-place (if (eq the-place :backpack) :equip :backpack))))
	       
	       ((and allow-floor (eq read-char #\-))
		(select-item dungeon player allow-from
			     :prompt prompt :where :floor))

	       ;; improve this code, it just bails out now.
	       ((alpha-char-p read-char)
		(c-prt! "" 0 0)
		(let ((num (a2i read-char)))
		  (if (>= num 0)
		      (return-from select-item (cons the-place num))
		      (return-from select-item nil))))
	       
	       (t
		;; maybe go through loop again instead?
		;;(error "Fell through on read-char in select-item ~s" read-char)
		))
	       
	 (c-prt! "" 0 0))))
	    
   
    ;; clear prompt
    #-cmu
    (c-prt! "" 0 0)
    
    nil))

(defun grab-a-selection-item (dungeon player  allow-from
			     &key prompt (where :backpack)
			     selection-function)
  "Wrapper for select-item which just gets and returns the 'removed' object, or NIL."
  (when-bind (selection (select-item dungeon player allow-from
				     :prompt prompt
				     :where where
				     :selection-function selection-function))

    (item-table-remove! (get-item-table dungeon player (car selection))
			(cdr selection) :only-single-items t)))

(defun get-ensured-floor-table (dungeon the-x the-y)
  (let ((cur-objs (cave-objects dungeon the-x the-y)))
    (unless cur-objs
      (setf cur-objs (make-floor-container dungeon the-x the-y))
      (setf (cave-objects dungeon the-x the-y) cur-objs))
    cur-objs))

(defmethod get-item-table ((dungeon dungeon) (player player) which-table &key x y)
  "Returns item-table or NIL."
  
  (ecase which-table
    (:floor
     (let* ((the-x (if x x (location-x player)))
	    (the-y (if y y (location-y player))))
       (get-ensured-floor-table dungeon the-x the-y)))
    (:backpack (aobj.contains (player.inventory player)))
    (:equip (player.eq player))))



;;; === Equipment-implementation for floors ===

(defmethod item-table-add! ((table items-on-floor) obj &optional key)
  (declare (ignore key))
;;  (lang-warn "Pushing ~a [~a,~a] onto floor [~a,~a]"
;;	    obj (location-x obj) (location-y obj)
;;	    (location-x table) (location-y table))
  (let ((tab-x (location-x table))
	(tab-y (location-y table))
	(dun (items.dun table)))
	
    (setf (location-x obj) tab-x
	  (location-y obj) tab-y)
    (push obj (dungeon.objects dun))
    (push obj (items.objs table))
    (incf (items.cur-size table))

    ;; let's notify about the change
    (note-spot! dun tab-x tab-y)
    (light-spot! dun tab-x tab-y)
    
    t))

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

(defmethod item-table-more-room? ((table items-on-floor) &optional obj)
  (declare (ignore obj))
  t)



(defmethod calculate-score (variant player)
  (declare (ignore variant))
  (+ (player.max-xp player) (* 100 (player.depth player))))

;;; Code for backpacks.. move later

(defconstant +common-backpack-size+ 23)

(defun common-creating-backpack (state dungeon player aobj)
  "Assigns a container to aobj.contains."
  
  (declare (ignore player dungeon state))
  
  (let ((container (make-container +common-backpack-size+)))
    (setf (aobj.contains aobj) container)
    t))


;;; === Some simple code for actual rooms, move them later.

(defclass simple-room (room-type)
  ())

(defclass overlapping-room (room-type)
  ())

(defun common-make-simple-room ()
  "constructor for the simple room."
  (let ((room (make-instance 'simple-room :id "simple-room"
			     :name "simple room"
			     :size-mod #1A(0 0 -1 1)
			     :min-level 1)))

    room))

(defun common-make-overlapping-room ()
  "constructor for the overlapping room."
  (let ((room (make-instance 'overlapping-room :id "overlapping-room"
			     :name "overlapping room"
			     :size-mod #1A(0 0 -1 1)
			     :min-level 1)))

    room))

(defun %room-has-light? (room dungeon &optional (chance 25))
  (declare (ignore room))
  (<= (dungeon.depth dungeon) (randint chance)))

(defmethod build-room! ((room simple-room) dungeon player x0 y0)

  (declare (ignore player))
  
  (let (;;(depth (dungeon.depth dungeon))
	(light (%room-has-light? room dungeon 25))
	(y1 (- y0 (randint 4)))
	(y2 (+ y0 (randint 3)))
	(x1 (- x0 (randint 11)))
	(x2 (+ x0 (randint 11))))

    (generate-room dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) light)
    (generate-draw dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) +feature-wall-outer+)
    (generate-fill dungeon x1 y1 x2 y2 +feature-floor+)

    (cond ((= 0 (random 20)) ;; pillar room
	   (loop for y from y1 to y2 by 2
		 do
		 (loop for x from x1 to x2 by 2
		       do
		       (setf (cave-feature dungeon x y) +feature-wall-inner+))))
	  
	  ((= 0 (random 50)) ;; ragged
	   (loop for y from (+ y1 2) to (- y2 2) by 2
		 do
		 (setf (cave-feature dungeon x1 y) +feature-wall-inner+
		       (cave-feature dungeon x2 y) +feature-wall-inner+))
	   
	   (loop for x from (+ x1 2) to (- x2 2) by 2
		 do
		 (setf (cave-feature dungeon x y1) +feature-wall-inner+
		       (cave-feature dungeon x y2) +feature-wall-inner+))
	   ))

    ))

(defmethod build-room! ((room overlapping-room) dungeon player x0 y0)

  (declare (ignore player))
  
  (let ((light (%room-has-light? room dungeon 25))
	(a-y1 (- y0 (randint 4)))
	(a-y2 (+ y0 (randint 3)))
	(a-x1 (- x0 (randint 11)))
	(a-x2 (+ x0 (randint 10)))
	(b-y1 (- y0 (randint 3)))
	(b-y2 (+ y0 (randint 4)))
	(b-x1 (- x0 (randint 10)))
	(b-x2 (+ x0 (randint 11))))
	

    (generate-room dungeon (1- a-x1) (1- a-y1) (1+ a-x2) (1+ a-y2) light)
    (generate-room dungeon (1- b-x1) (1- b-y1) (1+ b-x2) (1+ b-y2) light)
	
    (generate-draw dungeon (1- a-x1) (1- a-y1) (1+ a-x2) (1+ a-y2) +feature-wall-outer+)
    (generate-draw dungeon (1- b-x1) (1- b-y1) (1+ b-x2) (1+ b-y2) +feature-wall-outer+)
    
    (generate-fill dungeon a-x1 a-y1 a-x2 a-y2 +feature-floor+)
    (generate-fill dungeon b-x1 b-y1 b-x2 b-y2 +feature-floor+)
    ))


(defmethod can-creature-drop? ((variant variant) (mon active-monster))
  (let ((kind (amon.kind mon)))
    (if (monster.treasures kind)
	t
	nil)))

(defmethod creature-drop! ((variant variant) (mon active-monster) (level level))
  (let ((kind (amon.kind mon))
	 (to-drop '()))
     ;; first iterate over the drops and decide what is actually dropped
     (dolist (i (monster.treasures kind))
       (check-type i treasure-drop)
       (when (< (random 100) (* 100 (drop.chance i)))
	 (let ((amount (drop.amount i)))
	   (when (consp amount)
	     (setf amount (roll-dice (car amount) (cdr amount))))
	   (when (plusp amount)
	     (push (list amount (drop.quality i) (drop.type i)) to-drop)))))

     ;; now process the actual drops
     (flet ((drop-an-obj (quality type)
	      (declare (ignore quality))
	      (case type
		((:any :item)
		 (let ((new-obj (get-active-object-by-level variant level)))
		   (drop-near-location! variant (level.dungeon level)
					new-obj (location-x mon) (location-y mon)))
		 )
		(:gold
		 (warn "Gold-drops not implemented yet."))
		)

	      ))
;;	      (warn "Dropping ~s ~s at (~s,~s)" quality type (location-x mon) (location-y mon)
     
       ;; now we have a list of objects (hopefully) to drop
       
       (dolist (i to-drop)
	 (assert (consp i))
	 (dotimes (j (car i))
	   (drop-an-obj (second i) (third i))))
     

     )))

(defmethod drop-near-location! ((variant variant) (dungeon dungeon) (object active-object) x y)
  ;; we do this hackish
  (flet ((poss-drop (tx ty)
	   (when (= (cave-feature dungeon tx ty) +feature-floor+) ;; must be a floor
	     (let ((tbl (get-ensured-floor-table dungeon tx ty)))
	       ;;(warn "Dropped ~s at (~s,~s)" object tx ty)
	       (item-table-add! tbl object)
	       (return-from drop-near-location! t)))))
    (poss-drop x y)
    (poss-drop (1+ x) y)
    (poss-drop x (1+ y))
    (poss-drop (1+ x) (1+ y))
    nil))



(defmethod get-loadable-form ((object game-values) &key)
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-val val)))))))
      (setf the-form (list 'make-game-values))
      
      (possibly-add :base-ac (gval.base-ac object) 0)
      (possibly-add :ac-modifier (gval.ac-modifier object) 0)
      (possibly-add :base-dice (gval.base-dice object) 0)
      (possibly-add :num-dice (gval.num-dice object) 0)
      (possibly-add :tohit-modifier (gval.tohit-modifier object) 0)
      (possibly-add :dmg-modifier (gval.dmg-modifier object) 0)
      (possibly-add :mana (gval.mana object) 0)
      (possibly-add :charges (gval.charges object) 0)
      (possibly-add :food-value (gval.food-value object) 0)
      (possibly-add :light-radius (gval.light-radius object) 0)
      (possibly-add :tunnel (gval.tunnel object) 0)
      (possibly-add :speed (gval.speed object) 0)
      (possibly-add :skill-modifiers (gval.skill-modifiers object))
      (possibly-add :stat-modifiers (gval.stat-modifiers object))
      (possibly-add :ignores (gval.ignores object) 0)
      (possibly-add :resists (gval.resists object) 0)
      (possibly-add :immunities (gval.immunities object) 0)
      (possibly-add :abilities (gval.abilities object))
      (possibly-add :sustains (gval.sustains object))
      (possibly-add :slays (gval.slays object))

      the-form)))

;;(trace select-item :encapsulate t)
