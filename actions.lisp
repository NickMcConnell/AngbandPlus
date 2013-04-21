;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: actions.lisp - various actions from the kbd/player
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defun move-player! (dun pl direction)
  "Moves the player, in a direction, if possible.
The direction is a number from the keypad."

  (let* ((cur-x (location-x pl))
	 (cur-y (location-y pl))
	 (wanted-x cur-x)
	 (wanted-y cur-y))

    (case direction
      (9 (decf wanted-y) (incf wanted-x))
      (8 (decf wanted-y))
      (7 (decf wanted-y) (decf wanted-x))
      (6 (incf wanted-x))
      (5 nil)
      (4 (decf wanted-x))
      (3 (incf wanted-y) (incf wanted-x))
      (2 (incf wanted-y))
      (1 (incf wanted-y) (decf wanted-x))
      (otherwise
       (warn "Unknown direction ~s" direction)
       (return-from move-player! nil)
       ))

    #||
    (warn "Position ~a ~a has currently ~a,~a -> ~a" wanted-x wanted-y (cave-feature dun wanted-x wanted-y)
	  (cave-info dun wanted-x wanted-y)
	  (cave-floor-bold? dun wanted-x wanted-y))

    ||#

    (setf (player.energy-use pl) +energy-normal-action+)
    
    (let ((monsters (cave-monsters dun wanted-x wanted-y)))

      ;; monsters to attack
      (cond (monsters
	     (attack-location! dun pl wanted-x wanted-y)
	     )

	    ((is-closed-door? dun wanted-x wanted-y)
	     (open-door! dun wanted-x wanted-y))
	    
	    ;; something is in the way
	    ((not (cave-floor-bold? dun wanted-x wanted-y))
	     (c-print-message! "Cannot walk that way.."))

	    ;; default is just to move
	    (t
	     (swap-monsters! dun pl
			     (location-x pl)
			     (location-y pl)
			     wanted-x
			     wanted-y)

	     )
	    ))

    (bit-flag-add! *update* +update-view+)

    ;; hack
    (apply-possible-coord-trigger dun
				  (location-x pl)
				  (location-y pl))
    pl))

  

(defun use-stair! (dun pl dir)
  "Uses a stair in direction DIR if the player PL
is above a stair.  DIR can be :UP or :DOWN"

  (let* ((depth (player.depth pl))
	 (x (location-x pl))
	 (y (location-y pl))
	 (feat (cave-feature dun x y))
	 (leaving-sym nil))
    (declare (type u-16b depth x y feat))
    (case dir
      (:up
       (unless (= +feature-less+ feat)
	 (return-from use-stair! nil))
       
       (if (= depth 0)
	   (error "Cannot go upstairs from level 0")
	   (decf depth))
       (setf leaving-sym :up-stair))
      
      (:down
       (unless (= +feature-more+ feat)
	 (return-from use-stair! nil))
       
       (incf depth)
       (setf leaving-sym :down-stair))
      
      (otherwise
       (lang-warn "Unknown direction for stair-use ~a" dir)
       (return-from use-stair! nil)
       ))

    (bit-flag-add! *redraw* +print-map+ +print-basic+)
    (bit-flag-add! *update* +update-view+)
    
    (setf (player.depth pl) depth
	  (player.energy-use pl) +energy-normal-action+ 
	  (dungeon.depth dun) depth)

    (when (> depth (player.max-depth pl))
      (setf (player.max-depth pl) depth))

    
    (setf (player.leaving-p pl) leaving-sym)
    t))

(defun pick-up-from-floor! (dungeon player)
  "Tries to pick up from floor.  Should be implemented with selection from
a list if more items occupy the same place."
  
  (let* ((x (location-x player))
	 (y (location-y player))
	 (objs (cave-objects dungeon x y)))

    (if (not objs)
	(progn
	  (lang-warn "Nothing on floor.")
	  nil)

	(loop
	 (let ((removed-obj (item-table-remove! objs 0)))
	   (unless removed-obj
	       (return-from pick-up-from-floor! nil))

	   (cond ((obj-is? removed-obj '<money>)
		  ;; are we gold?
		  (incf (player.gold player) (aobj.number removed-obj))
		  ;; redraw left frame
		  (bit-flag-add! *redraw* +print-basic+)
		  
		  (when (= 0 (items.cur-size objs))
		    (setf (cave-objects dungeon x y) nil)))
		 

		 (t
		  (let* ((backpack (aobj.contains (player.inventory player)))
			 (retval (item-table-add! backpack removed-obj)))

		    (if retval
			;; succesful
			(when (= 0 (items.cur-size objs))
			  (setf (cave-objects dungeon x y) nil))
			;; not succesful.. put it back
			(progn
			  (item-table-add! objs removed-obj)
			  (warn "No room in backpack.")
			  (return-from pick-up-from-floor! nil))))))

	   (unless (cave-objects dungeon x y)
	     (return-from pick-up-from-floor! nil))))

	)))


(defun drop-something! (dun pl)
  "Drop some inventory"

  (when-bind (selection (select-item dun pl '(:backpack :equip)
				     :prompt "Drop item: "
				     :where :backpack))
    (let* ((the-table (get-item-table dun pl (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection))))
      (when removed-obj
	(item-table-add! (get-item-table dun pl :floor) removed-obj))))
  )

(defun %put-obj-in-cnt (dun pl cnt obj)
  (let* ((the-table (if (typep cnt 'item-table) cnt (get-item-table dun pl cnt)))
	 (back-to-inventory (item-table-add! the-table obj)))
    (unless back-to-inventory
      ;; drop to floor
      (item-table-add! (get-item-table dun pl :floor)
		       obj))
    ))

;;(trace %put-obj-in-cnt)

(defun wear-something! (dun pl)
  "Puts something in an equipment-slot."

  ;;    (warn "Selected ~a" selection)
  (when-bind (selection (select-item dun pl '(:backpack :floor)
				:prompt "Wear item"
				:where :backpack))
    
    
    (let* ((the-table (get-item-table dun pl (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection) :only-single-items t)))
;;      (warn "Removed ~a" removed-obj)
      
      (when removed-obj
	(let ((retval (item-table-add! (get-item-table dun pl :equip) removed-obj)))
;;	  (warn "Adding to equip gave: ~a" retval)
	  
		  
	  (cond ((eq retval nil)
		 ;; something screwed up, put object back
		 (item-table-add! the-table removed-obj))
		
		((typep retval 'active-object)
		 ;; an object was returned
		 (%put-obj-in-cnt dun pl :backpack retval))
		
		(t
		 ;; succesful and nothing returned.. do nothing, except waste energy
		 (setf (player.energy-use pl) +energy-normal-action+)
		 ))
	  
	  ))
      
      )))

(defun apply-usual-effects-on-used-object (dun pl obj)
  (declare (ignore dun))

  (let* ((okind (etypecase obj
		 (active-object (aobj.kind obj))
		 (object-kind obj)))
	 (gvals (object.game-values okind)))
  
  ;; we have tried object
  ;;(tried-object)
  
    (when (is-eatable? obj)
      (alter-food! pl (+ (player.food pl)
			 (gval.food-val gvals))))
    
    
    t))


(defun use-something! (dun pl
		       &key
		       restrict-type (prompt "Use item?")
		       (limit-from '(:backpack :floor)))
  
  "Tries to use an item."

  (declare (ignore restrict-type))
  (assert (consp limit-from))
  (assert (stringp prompt))
  
  (let ((selection (select-item dun pl limit-from
				:prompt prompt
				:where (first limit-from))))

    (unless (and selection (consp selection))
      (return-from use-something! nil))

    
;;    (warn "Selected ~s for use" selection)
    (let* ((the-table (get-item-table dun pl (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection))))
      
;;      (warn "Removed ~a" removed-obj)
      
      (unless (and removed-obj (typep removed-obj 'active-object))
	(return-from use-something! nil))
      
;;      (warn "Will ~a ~s" prompt removed-obj)
	  
      (let ((retval (use-object! *variant* dun pl removed-obj)))
;;	(warn "use returned ~s" retval)
	(cond ((eq retval nil) ;; didn't use the object for some reason..
	       (%put-obj-in-cnt dun pl the-table removed-obj))
	      
	      ((eq retval :still-useful) ;; we should keep it
	       (let ((back-obj (%put-obj-in-cnt dun pl the-table removed-obj)))
		 ;; add energy use
		 (apply-usual-effects-on-used-object dun pl removed-obj)
		 back-obj))
	      
	      ((eq retval :used) ;; we have used the item
	       ;; decrement number
	       (when (> (aobj.number removed-obj) 1)
		 (decf (aobj.number removed-obj))
		 (%put-obj-in-cnt dun pl the-table removed-obj))
;;	       (warn "used object ~a" removed-obj)
	       (apply-usual-effects-on-used-object dun pl removed-obj)
	       nil)
	      
	      (t
	       (warn "Fell through on use return-value: ~s" retval)))
	
	))
      ))

  

(defun invoke-spell! (dun pl &key spell-type)
  "Invokes a spell.. gee."

  (declare (ignore dun pl spell-type))
  ;; fill in all the nasty details
  (warn "player invokes a spell.")
  
  (values))

