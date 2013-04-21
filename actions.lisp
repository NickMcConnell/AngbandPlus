;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: actions.lisp - various actions from the kbd/player
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


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

    
(defun select-item (dungeon player allow-from
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
       
       (let ((read-char (read-one-character)))

	 (cond ((eq read-char #\*)
		(setq show-mode t))
	       
	       ((eq read-char #\/)
		(when (or (and allow-equip    (eq the-place :backpack))
			  (and allow-backpack (eq the-place :equip)))
		  (setq the-place (if (eq the-place :backpack) :equip :backpack))))
	       
	       ((and allow-floor (eq read-char #\-))
		(select-item dungeon player allow-from
			     :prompt prompt :where :floor))
	       
	       ((alpha-char-p read-char)
		(c-prt! "" 0 0)
		(return-from select-item (cons the-place (a2i read-char)))))
	 
	 (c-prt! "" 0 0))))
	    
   
    ;; clear prompt
    #-cmu
    (c-prt! "" 0 0)
    
    nil))
  

(defun use-stair! (dun pl dir)
  "Uses a stair in direction DIR if the player PL
is above a stair.  DIR can be :UP or :DOWN"

  (let* ((depth (player.depth pl))
	 (x (location-x pl))
	 (y (location-y pl))
	 (feat (cave-feature dun x y))
	 (leaving-sym nil))
    
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

    (let ((selection (select-item dun pl '(:backpack :equip)
				  :prompt "Drop item: "
				  :where :backpack)))
      (when selection
	(let* ((the-table (get-item-table dun pl (car selection)))
	       (removed-obj (item-table-remove! the-table (cdr selection))))
	  (when removed-obj
	    (item-table-add! (get-item-table dun pl :floor) removed-obj))))
      ))


(defun wear-something! (dun pl)
  "Puts something in an equipment-slot."

  (let ((selection (select-item dun pl '(:backpack :floor)
				:prompt "Wear item"
				:where :backpack)))

;;    (warn "Selected ~a" selection)
    (when selection
      (let* ((the-table (get-item-table dun pl (car selection)))
	     (removed-obj (item-table-remove! the-table (cdr selection))))
;;	(warn "Removed ~a" removed-obj)
	(when removed-obj
	  (let ((retval (item-table-add! (get-item-table dun pl :equip) removed-obj)))
;;	    (warn "Adding to equip gave: ~a" retval)
	    
		  
	      (cond ((eq retval nil)
		     ;; something screwed up, put object back
		     (item-table-add! the-table removed-obj))
		    
		    ((typep retval 'active-object)
		     ;; an object was returned

		     (let ((back-to-inventory (item-table-add! (get-item-table dun pl :backpack)
							       retval)))
		       (unless back-to-inventory
			 ;; drop to floor
			 (item-table-add! (get-item-table dun pl :floor)
					  retval))))
		    (t
		     ;; succesful and nothing returned.. do nothing, except waste energy
		     (setf (player.energy-use pl) +energy-normal-action+)
		     ))
		    
	      ))
	  
	  ))))


