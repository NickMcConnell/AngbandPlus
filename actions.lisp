;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: actions.lisp - various actions from the kbd/player
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defun move-player! (dungeon player direction)
  "Moves the player, in a direction, if possible.
The direction is a number from the keypad."

  (let* ((var-obj *variant*)
	 (cur-x (location-x player))
	 (cur-y (location-y player))
	 (wanted-x cur-x)
	 (wanted-y cur-y))

    ;; hack, move to variant later
    (when-bind (temp-attrs (player.temp-attrs player))
      (when (get-attribute-value '<confusion> temp-attrs)
	(let ((new-dir (randint 9)))
	  ;; ultra-hack
	  (when (eql new-dir 5)
	    (setf new-dir direction))
	  (setf direction new-dir))))
 
    
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
    (warn "Position ~a ~a has currently ~a,~a -> ~a" wanted-x wanted-y
          (cave-floor dungeon wanted-x wanted-y)
	  (cave-info dungeon wanted-x wanted-y)
	  (cave-floor-bold? dungeon wanted-x wanted-y))

    ||#

    (setf (player.energy-use player) +energy-normal-action+)
    
    (let ((monsters (cave-monsters dungeon wanted-x wanted-y)))

      ;; monsters to attack
      (cond (monsters
	     (attack-location! dungeon player wanted-x wanted-y)
	     )

	    ((is-closed-door? dungeon wanted-x wanted-y)
	     (open-door! dungeon wanted-x wanted-y))
	    
	    ;; something is in the way
	    ((not (cave-floor-bold? dungeon wanted-x wanted-y))
	     (print-message! "Cannot walk that way.."))

	    ;; default is just to move
	    (t
	     (swap-monsters! dungeon player
			     (location-x player)
			     (location-y player)
			     wanted-x
			     wanted-y)
	     (on-move-to-coord var-obj player wanted-x wanted-y)

	     )
	    ))

    (let ((new-x (location-x player))
	  (new-y (location-y player)))
      
      (when-bind (objs (cave-objects dungeon new-x new-y))
	(check-type objs item-table)
	(assert (plusp (items.cur-size objs)))
	(let ((obj (item-table-find objs 0)))
	  (check-type obj active-object)
	  (format-message! "You see ~a."
			   (with-output-to-string (s)
			     (write-obj-description var-obj obj s)))
	  ))
      
      (bit-flag-add! *update* +pl-upd-update-view+)

      
      ;; hack
      (apply-possible-coord-trigger dungeon new-x new-y))

    player))


(defun search-area! (dungeon player)
  "Searches nearby grids."

  (let ((chance (get-search-skill *variant* player))
	(x (location-x player))
	(y (location-y player))
	(ddx-ddd *ddx-ddd*)
	(ddy-ddd *ddy-ddd*))
    
    (check-type chance fixnum)
    
    (incf (player.energy-use player) +energy-normal-action+)
    
    (loop for i from 0 below +normal-direction-number+
	  for cur-x = (+ x (svref ddx-ddd i))
	  for cur-y = (+ y (svref ddy-ddd i))
	  do
	  (when (< (random 100) chance)
	    (let* ((coord (cave-coord dungeon cur-x cur-y))
		   ;;(floor (coord.floor coord))
		   (decor (coord.decor coord)))
	      
	      (when (typep decor 'active-trap)
		(make-trap-visible decor dungeon cur-x cur-y)
		(disturbance *variant* player decor :major)
		(print-message! "You have found a trap."))

	      (when (and (typep decor 'active-door)
			 (not (decor.visible? decor)))
		(print-message! "You found a secret door.")
		(setf (decor.visible? decor) t)
		(disturbance *variant* player decor :major)
		;;(place-closed-door! dungeon cur-x cur-y)
		(light-spot! dungeon cur-x cur-y)
		)
	  
	      ;; add more here, traps, chests.. 
	      )))
    ))


(defun change-depth! (dungeon player &key (direction :down) (amount 1) type)

  (let* ((cur-depth (player.depth player))
	 (wanted-depth (ecase direction
			 (:up (- cur-depth amount))
			 (:down (+ cur-depth amount)))))

    (cond ((minusp wanted-depth)
	   (setf wanted-depth 0))
	  ((> wanted-depth (variant.max-depth *variant*))
	   (setf wanted-depth (variant.max-depth *variant*))))
    
    (setf (player.depth player) wanted-depth
	  (player.energy-use player) +energy-normal-action+ 
	  (dungeon.depth dungeon) wanted-depth)
    
    (when (> wanted-depth (player.max-depth player))
      (setf (player.max-depth player) wanted-depth))
    
    
    (setf (player.leaving? player) type)

    (bit-flag-add! *redraw* +print-map+ +print-basic+)
    (bit-flag-add! *update* +pl-upd-update-view+)  
    
    t))

(defun use-stair! (dungeon player dir)
  "Uses a stair in direction DIR if the player PL
is above a stair.  DIR can be :UP or :DOWN"

  (let* ((depth (player.depth player))
	 (x (location-x player))
	 (y (location-y player))
	 (feat (cave-floor dungeon x y))
	 (flags (floor.flags feat)) 
	 (leaving-sym nil))
    
    ;;(declare (type u16b depth x y feat))
    (case dir
      (:up
       (unless (bit-flag-set? flags +floor-flag-exit-upwards+)
	 (return-from use-stair! nil))
       
       
       (if (= depth 0)
	   (error "Cannot go upstairs from level 0")
	   (decf depth))
       (setf leaving-sym :up-stair))
      
      (:down
       (unless (bit-flag-set? flags +floor-flag-exit-downwards+)
	 (return-from use-stair! nil))
       
       (incf depth)
       (setf leaving-sym :down-stair))
      
      (otherwise
       (lang-warn "Unknown direction for stair-use ~a" dir)
       (return-from use-stair! nil)
       ))



    (change-depth! dungeon player :direction dir :amount 1 :type leaving-sym)
    

    t))

(defun pick-up-from-floor! (dungeon player)
  "Tries to pick up from floor.  Should be implemented with selection from
a list if more items occupy the same place."
  
  (let* ((var-obj *variant*)
	 (x (location-x player))
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

	   ;; hackish, move to variant later
	   (let ((removed-type (object.the-kind (aobj.kind removed-obj))))
	   (cond ((eq removed-type '<money>)
		  ;; are we gold?
		  (incf (player.gold player) (aobj.number removed-obj))
		  ;; redraw left frame
		  (bit-flag-add! *redraw* +print-basic+)
		  
		  (when (= 0 (items.cur-size objs))
		    (setf (cave-objects dungeon x y) nil))
		  (light-spot! dungeon x y))
		 

		 (t
		  (let* ((backpack (aobj.contains (player.inventory player)))
			 (retval (item-table-add! backpack removed-obj)))

		    (cond (retval
			   (format-message! "You pick up ~a."
					    (with-output-to-string (s)
					      (write-obj-description var-obj removed-obj s)))
			   (on-pickup-object var-obj player removed-obj)
			   (bit-flag-add! *update* +pl-upd-bonuses+)
			   ;; succesful
			   (when (= 0 (items.cur-size objs))
			     (setf (cave-objects dungeon x y) nil))
			   (light-spot! dungeon x y))

			  (t 
			   ;; not succesful.. put it back
			   (item-table-add! objs removed-obj)
			   (print-message! "No room in backpack.")
			   (return-from pick-up-from-floor! nil)))))
		 ))

	   (unless (cave-objects dungeon x y)
	     (return-from pick-up-from-floor! nil))))

	)))


(defun interactive-drop-item! (dungeon player)
  "Drop some inventory"

  (when-bind (selection (with-frame (+query-frame+)
			  (select-item dungeon player '(:backpack :equip)
				       :prompt "Drop item: "
				       :where :backpack)))

    (let* ((var-obj *variant*)
	   (the-table (get-item-table dungeon player (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection))))

      (unless removed-obj
	(format-message! "Did not find selected obj ~a" selection)
	(return-from interactive-drop-item! nil))

      (when (and (typep removed-obj 'active-object)
		 (is-cursed? removed-obj))
	(print-message! "Hmmm, it seems to be cursed.")
	(item-table-add! the-table removed-obj) ;; put back
	(return-from interactive-drop-item! nil))

      (check-type removed-obj active-object)

      (drop-near-location! var-obj dungeon removed-obj (location-x player) (location-y player))
      (on-drop-object var-obj player removed-obj)
      (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+)
      ;;(item-table-add! (get-item-table dungeon player :floor) removed-obj)
      t)))
			     
(defun interactive-take-off-item! (dungeon player)

  (when-bind (selection (with-frame (+query-frame+)
			  (select-item dungeon player '(:equip)
				       :prompt "Take off item: "
				       :where :equip)))

    (let* ((the-table (get-item-table dungeon player (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection))))
      (when (and (typep removed-obj 'active-object)
		 (is-cursed? removed-obj))
	(print-message! "Hmmm, it seems to be cursed.")
	(item-table-add! the-table removed-obj) ;; put back
	(return-from interactive-take-off-item! nil))
      
      (cond ((typep removed-obj 'active-object)
	     ;; an object was returned
	     (%put-obj-in-cnt dungeon player :backpack removed-obj)
	     ;;(bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-mana+ +pl-upd-torch+)
	     (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+)
	     )
	    
	    (t
	     (format-message! "Did not find selected obj ~a" selection)
	     )))
    ))

(defun %put-obj-in-cnt (dungeon player cnt obj)
  (let* ((the-table (if (typep cnt 'item-table) cnt (get-item-table dungeon player cnt)))
	 (back-to-inventory (item-table-add! the-table obj)))
    (unless back-to-inventory
      ;; drop to floor
      (item-table-add! (get-item-table dungeon player :floor)
		       obj))
    ))

;;(trace %put-obj-in-cnt)

(defun interactive-wear-item! (dungeon player)
  "Puts something in an equipment-slot."

  (when-bind (selection (with-frame (+query-frame+)
			  (select-item dungeon player '(:backpack :floor)
				       :prompt "Wear item"
				       :where :backpack))
	      		)
    
    
    (let* ((the-table (get-item-table dungeon player (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection) :only-single-items t))
	   (other-obj nil))
;;      (warn "Removed ~a" removed-obj)

      (unless removed-obj ;; we need to get an object, if not return!
	(return-from interactive-wear-item! nil))

      ;; another obj?
      (setf other-obj (item-table-add! (get-item-table dungeon player :equip) removed-obj))
;;	  (warn "Adding to equip gave: ~a" other-obj)

      (when (eq other-obj nil)
	;; something screwed up, put object back
	(item-table-add! the-table removed-obj)
	(return-from interactive-wear-item! nil))

      (when (is-cursed? removed-obj)
	(print-message! "Oops! It feels deathly cold!"))
      
      (cond ((typep other-obj 'active-object)
	     ;; an object was returned
	     (%put-obj-in-cnt dungeon player :backpack other-obj)
	     (on-wear-object *variant* player removed-obj)
	     (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+))
	    
	    (t
	     ;; succesful and nothing returned.. do nothing, except waste energy
	     (incf (player.energy-use player) +energy-normal-action+)
	     ;; hack, fix later
	     (on-wear-object *variant* player removed-obj)
	     (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+)
	     ;; add window-stuff
	     ))
      )))

(defmethod apply-usual-effects-on-used-object! (dungeon player obj)
  (declare (ignore dungeon))

  (let* ((okind (etypecase obj
		 (active-object (aobj.kind obj))
		 (object-kind obj)))
	 (gvals (object.game-values okind)))
  
  ;; we have tried object
  ;;(tried-object)
  
    (when (is-eatable? player obj)
      (alter-food! player (+ (player.food player)
			 (gval.food-value gvals))))
    
    
    t))


(defun interactive-use-item! (dungeon player
			      &key
			      (need-effect nil)
			      (selection-function nil)
			      (prompt "Use item?")
			      (which-use :use)
			      (sound nil)
			      (limit-from '(:backpack :floor)))
  
  "Tries to use an item."


  (assert (consp limit-from))
  (assert (stringp prompt))

  (labels ((has-obj-effect (obj effect)
	     (find effect (object.effects (aobj.kind obj)) :key #'effect-entry-type))
	   (allowed-object (obj)
	     ;;(warn "checking ~s vs ~s, and have effects ~s" obj need-effect (object.effects (aobj.kind obj)))
	     (cond ((eq need-effect nil)
		    t)
		   ((consp need-effect)
		    (dolist (x need-effect)
		      (when (has-obj-effect obj x)
			(return-from allowed-object t)))
		    )
		   (t
		    (warn "Unknown need-effect value ~s" need-effect)
		    nil))))
  
  (let ((variant *variant*)
	(selection (with-frame (+query-frame+)
		     (select-item dungeon player limit-from
				  :prompt prompt
				  :where (first limit-from)
				  :selection-function (if selection-function
							  selection-function
							  #'allowed-object)
				  ))))

    (unless (and selection (consp selection))
      (return-from interactive-use-item! nil))

    
;;    (warn "Selected ~s for use" selection)
    (let* ((the-table (get-item-table dungeon player (car selection)))
	   (removed-obj (item-table-remove! the-table (cdr selection))))
      
;;      (warn "Removed ~a" removed-obj)
      
      (unless (and removed-obj (typep removed-obj 'active-object))
	(return-from interactive-use-item! nil))
      
      ;;(warn "Will ~a ~s" prompt removed-obj)
	  
      (let ((retval (use-object! variant dungeon player removed-obj :which-use which-use)))
;;	(warn "use returned ~s" retval)
	(cond ((or (eq retval nil) ;; didn't use the object for some reason..
		   (eq retval :not-used))
	       (%put-obj-in-cnt dungeon player the-table removed-obj))
	      
	      ((eq retval :still-useful) ;; we should keep it
	       (let ((back-obj (%put-obj-in-cnt dungeon player the-table removed-obj)))
		 ;; add energy use
		 (apply-usual-effects-on-used-object! dungeon player removed-obj)
		 (when sound
		   (play-sound sound))
		 back-obj))
	      
	      ((eq retval :used) ;; we have used the item
	       ;; decrement number
	       (when (> (aobj.number removed-obj) 1)
		 (decf (aobj.number removed-obj))
		 (%put-obj-in-cnt dungeon player the-table removed-obj))
;;	       (warn "used object ~a" removed-obj)
	       (apply-usual-effects-on-used-object! dungeon player removed-obj)
	       (when sound
		 (play-sound sound))
	       nil)
	      
	      (t
	       (warn "Fell through on use return-value: ~s" retval)))
	
	))
      )))


(defun open-door! (dungeon x y)
  "hackish, fix me later.."
  (when (is-closed-door? dungeon x y)
    (play-sound +sound-opendoor+)
    (decor-operation *variant* (cave-decor dungeon x y) :open :value t)
    ;;(setf (door.closed? (cave-decor dungeon x y)) nil)
    (light-spot! dungeon x y)))

(defun close-door! (dungeon x y)
  "hackish, fix me later.."
  (when (is-open-door? dungeon x y)
    (play-sound +sound-shutdoor+)
    (decor-operation *variant* (cave-decor dungeon x y) :close :value t)
;;    (setf (door.closed? (cave-decor dungeon x y)) t)
    (light-spot! dungeon x y)))

(defun bash-door! (dungeon x y)
  "hackish, fix me later.."
  ;; add paralysis, hitpoints, ...
  (warn "trying to bash door")
  (when (is-closed-door? dungeon x y)
    (when (= 1 (random 4)) ;; 1/4 chance
      (decor-operation *variant* (cave-decor dungeon x y) :break :value t)
      ;;(setf (door.broken? (cave-decor dungeon x y)) t)
      (light-spot! dungeon x y)
      )))


(defun interactive-door-operation! (dungeon player operation)

  (block door-operation

    (let ((check-predicate nil)
	  (operation-fun nil))
      
    (ecase operation
      (:open (setf check-predicate #'is-closed-door?
		   operation-fun #'open-door!))
      (:close (setf check-predicate #'is-open-door?
		    operation-fun #'close-door!))
      (:bash (setf check-predicate #'is-closed-door?
		   operation-fun #'bash-door!))
      (:jam
       (warn "Jamming is not properly implemented yet.")
       (return-from door-operation nil)))

;;    (warn "going ~s ~s ~s" operation check-predicate operation-fun)
     
    (let* ((x (location-x player))
	   (y (location-y player))
	   (ddx-ddd *ddx-ddd*)
	   (ddy-ddd *ddy-ddd*))
      

      ;; first check if there is just one door, then we operate on it

      (let ((count 0))
	(loop for i from 0 below +normal-direction-number+
	      for test-x = (+ x (svref ddx-ddd i))
	      for test-y = (+ y (svref ddy-ddd i))
	      do
	      (when (funcall check-predicate dungeon test-x test-y)
		(incf count)))

;;	(warn "Found ~s candidates" count)
	
	(cond ((= count 1)
	       (loop for i from 0 below +normal-direction-number+
		     for test-x = (+ x (svref ddx-ddd i))
		     for test-y = (+ y (svref ddy-ddd i))
		     do
		     (when (funcall check-predicate dungeon test-x test-y)
		       (funcall operation-fun dungeon test-x test-y)
		       (return-from door-operation t))))
	      ((= count 0)
	       ;;(print-message! "No doors near you.!")
	       (return-from door-operation t))
	      ))
      
      ;; then we need to find which door
      (let ((dir (get-aim-direction)))
	(cond ((integerp dir)
	       (let ((new-x (+ x (aref *ddx* dir)))
		     (new-y (+ y (aref *ddy* dir))))
		 ;;(warn "Checking ~s (~s,~s) from (~s,~s)" dir new-x new-y x y)
		 (if (funcall check-predicate dungeon new-x new-y)
		     (funcall operation-fun dungeon new-x new-y)
		     ;; maybe add message on failure?
		     nil)))

	      (t
	       nil)))
      ))
    ))

(defun disarm-trap! (dungeon x y)
  ;; hackish
  (let ((decor (cave-decor dungeon x y)))
    (when (and (typep decor 'active-trap)
	       (= (random 4) 1))
      (setf (cave-decor dungeon x y) nil)
      (setf (dungeon.decor dungeon) (delete decor (dungeon.decor dungeon)))
      (print-message! "You disarm the trap.")
      (light-spot! dungeon x y)
      t)))

(defun is-visible-trap? (dungeon x y)
  (let ((decor (cave-decor dungeon x y)))
    (and (typep decor 'active-trap)
	 (decor.visible? decor))))

   

(defun interactive-trap-operation! (dungeon player operation)

  (block trap-operation

    (let ((check-predicate nil)
	  (operation-fun nil))
      
    (ecase operation
      (:disarm (setf check-predicate #'is-visible-trap?
		     operation-fun #'disarm-trap!)))
    
    (let* ((x (location-x player))
	   (y (location-y player))
	   (ddx-ddd *ddx-ddd*)
	   (ddy-ddd *ddy-ddd*))
      
      
      ;; first check if there is just one trap, then we operate on it
      
      (let ((count 0))
	(loop for i from 0 below +normal-direction-number+
	      for test-x = (+ x (svref ddx-ddd i))
	      for test-y = (+ y (svref ddy-ddd i))
	      do
	      (when (funcall check-predicate dungeon test-x test-y)
		(incf count)))

	(cond ((= count 1)
	       (loop for i from 0 below +normal-direction-number+
		     for test-x = (+ x (svref ddx-ddd i))
		     for test-y = (+ y (svref ddy-ddd i))
		     do
		     (when (funcall check-predicate dungeon test-x test-y)
		       (funcall operation-fun dungeon test-x test-y)
		       (return-from trap-operation t))))
	      ((= count 0)
	       ;;(print-message! "No doors near you.!")
	       (return-from trap-operation t))
	      ))
      
      ;; then we need to find which trap
      (let ((dir (get-aim-direction)))
	(cond ((integerp dir)
	       (let ((new-x (+ x (aref *ddx* dir)))
		     (new-y (+ y (aref *ddy* dir))))
		 ;;(warn "Checking ~s (~s,~s) from (~s,~s)" dir new-x new-y x y)
		 (if (funcall check-predicate dungeon new-x new-y)
		     (funcall operation-fun dungeon new-x new-y)
		     ;; maybe add message on failure?
		     nil)))
	      
	      (t
	       nil)))
      ))
    ))


(defun %highlight-target (dungeon target)
  (check-type target target)
  (let ((legal (is-legal-target? dungeon target)))
    ;;  (warn "is at ~s,~s" (target.x target)	(target.y target))
    (put-cursor-relative! dungeon (target.x target)
			  (target.y target)
			  (if legal
			      :legal-crosshair
			      :bad-crosshair))))

	 

(defun %get-target (dungeon x y)
  (cond ((and (eql x (location-x *player*))
	      (eql y (location-y *player*)))
	 (make-target :obj *player* :x x :y y))
	
	((cave-monsters dungeon x y)
	 (make-target :obj (car (cave-monsters dungeon x y))
		      :x x :y y))
	
	(t
	 (make-target :obj (cave-coord dungeon x y)
		      :x x :y y))
	))


(defun %print-target (dungeon target)
  
  (let ((key-desc "[q,t,<dir>]") ;; add recall later
	(desc-str "Legal target!")
	(obj (target.obj target)))
    
    (cond ((and (typep obj 'active-monster)
		(amon.seen-by-player? obj))
	   (setf desc-str (get-creature-name obj)))
	  
	  ((typep obj 'dungeon-coord)
	   ;; add 'can see' check here
	   (let ((obj-table (cave-objects dungeon (target.x target) (target.y target))))
	     (when (and obj-table (typep obj-table 'item-table))

	       (let ((obj-len (items.cur-size obj-table))
		     (first-obj (item-table-find obj-table 0)))
		 (cond ((and (> obj-len 1) (aobj.marked first-obj))
			(setf desc-str "Pile of objects"))
		       (t
			(setf desc-str (with-output-to-string (s)
					 (write-obj-description *variant* first-obj s)))))))))
	  (t
	   nil))
    (with-frame (+message-frame+)
      (put-coloured-line! +term-white+ (format nil "~a ~a" desc-str key-desc) 0 0))

    ))

(defun %remove-target (target)
  (when (and target (typep target 'target))
    (let ((win (aref *windows* *map-frame*)))
      (multiple-value-bind (x y)
	  (get-relative-coords (target.x target) (target.y target))
	(setf (window-coord win +effect+ x y) 0)
	(paint-coord win x y)
	(flush-coords win x y 1 1)))))

;;; INPUT NOTE: needs to use more sophisticated input, e.g mouseclicks and keys
(defun interactive-targeting! (dungeon player)

  (let ((cur-target (player.target player)))

    (when (and cur-target
	       (typep (target.obj cur-target) 'active-monster)
	       (not (creature-alive? (target.obj cur-target))))
      (%remove-target cur-target)
      (setf cur-target nil))

    ;; always start at player if we have no old target
    (when (eq cur-target nil)
      (setf cur-target (make-target :obj player
				    :x (location-x player)
				    :y (location-y player))))

;;    (put-coloured-line! +term-white+ "Please use cursor keys to find target, use 't' to target and ESC to exit." 0 0)
    (set-cursor-visibility t)
    (block target-input
      (loop
       (%highlight-target dungeon cur-target)

       (let ((key (read-one-character))
	     (old-target cur-target))
	 (cond ((eql key #\t)
		(return-from target-input cur-target))
	       
	       ((digit-char-p key)
		(let ((num (digit-char-p key)))
		  (assert (and (integerp num) (< num 10)))
		  ;; move cursor
		  (let ((wanted-x (+ (aref *ddx* num)
				     (target.x cur-target)))
			(wanted-y (+ (aref *ddy* num)
				     (target.y cur-target))))
		    (when (and (in-bounds-fully? dungeon wanted-x wanted-y)
			       (panel-contains? player wanted-x wanted-y))
		      (setf cur-target (%get-target dungeon wanted-x wanted-y)))
		    )))

	       ((or (eql key #\q)
		    (eql key +escape+))
		(setf cur-target nil)
		(return-from target-input nil))
	       
	       (t
		(warn "weird key ~s" key))
	       )
	 (unless (eq old-target nil)
	   (%remove-target old-target))
	 (cond ((is-legal-target? dungeon cur-target)
		(%print-target dungeon cur-target))
	       ((eq cur-target nil)
		(with-frame (+message-frame+)
		  (put-coloured-line! +term-white+ "[No target]" 0 0))
		)
	       (t
		(with-frame (+message-frame+)
		  (put-coloured-line! +term-white+ "[Target isn't legal]" 0 0))
		))
	 )))

    (set-cursor-visibility nil)
    ;; flush!
    ;;(put-coloured-line! +term-white+ "" 0 0)
;;    (warn "New target is ~s" cur-target)
    (setf (player.target player) cur-target)
    
    nil))

(defun run-ok? (dungeon player x y)
  "Helper function to determine if a square might be legal to move to."
  (declare (ignore player))
  (unless (legal-coord? dungeon x y)
    (return-from run-ok? nil))

  ;; TILE_OUTSIDE is considered to be "wall" 
;;    if (MAP(x, y).is_kidx(LAYER_BASE, TILE_OUTSIDE))
;;        return (FALSE);

;;	/* We assume squares that can't be seen are ok */
;;	if (m_ptr == player && (!MAP(x, y).seen_tile))
;;		return (TRUE);

  ;; doors should be ok, add later
  #||
		
	/* Doors are ok - they can be opened */
    if (MAP(x, y).is_kidx(LAYER_FEAT, TILE_DOOR))
		return (TRUE);
    if (MAP(x, y).is_kidx(LAYER_FEAT, TILE_SECRET_DOOR) && IS_DETECTED(x, y))
		return (TRUE);
||#
  (if (cave-floor-bold? dungeon x y)
      t
      nil))


(defun careful-run-ok? (dungeon player x y)
  "Helper function to determine if a square might be legal to move to.
This function differs from run_ok() in that unseen squares are
considered illegal, instead of legal. This function is called from
run_in_room(), which should be conservative in deciding the player
is in a room."
  (declare (ignore player))
  (unless (legal-coord? dungeon x y)
    (return-from careful-run-ok? nil))

#||  
	/* We assume squares that can't be seen are _bad_ here */
	if (m_ptr == player && !IS_SEEN(x, y))
		return (FALSE);

	/* Doors are ok - they can be opened */
    if (MAP(x, y).is_kidx(LAYER_FEAT, TILE_DOOR))
		return (TRUE);
    if (MAP(x, y).is_kidx(LAYER_FEAT, TILE_SECRET_DOOR) && IS_DETECTED(x, y))
		return (TRUE);
||#

    (if (cave-floor-bold? dungeon x y)
      t
      nil))


(defun move-ok? (dungeon player x y)
  "Helper function to determine if a square is legal to move to."
  (declare (ignore player))
  (unless (legal-coord? dungeon x y)
    (return-from move-ok? nil))

#||
	/* We assume squares that can't be seen are ok */
	if (m_ptr == player && !IS_VISIBLE(x, y))
		return (TRUE);
||#

  (if (cave-floor-bold? dungeon x y)
      t
      nil))

(defun run-along-corridor (dungeon player last-dir)
  "Determine the appropriate direction to move to continue along
a corridor."

  (let ((ok (make-array 10 :initial-element nil))
	(px (location-x player))
	(py (location-y player)))

    ;; Determine provisionally legal destinations
    
    (when (or (= last-dir 1) (= last-dir 2) (= last-dir 3))
      (setf (svref ok 1) t
	    (svref ok 2) t
	    (svref ok 3) t))

    (when (or (= last-dir 1) (= last-dir 4) (= last-dir 7))
      (setf (svref ok 1) t
	    (svref ok 4) t
	    (svref ok 7) t))

    (when (or (= last-dir 7) (= last-dir 8) (= last-dir 9))
      (setf (svref ok 7) t
	    (svref ok 8) t
	    (svref ok 9) t))

    (when (or (= last-dir 3) (= last-dir 6) (= last-dir 9))
      (setf (svref ok 3) t
	    (svref ok 6) t
	    (svref ok 9) t))

;;    (warn "ok is ~s" ok)
    
    (when (typep player 'player)
      ;; This weirdness continues on after T-junctions correctly. 

      (when (and (= last-dir 1) (run-ok? dungeon player px (1- py)))
	(setf (svref ok 7) nil))

      (when (and (= last-dir 1) (run-ok? dungeon player (1+ px) py))
	(setf (svref ok 3) nil))

      (when (and (= last-dir 3) (run-ok? dungeon player px (1- py)))
	(setf (svref ok 9) nil))

      (when (and (= last-dir 3) (run-ok? dungeon player (1- px) py))
	(setf (svref ok 1) nil))

      
      (when (and (= last-dir 7) (run-ok? dungeon player px (1+ py)))
	(setf (svref ok 1) nil))

      (when (and (= last-dir 7) (run-ok? dungeon player (1+ px) py))
	(setf (svref ok 9) nil))


      (when (and (= last-dir 9) (run-ok? dungeon player px (1+ py)))
	(setf (svref ok 3) nil))

      (when (and (= last-dir 9) (run-ok? dungeon player (1- px) py))
	(setf (svref ok 7) nil))

      )

;;    (warn "ok is ~s" ok)
    
    ;; eliminate impossible squares
    (dotimes (i 10)
      (when (and (svref ok i)
		 (not (run-ok? dungeon player (+ (aref *ddx* i) px)
			       (+ (aref *ddy* i) py))))
	(setf (svref ok i) nil)))

    ;; Eliminate orthagonals next to good diagonals 
    (when (svref ok 1)
      (setf (svref ok 2) nil
	    (svref ok 4) nil))
    
    (when (svref ok 3)
      (setf (svref ok 2) nil
	    (svref ok 6) nil))

    (when (svref ok 7)
      (setf (svref ok 4) nil
	    (svref ok 8) nil))

    (when (svref ok 9)
      (setf (svref ok 6) nil
	    (svref ok 8) nil))

;;    (warn "ok is ~s" ok)

    ;; Reinstate some orthagonals, to traverse junctions correctly

    (when (and (or (= last-dir 3) (= last-dir 6) (= last-dir 9))
	       (run-ok? dungeon player (+ px 1) py)
      	       (run-ok? dungeon player (+ px 2) py))
      (setf (svref ok 6) t))

    (when (and (or (= last-dir 1) (= last-dir 4) (= last-dir 7))
	       (run-ok? dungeon player (- px 1) py)
      	       (run-ok? dungeon player (- px 2) py))
      (setf (svref ok 4) t))

    (when (and (or (= last-dir 1) (= last-dir 2) (= last-dir 3))
	       (run-ok? dungeon player px (+ py 1))
      	       (run-ok? dungeon player px (+ py 2)))
      (setf (svref ok 2) t))

    (when (and (or (= last-dir 7) (= last-dir 8) (= last-dir 9))
	       (run-ok? dungeon player px (- py 1))
      	       (run-ok? dungeon player px (- py 2)))
      (setf (svref ok 8) t))

;;    (warn "ok is ~s" ok)

    ;; Count the okay moves & pick one at random
    (let ((good 0)
	  (dir 0))
      (dotimes (i 10)
	(when (svref ok i)
	  (incf good)
	  ;; odd
	  (when (= (random good) 0)
	    (setf dir i))))

;;      (warn "good ~s" good)
      (cond ((= good 0) ;; no good moves
	     0)
	    ((and (typep player 'player) (> good 1)) ;; player choose
	     0)	    
	    ;; move into a door
	    ;;if (!move_ok(m_ptr, m_ptr->x + keyx[dir], m_ptr->y + keyy[dir]))
	    ;; return (0);
	    (t
	     dir))
      )))

(defmethod on-move-to-coord (variant creature x y)
  (declare (ignore variant x y))
  creature)
