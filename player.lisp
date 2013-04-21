;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: player.lisp - code for the character object
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (bt:define-binary-struct (player (:conc-name player.)) ()

    ;; === Need Special saving ===
    
    (name "Foo")
    (class nil)
    (race nil)
    (sex nil)

    (base-stats nil);; "this is the base stats"
    (curbase-stats nil);; "this is the current (possibly drained) base stats"
    (hp-table    nil) ;; should be saved
    (equipment   nil)

    ;; === Directly savable to binary ===

    (loc-x +illegal-loc-x+ :bt u16)
    (loc-y +illegal-loc-y+ :bt u16)

    (view-x +illegal-loc-x+ :bt u16);; wx
    (view-y +illegal-loc-y+ :bt u16);; wy
    
    (depth     0 :bt s16)
    (max-depth 0 :bt s16)
    
    (max-xp      0 :bt u32)
    (cur-xp      0 :bt u32)
    (fraction-xp 0 :bt u32) 

    (cur-hp      0 :bt u32)
    (fraction-hp 0 :bt u32)

    (cur-mana      0 :bt u32)
    (fraction-mana 0 :bt u32)

    (gold        0 :bt u32)
    (energy      0 :bt u16)

    ;; === The remaining values can be calculated from the above ===
    
    (level     1)  ;; can be calculated from cur-xp
    (max-level 1)  ;; can be calculated from max-xp

    (max-hp      0)   ;; can be calculated
    (max-mana    0)   ;; can be calculated
    (xp-table    nil) ;; can be calculated
    
    (energy-use  0)   ;; is just a temp-variable
    (leaving-p   nil) ;; need to save it?
    (dead-p      nil) ;; need to save it?
    (speed       +speed-base+)  ;; does this change?
    

    (base-ac      0)
    (ac-bonus     0)
    (light-radius 0)
   
    (infravision 0)
    (inventory   nil) ;; quick variable to equipment.backpack.content
    (skills      nil)

    (modbase-stats nil);; "this is the modified base stats (base + race + class + eq)"
    (active-stats nil);; "this is the current active stat-value (curbase + race + class + eq)"
  
    ))

;; hack, remove later
(defun player.eq (pl-obj)
  (player.equipment pl-obj))

(defun (setf player.eq) (val pl-obj)
  (setf (player.equipment pl-obj) val))

(defmethod location-x ((obj player))
  (player.loc-x obj))
(defmethod location-y ((obj player))
  (player.loc-y obj))
  
(defmethod (setf location-x) (value (obj player))
  (setf (player.loc-x obj) value))
(defmethod (setf location-y) (value (obj player))
  (setf (player.loc-y obj) value))

(defmethod current-hp ((crt player))
  (player.cur-hp crt))

(defmethod (setf current-hp) (value (crt player))
  (setf (player.cur-hp crt) value))
  
(defmethod creature-alive? ((crt player))
  (not (player.dead-p crt)))

(defmethod (setf creature-alive?) (value (crt player))
  (setf (player.dead-p crt) (not value))
  (when (eq value nil)
    (setf (player.leaving-p crt) :dead)))
    

(defmethod get-xp-value ((crt player))
  (* (player.level crt) 20))


(defun get-race-name (player)
  "Returns a string with the name of the race."
  (race.name (player.race player)))

(defun get-class-name (player)
  "Returns a string with the name of the class."
  (class.name (player.class player)))

(defun get-sex-name (player)
  "Returns a string with the name of the sex."
  (cadr (assoc (player.sex player) +sexes+)))

(defmethod get-creature-ac ((crt player))
  (+ (player.base-ac crt)
     (player.ac-bonus crt)))

(defmethod get-creature-energy ((crt player))
  (player.energy crt))

(defmethod (setf get-creature-energy) (val (crt player))
;;  (when (< val (player.energy crt)) (warn "Reducing energy from ~a to ~a" (player.energy crt) val))
  (setf (player.energy crt) val))

(defmethod get-creature-speed ((crt player))
  (player.speed crt))

(defmethod (setf get-creature-speed) (val (crt player))
  (setf (player.speed crt) val))

(defun make-level-array ()
  (let ((max-char-level (variant.max-charlevel *variant*)))
    (make-array max-char-level :initial-element 0)))



(defun init-player-obj! (t-p)
  "Creates and returns a PLAYER object."
  
    (setf (player.base-stats t-p)    (make-stat-array)
	  (player.curbase-stats t-p) (make-stat-array)
	  (player.modbase-stats t-p) (make-stat-array)
	  (player.active-stats t-p)  (make-stat-array))

    (setf (player.skills t-p) (make-skills))
    (setf (player.eq t-p) (make-equipment-slots))
    
    (setf (player.hp-table t-p) (make-level-array)
	  (player.xp-table t-p) (make-level-array))
    
    (flet ((make-and-assign-backpack! (id)
	     (let ((back-obj (create-aobj-from-id id))
		   (eq-slots (player.eq t-p)))
	       ;;(warn "eq-slots ~a" eq-slots)
	       (item-table-add! eq-slots back-obj 'eq.backpack)
	       (setf (player.inventory t-p) back-obj))))
      
      (let ((backpack-val (game-parameter-value :initial-backpack)))
	(case backpack-val
	  (:backpack (make-and-assign-backpack! :backpack))
	  (otherwise
	   (warn "No initial known backpack-setting, assuming :backpack")
	   (make-and-assign-backpack! :backpack)))))

    ;; hack
;;    (setf (player.light-radius t-p) 3)
    
    t-p)

(defun create-player-obj ()
  (init-player-obj! (make-player)))

;;(trace init-player-obj!)

(defun get-stat-bonus (player stat-num)
  "Returns the stat-bonus from race, class and equipment for given stat"
  
  (let ((race-mod (race.stat-changes (player.race player)))
	(class-mod (class.stat-changes (player.class player))))
    
    ;; iterate through equipment
    (+ (svref race-mod stat-num)
       (svref class-mod stat-num))))
    
(defun add-stat-bonus (base amount)
  "Returns a numeric value with base incremented with amount"
  (let ((retval base))
    (if (< amount 0)
	(dotimes (i (abs amount))
	  (cond ((>= retval (+ 18 10))
		 (decf retval 10))
		((> retval 18) ;; hackish
		 (setq retval 18))
		((> retval 3) ;; minimum
		 (decf retval 1))
		(t
		 ;; too low to care
		 )))
	;; positive amount
	(dotimes (i amount)
	  (if (< retval 18)
	      (incf retval)
	      (incf retval 10))))
    retval))
		

(defun calculate-stat! (player num)
  "modifies appropriate arrays.."

  (let ((bstat-table (player.base-stats player))
	(cstat-table (player.curbase-stats player))
	(mstat-table (player.modbase-stats player))
	(astat-table (player.active-stats player)))
	
    ;; two of these are required.. bstat and cstat

  
    (let ((base-stat (svref bstat-table num))
	  (cur-stat (svref cstat-table num))
	  (bonus (get-stat-bonus player num)))

      (setf (svref mstat-table num) (add-stat-bonus base-stat bonus))
      (setf (svref astat-table num) (add-stat-bonus cur-stat bonus)))
  
    (values)))

(defun update-player! (player)
  "modifies player object appropriately"

  ;; we start the show by reseting variables

  ;; reset some key variables
  (setf (player.base-ac player) 0
	(player.ac-bonus player) 0
	(player.light-radius player) 0
	(player.speed player) +speed-base+)
  
  
  (let ((race (player.race player)))

    ;; if cur and base are missing, that should be fixed elsewhere
    
    (unless (arrayp (player.modbase-stats player))
      (setf (player.modbase-stats player) (make-stat-array)))
    (unless (arrayp (player.active-stats player))
      (setf (player.active-stats player) (make-stat-array)))

    ;; check that they're all there
    (assert (and (arrayp (player.base-stats player))
		 (arrayp (player.curbase-stats player))
		 (arrayp (player.active-stats player))
		 (arrayp (player.modbase-stats player))))

    
    (dotimes (i +stat-length+)
      (calculate-stat! player i))


    ;; hackish, change later
    (let ((race-ab (race.abilities race)))
      (dolist (i race-ab)
;;	(Warn "checking ~a" i)
	(when (consp i)
	  (case (car i)
	    (<infravision> (setf (player.infravision player) (cadr i)))
	    (<resist> ;; handle later
	     )
	    (otherwise
	     #+cmu ;; FIX
	     (warn "Unhandled racial ability ~a" (car i)))))))


     
    ;; let us skim through items and update variables
    (let ((slots (player.eq player)))
      (unless slots
	(error "Can't find equipment-slots for player, bad."))
      (item-table-iterate!
       slots
       #'(lambda (table key obj)
	   (declare (ignore table key))
	   (when obj
	     (let* ((kind (aobj.kind obj))
		    (gval (object.game-values kind)))
	       (when gval
		 (when (> (gval.light-radius gval) (player.light-radius player))
		   (setf (player.light-radius player) (gval.light-radius gval)))
		 (incf (player.base-ac player) (gval.base-ac gval))
		 (incf (player.ac-bonus player) (gval.ac-bonus gval))))))
       ))

    (update-xp-table! player)

    (let ((xp-table (player.xp-table player)))
      (setf (player.level player) (find-level-for-xp (player.cur-xp player)
						     xp-table)
	    (player.max-level player) (find-level-for-xp (player.max-xp player)
							 xp-table)))

    (update-max-hp! player)
    (update-skills! player (player.skills player))
    
    (bit-flag-add! *redraw* +print-basic+)

    ;; when leavin we should be ok
    (assert (ok-object? player))
    player))


(defun gain-level! (player)
  "lets the player gain a level.. woah!  must be updated later"

  (let* ((the-level (player.level player))
	 (hp-table (player.hp-table player))
	 (next-hp (aref hp-table the-level)))

    ;; we have been to this level earlier..
    (unless next-hp
      (let* ((the-class (player.class player))
	     (the-race (player.race player))
	     (hit-dice (+ (class.hit-dice the-class)
			  (race.hit-dice the-race))))
	(setq next-hp (random hit-dice))
	(setf (aref hp-table the-level) next-hp)))

    (incf (player.max-hp player) next-hp)
    (incf (player.level player))

    (when (< (player.max-level player) (player.level player))
      (setf (player.max-level player)  (player.level player)))
	     
    
    ))

(defun find-level-for-xp (xp xp-table)
  "Returns level for given xp according to given xp-table."
  (loop for x across xp-table
	for i from 1
	do
	(when (> x xp)
	  (return-from find-level-for-xp (1- i))))
  50) ;; fix me later

;;(trace find-level-for-xp)

(defmethod increase-xp! ((player player) amount)
  "increases xp for the player. update later."

  (incf (player.cur-xp player) amount)
  (incf (player.max-xp player) amount)

  (loop
   (let* ((cur-level (player.level player))
	  (next-limit (aref (player.xp-table player) cur-level)))
  
     (if (> (player.cur-xp player) next-limit)
	 (gain-level! player)
	 (return-from increase-xp! nil)))
  
   ))


(defmethod get-weapon ((crt player))
  (let ((the-eq (player.eq crt)))
    (assert (typep the-eq 'item-table))
    (item-table-find the-eq 'eq.weapon)))


(defun reset-skills! (variant skills-obj reset-val)
  "Sets all skills to RESET-VAL."
  (dolist (i (variant.skill-translations variant))
    (setf (slot-value skills-obj (cdr i)) reset-val)))


(defun update-skills! (player skills-obj)
  "Recalculates and cleans up as needed."

  (flet ((add-to-skill! (which the-skills-obj player-lvl source)
	   (when source
	     (let ((obj (slot-value source which)))
	       (if (not obj)
		   (warn "Skill-slot ~s does have NIL value, please fix." which)
		   (incf (slot-value the-skills-obj which)
			 (cond ((eq obj nil) 0)
			       ((numberp obj) obj)
				 ((skill-p obj)
				  (+ (skill.base obj)
				     (int-/ (* player-lvl (skill.lvl-gain obj))
					    10)))
				 (t
				  (error "Unknown skill-obj ~a" obj)))))))))
    
  (let* ((var-obj *variant*)
	 (race (player.race player))
	 (the-class (player.class player))
	 (racial-skills (race.skills race))
	 (class-skills (class.skills the-class))
	 (player-lvl (player.level player))
	 (skill-list (variant.skill-translations var-obj)))
    
    ;; reset to value 0 first
    (reset-skills! var-obj skills-obj 0)

    (dolist (i skill-list)
      
      (add-to-skill! (cdr i)
		     skills-obj
		     player-lvl
		     racial-skills)
      (add-to-skill! (cdr i)
		     skills-obj
		     player-lvl
		     class-skills))

;;    (describe skills-obj)
    
    t)))

(defun update-xp-table! (player)
  "Updates the xp-table on the player, and returns updated player."
  
  (let* ((base-xp-table (variant.xp-table *variant*))
	 (max-char-level (variant.max-charlevel *variant*))
	 (the-race (player.race player))
	 (the-class (player.class player))
	 (xp-extra (+ 100
		      (race.xp-extra the-race)
		      (class.xp-extra the-class))))

    (unless (arrayp (player.xp-table player))
      (setf (player.xp-table player) (make-level-array)))

    (let ((xp-table (player.xp-table player)))
    
      (setf (aref xp-table 0) 0)
      (loop for i of-type u-fixnum from 1 to (1- max-char-level)
	    do
	    (setf (aref xp-table i) (int-/ (* (aref base-xp-table (1- i)) xp-extra)
					   100))))
    player))

(defun update-max-hp! (player)
  "Updates the maximum number of hitpoints.  Returns an updated player."

  (let ((lvl (player.level player))
	(hp-table (player.hp-table player)))
    
    (setf (player.max-hp player)
	  (loop for i from 0 to (1- lvl)
		summing (aref hp-table i))))
  player)
	   
