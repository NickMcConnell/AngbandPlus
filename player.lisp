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


(def-saveable-struct
    (player (:conc-name player.))
  
    (name "Foo")
  (class nil)
  (race nil)
   (sex nil)
   
   (base-stats nil) ;; "this is the base stats"
   (curbase-stats nil) ;; "this is the current (possibly drained) base stats"
   (modbase-stats nil) ;; "this is the modified base stats (base + race + class + eq)"
   (active-stats nil) ;; "this is the current active stat-value (curbase + race + class + eq)"

   (loc-x nil)
   (loc-y nil)
   
   (view-x nil)  ;; wx
   (view-y nil)  ;; wy
   
   (depth     0)
   (max-depth nil)
   (level     1)
   (max-level 1)

   (xp-table    nil)
   (max-xp      0)
   (cur-xp      0)
   (fraction-xp 0) 
   
   (hp-table    nil)
   (max-hp      0)
   (cur-hp      0)
   (fraction-hp 0)

   (max-mana      0)
   (cur-mana      0)
   (fraction-mana 0)

   
   (leaving-p nil)

   (energy      0)
   (energy-use  0)
   (speed       +speed-base+)

   (base-ac      0)
   (ac-bonus     0)
   (light-radius 0)
   
   (gold        0)
   (infravision 0)

   (dead-p      nil)

   (equipment   nil)
   ;; quick variable to equipment.backpack.content
   (inventory   nil)

   (skills      nil)

   ;; the map as seen from the player.
   (map         nil) 
   
   )

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

(defun create-player-obj ()
  "Creates and returns a PLAYER object."
  
  (let ((t-p (make-player)))
    (setf (player.base-stats t-p) #1A(0 0 0 0 0 0))
    (setf (player.curbase-stats t-p) #1A(0 0 0 0 0 0))
    (setf (player.modbase-stats t-p) #1A(0 0 0 0 0 0))
    (setf (player.active-stats t-p) #1A(0 0 0 0 0 0))

    (setf (player.skills t-p) (make-skills))
    (setf (player.eq t-p) (make-equipment-slots))

    (let ((max-char-level (variant.max-charlevel *variant*)))
      (setf (player.hp-table t-p) (make-array max-char-level :initial-element nil))
      (setf (player.xp-table t-p) (make-array max-char-level :initial-element nil)))
    
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
    
    t-p))
 

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
  
  (let ((base-stat (svref (player.base-stats player) num))
	(cur-stat (svref (player.curbase-stats player) num))
	(bonus (get-stat-bonus player num)))

    (setf (svref (player.modbase-stats player) num) (add-stat-bonus base-stat bonus))
    (setf (svref (player.active-stats player) num) (add-stat-bonus cur-stat bonus)))
  
  (values))

(defun update-player! (player)
  "modifies player object appropriately"

  ;; we start the show by reseting variables

  ;; reset some key variables
  (setf (player.base-ac player) 0
	(player.ac-bonus player) 0
	(player.light-radius player) 0
	(player.speed player) +speed-base+)
  
  
  (let ((race (player.race player)))
  
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

    (update-skills! player (player.skills player))
    
    (bit-flag-add! *redraw* +print-basic+)
    
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


;; map related stuff:
(defun cave-info-from-map (player x y)
  (aref (player.map player) x y))



(defun (setf cave-info-from-map) (val player x y)
;;  (warn "Setting ~a ~a to ~a" x y val)
  (setf (aref (player.map player) x y) val)
  ;; more
  )


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


#||
(defmethod print-object ((inst l-player) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A ~A]" (class-name (class-of inst))
	   (player.name inst)
	   (player.race inst)
	   (player.class inst)))
  inst)
||#
