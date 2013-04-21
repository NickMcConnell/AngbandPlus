;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: player.lisp - code for the character object
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun make-old-player-info (variant)
  "creates and returns a freshly init'ed OLD-PLAYER-INFO object."
  (let ((old (make-instance 'old-player-info)))
    (setf (old.stats old) (make-stat-array variant)
	  (old.abilities old) (make-instance 'player-abilities))
    old))


(defun get-attribute-value (key table)
  "Returns the value of the attribute identified by KEY in the TABLE."
  (let ((val (gethash key table)))
    (check-type val player-attribute)
    (attr.value val)))

(defun add-player-attribute (player attr)
  (ecase (attr.type attr)
    (:calculated (setf (gethash (attr.key attr) (player.calc-attrs player))
		       attr))
    (:temporary (setf (gethash (attr.key attr) (player.temp-attrs player))
		       attr))))

(defun make-player-attribute (name key &key type desc value default-value turned-on-msg turned-off-msg update-fun on-update)
  (case type
    (:temporary
     (make-instance 'temp-player-attribute
		    :name name :key key :type type :desc desc
		    :value value :default-value default-value
		    :duration 0 :turned-on-msg turned-on-msg
		    :turned-off-msg turned-off-msg
		    :on-update on-update
		    :update-fun update-fun))
    (otherwise
     (make-instance 'player-attribute
		    :name name :key key :type type :desc desc
		    :value value :default-value default-value))
    ))


;; hack, remove later
(defun player.eq (pl-obj)
  (player.equipment pl-obj))

(defun (setf player.eq) (val pl-obj)
  (setf (player.equipment pl-obj) val))

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

(defun get-gender-name (player)
  "Returns a string with the name of the gender."
  (gender.name (player.gender player)))

(defmethod get-creature-ac ((crt player))
  (let ((actual (player.actual-abilities crt)))
    (the fixnum (+ (the fixnum (pl-ability.base-ac actual))
		   (the fixnum (pl-ability.ac-modifier actual))))))

(defmethod get-weight ((crt player))
  (the fixnum (+ (the fixnum (player.burden crt))
		 (the fixnum (playermisc.weight (player.misc crt))))))

(defmethod get-creature-burden ((crt player))
  (player.burden crt))

(defmethod get-creature-energy ((crt player))
  (the fixnum (player.energy crt)))

(defmethod (setf get-creature-energy) (val (crt player))
;;  (when (< val (player.energy crt)) (warn "Reducing energy from ~a to ~a" (player.energy crt) val))
  (setf (player.energy crt) val))

(defmethod get-creature-speed ((crt player))
  (the fixnum (player.speed crt)))

(defmethod (setf get-creature-speed) (val (crt player))
  (setf (player.speed crt) val))

(defun get-chance (variant skill the-ac)
  (declare (ignore variant))
  (let* ((ac-factor (int-/ (* 3 the-ac) 4))
	 (calc-chance (int-/ (* 90 (- skill ac-factor)) skill)))
    (if (plusp calc-chance)
	(+ 5 calc-chance)
	5)))


(defun %make-level-array (var-obj)
  (check-type var-obj variant)
  (let ((max-char-level (variant.max-charlevel var-obj)))
    (make-array max-char-level :initial-element 0)))



(defmethod produce-player-object ((variant variant))
  "Creates and returns a PLAYER object."
  (let ((t-p (make-instance 'player)))
  
    (setf (player.base-stats t-p)    (make-stat-array variant)
	  (player.cur-statmods t-p) (make-stat-array variant)
	  (player.modbase-stats t-p) (make-stat-array variant)
	  (player.active-stats t-p)  (make-stat-array variant))

    (setf (player.misc t-p) (make-instance 'misc-player-info) ;; fix to allow variants to override
	  (player.perceived-abilities t-p) (make-instance 'player-abilities) ;; fix to allow variants to override
	  (player.actual-abilities t-p) (make-instance 'player-abilities)) ;; fix to allow variants to override
    
    #+langband-extra-checks
    (assert (let ((bstat-table (player.base-stats t-p))
		  (cstat-table (player.cur-statmods t-p))
		  (mstat-table (player.modbase-stats t-p))
		  (astat-table (player.active-stats t-p)))
	      
	      (and (not (eq bstat-table cstat-table))
		   (not (eq bstat-table mstat-table))
		   (not (eq bstat-table astat-table))
		   (not (eq cstat-table mstat-table))
		   (not (eq cstat-table astat-table))
		   (not (eq mstat-table astat-table)))))

    ;; hack to get things moving!
;;    (setf (creature.attributes t-p) (make-instance 'calculated-attributes))
    
    (setf (player.skills t-p) (produce-skills-object variant))
    (setf (player.eq t-p) (make-equipment-slots))
    
    (setf (player.hp-table t-p) (%make-level-array variant)
	  (player.xp-table t-p) (%make-level-array variant)
	  )

    (flet ((make-and-assign-backpack! (id)
	     (let ((back-obj (create-aobj-from-id id))
		   (eq-slots (player.eq t-p)))
	       ;;(warn "eq-slots ~a" eq-slots)
	       (item-table-add! eq-slots back-obj 'eq.backpack)
	       (setf (player.inventory t-p) back-obj))))
      
      (let ((backpack-val (game-parameter-value :initial-backpack)))
	(case backpack-val
	  (:backpack (make-and-assign-backpack! "backpack"))
	  (otherwise
	   (warn "No initial known backpack-setting, assuming \"backpack\"")
	   (make-and-assign-backpack! "backpack")))))

    ;; hack
    ;;    (setf (player.light-radius t-p) 3)
    
    t-p))


(defun get-stat-bonus (player stat-num)
  "Returns the stat-bonus from race, class and equipment for given stat"
  
  (let ((race-mod (race.stat-changes (player.race player)))
	(class-mod (class.stat-changes (player.class player))))
    
    ;; iterate through equipment
    (the fixnum (+ (the fixnum (svref race-mod stat-num))
		   (the fixnum (svref class-mod stat-num))))))

(defun add-stat-bonus (base amount)
  "Returns a numeric value with base incremented with amount"
  (declare (type fixnum base amount))
  (let ((retval base))
    (declare (type fixnum retval))
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

  (let* ((var-obj *variant*)
	 (stat-len (variant.stat-length var-obj))
	 (bstat-table (player.base-stats player))
	 (cstat-table (player.cur-statmods player))
	 (mstat-table (player.modbase-stats player))
	 (astat-table (player.active-stats player))
	 )

    (assert (and (>= num 0)
		 (< num stat-len)))
    
    #+langband-extra-checks
    (assert (and (not (eq bstat-table cstat-table))
		 (not (eq bstat-table mstat-table))
		 (not (eq bstat-table astat-table))
		 (not (eq cstat-table mstat-table))
		 (not (eq cstat-table astat-table))
		 (not (eq mstat-table astat-table))))
    
    ;; two of these are required.. bstat and cstat
    
    
    (let ((base-stat (svref bstat-table num))
	  ;;	  (cur-stat (svref cstat-table num))
	  (bonus (get-stat-bonus player num)))
      
      (setf (svref mstat-table num) (add-stat-bonus base-stat bonus))
      (setf (svref astat-table num) (+ (svref cstat-table num)
				       (svref mstat-table num)))
      )
    
    (values)))

(defmethod calculate-creature-hit-points! ((variant variant) (player player))

  t)

(defmethod calculate-creature-light-radius! ((variant variant) (player player))
  (let ((old-val (player.light-radius player))
	(slots (player.eq player)))
    (unless slots
      (error "Can't find equipment-slots for player, bad."))
    
    ;; hackish, simplify later
    (flet ((item-iterator (table key obj)
	     (declare (ignore table key))
	     (when obj
	       (when-bind (gvals (aobj.game-values obj))
		 ;; fix light-radius first
		 (when (> (gval.light-radius gvals) (player.light-radius player))
		   (setf (player.light-radius player) (gval.light-radius gvals)))
		 ))))
      
      (declare (dynamic-extent #'item-iterator))
      (item-table-iterate! slots #'item-iterator))
    (when (/= old-val (player.light-radius player))
      (bit-flag-add! *update* +pl-upd-update-view+ +pl-upd-monsters+)
      t)))



(defmethod get-old-player-info ((variant variant) (player player) &key (reuse-object nil))

  (let ((old (cond ((and reuse-object (typep reuse-object 'old-player-info))
		    reuse-object)
		   (reuse-object
		    (warn "Unknown type ~s for reuse-object, using a new object." (type-of reuse-object)))
		   (t
		    (make-old-player-info variant))))
	(perc-abs (player.perceived-abilities player))
	(active-stats (player.active-stats player)))
    
    (fill-player-abilities! variant (old.abilities old) perc-abs)
    
    (let ((old/stats (old.stats old))
	  (stat-len (variant.stat-length variant)))
      (dotimes (i stat-len)
	(setf (aref old/stats i) (aref active-stats i))))

    (setf (old.see-inv old) (player.see-invisible player))
    
    old))

(defmethod handle-player-updates! ((variant variant) (player player) (old old-player-info))
  (let ((old/abilities (old.abilities old))
	(perc-abs (player.perceived-abilities player)))
    
    ;; only check perceived changes!
    (when (or (/= (pl-ability.base-ac old/abilities) (pl-ability.base-ac perc-abs))
	      (/= (pl-ability.ac-modifier old/abilities) (pl-ability.ac-modifier perc-abs)))
      (bit-flag-add! *redraw* +print-armour+)
      ;; skip window
      )

    (when (/= (player.see-invisible player) (old.see-inv old))
      (bit-flag-add! *update* +pl-upd-monsters+))

    player))

#||
(defun %reset-plattr (key table)
  (let ((attr (gethash key table)))
    (check-type attr player-attribute)
    (setf (attr.value attr) (attr.default-value attr))))
||#


(defmethod reset-player-object! ((variant variant) (player player))

  (let ((actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player))
	(active-stats (player.active-stats player))
	(modbase-stats (player.modbase-stats player))
	(stat-len (variant.stat-length variant)))

    (reset-skills! variant (player.skills player) 0)

    
    (setf (pl-ability.base-ac actual-abs) 0
	  (pl-ability.ac-modifier actual-abs) 0
	  (pl-ability.base-ac perc-abs) 0
	  (pl-ability.ac-modifier perc-abs) 0
	  (player.burden player) 0
	  (player.speed player) 110)

    (dotimes (i stat-len)
      (setf (aref active-stats i) 0)
      (setf (aref modbase-stats i) 0))
    
    t))

(defun alter-attribute! (key table new-value)
  (let ((attr (gethash key table)))
    (check-type attr player-attribute)
    (setf (attr.value attr) new-value)
    new-value))
    

(defmethod calculate-abilities! ((variant variant) (player player) (race character-race))
  
  (dolist (i (variant.skill-translations variant))
      (%add-to-a-skill! (cdr i)
			(player.skills player)
			(player.level player)
			(race.skills race)))
  t)


(defmethod calculate-abilities! ((variant variant) (player player) (cls character-class))
  
  (dolist (i (variant.skill-translations variant))
    (%add-to-a-skill! (cdr i)
		      (player.skills player)
		      (player.level player)
		      (class.skills cls)))

  t)

(defmethod calculate-abilities! ((variant variant) (player player) (items items-worn))
  "Handles ac and burden."
  ;; we don't use iterator, but access directly
  (let ((actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player)))

  (loop for obj across (items.objs items)
	do
	(when obj
	  (when-bind (gvals (aobj.game-values obj))
	    (incf (pl-ability.base-ac actual-abs) (gval.base-ac gvals))
	    ;; armour-value always known?  (move to variant?)
	    (incf (pl-ability.base-ac perc-abs) (gval.base-ac gvals))
	    (incf (pl-ability.ac-modifier actual-abs) (gval.ac-modifier gvals))
	    ;; sometimes we know bonus
	    (when (is-object-known? obj)
	      (incf (pl-ability.ac-modifier perc-abs) (gval.ac-modifier gvals)))
	    ;; fix this
;;	    (bit-flag-add! (creature.resists player) (gval.resists gvals))
	    (incf (player.burden player) (object.weight obj))
	    )))
  
;;  (warn "item-calc")
  t))

(defmethod get-weight ((items items-in-container))
  "Returns the weight as a fixnum of the combined total of the container."
  (let ((ret-weight 0))
    (loop for obj across (items.objs items)
	  do
	  (when (and obj (typep obj 'active-object))
	    (incf ret-weight (object.weight obj))))
    ret-weight))

;; move to variant later
(defvar *hack-old/player-info* nil)

(defmethod calculate-creature-bonuses! ((variant variant) (player player))
  "This method is relatively often.  It should not cons!"

  ;;; must be fixed
  (when (eq *hack-old/player-info* nil)
    (setf *hack-old/player-info* (make-old-player-info variant)))
  
  ;; we need to save old values first
  (let ((stat-len (variant.stat-length variant))
	(old (get-old-player-info variant player
				  :reuse-object *hack-old/player-info*))) 
    
    ;;; reset all values that should be filled
    (reset-player-object! variant player)
    
    ;;; then calculate things based on race and class (fairly constant)
    (calculate-abilities! variant player (player.race player))
    (calculate-abilities! variant player (player.class player))

    ;;; calculate based on what you're wearing
    (calculate-abilities! variant player (player.eq player))
    
    ;; recalculate stats, still needed?
    (dotimes (i stat-len)
      (calculate-stat! player i))
    
    (let ((backpack-weight (get-weight (aobj.contains (player.inventory player)))))
      (incf (player.burden player) backpack-weight))

    ;; check what has happened, and do necessary updates
    (handle-player-updates! variant player old)

	      
    t))


(defun gain-level! (player)
  "lets the player gain a level.. woah!  must be updated later"

  (let* ((the-level (player.level player))
	 (hp-table (player.hp-table player))
	 (next-hp (aref hp-table the-level)))

    ;; we have been to this level earlier..
    (when (or (eq next-hp nil) (< next-hp 1))
      (let* ((the-class (player.class player))
	     (the-race (player.race player))
	     (hit-dice (the fixnum (+ (the fixnum (class.hit-dice the-class))
				      (the fixnum (race.hit-dice the-race))))))
	(setq next-hp (randint hit-dice))
	(setf (aref hp-table the-level) next-hp)))

    (incf (maximum-hp player) next-hp)
    (incf (player.level player))

    (when (< (player.max-level player) (player.level player))
      (setf (player.max-level player)  (player.level player)))

    (with-foreign-str (s)
      (lb-format s "You attain level ~d and ~d new hitpoints. " (player.level player) next-hp)
      (c-print-message! s))

    (bit-flag-add! *update* +pl-upd-hp+ +pl-upd-bonuses+ +pl-upd-mana+ +pl-upd-spells+)
    (bit-flag-add! *redraw* +print-level+ +print-title+ +print-xp+)
    
    ))

(defun find-level-for-xp (xp xp-table)
  "Returns level for given xp according to given xp-table."
  (loop for x across xp-table
	for i of-type fixnum from 1
	do
	(when (> x xp)
;;	  (warn "Returning lvl ~s for xp ~s" (1- i) xp)
	  (return-from find-level-for-xp (1- i))))
  1) ;; fix me later

;;(trace find-level-for-xp)

(defmethod alter-xp! ((player player) amount)
  "Alters the xp for the player with the given amount."

  (assert (numberp amount))
  
  (when (minusp amount)
    (warn "Not implemented reduction in XP yet.")
    (return-from alter-xp! nil))

  (when (= amount 0)
    (return-from alter-xp! nil))
  
  (incf (player.cur-xp player) amount)
  (incf (player.max-xp player) amount)

  (bit-flag-add! *redraw* +print-xp+)

  (loop
   (let* ((cur-level (player.level player))
	  (next-limit (aref (player.xp-table player) cur-level))
	  (cur-xp (player.cur-xp player)))

;;     (warn "comparing ~s and ~s at lvl ~s -> ~a" cur-xp next-limit cur-level (> cur-xp next-limit))
     (if (>= cur-xp next-limit)
	 (gain-level! player)
	 (return-from alter-xp! nil)))
  
   ))


(defmethod get-weapon ((crt player))
  (let ((the-eq (player.eq crt)))
    (check-type the-eq item-table)
    
    (item-table-find the-eq 'eq.weapon)))

(defmethod get-missile-weapon ((crt player))
  (let ((the-eq (player.eq crt)))
    (check-type the-eq item-table)
    
    (item-table-find the-eq 'eq.bow)))

(defmethod get-light-source ((crt player))
  (let ((the-eq (player.eq crt)))
    (check-type the-eq item-table)
    
    (item-table-find the-eq 'eq.light)))


(defun reset-skills! (variant skills-obj reset-val)
  "Sets all skills to RESET-VAL."
  (dolist (i (variant.skill-translations variant))
    (setf (slot-value skills-obj (cdr i)) reset-val)))

(defun %add-to-a-skill! (which the-skills-obj player-lvl source)
  (declare (type fixnum player-lvl))
  (when source
    (let ((obj (slot-value source which)))
      (if (not obj)
	  (warn "Skill-slot ~s does have NIL value, please fix." which)
	  (incf (slot-value the-skills-obj which)
		(cond ((eq obj nil) 0)
		      ((numberp obj) obj)
		      ((skill-p obj)
		       (the fixnum (+ (the fixnum (skill.base obj))
				      (int-/ (* player-lvl (the fixnum (skill.lvl-gain obj)))
					     10))))
		      (t
		       (error "Unknown skill-obj ~a" obj)))
		)))))


(defmethod update-xp-table! ((variant variant) (player player))
  "Updates the xp-table on the player, and returns updated player."
  
  (let* ((base-xp-table (variant.xp-table variant))
	 (max-char-level (variant.max-charlevel variant))
	 (the-race (player.race player))
	 (the-class (player.class player))
	 (xp-extra (+ 100
		      (race.xp-extra the-race)
		      (class.xp-extra the-class))))

    (unless (arrayp (player.xp-table player))
      (setf (player.xp-table player) (%make-level-array variant)))

    (let ((xp-table (player.xp-table player)))
      
      (setf (aref xp-table 0) 0)
      (loop for i of-type u-fixnum from 1 to (1- max-char-level)
	    do
	    (setf (aref xp-table i) (int-/ (* (aref base-xp-table (1- i)) xp-extra)
					   100))))
    player))

(defmethod update-max-hp! ((variant variant) (player player))
  "Updates the maximum number of hitpoints.  Returns an updated player."

  (let ((lvl (player.level player))
	(hp-table (player.hp-table player)))
    
    (setf (maximum-hp player)
	  (loop for i from 0 to (1- lvl)
		summing (aref hp-table i))))
  player)
	   
(defmethod heal-creature! ((pl player) amount)
  "Heals the player and adds notify where needed."

  (let ((max-hp (maximum-hp pl)))
  
    (when (< (current-hp pl) max-hp)
      
      (incf (current-hp pl) amount)
      
      (when (< max-hp (current-hp pl)) ;; no more than max..
	(setf (current-hp pl) max-hp
	      (player.fraction-hp pl) 0))
      
      (bit-flag-add! *redraw* +print-hp+)
      
      ;; message
      (cond ((< amount 5)
	     (c-print-message! "You feel a little better."))
	    ((< amount 15)
	     (c-print-message! "You feel better."))
	    ((< amount 35)
	     (c-print-message! "You feel much better."))
	    (t
	     (c-print-message! "You feel very good.")))
      
      t))) ;; it returns nil if when doesn't make sense


(defmethod possible-identify! ((player player) (obj active-object))
  (learn-about-object! player obj :tried)
  ;; fix later
  (learn-about-object! player obj :aware)
  (learn-about-object! player obj :known)
  ;; add xp?
  )

;; Deprecated.. 
(defmethod possible-identify! ((player player) (obj object-kind))
  (warn "Deprecated identify-method called on ~s" obj)
  (learn-about-object! player obj :tried)
  ;; fix later
  (learn-about-object! player obj :aware)
;;  (learn-about-object! pl obj :known)
  ;; add xp?
  )


(defun update-player-stat! (pl stat action)
  "Action can be <restore> or a positive or negative integer."

;;  (declare (ignore pl stat action))

  ;; currently does restore
  (let ((bs (player.base-stats pl))
	(cbs (player.base-stats pl))
	(num (etypecase stat
	       (number stat)
	       (symbol (get-stat-num-from-sym stat)))))

    (ecase action
      (<restore>
       (let ((bval (aref bs num))
	     (cbval (aref cbs num)))
	 (when (< cbval bval)
	   (with-foreign-str (s)
	     (lb-format s "You feel less ~s" (get-stat-name-from-sym stat))
	     (c-print-message! s))
	   (setf (aref cbs num) (aref bs num))
	   t)))
      
      (<increase>
       (warn "increase stat not implemented.")
       t)
      (<reduce>
       (warn "reduce stat not implemented.")
       t))

    ))


(defun alter-food! (pl new-food-amount)
  ;; lots of minor pooh
  (setf (player.food pl) new-food-amount))

(defmethod copy-player-abilities ((variant variant) (ab player-abilities))
  (let ((new-ab (make-instance 'player-abilities)))
    (fill-player-abilities! variant new-ab ab)
    new-ab))

(defmethod fill-player-abilities! ((variant variant) (to player-abilities) (from player-abilities))
  (dolist (i '(base-ac ac-modifier to-hit-modifier to-dmg-modifier))
    (setf (slot-value to i) (slot-value from i)))
  to)



;;(trace calculate-creature-bonuses!)
;;(trace calculate-creature-light-radius!)
