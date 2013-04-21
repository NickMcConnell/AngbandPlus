;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/effects.lisp - apply effects on stuff
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defun is-legal-effect-type? (effect-type)
  (find effect-type '("teleport" "cold" "light" "acid" "fire" "healing" "mana"
		      "divination" "enhance" "meteor" "darkness" "arrow"
		      "magic-missile" "poison" "electricity" "enchant")
	:test #'equal))


(defun define-spell-effect (id &key gfx-beam text-beam gfx-ball text-ball
			    gfx-orb text-orb gfx-bolts text-bolts)
  (declare (ignore gfx-beam text-beam))
  (assert (verify-id id))
  
  (let ((spell-effect (make-instance 'van/spell-effect :id id)))

    (when (arrayp gfx-bolts)
      (setf (projectile.gfx-path spell-effect) gfx-bolts))
    (when (arrayp text-bolts)
      (setf (projectile.text-path spell-effect) text-bolts))

    (when (numberp gfx-ball)
      (setf (projectile.gfx-explosion spell-effect) gfx-ball))
    (when (numberp text-ball)
      (setf (projectile.text-explosion spell-effect) text-ball))

    (when (numberp gfx-orb)
      (setf (projectile.gfx-impact spell-effect) gfx-orb))
    (when (numberp text-orb)
      (setf (projectile.text-impact spell-effect) text-orb))

    
    (setf (gethash id (variant.visual-effects *variant*)) spell-effect)
    
    spell-effect))


(defun get-spell-effect (type)
  #'(lambda (var source target &key x y damage state-object)
      (ignore-errors
	(warn "GSE: Apply ~A on ~s by ~s" type (get-creature-name target) (get-creature-name source)))
      (apply-spell-effect! var type source target :x x :y y :damage damage :state-object state-object)))


(defmethod damaged-by-element? ((variant vanilla-variant) (creature active-monster) element)
  (declare (ignore element))
  t)

(defmethod damaged-by-element? ((variant vanilla-variant) (creature player) element)
  (declare (ignore element))
  t)

;; simple
(defmethod damaged-by-element? ((variant vanilla-variant) (object active-object) (element (eql '<magic-missile>)))
  nil)
;; hack
(defmethod damaged-by-element? ((variant vanilla-variant) (object active-object) (element (eql '<arrow>)))
  nil)

(defmethod damaged-by-element? ((variant vanilla-variant) (object active-object) element)
  ;; must be improved by looking not only at general type, but also at ignores and resists!

  (when-bind (gvals (aobj.game-values object))
    (when (bit-flag-and (get-element-flag variant element) (gval.ignores gvals))
      ;;(warn "~a ignored ~a" (object.name object) element)
      (return-from damaged-by-element? nil)))
  
  (let* ((okind (aobj.kind object))
	 (otype (object.the-kind okind)))

    (assert (symbolp otype))
    ;;(warn "Checking if ~s ~s resists ~s" (object.name object) otype element)
    
    (case element
      ((<sound> <cold> <shards> <force>) (typep object 'active-object/potion)) ;; flask bottle
      
      (<holiness> (is-cursed? object))
      
      (<electricity> (or (typep object 'active-object/ring)
			 (typep object 'active-object/wand)))
      
      ((<fire> <plasma>) (or (find otype '(<light-source> 
				<bow> <hafted> <pole-arm>
				<boots> <gloves> <cloak> <soft-body-armour>
				<prayerbook> <spellbook> 
				;; chest
				<staff> <scroll>))
		  ;; add arrows
		  ))

      (<water> (typep object 'active-object/scroll))
      
      ((<poison> <time>) (typep object 'active-object/food))

      (<acid> (or (find otype '(<bow> <sword> <hafted> <pole-arm>
				<helmet> <crown> <shield> <boots> <gloves> <cloak>
				<soft-body-armour> <hard-body-armour> <dsm-armour>
				<staff> <scroll>
				;; chest
				<skeleton> <junk>))
		  ;; bottle
		  ))
      (otherwise nil)
      )))

;;(trace damaged-by-element?)

(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source (target floor-type)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (let ((balanced-damage (int-/ (+ damage distance) (1+ distance))))
    (when (functionp effect)
      (funcall effect variant source target :x x :y y :damage balanced-damage)
      t)))

(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source (target decor)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (let ((balanced-damage (int-/ (+ damage distance) (1+ distance))))
    (when (functionp effect)
      (funcall effect variant source target :x x :y y :damage balanced-damage)
      t)))

;;; this one is a bloody nightmare
(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source target
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
;;  (declare (ignore x y damage effect distance source))
  (when (or (is-monster? target) (is-player? target))
    (warn "APET(VV): Apply damaging (~s) proj on ~s by ~s" damage (get-creature-name target) (get-creature-name source)))
  
  (let* ((balanced-damage (int-/ (+ damage distance) (1+ distance)))
	 
	 (seen? (or (is-player? target) (amon.seen-by-player? target)))
	 (meff (make-instance 'vanilla-monster-effect :seen seen?
			      :damage balanced-damage
			      :note nil :dying-note "dies")))

    (when (is-monster? target)
      (let ((type (monster.type (amon.kind target))))
	(when (or (eq type '<demon>)
		  (eq type '<undead>)
		  (eq type '<stupid>)) ;; fix
	  (setf (meff.dying-note meff) "is destroyed"))))
    
    
    (cond ((functionp effect)
	   ;;	       (warn "Function-effect not implemented for project-monster")
	   (let ((retval (funcall effect variant source target :x x :y y :damage balanced-damage
				  :state-object meff)))
	     (when (typep retval 'vanilla-monster-effect)
	       (setf meff retval))))
	  (t
	   (warn "No effect-function for ~s" effect)
	   ;;	       (warn "Hit monster ~s at (~s,~s) from ~s at (~s,~s) [~s]" (monster.name the-monster) loc-x loc-y
	   ;;		     (if (typep source 'player) "player" "someone")
	   ;;		     (location-x source) (location-y source) distance)
	   ))

    ;; add skip!
    
    ;; we simplify greatly here!
    
    (setf balanced-damage (meff.damage meff))
    
    ;; uniques only killed by player
    (when (and (is-unique-monster? target)
	       (is-player? source) 
	       (< (current-hp target) balanced-damage))
      (setf balanced-damage (current-hp target)))

   
 
    (cond ((and (is-monster? target)
		(> balanced-damage (current-hp target)))
	   (setf (meff.note meff) (meff.dying-note meff)))
	  ;; skip polymorph
	  ;; skip teleport
	  ;; skip stun
	  ;; skip confusion
	  (t))
    ;; skip fear
    
    (cond ((is-monster? source)
	   (warn "APET(VV): Monster ~s attacked ~s" (get-creature-name source) (get-creature-name target))
	   (deliver-damage! variant source target balanced-damage))
	  
	  ((is-player? source)
	   (let ((is-dead? (deliver-damage! variant source target balanced-damage
					    :dying-note (meff.dying-note meff))))
	     (unless is-dead? ;; he died
	       ;; improve message later
	       (format-message! "~@(~A~) was hurt." (get-creature-desc target #x00))
	       ;; skip fear
	       ;; skip sleep
	       )))
	  (t
	   (error "Who was source?? ~s" source)))
    
    
    (cond ((is-monster? target)
	   (update-monster! variant target nil))
	  ((is-player? target)
	   (bit-flag-add! *update* +pl-upd-bonuses+)
	   t)
	  (t
	   (warn "APET(VV): ODD TARGET ~s" target)))
    
    (light-spot! *dungeon* x y)
    
    
    ;; skip window
    
    ;; return if the object was obviously seen
    (meff.obvious meff)))


(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source (target active-object)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (declare (ignore distance))
;;  (warn "VAN-OBJ: Applying effect ~s to ~s" effect target)
  (when (and effect (functionp effect))
    (funcall effect variant source target :x x :y y :damage damage)))


  
(defun %destroy-floor-obj (variant dungeon x y obj msg)
  (let ((item-table (cave-objects dungeon x y)) 
	(desc (with-output-to-string (s)
		(write-obj-description variant obj s)))
	(verb (if (plusp (aobj.number obj))
		  "are"
		  "is")))
    (format-message! "~@(~a~) ~a ~a." desc verb msg)
    (item-table-remove! item-table obj)
    (when (= 0 (items.cur-size item-table))
      (setf (cave-objects dungeon x y) nil))
    (light-spot! dungeon x y)))


(defmethod apply-spell-effect! ((variant vanilla-variant) type source target &key x y (damage 0) (state-object nil))
  (declare (ignore x y type damage source target))
  ;; do nothing default
  ;;(ignore-errors
  ;;  (warn "ASE(VV): No apply [~a ~s] from ~a" type (get-creature-name target) (get-creature-name source)))
  state-object)


;;(defmethod apply-fire-effect! ((variant vanilla-variant) source target &key x y (damage 0) (state-object nil))
;;  (declare (ignore x y damage source target state-object))
;;  )

(defmethod apply-spell-effect! ((variant vanilla-variant) type source (target player)
				&key x y (damage 0) (state-object nil))

  (declare (ignore source x y))
  ;; iterate over equipment

  (when-bind (inv (player.inventory target))
    (when-bind (container (aobj.contains inv))
      (let ((cur-size (items.cur-size container))
	    (objs (items.objs container))
	    (any-removed nil))
	;;(warn "We have ~s objs" cur-size)
	(loop for i from 0
	      for obj across objs
	      do
	      (when obj
		;;(format t "~&~s obj ~s is ~s damaged by ~s~%"
		;;(aobj.number obj) (object.name obj) (damaged-by-element? variant obj type) type)
		(when (damaged-by-element? variant obj type) ;; it can die :-)
		  (let ((chance (cond ((< damage 30) 1)
				      ((< damage 60) 2)
				      (t 3)))
			(count 0)
			(num-objs (aobj.number obj))
			(oname (with-output-to-string (s)
				 (write-obj-description variant obj s :numeric-prefix nil))))
		    (dotimes (i num-objs)
		      (when (< (random 100) chance)
			(incf count)))

		    (cond ((= count num-objs)
			   ;;(warn "Remove ~a" (object.name obj))
			   (setf (aref objs i) nil)
			   (decf cur-size)
			   (setf any-removed t)
			   (if (= count 1)
			       (format-message! "Your ~a was destroyed!" oname)
			       (format-message! "All of your ~a were destroyed!" oname))
			   )
			   
			  ((> count 0)
			   (decf (aobj.number obj) count)
			   (if (= count 1)
			       (format-message! "One of your ~a was destroyed!" oname)
			       (format-message! "Some of your ~a were destroyed!" oname))
			   )
			  (t ;; they resisted!
			   ))
		    
		    ))
		))

	
	(when any-removed
	  (setf (items.cur-size container) (shrink-array! objs))
	  ;;(warn "Shrink claims there are now ~s objs"  (items.cur-size container))
	  (assert (= cur-size (items.cur-size container)))
	  (bit-flag-add! *redraw* +print-equip+)
	  )

	)))
  
  state-object)


(defmethod apply-spell-effect! ((variant vanilla-variant) type source (target active-object)
				&key x y (damage 0) (state-object nil))
  (declare (ignore damage source))
  
  (when (damaged-by-element? variant target type)
    (%destroy-floor-obj variant *dungeon* x y target "destroyed"))
  state-object)

;; example!
(defmethod apply-spell-effect! ((variant vanilla-variant) (type (eql '<fire>)) source (target active-object)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (when (damaged-by-element? variant target '<fire>)
    (%destroy-floor-obj variant *dungeon* x y target "burns"))
  state-object)

#||
(defmethod apply-spell-effect! ((variant vanilla-variant) (type (eql '<magic-missile>)) source target
			       &key
			       x y (damage 0)  (state-object nil))
  (declare (ignore x y source damage))
  
  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  state-object)
||#
    
  
(defmethod apply-spell-effect! ((variant vanilla-variant) (type (eql '<fire>)) source (target active-monster)
			       &key
			       x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))
  
  (unless (damaged-by-element? variant target '<fire>)
    ;; we're resisting
    (setf (meff.note state-object) " resists a lot.")
    (setf (meff.damage state-object) (int-/ damage 9))
    ;; skip lore
    )
    
  state-object)
