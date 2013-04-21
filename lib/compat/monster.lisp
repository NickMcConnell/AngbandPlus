;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: lib/compat/monster.lisp - code to be compatible with r_info.txt
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun add-new-mkind! (obj id)
  ""
  (declare (ignore id))
  (check-type *variant* variant)

  ;; applies the filters registered for newly read monsters
  (apply-filters-on-obj :monsters *variant* obj))


(defun compat-read-monsters& (old-file)
  "Reads a monster file from 2.9.0"
  
  (with-open-file (in-str (pathname old-file)
			  :direction :input)
    (let ((cur-monster nil)
	  (coll-desc nil))
    
      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
		
	      (case first-char
		((#\# nil #\Space) nil)
		;; Version
		((#\V #\v)
		 ;;(format t "Version: ~a~%" l)
		 )

		;; Descriptions
		((#\D #\d)
		 (if coll-desc
		     (setf coll-desc (concatenate 'string coll-desc " " (subseq l 2)))
		     (setq coll-desc (subseq l 2))))
	      
		;; New monster
		((#\N #\n)
		 (when cur-monster
		   (when coll-desc
		     (setf (monster.desc cur-monster) coll-desc)
		     (setf coll-desc nil))
		   (add-new-mkind! cur-monster (monster.id cur-monster))
;;		   (setf (get-monster-kind (monster.id cur-monster)) cur-monster)
		   (setf cur-monster nil))

		 (let ((res (split-seq-on l #\:)))
		   ;; the first should be N
		   (assert (string-equal (car res) "n"))

		   ;; the second should be a number
		   (let ((num (parse-integer (second res))))
		     (when (> num 0)
		       (setf cur-monster (make-instance 'monster-kind))
		       (setf (monster.id cur-monster) (concatenate 'string "monster-" (second res)))
		       (setf (monster.name cur-monster) (nstring-downcase (third res))))))
		 )

		;; Graphics
		((#\G #\g)
	       
		 (when cur-monster
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be G
		     (assert (string-equal (car res) "g"))
		     ;; the next is the symbol
		     (setf (monster.symbol cur-monster) (schar (cadr res) 0))
		     ;; the third is the colour
		     (setf (monster.colour cur-monster)
			   (convert-obj (schar (caddr res) 0) :colour-code))
		     )))

		;; Info
		((#\I #\i)
		 (when cur-monster
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be I
		     (assert (string-equal (first res) "i"))
		     ;; the second should be speed
		     (setf (monster.speed cur-monster) (parse-integer (second res)))
		     ;; the third should be hitpoints
		     (setf (monster.hitpoints cur-monster) (parse-dice (third res)))
		     ;; the fourth should be vision
		     (setf (monster.vision cur-monster) (parse-integer (fourth res)))
		     ;; the fifth is armour class
		     (setf (monster.armour cur-monster) (parse-integer (fifth res)))
		     ;; the sixth is alertness
		     (setf (monster.alertness cur-monster) (parse-integer (sixth res)))
		     )))

		;; More info
		((#\W #\w)
		 (when cur-monster
		   (let ((res (split-seq-on l #\:)))

		     ;; the first should be W
		     (assert (string-equal (first res) "w"))
		     ;; the second is level/depth
		     (setf (monster.depth cur-monster) (parse-integer (second res)))
		     ;; the third is rarity
		     (setf (monster.rarity cur-monster) (parse-integer (third res)))
		     ;; the fourth is unused
		     ;; -----
		     ;; the fifth is xp
		     (setf (monster.xp cur-monster) (parse-integer (fifth res)))
		   
		   
		     )))

		;; Attacks
		((#\B #\b)
		 (when cur-monster
		   (let ((res (split-seq-on l #\:))
			 (attk (make-instance 'attack)))

		     ;; the first should be B
		     (assert (string-equal (first res) "b"))
		     
		     ;; the second is what kind
		     (let ((the-kind (second res)))
		       (when the-kind
			 (setf (attack.kind attk) (intern (concatenate 'string "<" the-kind ">")))))
		     
		     ;; the third is damage type
		     (let ((dmg-type (third res)))
		       (when dmg-type
			 (setf (attack.dmg-type attk) (intern (concatenate 'string "<" dmg-type ">")))))
		     
		     ;; the fourth is damage
		     (let ((the-dmg (fourth res)))
		       (when the-dmg
			 (setf (attack.damage attk) (parse-dice the-dmg))))

		     (push attk (monster.attacks cur-monster))
		     )))

	      
		;; Flags
		((#\F #\f)
		 (when cur-monster
		   (let* ((res (split-seq-on (subseq l 2) #\|))
			  (real-res (loop for i in res collecting (string-trim '(#\Space #\Tab #\Newline) i))))
		   
		     (dolist (j real-res)
		       (when (> (length j) 0)
			 (cond ((string-equal j "IM_ELEC")
				(push '<lightning> (monster.immunities cur-monster)))
			       ((string-equal j "IM_FIRE")
				(push '<fire> (monster.immunities cur-monster)))
			       ((string-equal j "IM_COLD")
				(push '<cold> (monster.immunities cur-monster)))
			       ((string-equal j "IM_ACID")
				(push '<acid> (monster.immunities cur-monster)))
			       ((string-equal j "IM_POIS")
				(push '<poison> (monster.immunities cur-monster)))
			       ((string-equal j "NO_FEAR")
				(push '<fear> (monster.immunities cur-monster)))
			       ((string-equal j "NO_CONF")
				(push '<confuse> (monster.immunities cur-monster)))
			       ((string-equal j "NO_SLEEP")
				(push '<sleep> (monster.immunities cur-monster)))

			       ((string-equal j "HURT_LITE")
				(push '<light> (monster.vulnerabilities cur-monster)))
			       ((string-equal j "HURT_ROCK");; augh
				(push '<earth-destruction> (monster.vulnerabilities cur-monster)))
			     			     
			       ((string-equal j "EVIL")
				(setf (monster.alignment cur-monster) '<evil>))
			     
			       ((string-equal j "MALE")
				(setf (monster.sex cur-monster) '<male>))
			       ((string-equal j "FEMALE")
				(setf (monster.sex cur-monster) '<female>))

			       ;; FRIENDS causes the monster to be generated as a group.
			       ;; ESCORT causes a bunch of other monsters with the same symbol to be
			       ;; generated next to the monster.
			       ;; ESCORTS works like the ESCORT flag, but the extra monsters are
			       ;; treated as if they had the FRIENDS flag.
			       ((string-equal j "FRIENDS")
			      
				)
			     
			       ((string-equal j "ESCORT")
				)
			     
			       ((string-equal j "ESCORTS")
				)


			       ((string-equal j "ANIMAL")
				(push '<animal> (monster.type cur-monster)))
			       ((string-equal j "UNDEAD")
				(push '<undead> (monster.type cur-monster)))
			       ((string-equal j "DEMON")
				(push '<demon> (monster.type cur-monster)))
			       ((string-equal j "UNIQUE")
				(push '<unique> (monster.type cur-monster)))
			       ((string-equal j "DRAGON")
				(push '<dragon> (monster.type cur-monster)))
			       ((string-equal j "TROLL")
				(push '<troll> (monster.type cur-monster)))
			       ((string-equal j "ORC")
				(push '<orc> (monster.type cur-monster)))
			       ((string-equal j "GIANT")
				(push '<giant> (monster.type cur-monster)))
			       ((string-equal j "QUESTOR")
				(push '<quest-monster> (monster.type cur-monster)))
			     
			       ((string-equal j "FORCE_DEPTH")
				(push '<only-on-set-depth> (monster.abilities cur-monster)))
			       ((string-equal j "FORCE_MAXHP")
				(push '<max-hitpoints> (monster.abilities cur-monster)))
			       ((string-equal j "MOVE_BODY")
				(push '<push-others> (monster.abilities cur-monster)))
			       ((string-equal j "KILL_BODY")
				(push '<overrun-others> (monster.abilities cur-monster)))
			       ((string-equal j "KILL_ITEM")
				(push '<overrun-items> (monster.abilities cur-monster)))
			       ((string-equal j "POWERFUL")
				(push '<powerful-breath> (monster.abilities cur-monster)));; think so
			       ((string-equal j "KILL_WALL")
				(push '<destroy-wall> (monster.abilities cur-monster)))
			       ((string-equal j "PASS_WALL")
				(push '<pass-wall> (monster.abilities cur-monster)))
			       ((string-equal j "RAND_25")
				(push '(<random-mover> 1/4) (monster.abilities cur-monster)))
			       ((string-equal j "RAND_50")
				(push '(<random-mover> 1/2) (monster.abilities cur-monster)))
			       ((string-equal j "WEIRD_MIND")
				(push '<weird-mind> (monster.abilities cur-monster)));; not sure yet
			       ((string-equal j "EMPTY_MIND")
				(push '<empty-mind> (monster.abilities cur-monster)));; no esp
			       ((string-equal j "NEVER_BLOW")
				(push '<never-attack> (monster.abilities cur-monster)))
			       ((string-equal j "FORCE_SLEEP")
				(push '<initial-sleeper> (monster.abilities cur-monster)))
			       ((string-equal j "SMART")
				(push '<smart> (monster.abilities cur-monster)))
			       ((string-equal j "STUPID")
				(push '<stupid> (monster.abilities cur-monster)))
			       ((string-equal j "NEVER_MOVE")
				(push '<never-move> (monster.abilities cur-monster)))
			       ((string-equal j "COLD_BLOOD")
				(push '<cold-blood> (monster.abilities cur-monster)))
			       ((string-equal j "INVISIBLE")
				(push '<invisible> (monster.abilities cur-monster)))
			       ((string-equal j "REGENERATE")
				(push '<regenerate> (monster.abilities cur-monster)))
			       ((string-equal j "OPEN_DOOR")
				(push '<open-door> (monster.abilities cur-monster)))
			       ((string-equal j "BASH_DOOR")
				(push '<bash-door> (monster.abilities cur-monster)))
			       ((string-equal j "TAKE_ITEM")
				(push '<pick-up-item> (monster.abilities cur-monster)))
			       ((string-equal j "MULTIPLY")
				(push '<breeder> (monster.abilities cur-monster)))
			       ((string-equal j "ATTR_CLEAR")
				(push '<see-through> (monster.abilities cur-monster)))
			       ((string-equal j "ATTR_MULTI")
				(push '<colour-changing> (monster.abilities cur-monster)))
			       ((string-equal j "CHAR_CLEAR")
				(push '<special-symbol> (monster.abilities cur-monster)))
			       ((string-equal j "CHAR_MULTI")
				(push '<special-symbol-2> (monster.abilities cur-monster)))

			       ((string-equal j "DROP_GREAT")
				(push '<drop-great> (monster.treasures cur-monster)))
			       ((string-equal j "DROP_GOOD")
				(push '<drop-good> (monster.treasures cur-monster)))
			       ((string-equal j "DROP_CHOSEN")
				(push '<drop-planned> (monster.treasures cur-monster)))
			       ((string-equal j "DROP_1D2")
				(push '(<drop> "1d2") (monster.treasures cur-monster)))
			       ((string-equal j "DROP_2D2")
				(push '(<drop> "2d2") (monster.treasures cur-monster)))
			       ((string-equal j "DROP_3D2")
				(push '(<drop> "3d2") (monster.treasures cur-monster)))
			       ((string-equal j "DROP_4D2")
				(push '(<drop> "4d2") (monster.treasures cur-monster)))
			       ((string-equal j "DROP_60")
				(push '(<drop-chance> 60/100) (monster.treasures cur-monster)))
			       ((string-equal j "DROP_90")
				(push '(<drop-chance> 90/100) (monster.treasures cur-monster)))
			       ((string-equal j "ONLY_ITEM")
				(push '<only-drop-items> (monster.treasures cur-monster)))
			       ((string-equal j "ONLY_GOLD")
				(push '<only-drop-gold> (monster.treasures cur-monster)))
			     
			       (t
				(warn "Non-supported flag ~s" j)))))
		 
		     ))
		 nil)

		;; Spells, breathing and extras
		((#\S #\s)
		 (when cur-monster
		   (let* ((res (split-seq-on (subseq l 2) #\|))
			  (real-res (loop for i in res collecting (string-trim '(#\Space #\Tab #\Newline) i))))
		   
		     (dolist (j real-res)
		       (when (> (length j) 0)
		       
			 (cond ((string-equal j "1_IN_1")
				(push '(<frequency> 1) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_2")
				(push '(<frequency> 1/2) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_3")
				(push '(<frequency> 1/3) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_4")
				(push '(<frequency> 1/4) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_5")
				(push '(<frequency> 1/5) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_6")
				(push '(<frequency> 1/6) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_7")
				(push '(<frequency> 1/7) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_8")
				(push '(<frequency> 1/8) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_9")
				(push '(<frequency> 1/9) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_10")
				(push '(<frequency> 1/10) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_11")
				(push '(<frequency> 1/11) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_12")
				(push '(<frequency> 1/12) (monster.sp-abilities cur-monster)))
			       ((string-equal j "1_IN_15")
				(push '(<frequency> 1/15) (monster.sp-abilities cur-monster)))
			     
			       ((string-equal j "BR_ELEC")
				(push '(<breath> <lightning>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_FIRE")
				(push '(<breath> <fire>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_ACID")
				(push '(<breath> <acid>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_COLD")
				(push '(<breath> <cold>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_POIS")
				(push '(<breath> <poison>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_DARK")
				(push '(<breath> <darkness>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_LITE")
				(push '(<breath> <light>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_CONF")
				(push '(<breath> <confusion>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_CHAO")
				(push '(<breath> <chaos>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_DISE")
				(push '(<breath> <disenchant>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_NETH")
				(push '(<breath> <nether>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_SOUN")
				(push '(<breath> <sound>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_NEXU")
				(push '(<breath> <nexus>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_TIME")
				(push '(<breath> <time>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_INER")
				(push '(<breath> <inertia>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_GRAV")
				(push '(<breath> <gravity>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_SHAR")
				(push '(<breath> <shards>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_PLAS")
				(push '(<breath> <plasma>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BR_WALL")
				(push '(<breath> <force>) (monster.sp-abilities cur-monster)))
			     
			       ((string-equal j "S_MONSTERS")
				(push '(<summon> <monsters>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_DEMON")
				(push '(<summon> <demon>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_DRAGON")
				(push '(<summon> <dragon>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_HYDRA")
				(push '(<summon> <hydra>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_SPIDER")
				(push '(<summon> <spider>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_ANGEL")
				(push '(<summon> <angel>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_HOUND")
				(push '(<summon> <hound>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_UNIQUE")
				(push '(<summon> <unique>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_ANT")
				(push '(<summon> <ant>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_MONSTER")
				(push '(<summon> <monster>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_KIN")
				(push '(<summon> <kin>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_UNDEAD")
				(push '(<summon> <undead>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_WRAITH")
				(push '(<summon> <wraith>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_HI_UNDEAD")
				(push '(<summon> <high-undead>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_HI_DRAGON")
				(push '(<summon> <high-dragon>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "S_HI_DEMON")
				(push '(<summon> <high-demon>) (monster.sp-abilities cur-monster)))
			     
			       ((string-equal j "BO_MANA")
				(push '(<spell> (<bolt> <mana>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_NETH")
				(push '(<spell> (<bolt> <nether>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_ELEC")
				(push '(<spell> (<bolt> <lightning>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_FIRE")
				(push '(<spell> (<bolt> <fire>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_ACID")
				(push '(<spell> (<bolt> <acid>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_COLD")
				(push '(<spell> (<bolt> <cold>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_PLAS")
				(push '(<spell> (<bolt> <plasma>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_WATE")
				(push '(<spell> (<bolt> <water>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BO_ICEE")
				(push '(<spell> (<bolt> <ice>)) (monster.sp-abilities cur-monster)))
			     
			       ((string-equal j "BA_MANA")
				(push '(<spell> (<ball> <mana>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_NETH")
				(push '(<spell> (<ball> <nether>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_ELEC")
				(push '(<spell> (<ball> <lightning>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_FIRE")
				(push '(<spell> (<ball> <fire>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_DARK")
				(push '(<spell> (<ball> <darkness>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_COLD")
				(push '(<spell> (<ball> <cold>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_ACID")
				(push '(<spell> (<ball> <acid>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_POIS")
				(push '(<spell> (<ball> <poison>)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BA_WATE")
				(push '(<spell> (<ball> <water>)) (monster.sp-abilities cur-monster)))
			     

			       ((string-equal j "SCARE")
				(push '(<spell> <scare>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BLIND")
				(push '(<spell> <blindness>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "CONF")
				(push '(<spell> <confusion>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "SLOW")
				(push '(<spell> <slow>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "HASTE")
				(push '(<spell> <haste>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BLINK")
				(push '(<spell> <blink>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "TPORT")
				(push '(<spell> <teleport>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "TELE_LEVEL")
				(push '(<spell> <teleport-level>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "TELE_TO")
				(push '(<spell> <teleport-player>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "TELE_AWAY")
				(push '(<spell> <teleport-away>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "HOLD")
				(push '(<spell> <paralysis>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "FORGET")
				(push '(<spell> <forget>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "DARKNESS")
				(push '(<spell> <darkness>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "DRAIN_MANA")
				(push '(<spell> <drain-mana>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "MISSILE")
				(push '(<spell> <missile>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "TRAPS")
				(push '(<spell> <traps>) (monster.sp-abilities cur-monster)))

			       ((string-equal j "ARROW_1")
				(push '(<arrow> 1) (monster.sp-abilities cur-monster)))
			       ((string-equal j "ARROW_2")
				(push '(<arrow> 2) (monster.sp-abilities cur-monster)))
			       ((string-equal j "ARROW_3")
				(push '(<arrow> 3) (monster.sp-abilities cur-monster)))
			       ((string-equal j "ARROW_4")
				(push '(<arrow> 4) (monster.sp-abilities cur-monster)))
			     
			       ((string-equal j "HEAL")
				(push '(<spell> <heal>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "CAUSE_1")
				(push '(<spell> (<cause> 1)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "CAUSE_2")
				(push '(<spell> (<cause> 2)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "CAUSE_3")
				(push '(<spell> (<cause> 3)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "CAUSE_4")
				(push '(<spell> (<cause> 4)) (monster.sp-abilities cur-monster)))
			       ((string-equal j "BRAIN_SMASH")
				(push '(<spell> <brain-smash>) (monster.sp-abilities cur-monster)))
			       ((string-equal j "MIND_BLAST")
				(push '(<spell> <mind-blast>) (monster.sp-abilities cur-monster)))
			     
			       ((string-equal j "SHRIEK")
				(push '<shriek> (monster.sp-abilities cur-monster)))
			       (t
				(warn "Non-supported special ~s" j)))))

		     ))
		 nil)
	      
		(otherwise
		 (warn "Unknown directive: ~a~%" l)
		 nil))
	      ))

      (when cur-monster
	(when coll-desc
	  (setf (monster.desc cur-monster) coll-desc)
	  (setf coll-desc nil))

	(add-new-mkind! cur-monster (monster.id cur-monster))
;;	(setf (get-monster-kind (monster.id cur-monster)) cur-monster)
	(setf cur-monster nil))
    

      )))


(pushnew :compatibility-monsters cl:*features*)
