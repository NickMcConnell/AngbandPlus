;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: lib/compat/ego.lisp - reads standard vanilla ego-items
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#



(in-package :org.langband.engine)


(defun compat-read-ego-file& (fname)
  "Reads floor from 2.0.x, not working with 2.9.3 anymore"
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-ego nil))

      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
		
	      (case first-char
		((#\# nil #\Space) nil)
		;; Version
		((#\V #\d);;(format t "Version: ~a~%" l)
		 )

		;; Name
		((#\N #\n)
		 (let ((res (split-seq-on l #\:)))
		   (when cur-ego
;;		     (warn "Register ego ~s" cur-ego)
		     (setf (get-ego (ego.numeric-id cur-ego)) cur-ego)

		     (setq cur-ego nil))
		 
		   (setq cur-ego (make-instance 'ego-item))

		   ;; the first should be N
		   (assert (string-equal (car res) "n"))
		   ;; the second should be the numeric id
		   (setf (ego.numeric-id cur-ego) (parse-integer (second res)))
		   ;; the third is the name
		   (setf (ego.name cur-ego) (third res))

		   ;; ultra-bad hack
		   (setf (ego.id cur-ego)
			 (format nil "ego-item-~s" (ego.numeric-id cur-ego)))

		   ))
		
		;; aux-info 
		((#\X #\x)
		 
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be X
		     (assert (string-equal (first res) "x"))
		     
		     ;; the second should be the inventory-slot
		     ;;(setf (ego.inv-slot cur-ego) (parse-integer (second res)))
		     ;; the third should be the rating
		     (setf (ego.power-lvl cur-ego) (parse-integer (second res)))
		     ;; the fourth should be some extra-slot
		     (setf (ego.xtra cur-ego) (parse-integer (third res)))
		     
		     )))

		;; combat
		((#\C #\c)
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be C
		     (assert (string-equal (first res) "c"))
		     
		     ;; the second should be the max-to-hit
		     (setf (ego.max-to-hit cur-ego) (parse-integer (second res)))
		     ;; the third should be the max-to-dmg
		     (setf (ego.max-to-dmg cur-ego) (parse-integer (third res)))
		     ;; the fourth should be the max-to-ac
		     (setf (ego.max-to-ac cur-ego) (parse-integer (fourth res)))
		     ;; the fifth should be the pval
		     (setf (ego.pval cur-ego) (parse-integer (fifth res)))
		   
		   )))

		;; other vital info
		((#\W #\w)
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be W
		     (assert (string-equal (first res) "w"))
		     
		     ;; the second should be the depth
		     ;;(setf (ego.depth cur-ego) (parse-integer (second res)))
		     ;; the third should be the rarity
		     ;;(setf (ego.rarity cur-ego) (parse-integer (third res)))
		     (push (cons (parse-integer (second res))
				 (parse-integer (third res))) (ego.locations cur-ego))
		     ;; the fourth should be the weight
		     (setf (ego.weight cur-ego) (parse-integer (fourth res)))
		     ;; the fifth should be the cost
		     (setf (ego.cost cur-ego) (parse-integer (fifth res)))
		   
		     )))

		;; type-info
		((#\T #\t)
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be T
		     (assert (string-equal (first res) "t"))
		     
		     ;; the second should be the tval
		     ;;(setf (ego.tval cur-ego) (parse-integer (second res)))
		     ;; the third should be the min-sval
		     ;;(setf (ego.min-sval cur-ego) (parse-integer (third res)))
		     ;; the fourth should be the max-sval
		     ;;(setf (ego.max-sval cur-ego) (parse-integer (fourth res)))
		     (let ((tval (parse-integer (second res)))
			   (min-sval (parse-integer (third res)))
			   (max-sval (parse-integer (fourth res))))
		       
		     (cond ((and (= tval 16) (= min-sval 0) (= max-sval 99)) ;; shot
			    (pushnew '<ammo> (ego.obj-types cur-ego)))
			   ((and (= tval 17) (= min-sval 0) (= max-sval 99)) ;; arrow
			    (pushnew '<ammo> (ego.obj-types cur-ego)))
			   ((and (= tval 18) (= min-sval 0) (= max-sval 99)) ;; bolt
			    (pushnew '<ammo> (ego.obj-types cur-ego)))
			   ((and (= tval 19) (= min-sval 0) (= max-sval 99))
			    (pushnew '<missile-weapon> (ego.obj-types cur-ego)))
			   ((and (= tval 20) (= min-sval 0) (= max-sval 99))
			    (pushnew '<digger> (ego.obj-types cur-ego)))
			   ((and (= tval 21) (= min-sval 0) (= max-sval 99))
			    (pushnew '<hafted> (ego.obj-types cur-ego)))
			   ((and (= tval 22) (= min-sval 0) (= max-sval 99))
			    (pushnew '<pole-arm> (ego.obj-types cur-ego)))
			   ((and (= tval 23) (= min-sval 0) (= max-sval 99))
			    (pushnew '<sword> (ego.obj-types cur-ego)))
			   ((and (= tval 30) (= min-sval 0) (= max-sval 99))
			    (pushnew '<boots> (ego.obj-types cur-ego)))
			   ((and (= tval 31) (= min-sval 0) (= max-sval 99))
			    (pushnew '<gloves> (ego.obj-types cur-ego)))
			   ((and (= tval 32) (= min-sval 0) (= max-sval 99))
			    (pushnew '<helmet> (ego.obj-types cur-ego)))
			   ((and (= tval 33) (= min-sval 0) (= max-sval 99))
			    (pushnew '<crown> (ego.obj-types cur-ego)))
			   ((and (= tval 34) (= min-sval 0) (= max-sval 99))
			    (pushnew '<shield> (ego.obj-types cur-ego)))
			   ((and (= tval 35) (= min-sval 0) (= max-sval 99))
			    (pushnew '<cloak> (ego.obj-types cur-ego)))
			   ((and (= tval 36) (= min-sval 0) (= max-sval 99))
			    (pushnew '<soft-body-armour> (ego.obj-types cur-ego)))
			   ((and (= tval 37) (= min-sval 0) (= max-sval 99))
			    (pushnew '<hard-body-armour> (ego.obj-types cur-ego)))
			   ((and (= tval 38) (= min-sval 0) (= max-sval 99))
			    (pushnew '<dsm-armour> (ego.obj-types cur-ego)))
			   (t
			    (push (list :tval (parse-integer (second res))
					:min-sval (parse-integer (third res))
					:max-sval (parse-integer (fourth res)))
				  (ego.obj-types cur-ego)))))
		     
		   )))

		;; flags
		((#\f #\F)
                  (when cur-ego
                    (let* ((res (split-seq-on (subseq l 2) #\|))
                           (real-res (loop for i in res
					   collecting (string-trim '(#\Space #\Tab #\Newline) i))))
                     
                      (dolist (j real-res)
                        (when (> (length j) 0)
			  (cond ((string-equal j "INT")
				 (pushnew '<int> (ego.flags cur-ego)))
				((string-equal j "STR")
				 (pushnew '<str> (ego.flags cur-ego)))
				((string-equal j "WIS")
				 (pushnew '<wis> (ego.flags cur-ego)))
				((string-equal j "CON")
				 (pushnew '<con> (ego.flags cur-ego)))
				((string-equal j "CHR")
				 (pushnew '<chr> (ego.flags cur-ego)))
				((string-equal j "DEX")
				 (pushnew '<dex> (ego.flags cur-ego)))
				
				((string-equal j "SEE_INVIS")
				 (pushnew '<see-invisible> (ego.flags cur-ego)))
				((string-equal j "REGEN")
				 (pushnew '<regeneration> (ego.flags cur-ego)))
				((string-equal j "FREE_ACT")
				 (pushnew '<free-action> (ego.flags cur-ego)))
				((string-equal j "FEATHER")
				 (pushnew '<feather-fall> (ego.flags cur-ego)))
				((string-equal j "STEALTH")
				 (pushnew '<stealth> (ego.flags cur-ego)))
				((string-equal j "SEARCH")
				 (pushnew '<search> (ego.flags cur-ego)))
				((string-equal j "LITE")
				 (pushnew '<light-source> (ego.flags cur-ego)))
				((string-equal j "TUNNEL")
				 (pushnew '<tunnel> (ego.flags cur-ego)))
				((string-equal j "BLESSED")
				 (pushnew '<blessed-blade> (ego.flags cur-ego)))
				((string-equal j "BLOWS")
				 (pushnew '<extra-blows> (ego.flags cur-ego)))
				((string-equal j "SHOTS")
				 (pushnew '<extra-shots> (ego.flags cur-ego)))
				((string-equal j "SPEED")
				 (pushnew '<speed> (ego.flags cur-ego)))
				((string-equal j "MIGHT")
				 (pushnew '<extra-might> (ego.flags cur-ego)))
				((string-equal j "IMPACT")
				 (pushnew '<impact> (ego.flags cur-ego)))
				((string-equal j "TELEPATHY")
				 (pushnew '<telepathy> (ego.flags cur-ego)))
				((string-equal j "INFRA")
				 (pushnew '<infravision> (ego.flags cur-ego)))
				((string-equal j "HOLD_LIFE")
				 (pushnew '<hold-life> (ego.flags cur-ego)))
				((string-equal j "SLOW_DIGEST")
				 (pushnew '<slow-digest> (ego.flags cur-ego)))

				((string-equal j "SLAY_ORC")
				 (pushnew '(<slay> <orc>) (ego.flags cur-ego)))
				((string-equal j "SLAY_TROLL")
				 (pushnew '(<slay> <troll>) (ego.flags cur-ego)))
				((string-equal j "SLAY_DRAGON")
				 (pushnew '(<slay> <dragon>) (ego.flags cur-ego)))
				((string-equal j "SLAY_EVIL")
				 (pushnew '(<slay> <evil>) (ego.flags cur-ego)))
				((string-equal j "SLAY_DEMON")
				 (pushnew '(<slay> <demon>) (ego.flags cur-ego)))
				((string-equal j "SLAY_GIANT")
				 (pushnew '(<slay> <giant>) (ego.flags cur-ego)))
				((string-equal j "SLAY_ANIMAL")
				 (pushnew '(<slay> <animal>) (ego.flags cur-ego)))
				((string-equal j "SLAY_UNDEAD")
				 (pushnew '(<slay> <undead>) (ego.flags cur-ego)))
				
				((string-equal j "KILL_DRAGON")
				 (pushnew '(<execute> <dragon>) (ego.flags cur-ego)))
				((string-equal j "KILL_UNDEAD")
				 (pushnew '(<execute> <undead>) (ego.flags cur-ego)))
				((string-equal j "KILL_DEMON")
				 (pushnew '(<execute> <demon>) (ego.flags cur-ego)))
				
				((string-equal j "SUST_STR")
				 (pushnew '(<sustain> <str>) (ego.flags cur-ego)))
				((string-equal j "SUST_DEX")
				 (pushnew '(<sustain> <dex>) (ego.flags cur-ego)))
				((string-equal j "SUST_CON")
				 (pushnew '(<sustain> <con>) (ego.flags cur-ego)))
				((string-equal j "SUST_INT")
				 (pushnew '(<sustain> <int>) (ego.flags cur-ego)))
				((string-equal j "SUST_WIS")
				 (pushnew '(<sustain> <wis>) (ego.flags cur-ego)))
				((string-equal j "SUST_CHR")
				 (pushnew '(<sustain> <chr>) (ego.flags cur-ego)))
								
				((string-equal j "LIGHT_CURSE")
				 (pushnew '(<curse> <light>) (ego.flags cur-ego)))
				((string-equal j "HEAVY_CURSE")
				 (pushnew '(<curse> <heavy>) (ego.flags cur-ego)))
				((string-equal j "AGGRAVATE")
				 (pushnew '<aggravate> (ego.flags cur-ego)))
				((string-equal j "TELEPORT")
				 (pushnew '<random-teleport> (ego.flags cur-ego)))
				((string-equal j "DRAIN_EXP")
				 (pushnew '<drain-xp> (ego.flags cur-ego)))
				
				((string-equal j "BRAND_FIRE")
				 (pushnew '(<brand> <fire>) (ego.flags cur-ego)))
				((string-equal j "BRAND_COLD")
				 (pushnew '(<brand> <cold>) (ego.flags cur-ego)))
				((string-equal j "BRAND_ACID")
				 (pushnew '(<brand> <acid>) (ego.flags cur-ego)))
				((string-equal j "BRAND_ELEC")
				 (pushnew '(<brand> <electricity>) (ego.flags cur-ego)))
				((string-equal j "BRAND_POIS")
				 (pushnew '(<brand> <poison>) (ego.flags cur-ego)))
				
				((string-equal j "IGNORE_ACID")
				 (pushnew '(<ignore> <acid>) (ego.flags cur-ego)))
				((string-equal j "IGNORE_FIRE")
				 (pushnew '(<ignore> <fire>) (ego.flags cur-ego)))
				((string-equal j "IGNORE_COLD")
				 (pushnew '(<ignore> <cold>) (ego.flags cur-ego)))
				((string-equal j "IGNORE_ELEC")
				 (pushnew '(<ignore> <electricity>) (ego.flags cur-ego)))
				
				((string-equal j "RES_FIRE")
				 (pushnew '(<resist> <fire>) (ego.flags cur-ego)))
				((string-equal j "RES_COLD")
				 (pushnew '(<resist> <cold>) (ego.flags cur-ego)))
				((string-equal j "RES_ELEC")
				 (pushnew '(<resist> <electricity>) (ego.flags cur-ego)))
				((string-equal j "RES_ACID")
				 (pushnew '(<resist> <acid>) (ego.flags cur-ego)))
				((string-equal j "RES_LITE")
				 (pushnew '(<resist> <light>) (ego.flags cur-ego)))
				((string-equal j "RES_DARK")
				 (pushnew '(<resist> <darkness>) (ego.flags cur-ego)))
				((string-equal j "RES_BLIND")
				 (pushnew '(<resist> <blindness>) (ego.flags cur-ego)))
				((string-equal j "RES_FEAR")
				 (pushnew '(<resist> <fear>) (ego.flags cur-ego)))
				((string-equal j "RES_DISEN")
				 (pushnew '(<resist> <disenchant>) (ego.flags cur-ego)))
				((string-equal j "RES_NEXUS")
				 (pushnew '(<resist> <nexus>) (ego.flags cur-ego)))
				((string-equal j "RES_SOUND")
				 (pushnew '(<resist> <sound>) (ego.flags cur-ego)))
				((string-equal j "RES_CONFU")
				 (pushnew '(<resist> <confusion>) (ego.flags cur-ego)))
				((string-equal j "RES_SHARD")
				 (pushnew '(<resist> <shards>) (ego.flags cur-ego)))
				
				((string-equal j "HIDE_TYPE")
				 (pushnew '<hide-type> (ego.flags cur-ego)))
				((string-equal j "SHOW_MODS")
				 (pushnew '<show-modifiers> (ego.flags cur-ego)))
				(t
				 (warn "Flag ~s fell through" j)))))
		      )))
				 
		
		(t
		 (format t "Unhandled [~s]: ~a~%" first-char l)))
	      ))
      
      (when cur-ego
;;	(warn "Handle ego ~s" cur-ego)
	(setf (get-ego (ego.numeric-id cur-ego)) cur-ego)
	(setq cur-ego nil))
      )))


(pushnew :compatibility-ego cl:*features*)
