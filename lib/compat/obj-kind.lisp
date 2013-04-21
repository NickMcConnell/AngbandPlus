;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/compat/obj-kind.lisp - code to be compatible with k_info.txt
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(defun compat-read-obj-kind& (fname)
  "not finished"
  #+cmu
  (declare (optimize (ext:inhibit-warnings 3)))
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-obj nil)
	  (patch-val nil))

      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
		
	      (case first-char
		((#\# nil #\Space) nil)
		;; Version
		((#\V #\v);;(format t "Version: ~a~%" l)
		 )

		((#\n #\N)
		 (let ((res (split-seq-on l #\:)))
		   (when cur-obj
		     (add-new-okind! cur-obj (object.id cur-obj))
;;		     (setf (get-obj-kind (object.id cur-obj)) cur-obj)
		     (setq cur-obj nil
			   patch-val nil))
		   
		   
		   (setq cur-obj (make-instance 'object-kind))
		   ;; the first should be N
		   (assert (string-equal (car res) "n"))
		   ;; the second should be the numeric id
		   (let ((num-id (parse-integer (second res))))
		     (setf (object.id cur-obj) (concatenate 'string "object-" (second res))
			   (object.numeric-id cur-obj) num-id))
		   ;; the third is the name
		   (setf (object.name cur-obj) (nstring-downcase (third res)))
		   ))

		;; graphics
		((#\g #\G)
		 (when cur-obj
		   ;; the : shows up here, so no ordinary tokenising :(
		   ;; no assert
		   ;; the second should be the symbol to paint
		   (setf (object.x-char cur-obj) (schar l 2))
		   ;; the third should be the colour
		   (setf (object.x-attr cur-obj) (convert-obj (schar l 4) :colour-code))
		 
		   ))

		;; kindof vital code
		((#\i #\I)
		 
		 (let ((res (split-seq-on l #\:)))
		   ;; the first should be i
		   (assert (string-equal (car res) "i"))

		   ;; vanilla has these kludgy ways to have variables,
		   ;; types and subtypes.  Attempt to make this more
		   ;; reasonable
		   (labels ((add-str-tval (the-symbol)
			      (cond ((consp the-symbol)
				     (setf (object.obj-type cur-obj) (append (object.obj-type cur-obj) the-symbol)))
				    ((null the-symbol)
				     (warn "Unhandled object [~a]~%" the-symbol))
				    (t
				     (pushnew the-symbol (object.obj-type cur-obj)))))

			    (ensure-gval! ()
			      (let ((gval (object.game-values cur-obj)))
				(unless gval
				  (setq gval (make-game-values))
				  (setf (object.game-values cur-obj) gval))
				gval))
				  
			    
			    (add-resist! (the-resist)
			      (let ((gval (ensure-gval!)))
				(pushnew the-resist (gval.resists gval))))
			    
			    (add-sustain! (the-stat)
			      (let ((gval (ensure-gval!)))
				(pushnew the-stat (gval.sustains gval))))
			    
			    (add-stat-bonus! (the-stat)
			      (let ((gval (ensure-gval!)))
				(pushnew (list the-stat '<random>) (gval.stat-bonuses gval))))

			    (add-skill-bonus! (the-skill)
			      (let ((gval (ensure-gval!)))
				(push (list the-skill '<random>) (gval.sustains gval))))
			    
			    (add-tunnel-bonus! (the-bonus)
			      (let ((gval (ensure-gval!)))
				(setf (gval.tunnel gval) the-bonus)))

			    
			    )
			    
		   
		     (let* (;; the second should be type-val (tval)
			    (type-val (parse-integer (second res)))
			  
			    ;; the third should be subtype-val (sval)
			    (subtype-val (parse-integer (third res))))

		       (assert (numberp type-val))
		       (assert (numberp subtype-val))
			       
		       ;; the fourth should be patch-val (pval)
		       (setq patch-val (parse-integer (fourth res)))
		       
		       
		       ;; The case-values are gotten from defines.h
		       
		       (setf (object.sort-value cur-obj) (+ (get-sort-value type-val) subtype-val))

		       (case type-val
			 (1   (add-str-tval '<skeleton>))
			 (2   (add-str-tval '<bottle>))
			 (3   (add-str-tval '<junk>))
			 (5   (add-str-tval '<spike>))
			 (7   (add-str-tval '<chest>))
		       
			 (16  (add-str-tval '(<ammo> <shot>))
			      (case subtype-val
				(0 (add-str-tval '<light>))
				(1 (add-str-tval '<normal>))
				(2 (add-str-tval '<heavy>))
				(otherwise (warn "Unknown subtype for shot"))))
				 
			 (17  (add-str-tval '(<ammo> <arrow>))
			      (case subtype-val
				(0 (add-str-tval '<light>))
				(1 (add-str-tval '<normal>))
				(2 (add-str-tval '<heavy>))
				(otherwise (warn "Unknown subtype for arrow"))))
		       
			 (18  (add-str-tval '(<ammo> <bolt>))
			      (case subtype-val
				(0 (add-str-tval '<light>))
				(1 (add-str-tval '<normal>))
				(2 (add-str-tval '<heavy>))
				(otherwise (warn "Unknown subtype for bolt"))))
		       
			 (19  (add-str-tval '<bow>)
			      (case subtype-val
				(2  (add-str-tval '<sling>))
				(12 (add-str-tval '(<short> <bow>)))
				(13 (add-str-tval '(<long> <bow>)))
				(23 (add-str-tval '(<light> <xbow>)))
				(24 (add-str-tval '(<heavy> <xbow>)))
				(otherwise (warn "Unknown subtype for bow ~a" subtype-val))))
		       
			 (20  (add-str-tval '<digging>)
			      
			      (add-tunnel-bonus! patch-val)
			      (setq patch-val 0)
			      
			      (case subtype-val
				(1 (add-str-tval '<shovel>))
				(2 (add-str-tval '(<shovel> <gnome>)))
				(3 (add-str-tval '(<shovel> <dwarf>)))
				(4 (add-str-tval '<pick>))
				(5 (add-str-tval '(<pick> <orc>)))
				(6 (add-str-tval '(<pick> <dwarf>)))
				(otherwise (warn "Unknown subtype for digger ~a" subtype-val))))
		       
			 (21  (add-str-tval '(<weapon> <hafted>))
			      (case subtype-val
				(2 (add-str-tval '<whip>))
				(3 (add-str-tval '<quarterstaff>))
				(5 (add-str-tval '<mace>))
				(6 (add-str-tval '<ball-and-chain>))
				(8 (add-str-tval '<war-hammer>))
				(10 (add-str-tval '<lucern-hammer>))
				(12 (add-str-tval '<morning-star>))
				(13 (add-str-tval '<flail>))
				(15 (add-str-tval '<lead-filled-mace>))
				(18 (add-str-tval '<two-handed-flail>))
				(20 (add-str-tval '<mace-of-disruption>))
				(50 (add-str-tval '<grond>))
				(otherwise (warn "Unknown subtype for hafted weapon ~a" subtype-val))))
		       
			 (22  (add-str-tval '(<weapon> <polearm>))
			      (case subtype-val
				(2 (add-str-tval '<spear>))
				(4 (add-str-tval '<awl-pike>))
				(5 (add-str-tval '<trident>))
				(8 (add-str-tval '<pike>))
				(10 (add-str-tval '<beaked-axe>))
				(11 (add-str-tval '<broad-axe>))
				(13 (add-str-tval '<glaive>))
				(15 (add-str-tval '<halberd>))
				(17 (add-str-tval '<scythe>))
				(20 (add-str-tval '<lance>))
				(22 (add-str-tval '<battle-axe>))
				(25 (add-str-tval '<great-axe>))
				(28 (add-str-tval '<lochaber-axe>))
				(30 (add-str-tval '<scythe-slicing>))
				(otherwise (warn "Unknown subtype for polearm weapon ~a" subtype-val))))
		       
			 (23  (add-str-tval '(<weapon> <sword>))
			      (case subtype-val
				(1 (add-str-tval '<broken-dagger>))
				(2 (add-str-tval '<broken-sword>))
				(4 (add-str-tval '<dagger>))
				(5 (add-str-tval '<main-gauche>))
				(7 (add-str-tval '<rapier>))
				(8 (add-str-tval '<small-sword>))
				(10 (add-str-tval '<short-sword>))
				(11 (add-str-tval '<sabre>))
				(12 (add-str-tval '<cutlass>))
				(15 (add-str-tval '<tulwar>))
				(16 (add-str-tval '<broad-sword>))
				(17 (add-str-tval '<long-sword>))
				(18 (add-str-tval '<scimitar>))
				(20 (add-str-tval '<katana>))
				(21 (add-str-tval '<bastard>))
				(25 (add-str-tval '<two-handed-sword>))
				(28 (add-str-tval '<exec-sword>))
				(30 (add-str-tval '<blade-of-chaos>))
				(otherwise (warn "Unknown subtype for sword ~a" subtype-val))))
		       
			      
			 (30  (add-str-tval '<boots>)
			      (case subtype-val
				(2 (add-str-tval '<soft-leather))
				(3 (add-str-tval '<hard-leather>))
				(6 (add-str-tval '<metal-shod>))
				(otherwise (warn "Unknown subtype for boots ~a" subtype-val))))
		       
			 (31  (add-str-tval '<gloves>)
			      (case subtype-val
				(1 (add-str-tval '<leather>))
				(2 (add-str-tval '<gauntlets>))
				(5 (add-str-tval '<cesti>))
				(otherwise (warn "Unknown subtype for gloves ~a" subtype-val))))
		       
			 (32  (add-str-tval '(<headgear> <helmet>))
			      (case subtype-val
				(2 (add-str-tval '<hard-leather>))
				(3 (add-str-tval '<metal-cap>))
				(5 (add-str-tval '<iron-helm>))
				(6 (add-str-tval '<steel-helm>))
				(otherwise (warn "Unknown subtype for helm ~a" subtype-val))))
		       
			 (33  (add-str-tval '(<headgear> <crown>))
			      (case subtype-val
				(10 (add-str-tval '<iron>))
				(11 (add-str-tval '<golden>))
				(12 (add-str-tval '<jeweled>))
				(50 (add-str-tval '<morgoth>))
				(otherwise (warn "Unknown subtype for crown ~a" subtype-val))))
		       
			 (34  (add-str-tval '<shield>)
			      (case subtype-val
				(2  (add-str-tval '(<small> <leather>)))
				(3  (add-str-tval '(<small> <metal>)))
				(4  (add-str-tval '(<large> <leather>)))
				(5  (add-str-tval '(<large> <metal>)))
				(10 (add-str-tval '(<large> <metal> <deflection>)))
				(otherwise (warn "Unknown subtype for shield ~a" subtype-val))))
		       
			 (35  (add-str-tval '<cloak>)
			      (case subtype-val
				(1 (add-str-tval '<cloth>))
				(6 (add-str-tval '<shadow>))
				(otherwise (warn "Unknown subtype for cloak ~a" subtype-val))))
		       
			 (36  (add-str-tval '(<body-armour> <soft>))
			      (case subtype-val
				(1 (add-str-tval '<filthy-rag>))
				(2 (add-str-tval '<robe>))
				(4 (add-str-tval '<soft-leather>))
				(5 (add-str-tval '<soft-studded>))
				(6 (add-str-tval '<hard-leather>))
				(7 (add-str-tval '<hard-studded>))
				(11 (add-str-tval '<leather-scale>))
				(otherwise (warn "Unknown subtype for soft armour ~a" subtype-val))))
		       
			 (37  (add-str-tval '(<body-armour> <hard>))
			      (case subtype-val
				(1 (add-str-tval '<rusty-chain>))
				(3 (add-str-tval '<metal-scale>))
				(4 (add-str-tval '<chain>))
				(6 (add-str-tval '<augmented-chain>))
				(7 (add-str-tval '<double-chain>))
				(8 (add-str-tval '<bar-chain>))
				(9 (add-str-tval '<metal-brigandine>))
				(12 (add-str-tval '<partial-plate>))
				(13 (add-str-tval '<metal-lamellar>))
				(15 (add-str-tval '<full-plate>))
				(18 (add-str-tval '<ribbed-plate>))
				(20 (add-str-tval '<mithril-chain>))
				(25 (add-str-tval '<mithril-plate>))
				(30 (add-str-tval '<adamantite-plate>))
				(otherwise (warn "Unknown subtype for hard armour ~a" subtype-val))))
		       
			 (38  (add-str-tval '(<body-armour> <dragon-scale>))
			      (case subtype-val
				(1 (add-str-tval '<black>))
				(2 (add-str-tval '<blue>))
				(3 (add-str-tval '<white>))
				(4 (add-str-tval '<red>))
				(5 (add-str-tval '<green>))
				(6 (add-str-tval '<multihued>))
				(10 (add-str-tval '<shining>))
				(12 (add-str-tval '<law>))
				(14 (add-str-tval '<bronze>))
				(16 (add-str-tval '<gold>))
				(18 (add-str-tval '<chaos>))
				(20 (add-str-tval '<balance>))
				(30 (add-str-tval '<power>))
				(otherwise (warn "Unknown subtype for dragon scale armour ~a" subtype-val))))
			    
			 (39  (add-str-tval '<light-source>)
			      
			      (let ((gval (ensure-gval!)))
				(setf (gval.charges gval) patch-val)
				(setq patch-val 0)
			      
				(case subtype-val
				  (0 (add-str-tval '<torch>)
				     (setf (gval.light-radius gval) 1))
				  (1 (add-str-tval '<lantern>)
				     (setf (gval.light-radius gval) 2))
				  (4 (add-str-tval '<phial>)
				     (setf (gval.light-radius gval) 3))
				  (5 (add-str-tval '<star>)
				     (setf (gval.light-radius gval) 3))
				  (6 (add-str-tval '<arkenstone>)
				     (setf (gval.light-radius gval) 3))
				  (otherwise (warn "Unknown subtype for light source ~a" subtype-val))
				  )))
		       
			 (40  (add-str-tval '<neckwear>)
			      (case subtype-val
				(0 (add-str-tval '(<amulet> <doom>)))
				(1 (add-str-tval '(<amulet> <teleport>)))
				(2 (add-str-tval '(<amulet> <adornment>)))
				(3 (add-str-tval '(<amulet> <slow-digestion>)))
				(4 (add-str-tval '(<amulet> <resist> <acid>)))
				(5 (add-str-tval '(<amulet> <searching>)))
				(6 (add-str-tval '(<amulet> <wis>)))
				(7 (add-str-tval '(<amulet> <chr>)))
				(8 (add-str-tval '(<amulet> <magi>)))
				(10 (add-str-tval '(<amulet> <carlammas>)))
				(11 (add-str-tval '(<amulet> <ingwe>)))
				(12 (add-str-tval '(<necklace> <dwarves>)))
				(otherwise (warn "Unknown subtype for neckwear ~a" subtype-val))))
			    
			 (45  (add-str-tval '<ring>)
			      (case subtype-val
				(0 (add-str-tval '<woe>))
				(1 (add-str-tval '<aggravation>))
				(2 (add-str-tval '<weakness>))
				(3 (add-str-tval '<stupidity>))
				(4 (add-str-tval '<teleport>))
				(6 (add-str-tval '<slow-digestion>))
				(7 (add-str-tval '<feather-fall>))
				(8 (add-str-tval '(<resist> <fire>)))
				(9 (add-str-tval '(<resist> <cold>)))
				(10 (add-str-tval '(<sustain> <str>)))
				(11 (add-str-tval '(<sustain> <int>)))
				(12 (add-str-tval '(<sustain> <wis>)))
				(13 (add-str-tval '(<sustain> <dex>)))
				(14 (add-str-tval '(<sustain> <con>)))
				(15 (add-str-tval '(<sustain> <chr>)))
				(16 (add-str-tval '<protection>))
				(17 (add-str-tval '(<protection> <acid>)))
				(18 (add-str-tval '(<protection> <fire>)))
				(19 (add-str-tval '(<protection> <cold>)))
				(20 (add-str-tval '(<resist> <poison>)))
				(21 (add-str-tval '<free-action>))
				(22 (add-str-tval '<see-invisible>))
				(23 (add-str-tval '<searching>))
				(24 (add-str-tval '<str>))
				(25 (add-str-tval '<int>))
				(26 (add-str-tval '<dex>))
				(27 (add-str-tval '<con>))
				(28 (add-str-tval '<accuracy>))
				(29 (add-str-tval '<damage>))
				(30 (add-str-tval '<slaying>))
				(31 (add-str-tval '<speed>))
				;; artifact rings
				((32 33 34 35 36 37)
				 ;; nothing
				 )


				(otherwise (warn "Unknown subtype for ring ~a" subtype-val))))
			      
			 (55  (add-str-tval '<staff>)
			      
			      (case subtype-val
				(0 (add-str-tval '<darkness>))
				(1 (add-str-tval '<slowness>))
				(2 (add-str-tval '<haste-monster>))
				(3 (add-str-tval '<summoning>))
				(4 (add-str-tval '<teleportation>))
				(5 (add-str-tval '<identify>))
				(6 (add-str-tval '<remove-curse>))
				(7 (add-str-tval '<star-light>))
				(8 (add-str-tval '<light>))
				(9 (add-str-tval '<mapping>))
				(10 (add-str-tval '(<detect> <money>)))
				(11 (add-str-tval '(<detect> <item>)))
				(12 (add-str-tval '(<detect> <trap>)))
				(13 (add-str-tval '(<detect> <door>)))
				(14 (add-str-tval '(<detect> <invisible>)))
				(15 (add-str-tval '(<detect> <evil>)))
				(16 (add-str-tval '(<cure> <light>)))
				(17 (add-str-tval '(<cure> <curing>)))
				(18 (add-str-tval '(<cure> <healing>)))
				(19 (add-str-tval '<magi>))
				(20 (add-str-tval '<sleep-monster>))
				(21 (add-str-tval '<slow-monster>))
				(22 (add-str-tval '<speed>))
				(23 (add-str-tval '<probing>))
				(24 (add-str-tval '(<dispel> <evil>)))
				(25 (add-str-tval '<power>))
				(26 (add-str-tval '<holiness>))
				(27 (add-str-tval '<genocide>))
				(28 (add-str-tval '<earthquake>))
				(29 (add-str-tval '<destruction>))
			      
				(otherwise (warn "Unknown subtype for staff ~a" subtype-val))))
		       
			 (65  (add-str-tval '<wand>)
			      
			      (case subtype-val
				(0 (add-str-tval '<heal-monster>))
				(1 (add-str-tval '<haste-monster>))
				(2 (add-str-tval '<clone-monster>))
				(3 (add-str-tval '<teleport-away>))
				(4 (add-str-tval '<disarm>))
				(5 (add-str-tval '<trap/door-destruction>))
				(6 (add-str-tval '<stone-to-mud>))
				(7 (add-str-tval '<light>))
				(8 (add-str-tval '<sleep-monster>))
				(9 (add-str-tval '<slow-monster>))
				(10 (add-str-tval '<confuse-monster>))
				(11 (add-str-tval '<fear-monster>))
				(12 (add-str-tval '<drain-life>))
				(13 (add-str-tval '<polymorph>))
				(14 (add-str-tval '<stinking-cloud>))
				(15 (add-str-tval '<magic-missile>))
				(16 (add-str-tval '(<bolt> <acid>)))
				(17 (add-str-tval '(<bolt> <lightning>)))
				(18 (add-str-tval '(<bolt> <fire>)))
				(19 (add-str-tval '(<bolt> <cold>)))
				(20 (add-str-tval '(<ball> <acid>)))
				(21 (add-str-tval '(<ball> <lightning>)))
				(22 (add-str-tval '(<ball> <fire>)))
				(23 (add-str-tval '(<ball> <cold>)))
				(24 (add-str-tval '<wonder>))
				(25 (add-str-tval '<annihiliation>))
				(26 (add-str-tval '(<dragon> <fire>)))
				(27 (add-str-tval '(<dragon> <cold>)))
				(28 (add-str-tval '(<dragon> <breath>)))

				(otherwise (warn "Unknown subtype for wand ~a" subtype-val))))
		       
			 (66  (add-str-tval '<rod>)
			      
			      (case subtype-val
				(0 (add-str-tval '(<detect> <trap>)))
				(1 (add-str-tval '(<detect> <door>)))
				(2 (add-str-tval '<identify>))
				(3 (add-str-tval '<recall>))
				(4 (add-str-tval '<illumination>))
				(5 (add-str-tval '<mapping>))
				(6 (add-str-tval '<detection>))
				(7 (add-str-tval '<probing>))
				(8 (add-str-tval '<curing>))
				(9 (add-str-tval '<healing>))
				(10 (add-str-tval '<restoration>))
				(11 (add-str-tval '<speed>))
				(13 (add-str-tval '<teleport-away>))
				(14 (add-str-tval '<disarming>))
				(15 (add-str-tval '<light>))
				(16 (add-str-tval '<sleep-monster>))
				(17 (add-str-tval '<slow-monster>))
				(18 (add-str-tval '<drain-life>))
				(19 (add-str-tval '<polymorph>))
				(20 (add-str-tval '(<bolt> <acid>)))
				(21 (add-str-tval '(<bolt> <lightning>)))
				(22 (add-str-tval '(<bolt> <fire>)))
				(23 (add-str-tval '(<bolt> <cold>)))
				(24 (add-str-tval '(<ball> <acid>)))
				(25 (add-str-tval '(<ball> <lightning)))
				(26 (add-str-tval '(<ball> <fire>)))
				(27 (add-str-tval '(<ball> <cold)))
				(otherwise (warn "Unknown subtype for rod ~a" subtype-val))))
		       
			 (70  (add-str-tval '<scroll>)
			      
			      (case subtype-val
				(0 (add-str-tval '<darkness>))
				(1 (add-str-tval '<aggravate>))
				(2 (add-str-tval '<curse-armour>))
				(3 (add-str-tval '<curse-weapon>))
				(4 (add-str-tval '(<summon> <monster>)))
				(5 (add-str-tval '(<summon> <undead>)))
			      
				(7 (add-str-tval '<create-trap>))
				(8 (add-str-tval '<phase-door>))
				(9 (add-str-tval '<teleportation>))
				(10 (add-str-tval '<teleport-level>))
				(11 (add-str-tval '<word-of-recall>))
				(12 (add-str-tval '(<identify> <normal>)))
				(13 (add-str-tval '(<identify> <powerful>)))
				(14 (add-str-tval '(<remove-curse> <normal>)))
				(15 (add-str-tval '(<remove-curse> <powerful>)))
				(16 (add-str-tval '(<enchant> <armour> <normal>)))
				(17 (add-str-tval '(<enchant> <weapon> <to-hit>)))
				(18 (add-str-tval '(<enchant> <weapon> <to-dmg>)))
			      			      
				(20 (add-str-tval '(<enchant> <armour> <powerful>)))
				(21 (add-str-tval '(<enchant> <weapon> <powerful>)))
				(22 (add-str-tval '<recharge>))
			      
				(24 (add-str-tval '<light>))
				(25 (add-str-tval '<mapping>))
				(26 (add-str-tval '(<detect> <money>)))
				(27 (add-str-tval '(<detect> <item>)))
				(28 (add-str-tval '(<detect> <trap>)))
				(29 (add-str-tval '(<detect> <door>)))
				(30 (add-str-tval '(<detect> <invisible>)))
				(32 (add-str-tval '<satisfy-hunger>))
				(33 (add-str-tval '(<blessing> <light>)))
				(34 (add-str-tval '(<blessing> <chant>)))
				(35 (add-str-tval '(<blessing> <prayer>)))
				(36 (add-str-tval '<confuse-monster>))
				(37 (add-str-tval '(<protection> <evil>)))
				(38 (add-str-tval '(<protection> <rune>)))
				(39 (add-str-tval '<trap/door-destruction>))
			      
				(41 (add-str-tval '(<destruction> <powerful>)))
				(42 (add-str-tval '(<dispel> <undead>)))
			      
				(44 (add-str-tval '<genocide>))
				(45 (add-str-tval '(<genocide> <mass>)))
				(46 (add-str-tval '(<acquirement> <normal>)))
				(47 (add-str-tval '(<acquirement> <powerful>)))
				(otherwise (warn "Unknown subtype for scroll ~a" subtype-val))))
		       
			 (75  (add-str-tval '<potion>)
			      ;; food value?
			      (when (> patch-val 0)
				(let ((gval (ensure-gval!)))
				  (setf (gval.food-val gval) patch-val)
				  (setq patch-val 0)))
			      
			      (case subtype-val
				(0 (add-str-tval '<water>))
				(1 (add-str-tval '<apple-juice>))
				(2 (add-str-tval '<slime-mold>))
				(4 (add-str-tval '<slowness>))
				(5 (add-str-tval '<salt-water>))
				(6 (add-str-tval '<poison>))
				(7 (add-str-tval '<blindness>))
				(9 (add-str-tval '<confusion>))
				(11 (add-str-tval '<sleep>))
				(13 (add-str-tval '<amnesia>))
				(15 (add-str-tval '<ruination>))
				(16 (add-str-tval '(<reduce> <str>)))
				(17 (add-str-tval '(<reduce> <int>)))
				(18 (add-str-tval '(<reduce> <wis>)))
				(19 (add-str-tval '(<reduce> <dex>)))
				(20 (add-str-tval '(<reduce> <con>)))
				(21 (add-str-tval '(<reduce> <chr>)))
				(22 (add-str-tval '<detonations>))
				(23 (add-str-tval '<death>))
				(24 (add-str-tval '<infravision>))
				(25 (add-str-tval '(<detect> <invisible>)))
				(26 (add-str-tval '<slow-poison>))
				(27 (add-str-tval '(<cure> <poison>)))
				(28 (add-str-tval '<boldness>))
				(29 (add-str-tval '<speed>))
				(30 (add-str-tval '(<resist> <fire>)))
				(31 (add-str-tval '(<resist> <cold>)))
				(32 (add-str-tval '<heroism>))
				(33 (add-str-tval '<berserk-strength>))
				(34 (add-str-tval '(<cure> <light>)))
				(35 (add-str-tval '(<cure> <serious>)))
				(36 (add-str-tval '(<cure> <critical>)))
				(37 (add-str-tval '(<cure> <healing> <normal>)))
				(38 (add-str-tval '(<cure> <healing> <powerful>)))
				(39 (add-str-tval '<life>))
				(40 (add-str-tval '(<restore> <mana>)))
				(41 (add-str-tval '(<restore> <xp>)))
				(42 (add-str-tval '(<restore> <str>)))
				(43 (add-str-tval '(<restore> <int>)))
				(44 (add-str-tval '(<restore> <wis>)))
				(45 (add-str-tval '(<restore> <dex>)))
				(46 (add-str-tval '(<restore> <con>)))
				(47 (add-str-tval '(<restore> <chr>)))
				(48 (add-str-tval '(<increase> <str>)))
				(49 (add-str-tval '(<increase> <int>)))
				(50 (add-str-tval '(<increase> <wis>)))
				(51 (add-str-tval '(<increase> <dex>)))
				(52 (add-str-tval '(<increase> <con>)))
				(53 (add-str-tval '(<increase> <chr>)))
				(55 (add-str-tval '<augmentation>))
				(56 (add-str-tval '(<enlightenment> <normal>)))
				(57 (add-str-tval '(<enlightenment> <powerful>)))
				(58 (add-str-tval '<self-knowledge>))
				(59 (add-str-tval '<xp>))
			      
				(otherwise (warn "Unknown subtype for potion ~a" subtype-val))))
		       
			 (77  (add-str-tval '<flask>)
			      
			      (case subtype-val
				(0 (add-str-tval '<oil>)
				   (when (> patch-val 0)
				     (let ((gval (ensure-gval!)))
				       (setf (gval.charges gval) patch-val)
				       (setq patch-val 0))))
				(otherwise (warn "Unknown subtype for flask ~a" subtype-val))))

			 (80  (add-str-tval '<food>)
			      ;; food value?
			      (when (> patch-val 0)
				(let ((gval (ensure-gval!)))
				  (setf (gval.food-val gval) patch-val)
				  (setq patch-val 0)))
			      
			      (when (< subtype-val 20)
				(add-str-tval '<mushroom>))
			    
			      (case subtype-val
				(0 (add-str-tval '<poison>))
				(1 (add-str-tval '<blindness>))
				(2 (add-str-tval '<paranoia>))
				(3 (add-str-tval '<confusion>))
				(4 (add-str-tval '<hallucination>))
				(5 (add-str-tval '<paralysis>))
				(6 (add-str-tval '(<reduce> <str>)))
				(7 (add-str-tval '(<reduce> <con>)))
				(8 (add-str-tval '(<reduce> <int>)))
				(9 (add-str-tval '(<reduce> <wis>)))
				(10 (add-str-tval '(<reduce> <con> <powerful>)))
				(11 (add-str-tval '(<reduce> <str> <powerful>)))
				(12 (add-str-tval '(<cure> <poison>)))
				(13 (add-str-tval '(<cure> <blindness>)))
				(14 (add-str-tval '(<cure> <paranoia>)))
				(15 (add-str-tval '(<cure> <confusion>)))
				(16 (add-str-tval '(<cure> <serious>)))
				(17 (add-str-tval '(<restore> <str>)))
				(18 (add-str-tval '(<restore> <con>)))
				(19 (add-str-tval '<restoring>))
			      
				;; other food
				(32 (add-str-tval '<biscuit>))
				(33 (add-str-tval '<jerky>))
				(35 (add-str-tval '<ration>))
				(36 (add-str-tval '<slime-mold>))
				(37 (add-str-tval '<waybread>))
				(38 (add-str-tval '<ale>))
				(39 (add-str-tval '<wine>))

				(otherwise (warn "Unknown subtype for food ~a" subtype-val))))
		       
			      (90  (add-str-tval '(<spellbook> <mage>))
				   
				   (case subtype-val
				     (0 (add-str-tval '<beginner>))
				     (1 (add-str-tval '<conjuring>))
				     (2 (add-str-tval '<illusions>))
				     (3 (add-str-tval '<sorcery>))
				     (4 (add-str-tval '<resistance>))
				     (5 (add-str-tval '<escapes>))
				     (6 (add-str-tval '<grimoire>))
				     (7 (add-str-tval '<transformations>))
				     (8 (add-str-tval '<destruction>))
				     (otherwise (warn "Unknown subtype for mage spellbook ~a" subtype-val))))
		       
				     
			      (91  (add-str-tval '(<spellbook> <priest>))
				   
				   (case subtype-val
				     (0 (add-str-tval '<beginner>))
				     (1 (add-str-tval '<words>))
				     (2 (add-str-tval '<chants>))
				     (3 (add-str-tval '<exorcism>))
				     (4 (add-str-tval '<openings>))
				     (5 (add-str-tval '<insights>))
				     (6 (add-str-tval '<healing>))
				     (7 (add-str-tval '<infusions>))
				     (8 (add-str-tval '<wrath>))
				     (otherwise (warn "Unknown subtype for food ~a" subtype-val))))
			      
			      (100 (add-str-tval '<money>))
				   

			      (otherwise
			       (warn "Unable to find type of object ~a" type-val)))

		       (when (> patch-val 0)
			 (warn "Patch-val is ~a for ~a" patch-val cur-obj))
		       

		       ))

		     ;; no duplicates please
		     (setf (object.obj-type cur-obj) (remove-duplicates (object.obj-type cur-obj)))
		     ))
		 	
		 ;; Extra info
		 ((#\w #\W)
		  (when cur-obj
		    (let ((res (split-seq-on l #\:)))
		      ;; the first should be W
		      (assert (string-equal (car res) "w"))
		      ;; the second should be level
		      (setf (object.level cur-obj) (parse-integer (second res)))
		      ;; the third should be rarity
		      (setf (object.rarity cur-obj) (parse-integer (third res)))
		      ;; the fourth should be weight
		      (setf (object.weight cur-obj) (parse-integer (fourth res)))
		      ;; the fifth should be cost
		      (setf (object.cost cur-obj) (parse-integer (fifth res)))

		   
		      )))
		
		 ((#\f #\F)
		  (when cur-obj
		    (let* ((res (split-seq-on (subseq l 2) #\|))
			   (cur-val (object.game-values cur-obj))
			   (game-vals (if cur-val cur-val (make-game-values)))
			   (real-res (loop for i in res collecting (string-trim '(#\Space #\Tab #\Newline) i))))
		     
		      (dolist (j real-res)
			(when (> (length j) 0)
			  (cond ((string-equal j "SHOW_MODS")
				 (push '<show-modififers> (object.flags cur-obj)))
				((string-equal j "INSTA_ART")
				 (push '<instant-artifact> (object.flags cur-obj)))
				((string-equal j "ACTIVATE")
				 (push '<activation> (object.flags cur-obj)))
				((string-equal j "EASY_KNOW")
				 (push '<easy-know> (object.flags cur-obj)))
				((string-equal j "HIDE_TYPE")
				 (push '<hide-type> (object.flags cur-obj)))
				((string-equal j "LIGHT_CURSE")
				 (push '<curse> (object.flags cur-obj)))

			       
				((string-equal j "IGNORE_ACID")
				 (push '<acid> (gval.ignores game-vals)))
				((string-equal j "IGNORE_FIRE")
				 (push '<fire> (gval.ignores game-vals)))
				((string-equal j "IGNORE_COLD")
				 (push '<cold> (gval.ignores game-vals)))
				((string-equal j "IGNORE_ELEC")
				 (push '<electricity> (gval.ignores game-vals)))

				((string-equal j "FREE_ACT")
				 (push '<free-action> (gval.abilities game-vals)))
				((string-equal j "SLOW_DIGEST")
				 (push '<slow-digestion> (gval.abilities game-vals)))
				((string-equal j "AGGRAVATE")
				 (push '<aggravate> (gval.abilities game-vals)))
				((string-equal j "SEE_INVIS")
				 (push '<see-invisible> (gval.abilities game-vals)))
				((string-equal j "TELEPORT")
				 (push '<teleport> (gval.abilities game-vals)))
				((string-equal j "FEATHER")
				 (push '<feather-fall> (gval.abilities game-vals)))
				((string-equal j "TUNNEL")
				 (setf (gval.tunnel game-vals) patch-val))
				((string-equal j "SPEED")
				 (push '<speed> (gval.abilities game-vals)))
			       
				((string-equal j "SEARCH")
				 (push '<search> (gval.skill-bonuses game-vals)))

				((string-equal j "SLAY_UNDEAD")
				 (push '<undead> (gval.slays game-vals)))

				((string-equal j "STR")
				 (push '<str> (gval.stat-bonuses game-vals)))
				((string-equal j "DEX")
				 (push '<dex> (gval.stat-bonuses game-vals)))
				((string-equal j "CON")
				 (push '<con> (gval.stat-bonuses game-vals)))
				((string-equal j "INT")
				 (push '<int> (gval.stat-bonuses game-vals)))
				((string-equal j "WIS")
				 (push '<wis> (gval.stat-bonuses game-vals)))
				((string-equal j "CHR")
				 (push '<chr> (gval.stat-bonuses game-vals)))
									       
				((string-equal j "SUST_STR")
				 (push '<str> (gval.sustains game-vals)))
				((string-equal j "SUST_DEX")
				 (push '<dex> (gval.sustains game-vals)))
				((string-equal j "SUST_CON")
				 (push '<con> (gval.sustains game-vals)))
				((string-equal j "SUST_INT")
				 (push '<int> (gval.sustains game-vals)))
				((string-equal j "SUST_WIS")
				 (push '<wis> (gval.sustains game-vals)))
				((string-equal j "SUST_CHR")
				 (push '<chr> (gval.sustains game-vals)))
						       
				((string-equal j "RES_FIRE")
				 (push '<fire> (gval.resists game-vals)))
				((string-equal j "RES_COLD")
				 (push '<cold> (gval.resists game-vals)))
				((string-equal j "RES_ACID")
				 (push '<acid> (gval.resists game-vals)))
				((string-equal j "RES_ELEC")
				 (push '<electricity> (gval.resists game-vals)))
				((string-equal j "RES_POIS")
				 (push '<poison> (gval.resists game-vals)))
				((string-equal j "RES_CHAOS")
				 (push '<chaos> (gval.resists game-vals)))
				((string-equal j "RES_NEXUS")
				 (push '<nexus> (gval.resists game-vals)))
				((string-equal j "RES_NETHR")
				 (push '<nether> (gval.resists game-vals)))
				((string-equal j "RES_DISEN")
				 (push '<disenchant> (gval.resists game-vals)))
				((string-equal j "RES_DARK")
				 (push '<dark> (gval.resists game-vals)))
				((string-equal j "RES_CONFU")
				 (push '<confusion> (gval.resists game-vals)))
				((string-equal j "RES_SOUND")
				 (push '<sound> (gval.resists game-vals)))
				((string-equal j "RES_LITE")
				 (push '<light> (gval.resists game-vals)))
				((string-equal j "RES_SHARD")
				 (push '<shard> (gval.resists game-vals)))			       
				(t
				 (warn "Non-supported obj-kind flag ~s" j))) 
			  ))
		     
		      ;; add it to the kind
		      (setf (object.game-values cur-obj) game-vals)		     
		      )))

		 ((#\p #\P)
		  (when cur-obj
		    (let* ((res (split-seq-on l #\:))
			   (cur-val (object.game-values cur-obj))
			   (game-vals (if cur-val cur-val (make-game-values))))
		     
		      ;;		     (warn "Going parse of res ~a" res)
		      ;; the first should be P
		      (assert (string-equal (car res) "p"))
		     
		      ;; the second should be ac
		      (setf (gval.base-ac game-vals) (parse-integer (second res)))
		      ;; the third should be num dice and base dice
		      (let ((the-dice (parse-dice (third res))))
			(setf (gval.num-dice  game-vals) (car the-dice)
			      (gval.base-dice game-vals) (cdr the-dice)))
		      ;; the fourth is to-hit
		      (setf (gval.tohit-bonus game-vals) (parse-integer (fourth res)))
		      ;; the fifth is dmg
		      (setf (gval.dmg-bonus game-vals) (parse-integer (fifth res)))
		      ;; the sixth is ac bonus
		      (setf (gval.ac-bonus game-vals) (parse-integer (sixth res)))

		      ;; add it to the kind
		      (setf (object.game-values cur-obj) game-vals)
		      )))
		
		 ;; allocation stuff
		 ((#\a #\A)
		  (when cur-obj
		    (let ((res (split-seq-on l #\:)))

		      (loop for i from 0
			    for j in res
			    do
			    ;; first
			    (cond ((eq i 0)
				   (assert (string-equal j "a")))
				  (t
				   (let* ((pos (position #\/ j))
					  (locale (parse-integer (subseq j 0 pos)))
					  (chance (parse-integer (subseq j (1+ pos)))))

				     ;;				      (warn "checking ~a which is ~a ~a" j locale chance) 
				     (setf (svref (object.chance cur-obj) (1- i)) chance)
				     (setf (svref (object.locale cur-obj) (1- i)) locale)
				      
				     ))
				  ))
				 
			       

		      )))
		
		 (otherwise

		  (warn "Fell through ~a" first-char)))

		))
	    (when cur-obj
	      (add-new-okind! cur-obj (object.id cur-obj))
;;	      (setf (get-obj-kind (object.id cur-obj)) cur-obj)
	      (setq cur-obj nil))
      
	    )))


(pushnew :compatibility-objects cl:*features*)
