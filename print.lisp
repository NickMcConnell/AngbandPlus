;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: print.lisp - various display code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Various code which just prints stuff somewhere

|#

(in-package :org.langband.engine)


(defconstant +token-name+ 1)
(defconstant +token-cur-mp+ 2)
(defconstant +token-max-mp+ 3)
(defconstant +token-nrm-lvl+ 4)
(defconstant +token-big-lvl+ 5)
(defconstant +token-nrm-xp+ 6)
(defconstant +token-big-xp+ 7)
(defconstant +token-cur-hp+ 8)
(defconstant +token-max-hp+ 9)
(defconstant +token-cur-ac+ 10)
(defconstant +token-au+ 11)
(defconstant +token-stat+ 12)
(defconstant +token-empty-field+ 24)

(defun print-field (str coord)
  "Print string at given coordinates in light-blue."
  ;; clear and then write
  (let ((y (car coord))
	(x (cdr coord)))

    (c-prt-token! +term-white+ +token-empty-field+
		  y x)

    ;; (put-coloured-str! +term-white+ "            " x y)
    (put-coloured-str! +term-l-blue+ str x y)
    ))

(defun print-stat (player setting num)
  "Prints stats in the left frame."

  (let* ((stat-val (svref (player.active-stats player) num))
	 (max-val (svref (player.modbase-stats player) num))
	 (reduced-stat-p (> max-val stat-val))
	 (stat-set (slot-value setting 'stat))
	 (row (car stat-set))
	 (col (cdr stat-set))
	 ;;(name (get-stat-name-from-num num))
	 )
    
    (c-prt-token! +term-white+ (if reduced-stat-p
				   (+ +token-stat+ num)
				   (+ +token-stat+ num 6))
		  (+ num row)
		  col)
    
;;    (put-coloured-str! +term-white+ (if reduced-stat-p
;;		   name
;;		   (string-upcase name))
;;	       col
;;	       (+ num row))


    (c-prt-stat! (if reduced-stat-p +term-yellow+ +term-l-green+)
		 stat-val (+ row num) (+ col 6))
    
    ))

(defun print-title (player setting)
  "Prints the title for the class at the player's level."
  
  (let* ((the-class (player.class player))
	 (the-lvl (player.level player))
	 (title-set (slot-value setting 'title))
	 (title (get-title-for-level the-class the-lvl)))
    (print-field title title-set)))

(defun print-level (player setting)
  "Prints level in the left frame."
  
  (let* ((lev (player.level player))
	 (lev-set (slot-value setting 'level))
	 (lower-lvl-p (< lev (player.max-level player))))
    
    (c-prt-token! +term-white+ (if lower-lvl-p +token-nrm-lvl+ +token-big-lvl+)
		  (car lev-set) (cdr lev-set))

    (c-prt-number! (if lower-lvl-p +term-yellow+ +term-l-green+)
		   lev
		   6
		   (car lev-set)
		   (+ (cdr lev-set) 6))))

(defun print-xp (player setting)
  "Prints xp in the left frame."
  (let* ((xp (player.cur-xp player))
	 (xp-set (slot-value setting 'xp))
	 (lower-xp-p (< xp (player.max-xp player))))

    (c-prt-token! +term-white+ (if lower-xp-p +token-nrm-xp+ +token-big-xp+)
		  (car xp-set) (cdr xp-set))

    (c-prt-number! (if lower-xp-p +term-yellow+ +term-l-green+)
		   xp
		   8
		   (car xp-set)
		   (+ (cdr xp-set) 4))))


(defun print-gold (player setting)
  "Prints gold to left frame."
  
  (let ((gold (player.gold player))
	(gold-set (slot-value setting 'gold)))

    (c-prt-token! +term-white+ +token-au+ (car gold-set) (cdr gold-set))

    (c-prt-number! +term-l-green+ gold 9
		   (car gold-set)
		   (+ (cdr gold-set) 3))
    ))


(defun print-armour-class (player setting)
  "Prints AC to left frame."
  
  (let* ((perc (player.perceived-abilities player))
	 (ac (+ (pl-ability.base-ac perc)
		(pl-ability.ac-modifier perc)))
	(ac-set (slot-value setting 'ac)))

    (c-prt-token! +term-white+ +token-cur-ac+ (car ac-set) (cdr ac-set))

    (c-prt-number! +term-l-green+
		   ac
		   5
		   (car ac-set)
		   (+ (cdr ac-set) 7))))


(defun print-hit-points (player setting)
  "Prints hit-points info to left frame."
  
  (let ((cur-hp (current-hp player))
	(max-hp (maximum-hp player))
	(cur-set (slot-value setting 'cur-hp))
	(max-set (slot-value setting 'max-hp)))
    
    (c-prt-token! +term-white+ +token-max-hp+ (car max-set) (cdr max-set))
    (c-prt-token! +term-white+ +token-cur-hp+ (car cur-set) (cdr cur-set))

    ;; max
    (c-prt-number! +term-l-green+
		    max-hp
		    5
		    (car max-set)
		    (+ (cdr max-set) 7))

    ;; cur
    (c-prt-number! (cond ((>= cur-hp max-hp) +term-l-green+)
			 ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
			 (t +term-red+))
		   cur-hp
		   5
		   (car cur-set)
		   (+ (cdr cur-set) 7))
    
    ))

(defmethod print-mana-points ((variant variant) player setting)
  "Prints mana-points info to left frame."
  
  (let ((cur-hp (current-mana player))
	(max-hp (maximum-mana player))
	(cur-set (slot-value setting 'cur-mana))
	(max-set (slot-value setting 'max-mana)))

    (c-prt-token! +term-white+ +token-max-mp+ (car max-set) (cdr max-set))
    (c-prt-token! +term-white+ +token-cur-mp+ (car cur-set) (cdr cur-set))
  
    (c-prt-number! +term-l-green+
		   max-hp
		   5
		   (car max-set)
		   (+ (cdr max-set) 7))


    (c-prt-number! (cond ((>= cur-hp max-hp) +term-l-green+)
			 ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
			 (t +term-red+))

		   cur-hp
		   5
		   (car cur-set)
		   (+ (cdr cur-set) 7))

    ))
  

(defun print-basic-frame (variant dungeon player)
  "Prints the left frame with basic info"
  
  (declare (ignore dungeon))
  ;;  (warn "Printing basic frame..")

  (let ((pr-set (get-setting variant :basic-frame-printing))
	(bot-set (get-setting variant :bottom-row-printing))
	(stat-len (variant.stat-length variant)))
  
    (print-field (get-race-name player) (slot-value pr-set 'race))
    (print-field (get-class-name player) (slot-value pr-set 'class))
    (print-title player pr-set)

    (print-level player pr-set)
    (print-xp player pr-set) 
  
    (dotimes (i stat-len)
      (print-stat player pr-set i))

    (print-armour-class player pr-set)
    (print-hit-points player pr-set)
    (print-mana-points variant player pr-set)

    (print-gold player pr-set)

    #||
    #+maintainer-mode
    (let ((food-set (slot-value pr-set 'food))
	  (energy-set (slot-value pr-set 'energy)))
      
      (print-field "Food" food-set)
      (print-field "Energy" energy-set)
      
      (c-prt-number! +term-l-green+ (player.food player) 5
		     (car food-set) (+ (cdr food-set) 7))
      (c-prt-number! +term-l-green+ (player.energy player) 5
		     (car energy-set) (+ (cdr energy-set) 7))
      )
    ||#
      
    
    (print-depth *level* bot-set)
  
    ))



(defmethod print-speed ((variant variant) (player player) (setting bottom-row-locations))
  (let ((column (slot-value setting 'speed))
	(speed (player.speed player))
        (colour +term-l-green+))
    (with-foreign-str (s)
      (cond ((= speed +speed-base+)
             (lb-format s "            "))
            ((> speed +speed-base+)
	     (lb-format s "Fast (+~d)" (- speed +speed-base+)))
            (t
             (lb-format s "Slow (-~d)" (abs (- speed +speed-base+)))
             (setf colour +term-l-umber+)
             ))

      ;;(warn "Printing speed ~s" s)

      (put-coloured-str! +term-l-green+ s column
			 (get-last-console-line))
      )))
  
  
(defmethod display-creature ((variant variant) (player player) &key mode)

  (declare (ignore mode))
  
  (c-clear-from! 0)
  (display-player-misc  variant player)
  (display-player-stats variant player)
  (display-player-extra variant player)
  )

(defun display-player-misc (variant player)
  (declare (ignore variant))
  (let* ((the-class (player.class player))
	 (the-lvl (player.level player))
	 (title (get-title-for-level the-class the-lvl)))

    (flet ((print-info (title text row)
	     (put-coloured-str! +term-white+ title 1 row)
	     (put-coloured-str! +term-l-blue+ text 8 row)))

      (print-info "Name" (player.name player) 2)
      (print-info "Gender" (get-gender-name player) 3)
      (print-info "Race" (get-race-name player) 4)
      (print-info "Class"  (get-class-name player) 5)
      (print-info "Title" title 6)
      
      (print-info "HP" (format nil "~d/~d" (current-hp player)
			       (maximum-hp player))
		  7)
      
      (print-info "MP" (format nil "~d/~d" (current-mana player)
			       (maximum-mana player))
		       8)

      )))

(defun display-player-extra (variant player)
  (declare (ignore variant))
  (let* ((col 26)
	 (f-col (+ 9 25)) 
	 (cur-xp (player.cur-xp player))
	 (max-xp (player.max-xp player))
	 (lvl (player.level player))
	 (misc (player.misc player)))
    

    (put-coloured-str! +term-white+  "Age" col 3)
    (put-coloured-str! +term-l-blue+ (%get-4str (playermisc.age misc)) f-col 3)

    (put-coloured-str! +term-white+  "Height" col 4)
    (put-coloured-str! +term-l-blue+ (%get-4str (playermisc.height misc))  f-col 4)

    (put-coloured-str! +term-white+  "Weight" col 5)
    (put-coloured-str! +term-l-blue+ (%get-4str (playermisc.weight misc))  f-col 5)
    (put-coloured-str! +term-white+  "Status" col 6)
    (put-coloured-str! +term-l-blue+ (%get-4str (playermisc.status misc))  f-col 6)

    ;; always in maximize and preserve, do not include

    ;; another area
    (setq col 1)
    (setq f-col (+ col 8))

    (put-coloured-str! +term-white+ "Level" col 10)
    (put-coloured-str! (if (>= lvl
			       (player.max-level player))
			   +term-l-green+
			   +term-yellow+)
		       (format nil "~10d" lvl)  f-col 10)
  
  
    (put-coloured-str! +term-white+ "Cur Exp" col 11)
    
    (put-coloured-str! (if (>= cur-xp
			       max-xp)
			   +term-l-green+
			   +term-yellow+)
		       (format nil "~10d" cur-xp) f-col 11)
    
    (put-coloured-str! +term-white+ "Max Exp" col 12)
    
    (put-coloured-str! +term-l-green+
		       (format nil "~10d" max-xp) f-col 12)
    

    (put-coloured-str! +term-white+ "Adv Exp" col 13)
    (put-coloured-str! +term-l-green+
		       (format nil "~10d" (aref (player.xp-table player)
						(player.level player)))
		       f-col 13)

    
    
    (put-coloured-str! +term-white+ "Gold" col 15)

    (put-coloured-str! +term-l-green+
		       (format nil "~10d" (player.gold player)) f-col 15)

    (put-coloured-str! +term-white+ "Burden" col 17)
    (let* ((weight (player.burden player))
	   (pound (int-/ weight 10))
	   (frac (mod weight 10))
	   (str (format nil "~10d.~d lbs" pound frac)))
      (put-coloured-str! +term-l-green+ str f-col 17))

    ;; middle again
    (setq col 26)
    (setq f-col (+ col 5))

    (put-coloured-str! +term-white+ "Armour" col 10)
    (let* ((perc (player.perceived-abilities player))
	   (p-ac (pl-ability.base-ac perc))
	   (p-acmod (pl-ability.ac-modifier perc)))
         
      (put-coloured-str! +term-l-blue+
			 (format nil "~12@a" (format nil "[~d,~@d]" p-ac p-acmod))
			 (1+ f-col) 10))

;;    (let ((weapon (get-weapon player)))
;;     
;;      nil)

    (let* ((perc (player.perceived-abilities player))
	   (tohit (pl-ability.to-hit-modifier perc))
	   (dmg (pl-ability.to-dmg-modifier perc))
	   )
      
    (put-coloured-str! +term-white+  "Fight" col 11)
    (put-coloured-str! +term-l-blue+ (format nil "~13@a" (format nil "(~@d,~@d)" tohit dmg))
		       f-col 11)

    ;; skip weapon+bow specifics now
    (put-coloured-str! +term-white+  "Melee" col 12)
    (put-coloured-str! +term-l-blue+ (format nil "~13@a" (format nil "(~@d,~@d)" tohit dmg))
		       f-col 12)

    (put-coloured-str! +term-white+ "Shoot" col 13)
    (put-coloured-str! +term-l-blue+ (format nil "~13@a" (format nil "(~@d,+0)" tohit))
		       f-col 13)

    )
    
    (put-coloured-str! +term-white+  "Blows" col 14)
    (put-coloured-str! +term-l-blue+ (%get-13astr "1/turn")
		       f-col 14)
    
    (put-coloured-str! +term-white+  "Shots" col 15)
    (put-coloured-str! +term-l-blue+ (%get-13astr "1/turn")
		       f-col 15)
		   
    (put-coloured-str! +term-white+ "Infra" col 17)
	    
    (put-coloured-str! +term-l-blue+
		       (format nil "~10d ft" (* 10 (player.infravision player)))
		       f-col 17)

    ;; then right
    (setq col 49)
    (setq f-col (+ col 14))

    (flet ((print-skill (skill div row)
	     (declare (ignore div))
	     (let ((val (slot-value (player.skills player) skill)))
	       (put-coloured-str! +term-l-green+
				  (format nil "~9d" val)
				  f-col
				  row))))
      
      (put-coloured-str! +term-white+ "Saving Throw" col 10)
      (print-skill 'saving-throw 6 10)

      (put-coloured-str! +term-white+ "Stealth" col 11)
      (print-skill 'stealth 1 11)
      
      (put-coloured-str! +term-white+ "Fighting" col 12)
      (print-skill 'fighting 12 12)

      (put-coloured-str! +term-white+ "Shooting" col 13)
      (print-skill 'shooting 12 13)

      (put-coloured-str! +term-white+ "Disarming" col 14)
      (print-skill 'disarming 8 14)

      (put-coloured-str! +term-white+ "Magic Device" col 15)
      (print-skill 'device 6 15)

      (put-coloured-str! +term-white+ "Perception" col 16)
      (print-skill 'perception 6 16)

      (put-coloured-str! +term-white+ "Searching" col 17)
      (print-skill 'searching 6 17))
      
  ))


(defun display-player-stats (variant player)
  ;;  (warn "Displaying character.. ")
  
  (let ((row 3)
	(col 42)
;;	(num-stats 6)
	;; more pressing variables
	(stat-len (variant.stat-length variant))
	(base (player.base-stats player))
;;	(cur (player.curbase-stats player))
	(mod (player.modbase-stats player))
	(active (player.active-stats player))
	(racial-adding (race.stat-changes (player.race player)))
	(class-adding (class.stat-changes (player.class player)))
	)
    ;; labels
    
    (put-coloured-str! +term-white+ "  Self" (+ col  5) (1- row))
    (put-coloured-str! +term-white+ " RB"    (+ col 12) (1- row))
    (put-coloured-str! +term-white+ " CB"    (+ col 16) (1- row))
    (put-coloured-str! +term-white+ " EB"    (+ col 20) (1- row))
    (put-coloured-str! +term-white+ "  Best" (+ col 24) (1- row))


    (dotimes (i stat-len)
      (let ((its-base (gsdfn base i))
	    ;;(its-cur (gsdfn cur i))
	    (its-mod (gsdfn mod i))
	    (its-active (gsdfn active i))
	      
	    (cur-race-add (gsdfn racial-adding i))
	    (cur-class-add (gsdfn  class-adding i)))

	(put-coloured-str! +term-white+ (get-stat-name-from-num i)
		    col
		   (+ i row))


	;; base stat
	(c-prt-stat! +term-l-green+ its-base (+ row i) (+ col 5))

;;	(put-coloured-str! +term-l-green+ (%get-stat its-base)
;;		       (+ col 5) (+ row i))


	;; racial bonus
	(put-coloured-str! +term-l-green+ (format nil "~3@d" cur-race-add)
			   (+ col 12)
			   (+ row i))

	;; class bonus
	(put-coloured-str! +term-l-green+ (format nil "~3@d" cur-class-add)
			   (+ col 16)
			   (+ row i))

	;; equipment
	(put-coloured-str! +term-l-green+ " +0"
			   (+ col 20)
			   (+ row i))


	;; max stat
	(c-prt-stat! +term-l-green+ its-mod (+ row i) (+ col 24))

;;	(put-coloured-str! +term-l-green+ (%get-stat its-mod)
;;		       (+ col 24) (+ row i))

	;; if active is lower than max
	(when (< its-active its-mod)
	  ;; max stat
	  (c-prt-stat! +term-yellow+ its-active (+ row i) (+ col 28)))

;;	  (put-coloured-str! +term-yellow+ (%get-stat its-active)
;;			 (+ col 28) (+ row i))
	      
	      
	)))


  )


;; no warning on duplicates
(defun register-help-topic& (variant topic)
  "Registers a help-topic with the variant."
  (setf (gethash (help-topic-id topic) (variant.help-topics variant)) topic))


(defun display-help-topics (variant title start-row)
  "Displays help-topics to screen and asks for input on further help."
  (let ((topics (variant.help-topics variant))
	(title-len (length title)))

    (flet ((show-title ()
	     (put-coloured-str! +term-l-blue+ title 12 start-row)
	     (put-coloured-str! +term-l-blue+ (make-string title-len :initial-element #\=)
				12 (1+ start-row)))
	   (show-entries ()
	     (loop for cnt from 3
		   for i being the hash-values of topics
		   do
		   (let ((key (help-topic-key i)))
		     (put-coloured-str! +term-l-green+ (format nil "~a." key) 3 (+ start-row cnt))
		     (put-coloured-str! +term-l-green+ (help-topic-name i)    6 (+ start-row cnt))
		     )))
	   (get-valid-key ()
	     (put-coloured-str! +term-l-blue+ "-> Please enter selection (Esc to exit): " 3 20)
	     (read-one-character)))

      (loop 
       (show-title)
       (show-entries)
       (let ((key (get-valid-key)))
	 (when (eql key +escape+)
	   (return-from display-help-topics nil)))
       ))))

(defun print-attack-graph (var-obj player)
;;  (declare (ignore player))
    (c-clear-from! 0)
    
    (dotimes (i 10)
      (let ((y (- 19 (* i 2))))
	(put-coloured-str! +term-l-blue+ (format nil "~3d%" (* i 10))
			3 y)))
    (dotimes (i 20)
      (put-coloured-str! +term-l-blue+ "|" 7 i))

    (put-coloured-str! +term-l-blue+ "CHANCE TO HIT" 64 1)
    (put-coloured-str! +term-l-blue+ "=============" 64 2)
    
    (put-coloured-str! +term-l-red+ "Unarm." 8 20)
    (put-coloured-str! +term-white+ "Leath." 13 21)
    (put-coloured-str! +term-orange+ "L. met." 18 22)
    (put-coloured-str! +term-yellow+ "Met." 22 20)
    (put-coloured-str! +term-violet+ "H. Met." 26 21)
    (put-coloured-str! +term-l-green+ "Plate" 30 22)
    (put-coloured-str! +term-l-red+ "H. Plate" 35 20)
    (put-coloured-str! +term-white+ "Dragon" 40 21)
    (put-coloured-str! +term-orange+ "H. Dragon" 44 22)
    (put-coloured-str! +term-yellow+ "Ench Drg" 50 20)
    (put-coloured-str! +term-violet+ "Legend" 62 21)
    (put-coloured-str! +term-l-green+ "Myth" 74 22)

    (let ((skill (get-known-combat-skill var-obj player)))
      
      (flet ((get-x (val)
	       (+ 8 val))
	     (get-y (val)
	       (- 19 (int-/ val 5))))
	(loop for i from 5 to 180 by 5
	      for j from 1 by 2
	      do
	      (let ((chance (get-chance var-obj skill i))
		    (desc (get-armour-desc var-obj i)))
		(check-type desc cons)
		(put-coloured-str! (cdr desc) "*" (get-x j) (get-y chance)))
	      
	      )))
      
			    
    (pause-last-line!)
    )

(defun print-attack-table (var-obj player)
;;  (declare (ignore player))
    (c-clear-from! 0)

    (put-coloured-str! +term-l-blue+ "CHANCE TO HIT" 2 0)
    (put-coloured-str! +term-l-blue+ "=============" 2 1)
    (let ((last-colour +term-green+)
	  (count 2)
	  (skill (get-known-combat-skill var-obj player)))
      (loop for i from 5 to 200 by 10
	    
	    do
	    (let ((desc (get-armour-desc var-obj i))
		  (chance (get-chance var-obj skill i)))
	      (check-type desc cons)
	      (cond ((equal last-colour (cdr desc))
		     ;; next
		     )
		    (t
		     (setf last-colour (cdr desc))
		     (incf count)
		     (put-coloured-str! (cdr desc) (format nil "~40a: ~a%" (car desc) chance)
					4 count)
		     ))
	      ))

      (c-print-text! 2 16 +term-l-blue+ "
The armour-value describes a full set-up of armour, including
    helmet, shield, gloves, boots, cloak and body-armour.  Parts of
    the outfit is expected to have appropriate enchantments, e.g a
    dragon-armour will always be slightly enchanted, just as a
    full-plate combat armour is enchanted to allow it's wearer to move
    at all.  A creature can have natural armour, but
    it might be just as tough as plated armour and as such use the
    plated armour label. " :end-col 75)

      
      
      (pause-last-line!)
      ))

(defun print-resists (var-obj player)
;;  (declare (ignore player))
    (c-clear-from! 0)

    (put-coloured-str! +term-l-blue+ "RESISTANCES" 2 0)
    (put-coloured-str! +term-l-blue+ "===========" 2 1)

    (let ((elms (variant.elements var-obj))
	  (resists (player.resists player))
	  (row 3))

      (dolist (i elms)
	(let* ((idx (element.number i))
	       (str (format nil "~20a ~20s ~10s" (element.name i) (element.symbol i) idx)))
	  (when (plusp (aref resists idx))
	    (put-coloured-str! +term-l-red+ "*" 1 row)) 
	  (put-coloured-str! +term-l-green+ str 3 row)
	  (incf row)))

      (pause-last-line!)
      ))

(defun print-misc-info (var-obj player)
  (declare (ignore var-obj player))

  (c-clear-from! 0)
  (put-coloured-str! +term-l-blue+ "MISC INFO" 2 0)
  (put-coloured-str! +term-l-blue+ "=========" 2 1)

  (put-coloured-str! +term-l-green+ "Height:" 2 3)
  (put-coloured-str! +term-yellow+ (format nil "~5d" 0) 12 3)
;;  (put-coloured-str! +term-l-green+ "Weight:" 2 4)
;;  (put-coloured-str! +term-yellow+ (format nil "~5d" (creature.weight player)) 12 4)
  (pause-last-line!)
  )
