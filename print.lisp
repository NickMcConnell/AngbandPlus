;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: print.lisp - various display code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Various code which just prints stuff somewhere

|#

(in-package :langband)


(defun print-field (str coord)
  "Print string at given coordinates in light-blue."
  ;; clear and then write
  (let ((y (car coord))
	(x (cdr coord)))
	  
  (c-col-put-str +term-white+ "            " y x)
  (c-col-put-str +term-l-blue+ str y x)))

(defun print-stat (pl setting num)
  "Prints stats in the left frame."

  (let* ((stat-val (svref (player.active-stats pl) num))
	 (max-val (svref (player.modbase-stats pl) num))
	 (reduced-stat-p (> max-val stat-val))
	 (stat-set (slot-value setting 'stat))
	 (row (car stat-set))
	 (col (cdr stat-set))
	 (name (get-stat-name-from-num num)))
    
    (c-put-str (if reduced-stat-p
		   name
		   (string-upcase name))
	       (+ num row)
	       col)
    
    (c-col-put-str (if reduced-stat-p +term-yellow+ +term-l-green+)
		   (cnv-stat stat-val)
		   (+ row num)
		   (+ col 6))
    ))

(defun print-title (pl setting)
  "Prints the title for the class at the player's level."
  
  (let* ((the-class (player.class pl))
	 (the-lvl (player.level pl))
	 (title-set (slot-value setting 'title))
	 (title (get-title-for-level the-class the-lvl)))
    (print-field title title-set)))

(defun print-level (pl setting)
  "Prints level in the left frame."
  
  (let* ((lev (player.level pl))
	 (lev-set (slot-value setting 'level))
	 (lower-lvl-p (< lev (player.max-level pl))))
    
    (c-put-str (if lower-lvl-p "Level" "LEVEL")
	       (car lev-set) (cdr lev-set))
    (c-col-put-str (if lower-lvl-p +term-yellow+ +term-l-green+)
		   (format nil "~6d" lev)
		   (car lev-set)
		   (+ (cdr lev-set) 6))))

(defun print-xp (pl setting)
  "Prints xp in the left frame."
  (let* ((xp (player.cur-xp pl))
	 (xp-set (slot-value setting 'xp))
	 (lower-xp-p (< xp (player.max-xp pl))))
    
    (c-put-str (if lower-xp-p "Exp" "EXP")
	       (car xp-set) (cdr xp-set))
    (c-col-put-str (if lower-xp-p +term-yellow+ +term-l-green+)
		   (format nil "~8d" xp)
		   (car xp-set)
		   (+ (cdr xp-set) 4))))

(defun print-gold (pl setting)
  "Prints gold to left frame."
  
  (let ((gold (player.gold pl))
	(gold-set (slot-value setting 'gold)))
    
    (c-put-str "AU"
	       (car gold-set) (cdr gold-set))
    (c-col-put-str +term-l-green+
		   (format nil "~9d" gold)
		   (car gold-set)
		   (+ (cdr gold-set) 3))))

(defun print-armour-class (pl setting)
  "Prints AC to left frame."
  
  (let ((ac (+ (player.base-ac pl)
	       (player.ac-bonus pl)))
	(ac-set (slot-value setting 'ac)))

       
    (c-put-str "Cur AC"
	       (car ac-set) (cdr ac-set))
    (c-col-put-str +term-l-green+
		   (format nil "~5d" ac)
		   (car ac-set)
		   (+ (cdr ac-set) 7))))


(defun print-hit-points (pl setting)
  "Prints hit-points info to left frame."
  
  (let ((cur-hp (player.cur-hp pl))
	(max-hp (player.max-hp pl))
	(cur-set (slot-value setting 'cur-hp))
	(max-set (slot-value setting 'max-hp)))
	
	(c-put-str "Max HP" (car max-set) (cdr max-set))
  
	(c-col-put-str +term-l-green+
	  (format nil "~5d" max-hp)
	  (car max-set)
	  (+ (cdr max-set) 7))

	(c-put-str "Cur HP" (car cur-set) (cdr cur-set))

	(c-col-put-str (cond ((>= cur-hp max-hp) +term-l-green+)
			     ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
			     (t +term-red+))
		       
	  (format nil "~5d" cur-hp)
	  (car cur-set)
	  (+ (cdr cur-set) 7))

	))

(defun print-mana-points (pl setting)
  "Prints mana-points info to left frame."
  
  (let ((cur-hp (player.cur-mana pl))
	(max-hp (player.max-mana pl))
	(cur-set (slot-value setting 'cur-mana))
	(max-set (slot-value setting 'max-mana)))
	
	(c-put-str "Max MP" (car max-set) (cdr max-set))
  
	(c-col-put-str +term-l-green+
	  (format nil "~5d" max-hp)
	  (car max-set)
	  (+ (cdr max-set) 7))

	(c-put-str "Cur MP" (car cur-set) (cdr cur-set))

	(c-col-put-str (cond ((>= cur-hp max-hp) +term-l-green+)
			     ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
			     (t +term-red+))
		       
	  (format nil "~5d" cur-hp)
	  (car cur-set)
	  (+ (cdr cur-set) 7))

	))
  

(defun print-basic-frame (dun pl)
  "Prints the left frame with basic info"
  
  (declare (ignore dun))
  ;;  (warn "Printing basic frame..")

  (let ((pr-set (get-setting :basic-frame-printing)))
  
    (print-field (get-race-name pl) (slot-value pr-set 'race))
    (print-field (get-class-name pl) (slot-value pr-set 'class))
    (print-title pl pr-set)

    (print-level pl pr-set)
    (print-xp pl pr-set) 
  
    (dotimes (i +stat-length+)
      (print-stat pl pr-set i))

    (print-armour-class pl pr-set)
    (print-hit-points pl pr-set)
    (print-mana-points pl pr-set) 
  
    (print-gold pl pr-set)
    (print-depth *level* pr-set)
  
    ))


(defmethod print-depth (level setting)
  "prints current depth somewhere"
  (declare (ignore setting))
  (c-prt (format nil "~a ft" (* 50 (level.depth level))) *last-console-line* 70)) ;;fix 


(defun display-player (player &optional mode)

  (declare (ignore mode))
  
  (c-clear-from 0)
  (display-player-misc player)
  (display-player-stats player)
  (display-player-extra player)
  )

(defun display-player-misc (player)
  
  (let* ((the-class (player.class player))
	 (the-lvl (player.level player))
	 (title (get-title-for-level the-class the-lvl)))

    (flet ((print-info (title text row)
	     (c-put-str title row 1)
	     (c-col-put-str +term-l-blue+ text row 8)))

      (print-info "Name" (player.name player) 2)
      (print-info "Sex" (get-sex-name player) 3)
      (print-info "Race" (get-race-name player) 4)
      (print-info "Class"  (get-class-name player) 5)
      (print-info "Title" title 6)
      
      (print-info "HP" (format nil "~d/~d" (player.cur-hp player)
			       (player.max-hp player))
		  7)
      
      (print-info "MP" (format nil "~d/~d" (player.cur-mana player)
			       (player.max-mana player))
		       8)

      )))

(defun display-player-extra (pl)

  (let ((col 26)
	(f-col (+ 9 25)) 
	(cur-xp (player.cur-xp pl))
	(max-xp (player.max-xp pl))
	(lvl (player.level pl)))


    (c-put-str "Age" 3 col)
    (c-col-put-str +term-l-blue+ (format nil "~4d" 0)  3 f-col)

    (c-put-str "Height" 4 col)
    (c-col-put-str +term-l-blue+ (format nil "~4d" 0)  4 f-col)

    (c-put-str "Weight" 5 col)
    (c-col-put-str +term-l-blue+ (format nil "~4d" 0)  5 f-col)
    (c-put-str "Status" 6 col)
    (c-col-put-str +term-l-blue+ (format nil "~4d" 0)  6 f-col)

    ;; always in maximize and preserve, do not include

    ;; another area
    (setq col 1)
    (setq f-col (+ col 8))

    (c-put-str "Level" 10 col)
    (c-col-put-str (if (>= lvl
			   (player.max-level pl))
		       +term-l-green+
		       +term-yellow+)
		   (format nil "~10d" lvl)  10 f-col)
  
  
    (c-put-str "Cur Exp" 11 col)
    
    (c-col-put-str (if (>= cur-xp
			   max-xp)
		       +term-l-green+
		       +term-yellow+)
		   (format nil "~10d" cur-xp)  11 f-col)
    
    (c-put-str "Max Exp" 12 col)
    
    (c-col-put-str +term-l-green+
		   (format nil "~10d" max-xp)  12 f-col)
    

    (c-put-str "Adv Exp" 13 col)
    (c-col-put-str +term-l-green+
		   (format nil "~10d" (aref (player.xp-table pl)
					    (player.level pl)))
		   13 f-col)

    
    
    (c-put-str "Gold" 15 col)

    (c-col-put-str +term-l-green+
		   (format nil "~10d" (player.gold pl))  15 f-col)

    (c-put-str "Burden" 17 col)
    (c-col-put-str +term-l-green+
		   (format nil "~10d lbs" 0)  17 f-col)

    ;; middle again
    (setq col 26)
    (setq f-col (+ col 5))

    (c-put-str "Armour" 10 col)
    (c-col-put-str +term-l-blue+
		   (format nil "~12@a" (format nil "[~d,~@d]" (player.base-ac pl) (player.ac-bonus pl)))
		   10 (1+ f-col))

    (c-put-str "Fight" 11 col)
    (c-col-put-str +term-l-blue+ (format nil "~13@a" "(+0,+0)")
		   11 f-col)

    (c-put-str "Melee" 12 col)
    (c-col-put-str +term-l-blue+ (format nil "~13@a" "(+0,+0)")
		   12 f-col)

    (c-put-str "Shoot" 13 col)
    (c-col-put-str +term-l-blue+ (format nil "~13@a" "(+0,+0)")
		   13 f-col)
    
    (c-put-str "Blows" 14 col)
    (c-col-put-str +term-l-blue+ (format nil "~13@a" "1/turn")
		   14 f-col)
    
    (c-put-str "Shots" 15 col)
    (c-col-put-str +term-l-blue+ (format nil "~13@a" "1/turn")
		   15 f-col)
		   
    (c-put-str "Infra" 17 col)
	    
    (c-col-put-str +term-l-blue+
		   (format nil "~10d ft" (* 10 (player.infravision pl)))
		   17 f-col)

    ;; then right
    (setq col 49)
    (setq f-col (+ col 14))

    (flet ((print-skill (skill div row)
	     (declare (ignore div))
	     (let ((val (slot-value (player.skills pl) skill)))
	       (c-col-put-str +term-l-green+
			      (format nil "~9d" val)
			      row
			      f-col))))
      
      (c-put-str "Saving Throw" 10 col)
      (print-skill 'saving-throw 6 10)

      (c-put-str "Stealth" 11 col)
      (print-skill 'stealth 1 11)
      
      (c-put-str "Fighting" 12 col)
      (print-skill 'fighting 12 12)

      (c-put-str "Shooting" 13 col)
      (print-skill 'shooting 12 13)

      (c-put-str "Disarming" 14 col)
      (print-skill 'disarming 8 14)

      (c-put-str "Magic Device" 15 col)
      (print-skill 'device 6 15)

      (c-put-str "Perception" 16 col)
      (print-skill 'perception 6 16)

      (c-put-str "Searching" 17 col)
      (print-skill 'searching 6 17))
      
  ))


(defun display-player-stats (player)

  ;;  (warn "Displaying character.. ")
  
  (let ((row 3)
	(col 42)
;;	(num-stats 6)
	;; more pressing variables	
	(base (player.base-stats player))
;;	(cur (player.curbase-stats player))
	(mod (player.modbase-stats player))
	(active (player.active-stats player))
	(racial-adding (race.stat-changes (player.race player)))
	(class-adding (class.stat-changes (player.class player)))
	)
    ;; labels
    
    (c-col-put-str +term-white+ "  Self" (1- row) (+ col  5))
    (c-col-put-str +term-white+ " RB"    (1- row) (+ col 12))
    (c-col-put-str +term-white+ " CB"    (1- row) (+ col 16))
    (c-col-put-str +term-white+ " EB"    (1- row) (+ col 20))
    (c-col-put-str +term-white+ "  Best" (1- row) (+ col 24))


    (dotimes (i +stat-length+)
      (let ((its-base (gsdfn base i))
	    ;;(its-cur (gsdfn cur i))
	    (its-mod (gsdfn mod i))
	    (its-active (gsdfn active i))
	      
	    (cur-race-add (gsdfn racial-adding i))
	    (cur-class-add (gsdfn  class-adding i)))

	(c-put-str (get-stat-name-from-num i)
		   (+ i row)
		   col)

	;; base stat
	(c-col-put-str +term-l-green+ (cnv-stat its-base)
		       (+ row i)
		       (+ col 5))


	;; racial bonus
	(c-col-put-str +term-l-green+ (format nil "~3@d" cur-race-add)
		       (+ row i)
		       (+ col 12))

	;; class bonus
	(c-col-put-str +term-l-green+ (format nil "~3@d" cur-class-add)
		       (+ row i)
		       (+ col 16))

	;; equipment
	(c-col-put-str +term-l-green+ " +0"
		       (+ row i)
		       (+ col 20))

	;; max stat
	(c-col-put-str +term-l-green+ (cnv-stat its-mod)
		       (+ row i)
		       (+ col 24))

	;; if active is lower than max
	(when (< its-active its-mod)
	  ;; max stat
	  (c-col-put-str +term-yellow+ (cnv-stat its-active)
			 (+ row i)
			 (+ col 28)))
	      
	      
	)))


  )

