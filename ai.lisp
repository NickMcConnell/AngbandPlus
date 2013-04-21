;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: ai.lisp - the game's "intelligence"
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defun %get-moves (mon player)
  "Returns an array of five elements with directions in preferred
order to get from monster to player."
  ;; the graph of moves.
  ;; 88AAECC
  ;; 8     C
  ;; 9     B
  ;; 1  M  5
  ;; 1     5
  ;; 0     4
  ;; 0022644

  ;; ignore invisibility, etc
  (let* ((px (location-x player))
	 (py (location-y player))
	 (mx (location-x mon))
	 (my (location-y mon))
	 (dx (- mx px))
	 (dy (- my py))
	 (lx (abs dx))
	 (ly (abs dy))
	 (move-value 0)
	 )

    (when (< dy 0) (incf move-value 8))  ;; above is +8
    (when (> dx 0) (incf move-value 4))  ;; right is +4

    (cond ((> ly (* lx 2))
	   (incf move-value 2))  ;; vertical centre
	  ((> lx (* ly 2))
	   (incf move-value 1))) ;; horizontal centre

    (cond ((= move-value 0)
	   (if (> ly lx) #(9 8 6 7 3) #(9 6 8 3 7)))
	  ((or (= move-value 1) (= move-value 9))  
	   (if (< dy 0)  #(6 3 9 2 8) #(6 9 3 8 2)))
	  ((or (= move-value 2) (= move-value 6))  
	   (if (< dx 0)  #(8 9 7 6 4) #(8 7 9 4 6)))
	  ((= move-value 4)
	   (if (> ly lx) #(7 8 4 9 1) #(7 4 8 1 9)))
	  ((or (= move-value 5) (= move-value 13))  
	   (if (< dy 0)  #(4 1 7 2 8) #(4 7 1 8 2)))
	  ((= move-value 8)
	   (if (> ly lx) #(3 2 6 1 9) #(3 6 2 9 1)))
	  ((or (= move-value 10) (= move-value 14))  
	   (if (< dx 0)  #(2 3 1 6 4) #(2 1 3 4 6)))
	  (t
	   (if (> ly lx) #(1 2 4 3 7) #(1 4 2 7 3))))
    ))

(defun process-single-monster! (dungeon player mon)
  "Tries to process a single monster and make it do nasty stuff."

  (let ((mx (location-x mon))
	(my (location-y mon))
	(px (location-x player))
	(py (location-y player))
	(mstatus (amon.status mon))
	(staggering nil)
	(moves nil)
	(use-move nil)
	(use-turn nil)
	)
	
    
    (declare (type u-16b mx my))

    ;;; first check sleep
    (when (plusp (status.sleeping mstatus))
      ;; we do a hack here
      (cond ((<= 50 (random 100))
	     (setf (status.sleeping mstatus) 0) ;; awakened
	     )
	    (t ;; still sleeping
	     (return-from process-single-monster! t))))

    ;;; check stun

    ;;; check confusion

    ;;; check fear

    ;;; check breeding

    ;;; check spell

    ;;; check confuse

    ;; confused monsters stagger about
    (cond ((plusp (status.confused mstatus))
	   (setf staggering t))
	  ;; some monsters even move randomly
	  ((when-bind (random-mover (has-ability? mon '<random-mover>))
	     (let ((how-often (second random-mover)))
	       (when (< (random 100) (* 100 how-often))
		 (setf staggering t))))))

    (unless staggering
      (setf moves (%get-moves mon player)))

;;    (warn "~a ~a at (~s,~s) -> (~s,~s) ~s"
;;	  (if staggering "staggering" "") (monster.name mon) mx my px py moves)

    (loop named move-attempts
	  for i from 0 to 4
	  do
	  (let* ((dir (if staggering (aref *ddd* (random 8)) (aref moves i)))
		 (nx (+ mx (aref *ddx* dir)))
		 (ny (+ my (aref *ddy* dir)))
		 )

	    (cond ((cave-floor-bold? dungeon nx ny)
		   (setf use-move t))
		  ((>= (cave-floor dungeon nx ny) +floor-wall-extra+)
		   ;; nothing
		   )
		  ;; skip move through walls
		  ;; skip ruin walls
		  ;; skip doors

		  )
	    ;; skip glyph

	    ;; some monsters never attack, even when they can
	    (when (and use-move (has-ability? mon '<never-attack>))
	      ;; skip learn
	      (setf use-move nil))

	    ;; we have the player next to us.. kill him
	    (when (and use-move (= nx px) (= ny py))
	      (cmb-monster-attack! dungeon player mon nx ny)
	      (setf use-turn t
		    use-move nil))

	    ;; some monsters never move
	    (when (and use-move (has-ability? mon '<never-move>))
	      ;; skip learn
	      (setf use-move nil))
	    
	    ;; if some monster is in the way, stop.. fix later
	    (when (and use-move (cave-monsters dungeon nx ny))
	      (setf use-move t))

	    ;; skip more treatment of monsters

	    ;; ok, now move
	    (when use-move
	      (setf use-turn t)
	      (swap-monsters! dungeon player mx my nx ny)
	      ;; skip all the special handling of this case with pickup, ..
	      
	      )

	    (when use-turn
	      (return-from move-attempts t))))
    
    ;; skip fallback spellcasting

    ;; skip update

    ;; skip monster learning

    ;; skip 'remove fear'
    t))
	    

(defun process-monsters& (dungeon player needed-energy)
  "Tries to do something nasty to all the monsters."
  
  (with-dungeon-monsters (dungeon mon)
;;    (warn "Trying to process monster ~a with energy ~a and speed ~a -> will need ~a energy"
;;	  (get-creature-name m) (get-creature-energy m)
;;	  (get-creature-speed m) needed-energy)
    
    (when (>= (get-creature-energy mon) needed-energy)
      ;; we deduct before we do things to be sure, improve  later
      (decf (get-creature-energy mon) +energy-normal-action+)
      
      (let ((mx (location-x mon))
	    (my (location-y mon)))

	;; skip the 'sensing' of player

	;; only do something if there is clear sight
	(when (player-has-los-bold? dungeon mx my)
	  (process-single-monster! dungeon player mon))
	))
    
    nil))

