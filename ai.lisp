;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: ai.lisp - the game's "intelligence"
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defclass ai-strategy ()
  ((id :initarg :id :initform nil :accessor strategy.id)))

(defclass primitive-melee-attacker (ai-strategy)
  ((id :initform "primitive-melee-attacker")))

(defclass peaceful-mover (ai-strategy)
  ((id :initform "peaceful-mover")
   (destinations :initform '() :accessor strategy.destinations
		 :documentation "destinations is a list of (x y fun) lists, where
x is the x coord, y is the y coord and fun is an otional trigger function that's called
with (player dungeon monster strategy).  When a corrdinate has been reached the list for
the corrdinate is removed.")))


#||
(defgeneric get-suggested-priority (ai-strategy creature)
  (:documentation "Cheap calculation which returns an integer between 0 and 1000
telling how much it would recommend itself as the strategy to use."))

(defgeneric get-suggested-action (ai-strategy creature)
  (:documentation "Returns an action for the ai-controller to check and
possibly modify."))
||#

(defgeneric execute-strategy (ai-strategy creature dungeon &key action force)
  (:documentation "Asks the strategy to execute an action for the creature.
The strategy will find a fitting action if ACTION is NIL, or adapt
the ACTION argument to the situation.  If FORCE is NIL, the method
is allowed to return NIL ie give up for the controller to choose another
strategy."))


(defun get-move-direction (src-x src-y dest-x dest-y)
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
  (let* ((px dest-x)
	 (py dest-y)
	 (mx src-x)
	 (my src-y)
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
  
  (declare (ignorable player))
  
  (let ((temp-attrs (amon.temp-attrs mon)))

    ;;; first check sleep
    (when-bind (attr (gethash '<sleeping> temp-attrs))
      (when (plusp (attr.value attr))
	;; we do a hack here
	(cond ((<= 50 (random 100))
	       (modify-creature-state! mon '<sleeping> :new-value 0)) ;; awakened
	      
	      (t ;; still sleeping
	       (return-from process-single-monster! t)))))

    ;;; check stun

    ;;; check confusion

    ;;; check fear

    ;;; check breeding

    ;;; check spell

    ;;; check confuse

    ;;(when-bind (strat (amon.strategies mon))
    ;;  (warn "Strategies are ~s" strat))

    (loop for i in (amon.strategies mon)
	  do
	  (let ((attempt (execute-strategy i mon dungeon :force nil)))
	    (when attempt
	      (return-from process-single-monster! t))))

    (loop for i in (amon.strategies mon)
	  do
	  (let ((attempt (execute-strategy i mon dungeon :force t)))
	    (when attempt
	      (return-from process-single-monster! t))))

    ;; then do nothing
    t))
 

(defmethod execute-strategy ((strategy primitive-melee-attacker) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))
  (let ((mx (location-x mon))
	(my (location-y mon))
	(*strategy* strategy)
	(mon-type (amon.kind mon))
	(temp-attrs (amon.temp-attrs mon))
	(px (location-x *player*))
	(py (location-y *player*))
	(player *player*)
	(staggering nil)
	(moves nil)
	(use-move nil)
	(use-turn nil))

    (declare (type u16b mx my))
     
    ;; confused monsters stagger about
    (let ((confusion-attr (gethash '<confusion> temp-attrs)))
      (cond ((and confusion-attr
		  (plusp (attr.value confusion-attr)))
	     (setf staggering t))
	    ;; some monsters even move randomly
	    ((when-bind (random-mover (has-ability? mon '<random-mover>))
	       (let ((how-often (second random-mover)))
		 (when (< (random 100) (* 100 how-often))
		   (setf staggering t)))))))

    (unless staggering
      ;; check use of special-abilities
      (when-bind (spabs (monster.sp-abilities mon-type))
	(block check-spabs
	  (let ((try (random 100))
		(retval nil))
	    (when (< try (first spabs))
	      ;;(warn "Use random spab from ~s" (cdr spabs))
	      (setf retval (trigger-special-ability *variant* mon (rand-elm (cdr spabs)) *player* dungeon))
	      (when retval
		(return-from execute-strategy retval))
	      ))
	  )))
    
    
    (unless staggering
      (setf moves (get-move-direction mx my px py)))

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
		  ((bit-flag-set? (floor.flags (cave-floor dungeon nx ny))
				  +floor-flag-wall+)
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


(defmethod execute-strategy ((strategy peaceful-mover) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))
  (let ((mx (location-x mon))
	(my (location-y mon))
	(*strategy* strategy)
	(temp-attrs (amon.temp-attrs mon))
	(staggering nil)
	)

    ;; confused monsters stagger about
    (let ((confusion-attr (gethash '<confusion> temp-attrs)))
      (cond ((and confusion-attr
		  (plusp (attr.value confusion-attr)))
	     (setf staggering t))
	    ;; some monsters even move randomly
	    ((when-bind (random-mover (has-ability? mon '<random-mover>))
	       (let ((how-often (second random-mover)))
		 (when (< (random 100) (* 100 how-often))
		   (setf staggering t)))))))

    
    (when-bind (dest (first (strategy.destinations strategy)))
      ;;(warn "try to go ~s" dest)
      (let ((dest-x (first dest))
	    (dest-y (second dest))
	    (moves nil))

	(when (and (= dest-x mx) (= dest-y my))
	  (when-bind (evt (third dest))
	    (funcall evt *player* dungeon mon strategy))
	    (setf (strategy.destinations strategy) (cdr (strategy.destinations strategy)))
	  (setf dest (first (strategy.destinations strategy)))
	  (if (consp dest)
	      (setf dest-x (first dest)
		    dest-y (second dest))
	      (return-from execute-strategy nil)))

	(setf moves (get-move-direction mx my dest-x dest-y))

	(loop named move-attempts
	      for i from 0 to 4
	      do
	      (let* ((dir (if staggering (aref *ddd* (random 8)) (aref moves i)))
		     (nx (+ mx (aref *ddx* dir)))
		     (ny (+ my (aref *ddy* dir)))
		     )
		;;(warn "At (~s,~s) checking (~s,~s) ~s ~s"
		;;      mx my nx ny (cave-floor-bold? dungeon nx ny) (cave-empty-bold? dungeon nx ny))
		(when (and (cave-empty-bold? dungeon nx ny)
			   (not (and (= nx (location-x *player*))
				     (= ny (location-y *player*)))))
		  ;;(warn "Going (~s,~s)" nx ny)
		  (swap-monsters! dungeon *player* mx my nx ny)
		  (return-from execute-strategy t))

		))
	))

    t))

#||
(defun process-monsters& (dungeon player needed-energy)
  "Tries to do something nasty to all the monsters."
  
  (with-dungeon-monsters (dungeon mon)
;;    (warn "Trying to process monster ~a with energy ~a and speed ~a -> will need ~a energy"
;;	  (get-creature-name m) (get-creature-energy m)
;;	  (get-creature-speed m) needed-energy)
    
    (when (>= (get-creature-energy mon) needed-energy)
      ;; we deduct before we do things to be sure, improve later
      (decf (get-creature-energy mon) +energy-normal-action+)
      
      (let ((mx (location-x mon))
	    (my (location-y mon)))

	;; skip the 'sensing' of player

	;; only do something if there is clear sight
	(when (player-has-los-bold? dungeon mx my)
	  (process-single-monster! dungeon player mon))
	))
    
    nil))
||#
