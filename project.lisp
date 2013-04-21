;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: project.lisp - code for doing projections
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defun project-path (dun max-range path-array x1 y1 x2 y2 flag)
  "Tries to project a path from (x1,y1) to (x2,y2)."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3)))
	   

  (declare (type fixnum x1 y1 y2 x2 flag max-range))
		     
  (when (and (= x1 x2)
	     (= y1 y2))
    (return-from project-path 0))
  

  (let ((ax 0) ;; absolute
	(ay 0)
	(sx 0) ;; offset
	(sy 0))

    (declare (type fixnum ax ay)
	     (type fixnum sx sy))
    
    (if (< y2 y1)
	(setq ay (the fixnum (- y1 y2))
	      sy -1)
	(setq ay (the fixnum (- y2 y1))
	      sy 1))
    (if (< x2 x1)
	(setq ax (the fixnum (- x1 x2))
	      sx -1)
	(setq ax (the fixnum (- x2 x1))
	      sx 1))

    (flet ((go-direction (dir frac x y)
	     (declare (type fixnum frac x y))
	     (let* ((m (* frac 2))
		    (len 0)
		    (k 0)
		    (half (the fixnum (* ay ax)))
		    (full (the fixnum (* half 2))))
	       
	       (declare (type fixnum m len k half full))
	       
	       (loop
		(vector-push (grid x y) path-array)
		(incf len)
		(when (eq dir :diagonal)
		  (setq k len))
		
		(when (or (>= (+ len (int-/ k 2)) max-range)
			  (and (not (bit-flag-set? flag +project-through+))
			       (and (= x x2)
				    (= y y2)))
			  (and (> len 0)
			       (not (cave-floor-bold? dun x y)))
			  (and (bit-flag-set? flag +project-stop+)
			       (and (> len 0)
				    (cave-monsters dun x y))))
		  (return-from go-direction len))
		
		(unless (= m 0)
		  (assert (not (eq :diagonal dir)))
		  (incf frac m)
		  (when (>= frac half)
		    (ecase dir
			  (:vertical   (incf x sx))
			  (:horisontal (incf y sy)))
		    
		    (decf frac full)
		    (incf k)))
		
		(ecase dir
		  (:vertical   (incf y sy))
		  (:horisontal (incf x sx))
		  (:diagonal (incf y sy)
			     (incf x sx)))
		)))
	   
	   )
      
      (cond ((> ay ax) ;; vertical
	     (go-direction :vertical
			   (the fixnum (* ax ax))
			   x1
			   (the fixnum (+ y1 sy))
			   ))
	    ((> ax ay) ;; horisontal
	     (go-direction :horisontal
			   (the fixnum (* ay ay))
			   (the fixnum (+ x1 sx))
			   y1
			   ))
	    (t	     
	     (go-direction :diagonal
			   0
			   (the fixnum (+ x1 sx))
			   (the fixnum (+ y1 sy))
			   ))
	    )
      )))

;;(trace project-path)

(defun project-hack (dungeon player)
  "a hack"
  (let* ((pvx (location-x player))
	 (pvy (location-y player))
	 (tx (+ 5 pvx))
	 (ty (+ 4 pvy))
	 (path-arr (make-array 40 :fill-pointer 0))
	 (miss-char #\-)
	 (miss-attr +term-l-blue+)
	 )

    (let ((path-len (project-path dungeon 18 path-arr pvx pvy tx ty 0))
;;	  (lst '())
	  )
;;      (warn "len from ~s to ~s with ~s is ~s" (cons pvx pvy) (cons tx ty) path-arr path-len)
      (declare (ignore path-len))

      (loop for g across path-arr
	    do
	    (let ((x (grid-x g))
		  (y (grid-y g)))
;;	      (push (cons x y) lst)
	      (display-moving-object dungeon x y miss-char miss-attr)
	      ))

;;      (warn "did ~s" lst)
      nil)))

(defun display-moving-object (dungeon x y obj-char obj-attr)
  "Prints the OBJ-CHAR with OBJ-ATTR at given coordinates with proper delay."
  (print-relative! dungeon x y obj-char obj-attr)
  (put-cursor-relative! dungeon x y)
  (c-term-fresh!)
  (org.langband.ffi:c-term-xtra& 13 50) ;; 50 msec delay
  (light-spot! dungeon x y)
  (c-term-fresh!))

(defun temp-shoot-an-arrow (dungeon player)
  "Hackish shoot-code."
  (block missile-shooting
    (let ((the-bow (get-missile-weapon player))
	  (the-missile nil))
      (unless (and the-bow (typep the-bow 'active-object/bow))
	(c-print-message! "You have no missile weapon!")
	(return-from missile-shooting nil))

      (with-new-screen ()
	(setq the-missile (grab-a-selection-item dungeon player '(:backpack :floor)
						 :prompt "Select missile:"
						 :where :backpack)))

	
      (cond ((and the-missile (typep the-missile 'active-object/ammo))
	     (shoot-a-missile dungeon player the-bow the-missile))
	    (t
	     (c-print-message! "No missile selected!")))
	)))


(defmethod missile-hit-creature? ((attacker player) (target active-monster) missile-weapon missile)
;;  (declare (ignore missile-weapon missile))
  
  (let ((num (random 100)))
    (when (< num 10)
      ;; instant hit and miss 5%
      (return-from missile-hit-creature? (< num 5)))

  
    (let* ((bonus (+ 0 (gval.tohit-bonus (aobj.game-values missile-weapon))
		     (gval.tohit-bonus (aobj.game-values missile))))
	   (chance (+ (skills.shooting (player.skills attacker)) (* 3 bonus))) ;; hack
	   (dist (distance (location-x attacker) (location-y attacker)
			   (location-x target) (location-y target)))
	   (red-chance (- chance dist))
	   (target-ac (get-creature-ac target))
	   )

      ;; fix invisible later      
      (warn "chance to hit is ~s on ac ~s" red-chance target-ac)
      
      (when (and (plusp red-chance)
		 (>= (random red-chance) (int-/ (* 3 target-ac) 4)))
	(return-from missile-hit-creature? t))
      
      nil)))


(defmethod missile-inflict-damage! ((attacker player) (target active-monster) missile-weapon missile)
  (declare (ignore missile-weapon missile))
  (deduct-hp! target (roll-dice 2 4)))

(defmethod shoot-a-missile ((dungeon dungeon) (player player)
			   (missile-weapon active-object/bow)
			   (arrow active-object/ammo))
  
;;  (declare (ignore missile-weapon))
  (block missile-shooting
    (when-bind (dir (%read-direction))
      (assert (and (numberp dir) (< dir 10)))
;;      (check-type arrow active-object)
      
;;      (warn "dir is ~s with ~s + ~s" dir missile-weapon arrow)
      (let* ((pvx (location-x player))
	     (pvy (location-y player))
	     (tx (+ pvx (* 99 (aref +ddx+ dir))))
	     (ty (+ pvy (* 99 (aref +ddy+ dir))))
	     (max-range (+ 10 (* 5 (object.multiplier (aobj.kind missile-weapon)))))
	     (path-arr (make-array (1+ max-range) :fill-pointer 0))
	     (path-len (project-path dungeon max-range path-arr pvx pvy tx ty 0))
	     (miss-attr (object.x-attr arrow))
	     (miss-char (object.x-char arrow))
	     (cur-x pvx)
	     (cur-y pvy)
	     )

	(declare (ignore path-len))

	
	(loop named follow-path
	      for g across path-arr
	      do
	      (let ((x (grid-x g))
		    (y (grid-y g)))
		(setq cur-x x
		      cur-y y)
		(unless (cave-floor-bold? dungeon x y)
		  (return-from follow-path nil))
	      
		(display-moving-object dungeon x y miss-char miss-attr)
	      
		(when-bind (monsters (cave-monsters dungeon x y))
		  (let* ((fmon (if (consp monsters) (car monsters) monsters))
			 (mon-name (get-creature-name fmon)))
		    (when (missile-hit-creature? player fmon missile-weapon arrow)
		      
		      (c-print-message! (format nil "The ~a was hit." mon-name))
		      (missile-inflict-damage! player fmon missile-weapon arrow)
		      (when (< (current-hp fmon) 0)
			(c-print-message! (format nil "The ~a died." mon-name))
			(let ((target-xp (get-xp-value fmon)))
			  (alter-xp! player (if target-xp target-xp 0)))
			(kill-target! dungeon player fmon x y)
			;; repaint spot
			(light-spot! dungeon x y))

		      (return-from follow-path nil))))
		))

	;; if it crashes in a wall, your arrow is gone.
	(when (cave-floor-bold? dungeon cur-x cur-y)
	  (item-table-add! (get-item-table dungeon player :floor :x cur-x :y cur-y)
			   arrow))

	))))

	
	    

(defun %read-direction ()
  (block read-loop
    (loop
     (c-prt! "Direction: " 0 0)
     (let ((val (read-one-character)))
       (cond ((or (eql val #\.)
		  (eql val #\0)
		  (eql val #\t))
	      (c-prt! "" 0 0)
	      (return-from read-loop 5))
	     ((digit-char-p val)
	      (c-prt! "" 0 0)
	      (return-from read-loop (digit-char-p val)))
	     ((eql val +escape+)
	      (c-prt! "" 0 0)
	      (return-from read-loop nil))
	     (t
	      (c-prt! "Unknown direction!" 0 0)))))))
