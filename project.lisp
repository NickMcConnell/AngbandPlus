;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: project.lisp - code for doing projections
Copyright (c) 2001-2002 - Stig Erik Sandø

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

(defun projectable? (dungeon x1 y1 x2 y2)
  "Is it projectable from (x1,y1) to (x2,y2)"
  
  (let* ((max-range 50)
	 (path-arr (make-array (1+ max-range) :fill-pointer 0))
	 (path-len (project-path dungeon max-range path-arr x1 y1 x2 y2 0)))

    (when (plusp path-len)
      (let* ((last-g (aref path-arr (1- path-len)))
	     (last-x (grid-x last-g))
	     (last-y (grid-y last-g)))
	(when (and (cave-floor-bold? dungeon last-x last-y)
		   (= x2 last-x)
		   (= y2 last-y))
	  t)))))


(defun display-moving-object (dungeon x y obj-char obj-attr)
  "Prints the OBJ-CHAR with OBJ-ATTR at given coordinates with proper delay."
  (print-relative! dungeon x y obj-char obj-attr)
  (put-cursor-relative! dungeon x y)
  (c-term-fresh!)
  (org.langband.ffi:c-term-xtra& 13 50) ;; 50 msec delay
  (light-spot! dungeon x y)
  (c-term-fresh!))

(defun interactive-fire-a-missile (dungeon player)
  "Hackish shoot-code."
  (block missile-shooting
    (let ((the-bow (get-missile-weapon player))
	  (the-missile nil))
      (unless (and the-bow (typep the-bow 'active-object/bow))
	(print-message! "You have no missile weapon!")
	(return-from missile-shooting nil))

      (with-new-screen ()
	(setq the-missile (grab-a-selection-item dungeon player '(:backpack :floor)
						 :prompt "Select missile:"
						 :where :backpack)))

	
      (cond ((and the-missile (typep the-missile 'active-object/ammo))
	     (shoot-a-missile dungeon player the-bow the-missile))
	    (t
	     (print-message! "No missile selected!")))
	)))


(defmethod missile-hit-creature? ((attacker player) (target active-monster) missile-weapon missile)
;;  (declare (ignore missile-weapon missile))
  
  (let ((num (random 100)))
    (when (< num 10)
      ;; instant hit and miss 5%
      (return-from missile-hit-creature? (< num 5)))

  
    (let* ((bonus (+ 0 (gval.tohit-modifier (aobj.game-values missile-weapon))
		     (gval.tohit-modifier (aobj.game-values missile))))
	   (chance (+ (skills.shooting (player.skills attacker)) (* 3 bonus))) ;; hack
	   (dist (distance (location-x attacker) (location-y attacker)
			   (location-x target) (location-y target)))
	   (red-chance (- chance dist))
	   (target-ac (get-creature-ac target))
	   )

      ;; fix invisible later
      #+never
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
	     (ddx *ddx*)
	     (ddy *ddy*)
	     (tx (+ pvx (* 99 (aref ddx dir))))
	     (ty (+ pvy (* 99 (aref ddy dir))))
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
		      
		      (print-message! (format nil "The ~a was hit." mon-name))
		      (missile-inflict-damage! player fmon missile-weapon arrow)
		      (when (< (current-hp fmon) 0)
			(print-message! (format nil "The ~a died." mon-name))
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

	
	    

;;(trace project-path)
#+never
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

(defmethod do-projection (source target-x target-y flag &key (effect nil) (damage 0) (radius 0) (range +max-range+))

  
  (let ((source-x (location-x source))
	(source-y (location-y source))
	(player *player*)
	(dungeon *dungeon*)
	(notice nil) ;; can the player see anything?
	(drawn-beam nil) ;; have we drawn anything?
	(drawn-blast nil) ;; have we drawn anything?
	(blind-player nil) ;; fix
	(draw-delay 50)
	(start-x 0) ;; start-coordinate
	(start-y 0) ;; start-coordinate
	(dest-x 0)
	(dest-y 0)
	(cur-x target-x)
	(cur-y target-y)
	(affected-grids (make-array 256 :fill-pointer 0))
	)

    ;; hack, jump directly to target
    (cond ((bit-flag-set? flag +project-jump+)
	   (setf start-x target-x
		 start-x target-y)
	   ;; clear flag
	   (bit-flag-remove! flag +project-jump+))
	  ((or (is-player? source) (is-monster? source))
	   (setf start-x source-x
		 start-y source-y))
	  (t
	   (warn "Fell through source-check in PROJECT with source ~s" source)
	   ;; assume target is source
	   (setf start-x target-x
		 start-y target-y)
	   ))

    ;; default
    (setf dest-x target-x
	  dest-y target-y)

    ;; hack, a verify
    (when (and (bit-flag-set? flag +project-through+)
	       (= start-x dest-x)
	       (= start-y dest-y))
      (bit-flag-remove! flag +project-through+))

    ;; skip radius-thing

    ;; initial pos
    (setf cur-x start-x
	  cur-y start-y)
    

    ;; if we're a beam, include this spot
    (when (bit-flag-set? flag +project-beam+)
      (vector-push (grid cur-x cur-y) affected-grids))
	
    (let* ((max-range range)
	   (path-arr (make-array max-range :fill-pointer 0)) ;; set to max-range
	   (path-len (project-path dungeon max-range path-arr start-x start-y dest-x dest-y flag))
	   
	   (max-explosion 25)
	   (explosion-area (make-array max-explosion :initial-element nil))
	   )

;;      (warn "Projection from (~s,~s) to (~s,~s) [target: ~s,~s]" start-x start-y dest-x dest-y target-x target-y)

      (loop named path-tracer
	    for i from 0 to (1- path-len)
	    do

	    (let* (;;(old-x cur-x)
		   ;;(old-y cur-y)
		   (new-g (aref path-arr i))
		   (new-x (grid-x new-g))
		   (new-y (grid-y new-g)))

	      ;; if we're going to make a ball, stop one step before a wall!
	      (when (and (not (cave-floor-bold? dungeon new-x new-y))
			 (plusp radius))
		(return-from path-tracer t))
	      
	      (setf cur-x new-x
		    cur-y new-y)

	      ;; if we're a beam, include this spot
	      (when (bit-flag-set? flag +project-beam+)
		(vector-push (grid cur-x cur-y) affected-grids))

	      (when (and (not blind-player) (not (bit-flag-set? flag +project-hide+)))

		(cond ((and (panel-contains? player cur-x cur-y)
			    (player-has-los-bold? dungeon cur-x cur-y))
		       (display-moving-object dungeon cur-x cur-y #\* +term-red+)
		       (setf drawn-beam t))
		      (drawn-beam
		       ;; add delay
		       )))
		
			    
		
;;	      (format t "~&At (~3s,~3s)~%" cur-x cur-y)
	      ))

      ;; hack
      (setf dest-x cur-x
	    dest-y cur-y)
      
#||
;;      (loop for g across path-arr
;;	    do
;;	    (format t "~&>At (~3s,~3s)~%" (grid-x g) (grid-y g)))

      (loop for g across affected-grids
	    do
	    (format t "~&-Effect (~3s,~3s)~%" (grid-x g) (grid-y g)))
||#

      #||
      ;; hack.. 
      (when (bit-flag-set? flag +project-beam+)
	(setf (fill-pointer affected-grids) (1- (fill-pointer affected-grids))))
      ||#

      ;; get blast area
      (loop for dist from 0 to radius
	    do
	    (loop for y from (- dest-y dist) to (+ dest-y dist)
		  do
		  (loop for x from (- dest-x dist) to (+ dest-x dist)
			do
			(when (and (in-bounds? dungeon x y) ;; skip illegal
				   (= (distance dest-x dest-y x y) dist) ;; circular
				   (projectable? dungeon dest-x dest-y x y)
				   )
			  (push (grid x y) (aref explosion-area dist))
			  ))))
      #||
      (format t "~&Explosion [~s]: ~s~%" radius explosion-area)
				   
      
      
      (warn "Went from (~s,~s,~s) to (~s,~s,~s) with [~s ~s]"
	    start-x start-y #\.
	    dest-x dest-y #\.
	    path-len path-arr)
      ||#
      ;; do a check here if we have explosion


      ;; should we show the explosion?

      (when (and (not blind-player) (not (bit-flag-set? flag +project-hide+)))

	(loop for i from 0 to radius
	      do
	      (let ((cur-val (aref explosion-area i)))
		(when (consp cur-val)
		  (dolist (j cur-val)
		    (let ((loc-x (grid-x j))
			  (loc-y (grid-y j)))
		      (when (and (panel-contains? player loc-x loc-y)
				 (player-has-los-bold? dungeon loc-x loc-y))
			(setf drawn-blast t)
			(print-relative! dungeon loc-x loc-y #\* +term-red+))
		      )))
		(put-cursor-relative! dungeon dest-x dest-y)
		(c-term-fresh!)
		(when (or drawn-blast drawn-beam)
		  (org.langband.ffi:c-term-xtra& 13 draw-delay) 
		  )
;;		(light-spot! dungeon x y)
;;		(c-term-fresh!)
		))

	(when drawn-blast
	  ;; clean up
	  (loop for i from 0 to radius
		do
		(let ((cur-val (aref explosion-area i)))
		  ;;		(warn "Cur-val for ~s is ~s" i cur-val)
		  (when (consp cur-val)
		    (dolist (j cur-val)
		      (let ((loc-x (grid-x j))
			    (loc-y (grid-y j)))
			(light-spot! dungeon loc-x loc-y)
			)))
		  ))
	
	  (put-cursor-relative! dungeon dest-x dest-y)
	  (c-term-fresh!)
	  ))

      (setf notice (apply-projection-effect! *variant* source path-arr :explosion-area explosion-area
					     :flag flag :damage damage :effect effect))

      )

    notice))

(defmethod apply-projection-effect-to-target! ((variant variant) source target &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (declare (ignore x y damage effect distance source))
  (warn "Applying effect to ~s" target)
  
  )

(defmethod apply-projection-effect-to-target! ((variant variant) source (target active-object)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (declare (ignore x y damage effect distance source))
  (warn "OBJ: Applying effect to ~s" target)
  )

(defmethod apply-projection-effect-to-target! ((variant variant) source (target floor-type)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (declare (ignore x y damage effect distance source))
;;  (warn "FLOOR: Applying effect to ~s" (floor.name target))
  )



(defmethod apply-projection-effect! ((variant variant) source path-array
				    &key (explosion-area nil) (flag 0) 
				    (distance 0) (damage 0) (effect nil))
  (declare (ignore distance))
  
  (let ((notice nil)
	(dungeon *dungeon*))

;;    (warn "We're going to dish out effect ~s" effect)
    
    (flet ((apply-to! (target distance grid)
	     (when (apply-projection-effect-to-target! variant source target
						       :x (grid-x grid) :y (grid-y grid)
						       :damage damage :effect effect
						       :distance distance)
	       (setf notice t))))
    
    ;; check floors
      (when (bit-flag-set? flag +project-grid+)
	(flet ((apply-to-floor! (grid distance)
		 (apply-to! (get-floor-type (cave-floor dungeon (grid-x grid) (grid-y grid))) distance grid)))
	  
	  ;; first do beam
	  (loop for g across path-array do
		(apply-to-floor! g 0))
	  ;; then do explosion
	  (loop for i from 0
		for x across explosion-area
		do
		(dolist (c x) ;; coordinates
		  (apply-to-floor! c i)))
	  ))

      ;; do updates
      ;; check objects
      (when (bit-flag-set? flag +project-item+)
	(flet ((apply-to-objs! (grid distance)
		 (let ((obj-table (cave-objects dungeon (grid-x grid) (grid-y grid))))
		   (when obj-table
		     (dolist (i (items.objs obj-table))
		       (apply-to! i distance grid))))))
	  
	  ;; first do beam
	  (loop for g across path-array do
		(apply-to-objs! g 0))
	  ;; then do explosion
	  (loop for i from 0
		for x across explosion-area
		do
		(dolist (c x) ;; coordinates
		  (apply-to-objs! c i)))
	  ))
      
      ;; check monsters
      (when (bit-flag-set? flag +project-kill+)
	(flet ((apply-to-monsters! (grid distance)
		 (let ((loc-x (grid-x grid))
		       (loc-y (grid-y grid)))
		   (when (cave-floor-bold? dungeon loc-x loc-y) ;; monsters in wall are safe
		     (dolist (m (cave-monsters dungeon loc-x loc-y))
		       (unless (eq source m)
			 (apply-to! m distance grid)))))))
		   
	;; first do beam
	(loop for g across path-array do
	      (apply-to-monsters! g 0))

	;; then do explosion
	(loop for i from 0
	      for x across explosion-area
		do
		(dolist (c x) ;; coordinates
		  (apply-to-monsters! c i)))
	))
	
      ;; check player
            
   
    
    notice)))
