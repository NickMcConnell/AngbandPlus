;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: loop.lisp - the game loop(s)
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with the game loops.

|#

(in-package :org.langband.engine)


(defmethod redraw-stuff ((variant variant) (dungeon dungeon) (player player))
  "Redraws stuff according to *REDRAW*."
  
  (when (= 0 *redraw*) (return-from redraw-stuff nil))

  (let ((retval nil)
	(bot-set nil)
	(pr-set nil))

    (when (bit-flag-set? *redraw* +print-map+)
      (bit-flag-remove! *redraw* +print-map+)
      (print-map dungeon player)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-basic+)
      (bit-flag-remove! *redraw* +print-basic+)
      (bit-flag-remove! *redraw* +print-misc+)
      (bit-flag-remove! *redraw* +print-title+)
      (bit-flag-remove! *redraw* +print-stats+)
      (bit-flag-remove! *redraw* +print-level+)
      (bit-flag-remove! *redraw* +print-xp+)
      (bit-flag-remove! *redraw* +print-gold+)
      (bit-flag-remove! *redraw* +print-armour+)
      (bit-flag-remove! *redraw* +print-mana+)
      (bit-flag-remove! *redraw* +print-hp+)
      (bit-flag-remove! *redraw* +print-depth+)
      (bit-flag-remove! *redraw* +print-health+)
      (print-basic-frame variant dungeon player)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-misc+)
      (bit-flag-remove! *redraw* +print-misc+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))

      (print-field (get-race-name player) (slot-value pr-set 'race) +charinfo-frame+)
      (print-field (get-class-name player) (slot-value pr-set 'class) +charinfo-frame+)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-title+)
      (bit-flag-remove! *redraw* +print-title+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-title player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-level+)
      (bit-flag-remove! *redraw* +print-level+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-level player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-xp+)
      (bit-flag-remove! *redraw* +print-xp+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-xp player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-stats+)
      (bit-flag-remove! *redraw* +print-stats+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (dotimes (i (variant.stat-length variant))
	(print-stat player pr-set i)) ;; probably not optimal handling
      (setf retval t))
    
    (when (bit-flag-set? *redraw* +print-armour+)
      (bit-flag-remove! *redraw* +print-armour+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-armour-class player pr-set)
      (setf retval t))
    
    (when (bit-flag-set? *redraw* +print-hp+)
      (bit-flag-remove! *redraw* +print-hp+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-hit-points player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-mana+)
      (bit-flag-remove! *redraw* +print-mana+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-mana-points variant player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-gold+)
      (bit-flag-remove! *redraw* +print-gold+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-gold player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-depth+)
      (bit-flag-remove! *redraw* +print-depth+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      ;;(warn "Depth ~s ~s ~s" (dungeon.depth dungeon) bot-set (get-setting variant :bottom-row-printing))
      (print-depth (dungeon.depth dungeon) bot-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-health+)
      (bit-flag-remove! *redraw* +print-health+)
      ;; fix
      (setf retval t))

    ;; extra-printing moved to variant
    ;; cut moved to variant
    ;; stun moved to variant
    ;; moved hunger to variant
    ;; moved blind to variant
    ;; moved confused to variant
    ;; moved afraid to variant
    ;; poisoned moved to variant

    (when (bit-flag-set? *redraw* +print-state+)
      (bit-flag-remove! *redraw* +print-state+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-state variant player bot-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-speed+)
      (bit-flag-remove! *redraw* +print-speed+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-speed variant player bot-set)
      (setf retval t))

;;    )
    ;; moved study to variant
    
    (when (/= 0 *redraw*)
      (warn "Unhandled redraw flags ~s" *redraw*))
    
    retval))


(defun update-stuff (variant dungeon player)
  "Updates stuff according to *UPDATE*."
  
  (when (= 0 *update*) (return-from update-stuff nil))
  
  (let ((retval nil))
    
    (when (bit-flag-set? *update* +pl-upd-bonuses+)
      (bit-flag-remove! *update* +pl-upd-bonuses+)
      (calculate-creature-bonuses! variant player)
      ;; move later
      (update-inventory-row player);
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-torch+)
      (bit-flag-remove! *update* +pl-upd-torch+)
      (calculate-creature-light-radius! variant player)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-hp+)
      (bit-flag-remove! *update* +pl-upd-hp+)
      (calculate-creature-hit-points! variant player)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-mana+)
      (bit-flag-remove! *update* +pl-upd-mana+)
      (calculate-creature-mana! variant player)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-spells+)
      (bit-flag-remove! *update* +pl-upd-spells+)
;;      (calculate-creature-hit-points! variant player)
      (setf retval t))
    
    
    
    (when (bit-flag-set? *update* +pl-upd-forget-view+)
      (bit-flag-remove! *update* +pl-upd-forget-view+)
      (forget-view! dungeon player)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-update-view+)
      (bit-flag-remove! *update* +pl-upd-update-view+)
      (update-view! dungeon player)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-forget-flow+)
      (bit-flag-remove! *update* +pl-upd-forget-flow+)
;;      (forget-view! dungeon player)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-update-flow+)
      (bit-flag-remove! *update* +pl-upd-update-flow+)
;;      (update-view! dungeon player)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-distance+)
      (bit-flag-remove! *update* +pl-upd-distance+)
      (bit-flag-remove! *update* +pl-upd-monsters+)
      (update-monsters! variant dungeon t)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-monsters+)
      (bit-flag-remove! *update* +pl-upd-monsters+)
      (update-monsters! variant dungeon nil)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-panel+)
      (bit-flag-remove! *update* +pl-upd-panel+)
      (verify-panel dungeon player)
      (setf retval t))


    (when (/= 0 *update*)
      (warn "Unhandled upd-flags ~s" *update*))

    
    retval))

(defun handle-stuff (variant dungeon player)
  (let ((retval nil))
    (unless (= 0 *update*)
      (setf retval (update-stuff variant dungeon player)))
    (unless (= 0 *redraw*)
      (setf retval (or (redraw-stuff variant dungeon player) retval)))
    ;; add window-stuff
    retval))


(defun %mouse-clicked (button x y)
  "Figures out what window it happened in and calls handle-mouse-click"
  (when (eq (get-system-type) 'sdl)
    ;; let us figure out what window:
    (let ((win nil))
	
      (loop for w across *windows*
	    for x-off = (window.x-offset w)
	    for y-off = (window.y-offset w)
	    do
	    (when (and (window.visible? w)
		       (>= x x-off) (< x (+ x-off (window.pixel-width w)))
		       (>= y y-off) (< y (+ y-off (window.pixel-height w))))
	      (setf win w)))

      (unless win
	(warn "Totally unable to figure out which window the button was clicked!"))

      (when win
	(let ((loc-x (- x (window.x-offset win)))
	      (loc-y (- y (window.y-offset win))))
	  (handle-mouse-click *variant* win button loc-x loc-y)))

      t)))


(defun get-and-process-command! (dungeon player table)
  "remove me later"

  (let ((loc-table (gethash table *current-key-table*))
	(event nil))

    (loop
     (setf event (fetch-event *input-event* nil))
     (when event
       (cond ((eq (input-event.type event) :mouse)
	      (let ((m-ev (input-event.mouseclick event)))
		;; don't return now, maybe later
		(%mouse-clicked (mouse-event.button m-ev)
				(mouse-event.x m-ev)
				(mouse-event.y m-ev))))
	     
	     ((eq (input-event.type event) :key)
	      (let* ((kev (input-event.keypress event))
		     (ch (kbd-event.key kev))
		     (check nil)
		     (fun nil))

		;; add ALT-key later as well.. 
		(when (kbd-event.shift kev)
		  (setf check (list 'shift ch)) ;; can avoid consing later if it is a problem.
		  ;;(warn "Looking for ~s" check)
		  (setf fun (check-keypress loc-table check)))

		(unless (functionp fun)
		  (setf check (kbd-event.key kev))
		  (setf fun (check-keypress loc-table check)))

		(cond ((functionp fun)
		       (return-from get-and-process-command! (funcall fun dungeon player)))
		      (t
		       (warn "fell through key with ~s" kev)))
		))
	     (t
	      (warn "Unknown event ~s" (input-event.type event))))
       ))
    ))


(defun process-player! (variant dungeon player)
  "processes the player in a given turn"

  (let ((temp-attrs (player.temp-attrs player)))
  
    (loop named waste-energy
	  for run-status = (get-information "running" :default nil)
	  for run-dir = (get-information "run-direction" :default 0)

	  do
	
	  (when (/= *update* 0) (update-stuff variant dungeon player))
	  (when (/= *redraw* 0) (redraw-stuff variant dungeon player))
	
	  ;;(put-cursor-relative! dungeon (location-x player) (location-y player))

	  ;; assume no energy is used
	  (setf (player.energy-use player) 0)

	
	
	  (cond ((or (get-attribute-value '<paralysed> temp-attrs)
		     (>= (get-attribute-value '<cut> temp-attrs) 100)) ;; move to variant
		 (setf (player.energy-use player)  +energy-normal-action+))

		;; skip resting
		((and run-status (>= run-dir 0))
		 ;;(warn "from loop")
		 (unless (let-player-run! dungeon player run-dir)
		   ;;(warn "turning off running")
		   (setf (get-information "run-direction") -1
			 (get-information "running") nil)))
		       

		;; skip repeat
		(t
		 ;; do normal command
		 (get-and-process-command! dungeon player :global)))
	
	
	  (when (plusp (player.energy-use player))
	    (decf (get-creature-energy player) (player.energy-use player)))

	  while (and (= 0 (player.energy-use player))
		     (not (player.leaving? player)))
	  )
    t))

  

(defun regenerate-mana! (crt percent)
  ;; clean up later
    
  (let* ((regen-base 1442)
	 (old-mana (current-mana crt))
	 (new-mana (+ (* (maximum-mana crt) percent) regen-base))
	 (max-short 32767)
	 (increase (int-/ new-mana (expt 2 16)))
	 (new-frac (+ (player.fraction-mana crt)
		      (logand new-mana #xffff)))
	 )

    (incf (current-mana crt) increase)

    (when (and (minusp (current-mana crt))
	       (plusp old-mana))
      (setf (current-mana crt) max-short))

    (if (> new-frac #x10000)
	(progn
	  (setf (player.fraction-mana crt) (- new-frac #x10000))
	  (incf (current-mana crt)))
	(setf (player.fraction-mana crt) new-frac))

    (when (>= (current-mana crt)
	      (maximum-mana crt))
      (setf (current-mana crt) (maximum-mana crt)
	    (player.fraction-mana crt) 0))

    (when (/= old-mana (current-mana crt))
;;      (warn "Regenerated..")
      (bit-flag-add! *redraw* +print-mana+))
      
    (current-mana crt)))


(defun regenerate-hp! (crt percent)
  "Tries to regenerate the creature, and includes percent-chance."
  
  (let* ((regen-base 1442)
	 (old-hp (current-hp crt))
	 (new-hp (+ (* (maximum-hp crt) percent) regen-base))
	 (max-short 32767)
	 (increase (int-/ new-hp (expt 2 16)))
	 (new-frac (+ (player.fraction-hp crt)
		      (logand new-hp #xffff)))
	 )

    (incf (current-hp crt) increase)

    (when (and (minusp (current-hp crt))
	       (plusp old-hp))
      (setf (current-hp crt) max-short))

    (if (> new-frac #x10000)
	(progn
	  (setf (player.fraction-hp crt) (- new-frac #x10000))
	  (incf (current-hp crt)))
	(setf (player.fraction-hp crt) new-frac))

    (when (>= (current-hp crt)
	      (maximum-hp crt))
      (setf (current-hp crt) (maximum-hp crt)
	    (player.fraction-hp crt) 0))

    (when (/= old-hp (current-hp crt))
;;      (warn "Regenerated..")
      (bit-flag-add! *redraw* +print-hp+))
      
    (current-hp crt)))

;;(trace regenerate-hp!)

(defmethod process-world& ((variant variant) (dungeon dungeon) (player player))
  "tries to process important world-stuff every 10 turns."

  (let ((the-turn (variant.turn variant)))

    (unless (= 0 (mod the-turn 10)) ;; every 10 turns only
      (return-from process-world& nil))

    ;; see variant for real code!

    t))
 
(defun energy-for-speed (crt)
  (aref *energy-table* (get-creature-speed crt)))
			 
#||
(defun energise-creatures! (variant dungeon player)

  (incf (get-creature-energy player) (energy-for-speed player))

  ;; boost all monsters
  (with-dungeon-monsters (dungeon mon)
    (declare (ignore dungeon))
    (incf (get-creature-energy mon) (energy-for-speed mon)))
  
  ;; can our dear player do anything?

  (loop named player-fun
	while (and (>= (get-creature-energy player) +energy-normal-action+) ;; better solution?
		   (not (player.leaving? player)))
	do
	(progn
	  (process-monsters& dungeon player (1+ (get-creature-energy player)))
	  
	  (unless (player.leaving? player)
	    (process-player! variant dungeon player))))
  )
||#
  
(defun run-level! (level player)
  "a loop which runs a dungeon level"

  (let* ((dungeon (level.dungeon level))
	 (var-obj *variant*)
	 (variant *variant*)
	 (*dungeon* dungeon)
	 (dungeon-height (dungeon.height dungeon))
	 (dungeon-width  (dungeon.width dungeon))
	 )

    ;; we're not leaving
    (setf (player.leaving? player) nil
	  (player.target player) nil)
      
    ;; setting these to illegal values
    (setf (player.view-x player) dungeon-width
	  (player.view-y player) dungeon-height)
  
    ;; no stairs from town
;;    (setf (dungeon.up-stairs-p dungeon) nil
;;	  (dungeon.down-stairs-p dungeon) nil)
    

    ;; create stairs.. (postponed)

    ;; postpone verify of panel
    (verify-panel dungeon player)
    
    (print-message! nil) ;; flushes

    ;;; == this section needs serious rework.. see angband
    ;; postpone flush


    (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+) 
    (bit-flag-add! *update* +pl-upd-hp+ +pl-upd-spells+ +pl-upd-mana+) 

    (update-stuff var-obj dungeon player)
    
    (bit-flag-add! *redraw* +print-map+ +print-basic+ +print-extra+)
    (bit-flag-add! *update* +pl-upd-forget-view+ +pl-upd-update-view+ +pl-upd-distance+)
  
    
    ;; postpone stuff..
    (update-stuff var-obj dungeon player)

    (redraw-stuff var-obj dungeon player)


    ;; can be moved inside loop below for _very_ frequent checking
    #+langband-extra-checks
    (progn
      (assert (eq player *player*))
      (assert (eq dungeon *dungeon*))
      (assert (eq var-obj *variant*))
      (assert (ok-object? player :context :in-game :warn-on-failure t))
      (assert (ok-object? dungeon :context :in-game :warn-on-failure t))
      (assert (ok-object? var-obj :context :in-game :warn-on-failure t))
      ;; hack
      (loop for x being the hash-values of (variant.spells var-obj)
	    do
	    (assert (ok-object? x :context :in-game :warn-on-failure t)))
      )

    
    (let* ((mon-len (length (dungeon.monsters dungeon)))
	   (pq (lb-ds:make-priority-queue :size (* 2 mon-len)))
	   (pq-arr (make-array (* 2 mon-len) :initial-element nil)))
	  

      ;; insert all entries
      (dolist (i (dungeon.monsters dungeon))
	;; all monsters should get some randomness to actions
	(incf (get-creature-energy i) (random 10))
	(lb-ds:pq-insert i (get-creature-energy i) pq))
      ;; we let the player get some randomness too
      (incf (get-creature-energy player) (random 20))
      (lb-ds:pq-insert player (get-creature-energy player) pq)

   
      (block main-dungeon-loop
	
	(loop

	 (let ((front (lb-ds:pq-front pq)))

	   (refresh-window *map-frame*)
	   (refresh-window +charinfo-frame+)

	   ;;(warn "[~s] Checking ~s with energy ~s" (lb-ds:pq-size pq) (get-creature-name front) (get-creature-energy front))
	   (cond ((> (get-creature-energy front) +energy-normal-action+) ;; he or she can do an action
		  (etypecase front
		    (player
		     (lb-ds:pq-remove pq)
		     (process-player! variant dungeon player)
		     (verify-panel dungeon player) ;; hack
		     (lb-ds:pq-insert player (get-creature-energy player) pq))

		    (active-monster
		     (let* ((mon (lb-ds:pq-remove pq))
			    (mx (location-x mon))
			    (my (location-y mon)))
		       (check-type mon active-monster)
		       (when (creature-alive? mon)
			 (decf (get-creature-energy mon) +energy-normal-action+)
			 ;; skip the 'sensing' of player
			 ;; only do something if there is clear sight
			 (when (player-has-los-bold? dungeon mx my)
			   (process-single-monster! dungeon player mon)))

		       (when (creature-alive? mon)
			 (lb-ds:pq-insert mon (get-creature-energy mon) pq))
		       
		       ))))
		 
		 ;; not even the front one has enough energy, boost them all
		 (t
		  #||
		  (warn "boost energy.")
		  (loop for i from 1 to (lb-ds::heap-size pq)
			do (format t "~&~d ~a~%" (lb-ds::pq-elem-priority (aref (lb-ds::heap-array pq) i))
				   (get-creature-name (lb-ds::pq-elem-value (aref (lb-ds::heap-array pq) i)))))
		  ||#
		  ;; move to pq-arr
		  (loop for cnt from 0 below (lb-ds:pq-size pq)
			do
			(setf (aref pq-arr cnt) (lb-ds:pq-remove pq)))
		  
		  (loop for i from 0
			for x across pq-arr
			do
			(progn
			  (when (and x (creature-alive? x))
			    (incf (get-creature-energy x) (energy-for-speed x))
			    (lb-ds:pq-insert x (get-creature-energy x) pq))
			  (setf (aref pq-arr i) nil)))

		  ;; this should be a diff vs the last entry
		  (incf (variant.turn var-obj))

		  #||
		  (warn "Boosted")
		  (loop for i from 1 to (lb-ds::heap-size pq)
			do (format t "~&~d ~a~%" (lb-ds::pq-elem-priority (aref (lb-ds::heap-array pq) i))
				   (get-creature-name (lb-ds::pq-elem-value (aref (lb-ds::heap-array pq) i)))))

		  (warn "---")
		  ||#
		  ))
	   
	   (let ((leave-sym (player.leaving? player)))
	     (when leave-sym
	       (return-from run-level! leave-sym)))
	   
	   (when (/= 0 *update*) (update-stuff var-obj dungeon player))
	   (process-world& var-obj dungeon player)
	   ;; do other stuff
	   ;; hack
	   (when (/= 0 *update*) (update-stuff var-obj dungeon player))
	   (when (/= 0 *redraw*) (redraw-stuff var-obj dungeon player))
	   )))
      
      )))
       
       #||
       ;; postpone compact
;;       (warn "loop")
       (refresh-window *map-frame*)
       (refresh-window +charinfo-frame+)

       (warn "*Player ~s ~s, Monsters ~s ~s" (get-creature-energy player) (energy-for-speed player)
	     (length (dungeon.monsters dungeon))
	     (mapcar #'get-creature-energy (dungeon.monsters dungeon)))

       (energise-creatures! var-obj dungeon player)

       (warn "%Player ~s Monsters ~s ~s" (get-creature-energy player) (length (dungeon.monsters dungeon))
	     (mapcar #'get-creature-energy (dungeon.monsters dungeon)))

       (when (/= 0 *update*) (update-stuff var-obj dungeon player))
	      
       (process-monsters& dungeon player +energy-normal-action+)
       ;; stuff

       (let ((leave-sym (player.leaving? player)))
	 (when leave-sym
	   (return-from run-level! leave-sym)))

       (when (/= 0 *update*) (update-stuff var-obj dungeon player))
       
       (process-world& var-obj dungeon player)
       ;; do other stuff
       ;; hack
       (verify-panel dungeon player)

       (when (/= 0 *update*) (update-stuff var-obj dungeon player))

       ;; (warn "redraw is ~a" *redraw*)
       (when (/= 0 *redraw*) (redraw-stuff var-obj dungeon player))


       ;; do this till things are fixed..
;;       (print-map dungeon player)

       ;;(warn "turn ~s" (variant.turn var-obj))
       (incf (variant.turn var-obj))

       ))
       ||#
               
    

(defun game-loop& ()
  "This is the main game-loop.  and this function looks _ugly_."
  (multiple-value-setq (*player* *variant* *level*)
      (load-old-environment&))
    (update-term-sizes!)    
    (loop
     ;; clean up to prevent too many delays while running the dungeon
     ;; it may take quite some time
     (garbage-collect :global t)
     
     ;; let's run this dungeon
     
     (let ((how-level-was-left nil))
       
       (setq how-level-was-left (run-level! *level* *player*))
       
;;       (tricky-profile
;;	(setq how-level-was-left (run-level! *level* *player*))
;;	:time)
       
       ;; return if we're toast
       (when (or (player.dead? *player*)
		 (eq (player.leaving? *player*) :quit))
	 ;;(warn "->End game and level is ~s" *level*)
	 (return-from game-loop&))
       
       ;; generate new cave
       (setq *level* (create-appropriate-level *variant* *level*
					       *player* (player.depth *player*)))
       
       (activate-object *level* :player *player*
			:leave-method how-level-was-left)

       ;; do it again?
       (garbage-collect :global t)
       ;; safety? we will reload in less than a second :-)
       (save-current-environment&))
     ))


(defun save-current-environment& ()
  "Attempts to save the environment."
  (setf (get '*player* 'last-value) *player*
	(get '*variant* 'last-value) *variant*
	(get '*level* 'last-value) *level*)
  'last-value)

(defun load-old-environment& ()
  "Returns three values with an old environment."
  (values (get '*player* 'last-value)
	  (get '*variant* 'last-value)
	  (get '*level* 'last-value)))

(defun %load-saved-game (fname)
  "Will assign values to *variant*, *player* and *level if it can."

  ;; use default loader
  (let ((loaded nil)
	(pname fname)
	(format :binary))

    (when (stringp pname)
      (setf pname (pathname fname)))

    (when (equal (pathname-type fname) "lisp")
      (setf format :readable))
    
    (handler-case
	(setf loaded (load-a-saved-game nil fname format))
      (savefile-problem (sp)
	(pause-last-line! :msg (saveproblem.desc sp) :attr +term-l-red+)
	(pause-last-line! :msg "[Creating new character instead.]" :attr +term-l-red+)
	)
      (end-of-file (co)
	(pause-last-line! :attr +term-l-red+
			  :msg (format nil "Error when loading a savegame (~s), creating new character." co))))
    
    ;; we're lenient about the order things are returned in
    (dolist (i loaded)
      (typecase i
	(player (setf *player* i))
	(level (setf *level* i))
	(variant (setf *variant* i))
	(null ;; a nil value is always total miss
	 (return-from %load-saved-game nil))
	(otherwise
	 (warn "Loading gave weird value back: ~s" i))))

    nil))

(defun interactive-variant-select ()
  "Returns id to a selected variant, or NIL on failure."
  ;; move this to variant-selection
  (let ((vars (get-registered-variants)))
   
    (cond ((> (length vars) 1)
	   (let ((hgt (get-frame-height))
		 (result nil))
	     (clear-window-from *cur-win* (- hgt 4))
	     (setf result  (interactive-alt-sel 5 (- hgt 3) vars :ask-for "variant to play"))
	     
	     (when (and (integerp result) (>= result 0))
	       (return-from interactive-variant-select (elt vars result)))
	     
	     (warn "Got odd variant selection result ~s" result)
	     ;; just using the first one
	     (first vars)))
	  
	  ((= (length vars) 0)
	   (error "No variant plugin found."))
	  
	  (t
	   (first vars)))))
 

(defun interactive-savefile-select (variant-id)
  "Returns 'new-game for new game or a string with filename to wanted savegame to load."
  
  ;;; This function is ugly.. be careful
  
  (unless (stringp variant-id)
    (return-from interactive-savefile-select nil))
  
  ;; now we should find savegames, or select new game
  (let* ((files (directory (variant-save-dir variant-id))))
    
    (unless files ;; we have no files, assume new game
      (return-from interactive-savefile-select 'new-game))
    
    ;; only select binaries
    (let ((real-files '()))
      (dolist (i files)
	(let ((type (pathname-type i)))
	  (cond ((equal type nil)
		 nil) ;; do nothing
		
		((equal type "bin") ;; we want this one
		 (when-bind (header (load-saveheader i))
		   (check-type header saveheader)
		   ;;(warn "Header is ~s" header)
		   (when (plusp (saveheader.status header))
		     (push (cons i (saveheader.desc header)) real-files))))
		
		((equal type "lisp") ;; we ignore this one
		 nil)
		
		(t ;; check for badness
		 (warn "Unknown filetype ~s for file ~s.  Ignoring." type i)))))
      
    
      ;;(warn "files ~s" real-files)
    
      (flet ((show-desc (num)
	       (cond ((plusp num)
		      (let ((elm (elt real-files (1- num))))
			;;(warn "Want desc of ~s -> ~s" num elm)
			(cdr elm)))
		     (t
		      "                     "))))
      
	(cond ((= (length real-files) 0)
	       (return-from interactive-savefile-select nil))
	      ((> (length real-files) 0) ;; if more files we need choices
	     
	       (let ((hgt (get-frame-height))
		     (to-show (loop for i in real-files
				    collecting (pathname-name (car i))))
		     (result nil))
		 (clear-window-from *cur-win* (- hgt 6))
		 (setf result  (interactive-alt-sel 5 (- hgt 3) (cons "<<NEW>>" to-show)
						    :ask-for "savefile to use"
						    :display-fun #'show-desc
						    :mod-value 5
						    ;; hack
						    :settings (make-instance 'birth-settings
									     :text-x 8
									     :text-y (- hgt 5)
									     :text-attr +term-yellow+)))
		 
		 (when (integerp result)
		   (cond ((= result 0)
			  (return-from interactive-savefile-select 'new-game)) ;; new game
			 ((> result 0)
			  (return-from interactive-savefile-select (elt real-files (1- result))))))
		 ))))
	     
	     
      nil)))

(defun %create-new-character (wanted-variant)
  "Handles creation of a new character in given variant."

  (unless wanted-variant
    (error "Tried to created new character but no variant is specified."))

  ;; so far we just load vanilla
  (let ((var-obj (load-variant& wanted-variant :verbose t)))
    (cond ((not var-obj)
	   (warn "Unable to find variant ~s" wanted-variant)
	   (return-from %create-new-character nil))
	  (t
	   (setf *variant* var-obj)
	   (activate-object var-obj))))

  ;; then it's time to actually create our player (with a dummy level)
  (let ((*level* (make-instance 'level))) ;; evil hack
    (interactive-creation-of-player *variant*)))


;;; This one is a mess!!! please divide in more fitting functions!
(defun play-game& ()
  "Should not be called directly."

  ;;(warn "back in lisp!!")
  
  (update-term-sizes!)
  
  (let ((*player* nil)
	(*level* nil)
;;	#+allegro
;;	(old-spread (sys:gsgc-parameter :generation-spread))
	)

    (when (eq (get-system-type) 'gcu)
      (setf *map-frame* +asciimap-frame+))

    (loop for x across *windows*
	  for i from 0
	  do
	  (let ((idx (+ 51 i)))
	    (establish-data-in-window x)
	    (when (window.backgroundfile x)
	      (lb-ffi:c-load-texture& idx
			       (concatenate 'string *engine-graphics-dir* (window.backgroundfile x))
			       (window.pixel-width x) (window.pixel-height x) 0)
	      (setf (window.background x) idx)
	      ;; c-side needs negative value for bad values
	      (lb-ffi:c-add-frame-bg! i idx)
	      ;;(print x)
	      )))

    (setf *cur-win* (aref *windows* 0))
    
    ;; !!check if we use gfx first!!
    (let ((splash-idx (load-image& *variant* "other/langtitle.bmp" 1 0)))
      (when (plusp splash-idx)
	(fill-area *cur-win* splash-idx 0 0 0 99 36) ;; hack
	(paint-gfx-image *cur-win* splash-idx 5 0)))
    
    (print-note! "[Initing sound-system]")
	    
    (init-sound-system& 40) ;; fix this later
    
    (print-note! "[Initialization complete]")

;;    (pause-last-line!)
     ;; end init_ang
     
    (let ((*load-verbose* nil))
      (load-game-data "prefs.lisp"))

    ;; hack to remove cursor
    (set-cursor-visibility nil)
    (flush-messages! t)

    (pause-last-line!)

    (let* ((wanted-variant (interactive-variant-select))
	   (start-action nil))

      (unless (and wanted-variant (stringp wanted-variant))
	(error "Unable to find any variant ~s" wanted-variant))

      ;;(warn "*variant* here is ~s ~s" *variant* wanted-variant)
      
      (setf start-action (interactive-savefile-select wanted-variant))

      (when (consp start-action)
	(setf start-action (car start-action)))
      
      ;;(warn "start ~s ~s" wanted-variant start-action)

      (when (or (pathnamep start-action)
		(stringp start-action))
	;; we found a savefile we want
	(print-note! "[Trying to load savefile ..]")
	(%load-saved-game start-action))
      
      (when (or (eq start-action nil)
		(eq start-action 'new-game)
		(eq *player* nil)) ;; in case we screwed up loading a game
	;; start a new game, no savefiles with player found
	(setf *player* (%create-new-character wanted-variant)))
      )
    

    ;; at this point *player* and *variant* must be correct.. *level* can be fixed
    
    (unless *current-key-table*
      (setf *current-key-table* *ang-keys*))

    ;; we must make sure that our player has proper symbol
    (setf (x-attr *player*) (tile-file +tilefile-classes+)
	  (x-char *player*) (tile-number (get-class-tile-number *variant* *player*)))
    (setf (gfx-sym *player*) (logior (x-attr *player*) (x-char *player*)))
    
    
;;    (unless *level*
;;      (put-coloured-line! +term-white+ "Please wait..." 0 0)  
;;      (pause-last-line!))

    ;; now we want normal layout!
    (switch-to-regular-frameset&)

    #||
    (loop for x across *windows*
	  do
	  (warn "Win ~s, has ~s,~s offset and ~s,~s coords ~s"
		(window.id x) (window.x-offset x) (window.y-offset x)
		(window.pixel-width x) (window.pixel-height x)
		(window.visible? x)))
    ||#
    
    
    (setf *cur-win* (aref *windows* *map-frame*))
    ;;(warn "Curwin is ~s" *cur-win*)
    
    (block dungeon-running
      (unless *level* 
	(setf *level* (create-appropriate-level *variant* *level*
						*player* (player.depth *player*)))
	(activate-object *level* :player *player*
			 :leave-method nil))
      
      (save-current-environment&)
      (game-loop&))

    ;;(warn "End game and level is ~s" *level*)
    (cond ((and (is-player? *player*) ;;(player.dead? *player*)
		)
	   (texture-background! +full-frame+ "" -1)
	   (flush-messages! t)
	   (arrange-game-exit& *variant* *player*))
	   
	  (t
	   (put-coloured-line! +term-white+ "Quitting..." 0 0)

	   ))
    
    ;;(pause-last-line!)
    (quit-game&)
    t))

;; low-level definitions, move it somewhere else later..
#+allegro
(ff:defun-foreign-callable c-callable-play ()
  (play-game&))

#+allegro
(ff:defun-foreign-callable c-callable-resize (w h)
  (%adjust-screen-size w h))

#+allegro
(ff:defun-foreign-callable c-callable-mouseclick (button x y)
  (%mouse-clicked button x y))

#+lispworks
(fli:define-foreign-callable ("LB_PlayGame" :result-type :void) ()
;;  (warn "callback play")
  (play-game&))

#+lispworks
(fli:define-foreign-callable ("LB_AdjustSize" :result-type :void)
    ((w :int) (h :int))
;;  (warn "Resize to ~s ~s" w h)
  (%adjust-screen-size w h))

#+lispworks
(fli:define-foreign-callable ("LB_MouseClicked" :result-type :void)
    ((button :int) (x :int) (y :int))
  (%mouse-clicked button x y))
