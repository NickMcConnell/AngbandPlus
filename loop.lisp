;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: loop.lisp - the game loop(s)
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with the game loops.

|#

(in-package :org.langband.engine)


(defun redraw-stuff (dun pl)
  "Redraws stuff according to *REDRAW*."
  
  (when (= 0 *redraw*) (return-from redraw-stuff nil))

  (let ((retval nil)
	(pr-set nil))
    
    (when (bit-flag-set? *redraw* +print-map+)
      (bit-flag-remove! *redraw* +print-map+)
      (print-map dun pl)
;;      (warn "frsh")
;;      (c-term-fresh!)
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
      (print-basic-frame dun pl)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-misc+)
      (bit-flag-remove! *redraw* +print-misc+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))

      (print-field (get-race-name pl) (slot-value pr-set 'race))
      (print-field (get-class-name pl) (slot-value pr-set 'class))
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-title+)
      (bit-flag-remove! *redraw* +print-title+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-title pl pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-level+)
      (bit-flag-remove! *redraw* +print-level+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-level pl pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-xp+)
      (bit-flag-remove! *redraw* +print-xp+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-xp pl pr-set)
      (setf retval t))

    ;; stats
    
    (when (bit-flag-set? *redraw* +print-armour+)
      (bit-flag-remove! *redraw* +print-armour+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-armour-class pl pr-set)
      (setf retval t))
    
    (when (bit-flag-set? *redraw* +print-hp+)
      (bit-flag-remove! *redraw* +print-hp+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-hit-points pl pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-mana+)
      (bit-flag-remove! *redraw* +print-mana+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-mana-points pl pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-gold+)
      (bit-flag-remove! *redraw* +print-gold+)
      (unless pr-set (setf pr-set (get-setting :basic-frame-printing)))
      (print-gold pl pr-set)
      (setf retval t))

    
    ;; more stuff here
      
    
    (when (/= 0 *redraw*)
      (warn "Unhandled flags ~s" *redraw*))
    
    retval))


(defun update-stuff (dun pl)
  "Updates stuff according to *UPDATE*."

  (when (= 0 *update*) (return-from update-stuff nil))
  
  (let ((retval nil)
	(var-obj *variant*))
    
    (when (bit-flag-set? *update* +pl-upd-bonuses+)
      (bit-flag-remove! *update* +pl-upd-bonuses+)
      (calculate-creature-bonuses! var-obj pl)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-torch+)
      (bit-flag-remove! *update* +pl-upd-torch+)
      (calculate-creature-light-radius! var-obj pl)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-hp+)
      (bit-flag-remove! *update* +pl-upd-hp+)
      (calculate-creature-hit-points! var-obj pl)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-mana+)
      (bit-flag-remove! *update* +pl-upd-mana+)
;;      (calculate-creature-hit-points! var-obj pl)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-spells+)
      (bit-flag-remove! *update* +pl-upd-spells+)
;;      (calculate-creature-hit-points! var-obj pl)
      (setf retval t))
    
    
    
    (when (bit-flag-set? *update* +pl-upd-forget-view+)
      (bit-flag-remove! *update* +pl-upd-forget-view+)
      (forget-view! dun pl)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-update-view+)
      (bit-flag-remove! *update* +pl-upd-update-view+)
      (update-view! dun pl)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-forget-flow+)
      (bit-flag-remove! *update* +pl-upd-forget-flow+)
;;      (forget-view! dun pl)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-update-flow+)
      (bit-flag-remove! *update* +pl-upd-update-flow+)
;;      (update-view! dun pl)
      (setf retval t))

    (when (bit-flag-set? *update* +pl-upd-distance+)
      (bit-flag-remove! *update* +pl-upd-distance+)
      (bit-flag-remove! *update* +pl-upd-monsters+)
;;      (update-view! dun pl)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-monsters+)
      (bit-flag-remove! *update* +pl-upd-monsters+)
;;      (update-view! dun pl)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-panel+)
      (bit-flag-remove! *update* +pl-upd-panel+)
      (verify-panel dun pl)
      (setf retval t))


    (when (/= 0 *update*)
      (warn "Unhandled upd-flags ~s" *update*))

    
    retval))

(defun handle-stuff (dun pl)
  (let ((retval nil))
    (unless (= 0 *update*)
      (setf retval (update-stuff dun pl)))
    (unless (= 0 *redraw*)
      (setf retval (or (redraw-stuff dun pl) retval)))
  ;; add window-stuff
    retval))


(defvar *proj-arr-hack* (make-array 40 :fill-pointer 0))

(defun %possibly-move-monster! (dun pl mon)
  "Tries to process a single monster."

  (let ((mx (location-x mon))
	(my (location-y mon))
	(random-mover (has-ability? mon '<random-mover>)))
    
    (declare (type u-16b mx my))
    
    (when random-mover
;;      (warn "~s is random.." mon)
      (let ((how-often (second random-mover)))
	(when (< (random 100) (* 100 how-often))
	  ;;(warn "moving at random..")
	  (return-from %possibly-move-monster! t))
	))

    (when (player-has-los-bold? dun mx my)
    
      (let ((px (location-x pl))
	    (py (location-y pl))
	    (proj-arr *proj-arr-hack*)
	    )

;;    (lang-warn "Action for '~a' at (~s,~s)" (monster.name mon)
;;	       mx my)

    

      (setf (fill-pointer proj-arr) 0)
;;      (lang-warn "-> '~a' at (~s,~s) can see player at (~s,~s)" (get-creature-name mon) mx my px py)
      (let ((res (project-path dun 18 proj-arr mx my px py 0)))
;;	(warn "Project gave ~s -> ~s" res (loop for x across proj-arr collecting (cons (grid-x x) (grid-y x))))
	(when (plusp res)
	  (let* ((the-grid (aref proj-arr 0))
		 (the-x (grid-x the-grid))
		 (the-y (grid-y the-grid)))
	    
	    (cond ((and (= px the-x)
			(= py the-y))
		   ;; are we close enough to kill the character?
		   (cmb-monster-attack! dun pl mon the-x the-y))
		  (t
		   (let ((legal-x nil)
			 (legal-y nil)
			 (project-legal-p (legal-move? dun mx my the-x the-y)))

		     (when project-legal-p
		       (setq legal-x the-x
			     legal-y the-y))

		     (unless (and legal-x legal-y)
		       (let ((sx (if (< px mx) -1 1))
			     (sy (if (< py my) -1 1)))

;;			 (warn "Unable to get legal move, trying fuzzy..")
			 
			 (cond ((legal-move? dun mx my (+ sx mx) my)
				(setq legal-x (+ sx mx)
				      legal-y my))
			       ((legal-move? dun mx my mx (+ sy my))
				(setq legal-x mx
				      legal-y (+ sy my))))))

		     (when (and legal-x legal-y)
		       (swap-monsters! dun pl mx my legal-x legal-y))
		     #||
			 (progn
			   (unless project-legal-p
			     (warn "We're at [~s,~s,~a] wanted to go [~s,~s,~a] but went [~s,~s,~a] (~a <-> ~a)"
				   mx my (cadr (multiple-value-list (map-info dun mx my)))
				   the-x the-y (cadr (multiple-value-list (map-info dun the-x the-y)))
				   legal-x legal-y (cadr (multiple-value-list (map-info dun legal-x legal-y)))
				   res (distance mx my px py)))
			   
			   
			 (warn "We're at [~s,~s,~a] wanted to go [~s,~s,~a] but went [~s,~s,~a] (~a <-> ~a)"
			       mx my (cadr (multiple-value-list (map-info dun mx my)))
			       the-x the-y (cadr (multiple-value-list (map-info dun the-x the-y)))
			       legal-x legal-y nil ;;(cadr (multiple-value-list (map-info dun legal-x legal-y)))
			       res (distance mx my px py)))
			 ||#
		     )))


	   ))
	))
    )))


(defun process-single-monster! (dun pl mon)

  (let ((val (has-ability? mon '<never-move>)))
    (if val
	nil ;; no nothing
	(%possibly-move-monster! dun pl mon))))

  
(defun legal-move? (dun x1 y1 x2 y2)
  (declare (ignore x1 y1))
  (if (cave-floor-bold? dun x2 y2)
      t
      nil))


(defun process-monsters& (dun pl needed-energy)
  "Tries to do something nasty to all the monsters."
  
  (with-dungeon-monsters (dun m)
;;    (warn "Trying to process monster ~a with energy ~a and speed ~a -> will need ~a energy"
;;	  (get-creature-name m) (get-creature-energy m)
;;	  (get-creature-speed m) needed-energy)
    (when (>= (get-creature-energy m) needed-energy)
      ;; we deduct before we do things to be sure, improve  later
      (decf (get-creature-energy m) +energy-normal-action+)
      (process-single-monster! dun pl m)))
  nil)


(defun process-player! (dun pl)
  "processes the player in a given turn"

  (loop named waste-energy
	do
	(when (/= *update* 0) (update-stuff dun pl))
	(when (/= *redraw* 0) (redraw-stuff dun pl))
	
	(put-cursor-relative! dun (location-x pl) (location-y pl))

	;; assume no energy is used
	(setf (player.energy-use pl) 0)
	
	;; skip paralysis/stun
	;; skip resting
	;; skip running
	;; skip repeat
	;; do normal command

	(get-and-process-command! dun pl :global)
	
	
	(when (plusp (player.energy-use pl))
	  (decf (get-creature-energy pl) (player.energy-use pl)))

	while (and (= 0 (player.energy-use pl))
		   (not (player.leaving-p pl)))
	))

    
	       
  

(defun regenerate-mana! (crt percent)
  (declare (ignore crt percent))
  nil)

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

(defun process-world& (dun pl)
  "tries to process important world-stuff every 10 turns."

  (let* ((var-obj *variant*)
	 (the-turn (variant.turn var-obj))
	 (lvl (dungeon.depth dun)))

    (unless (= 0 (mod the-turn 10)) ;; every 10 turns only
      (return-from process-world& nil))

    ;; if in town fix lightning

    (when (plusp lvl) ;; in dungeon
      ;; shuffle stores
      nil)

    ;; possibly allocate new monster
    ;; possible monster-regeration

    ;; fix special damage

    ;; check food
    (decf (player.food pl))

    ;; possible regenerate
    (let ((regen-amount 197))
	  
      ;; affect regen by food
      ;; affect regen by abilities and items

      (when (< (player.cur-mana pl)
	       (player.max-mana pl))
	(regenerate-mana! pl regen-amount))

      ;; affected by condition

      (when (< (current-hp pl)
	       (maximum-hp pl))
	(regenerate-hp! pl regen-amount)))
      

    ;; do timeout'ing of effects
    
    ;; burn fuel when needed
    (when-bind (l-s (get-light-source pl))
      (unless (is-artifact? l-s)
	(let ((gvals (aobj.game-values l-s)))
	  (decf (gval.charges gvals))
	  (when (< (gval.charges gvals) 1)
	    (setf (gval.light-radius gvals) 0)))))
		 
    
    ;; drain xp
    
    ;; check for timeouts on equipment
    
    ;; recharge rods

    ;; recharge things on the ground
    
    ;; random teleport/WoR

    ))

(defun energy-for-speed (crt)
  (aref +energy-table+ (get-creature-speed crt)))
			 

(defun energise-creatures! (dun pl)

  (incf (get-creature-energy pl) (energy-for-speed pl))

  ;; boost all monsters
  (with-dungeon-monsters (dun m)
    (declare (ignore dun))
    (incf (get-creature-energy m) (energy-for-speed m)))
  
  ;; can our dear player do anything?

  (loop named player-fun
	while (and (>= (get-creature-energy pl) +energy-normal-action+) ;; better solution?
		   (not (player.leaving-p pl)))
	do
	(process-monsters& dun pl (1+ (get-creature-energy pl)))
	
	(unless (player.leaving-p pl)
	  (process-player! dun pl)))
  )

  
(defun run-level! (level pl)
  "a loop which runs a dungeon level"

  (let* ((dun (level.dungeon level))
	 (var-obj *variant*)
	 (*dungeon* dun)
	 (dungeon-height (dungeon.height dun))
	 (dungeon-width  (dungeon.width dun))

	 )
  
    ;; we're not leaving
    (setf (player.leaving-p pl) nil)
      
    ;; setting these to illegal values
    (setf (player.view-x pl) dungeon-width)
    (setf (player.view-y pl) dungeon-height)
  
    ;; no stairs from town
;;    (setf (dungeon.up-stairs-p dun) nil
;;	  (dungeon.down-stairs-p dun) nil)
    

    ;; create stairs.. (postponed)

    ;; postpone verify of panel
    (verify-panel dun pl)
    
    (c-print-message! +c-null-value+)

    ;;; == this section needs serious rework.. see angband
    ;; postpone flush

    (clear-the-screen!)

    (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+) 
    (bit-flag-add! *update* +pl-upd-hp+ +pl-upd-spells+ +pl-upd-mana+) 

    (update-stuff dun pl)
    
    (bit-flag-add! *redraw* +print-map+ +print-basic+)
    (bit-flag-add! *update* +pl-upd-forget-view+ +pl-upd-update-view+)
  
    
    ;; postpone stuff..
    (update-stuff dun pl)

    (redraw-stuff dun pl)


    (block main-dungeon-loop

      (loop
       ;; postpone compact
;;       (warn "loop")


       (energise-creatures! dun pl)

       (when (/= 0 *update*) (update-stuff dun pl))
	      
       (process-monsters& dun pl +energy-normal-action+)
       ;; stuff

       (let ((leave-sym (player.leaving-p pl)))
	 (when leave-sym
	   (return-from run-level! leave-sym)))

       (when (/= 0 *update*) (update-stuff dun pl))
       
       (process-world& dun pl)
       ;; do other stuff
       ;; hack
       (verify-panel dun pl)

       (when (/= 0 *update*) (update-stuff dun pl))

       ;; (warn "redraw is ~a" *redraw*)
       (when (/= 0 *redraw*) (redraw-stuff dun pl))


       ;; do this till things are fixed..
;;       (print-map dun pl)
     
       (incf (variant.turn var-obj))

       ))
               
    ))

(defun game-loop& ()
  "This is the main game-loop.  and this function looks _ugly_."
  (multiple-value-bind (*player* *variant* *level*)
      (load-old-environment&)
    
    (loop
     ;; clean up to prevent too many delays while running the dungeon
     (garbage-collect :global t)
     
     ;; let's run this dungeon
     
     (let ((how-level-was-left nil))
       
       (setq how-level-was-left (run-level! *level* *player*))
       
;;       (tricky-profile
;;	(setq how-level-was-left (run-level! *level* *player*))
;;	:time)
       
       ;; return if we're toast
       (when (player.dead-p *player*)
	 (return-from game-loop&))
       
       ;; generate new cave
       (setq *level* (create-appropriate-level *variant* *level*
					       *player* (player.depth *player*)))
       
       (activate-object *level* :player *player*
			:leave-method how-level-was-left)
       
       ;; safety? we will reload in less than a second :-)
       (save-current-environment&)))
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

(defun load-saved-game (fname format)
  "Returns three values."
  
  (let ((loaded (do-load nil fname (list :variant :player :level) format))
	(the-player nil)
	(the-level nil)
	(the-var nil))

    ;; we're lenient about the order things are returned in
    (dolist (i loaded)
      (typecase i
	(player (setf the-player i))
	(level (setf the-level i))
	(variant (setf the-var i))
	(otherwise
	 (warn "Loading gave weird value back: ~s" i))))


    (values the-level the-player the-var)))

;;(trace load-saved-game)

(defun play-game& ()
  "Should not be called directly."
  

  (let ((*player* nil)
	(*level* nil)
;;	#+allegro
;;	(old-spread (sys:gsgc-parameter :generation-spread))
	)

    (let ((*load-verbose* nil))
      (load-game-data "prefs.lisp"))

    ;; hack to remove cursor
    (c-set-cursor& 0)

    ;; FIX: this code _must_ be rewritten!!
    (block creation
      (loop
       (let ((the-player nil)
	     (the-level nil)
	     (the-variant nil)
	     (the-save-file (concatenate 'string (home-langband-path) +binary-save-file+))
	     (format :binary)
	     ;;(the-save-file +readable-save-file+)
	     ;;(format :readable)
	     )
	 
	 (when (probe-file (pathname the-save-file))
	   ;; we can load a saved game
	   (let ((*level* (make-instance 'level))) ;; evil hack
	     (multiple-value-bind (lv pl var)
		 (load-saved-game the-save-file format)
	       (if (and pl (typep pl 'player))
		   (setf the-player pl)
		   (warn "Unable to load player from ~s" the-save-file))
	       (if (and lv (typep lv 'level))
		   (setf the-level lv)
		   (warn "Unable to load level from ~s" the-save-file))
	       (if (and var (typep var 'variant))
		   (setf the-variant var)
		   (warn "Unable to load variant from ~s" the-save-file))
			
	       )))

	 (unless the-player ;; unable to load one.
	   
	   (let ((*level* (make-instance 'level))) ;; evil hack
	     (setf the-player (interactive-creation-of-player *variant*))))

	 ;; ok have we gotten anything?
	 (when the-player
	   (setf *player* the-player)
	   (when the-level (setf *level* the-level))
	   (when the-variant (setf *variant* the-variant))
	   (return-from creation))
       
	 (warn "Trying to create player again..")
	 )))

;;    (unless *level*
;;      (c-prt! "Please wait..." 0 0)  
;;      (c-pause-line! *last-console-line*))
    
    (clear-the-screen!)
    
    (block dungeon-running
      (unless *level* 
	(setf *level* (create-appropriate-level *variant* *level*
						*player* (player.depth *player*)))
	(activate-object *level* :player *player*
			 :leave-method nil))
      
      (save-current-environment&)
      (game-loop&))

    (cond ((and *player* (player.dead-p *player*))
	   (organise-death& *player* *variant*))
	   
	  (t
	   (c-prt! "Quitting..." 0 0)

	   ))
    
    (c-pause-line! *last-console-line*)
    (quit-game&)
    t))

;; low-level definitions, move it somewhere else later..
#+allegro
(ff:defun-foreign-callable c-callable-play ()
  (play-game&))


;;(trace redraw-stuff)
;;(trace handle-stuff update-stuff)
