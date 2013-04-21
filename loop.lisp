;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

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


(defmethod redraw-stuff ((variant variant) (dungeon dungeon) (player player))
  "Redraws stuff according to *REDRAW*."
  
  (when (= 0 *redraw*) (return-from redraw-stuff nil))

  (let ((retval nil)
	(bot-set nil)
	(pr-set nil))
    
    (when (bit-flag-set? *redraw* +print-map+)
      (bit-flag-remove! *redraw* +print-map+)
      (print-map dungeon player)
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
      (print-basic-frame variant dungeon player)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-misc+)
      (bit-flag-remove! *redraw* +print-misc+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))

      (print-field (get-race-name player) (slot-value pr-set 'race))
      (print-field (get-class-name player) (slot-value pr-set 'class))
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
      (warn "Depth ~s ~s ~s" (dungeon.depth dungeon) bot-set (get-setting variant :bottom-row-printing))
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


(defun process-player! (variant dungeon player)
  "processes the player in a given turn"

  (let ((temp-attrs (player.temp-attrs player)))
  
  (loop named waste-energy
	do
	(when (/= *update* 0) (update-stuff variant dungeon player))
	(when (/= *redraw* 0) (redraw-stuff variant dungeon player))
	
	(put-cursor-relative! dungeon (location-x player) (location-y player))

	;; assume no energy is used
	(setf (player.energy-use player) 0)

	(cond ((or (get-attribute-value '<paralysed> temp-attrs)
		   (>= (get-attribute-value '<cut> temp-attrs) 100)) ;; move to variant
	       (setf (player.energy-use player)  +energy-normal-action+))

	      ;; skip resting
	      ;; skip running
	      ;; skip repeat
	      (t
	       ;; do normal command
	       (get-and-process-command! dungeon player :global)))
	
	
	(when (plusp (player.energy-use player))
	  (decf (get-creature-energy player) (player.energy-use player)))

	while (and (= 0 (player.energy-use player))
		   (not (player.leaving-p player)))
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
			 

(defun energise-creatures! (variant dungeon player)

  (incf (get-creature-energy player) (energy-for-speed player))

  ;; boost all monsters
  (with-dungeon-monsters (dungeon mon)
    (declare (ignore dungeon))
    (incf (get-creature-energy mon) (energy-for-speed mon)))
  
  ;; can our dear player do anything?

  (loop named player-fun
	while (and (>= (get-creature-energy player) +energy-normal-action+) ;; better solution?
		   (not (player.leaving-p player)))
	do
	(progn
	  (process-monsters& dungeon player (1+ (get-creature-energy player)))
	  
	  (unless (player.leaving-p player)
	    (process-player! variant dungeon player))))
  )

  
(defun run-level! (level player)
  "a loop which runs a dungeon level"

  (let* ((dungeon (level.dungeon level))
	 (var-obj *variant*)
	 (*dungeon* dungeon)
	 (dungeon-height (dungeon.height dungeon))
	 (dungeon-width  (dungeon.width dungeon))
	 )
  
    ;; we're not leaving
    (setf (player.leaving-p player) nil)
      
    ;; setting these to illegal values
    (setf (player.view-x player) dungeon-width)
    (setf (player.view-y player) dungeon-height)
  
    ;; no stairs from town
;;    (setf (dungeon.up-stairs-p dungeon) nil
;;	  (dungeon.down-stairs-p dungeon) nil)
    

    ;; create stairs.. (postponed)

    ;; postpone verify of panel
    (verify-panel dungeon player)
    
    (print-message! +c-null-value+)

    ;;; == this section needs serious rework.. see angband
    ;; postpone flush

    (clear-the-screen!)

    (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-torch+) 
    (bit-flag-add! *update* +pl-upd-hp+ +pl-upd-spells+ +pl-upd-mana+) 

    (update-stuff var-obj dungeon player)
    
    (bit-flag-add! *redraw* +print-map+ +print-basic+)
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


    (block main-dungeon-loop

      (loop
       ;; postpone compact
;;       (warn "loop")
       
       (energise-creatures! var-obj dungeon player)

       (when (/= 0 *update*) (update-stuff var-obj dungeon player))
	      
       (process-monsters& dungeon player +energy-normal-action+)
       ;; stuff

       (let ((leave-sym (player.leaving-p player)))
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
               
    ))

(defun game-loop& ()
  "This is the main game-loop.  and this function looks _ugly_."
  (multiple-value-bind (*player* *variant* *level*)
      (load-old-environment&)
    
    ;; ultra hackish
    (let ((*screen-height* (get-screen-height))
	  (*screen-width* (get-screen-width)))
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
       (when (player.dead-p *player*)
	 (return-from game-loop&))
       
       ;; generate new cave
       (setq *level* (create-appropriate-level *variant* *level*
					       *player* (player.depth *player*)))
       
       (activate-object *level* :player *player*
			:leave-method how-level-was-left)

       ;; do it again?
       (garbage-collect :global t)
       ;; safety? we will reload in less than a second :-)
       (save-current-environment&)))
    )))

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

(defun %load-saved-game (fname format)
  "Returns three values."

  ;; use default loader
  (let ((loaded (load-a-saved-game nil fname format))
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

#+never
(defun %show-splash-screen (fname)
  (c-term-clear!)
  (with-open-file (s (pathname fname)
		     :direction :input)
      (loop for x = (read-line s nil 'eof)
            for i from 0
            until (eq x 'eof)
            do
            (put-coloured-str! +term-white+ x 0 i)))
  (c-term-fresh!)
  t)


;;; This one is a mess!!! please divide in more fitting functions!
(defun play-game& ()
  "Should not be called directly."
  
  (let ((*player* nil)
	(*level* nil)
	;; insert these two to prevent f*cked values.. gcu seems to need them now though :-/
	(*screen-height* (get-screen-height))
	(*screen-width* (get-screen-width))
	(*panel-height* (get-panel-height))
	(*panel-width* (get-panel-width))

;;	#+allegro
;;	(old-spread (sys:gsgc-parameter :generation-spread))
	)

    ;; if non-graphical
;;    (unless *use-graphics*
;;      (%show-splash-screen (game-data-path "news.txt")))

    ;; doing init_angband() end here
    (print-note! "[Initing macros]")

    (init-macro-system&)
    
    (print-note! "[Initialization complete]")

;;    (pause-last-line!)
     ;; end init_ang
     
    (let ((*load-verbose* nil))
      (load-game-data "prefs.lisp"))

    ;; hack to remove cursor
    (c-set-cursor& 0)
    (flush-messages!)

    ;; FIX: this code _must_ be rewritten!!
    (block creation
      (loop
       (let ((the-player nil)
	     (the-level nil)
	     (the-variant nil)
	     (save-combos (list
			   (cons (concatenate 'string (home-langband-path)
					      *binary-save-file*)  :binary)
			   (cons (concatenate 'string (home-langband-path)
					      *readable-save-file*)  :readable)))
	     
	     )
	 
	 (block possible-read-file
	   (dolist (i save-combos)
	     (let ((the-save-file (car i))
		   (format (cdr i)))
	       (when (probe-file (pathname the-save-file))
		 ;; we can load a saved game
		 (multiple-value-bind (lv player var)
		     (%load-saved-game the-save-file format)
		   (if (and player (is-player? player))
		       (setf the-player player)
		       (warn "Unable to load player from ~s" the-save-file))
		   (if (and lv (typep lv 'level))
		       (setf the-level lv)
		       (warn "Unable to load level from ~s" the-save-file))
		   (if (and var (typep var 'variant))
		       (setf the-variant var)
		       (warn "Unable to load variant from ~s" the-save-file))
		   (pause-last-line!)
		   (return-from possible-read-file t)
		   )))))

	 (unless the-player ;; unable to load one.
	   
	   ;; so far we just load vanilla
	   (let* ((var-key "langband-vanilla")
		  (var-obj (load-variant& var-key :verbose t)))
	     (cond ((not var-obj)
		    (warn "Unable to find variant ~s" var-key)
		    (return-from play-game& nil))
		   (t
		    (setf *variant* var-obj)
		    (activate-object var-obj))))

    
	   ;; run tests after variant has been loaded
	   #+xp-testing
	   (do-a-test :post)
  
	   (pause-last-line!)
	   
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

    (unless *current-key-table*
      (setf *current-key-table* *ang-keys*))

;;    (unless *level*
;;      (c-prt! "Please wait..." 0 0)  
;;      (pause-last-line!))
    
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
	   (flush-messages!)
	   (organise-death& *variant* *player*))
	   
	  (t
	   (c-prt! "Quitting..." 0 0)

	   ))
    
    (pause-last-line!)
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


;;(trace redraw-stuff)
;;(trace handle-stuff update-stuff)

;; hackish
(defun update-floor-display (num &key x-attr x-char)
  (let ((floor (get-floor-type num)))
    (unless floor
      ;;(warn "did not find floor with id ~s" num)
      )
    (when floor
      (when x-attr
	(setf (x-attr floor) (etypecase x-attr
			       (character (convert-obj x-attr :colour-code))
			       (number x-attr))))
      (when x-char
	(setf (x-char floor) (etypecase x-char
			       (character (char-code x-char))
			       (number x-char))))

      floor)))

;; slow
(defun update-kind-display (num &key x-attr x-char text-attr text-char)
  (let ((big-obj-table (variant.objects *variant*)))
    (loop for x being the hash-values of big-obj-table
	  do
	  (when (eql num (object.numeric-id x))
	    (when x-attr
	      (setf (x-attr x) x-attr))
	    (when x-char
	      (setf (x-char x) x-char))
	    (when text-char
	      (setf (text-char x) text-char))
	    (when text-attr
	      (setf (text-attr x) text-attr))
	    ))))

(defun update-flavour-display (sym name &key x-attr x-char)
  (let ((fl-type (gethash sym (variant.flavour-types *variant*))))

    (unless fl-type
      (warn "Unable to find flavour-type for ~s" sym)
      (return-from update-flavour-display nil))

    (let ((tbl (flavour-type.table fl-type)))

      ;; at this stage it is a hash-table, not an array (it is later converted)
      
      (when (arrayp tbl)
	(warn "table for flavour ~s is not htbl but ~s" sym (type-of tbl))
	(return-from update-flavour-display nil))

      (loop for x being the hash-values of tbl
	    do
	    (when (string-equal (flavour.name x) name)
	      
	      (cond ((and x-attr (numberp x-attr))
		     (setf (x-attr x) x-attr))
		    (t
		     (warn "Odd flavour x-attr ~s for ~s" x-attr x)))
	      
	      (cond ((and x-char (numberp x-char))
		     (setf (x-char x) x-char))
		    (t
		     (warn "Odd flavour x-char ~s for ~s" x-char x)))
	      
	      (return-from update-flavour-display x)))

	
      (warn "Unable to find ~s flavour ~s" sym name)
      ;;(return-from update-flavour-display nil)
      nil)))


   


(defvar *hacked* nil)
(defun update-monster-display (num &key x-attr x-char)
  (let* ((var-obj *variant*)
	 (mons (variant.monsters var-obj)))

    ;; fallback
    (unless *hacked*
      (maphash #'(lambda (k v)
		   (declare (ignore k))
		   (setf (x-attr v) (+ +graphics-start+ 7)
			 (x-char v) (+ +graphics-start+ 2)))
	       mons)
      (setf *hacked* t))
    
		 
    (let ((mon (gethash num mons)))
      (unless mon
	(loop for x being the hash-values of mons
	  do
	  (when (eql num (monster.numeric-id x))
	    (setf mon x))))
      (unless mon
	;;(warn "Unable to find monster with id ~s" num)
	)
      (when mon
	(when x-attr
	  (setf (x-attr mon) (charify-number x-attr)))
	(when x-char
	  (setf (x-char mon) (charify-number x-char)))
	mon)
      )))

#||
(defun %hack-data (fname predicate alter)
  (with-open-file (s fname
		     :direction :input)
    (loop for x = (read s nil 'eof)
	  for i from 0
	  until (eq x 'eof)
	  do
	  (when (funcall predicate x)
	    (funcall alter x)))))

(defun guzzle ()
  (let ((*print-case* :downcase)
	(*print-right-margin* 120))
    (with-open-file (ffile (pathname "dump.lsp")
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      
      (%hack-data "graf-prefs.lisp"
		  #'(lambda (x) (and (consp x)
				     (eq 'update-kind-display (car x))))
		  #'(lambda (x)
		      (let ((attr (if (>= (fourth x) 128)
				      `(+ +graphics-start+ ,(- (fourth x) 128))
				      (fourth x)))
			    (char (if (>= (sixth x) 128)
				      `(+ +graphics-start+ ,(- (sixth x) 128))
				      (sixth x))))
			(pprint `(update-kind-display ,(second x) :x-attr ,attr :x-char ,char)
				ffile)))))))

(defun guzzle2 ()
  (let ((*print-case* :downcase)
	(*print-right-margin* 120))
    (with-open-file (ffile (pathname "dump.lsp")
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      
      (%hack-data "graf-prefs.lisp"
		  #'(lambda (x) (and (consp x)
				     (eq 'update-monster-display (car x))))
		  #'(lambda (x)
		      (let ((attr (if (>= (fourth x) 128)
				      `(+ +graphics-start+ ,(- (fourth x) 128))
				      (fourth x)))
			    (char (if (>= (sixth x) 128)
				      `(+ +graphics-start+ ,(- (sixth x) 128))
				      (sixth x))))
		      (pprint `(update-monster-display ,(cadr x) :x-attr ,attr :x-char ,char)
			      ffile)))))))

(defun guzzle3 ()
  (let ((*print-case* :downcase)
	(*print-right-margin* 120))
    (with-open-file (ffile (pathname "dump.lsp")
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      
      (%hack-data "graf-prefs.lisp"
		  #'(lambda (x) (and (consp x)
				     (eq 'update-floor-display (car x))))
		  #'(lambda (x)
		      (let ((attr (- (fourth x) 128))
			    (char (- (sixth x) 128)))
		      (pprint `(update-floor-display ,(cadr x) :x-attr (+ +graphics-start+ ,attr)
				:x-char (+ +graphics-start+ ,char))
			      ffile)))))))


||#