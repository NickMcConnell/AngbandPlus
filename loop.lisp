;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: loop.lisp - the game loop(s)
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with the game loops.

|#

(in-package :langband)



(defun redraw-stuff (dun pl)
  "Redraws stuff according to *REDRAW*."
  
  (when (= 0 *redraw*) (return-from redraw-stuff))

  (when (bit-flag-set? *redraw* +print-map+)
    (bit-flag-remove! *redraw* +print-map+)
    (print-map dun pl)
    )

  (when (bit-flag-set? *redraw* +print-basic+)
    (bit-flag-remove! *redraw* +print-basic+)
    (print-basic-frame dun pl))
  )


(defun update-stuff (dun pl)
  "Updates stuff according to *UPDATE*."

  (when (= 0 *update*) (return-from update-stuff))

  (when (bit-flag-set? *update* +forget-view+)
    (bit-flag-remove! *update* +forget-view+)
    (forget-view! dun pl))

  (when (bit-flag-set? *update* +update-view+)
    (bit-flag-remove! *update* +update-view+)
    (update-view! dun pl))
  )

 
(defun process-player! (pl dun)
  "processes the player in a given turn"
  
  ;; fake stuff
  (setf (player.energy pl) 110)
  (setf (player.energy-use pl) 100)
  
  ;; we need more energy maybe
  (when (> 100 (player.energy pl))
    (return-from process-player!))


  (while (<= 100 (player.energy pl))
    (get-and-process-command! dun pl :global)
    (when (player.energy-use pl)
      (decf (player.energy pl) (player.energy-use pl)))
  ))


(defun run-level! (level pl)
  "a loop which runs a dungeon level"

  (let* ((dun (level.dungeon level))
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
    (setf (dungeon.up-stairs-p dun) nil
	  (dungeon.down-stairs-p dun) nil)
    

    ;; create stairs.. (postponed)

    ;; postpone veri of panel
    (verify-panel dun pl)
    
    (c-print-message +c-null-value+)
  
    (bit-flag-add! *redraw* +print-map+ +print-basic+)
    (bit-flag-add! *update* +forget-view+ +update-view+)
  
    ;; postpone flush

    (clear-the-screen)

    ;; postpone stuff..
    (update-stuff dun pl)

    (redraw-stuff dun pl)
  
    (block main-dungeon-loop

      (loop
       ;; postpone compact

     
       ;; do player
       (update-player! pl)
       (process-player! pl dun)
     
       ;; stuff

       (let ((leave-sym (player.leaving-p pl)))
	 (when leave-sym
	   (return-from run-level! leave-sym)))
       
       ;; do other stuff
       ;; hack
       (verify-panel dun pl)

       (when (/= 0 *update*)
	 (update-stuff dun pl))

       ;;     (warn "redraw is ~a" *redraw*)
       (when (/= 0 *redraw*)
	 (redraw-stuff dun pl))

       ;; do this till things are fixed..
;;       (print-map dun pl)
     
       (incf (variant.turn *variant*))

       ))
    ))

(defun game-loop& ()
  "This is the main game-loop."
  (multiple-value-bind (*player* *variant* *level*)
      (load-old-environment&)
    
      (loop
       ;; clean up to prevent too many delays while running the dungeon
       #+cmu
       (ext:gc)
       #+allegro
       (excl:gc t)
       ;; let's run this dungeon

       (let* ((how-level-was-left
	       ;;(tricky-profile
	       (time
		(run-level! *level* *player*)
		;;:space)
		)
		))
	   
	 
	   ;; return if we're toast
	   (when (player.dead-p *player*)
	     (return-from game-loop&))
       
	 ;; generate new cave
	 (setq *level* (create-appropriate-level *variant* *level*
						 *player* (player.depth *player*)))
	 
	 (activate-object *level* :player *player*
			  :leave-method how-level-was-left)
	 ;; safety?
	 (save-current-environment&)

	 ))))

(defun save-current-environment& ()
  (setf (get '*player* 'last-value) *player*)
  (setf (get '*variant* 'last-value) *variant*)
  (setf (get '*level* 'last-value) *level*)
  (values))

(defun load-old-environment& ()
  (values (get '*player* 'last-value)
	  (get '*variant* 'last-value)
	  (get '*level* 'last-value)))

(defun play-game& ()
  "Should not be called directly."
;;  (c-init-angband!)
;;  (c-pause-line 23)

;;  (warn "playing the damned game")
  
  ;; hack to remove cursor
  (let ((*player* nil)
	(*level* nil)
;;	#+allegro
;;	(old-spread (sys:gsgc-parameter :generation-spread))
	)

    (let ((*load-verbose* t))
      (load "lib/file/prefs.lisp"))
    
    (c-set-cursor& 0)

;;    (key-test)
    
    (block creation
      (loop
       (let* (;; this is an evil hack, use dummy level
	      (*level* (make-instance 'level))
	      (the-player (create-character)))
	 (when the-player
	   (setf *player* the-player)
	   (return-from creation))
       
	 (warn "Trying to create player again.."))))

    ;; time to init the stores
    ;; postponed
;;    (initialise-stores&) 
    (c-prt "Please wait..." 0 0)  
    (c-pause-line *last-console-line*)
    (clear-the-screen)
    
    (block dungeon-running
      (unless *level* 
	(setf *level* (create-appropriate-level *variant* *level*
						*player* (player.depth *player*)))
	(activate-object *level* :player *player*
			  :leave-method nil))
      
      (save-current-environment&)
      (game-loop&))
    
    (c-prt "Quitting..." 0 0)  
    (c-pause-line *last-console-line*)
    (c-quit! +c-null-value+)))

;; low-level definitions
#+allegro
(ff:defun-foreign-callable c-callable-play ()
  (play-game&))

#+allegro
(ff:def-foreign-call set_lisp_callback (address))

