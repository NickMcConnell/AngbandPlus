;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: loop.lisp - the game loop(s)
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with the game loops.

|#

(in-package :langband)


(defun move-player! (dun pl direction)
  "Moves the player, in a direction, if possible.
The direction is a number from the keypad."

  (let* ((cur-x (player.loc-x pl))
	 (cur-y (player.loc-y pl))
	 (wanted-x cur-x)
	 (wanted-y cur-y))

    (case direction
      (8 (decf wanted-y))
      (6 (incf wanted-x))
      (2 (incf wanted-y))
      (4 (decf wanted-x))
      (otherwise (warn "Unknown direction")))

    #||
    (warn "Position ~a ~a has currently ~a,~a -> ~a" wanted-x wanted-y (cave-feature dun wanted-x wanted-y)
	  (cave-info dun wanted-x wanted-y)
	  (cave-floor-bold? dun wanted-x wanted-y))

    ||#

    (let ((monsters (cave-monsters dun wanted-x wanted-y)))

      ;; mmonsters to attack
      (cond (monsters
	     (attack-location! dun pl wanted-x wanted-y)
	     )

	    ;; something is in the way
	    ((not (cave-floor-bold? dun wanted-x wanted-y))
	     (c-print-message "Cannot walk that way.."))

	    ;; default is just to move
	    (t
	     (swap-monsters! dun pl
			     (player.loc-x pl)
			     (player.loc-y pl)
			     wanted-x
			     wanted-y)

	     )
	    ))

    (bit-flag-add! *update* +update-view+)
    
    pl))

(defun swap-monsters! (dun pl from-x from-y to-x to-y)
  "swaps two monsters or move one"

  (let ((mon-1 (cave-monsters dun from-x from-y))
	(mon-2 (cave-monsters dun to-x to-y)))

    (setf (cave-monsters dun from-x from-y) mon-2
	  (cave-monsters dun to-x to-y) mon-1)

    
    ;; hack, move to swap later
    (when (and (= from-x (player.loc-x pl))
	       (= from-y (player.loc-y pl)))
      (setf (player.loc-x pl) to-x
	    (player.loc-y pl) to-y))
	     
    
    (when mon-1
      (warn "mon-1"))

    (when mon-2
      (warn "mon-2"))
    
    (light-spot! dun from-x from-y)
    (light-spot! dun to-x to-y)))


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

  (when (bit-flag-set? *update* +update-view+)
    (bit-flag-remove! *update* +update-view+)
    (update-view! dun pl))
  )

     
(defun select-item (dungeon player allow-from
		    &key prompt (where :backpack)
		    selection-function)
  "Selects and returns a CONS (where . which) when succesful or
NIL.  Where is a keyword :floor, :backpack or :equip and which is either
a number or a symbol identifying the place."

  (let ((allow-floor (if (or (eq allow-from :floor)
			     (find :floor allow-from))
			 t
			 nil))
	
	(allow-equip (if (or (eq allow-from :equip)
			     (find :equip allow-from))
			 t
			 nil))
	
	(allow-backpack (if (or (eq allow-from :backpack)
				(find :backpack allow-from))
			    t
			    nil))
	(the-prompt (if prompt prompt "Inventory command:"))

	(show-mode nil)
	(the-place where)
	(printed-prompt nil)
	)



    (block read-loop
      (loop
       (when selection-function
	 (warn "selection function not implemented."))
       
       (setq printed-prompt (format nil "~a " the-prompt))
       (c-prt printed-prompt 0 0)
       
       (when show-mode
	 (item-table-print (get-item-table dungeon player the-place)
			   :show-pause nil))

       ;; add setting of cursor.
       
       (let ((read-char (read-one-character)))

	 (cond ((eq read-char #\*)
		(setq show-mode t))
	       
	       ((eq read-char #\/)
		(when (or (and allow-equip    (eq the-place :backpack))
			  (and allow-backpack (eq the-place :equip)))
		  (setq the-place (if (eq the-place :backpack) :equip :backpack))))
	       
	       ((and allow-floor (eq read-char #\-))
		(select-item dungeon player allow-from
			     :prompt prompt :where :floor))
	       
	       ((alpha-char-p read-char)
		(c-prt "" 0 0)
		(return-from select-item (cons the-place (a2i read-char)))))
	 
	 (c-prt "" 0 0))))
	    
   
    ;; clear prompt
    (c-prt "" 0 0)
    
     nil))
  

(defun use-stair! (dun pl dir)
  "temporary function to say that we're using a stair.."

  (let* ((depth (player.depth pl))
	 (x (player.loc-x pl))
	 (y (player.loc-y pl))
	 (feat (cave-feature dun x y)))
    
    (case dir
      (:up
       (unless (= +feature-less+ feat)
	 (return-from use-stair! nil))
       
       (if (= depth 0)
	   (warn "Cannot go upstairs here")
	   (decf depth)))
      (:down
       (unless (= +feature-more+ feat)
	 (return-from use-stair! nil))
       
       (incf depth))
      (otherwise
       (warn "Unknown direction for stair-use ~a" dir)))

    (bit-flag-add! *redraw* +print-map+ +print-basic+)
    (bit-flag-add! *update* +update-view+)
    
    (setf (player.depth pl) depth)
    (setf (dungeon.level dun) depth))

      
  (setf (player.leaving-p pl) t)
  t)

(defun pick-up-from-floor! (dungeon player)
  "Tries to pick up from floor.  Should be implemented with selection from
a list if more items occupy the same place."
  
  (let* ((x (player.loc-x player))
	 (y (player.loc-y player))
	 (objs (cave-objects dungeon x y)))

    (if (not objs)
	(progn
	  (warn "Nothing on floor.")
	  nil)

	(loop
	 (let ((removed-obj (item-table-remove! objs 0)))
	   (unless removed-obj
	       (return-from pick-up-from-floor! nil))

	   (cond ((obj-is? removed-obj '<money>)
		  ;; are we gold?
		  (incf (player.gold player) (aobj.number removed-obj))
		  ;; redraw left frame
		  (bit-flag-add! *redraw* +print-basic+)
		  
		  (when (= 0 (items.cur-size objs))
		    (setf (cave-objects dungeon x y) nil)))
		 

		 (t
		  (let* ((backpack (aobj.contains (player.inventory player)))
			 (retval (item-table-add! backpack removed-obj)))

		    (if retval
			;; succesful
			(when (= 0 (items.cur-size objs))
			  (setf (cave-objects dungeon x y) nil))
			;; not succesful.. put it back
			(progn
			  (item-table-add! objs removed-obj)
			  (warn "No room in backpack.")
			  (return-from pick-up-from-floor! nil))))))

	   (unless (cave-objects dungeon x y)
	     (return-from pick-up-from-floor! nil))))

	)))


(defun drop-something! (dun pl)
  "Drop some inventory"

    (let ((selection (select-item dun pl '(:backpack :equip)
				  :prompt "Drop item: "
				  :where :backpack)))
      (when selection
	(let* ((the-table (get-item-table dun pl (car selection)))
	       (removed-obj (item-table-remove! the-table (cdr selection))))
	  (when removed-obj
	    (item-table-add! (get-item-table dun pl :floor) removed-obj))))
      ))


(defun wear-something! (dun pl)
  "Puts something in an equipment-slot."

  (let ((selection (select-item dun pl '(:backpack :floor)
				:prompt "Wear item"
				:where :backpack)))

;;    (warn "Selected ~a" selection)
    (when selection
      (let* ((the-table (get-item-table dun pl (car selection)))
	     (removed-obj (item-table-remove! the-table (cdr selection))))
;;	(warn "Removed ~a" removed-obj)
	(when removed-obj
	  (let ((retval (item-table-add! (get-item-table dun pl :equip) removed-obj)))
;;	    (warn "Adding to equip gave: ~a" retval)
	    
		  
	      (cond ((eq retval nil)
		     ;; something screwed up, put object back
		     (item-table-add! the-table removed-obj))
		    
		    ((typep retval 'active-object)
		     ;; an object was returned

		     (let ((back-to-inventory (item-table-add! (get-item-table dun pl :backpack)
							       retval)))
		       (unless back-to-inventory
			 ;; drop to floor
			 (item-table-add! (get-item-table dun pl :floor)
					  retval))))
		    (t
		     ;; succesful and nothing returned.. do nothing
		     ))
		    
	      ))
	  
	  ))))


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


(defun run-dungeon! (pl dun)
  "a loop which runs a dungeon level"

  (let ((dungeon-height (dungeon.height dun))
	(dungeon-width  (dungeon.width dun)))
	
  
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

       (when (player.leaving-p pl)
	 (return-from run-dungeon! nil))
       
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
     
       (incf *turn*)

       ))
    ))



(defun play-game& ()
  "Should not be called directly."
;;  (c-init-angband!)
;;  (c-pause-line 23)

  (warn "playing the damned game")
  
  ;; hack to remove cursor
  (let ((*player* nil)
	(*dungeon* nil))
    
    (c-set-cursor& 0)

;;    (key-test)
    
    (block creation
      (loop
       (let ((the-player (create-character)))
	 (when the-player
	   (setf *player* the-player)
	   (return-from creation))
       
	 (warn "Trying to create player again.."))))

    ;; time to init the stores
    (initialise-stores&) 
    (c-prt "Please wait..." 0 0)  
    (c-pause-line *last-console-line*)
    (clear-the-screen)
    
    (block dungeon-running
      (unless *dungeon* 
	(setf *dungeon* (generate-cave! *player*)))
      
      (loop
       ;; clean up to prevent too many delays while running the dungeon
       #+cmu
       (ext:gc)
       ;; let's run this dungeon
       (run-dungeon! *player* *dungeon*)
       
       ;; return if we're toast
       (when (player.dead-p *player*)
	 (return-from dungeon-running))
       
       ;; generate new cave, try to use the existing
       (setf *dungeon* (generate-cave! *player* *dungeon*))))
    
    
    (c-prt "Quitting..." 0 0)  
    (c-pause-line *last-console-line*)
    (c-quit! +c-null-value+)))

;; low-level definitions
#+allegro
(ff:defun-foreign-callable c-callable-play ()
  (play-game&))

#+allegro
(ff:def-foreign-call set_lisp_callback (address))

