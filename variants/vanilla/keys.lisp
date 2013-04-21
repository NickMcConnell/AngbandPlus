;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/keys.lisp - assignment of keys
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(setf *current-key-table* *ang-keys*)

;; move somehwere else later?
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

(defvar *run-mode* nil "Are we running?")

(define-key-operation 'toggle-run-mode
    #'(lambda (dungeon player)
	(setf *run-mode* (not *run-mode*))))

(defun let-player-run! (dungeon player direction)
  (warn "run to ~s gave ~s" direction (run-along-corridor dungeon player direction)))

(defun van-move-player! (dungeon player direction)
  (if *run-mode*
      (let-player-run! dungeon player direction)
      (move-player! dungeon player direction))
  (setf *run-mode* nil))

(define-key-operation 'move-up
    #'(lambda (dungeon player) (van-move-player! dungeon player 8)))

(define-key-operation 'move-up-left
    #'(lambda (dungeon player) (van-move-player! dungeon player 7)))

(define-key-operation 'move-up-right
    #'(lambda (dungeon player) (van-move-player! dungeon player 9)))

(define-key-operation 'move-right
    #'(lambda (dungeon player) (van-move-player! dungeon player 6)))

(define-key-operation 'move-left
    #'(lambda (dungeon player) (van-move-player! dungeon player 4)))

(define-key-operation 'move-down
    #'(lambda (dungeon player) (van-move-player! dungeon player 2)))

(define-key-operation 'move-down-left
    #'(lambda (dungeon player) (van-move-player! dungeon player 1)))

(define-key-operation 'move-down-right
    #'(lambda (dungeon player) (van-move-player! dungeon player 3)))

(define-key-operation 'stand-still
    #'(lambda (dungeon player) (van-move-player! dungeon player 5)))


(define-key-operation 'show-equipment
     #'(lambda (dungeon player)
	 (declare (ignore dungeon))
	 (with-dialogue ()
	   (c-clear-from! 0) ;; hack
	   (let ((table (player.equipment player)))
	     (item-table-print table :show-pause t :start-x 3))
	   (c-term-fresh! +dialogue-frame+))
	 ))

(define-key-operation 'show-inventory
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(with-dialogue ()
	  (c-clear-from! 0) ;; hack
	  (let* ((backpack (player.inventory player))
		 (inventory (aobj.contains backpack)))
	    (item-table-print inventory :show-pause t :start-x 3))
	  (c-term-fresh! +dialogue-frame+))
	))
  
(define-key-operation 'show-character
    #'(lambda (dungeon player)
	(flush-messages! t)
	(with-full-frame ()
	  (c-texture-background! +full-frame+ "plainbook.png" -1)
	  (c-term-clear!)
	  (block display-input 
	    (let ((loc-table (gethash :display *current-key-table*)))
	      (loop
	       (c-clear-from! 0)
	       (display-creature *variant* player)
	       ;;(c-prt! "['C' to show combat-info, 'R' to show resists,  ESC to continue]"
	       ;;5 (get-last-console-line))
	       (print-note! "['C' to show combat-info, 'R' to show resists,  ESC to continue]")
	       
	       (let* ((ch (read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(funcall fun dungeon player))
		       ((eql ch +escape+)
			(return-from display-input t))
		       (t
			;; nil
			)))
	       )))
	  (c-texture-background! +full-frame+ "" -1)
	  (c-wipe-frame! +full-frame+)

	  )))


(define-key-operation 'go-downstairs
    #'(lambda (dungeon player) (use-stair! dungeon player :down)))

(define-key-operation 'go-upstairs
    #'(lambda (dungeon player) (use-stair! dungeon player :up)))

(define-key-operation 'quit-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
;;	(warn "Quitting")
	(with-frame (+query-frame+)
	  (c-prt! "Are you sure you wish to quit? " 0 0)
	  (let ((chr (read-one-character)))
	    (when (or (equal chr #\y)
		      (equal chr #\Y))
	      (setf (player.dead-p player) t
		    (player.dead-from player) "quitting"
		    (player.leaving-p player) :quit)
;;	(c-quit! +c-null-value+) ;; how to quit cleanly to the REPL?
	      
	    )))
	))

(define-key-operation 'get-item
    #'(lambda (dungeon player)
;;	(with-new-screen ()
	(pick-up-from-floor! dungeon player)))
  

(define-key-operation 'drop-item
    #'(lambda (dungeon player)
	(interactive-drop-item! dungeon player)
	))

(define-key-operation 'take-off-item
    #'(lambda (dungeon player)
	(interactive-take-off-item! dungeon player)
	))


(define-key-operation 'wear-item
    #'(lambda (dungeon player)
	(interactive-wear-item! dungeon player)))

(define-key-operation 'use-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player)))

(define-key-operation 'quaff-potion
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:quaff)
			       :which-use :quaff
			       :limit-from '(:backpack :floor) ;; only place with potions
			       :prompt "Quaff which potion?")))

(define-key-operation 'zap-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:zap)
			       :which-use :zap
			       :limit-from '(:backpack :floor) ;; only place with zappers I think
			       :prompt "Zap which stick?")))
	


(define-key-operation 'read-text
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:read)
			       :limit-from '(:backpack :floor) ;; only place with scrolls
			       :which-use :read
			       :prompt "Read which scroll?")))

(define-key-operation 'eat-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:eat)
			       :limit-from '(:backpack :floor) ;; only place with food
			       :which-use :eat
			       :sound +sound-eat+
			       :prompt "Eat what?")))

(define-key-operation 'invoke-spell
    #'(lambda (dungeon player)
	(van-invoke-spell! dungeon player)))


(define-key-operation 'open-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :open)))

(define-key-operation 'close-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :close)))

(define-key-operation 'bash-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :bash)))

(define-key-operation 'jam-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :jam)))

(define-key-operation 'search-area
    #'(lambda (dungeon player) (search-area! dungeon player)))

(define-key-operation 'disarm-trap
    #'(lambda (dungeon player)
	(interactive-trap-operation! dungeon player :disarm)))

(define-key-operation 'select-target
    #'(lambda (dungeon player)
	(interactive-targeting! dungeon player)))


;; unused
(define-key-operation 'print-mapper
    #'(lambda (dungeon player)
	(print-map dungeon player)))



(define-key-operation 'save-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(when-bind (func (get-late-bind-function 'langband 'save-the-game))
	  (let ((home-path  (home-langband-path)))
	    (lbsys/make-sure-dirs-exist& home-path)
	    (funcall func *variant* player *level*
		     :fname (concatenate 'string home-path *readable-save-file*)
		     :format :readable)
	    (funcall func *variant* player *level*
		     :fname (concatenate 'string home-path *binary-save-file*)
		     :format :binary))
	  (print-message! "Your game was saved [binary+source]")
	  )))



(define-key-operation 'show-help
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-dialogue ()
	  (c-clear-from! 0)
	  (display-help-topics *variant* "LAangband help (Vanilla)" 3)

	  )))


(define-key-operation 'learn-spell
    #'(lambda (dungeon player)
	(van-learn-spell! dungeon player)))

(define-key-operation 'browse-spells
    #'(lambda (dungeon player)
	(browse-spells dungeon player)))


(define-key-operation 'fire-missile
    #'(lambda (dungeon player)
	(interactive-fire-a-missile dungeon player)))

(define-key-operation 'previous-messages
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-new-screen ()
	  (show-messages :offset 0))))


(define-key-operation 'print-attack-table
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
;;	(with-new-screen ()
	  (print-attack-table *variant* player)
	  (print-attack-graph *variant* player)
	))

(define-key-operation 'print-resists
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let ((var *variant*))
	  (print-resists var player (get-setting var :resists-display)))
	))

(define-key-operation 'print-misc
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(print-misc-info *variant* player)
	))

(define-key-operation 'redraw-all
    #'(lambda (dungeon player)

	;; skip flushes
	;; do xtra react
	(org.langband.ffi:c-term-xtra& 10 0) ;; xtra_react
	;; skip combine & reorder

	(bit-flag-add! *update* #.(logior +pl-upd-torch+
					  +pl-upd-forget-view+
					  +pl-upd-update-view+ 
					  +pl-upd-monsters+))
	(bit-flag-add! *redraw* #.(logior +print-extra+
					  +print-basic+
					  +print-map+
					  ;; skip equippy
					  ))
	;; do clear
	(c-term-clear!)
	 
	;; call handle-stuff
	(handle-stuff *variant* dungeon player)
	
	;; do all windows, we might need this

	(c-term-fresh!)
	
	))

(define-key-operation 'swap-map
    #'(lambda (dungeon player)
	(when (eq (get-system-type) 'sdl)
	  (switch-map-mode))))

(define-keypress *ang-keys* :global #\a 'zap-item)
(define-keypress *ang-keys* :global #\b 'browse-spells)
(define-keypress *ang-keys* :global #\c 'close-door)
(define-keypress *ang-keys* :global #\d 'drop-item)
(define-keypress *ang-keys* :global #\e 'show-equipment)
(define-keypress *ang-keys* :global #\f 'fire-missile)
(define-keypress *ang-keys* :global #\g 'get-item)
(define-keypress *ang-keys* :global #\i 'show-inventory)
(define-keypress *ang-keys* :global #\j 'jam-door)
(define-keypress *ang-keys* :global #\m 'invoke-spell)
(define-keypress *ang-keys* :global #\o 'open-door)
(define-keypress *ang-keys* :global #\p 'invoke-spell)
(define-keypress *ang-keys* :global #\q 'quaff-potion)
(define-keypress *ang-keys* :global #\r 'read-text)
(define-keypress *ang-keys* :global #\s 'search-area)
(define-keypress *ang-keys* :global #\t 'take-off-item)
(define-keypress *ang-keys* :global #\u 'use-item)
(define-keypress *ang-keys* :global #\w 'wear-item)
(define-keypress *ang-keys* :global #\z 'zap-item)

(define-keypress *ang-keys* :global #\B 'bash-door)
(define-keypress *ang-keys* :global #\C 'show-character)
(define-keypress *ang-keys* :global #\D 'disarm-trap)
(define-keypress *ang-keys* :global #\E 'eat-item)
(define-keypress *ang-keys* :global #\L 'learn-spell)
(define-keypress *ang-keys* :global #\Q 'quit-game)
(define-keypress *ang-keys* :global #\S 'save-game)
(define-keypress *ang-keys* :global #\? 'show-help)

(define-keypress *ang-keys* :global #\> 'go-downstairs)
(define-keypress *ang-keys* :global #\< 'go-upstairs)
(define-keypress *ang-keys* :global #\* 'select-target)

;; these can die later..
;;(define-keypress *ang-keys* :global #\A 'print-mapper)

;; Ctrl-P
(define-keypress *ang-keys* :global (code-char 16) 'previous-messages)
;; Ctrl-R
(define-keypress *ang-keys* :global (code-char 18) 'redraw-all)
(define-keypress *ang-keys* :global (code-char 20) 'swap-map)



(define-keypress *ang-keys* :global #\, 'stand-still)
(define-keypress *ang-keys* :global #\1 'move-down-left)
(define-keypress *ang-keys* :global #\2 'move-down)
(define-keypress *ang-keys* :global #\3 'move-down-right)
(define-keypress *ang-keys* :global #\4 'move-left)
(define-keypress *ang-keys* :global #\5 'stand-still)
(define-keypress *ang-keys* :global #\6 'move-right)
(define-keypress *ang-keys* :global #\7 'move-up-left)
(define-keypress *ang-keys* :global #\8 'move-up)
(define-keypress *ang-keys* :global #\9 'move-up-right)

(define-keypress *ang-keys* :global #\. 'toggle-run-mode)

;; then those keys used for display
(define-keypress *ang-keys* :display #\C 'print-attack-table)
(define-keypress *ang-keys* :display #\M 'print-misc)
(define-keypress *ang-keys* :display #\R 'print-resists)


#||
(define-keypress *ang-keys* :global #\k 'move-up)
(define-keypress *ang-keys* :global #\l 'move-right)
(define-keypress *ang-keys* :global #\j 'move-down)
(define-keypress *ang-keys* :global #\h 'move-left)
||#
