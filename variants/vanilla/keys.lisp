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

(define-key-operation 'move-up
    #'(lambda (dun pl) (move-player! dun pl 8)))

(define-key-operation 'move-up-left
    #'(lambda (dun pl) (move-player! dun pl 7)))

(define-key-operation 'move-up-right
    #'(lambda (dun pl) (move-player! dun pl 9)))

(define-key-operation 'move-right
    #'(lambda (dun pl) (move-player! dun pl 6)))

(define-key-operation 'move-left
    #'(lambda (dun pl) (move-player! dun pl 4)))

(define-key-operation 'move-down
    #'(lambda (dun pl) (move-player! dun pl 2)))

(define-key-operation 'move-down-left
    #'(lambda (dun pl) (move-player! dun pl 1)))

(define-key-operation 'move-down-right
    #'(lambda (dun pl) (move-player! dun pl 3)))

(define-key-operation 'stand-still
    #'(lambda (dun pl) (move-player! dun pl 5)))


(define-key-operation 'show-equipment
     #'(lambda (dun pl)
	 (declare (ignore dun))
	 (with-new-screen ()
	   (let ((table (player.eq pl)))
	     (item-table-print table :show-pause t)))
	 ))

(define-key-operation 'show-inventory
    #'(lambda (dun pl)
	(declare (ignore dun))
	(with-new-screen ()
	  (let* ((backpack (player.inventory pl))
		 (inventory (aobj.contains backpack)))
	    (item-table-print inventory :show-pause t)))
	))
  
(define-key-operation 'show-character
    #'(lambda (dun pl)
	(with-new-screen ()
	  (block display-input 
	    (let ((loc-table (gethash :display *current-key-table*)))
	      (loop
	       (c-clear-from! 0)
	       (display-creature *variant* pl)
	       (c-prt! "['C' to show combat-info, 'R' to show resists,  ESC to continue]"
		       *last-console-line* 5)

	       (let* ((ch (read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(funcall fun dun pl))
		       ((eql ch +escape+)
			(return-from display-input t))
		       (t
			;; nil
			)))
	       )))
	  )))


(define-key-operation 'go-downstairs
    #'(lambda (dun pl) (use-stair! dun pl :down)))

(define-key-operation 'go-upstairs
    #'(lambda (dun pl) (use-stair! dun pl :up)))

(define-key-operation 'quit-game
    #'(lambda (dun pl)
	(declare (ignore dun))
;;	(warn "Quitting")
	(c-prt! "Are you sure you wish to quit? " 0 0)
	(let ((chr (read-one-character)))
	  (when (or (equal chr #\y)
		    (equal chr #\Y))
	    (setf (player.dead-p pl) t
		  (player.dead-from pl) "quitting"
		  (player.leaving-p pl) :quit)
;;	(c-quit! +c-null-value+) ;; how to quit cleanly to the REPL?
	
	    ))
	))

(define-key-operation 'get-item
    #'(lambda (dun pl)
;;	(with-new-screen ()
	  (pick-up-from-floor! dun pl)))
  

(define-key-operation 'drop-item
    #'(lambda (dun pl)
	(with-new-screen ()
	  (drop-something! dun pl))))

(define-key-operation 'wear-item
    #'(lambda (dun pl)
	(with-new-screen ()
	  (wear-something! dun pl))))

(define-key-operation 'use-item
    #'(lambda (dun pl)
	(with-new-screen ()
	  (use-something! dun pl))))

(define-key-operation 'quaff-potion
    #'(lambda (dun pl)
	(with-new-screen ()
	  (use-something! dun pl :restrict-type '(<potion>)
			  :which-use :quaff
			  :limit-from '(:backpack :floor) ;; only place with potions
			  :prompt "Quaff which potion?")
	  )))

(define-key-operation 'read-text
    #'(lambda (dun pl)
	(with-new-screen ()
	  (use-something! dun pl :restrict-type '(<scroll>)
			  :limit-from '(:backpack :floor) ;; only place with scrolls
			  :which-use :read
			  :prompt "Read which scroll?")
	  )))

(define-key-operation 'eat-something
    #'(lambda (dun pl)
	(with-new-screen ()
	  (let ((retval (use-something! dun pl :restrict-type '(<food>)
					:limit-from '(:backpack :floor) ;; only place with food
					:which-use :eat
					:prompt "Eat what?")))
	    ;;(warn "Used ~s" retval)
	    retval)
	  )))

(define-key-operation 'invoke-spell
    #'(lambda (dun pl)
	(invoke-spell! dun pl)))

;; hackish
(define-key-operation 'open-all
    #'(lambda (dun pl) (open-all! dun pl)))

(define-key-operation 'search-area
    #'(lambda (dun pl) (search-area! dun pl)))


;; unused
(define-key-operation 'print-mapper
    #'(lambda (dun pl)
	(print-map dun pl)))



(define-key-operation 'save-game
    #'(lambda (dun pl)
	(declare (ignore dun))
	(when-bind (func (get-late-bind-function 'langband 'save-the-game))
	  (let ((home-path  (home-langband-path)))
	    (lbsys/make-sure-dirs-exist& home-path)
	    (funcall func *variant* pl *level*
		     :fname (concatenate 'string home-path +readable-save-file+)
		     :format :readable)
	    (funcall func *variant* pl *level*
		     :fname (concatenate 'string home-path +binary-save-file+)
		     :format :binary))
	  (c-print-message! "Your game was saved [binary+source]")
	  )))



(define-key-operation 'show-help
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(with-new-screen ()
	  (c-clear-from! 0)
	  (display-help-topics *variant* "LAangband help (Vanilla)" 3)

;;	  (c-pause-line *last-console-line*)
	  )))




(define-key-operation 'fire-missile
    #'(lambda (dun pl)
	(interactive-fire-a-missile dun pl)))


(define-key-operation 'print-attack-table
    #'(lambda (dun pl)
	(declare (ignore dun))
;;	(with-new-screen ()
	  (print-attack-table *variant* pl)
	  (print-attack-graph *variant* pl)
	))

(define-key-operation 'print-resists
    #'(lambda (dun pl)
	(declare (ignore dun))
	(print-resists *variant* pl)
	))

(define-key-operation 'print-misc
    #'(lambda (dun pl)
	(declare (ignore dun))
	(print-misc-info *variant* pl)
	))


(define-keypress *ang-keys* :global #\d 'drop-item)
(define-keypress *ang-keys* :global #\e 'show-equipment)
(define-keypress *ang-keys* :global #\f 'fire-missile)
(define-keypress *ang-keys* :global #\g 'get-item)
(define-keypress *ang-keys* :global #\i 'show-inventory)
(define-keypress *ang-keys* :global #\m 'invoke-spell)
(define-keypress *ang-keys* :global #\o 'open-all)
(define-keypress *ang-keys* :global #\p 'invoke-spell)
(define-keypress *ang-keys* :global #\q 'quaff-potion)
(define-keypress *ang-keys* :global #\r 'read-text)
(define-keypress *ang-keys* :global #\s 'search-area)
(define-keypress *ang-keys* :global #\u 'use-item)
(define-keypress *ang-keys* :global #\w 'wear-item)

(define-keypress *ang-keys* :global #\C 'show-character)
(define-keypress *ang-keys* :global #\E 'eat-something)
(define-keypress *ang-keys* :global #\Q 'quit-game)
(define-keypress *ang-keys* :global #\S 'save-game)
(define-keypress *ang-keys* :global #\? 'show-help)

(define-keypress *ang-keys* :global #\> 'go-downstairs)
(define-keypress *ang-keys* :global #\< 'go-upstairs)

;; these can die later..
;;(define-keypress *ang-keys* :global #\A 'print-mapper)



(define-keypress *ang-keys* :global #\. 'stand-still)
(define-keypress *ang-keys* :global #\1 'move-down-left)
(define-keypress *ang-keys* :global #\2 'move-down)
(define-keypress *ang-keys* :global #\3 'move-down-right)
(define-keypress *ang-keys* :global #\4 'move-left)
(define-keypress *ang-keys* :global #\5 'stand-still)
(define-keypress *ang-keys* :global #\6 'move-right)
(define-keypress *ang-keys* :global #\7 'move-up-left)
(define-keypress *ang-keys* :global #\8 'move-up)
(define-keypress *ang-keys* :global #\9 'move-up-right)

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
