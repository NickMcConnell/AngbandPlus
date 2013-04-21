;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variants/vanilla/keys.lisp - assignment of keys
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


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
	(declare (ignore dun))
	(with-new-screen ()
	  (display-creature *variant* pl)
	  (c-pause-line! *last-console-line*))))

(define-key-operation 'go-downstairs
    #'(lambda (dun pl) (use-stair! dun pl :down)))

(define-key-operation 'go-upstairs
    #'(lambda (dun pl) (use-stair! dun pl :up)))

(define-key-operation 'quit-game
    #'(lambda (dun pl)
	(declare (ignore dun))
;;	(warn "Quitting")
	(setf (player.dead-p pl) t
	      (player.dead-from pl) "quitting"
	      (player.leaving-p pl) :quit)
;;	(c-quit! +c-null-value+) ;; how to quit cleanly to the REPL?
	
	))

(define-key-operation 'get-item
    #'(lambda (dun pl)
	(with-new-screen ()
	  (pick-up-from-floor! dun pl))))

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

(define-key-operation 'print-mapper
    #'(lambda (dun pl)
	(print-map dun pl)))

(define-key-operation 'print-map
    #'(lambda (dun pl)
	(declare (ignore pl))
	(print-map-to-file dun "./map.ascii")))

(define-key-operation 'print-map-as-ppm
    #'(lambda (dun pl)
	(declare (ignore pl))
	(print-map-as-ppm dun "./map.ppm")))

(define-key-operation 'halt-program
    #'(lambda (dun pl)
	(declare (ignore dun))
	(assert (eq pl nil))))

(define-key-operation 'in-game-test
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	;; temporary place
	#+xp-testing
	(do-a-test :in)
	#||
	  ;; for those times when things crash
	  (when-bind (func (get-late-bind-function 'lb-test '%loc-save-test))
	      (funcall func lb::*variant* :variant)
	      (funcall func lb::*level* :level)
	      (funcall func lb::*player* :player)))
	  ||#

	))

(define-key-operation 'save-game
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(when-bind (func (get-late-bind-function 'langband 'save-the-game))
	  (funcall func *variant* *player* *level* :fname +readable-save-file+ :format :readable)
	  (funcall func *variant* *player* *level* :fname +binary-save-file+ :format :binary)
	  )))



(define-key-operation 'inspect-coord
    #'(lambda (dun pl)
	(let* ((cur-x (location-x pl))
	       (cur-y (location-y pl))
	       (coord-obj (cave-coord dun cur-x cur-y)))
	  (warn "Describing [~a,~a]" cur-x cur-y)
	  (describe coord-obj)
	  (multiple-value-bind (the-attr the-char)
	      (map-info dun cur-x cur-y)
	    (warn "Mapped to (~s . ~s)" the-attr the-char)))))

(define-key-operation 'print-keys
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(print-key-table (gethash :global *ang-keys*)
			 "table.keys")))

(define-key-operation 'wamp-monsters
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(dump-features "dumps/feat.list")
	(dump-monsters "dumps/mon.list" :monster-list (get-all-monsters))
	(dump-objects "dumps/obj.list")
	))

(define-key-operation 'fire-something
    #'(lambda (dun pl)
	(temp-shoot-an-arrow dun pl)))

(define-key-operation 'projecteur
    #'(lambda (dun pl)
	(project-hack dun pl)))

(define-key-operation 'break-game
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(break)))

(define-keypress *ang-keys* :global #\d 'drop-item)
(define-keypress *ang-keys* :global #\e 'show-equipment)
(define-keypress *ang-keys* :global #\f 'fire-something) ;; hackish still
(define-keypress *ang-keys* :global #\g 'get-item)
(define-keypress *ang-keys* :global #\i 'show-inventory)
(define-keypress *ang-keys* :global #\m 'invoke-spell)
(define-keypress *ang-keys* :global #\o 'open-all)
(define-keypress *ang-keys* :global #\p 'invoke-spell)
(define-keypress *ang-keys* :global #\q 'quaff-potion)
(define-keypress *ang-keys* :global #\r 'read-text)
(define-keypress *ang-keys* :global #\u 'use-item)
(define-keypress *ang-keys* :global #\w 'wear-item)
(define-keypress *ang-keys* :global #\z 'halt-program)

(define-keypress *ang-keys* :global #\C 'show-character)
(define-keypress *ang-keys* :global #\E 'eat-something)
(define-keypress *ang-keys* :global #\I 'inspect-coord)
(define-keypress *ang-keys* :global #\K 'print-keys) 
(define-keypress *ang-keys* :global #\Q 'quit-game)
(define-keypress *ang-keys* :global #\S 'save-game)
(define-keypress *ang-keys* :global #\Z 'in-game-test)

(define-keypress *ang-keys* :global #\> 'go-downstairs)
(define-keypress *ang-keys* :global #\< 'go-upstairs)

;; these can die later..
(define-keypress *ang-keys* :global #\T 'print-map)
(define-keypress *ang-keys* :global #\P 'print-map-as-ppm)
(define-keypress *ang-keys* :global #\A 'print-mapper)
(define-keypress *ang-keys* :global #\F 'wamp-monsters)
(define-keypress *ang-keys* :global #\B 'break-game)
(define-keypress *ang-keys* :global #\Y 'projecteur)


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

#||
(define-keypress *ang-keys* :global #\k 'move-up)
(define-keypress *ang-keys* :global #\l 'move-right)
(define-keypress *ang-keys* :global #\j 'move-down)
(define-keypress *ang-keys* :global #\h 'move-left)
||#
