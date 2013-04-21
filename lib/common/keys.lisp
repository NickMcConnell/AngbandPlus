;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/common/keys.lisp - assignment of keys
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(defvar *ang-keys* (define-key-table "angband"))

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
	(display-player pl)
	(c-pause-line! *last-console-line*)
	(bit-flag-add! *redraw* +print-map+ +print-basic+)
	(clear-the-screen!)))

(define-key-operation 'go-downstairs
    #'(lambda (dun pl) (use-stair! dun pl :down)))

(define-key-operation 'go-upstairs
    #'(lambda (dun pl) (use-stair! dun pl :up)))

(define-key-operation 'quit-game
    #'(lambda (dun pl)
	(declare (ignore pl dun))
;;	(warn "Quitting")
	(c-quit! +c-null-value+)))

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

;; hackish
(define-key-operation 'open-all
    #'(lambda (dun pl) (open-all! dun pl)))

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
	  (let ((func (get-late-bind-function 'lb-test '%loc-save-test)))
	    (when func
	      (funcall func lb::*variant* :variant)
	      (funcall func lb::*level* :level)
	      (funcall func lb::*player* :player)))
	  ||#

	))

(define-key-operation 'save-game
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(let ((func (get-late-bind-function 'langband 'save-the-game)))
	  (when func
	    (funcall func *variant* *player* *level* :fname +readable-save-file+ :format :readable)
	    (funcall func *variant* *player* *level* :fname +binary-save-file+ :format :binary)
	    )
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



(define-keypress *ang-keys* :global #\z 'halt-program)
(define-keypress *ang-keys* :global #\S 'save-game)
(define-keypress *ang-keys* :global #\Z 'in-game-test)
(define-keypress *ang-keys* :global #\I 'inspect-coord)
(define-keypress *ang-keys* :global #\K 'print-keys) 

#||
(define-keypress *ang-keys* :global #\k 'move-up)
(define-keypress *ang-keys* :global #\l 'move-right)
(define-keypress *ang-keys* :global #\j 'move-down)
(define-keypress *ang-keys* :global #\h 'move-left)
||#

(define-keypress *ang-keys* :global #\9 'move-up-right)
(define-keypress *ang-keys* :global #\8 'move-up)
(define-keypress *ang-keys* :global #\7 'move-up-left)
(define-keypress *ang-keys* :global #\6 'move-right)
(define-keypress *ang-keys* :global #\5 'stand-still)
(define-keypress *ang-keys* :global #\4 'move-left)
(define-keypress *ang-keys* :global #\3 'move-down-right)
(define-keypress *ang-keys* :global #\2 'move-down)
(define-keypress *ang-keys* :global #\1 'move-down-left)
(define-keypress *ang-keys* :global #\. 'stand-still)

(define-keypress *ang-keys* :global #\e 'show-equipment)
(define-keypress *ang-keys* :global #\i 'show-inventory)
(define-keypress *ang-keys* :global #\C 'show-character)

(define-keypress *ang-keys* :global #\> 'go-downstairs)
(define-keypress *ang-keys* :global #\< 'go-upstairs)

(define-keypress *ang-keys* :global #\g 'get-item)
(define-keypress *ang-keys* :global #\d 'drop-item)
(define-keypress *ang-keys* :global #\w 'wear-item)
(define-keypress *ang-keys* :global #\o 'open-all)

(define-keypress *ang-keys* :global #\Q 'quit-game)


(define-keypress *ang-keys* :global #\p 'print-map)
(define-keypress *ang-keys* :global #\P 'print-map-as-ppm)

