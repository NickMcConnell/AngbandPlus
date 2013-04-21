;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

(in-package :langband)

(defvar *ang-keys* (define-key-table "angband"))

(setf *current-key-table* *ang-keys*)

(define-key-operation 'move-up
    #'(lambda (dun pl) (move-player! dun pl 8)))

(define-key-operation 'move-right
    #'(lambda (dun pl) (move-player! dun pl 6)))

(define-key-operation 'move-left
    #'(lambda (dun pl) (move-player! dun pl 4)))

(define-key-operation 'move-down
    #'(lambda (dun pl) (move-player! dun pl 2)))

(define-key-operation 'show-equipment
     #'(lambda (dun pl)
	 (declare (ignore dun))
	 (let ((table (player.eq pl)))
	   (item-table-print table :show-pause t))))

(define-key-operation 'show-inventory
    #'(lambda (dun pl)
	(declare (ignore dun))
	(let* ((backpack (player.inventory pl))
	       (inventory (aobj.contains backpack)))
	  (item-table-print inventory :show-pause t))))

(define-key-operation 'show-character
    #'(lambda (dun pl)
	(declare (ignore dun))
	(display-player pl)
	(c-pause-line *last-console-line*)
	(bit-flag-add! *redraw* +print-map+ +print-basic+)
	(clear-the-screen)))

(define-key-operation 'go-downstairs
    #'(lambda (dun pl) (use-stair! dun pl :down)))

(define-key-operation 'go-upstairs
    #'(lambda (dun pl) (use-stair! dun pl :up)))

(define-key-operation 'quit-game
    #'(lambda (dun pl)
	(declare (ignore pl dun))
	(warn "Quitting")
	(c-quit! +c-null-value+)))

(define-key-operation 'get-item
    #'(lambda (dun pl) (pick-up-from-floor! dun pl)))

(define-key-operation 'drop-item
    #'(lambda (dun pl) (drop-something! dun pl)))

(define-key-operation 'wear-item
    #'(lambda (dun pl) (wear-something! dun pl)))

;; hackish
(define-key-operation 'open-all
    #'(lambda (dun pl) (open-all! dun pl)))

(define-keypress *ang-keys* :global #\k 'move-up)
(define-keypress *ang-keys* :global #\l 'move-right)
(define-keypress *ang-keys* :global #\j 'move-down)
(define-keypress *ang-keys* :global #\h 'move-left)

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


;; redefine these later
#||
(define-keypress :global #\S #\S
		 #'(lambda (dun pl)
		     (declare (ignore pl dun)) (show-store 6)))
(define-keypress :global #\F #\F
		 #'(lambda (dun pl)
		     (declare (ignore pl)) (print-map-to-file dun "dumps/map.ascii")))
(define-keypress :global #\P #\P
		 #'(lambda (dun pl)
		     (declare (ignore pl)) (print-map-as-ppm dun "dumps/map.ppm")))
||#

#||
;; does not work
(define-keypress :global #\8 #\8 #'(lambda (dun pl) (move-viewport! dun pl 8)))
(define-keypress :global #\6 #\6 #'(lambda (dun pl) (move-viewport! dun pl 6)))
(define-keypress :global #\2 #\2 #'(lambda (dun pl) (move-viewport! dun pl 2)))
(define-keypress :global #\4 #\4 #'(lambda (dun pl) (move-viewport! dun pl 4)))
||#
