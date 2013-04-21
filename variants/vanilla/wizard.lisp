;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variants/vanilla/wizard.lisp - wizard-commands
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(setf *current-key-table* *ang-keys*)

(define-key-operation 'break-game
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(break)))

(define-key-operation 'summon
    #'(lambda (dun pl)
	(let* ((summon (get-string-input "Monster to summon: "))
	       (mon (if summon (produce-active-monster *variant* summon))))
	  (when mon
	    (block mon-placement
	      (let ((px (location-x pl))
		    (py (location-y pl)))
		(flet ((put-mon (x y)
			 (when (cave-floor-bold? dun x y)
			   (place-single-monster! dun pl mon x y nil)
			   (light-spot! dun x y)
			   (return-from mon-placement nil))))
		  (put-mon (1+ px) py)
		  (put-mon px (1+ py))
		  (put-mon (1+ px) (1+ py)))))
	    mon))))

(define-key-operation 'set-gold
    #'(lambda (dun pl)
	(declare (ignore dun))
	(let* ((str-amount (get-string-input "Gold-amount: "))
	       (amount (ignore-errors (parse-integer str-amount))))
	  (when (and (integerp amount) (plusp amount))
	    (setf (player.gold pl) amount)
	    (bit-flag-add! *redraw* +print-gold+))
	  )))

(define-key-operation 'go-to-depth
    #'(lambda (dun pl)

	(let* ((which-depth (get-string-input "Depth: "))
	       (depth (ignore-errors (parse-integer which-depth))))
	  (when (and (integerp depth) (plusp depth))
	    (setf (player.depth pl) depth
		  (dungeon.depth dun) depth)
		  
	    (when (> depth (player.max-depth pl))
	      (setf (player.max-depth pl) depth))

	    (setf (player.leaving-p pl) :teleport)
	    t))
	))

    

(define-key-operation 'print-odd-info
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(let ((var-obj *variant*)
	      (fname "dumps/odd.info")
	      (*print-case* :downcase))
	  (with-open-file (s (pathname fname)
			     :direction :output 
			     :if-exists :supersede
			     :if-does-not-exist :create)
	    (let ((effects (variant.effects var-obj)))
	      (dolist (i (reverse effects))
		(format s "~&Effect ~d: ~a - ~a" (effect.index i) (effect.name i) (effect.symbol i))))

	    (format s "~2%")
	    
	    (let ((elements (variant.elements var-obj)))
	      (dolist (i (reverse elements))
		(format s "~&Element ~d: ~a - ~a" (element.index i) (element.name i) (element.symbol i))))

	    (format s "~2%")
	    
	    (loop for i from 5 to 200 by 10
		  do
		  (format s "~&~a% chance to hit armour ~a (~a)~%"
			  (get-chance var-obj 80 i)
			  i
			  (get-armour-desc var-obj i)))
	    
	    )
	  (c-print-message! "Odd info was dumped to dumps/odd.info")
	  )))

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

(define-key-operation 'print-map
    #'(lambda (dun pl)
	(declare (ignore pl))
	(print-map-to-file dun "./map.ascii")
	(c-print-message! "Map printed to map.ascii.")
	))


(define-key-operation 'print-map-as-ppm
    #'(lambda (dun pl)
	(declare (ignore pl))
	(print-map-as-ppm dun "./map.ppm")
	(c-print-message! "Map printed to map.ppm.")
	))


(define-key-operation 'wizard-menu
    #'(lambda (dun pl)

;;	(with-new-screen ()
	  (block wizard-input 
	    (let ((loc-table (gethash :wizard *current-key-table*)))
	      (loop
;;	       (c-clear-from! 0)
;;	       (display-creature *variant* pl)
	       
	       (c-prt! "Wizard command: " 0 0)

	       (let* ((ch (read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(return-from wizard-input (funcall fun dun pl)))
		       ((eql ch +escape+)
			(return-from wizard-input t))
		       (t
			;; nil
			)))
	       )))
	  ))

;;(define-keypress *ang-keys* :wizard #\Y 'projecteur)


;; Ctrl-A
(define-keypress *ang-keys* :global (code-char 1) 'wizard-menu)



;; wizard stuff

(define-keypress *ang-keys* :wizard #\B 'break-game)
(define-keypress *ang-keys* :wizard #\D 'go-to-depth)
(define-keypress *ang-keys* :wizard #\F 'wamp-monsters)
(define-keypress *ang-keys* :wizard #\G 'set-gold)
(define-keypress *ang-keys* :wizard #\I 'inspect-coord)
(define-keypress *ang-keys* :wizard #\K 'print-keys) 
(define-keypress *ang-keys* :wizard #\P 'print-map-as-ppm)
(define-keypress *ang-keys* :wizard #\T 'print-map)
(define-keypress *ang-keys* :wizard #\U 'summon)
(define-keypress *ang-keys* :wizard #\W 'print-odd-info)
(define-keypress *ang-keys* :wizard #\Z 'in-game-test)

;; obsolete
