;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/wizard.lisp - wizard-commands
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(setf *current-key-table* *ang-keys*)

(defun van-obj-printer (file obj)
  (let ((var-obj *variant*))
    (print (lb-engine:get-loadable-form var-obj obj) file))
  (terpri file))


(defun print-key-table (table fname)
  "Prints a key-table to the given file."
  
  (with-open-file (s (pathname fname)
                     :direction :output 
                     :if-exists :supersede)
    (let ((collected nil))
      (maphash #'(lambda (k v)
		   (push (cons k v) collected))

	       table)
      ;; hackish
      (let ((key-ops (get-key-operations)))
	(dolist (i key-ops)
	  (dolist (j collected)
	    (when (eq (cdr i) (cdr j))
	      (setf (cdr j) (car i))))))
      
      (let ((sorted (sort (mapcar #'(lambda (k)
				      (format nil "key ~a -> ~a" (car k) (cdr k)))
				  collected)
			  #'string-lessp)))
	(dolist (i sorted)
	  (format s "~a~%" i))))))

(defun van-dump-monsters (out-file &key (monster-list nil) (var-obj *variant*) (action-fun #'van-obj-printer))

  (assert (functionp action-fun))
  
  (let ((mon-list (if monster-list
		       monster-list
		       (lb-engine:get-monster-list var-obj)))
	(*print-case* :downcase)
	(*print-right-margin* 120))
    
    (with-open-file (ffile (pathname out-file)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (pprint '(in-package :langband) ffile)
      (terpri ffile)

      (dolist (x mon-list)
	(funcall action-fun ffile x))
      
      (terpri ffile))))


(define-key-operation 'break-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(break)))

(define-key-operation 'deliver-damage
    #'(lambda (dungeon player)
        (declare (ignore dungeon))
        (modify-creature-state! player '<cut>      :add 20)
	(modify-creature-state! player '<stun>     :add 20)
	(modify-creature-state! player '<poisoned> :add 20)
	))

(define-key-operation 'summon
    #'(lambda (dungeon player)
	(let* ((summon (get-string-input "Monster to summon: "))
	       (mon (if summon (produce-active-monster *variant* summon))))
	  (when mon
	    (block mon-placement
	      (let ((px (location-x player))
		    (py (location-y player)))
		(flet ((put-mon (x y)
			 (when (cave-floor-bold? dungeon x y)
			   (place-single-monster! dungeon player mon x y nil)
			   (light-spot! dungeon x y)
			   (return-from mon-placement nil))))
		  (put-mon (1+ px) py)
		  (put-mon px (1+ py))
		  (put-mon (1+ px) (1+ py)))))
	    mon))))

(define-key-operation 'object-create
    #'(lambda (dungeon player)
	(let* ((summon (get-string-input "Object to create: "))
	       (mon (if summon (produce-active-object *variant* summon))))
	  (when mon
	    (block mon-placement
	      (let ((px (location-x player))
		    (py (location-y player)))
		(flet ((put-mon (x y)
			 (when (cave-floor-bold? dungeon x y)
			   (drop-near-location! *variant* dungeon mon x y)
			   (light-spot! dungeon x y)
			   (return-from mon-placement nil))))
		  (put-mon (1+ px) py)
		  (put-mon px (1+ py))
		  (put-mon (1+ px) (1+ py)))))
	    mon))))


(define-key-operation 'set-gold
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let* ((str-amount (get-string-input "Gold-amount: "))
	       (amount (ignore-errors (parse-integer str-amount))))
	  (when (and (integerp amount) (plusp amount))
	    (setf (player.gold player) amount)
	    (bit-flag-add! *redraw* +print-gold+))
	  )))

(define-key-operation 'go-to-depth
    #'(lambda (dungeon player)

	(let* ((which-depth (get-string-input "Depth: "))
	       (depth (ignore-errors (parse-integer which-depth))))
	  (when (and (integerp depth) (plusp depth))
	    (setf (player.depth player) depth
		  (dungeon.depth dungeon) depth)
		  
	    (when (> depth (player.max-depth player))
	      (setf (player.max-depth player) depth))

	    (setf (player.leaving-p player) :teleport)
	    t))
	))

    

(define-key-operation 'print-odd-info
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((var-obj *variant*)
	      (fname (concatenate 'string *dumps-directory* "odd.info"))
	      (*print-case* :downcase))
	  (with-open-file (s (pathname fname)
			     :direction :output 
			     :if-exists :supersede
			     :if-does-not-exist :create)
	    (let ((effects (variant.effects var-obj)))
	      (dolist (i (reverse effects))
		(format s "~&Effect ~d: ~a - ~a" (effect.number i) (effect.name i) (effect.symbol i))))

	    (format s "~2%")
	    
	    (let ((elements (variant.elements var-obj)))
	      (dolist (i (reverse elements))
		(format s "~&Element ~d: ~a - ~a" (element.number i) (element.name i) (element.symbol i))))

	    (format s "~2%")
	    
	    (loop for i from 5 to 200 by 10
		  do
		  (format s "~&~a% chance to hit armour ~a (~a)~%"
			  (get-chance var-obj 80 i)
			  i
			  (get-armour-desc var-obj i)))
	    
	    )
	  (print-message! "Odd info was dumped to dumps/odd.info")
	  )))

(define-key-operation 'print-keys
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(print-key-table (gethash :global *ang-keys*)
			 "table.keys")))

(define-key-operation 'dump-monsters
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((var-obj *variant*))
	  (van-dump-monsters (concatenate 'string *dumps-directory* "mon-by-id.list")
			     :monster-list (get-monster-list var-obj
							     :predicate #'string<
							     :sort-key #'monster.id))
	  (van-dump-monsters (concatenate 'string *dumps-directory* "mon-by-id-short.list")
			     :monster-list (get-monster-list var-obj
							     :predicate #'string<
							     :sort-key #'monster.id)
			     :action-fun #'(lambda (file obj)
					     (format file "~&~30a ~30a~%" (monster.id obj)
						     (monster.depth obj))))

	  (van-dump-monsters (concatenate 'string *dumps-directory* "mon-by-depth.list")
			     :monster-list (get-monster-list var-obj
							     :predicate #'<
							     :sort-key #'monster.depth)
			     :action-fun #'(lambda (file obj)
					     (format file "~&~30a ~30a~%" (monster.id obj)
						     (monster.depth obj))))
	  )))

(define-key-operation 'dump-objects
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(dump-objects (concatenate 'string *dumps-directory* "obj.list"))
	))

(define-key-operation 'show-objects
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	  (let ((obj-list (get-object-list))
	    	(var-obj *variant*))

	    (with-new-screen ()
	      (c-term-clear!)

	    (handler-case
		(loop for obj in obj-list
		      for i from 0
		      for y from 1
		      do

		      (c_term_erase! 0 y 255)
		      (put-coloured-str! +term-white+ (format nil "~d" i) 1 y)
		      
		      (let* ((flav (object.flavour obj))
			     (the-attr (if flav (x-attr flav) (x-attr obj)))
			     (the-char (if flav (x-char flav) (x-char obj))))

			    (c-term-queue-char! 5 y the-attr the-char 0 0)
			    
			    (if (>= the-attr +graphics-start+)
				(c-term-queue-char! 6 y -1 -1 0 0)
				(c-term-queue-char! 6 y +term-white+ #.(char-code #\Space)
						    +term-white+ #.(char-code #\Space))))
		      
		      (let ((attr (text-attr obj)))
			(when (= attr +term-dark+)
			  (setf attr +term-white+))
			(put-coloured-str! attr (object.name obj) 9 y))
		      
		      (when (and (> i 5) (= 0 (mod i 20)))
			(c-clear-from! (1+ y))
			(setf y 0) ;; it's incf'ed
			(pause-last-line!))
		      )
	      (error (co)
		(warn "Erred ~s" co)))
	    
	    (c-term-clear!))
	    )))


(define-key-operation 'dump-features
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(dump-floors (concatenate 'string *dumps-directory* "floors.list"))
	))

(define-key-operation 'inspect-coord
    #'(lambda (dungeon player)
	(let* ((cur-x (location-x player))
	       (cur-y (location-y player))
	       (coord-obj (cave-coord dungeon cur-x cur-y)))
	  (warn "Describing [~a,~a]" cur-x cur-y)
	  (describe coord-obj)
	  (multiple-value-bind (the-attr the-char)
	      (map-info dungeon cur-x cur-y)
	    (warn "Mapped to (~s . ~s)" the-attr the-char)))))

(define-key-operation 'in-game-test
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
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
#||
(define-key-operation 'print-map
    #'(lambda (dungeon player)
	(declare (ignore player))
	(print-map-to-file dungeon "./map.ascii")
	(print-message! "Map printed to map.ascii.")
	))


(define-key-operation 'print-map-as-ppm
    #'(lambda (dungeon player)
	(declare (ignore player))
	(print-map-as-ppm dungeon "./map.ppm")
	(print-message! "Map printed to map.ppm.")
	))
||#
(define-key-operation 'gain-level
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let* ((cur-level (player.level player))
	       (next-limit (aref (player.xp-table player) cur-level))
	       (lacks (- next-limit (player.cur-xp player))))
	  (alter-xp! player lacks))
	))

(define-key-operation 'heal-player
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(setf (current-hp player) (maximum-hp player))
	(bit-flag-add! *redraw* +print-hp+)
	))
(define-key-operation 'load-vanilla
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(compat-read-savefile& "vanilla.save")))

(define-key-operation 'send-spell
    #'(lambda (dungeon player)
	(declare (ignore dungeon))

	(let ((px (location-x player))
	      (py (location-y player))
	      (fire-effect (get-spell-effect '<fire>)))
	  
	  ;; let us do a fire-bolt
	  (let ((flag (logior +project-kill+ +project-stop+ +project-through+)))
	    (when-bind (dir (%read-direction))
	      (do-projection player (+ (aref *ddx* dir) px) (+ (aref *ddy* dir) py) flag
			     :effect fire-effect :damage 4)))
	  
	  ;; let us do a fire-beam
	  (let ((flag (logior +project-kill+ +project-beam+ +project-through+)))
	    (when-bind (dir (%read-direction))
	      (do-projection player (+ (aref *ddx* dir) px) (+ (aref *ddy* dir) py) flag
			     :effect fire-effect :damage 4)))
	  
	  ;; let us do a fire-ball
	  (let ((flag (logior +project-kill+ +project-stop+ +project-grid+ +project-item+)))
	    (when-bind (dir (%read-direction))
	      (do-projection player (+ px (* 99 (aref *ddx* dir))) (+ py (* 99 (aref *ddy* dir))) flag
			     :effect fire-effect :radius 4 :damage 4)))

	)

	))

(define-key-operation 'jump-to-test-level
    #'(lambda (dungeon player)
	(let ((wanted-level 25)
	      (depth 40))
	;; get decent 
	(loop while (< (player.level player) wanted-level)
	      do
	      (alter-xp! player (+ 100 (player.max-xp player))))
	
	(heal-creature! player 7000)
	
	(when (and (integerp depth) (plusp depth))
	  (setf (player.depth player) depth
		(dungeon.depth dungeon) depth)
	  
	  (when (> depth (player.max-depth player))
	    (setf (player.max-depth player) depth))
	  
	  (setf (player.leaving-p player) :teleport)
	  t)

	)))


(define-key-operation 'wizard-menu
    #'(lambda (dungeon player)

;;	(with-new-screen ()
	  (block wizard-input 
	    (let ((loc-table (gethash :wizard *current-key-table*)))
	      (loop
;;	       (c-clear-from! 0)
;;	       (display-creature *variant* player)
	       (print-message! nil)
	       
	       (c-prt! "Wizard command: " 0 0)

	       (let* ((ch (read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(return-from wizard-input (funcall fun dungeon player)))
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
(define-keypress *ang-keys* :wizard #\G 'set-gold)
(define-keypress *ang-keys* :wizard #\H 'heal-player)
(define-keypress *ang-keys* :wizard #\I 'inspect-coord)
(define-keypress *ang-keys* :wizard #\J 'jump-to-test-level)
(define-keypress *ang-keys* :wizard #\K 'print-keys)
(define-keypress *ang-keys* :wizard #\L 'gain-level) 
(define-keypress *ang-keys* :wizard #\O 'object-create)
;;(define-keypress *ang-keys* :wizard #\P 'print-map-as-ppm)
(define-keypress *ang-keys* :wizard #\S 'send-spell)
;;(define-keypress *ang-keys* :wizard #\T 'print-map)
(define-keypress *ang-keys* :wizard #\U 'summon)
(define-keypress *ang-keys* :wizard #\W 'print-odd-info)
(define-keypress *ang-keys* :wizard #\Z 'in-game-test)

(define-keypress *ang-keys* :wizard #\d 'deliver-damage)
(define-keypress *ang-keys* :wizard #\l 'load-vanilla)
(define-keypress *ang-keys* :wizard #\m 'dump-monsters)
(define-keypress *ang-keys* :wizard #\o 'dump-objects)
(define-keypress *ang-keys* :wizard #\s 'show-objects)
