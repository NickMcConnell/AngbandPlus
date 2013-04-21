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

(define-key-operation 'object-create
    #'(lambda (dun pl)
	(let* ((summon (get-string-input "Object to create: "))
	       (mon (if summon (produce-active-object *variant* summon))))
	  (when mon
	    (block mon-placement
	      (let ((px (location-x pl))
		    (py (location-y pl)))
		(flet ((put-mon (x y)
			 (when (cave-floor-bold? dun x y)
			   (drop-near-location! *variant* dun mon x y)
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
	  (c-print-message! "Odd info was dumped to dumps/odd.info")
	  )))

(define-key-operation 'print-keys
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(print-key-table (gethash :global *ang-keys*)
			 "table.keys")))

(define-key-operation 'dump-monsters
    #'(lambda (dun pl)
	(declare (ignore dun pl))
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
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(dump-objects (concatenate 'string *dumps-directory* "obj.list"))
	))

(define-key-operation 'dump-features
    #'(lambda (dun pl)
	(declare (ignore dun pl))
	(dump-features (concatenate 'string *dumps-directory* "feat.list"))
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

(define-key-operation 'gain-level
    #'(lambda (dun player)
	(declare (ignore dun))
	(let* ((cur-level (player.level player))
	       (next-limit (aref (player.xp-table player) cur-level))
	       (lacks (- next-limit (player.cur-xp player))))
	  (alter-xp! player lacks))
	))

(define-key-operation 'heal-player
    #'(lambda (dun player)
	(declare (ignore dun))
	(setf (current-hp player) (maximum-hp player))
	(bit-flag-add! *redraw* +print-hp+)
	))


(define-key-operation 'wizard-menu
    #'(lambda (dun pl)

;;	(with-new-screen ()
	  (block wizard-input 
	    (let ((loc-table (gethash :wizard *current-key-table*)))
	      (loop
;;	       (c-clear-from! 0)
;;	       (display-creature *variant* pl)
	       (print-message! nil)
	       
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
(define-keypress *ang-keys* :wizard #\G 'set-gold)
(define-keypress *ang-keys* :wizard #\H 'heal-player)
(define-keypress *ang-keys* :wizard #\I 'inspect-coord)
(define-keypress *ang-keys* :wizard #\K 'print-keys)
(define-keypress *ang-keys* :wizard #\L 'gain-level) 
(define-keypress *ang-keys* :wizard #\O 'object-create)
(define-keypress *ang-keys* :wizard #\P 'print-map-as-ppm)
(define-keypress *ang-keys* :wizard #\T 'print-map)
(define-keypress *ang-keys* :wizard #\U 'summon)
(define-keypress *ang-keys* :wizard #\W 'print-odd-info)
(define-keypress *ang-keys* :wizard #\Z 'in-game-test)

(define-keypress *ang-keys* :wizard #\m 'dump-monsters)
(define-keypress *ang-keys* :wizard #\o 'dump-objects)

;; obsolete
