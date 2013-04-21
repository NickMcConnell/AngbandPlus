;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: util.lisp - utility-code dependant on other code
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Convenient utilities which is based on several
ADD_DESC: classes and must be loaded late.

||#

(in-package :org.langband.engine)

    
(defmethod select-item ((dungeon dungeon) (player player) allow-from
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


    (with-dialogue ()
      (block read-loop
	(loop

	 (setq printed-prompt (format nil "~a " the-prompt))
	 (put-coloured-line! +term-white+ printed-prompt 0 0)
       
	 (when show-mode
	   (item-table-print (get-item-table dungeon player the-place)
			     :show-pause nil
			     :print-selection selection-function))


	 ;; add setting of cursor.

	 ;; how to do graceful exit of loop?
	 (let ((read-char (read-one-character)))

	   ;;(warn "read-char ~s" read-char)
	   (cond ((eql read-char +escape+)
		  (return-from select-item nil))
	       
		 ((eql read-char #\*)
		  (setq show-mode t))
	       
		 ((eql read-char #\/)
		  (when (or (and allow-equip    (eq the-place :backpack))
			    (and allow-backpack (eq the-place :equip)))
		    (setq the-place (if (eq the-place :backpack) :equip :backpack))))
	       
		 ((and allow-floor (eq read-char #\-))
		  (select-item dungeon player allow-from
			       :prompt prompt :where :floor))

		 ;; improve this code, it just bails out now.
		 ((alpha-char-p read-char)
		  (put-coloured-line! +term-white+ "" 0 0)
		  (let ((num (a2i read-char)))
		    (if (>= num 0)
			(return-from select-item (cons the-place num))
			(return-from select-item nil))))
	       
		 (t
		  ;; maybe go through loop again instead?
		  ;;(error "Fell through on read-char in select-item ~s" read-char)
		  ))
	       
	   (put-coloured-line! +term-white+ "" 0 0))))
	    
   
      ;; clear prompt
      #-cmu
      (put-coloured-line! +term-white+ "" 0 0)
    
      nil)))

(defun grab-a-selection-item (dungeon player  allow-from
			     &key prompt (where :backpack)
			     selection-function)
  "Wrapper for select-item which just gets and returns the 'removed' object, or NIL."
  (when-bind (selection (select-item dungeon player allow-from
				     :prompt prompt
				     :where where
				     :selection-function selection-function))

    (item-table-remove! (get-item-table dungeon player (car selection))
			(cdr selection) :only-single-items t)))

(defun get-ensured-floor-table (dungeon the-x the-y)
  (let ((cur-objs (cave-objects dungeon the-x the-y)))
    (unless cur-objs
      (setf cur-objs (make-floor-container dungeon the-x the-y))
      (setf (cave-objects dungeon the-x the-y) cur-objs))
    cur-objs))

(defmethod get-item-table ((dungeon dungeon) (player player) which-table &key x y)
  "Returns item-table or NIL."
  
  (ecase which-table
    (:floor
     (let* ((the-x (if x x (location-x player)))
	    (the-y (if y y (location-y player))))
       (get-ensured-floor-table dungeon the-x the-y)))
    (:backpack (aobj.contains (player.inventory player)))
    (:equip (player.equipment player))))



;;; === Equipment-implementation for floors ===

(defmethod item-table-add! ((table items-on-floor) obj &optional key)
  (declare (ignore key))
;;  (lang-warn "Pushing ~a [~a,~a] onto floor [~a,~a]"
;;	    obj (location-x obj) (location-y obj)
;;	    (location-x table) (location-y table))
  (let ((tab-x (location-x table))
	(tab-y (location-y table))
	(dungeon (items.dungeon table)))
	
    (setf (location-x obj) tab-x
	  (location-y obj) tab-y)
    (push obj (dungeon.objects dungeon))
    (push obj (items.objs table))
    (incf (items.cur-size table))

;;    (warn "Added ~s to floor at ~s,~s" obj tab-x tab-y)
    ;; let's notify about the change
    (note-spot! dungeon tab-x tab-y)
    (light-spot! dungeon tab-x tab-y)
    
    t))

(defmethod item-table-remove! ((table items-on-floor) key &key only-single-items)
  (cond ((item-table-verify-key table key)
	 (let ((ret-obj nil)
	       (num-key (typecase key
			  (character (a2i key))
			  (number key)
			  (active-object (position key (items.objs table)))
			  (t nil))))
	   (when (numberp num-key)
	     (let ((old-obj (elt (items.objs table) num-key)))

	       ;; if only one, else remove
	       (cond ((and only-single-items (> (aobj.number old-obj) 1))
		      (setf ret-obj (create-aobj-from-kind (aobj.kind old-obj)))
		      (decf (aobj.number old-obj)))
		     (t
		      (setf (items.objs table) (delete old-obj (items.objs table)))
		      (remove-item-from-dungeon! (items.dungeon table) old-obj)
		      (decf (items.cur-size table))
		      (setf ret-obj old-obj)))))
	   
	   ret-obj))
	(t
	 (warn "illegal key ~a" key)
	 nil)))

(defmethod item-table-clean! ((table items-on-floor))
  (when (next-method-p)
    (call-next-method table))
  (let ((dungeon (items.dungeon table)))
    (dolist (i (items.objs table))
      (remove-item-from-dungeon! dungeon i)))

  (setf (items.objs table) nil))

(defmethod item-table-find ((table items-on-floor) key)
  (when (item-table-verify-key table key)
    (typecase key
      (character (elt (items.objs table) (a2i key)))
      (number (elt (items.objs table) key))
      (t
       (warn "unknown type ~a of key" (type-of key))
       nil))))


(defmethod item-table-sort! ((table items-on-floor) sorter)
  (declare (ignore sorter))
  ;; the floor is never sorted
  nil)

(defmethod item-table-iterate! ((table items-on-floor) function)
  (loop for i from 0
	for obj in (items.objs table)
	do
	(funcall function table i obj)))

(defmethod item-table-more-room? ((table items-on-floor) &optional obj)
  (declare (ignore obj))
  t)



(defmethod calculate-score (variant player)
  (declare (ignore variant))
  (+ (player.max-xp player) (* 100 (player.depth player))))

;;; === Some simple code for actual rooms, move them later.

(defclass simple-room (room-type)
  ())

(defclass overlapping-room (room-type)
  ())

(defun common-make-simple-room ()
  "constructor for the simple room."
  (let ((room (make-instance 'simple-room :id "simple-room"
			     :name "simple room"
			     :size-mod #1A(0 0 -1 1)
			     :min-level 1)))

    room))

(defun common-make-overlapping-room ()
  "constructor for the overlapping room."
  (let ((room (make-instance 'overlapping-room :id "overlapping-room"
			     :name "overlapping room"
			     :size-mod #1A(0 0 -1 1)
			     :min-level 1)))

    room))

(defun %room-has-light? (room dungeon &optional (chance 25))
  (declare (ignore room))
  (<= (dungeon.depth dungeon) (randint chance)))

(defmethod build-room! ((room simple-room) dungeon player x0 y0)

  (declare (ignore player))
  
  (let (;;(depth (dungeon.depth dungeon))
	(light (%room-has-light? room dungeon 25))
	(y1 (- y0 (randint 4)))
	(y2 (+ y0 (randint 3)))
	(x1 (- x0 (randint 11)))
	(x2 (+ x0 (randint 11)))
	(room-wall (get-floor-type "room-wall"))
	(inside-room-wall (get-floor-type "inside-room-wall"))
	(regular-floor (get-floor-type "normal-floor")))
	
    (unless (and room-wall inside-room-wall regular-floor)
      (warn "Unable to build room at (~s,~s) because walls cannot be found."
	    x0 y0)
      (return-from build-room! nil))
    
    
    (generate-room dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) light)
    (generate-draw dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) room-wall)
    (generate-fill dungeon x1 y1 x2 y2 regular-floor)

    (cond ((= 0 (random 20)) ;; pillar room
	   (loop for y from y1 to y2 by 2
		 do
		 (loop for x from x1 to x2 by 2
		       do
		       (setf (cave-floor dungeon x y) inside-room-wall))))
	  
	  ((= 0 (random 50)) ;; ragged
	   (loop for y from (+ y1 2) to (- y2 2) by 2
		 do
		 (setf (cave-floor dungeon x1 y) inside-room-wall
		       (cave-floor dungeon x2 y) inside-room-wall))
	   
	   (loop for x from (+ x1 2) to (- x2 2) by 2
		 do
		 (setf (cave-floor dungeon x y1) inside-room-wall
		       (cave-floor dungeon x y2) inside-room-wall))
	   ))

    ))

(defmethod build-room! ((room overlapping-room) dungeon player x0 y0)

  (declare (ignore player))
  
  (let ((light (%room-has-light? room dungeon 25))
	(a-y1 (- y0 (randint 4)))
	(a-y2 (+ y0 (randint 3)))
	(a-x1 (- x0 (randint 11)))
	(a-x2 (+ x0 (randint 10)))
	(b-y1 (- y0 (randint 3)))
	(b-y2 (+ y0 (randint 4)))
	(b-x1 (- x0 (randint 10)))
	(b-x2 (+ x0 (randint 11)))
	(room-wall (get-floor-type "room-wall"))
	(floor (get-floor-type "normal-floor")))
	

    (generate-room dungeon (1- a-x1) (1- a-y1) (1+ a-x2) (1+ a-y2) light)
    (generate-room dungeon (1- b-x1) (1- b-y1) (1+ b-x2) (1+ b-y2) light)
	
    (generate-draw dungeon (1- a-x1) (1- a-y1) (1+ a-x2) (1+ a-y2) room-wall)
    (generate-draw dungeon (1- b-x1) (1- b-y1) (1+ b-x2) (1+ b-y2) room-wall)
    
    (generate-fill dungeon a-x1 a-y1 a-x2 a-y2 floor)
    (generate-fill dungeon b-x1 b-y1 b-x2 b-y2 floor)
    ))


(defmethod can-creature-drop? ((variant variant) (mon active-monster))
  (let ((kind (amon.kind mon)))
    (if (monster.treasures kind)
	t
	nil)))

(defmethod creature-drop! ((variant variant) (mon active-monster) (level level))
  (let ((kind (amon.kind mon))
	 (to-drop '()))
     ;; first iterate over the drops and decide what is actually dropped
     (dolist (i (monster.treasures kind))
       (check-type i treasure-drop)
       (when (< (random 100) (* 100 (drop.chance i)))
	 (let ((amount (drop.amount i)))
	   (when (consp amount)
	     (setf amount (roll-dice (car amount) (cdr amount))))
	   (when (plusp amount)
	     (push (list amount (drop.quality i) (drop.type i)) to-drop)))))

     ;; now process the actual drops
     (flet ((drop-an-obj (quality type)
	      (declare (ignore quality))
	      (case type
		((:any :item)
		 (let ((new-obj (get-active-object-by-level variant level)))
		   (drop-near-location! variant (level.dungeon level)
					new-obj (location-x mon) (location-y mon)))
		 )
		(:gold
		 (let ((new-gold (create-gold variant (level.dungeon level) :originator mon)))
		   (drop-near-location! variant (level.dungeon level)
					new-gold (location-x mon) (location-y mon)))
		 ))

	      ))
;;	      (warn "Dropping ~s ~s at (~s,~s)" quality type (location-x mon) (location-y mon)
     
       ;; now we have a list of objects (hopefully) to drop
       
       (dolist (i to-drop)
	 (assert (consp i))
	 (dotimes (j (car i))
	   (drop-an-obj (second i) (third i))))
     

     )))

(defmethod drop-near-location! ((variant variant) (dungeon dungeon) (object active-object) x y)
  ;; we do this hackish
  (flet ((poss-drop (tx ty)
	   (when (bit-flag-set? (floor.flags (cave-floor dungeon tx ty))
				+floor-flag-allow-items+) ;; must allow items
	     (let ((tbl (get-ensured-floor-table dungeon tx ty)))
	       ;;(warn "Dropped ~s at (~s,~s)" object tx ty)
	       (item-table-add! tbl object)
	       (return-from drop-near-location! t)))))
    (poss-drop x y)
    (poss-drop (1+ x) y)
    (poss-drop x (1+ y))
    (poss-drop (1+ x) (1+ y))
    nil))


(defun swap-monsters! (dungeon player from-x from-y to-x to-y)
  "swaps two monsters or move one"

  (let ((var-obj *variant*)
	(mon-1 (cave-monsters dungeon from-x from-y))
	(mon-2 (cave-monsters dungeon to-x to-y))
	(target (player.target player)))
    
    (unless (is-legal-target? dungeon target)
      (setf target nil))

    (setf (cave-monsters dungeon from-x from-y) mon-2
	  (cave-monsters dungeon to-x to-y) mon-1)

    
    ;; hack, move to swap later
    (when (and (= from-x (location-x player))
	       (= from-y (location-y player)))
      (setf (location-x player) to-x
	    (location-y player) to-y)
      ;; add more stuff here
      ;;      (warn "move player (~s ~s) -> (~s ~s)" from-x from-y to-x to-y)
      (bit-flag-add! *update* +pl-upd-update-view+ +pl-upd-distance+
		     +pl-upd-panel+ +pl-upd-update-flow+)
      ;; skip overhead-window
      ;;      (bit-flag-add! *redraw* +print-map+)
      )	;; hack, fix later

	   
    (when mon-1
      (dolist (i mon-1)
	(when (and target (eq (target.obj target) i))
	  (%remove-target target)
	  (setf (target.x target) to-x
		(target.y target) to-y))
	(setf (location-x i) to-x
	      (location-y i) to-y)
	(update-monster! var-obj i t)))


    (when mon-2
      (dolist (i mon-2)

	(when (and target (eq (target.obj target) i))
	  (%remove-target target)
	  (setf (target.x target) from-x
		(target.y target) from-y))
	(setf (location-x i) from-x
	      (location-y i) from-y)
	(update-monster! var-obj i t)))

    
    (light-spot! dungeon from-x from-y)
    (light-spot! dungeon to-x to-y)
    ))



(defmethod update-monster! ((variant variant) (mon active-monster) full-update?)
  (let* ((player *player*)
	 (px (location-x player))
	 (py (location-y player))
	 (mx (location-x mon))
	 (my (location-y mon))
	 (dungeon *dungeon*)
	 (kind (amon.kind mon))
	 (seen nil) ;; seen at all
	 (by-eyes nil) ;; direct vision
	 (distance 666))
    
    (cond (full-update?  ;; get full distance
	   ;; no inlining
	   (setf distance (distance px py mx my)
		 (amon.distance mon) distance)
	   )
	  (t
	   ;; simple
	   (setf distance (amon.distance mon))))

    ;; more stuff
    (when (bit-flag-set? (amon.vis-flag mon) +monster-flag-mark+)
      (setf seen t))

    (when (<= distance +max-sight+)
      ;; skip telepathy

      ;; normal line of sight:
      (when (and (player-has-los-bold? dungeon mx my)
		 t) ;; add blindness check

	;; infravision
	(when (<= distance (player.infravision player))
	  (unless (has-ability? kind '<cold-blood>) ;; unless cold-blooded, infravision works
	    (setf seen t
		  by-eyes t)))

	(when (player-can-see-bold? dungeon mx my)
	  (cond ((has-ability? kind '<invisible>)
		 ;; can the player see it with his see-inv?
		 (when (<= distance (player.see-invisible player))
		   (setf seen t
			 by-eyes t))
		 )
		(t
		 (setf seen t
		       by-eyes t)))
	  
	  ;; skip lore
	  )))

    ;; is it visible?
    (cond (seen 
	   (unless (amon.seen-by-player? mon)
	     (setf (amon.seen-by-player? mon) t)
	     (light-spot! dungeon mx my)
	     ;; skip health bar
	     ;; skip disturb
	     ))
	  ;; no longer visible
	  (t
	   (when (amon.seen-by-player? mon)
	     (setf (amon.seen-by-player? mon) nil)
	     (light-spot! dungeon mx my)
	     ;; skip health bar
	     ;; skip disturb
	     )))

    (cond (by-eyes
	   (unless (bit-flag-set? (amon.vis-flag mon) +monster-flag-view+)
	     (bit-flag-add! (amon.vis-flag mon) +monster-flag-view+)
	     ;; skip disturb
	     ))
	  (t
	   (when (bit-flag-set? (amon.vis-flag mon) +monster-flag-view+)
	     (bit-flag-remove! (amon.vis-flag mon) +monster-flag-view+)
	     ;; skip disturb
	     )))
	  
    ))

(defmethod update-monsters! ((variant variant) (dungeon dungeon) full-update?)
  (let ((monsters (dungeon.monsters dungeon)))
    (dolist (i monsters)
      (when (creature-alive? i)
	(update-monster! variant i full-update?)))))

(defmethod deliver-damage! ((variant variant) source (target active-monster) damage &key note dying-note)
  "Delivers damage to someone."
  (declare (ignore source note))

  (let ((did-target-die? nil))
  
    ;; wake it up
    ;; deliver damage
    (setf (current-hp target) (- (current-hp target) damage))

    
    (when (minusp (current-hp target))
      (setf did-target-die? t)
      ;; make a message about it
      (cond (dying-note
	     (format-message! "The ~a~a" (monster.name target) dying-note))
	    (t
	     (format-message! "You have slain ~a." (monster.name target))))

      (let ((attacker *player*)
	    (dungeon *dungeon*)
	    (target-xp (get-xp-value target)))
	(alter-xp! attacker (if target-xp target-xp 0))
	(kill-target! dungeon attacker target (location-x target) (location-y target))
	))

    ;; skip fear
    did-target-die?))
  

(defmethod deliver-damage! ((variant variant) (source active-trap) (target player) damage &key note dying-note)
  
  (declare (ignore dying-note note))
  
  (let ((did-target-die? nil))
  
    ;; wake it up
    ;; deliver damage
    (setf (current-hp target) (- (current-hp target) damage))

    (bit-flag-add! *redraw* +print-hp+)
    
    (when (minusp (current-hp target))
      (setf did-target-die? t)

      (format-message! "You were killed by a ~a" (trap.name (decor.type source)))
      (kill-target! *dungeon* source target (location-x target) (location-y target))
      )

    did-target-die?))


(defconstant +random-normal-number+ 256 "Number of entries in table.")
(defconstant +random-normal-deviation+ 64 "The standard deviation of the table.")

(defparameter *random-normal-table* #256(
          206     613    1022    1430    1838       2245    2652    3058
         3463    3867    4271    4673    5075       5475    5874    6271
         6667    7061    7454    7845    8234       8621    9006    9389
         9770   10148   10524   10898   11269      11638   12004   12367
        12727   13085   13440   13792   14140      14486   14828   15168
        15504   15836   16166   16492   16814      17133   17449   17761
        18069   18374   18675   18972   19266      19556   19842   20124
        20403   20678   20949   21216   21479      21738   21994   22245

        22493   22737   22977   23213   23446      23674   23899   24120
        24336   24550   24759   24965   25166      25365   25559   25750
        25937   26120   26300   26476   26649      26818   26983   27146
        27304   27460   27612   27760   27906      28048   28187   28323
        28455   28585   28711   28835   28955      29073   29188   29299
        29409   29515   29619   29720   29818      29914   30007   30098
        30186   30272   30356   30437   30516      30593   30668   30740
        30810   30879   30945   31010   31072      31133   31192   31249

        31304   31358   31410   31460   31509      31556   31601   31646
        31688   31730   31770   31808   31846      31882   31917   31950
        31983   32014   32044   32074   32102      32129   32155   32180
        32205   32228   32251   32273   32294      32314   32333   32352
        32370   32387   32404   32420   32435      32450   32464   32477
        32490   32503   32515   32526   32537      32548   32558   32568
        32577   32586   32595   32603   32611      32618   32625   32632
        32639   32645   32651   32657   32662      32667   32672   32677

        32682   32686   32690   32694   32698      32702   32705   32708
        32711   32714   32717   32720   32722      32725   32727   32729
        32731   32733   32735   32737   32739      32740   32742   32743
        32745   32746   32747   32748   32749      32750   32751   32752
        32753   32754   32755   32756   32757      32757   32758   32758
        32759   32760   32760   32761   32761      32761   32762   32762
        32763   32763   32763   32764   32764      32764   32764   32765
        32765   32765   32765   32766   32766      32766   32766   32767))


(defun normalised-random (mean stand)
  "Generate a random integer number of NORMAL distribution
 
  The table above is used to generate a psuedo-normal distribution,
  in a manner which is much faster than calling a transcendental
  function to calculate a true normal distribution."

  ;; paranoia
  (when (< stand 1)
    (return-from normalised-random mean))
  
  (let ((low 0)
	(high +random-normal-number+)
	(tmp (random 32768)))
    (loop while (< low high)
	  do
	  (let ((mid (int-/ (+ low high) 2)))
	    (cond ((< (aref *random-normal-table* mid) tmp)
		   (setf low (1+ low)))
		  (t
		   (setf high mid)))))
    (let ((offset (int-/ (* stand low) 
			 +random-normal-deviation+)))
      (if (< (random 100) 50)
	  (- mean offset)
	  (+ mean offset)))))
  

(defun magic-bonus-for-level (max level)
  "Returns an appropriate bonus for the level."
  (let* ((variant *variant*)
	 (max-depth (variant.max-depth variant)))
    (assert (numberp max))
    (assert (numberp level))
    (when (> level (1- max-depth))
      (setf level (1- max-depth)))

    (let ((bonus (int-/ (* max level)
			max-depth))
	  (extra (mod (* max level)
			max-depth))
	  (stand (int-/ max 4)))
      
      ;; hack
      (when (< (random max-depth) extra)
	(incf bonus))

      ;; hack
      (when (< (random 4) (mod max 4))
	(incf stand))

      (let ((value (normalised-random bonus stand)))
	(cond ((minusp value) 0)
	      ((> value max) max)
	      (t value)))
      )))


(defvar *currently-showing-inv* :inventory)
;;(defvar *currently-showing-inv* :equipment)
(defvar *current-map-mode* :gfx-tiles)
  
(defun update-inventory-row (player)
  (unless (eq (get-system-type) 'sdl)
    (return-from update-inventory-row t))
  (let ((off-button (tile-file 38))
	(on-button (tile-file 39))
	(win (aref *windows* +inv-frame+)))
    
    (with-frame (+inv-frame+)
      (clear-window +inv-frame+) ;; hack
      (let ((table nil))
	(cond ((eq *currently-showing-inv* :inventory)
	       (setf table (aobj.contains (player.inventory player))))
	      ((eq *currently-showing-inv* :equipment)
	       (setf table (player.equipment player)))
	      (t
	       (warn "Unable to find good equipment for ~s" *currently-showing-inv*)
	       (return-from update-inventory-row nil)))
      
	(loop for i from 0 below (items.cur-size table)
	      for obj = (aref (items.objs table) i)
	      unless (and (typep table 'items-worn)
			  (worn-item-slot-hidden (aref (variant.worn-item-slots *variant*) i)))
	      do
	      (progn
		(setf (window-coord win +foreground+ i 0) (text-paint-value +term-white+ (+ #.(char-code #\a) i)))
		(when obj
		  (setf (window-coord win +foreground+ i 1) (gfx-sym obj))
		  ))))

      (let ((col (- (get-frame-width +inv-frame+) 1))
	    (equip-button (if (eq *currently-showing-inv* :inventory)
			      off-button
			      on-button))
	    (back-button (if (eq *currently-showing-inv* :inventory)
			     on-button
			     off-button)))

	(setf (window-coord win +foreground+ col 0) (logior (tile-file 10) (tile-number 0))
	      (window-coord win +background+ col 0) (logior back-button (tile-number 0))
	      (window-coord win +foreground+ col 1) (logior (tile-file 3) (tile-number 34))
	      (window-coord win +background+ col 1) (logior equip-button (tile-number 0))
	)

      (let ((col (- (get-frame-width +inv-frame+) 2))
	    (map-button (if (eq *current-map-mode* :ascii)
			    off-button
			    on-button))
	    (ascii-button (if (eq *current-map-mode* :ascii)
			      on-button
			      off-button)))

	(setf (window-coord win +foreground+ col 0) (logior (tile-file 10) (tile-number 19))
	      (window-coord win +background+ col 0) (logior map-button (tile-number 0))
	      (window-coord win +foreground+ col 1) (text-paint-value +term-l-blue+ #.(char-code #\A))
	      (window-coord win +background+ col 1) (logior ascii-button (tile-number 0)))
	      
	)
      ;; should do all the actual painting
      (refresh-window win)
    

      ))))

(defun switch-inventory-view ()
  (if (eq *currently-showing-inv* :inventory)
      (setf *currently-showing-inv* :equipment)
      (setf *currently-showing-inv* :inventory))
  (update-inventory-row *player*)
  *currently-showing-inv*)


(defun switch-map-mode (dungeon player)
  (if (eq *current-map-mode* :ascii)
      (setf *current-map-mode* :gfx-tiles)
      (setf *current-map-mode* :ascii))
  (update-inventory-row *player*)

  (cond ((eq *current-map-mode* :ascii)
	 (deactivate-window +gfxmap-frame+)
	 (activate-window +asciimap-frame+)
	 (setf *map-frame* +asciimap-frame+))
	
	((eq *current-map-mode* :gfx-tiles)
	 (deactivate-window +asciimap-frame+)
	 (activate-window +gfxmap-frame+)
	 (setf *map-frame* +gfxmap-frame+))
	
	(t ))

  (clear-window *map-frame*)
  (verify-panel dungeon player)
  (print-map dungeon player)
  (refresh-window *map-frame*)

  *current-map-mode*)

(defmethod decor-operation ((variant variant) (door active-door) operation &key value)
  "This one uses *dungeon* so please set it to a meaningful value before calling
this function."
  (let ((x (location-x door))
	(y (location-y door))
	(dungeon *dungeon*))
  
    (ecase operation
      (:close
       (unless (eq value t)
	 (warn "Close with non-T argument ~s" value))
       (setf (door.closed? door) t)
       (bit-flag-add! (cave-flags dungeon x y) +cave-wall+)

       (setf (decor.type door) (get-door-type "closed-door")))

      (:open
       (unless (eq value t)
	 (warn "Open with non-T argument ~s" value))
       (setf (door.closed? door) nil)
       (bit-flag-remove! (cave-flags dungeon x y) +cave-wall+)
       (setf (decor.type door) (get-door-type "open-door")))

    
      (:break
       (unless (eq value t)
	 (warn "Break with non-T argument ~s" value))
       (setf (door.broken? door) t)
       (setf (door.closed? door) nil)
       (bit-flag-remove! (cave-flags dungeon x y) +cave-wall+)

       (setf (decor.type door) (get-door-type "destroyed-door"))))

    ))

(defmethod decor-operation ((variant variant) (door active-trap) operation &key value)

  (warn "trap operation ~s (~s) not implemented." operation value)

  )


(defun %alt-sel-input (alt-len)
  "INTERNAL FUNCTION.  Might change!

Reads a character via READ-ONE-CHARACTER and
acts on the result:
  Q     - calls QUIT-GAME&
  S     - returns NIL
  ESC   - Picks random value and returns it (a number)
  *     - As ESC
  ENTER - Returns 'CURRENT (ie the currently selected value)
  SPACE - As ENTER
  [a-z] - Checks if the value is legal, returns number if ok, returns 'BAD-VALUE if not legal
"
  (let ((val (read-one-character)))
    #-cmu
    (assert (characterp val))
;;    (warn "Got back ~a ~s ~s" val val (type-of val))
    (cond ((eql val #\Q)
	   (quit-game&))
	  ;; start over
	  ((eql val #\S)
	   nil)
	  ;; pick a random value
	  ((or (eql val +escape+)
	       (eql val #\*))
	   (random alt-len))
	  ;; use highlighted value
	  ((or (eql val #\Return) (eql val #\Newline))
	   'current)
	  (t
	   (let ((its-char-code (char-code val)))
	     ;; legal char-code
	     (cond ((and (>= its-char-code (char-code #\a))
			 (<= its-char-code (char-code #\z)))
		    (let ((r-val (- its-char-code (char-code #\a))))
		      (if (and (>= r-val 0) (< r-val alt-len))
			  r-val
			  (progn
			    (warn (format nil "Invalid value: ~a" val))
			    'bad-value))))

		   ;; an arrow-key I guess
		   ((and (>= its-char-code (char-code #\0))
			 (<= its-char-code (char-code #\9)))
		    (case val
		      (#\8 'up)
		      (#\6 'right)
		      (#\4 'left)
		      (#\2 'down)
		      (t 'bad-value)))

		   (t
		    'bad-value))
	     )))
    ))


(defun interactive-alt-sel (col row alternatives
			    &key (display-fun nil) (mod-value 5) 
			    (ask-for "value") (settings nil))
"Interative selection of alternatives.

The COL, ROW argument specifies where the alternatives should start
ALTERNATIVES is a list of valid alternatives
DISPLAY-FUN is a function to display help for a given option
MOD-VALUE is how much space should be between rows (I think)"
  ;; [add more info]

  
  (let ((alt-len (length alternatives))
	(win *cur-win*))
    (labels ((display-alternatives (highlight-num)
	       (let* ((desc (when display-fun
			     (funcall display-fun highlight-num)))
		      (text-col (setting-value settings 'text-x 2))
		      (text-row (setting-value settings 'text-y 10))
		      (text-attr (setting-value settings 'text-attr +term-white+))
		      (text-wid (setting-value settings 'text-w 75))
		      (alt-colour (setting-value settings 'altern-attr +term-white+))
		      (salt-colour (setting-value settings 'altern-sattr +term-l-blue+))
		      (clear-row (if desc text-row row)))

		 
		 
		 ;; find a better solution here
		 (loop for i from clear-row below (get-frame-height)
		       do
		       ;;(warn "clear ~s ~s" text-col i)
		       (clear-row win text-col i))
;;		       (put-coloured-line! +term-white+ "" text-col i))
		 ;;(clear-window-from *cur-win* text-row) ;; clears things

		 (when desc
		   (print-text! text-col text-row text-attr desc :end-col (+ text-col text-wid)))
		 
		 (loop for cur-alt in alternatives
		       for i from 0
		       for the-col = (truncate (+ col (* 15 (mod i mod-value))))
		       for the-row = (truncate (+ 1 row (/ i mod-value)))
		       for cur-bg  = (if (= i highlight-num) salt-colour alt-colour)
		       do
		       (put-coloured-str! cur-bg (format nil "~c) " (i2a i)) the-col the-row)
		       (put-coloured-str! cur-bg (format nil "~10a" cur-alt) (+ 3 the-col) the-row)
		       )))
	     
	     (get-a-value (cur-sel)
	       (let* ((query-colour (setting-value settings 'query-attr +term-l-red+))
		      (red-query (setting-value settings 'query-reduced nil))
		      (query-str (if red-query
				     "Choose a ~a (~c-~c): "
				     "Choose a ~a (~c-~c, or * for random): ")))
		 
		 (display-alternatives cur-sel)
		 (put-coloured-str! query-colour (format nil query-str
							 ask-for (i2a 0) (i2a (- alt-len 1)))
				    col row))
	       (let ((rval (%alt-sel-input alt-len)))
		 (if (not (symbolp rval))
		     rval
		     (case rval
		       (bad-value (get-a-value cur-sel)) ;; retry
		       (current cur-sel)	     ;; return current
		       (up    (get-a-value (mod (- cur-sel mod-value) alt-len))) ;; move selection
		       (down  (get-a-value (mod (+ cur-sel mod-value) alt-len))) ;; move selection
		       (left  (get-a-value (mod (- cur-sel 1) alt-len))) ;; move selection
		       (right (get-a-value (mod (+ cur-sel 1) alt-len))) ;; move selection
		       (t
			(warn "Unknown symbol returned ~s" rval)))))

	       ))
      
      (get-a-value 0))))
