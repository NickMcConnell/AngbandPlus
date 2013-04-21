;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: birth.lisp - character creation
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the character creation code.  needs clean-up

|#

(in-package :org.langband.engine)

(defmethod query-for-character-basics! ((variant variant) (the-player player)
					(settings birth-settings))
  "Interactive questioning to select the basics of the character.
Modififes the passed player object THE-PLAYER.  This is a long function."

  (clear-window +full-frame+)

  ;; print info on process in upper right corner
  (print-text! (slot-value settings 'instr-x)
	       (slot-value settings 'instr-y)
	       (slot-value settings 'instr-attr)
	       #.(concatenate 'string
			      "Please answer the following questions.  "
			      "Legal answers are shown below the marked question.  You may use " 
			      "arrow-keys to highlight answers and display description, "
			      "or you may hit 'Q' to quit, 'S' to start all over or '?' " 
			      "to enter the generic help-system.")
	       :end-col (slot-value settings 'instr-w))

  (let ((info-col (slot-value settings 'info-x))
	(info-row (slot-value settings 'info-y))
	(info-colour (slot-value settings 'info-attr))
	(quest-row (slot-value settings 'query-y))
	(quest-col (slot-value settings 'query-x))
	(choice-col (slot-value settings 'choice-x))
	(choice-row (slot-value settings 'choice-y))
	(choice-colour (slot-value settings 'choice-attr))
	(choice-tcolour (slot-value settings 'choice-tattr))
	(mod-value (slot-value settings 'altern-cols))
	)
  
    ;; First we do the player gender
    (clear-window-from *cur-win* info-row) ;; clears things
    ;; Print extra-info Extra info
    (print-text! info-col info-row info-colour
		 "Your 'gender' does not have any significant gameplay effects."
		 :end-col (slot-value settings 'instr-w))

    (block input-loop
      (loop
       (let* ((genders (variant.genders variant))
	      (alt-len (length genders))
	      (inp (interactive-alt-sel quest-col quest-row
					(mapcar #'gender.name genders)
					:settings settings
					:ask-for "gender")))
	 (cond ((eq inp nil)
		(return-from query-for-character-basics! nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.gender the-player) (nth inp genders))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from input-loop ~s, must be [0..~s)" inp alt-len))
	       ))))
   
    (put-coloured-str! choice-tcolour  "Gender" choice-col choice-row) 
    (put-coloured-str! choice-colour   (get-gender-name the-player) (+ 7 choice-col) choice-row)

    
    (clear-window-from *cur-win* info-row) ;; clears things

    (print-text! info-col info-row info-colour
		 "Your 'race' determines various intrinsic factors and bonuses."
		 :end-col (slot-value settings 'instr-w))


    (block input-loop
      (loop
       (let* ((cur-races (get-races-as-a-list))
	      (alt-len (length cur-races))
	      (inp (interactive-alt-sel  quest-col quest-row
					 (mapcar #'race.name cur-races)
					 :display-fun #'(lambda (x)
							  (when (and (numberp x) (>= x 0) (< x alt-len))
							    (race.desc (elt cur-races x))))
					 :ask-for "race"
					 :settings settings
					 :mod-value mod-value
					 )))
	      
	 (cond ((eq inp nil)
		(return-from query-for-character-basics! nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.race the-player) (nth inp cur-races))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from race input-loop ~s, must be [0..~s)" inp alt-len))
	       ))))


      (put-coloured-str! choice-tcolour  "Race" choice-col (+ 1 choice-row))
      (put-coloured-str! choice-colour (get-race-name the-player) (+ 7 choice-col) (+ 1 choice-row))

      (clear-window-from *cur-win* info-row) ;; clears things

      ;; time to do classes.. slightly more tricky

      (print-text! info-col info-row info-colour
		   #.(concatenate 'string
				  "Your 'class' determines various intrinsic abilities and bonuses. "
				  "Any entries inside (parantheses) should only be used by advanced players."
				  )
		   
		   :end-col (slot-value settings 'instr-w))
      

   
      (let ((cur-classes (race.classes (player.race the-player)))
	    (other-classes nil)
	    (combined-classes nil)
	    (comb-class-len 0)
	    (class-len 0))

	(cond ((eq cur-classes t)
	       (setq cur-classes (get-classes-as-a-list)))
	      ((consp cur-classes)
	       (let ((all-classes (get-classes-as-a-list))
		     (tmp-classes nil))
		 (dolist (i all-classes)
;;		   (warn "Checking for ~s in ~s" (class.symbol i) cur-classes)
		   ;; maybe let this handle symbols and strings?
		   (if (find (class.symbol i) cur-classes :test #'eq)
		       (push i tmp-classes)
		       (push i other-classes)))
		 (setq cur-classes tmp-classes)))
	      (t
	       (warn "Unknown classes ~a for race ~a" cur-classes (get-race-name the-player))
	       (return-from query-for-character-basics! nil)))

	(setq class-len (length cur-classes))
	(setq combined-classes (append cur-classes other-classes))
	(setq comb-class-len (length combined-classes))
	
	(block input-loop
	  (loop
	   (let* ((class-names (loop for x in combined-classes
				     for i from 0
				     collecting 
				     (if (>= i class-len)
					 (concatenate 'string "(" (class.name x) ")")
					 (class.name x))))
		  (inp (interactive-alt-sel quest-col quest-row
					    class-names
					    :display-fun #'(lambda (x)
							     (when (and (numberp x) (>= x 0) (< x comb-class-len))
							       (class.desc (elt combined-classes x))))
					    :ask-for "class"
					    :settings settings
					    :mod-value mod-value
					    )))
	     
	     (cond ((eq inp nil)
		    (return-from query-for-character-basics! nil))
		   
		   ((and (numberp inp) (<= 0 inp) (< inp comb-class-len))
		    (setf (player.class the-player) (nth inp combined-classes))
		    (return-from input-loop nil))
		   
		   (t
		    (warn "Unknown return-value from class input-loop ~s, must be [0..~s)"
			  inp comb-class-len))
		   ))))
	)



  (put-coloured-str! +term-white+  "Class" choice-col (+ 2 choice-row))
  (put-coloured-str! +term-l-blue+ (get-class-name the-player)
		     (+ 7 choice-col) (+ 2 choice-row))

  (clear-window-from *cur-win* info-row) ;; clears things
  
   
  t))

(defun roll-stats! (variant player)
  "Rolls stats and modifies given player object.
Returns the base-stats as an array or NIL if something failed."
  
  (setf (player.base-stats player)    (make-stat-array variant)
	(player.cur-statmods player) (make-stat-array variant)
	(player.modbase-stats player) (make-stat-array variant)
	(player.active-stats player)  (make-stat-array variant))
  
  (let* ((stat-len (variant.stat-length variant))
	 (arr-len (* 3 stat-len))
;;	 (bonus 0)
	 (rolls (make-array arr-len)))

    ;; roll 1d3, 1d4, 1d5 series
    (block roller
      (while t
	(dotimes (i arr-len)
	  (setf (svref rolls i) (randint (+ 3 (mod i 3)))))

	;; sum up rolls
	(let ((sum (reduce #'+ rolls)))
	  (when (and (< 42 sum) (> 54 sum)) ;; within acceptable range
	    (return-from roller)))))

    ;; now assign values
    (dotimes (i stat-len)
      (let* ((arr-offset (* 3 i))
	     (stat-val (+ 5
			 (svref rolls (+ 0 arr-offset))
			 (svref rolls (+ 1 arr-offset))
			 (svref rolls (+ 2 arr-offset)))))
	
	(setf (svref (player.base-stats player) i) stat-val
	      (svref (player.cur-statmods player) i) 0)
	)))

  ;; returns the array
  (player.base-stats player))

	    
  

(defmethod roll-up-character! ((variant variant) (player player))
  "Rolls up a character and modifies given PLAYER-object."
  ;; dropping auto-roller
  
  (clear-window-from *cur-win* 10)
  
  (roll-stats! variant player)

  (let ((hit-dice (+ (race.hit-dice (player.race player))
		     (class.hit-dice (player.class player)))))

    ;; first level we have max
    (setf (aref (player.hp-table player) 0) hit-dice)
    
    (setf (maximum-hp player) hit-dice
	  (current-hp player) hit-dice))

  (update-xp-table! variant player) ;; hack
  
 ;; (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-hp+ +pl-upd-mana+)
;;  (warn "upd is ~s" *update*)
;;  (update-stuff variant nil player)



  ;; improve?  hackish.
  (calculate-creature-bonuses! variant player)
  (calculate-creature-mana! variant player)
  
  ;; hack
  (setf (current-hp player) (maximum-hp player)
	(current-mana player) (maximum-mana player))
    
  
  t)


(defun %create-obj-from-spec (variant spec)
  "Creates an object from a spec by guessing wildly."
  (let ((retobj nil))
    (cond ((and (consp spec) (eq (car spec) 'obj))
	   (destructuring-bind (dummy-id &key (type nil) (id nil) (numeric-id nil) (amount 1) (no-magic nil))
	       spec
	     (declare (ignore dummy-id))
	     (when type
	       (warn "Equipment with type-spec ~s, please update to id" spec))
	     (cond ((and id (stringp id))
		    (setf retobj (create-aobj-from-id id :variant variant :amount amount)))
		   ((and numeric-id (numberp numeric-id))
		    (setf retobj (create-aobj-from-kind-num numeric-id :variant variant :amount amount)))
		   (t
		    (warn "Unable to handle obj-spec ~s" spec)))
	     (unless no-magic
	       (apply-magic! variant retobj 1 :allow-artifact nil))
	     ))
		   
	  (t
	   (warn "Don't know how to handle obj-creation from ~s" spec)))
    
    retobj))


(defmethod equip-character! ((variant variant) player settings)
  "Equips the character with basic equipment.
Triggers the events :ON-PRE-EQUIP and :ON-POST-EQUIP

The equipment specififed for class and race will be added to the
player.
"
  
  ;; trigger an event if something should be done
  ;; before character is equipped
  (trigger-event settings :on-pre-equip (list player nil))
  
  ;; first check race and class
  (let* ((race (player.race player))
	 (class (player.class player))
	 (start-eq-race (race.start-eq race))
	 (start-eq-class (class.start-eq class))
	 ;; avoid duplicate equipment
	 (start-eq (remove-duplicates (append start-eq-race start-eq-class) :test #'eql)))

;;    (warn "Trying to equip [~a,~a] with ~s" race class start-eq)
    
    (flet ((add-obj-to-player! (obj a-player)
	     "Adds the object to the player." 
	     (let* ((backpack (player.inventory a-player))
		    (inventory (aobj.contains backpack))
		    ;;(okind (aobj.kind obj))
		    )
	       ;;(warn "adding ~a to inventory ~a" obj inventory)
	       (learn-about-object! a-player obj :aware)
	       (learn-about-object! a-player obj :known) ;; know the object already
	       (item-table-add! inventory obj)))
;;	   (object-id? (arg)
;;	     (keywordp arg))
	   )

      ;; iterate over possible start-equipment
      (dolist (i start-eq)
	(let ((obj (%create-obj-from-spec variant i)))
	  (if obj
	      (add-obj-to-player! obj player)
	      (warn "Unable to find starting-object with id ~s" i))))
      

      ;; hack.. give the player some gold
      (setf (player.gold player) (random 200))

      ;; trigger an event that should be done right after equip.
      (trigger-event settings :on-post-equip (list player nil))
      )))

(defun get-string-input (prompt &key (max-length 20) (x-pos 0) (y-pos 0))
  "Non-efficient code to read input from the user, and return the string
on success.  Returns NIL on failure or user-termination (esc)." 
  (put-coloured-line! +term-white+ prompt x-pos y-pos)

  (let ((xpos (+ x-pos (length prompt)))
	(ypos y-pos)
	(wipe-str (make-string max-length :initial-element #\Space))
	(cnt 0)
	(collected '())
	(return-value nil))

    ;; wipe before we start to enter stuff
    (put-coloured-str! +term-dark+ wipe-str xpos ypos)
    (set-cursor-to *cur-win* :input (+ cnt xpos) ypos)
    
    (block str-input
      (loop
       (let ((val (read-one-character)))
	 ;;(warn "got ~s" val)
	 (cond ((or (eql val +escape+) #|(eql val #\Escape)|#)
		(return-from str-input nil))
	       ((eql val #\Backspace)
		(when collected
		  (setq collected (cdr collected))
		  (decf cnt)))
	       ((or (eql val #\Return) (eql val #\Newline))
		
		(setq return-value (coerce (nreverse collected) 'string))
		     (return-from str-input nil))
	       ((or (alphanumericp val)
		    (eql val #\-)
		    (eql val #\.))
		(push val collected)
		(incf cnt))
	       (t
		(warn "Got unknown char ~s" val)))
	    
	 ;;	    (warn "print ~s" (coerce (reverse collected) 'string))
	 (put-coloured-str! +term-dark+ wipe-str xpos ypos)
	 (put-coloured-str! +term-l-blue+ (coerce (reverse collected) 'string) xpos ypos)
	 (set-cursor-to *cur-win* :input (+ cnt xpos) ypos))))
    
    (put-coloured-line! +term-white+ "" x-pos y-pos)
    
    return-value))

(defun %get-name-input! (the-player)
  (let ((new-name (get-string-input "Enter name for your character: " :max-length 15)))
    (when (and new-name (stringp new-name))
      (setf (player.name the-player) new-name))))
	;;      (warn "Got ~s" new-name))
  

(defmethod interactive-creation-of-player ((variant variant))
  "Creates a character with interactive selection.
Returns the new PLAYER object or NIL on failure."

  (let* ((player (produce-player-object variant))
	 (birth-settings (get-setting variant :birth))
	 (note-colour (if birth-settings
			  (slot-value birth-settings 'note-colour)
			  +term-white+)))
    
    (clear-window +full-frame+)
    (refresh-window +full-frame+)

    (texture-background! +full-frame+ "textures/plainbook.png" -1)

    ;; get basics of the character
    (let ((basics (query-for-character-basics! variant player birth-settings)))
      (unless basics
	(return-from interactive-creation-of-player nil)))

    ;; now we should have a race
    (let ((rand-name (generate-random-name variant player (player.race player))))
      (when (and rand-name (stringp rand-name))
	(setf (player.name player) rand-name)))

    (unless (player.name player)
      (setf (player.name player) "Foo"))

    
    ;;do rolling
    (let ((rolling (roll-up-character! variant player)))
      (unless rolling
	(return-from interactive-creation-of-player nil)))

    (block handle-misc
      (let ((misc (player.misc player))
	    (race (player.race player))
	    (my-class (player.class player)))

	;; first do age
	(setf (playermisc.age misc) (race.base-age race))

	(etypecase (race.mod-age race)
	  (integer (incf (playermisc.age misc) (race.mod-age race)))
	  (cons (incf (playermisc.age misc) (roll-dice (car (race.mod-age race))
						       (cdr (race.mod-age race)))))
	  (function (funcall (race.mod-age race) variant player race)))

	(etypecase (class.mod-age my-class)
	  (integer (incf (playermisc.age misc) (class.mod-age my-class)))
	  (cons (incf (playermisc.age misc) (roll-dice (car (class.mod-age my-class))
						       (cdr (class.mod-age my-class)))))
	  (function (funcall (class.mod-age my-class) variant player my-class)))

	;; then fix status
	(setf (playermisc.status misc) (race.base-status race))

	(etypecase (race.mod-status race)
	  (integer (incf (playermisc.status misc) (race.mod-status race)))
	  (cons (incf (playermisc.status misc) (roll-dice (car (race.mod-status race))
							  (cdr (race.mod-status race)))))
	  (function (funcall (race.mod-status race) variant player race)))

	(etypecase (class.mod-status my-class)
	  (integer (incf (playermisc.status misc) (class.mod-status my-class)))
	  (cons (incf (playermisc.status misc) (roll-dice (car (class.mod-status my-class))
							  (cdr (class.mod-status my-class)))))
	  (function (funcall (class.mod-status my-class) variant player my-class)))

	(incf (playermisc.status misc) (roll-dice 1 (get-information "status-roll" :default 60)))

	(let ((max-status (get-information "status-cap" :default 100)))
	  (when (> (playermisc.status misc) max-status)
	    (setf (playermisc.status misc) max-status)))

	(unless (plusp (playermisc.status misc))
	  (setf (playermisc.status misc) 1)) ;; scum

	;; fix this one and move it to variant later!
	(cond ((eq (gender.symbol (player.gender player)) '<female>)
	       (setf (playermisc.height misc) (normalised-random (slot-value race 'f-height)
								 (slot-value race 'f-height-mod))
		     (playermisc.weight misc) (normalised-random (slot-value race 'f-weight)
								 (slot-value race 'f-weight-mod))))
	      (t
	       (setf (playermisc.height misc) (normalised-random (slot-value race 'm-height)
								 (slot-value race 'm-height-mod))
		     (playermisc.weight misc) (normalised-random (slot-value race 'm-weight)
								 (slot-value race 'm-weight-mod)))))
		     
      ))
    
    ;; ok.. ask for name and re-roll?

    (block input-loop
      (loop
       (display-creature variant player)
       (print-note! "['c' to change name, 'r' to re-roll stats, 'Q' to quit, ESC to continue]"
		    note-colour)

       (let ((val (read-one-character)))
	 (cond ((eql val #\Q)
		(quit-game&))
	       ;; start over
	       ((eql val #\S)
		nil)
	       ((eql val +escape+)
		(return-from input-loop t))
	       ((or (eql val #\c) (eql val #\C))
		(%get-name-input! player))
	       ((or (eql val #\r) (eql val #\R))
		(roll-up-character! variant player))
	       (t
		nil)))))

    ;; we have a new character ready for prime-time, let's flavour the objects
    (distribute-flavours! variant)
  
    ;; time to give him some equipment
    (equip-character! variant player birth-settings)

;;    (warn "stats are now ~s ~s" (player.base-stats the-player) (ok-object? the-player))
    ;;    (add-object-to-inventory! the-player (create-aobj-from-kind-num 118))

    (texture-background! +full-frame+ "" -1)
    (clear-window +full-frame+)
    ;;(warn "going switch");
    
    ;;(warn "switched")
    
    player))

