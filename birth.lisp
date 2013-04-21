;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: birth.lisp - character creation
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the character creation code.  needs clean-up

|#

(in-package :org.langband.engine)


(defun %birth-input-char (alt-len)
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
			    (c-bell! (format nil "Invalid value: ~a" val))
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


(defun %birth-input (col row alternatives
		     &key (display-fun nil) (mod-value 5) (bottom-display 10)
		     (ask-for "value"))
  "INTERNAL FUNCTION.
The COL, ROW argument specifies where the alternatives should start
ALTERNATIVES is a list of valid alternatives
DISPLAY-FUN is a function to display help for a given option
MOD-VALUE is how much space should be between rows (I think)
BOTTOM-DISPLAY means something

[add more info]
"
  
  (let ((alt-len (length alternatives)))
    (labels ((display-alternatives (highlight-num)
	       (let ((desc (when display-fun
			     (funcall display-fun highlight-num))))

		 (c-clear-from! bottom-display) ;; clears things
		 
		 (when desc
		   (c-print-text! 2 bottom-display +term-white+ desc :end-col 75)
		   ;;   (c-term-putstr! 2 10 -1 +term-white+ "<no description>"))
		   ))
		 
		 (loop for cur-alt in alternatives
		       for i from 0
		       for the-col = (truncate (+ col (* 15 (mod i mod-value))))
		       for the-row = (truncate (+ 1 row (/ i mod-value)))
		       for cur-bg  = (if (= i highlight-num) +term-l-blue+ +term-white+)
		       do
		       (c-term-putstr! the-col the-row -1 +term-white+
				       (format nil "~c) " (i2a i)))
		       (c-term-putstr! (+ 3 the-col) the-row -1 cur-bg
				       (format nil "~a" cur-alt))
		       ))
	     
	     (get-a-value (cur-sel)
	       (display-alternatives cur-sel)
	       (c-term-putstr! col row -1 +term-l-red+
			       (format nil "Choose a ~a (~c-~c, or * for random): "
				       ask-for (i2a 0) (i2a (- alt-len 1))))
	       (let ((rval (%birth-input-char alt-len)))
		 (if (not (symbolp rval))
		     rval
		     (case rval
		       (bad-value (get-a-value cur-sel)) ;; retry
		       (current cur-sel)                 ;; return current
		       (up    (get-a-value (mod (- cur-sel mod-value) alt-len))) ;; move selection
		       (down  (get-a-value (mod (+ cur-sel mod-value) alt-len))) ;; move selection
		       (left  (get-a-value (mod (- cur-sel 1) alt-len))) ;; move selection
		       (right (get-a-value (mod (+ cur-sel 1) alt-len))) ;; move selection
		       (t
			(warn "Unknown symbol returned ~s" rval)))))

	       ))
      
      (get-a-value 0))))



(defmethod query-for-character-basics! ((variant variant) the-player)
  "Interactive questioning to select the basics of the character.
Modififes the passed player object THE-PLAYER.  This is a long function."

  (clear-the-screen!)

  ;; print info on process in upper right corner
  (c-print-text! 23 2 +term-white+
		#.(concatenate 'string
			       "Please answer the following questions.  "
			       "Legal answers are shown below the red question.  You may use " 
			       "arrow-keys to highlight answers and display description, "
			       "or you may hit 'Q' to quit, 'S' to start all over or '?' " 
			       "to enter the generic help-system."))

  (let ((info-col 2)
	(info-row 8)
	(info-colour +term-l-green+)
	(quest-row 21)
	(quest-col 2))
  
    ;; First we do the player gender
    (c-clear-from! info-row) ;; clears things
    ;; Print extra-info Extra info 
    (c-term-putstr! info-col info-row -1 info-colour
		    "Your 'gender' does not have any significant gameplay effects.")

    (block input-loop
      (loop
       (let* ((genders (variant.genders variant))
	      (alt-len (length genders))
	      (inp (%birth-input quest-col quest-row
				 (mapcar #'gender.name genders)
				 :ask-for "gender")))
	 (cond ((eq inp nil)
		(return-from query-for-character-basics! nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.gender the-player) (nth inp genders))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from input-loop ~s, must be [0..~s)" inp alt-len))
	       ))))
   
    (c-put-str! "Gender" 3 1)
    (c-col-put-str! +term-l-blue+ (get-gender-name the-player) 3 8)

    
    (c-clear-from! info-row) ;; clears things
   
    (c-term-putstr! info-col info-row  -1 info-colour
		   "Your 'race' determines various intrinsic factors and bonuses.")

    (block input-loop
      (loop
       (let* ((cur-races (get-races-as-a-list))
	      (alt-len (length cur-races))
	      (inp (%birth-input quest-col quest-row
				 (mapcar #'race.name cur-races)
				 :display-fun #'(lambda (x)
						  (when (and (numberp x) (>= x 0) (< x alt-len))
						    (race.desc (elt cur-races x))))
				 :ask-for "race"
				 )))
	      
	 (cond ((eq inp nil)
		(return-from query-for-character-basics! nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.race the-player) (nth inp cur-races))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from race input-loop ~s, must be [0..~s)" inp alt-len))
	       ))))


      (c-put-str! "Race" 4 1)
      (c-col-put-str! +term-l-blue+ (get-race-name the-player) 4 8)

      (c-clear-from! info-row) ;; clears things

      ;; time to do classes.. slightly more tricky

      (c-term-putstr! info-col info-row -1 info-colour
		     "Your 'class' determines various intrinsic abilities and bonuses.")
      (c-term-putstr! info-col (1+ info-row) -1 info-colour
		      "Any entries with a (*) should only be used by advanced players.")
      

   
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
		  (inp (%birth-input quest-col quest-row
				     class-names
				     :display-fun #'(lambda (x)
						      (when (and (numberp x) (>= x 0) (< x comb-class-len))
							(class.desc (elt combined-classes x))))
				     :mod-value 3
				     :ask-for "class"
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



  (c-put-str! "Class" 5 1)
  (c-col-put-str! +term-l-blue+ (get-class-name the-player) 5 8)

  (c-clear-from! info-row) ;; clears things
  
   
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
  
  (c-clear-from! 10)
  
  (roll-stats! variant player)

  (let ((hit-dice (+ (race.hit-dice (player.race player))
		     (class.hit-dice (player.class player)))))

    ;; first level we have max
    (setf (aref (player.hp-table player) 0) hit-dice)
    (setf (maximum-hp player) hit-dice
	  (current-hp player) hit-dice))

  (update-xp-table! variant player) ;; hack

  ;; improve?  hackish.
  (calculate-creature-bonuses! variant player)
  
;;  (c-pause-line *last-console-line*)
  
  t)

(defun %create-obj-from-spec (variant spec)
  "Creates an object from a spec by guessing wildly."
  (cond ((and (consp spec) (eq (car spec) 'obj))
	 (destructuring-bind (dummy-id &key (type nil) (id nil) (numeric-id nil) (amount 1))
	     spec
	   (declare (ignore dummy-id))
	   (cond ((and type (or (symbolp type) (consp type)))
		  (let ((objs (objs-that-satisfy type :var-obj variant)))
		    (cond ((not objs)
			   (warn "Did not find any objects satisfying ~s" type))
;;			  ((typep objs 'object-kind)
;;			   (create-aobj-from-kind objs :variant variant :amount amount))
			  ((consp objs)
			   (create-aobj-from-kind (rand-elm objs) :variant variant :amount amount))
			  #-cmu
			  (t
			   (warn "Fell through with object-type ~s -> ~s" type objs)))))
		 ((and id (stringp id))
		  (create-aobj-from-id id :variant variant :amount amount))
		 ((and numeric-id (numberp numeric-id))
		  (create-aobj-from-kind-num numeric-id :variant variant :amount amount))
		 (t
		  (warn "Unable to handle obj-spec ~s" spec)))))
	(t
	 (warn "Don't know how to handle obj-creation from ~s" spec))))


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
    
    (flet ((add-obj-to-player! (obj pl)
	     "Adds the object to the player." 
	     (let* ((backpack (player.inventory pl))
		    (inventory (aobj.contains backpack))
		    ;;(okind (aobj.kind obj))
		    )
	       ;;(warn "adding ~a to inventory ~a" obj inventory)
	       (learn-about-object! pl obj :aware)
	       (learn-about-object! pl obj :known) ;; know the object already
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
  (c-prt! prompt x-pos y-pos)

  (let ((xpos (+ x-pos (length prompt)))
	(ypos y-pos)
	(wipe-str (make-string max-length :initial-element #\Space))
	(cnt 0)
	(collected '())
	(return-value nil))

    ;; wipe before we start to enter stuff
    (c-term-putstr! xpos ypos -1 +term-dark+ wipe-str)
    (c-term-gotoxy! (+ cnt xpos) ypos)
    
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
		    (eql val #\-))
		(push val collected)
		(incf cnt))
	       (t
		(warn "Got unknown char ~s" val)))
	    
	 ;;	    (warn "print ~s" (coerce (reverse collected) 'string))
	 (c-term-putstr! xpos ypos -1 +term-dark+ wipe-str)
	 (c-term-putstr! xpos ypos -1 +term-l-blue+ (coerce (reverse collected) 'string))
	 (c-term-gotoxy! (+ cnt xpos) ypos))))
    
    (c-prt! "" x-pos y-pos)
    
    return-value))

(defun %get-name-input! (the-player)
  (let ((new-name (get-string-input "Enter name for your character: " :max-length 15)))
    (when (and new-name (stringp new-name))
      (setf (player.name the-player) new-name))))
	;;      (warn "Got ~s" new-name))
  

(defmethod interactive-creation-of-player ((variant variant))
  "Creates a character with interactive selection.
Returns the new PLAYER object or NIL on failure."

  (let ((the-player (produce-player-object variant))
	(birth-settings (get-setting variant :birth)))

   
    ;; get basics of the character
    (let ((basics (query-for-character-basics! variant the-player)))
      (unless basics
	(return-from interactive-creation-of-player nil)))

    ;; now we should have a race
    (let ((rand-name (generate-random-name variant the-player (player.race the-player))))
      (when (and rand-name (stringp rand-name))
	(setf (player.name the-player) rand-name)))

    (unless (player.name the-player)
      (setf (player.name the-player) "Foo"))

    
    ;;do rolling
    (let ((rolling (roll-up-character! variant the-player)))
      (unless rolling
	(return-from interactive-creation-of-player nil)))



    ;; ok.. ask for name and re-roll?

    (block input-loop
      (loop
       (display-creature variant the-player)
       (c-prt! "['c' to change name, 'r' to re-roll stats, 'Q' to quit, ESC to continue]"
	       *last-console-line* 2)
       (let ((val (read-one-character)))
	 (cond ((eql val #\Q)
		(quit-game&))
	       ;; start over
	       ((eql val #\S)
		nil)
	       ((eql val +escape+)
		(return-from input-loop t))
	       ((or (eql val #\c) (eql val #\C))
		(%get-name-input! the-player))
	       ((or (eql val #\r) (eql val #\R))
		(roll-up-character! variant the-player))
	       (t
		nil)))))

    ;; we have a new character ready for prime-time, let's flavour the objects
    (distribute-flavours! variant)
  
    ;; time to give him some equipment
    (equip-character! variant the-player birth-settings)

;;    (warn "stats are now ~s ~s" (player.base-stats the-player) (ok-object? the-player))
    ;;    (add-object-to-inventory! the-player (create-aobj-from-kind-num 118))
    
    the-player))
