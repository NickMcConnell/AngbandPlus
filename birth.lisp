;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: birth.lisp - character creation
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the character creation code.  needs clean-up

|#

(in-package :org.langband.engine)


(defun %birth-input-char (alt-len)
  (let ((val (read-one-character)))
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


;; this file deals with character creation


(defun create-character-basics! (the-player)
  "Interactive questioning to select the basics of the character.
Modififes the given player object."
  (clear-the-screen!)

  ;; in upper right corner
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
  
    ;; Player sex

    (c-clear-from! info-row) ;; clears things
    ;; Extra info 
    (c-term-putstr! info-col info-row -1 info-colour
		    "Your 'sex' does not have any significant gameplay effects.")

    (block input-loop
      (loop
       (let ((alt-len (length +sexes+))
	     (inp (%birth-input quest-col quest-row
				(mapcar #'cadr +sexes+)
				:ask-for "sex")))
	 (cond ((eq inp nil)
		(return-from create-character-basics! nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.sex the-player) (car (nth inp +sexes+)))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from input-loop ~s, must be [0..~s)" inp alt-len))
	       ))))
   
    (c-put-str! "Sex" 3 1)
    (c-col-put-str! +term-l-blue+ (get-sex-name the-player) 3 8)

    
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
		(return-from create-character-basics! nil))
	       
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
		   (if (find (class.id i) cur-classes :test #'eq)
		       (push i tmp-classes)
		       (push i other-classes)))
		 (setq cur-classes tmp-classes)))
	      (t
	       (warn "Unknown classes ~a for race ~a" cur-classes (get-race-name the-player))
	       (return-from create-character-basics! nil)))

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
		    (return-from create-character-basics! nil))
		   
		   ((and (numberp inp) (<= 0 inp) (< inp comb-class-len))
		    (setf (player.class the-player) (nth inp combined-classes))
		    (return-from input-loop nil))
		   
		   (t
		    (warn "Unknown return-value from class input-loop ~s, must be [0..~s)" inp comb-class-len))
		   ))))
	)



  (c-put-str! "Class" 5 1)
  (c-col-put-str! +term-l-blue+ (get-class-name the-player) 5 8)

  (c-clear-from! info-row) ;; clears things
  
   
  t))

(defun roll-stats! (player)
  "Rolls stats and modifies given player object."
  
  (setf (player.base-stats player) (make-stat-array))
  (setf (player.curbase-stats player) (make-stat-array))
  (setf (player.modbase-stats player) (make-stat-array))
  (setf (player.active-stats player) (make-stat-array))
  
  (let* ((arr-len 18)
;;	 (bonus 0)
	 (rolls (make-array arr-len)))

    (block roller
      (while t
	(dotimes (i arr-len)
	  (setf (svref rolls i) (randint (+ 3 (mod i 3)))))
	
	(let ((sum (reduce #'+ rolls)))
;;	  (warn "We have sum ~a" sum)
	  (when (and (< 42 sum) (> 54 sum))
	    (return-from roller)))))

    (dotimes (i +stat-length+)
      (let ((stat-val (+ 5
			 (svref rolls (* 3 i))
			 (svref rolls (1+ (* 3 i)))
			 (svref rolls (1+ (1+ (* 3 i)))))))
	
	(setf (svref (player.base-stats player) i) stat-val
	      (svref (player.curbase-stats player) i) stat-val))))
  
  (player.base-stats player))

	    
  

(defun roll-up-character! (player)
  "Rolls up a character and modifies given PLAYER-object."
  ;; dropping auto-roller
  
  (c-clear-from! 10)
  
  (roll-stats! player)

  (let ((hit-dice (+ (race.hit-dice (player.race player))
		     (class.hit-dice (player.class player)))))

    ;; first level we have max
    (setf (aref (player.hp-table player) 0) hit-dice)
    (setf (player.max-hp player) hit-dice
	  (player.cur-hp player) hit-dice))

  (update-xp-table! player) ;; hack
  (update-player! player)
  (display-player player)
  
;;  (c-pause-line *last-console-line*)
  
  t)

(defun equip-character! (player settings)
  "Equips the character with basic equipment."

  (trigger-event settings :on-pre-equip (list player nil))
  
  ;; first check race and class
  (let* ((race (player.race player))
	 (class (player.class player))
	 (start-eq-race (race.start-eq race))
	 (start-eq-class (class.start-eq class))
	 (start-eq (remove-duplicates (append start-eq-race start-eq-class) :test #'eql)))

;;    (warn "Trying to equip [~a,~a] with ~s" race class start-eq)

    
    
    (flet ((add-obj-to-player! (obj)
	     (let* ((backpack (player.inventory player))
		    (inventory (aobj.contains backpack))
		    (okind (aobj.kind obj)))
	       ;;(warn "adding ~a to inventory ~a" obj inventory)
	       (setf (object.identified okind) t) ;; know the object already
	       (item-table-add! inventory obj))))

      (dolist (i start-eq)
	(if (keywordp i)
	    ;; we deal with an id
	    (let ((obj (create-aobj-from-id i)))
	      (if obj
		  (add-obj-to-player! obj)
		  (warn "Unable to find starting-object with id ~s" i)))
	    ;; we have to find something that satisifies
	    (let ((objs (objs-that-satisfy i)))
	      ;; we only want one
	      (if (not objs)
		  (warn "Did not find any objects satisfying ~s" i)
		  (let ((len (length objs))
			(the-obj nil))
		    (if (> len 1)
			;; we must just return one
			(setq the-obj (nth (random len) objs))
			;; we have just one
			(setq the-obj (car objs)))
		    (add-obj-to-player! (create-aobj-from-kind the-obj)))))))

      ;; hack.. give him some gold
      (setf (player.gold player) (random 200))
      
      (trigger-event settings :on-post-equip (list player nil))
      )))

(defun create-character ()
  "Creates a character with interactive selection.
Returns the new L-PLAYER object or NIL on failure."
  
  (let ((the-player (create-player-obj))
	(birth-settings (get-setting :birth)))
	
	
    
    ;; get basics of the character
    (let ((basics (create-character-basics! the-player)))
      (unless basics
	(return-from create-character nil)))
    
    ;;do rolling
    (let ((rolling (roll-up-character! the-player)))
      (unless rolling
	(return-from create-character nil)))

    (equip-character! the-player birth-settings)

;;    (warn "stats are now ~s ~s" (player.base-stats the-player) (ok-object? the-player))
    ;;    (add-object-to-inventory! the-player (create-aobj-from-kind-num 118))
    
    the-player))
