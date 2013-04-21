;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

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

(in-package :langband)

;; this file deals with character creation

(defun create-character-basics! (the-player)
  "Interactive questioning to select the basics of the character.
Modififes the given player object."
  (clear-the-screen!)
    
  ;; Display some helpful information 
  (c-term-putstr! 5 10 -1 +term-white+
		 "Please answer the following questions.  Most of the questions")
  (c-term-putstr! 5 11 -1 +term-white+
		 "display a set of standard answers, and many will also accept")
  (c-term-putstr! 5 12 -1 +term-white+
		 "some special responses, including 'Q' to quit, 'S' to restart,")
  (c-term-putstr! 5 13 -1 +term-white+
		 "and '?' for help.  Note that 'Q' and 'S' must be capitalized.")


  ;; Player sex

  ;; Extra info 
  (c-term-putstr! 5 15 -1 +term-white+
		 "Your 'sex' does not have any significant gameplay effects.")

  (let ((sex-len (length +sexes+)))
    (dotimes (i sex-len)
      (c-put-str! (format nil "~c) ~a" (i2a i) (cadr (nth i +sexes+)))
		 (truncate (+ 21 (/ i 5)))
		 (truncate (+ 2 (* 15 (mod i 5))))))

    (block input-loop
      (loop

       (c-put-str! (format nil "Choose a sex (~c-~c, or * for random): "
			  (i2a 0) (i2a (- sex-len 1)))
		  20 2)
  
       (let ((val (read-one-character)))
;;	 (warn "Got back ~a ~s ~s" val val (type-of val))
	 (cond ((eql val #\Q) (c-quit! +c-null-value+))
	       ((eql val #\S) (return-from create-character-basics! nil))
	       ((eql val +escape+)
		(setf (player.sex the-player) (car (nth (random sex-len) +sexes+)))
		(return-from input-loop))
	       (t
		(let ((r-val (- (char-code val) (char-code #\a))))
		  (if (and (>= r-val 0) (< r-val sex-len))
		      (progn
			(setf (player.sex the-player) (car (nth r-val +sexes+)))
			(return-from input-loop))
		      (c-bell! "Illegal sex!"))))))
       ))
   
    (c-put-str! "Sex" 3 1)
    (c-col-put-str! +term-l-blue+ (get-sex-name the-player) 3 8)

    (c-clear-from! 15)
   
    (c-term-putstr! 5 15  -1 +term-white+
		   "Your 'race' determines various intrinsic factors and bonuses.")

    (let* ((cur-races (get-races-as-a-list))
	   (race-len (length cur-races)))
      (loop for i from 0
	    for elt in cur-races
	    do
	    (c-put-str! (format nil "~a) ~a" (i2a i) (race.name elt))
		       (truncate (+ 21 (/ i 5)))
		       (truncate (+ 2 (* 15 (mod i 5))))))

      (block input-loop
	(loop
	 (c-put-str! (format nil "Choose a race (~c-~c, or * for random): "
			    (i2a 0) (i2a (- race-len 1)))
		    20 2)

	 (let ((val (read-one-character)))
	   (cond ((eql val #\Q) (c-quit! +c-null-value+))
		 ((eql val #\S) (return-from create-character-basics! nil))
		 ((eql val +escape+)
		  (setf (player.race the-player) (nth (random race-len) cur-races))
		  (return-from input-loop))
		 (t
		  (let ((r-val (- (char-code val) (char-code #\a))))
		    (if (and (>= r-val 0) (< r-val race-len))
			(progn
			  (setf (player.race the-player) (nth r-val cur-races))
			  (return-from input-loop))
			(c-bell! "Illegal race!"))))))
     
	 ))
   
      (c-put-str! "Race" 4 1)
      (c-col-put-str! +term-l-blue+ (get-race-name the-player) 4 8)

      (c-clear-from! 15)

      ;; time to do classes.. slightly more tricky

      (c-term-putstr! 5 15 -1 +term-white+
		     "Your 'class' determines various intrinsic abilities and bonuses.")
      (c-term-putstr! 5 16 -1 +term-white+
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
		
	(loop for i from 0
	      for elt in combined-classes
	      do
	      (c-put-str! (format nil "~a) ~a" (i2a i)
				 (if (>= i class-len)
				     (concatenate 'string "(" (class.name elt) ")")
				     (class.name elt)))
			 (floor (+ 21 (int-/ i 3)))
			 (floor (+ 2 (* 20 (mod i 3))))))

	(block input-loop
	  (loop 
	   (c-put-str! (format nil "Choose a class (~c-~c, or * for random): "
			      (i2a 0) (i2a (- comb-class-len 1)))
		      20 2)


	   (let ((val (read-one-character)))
	     (cond ((eql val #\Q) (c-quit! +c-null-value+))
		   ((eql val #\S) (return-from create-character-basics! nil))
		   ((eql val +escape+)
		    (setf (player.class the-player) (nth (random comb-class-len) combined-classes))
		    (return-from input-loop))
		   (t
		    (let ((r-val (- (char-code val) (char-code #\a))))
		      (if (and (>= r-val 0) (< r-val comb-class-len))
			  (progn
			    (setf (player.class the-player) (nth r-val combined-classes))
			    (return-from input-loop))
			  (c-bell! "Illegal class!"))))))

	   )))))
   

  (c-put-str! "Class" 5 1)
  (c-col-put-str! +term-l-blue+ (get-class-name the-player) 5 8)
  
  (c-clear-from! 15)
	 
   
  t)

(defun roll-stats! (player)
  "Rolls stats and modifies given player object."
  
  (setf (player.base-stats player) #1A(0 0 0 0 0 0))
  (setf (player.curbase-stats player) #1A(0 0 0 0 0 0))
  (setf (player.modbase-stats player) #1A(0 0 0 0 0 0))
  (setf (player.active-stats player) #1A(0 0 0 0 0 0))
  
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
		    (inventory (aobj.contains backpack)))
	       ;;(warn "adding ~a to inventory ~a" obj inventory)
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

;;    (add-object-to-inventory! the-player (create-aobj-from-kind-num 118))
    
    the-player))

