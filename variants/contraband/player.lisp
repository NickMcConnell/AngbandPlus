;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/player.lisp - code dealing with player object
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defmethod generate-random-name ((variant contraband) creature race)
  "NulNulNix")

(defmethod get-class-tile ((variant contraband) player)
  (cond ((eq (race.symbol (player.race player)) '<atrocitan>)
	 ;;(values 7 10)
	 (values +tilefile-classes+ 6))

	((eq (race.symbol (player.race player)) '<copian>)
	 (values 7 1))
	(t
	 (values +tilefile-classes+ 6))))

(defmethod get-character-picture ((variant contraband) (player player))
  (let (;;(race-sym   (race.symbol   (player.race player)))
	;;(class-sym  (class.symbol  (player.class player)))
	(gender-sym (gender.symbol (player.gender player))))

    (cond ((eq gender-sym '<male>)
	   '(variant-gfx "people/male-copian-spy.png"))
	  ((eq gender-sym '<female>)
	   '(engine-gfx "people/female-human-bard.png")))))

(defmethod interactive-creation-of-player ((variant contraband))

  (con/show-intro)
  
  (call-next-method))

(defmethod initialise-character-class! ((var-obj contraband) (my-class character-class) keyword-args)

  (call-next-method)

  (when-bind (skills (getf keyword-args :skills))
    (setf (class.skills my-class) skills)) ;;(build-skills-obj-from-list var-obj skills)))

  my-class)


(defmethod produce-player-object ((variant contraband))
  (let ((player (call-next-method)))

    ;;(warn "produce..")

    (let ((table (make-hash-table :test #'equal)))
      (loop for x across (variant.skills variant)
	    do
	    (when x
	      (setf (gethash (con/skill.slot x) table) 0)))

      (setf (player.skills player) table))
		     
    (when-bind (backpack (create-aobj-from-id "backpack"))
      ;;(warn "Adding ~s" backpack)
      (let ((eq-slots (player.equipment player)))
	(item-table-add! eq-slots backpack 'eq.backpack)
	(setf (player.inventory player) backpack)))

    

    #||
    ;; ensure that we have hash-tables in place
    (unless (hash-table-p (player.calc-attrs player))
      (setf (player.calc-attrs player) (make-hash-table :test #'eq)))
    (unless (hash-table-p (player.temp-attrs player))
      (setf (player.temp-attrs player) (make-hash-table :test #'eq)))
    ||#
    
    player))


(defmethod query-for-character-basics! ((variant contraband) (player player)
					(settings birth-settings))
  "Interactive questioning to select the basics of the character.
Modififes the passed player object THE-PLAYER.  This is a long function."

  (let* (;;(info-col (slot-value settings 'info-x))
	 (info-row (slot-value settings 'info-y))
	 (instr-col (slot-value settings 'instr-x))
	 (instr-row (slot-value settings 'instr-y))
	 (instr-colour (slot-value settings 'instr-attr))
	 (instr-width (slot-value settings 'instr-w))
	 (choice-col (slot-value settings 'choice-x))
	 (choice-row (slot-value settings 'choice-y))
	 (win *cur-win*))
	 

    (clear-window +full-frame+)
    
    ;; print info on process in upper right corner
    (print-text! instr-col instr-row instr-colour
		 #.(concatenate 'string
				"Please answer the following questions.  "
				"Legal answers are shown below the marked question.  You may use " 
				"arrow-keys to highlight answers and display description, "
				"or you may hit 'Q' to quit, 'S' to start all over or '?' " 
				"to enter the generic help-system.")
		 :end-col instr-width)
  
    (clear-window-from win info-row) ;; clears things

    (unless (%query-for-gender variant player settings)
      (return-from query-for-character-basics! nil))
    
    (clear-window-from win info-row) ;; clears things

    (unless (%query-for-race variant player settings :race-name "Nationality")
      (return-from query-for-character-basics! nil))


    (cond ((eq (race.symbol (player.race player)) '<atrocitan>)
	   (setf (player.class player) (gethash "diplomat" (variant.classes variant))))
	  ((eq (race.symbol (player.race player)) '<copian>)
	   (setf (player.class player) (gethash "spy" (variant.classes variant))))
	  (t
	   (error "Unknown race ~s" (player.race player))))


    
    (clear-window +full-frame+)
    (refresh-window +full-frame+)

    ;; let's figure out the skill-basics
    (let* ((the-class (player.class player))
	   (skills (class.skills the-class)))

      (unless (consp skills)
	(warn "No info on skills for class."))

      (when (consp skills)
	(let ((skill-table (variant.skills variant)))
	  (dolist (i skills)
	    (let ((s-obj (find (first i) skill-table :key #'(lambda (x) (when x
									  (con/skill.alias x))))))
	      (unless s-obj
		(warn "Unable to find skill ~s" (first i)))
	      (when s-obj
		(setf (gethash (con/skill.slot s-obj) (player.skills player)) (second i)))))))
      )
    
    ;;(clear-window-from win info-row) ;; clears things

    (interactive-skillpoint-distribution variant player settings)
    
;;    (clear-window-from win info-row) ;; clears things


    ;;(check-type (player.class player) 'character-class)

    #||
    (put-coloured-str! +term-white+  "Class" choice-col (+ 2 choice-row))
    (put-coloured-str! +term-l-blue+ (get-class-name player)
		       (+ 7 choice-col) (+ 2 choice-row))
    
    
    (unless (%query-for-class variant player settings)
      (return-from query-for-character-basics! nil))
    ||#
    
;;    (clear-window-from win info-row) ;; clears things

    (clear-window +full-frame+)
    (refresh-window +full-frame+)

    
    t))

(defun interactive-skillpoint-distribution (variant player settings)

  
  (let* ((skill-table (variant.skills variant))
	 (objs (make-array (length skill-table) :initial-element nil))
	 (scores (make-array (length skill-table) :initial-element 0))
	 (score-diffs (make-array (length skill-table) :initial-element 0))
	 (points-to-allocate 20)
	 (left-side 15)
	 ;;(right-side 50)
	 (left-col 22)
	 (right-col 57)
	 (row 4)
	 ;;(split 30)
	 (split 0)
	 )

    (loop for x across skill-table
	  do
	  (when x
	    (let ((val (gethash (con/skill.slot x) (player.skills player))))
	      (setf (aref scores (con/skill.idx x)) val))))
    
    (flet ((display-skill (num colour)
	     (let ((x (aref objs num)))
	       (when x
		 (put-coloured-str! +term-green+ (format nil "~2,'0d" (+ (aref scores num)
									 (aref score-diffs num)))
				    (second x) (third x))
		 (put-coloured-str! colour (con/skill.id (first x)) (+ 5 (second x)) (third x)))))
	   
	   (display-points-left (row)
	     (put-coloured-str! +term-blue+ "Points left:" (1+ left-side) row)
	     (put-coloured-str! +term-green+ (format nil "~2d" points-to-allocate) (+ 14 left-side) row)
	     (put-coloured-str! +term-blue+ "===============" (1+ left-side) (1+ row)))
	   
	   (display-skill-desc (num row)
	     ;; dumbo clear
	     (dotimes (i 12)
	       (put-coloured-str! +term-red+ "                             " left-side (+ i row)))
	     (when-bind (sk (aref skill-table num))
	       (put-coloured-str! +term-blue+ "Skill: " (1+ left-side) row)
	       (put-coloured-str! +term-red+ (con/skill.id sk) (+ 8 left-side) row)
	       (print-text! left-side (+ 2 row) +term-umber+
			    (con/skill.desc sk) :end-col 43)
	       )))
	     
      (put-coloured-str! +term-blue+ "Assigning skillpoints" (1+ left-side) 2)
      ;;(put-coloured-str! +term-blue+ "=====================" (1+ left-side) 2)

      (print-text! left-side 4 +term-umber+ 
		   "Please assign skillpoints to the skills you wish to develop. 
Add a point to a skill with the '+' key and use '-' if you change your mind.  Move 
up and down the skill-list with the arrowkeys.  Hit ESC or 'Q' when 
you're done.  Information about a skill will be shown below.  Leftover points 
can be assigned later." :end-col 43)

    
      (loop for i from 0
	    for x across skill-table
	    for left = nil ;;(<= i split) 
	    when x do
	    (progn
	      (setf (aref objs i) (list x (if left left-col right-col)
					(if left (+ row i) (+ row i (- split)))))))


      (dotimes (i (length objs))
	(display-skill i +term-blue+))

      (block award-points
	(let ((current 0)
	      (last nil)
	      (max (1- (length objs))))
	  (loop
	   (display-skill current +term-l-red+)
	   (display-points-left 20)
	   (display-skill-desc current 23)
	   
	   (setf last current)
	   (let ((key-input (read-one-character)))
	     (case key-input
	       (#\8 (decf current))
	       (#\2 (incf current))
	       (#\+ (when (plusp points-to-allocate)
		      (incf (aref score-diffs current))
		      (decf points-to-allocate)))
	     
	       (#\- (when (plusp (aref score-diffs current))
		      (decf (aref score-diffs current))
		      (incf points-to-allocate)))
	     
	       (#\Escape (return-from award-points nil))
	       (#\Q (return-from award-points nil))
	       )

	     (when (minusp current)
	       (setf current max))
	     (when (> current max)
	       (setf current 0))

	     ;;(warn "current is ~s, last is ~s, points left ~s" current last points-to-allocate)

	     ;; change colour if there has been a change
	     (if (plusp (aref score-diffs last))
		 (display-skill last +term-red+)
		 (display-skill last +term-blue+))
	     )
	   )))
      )

    ;; add diffs here
    (loop for i from 0
	  for x across score-diffs
	  for s-o = (aref skill-table i)
	  do
	  (when s-o
	    (incf (gethash (con/skill.slot s-o) (player.skills player))
		  (aref score-diffs i))))
    ;;(warn "player.skills ~s" (player.skills player))

    (pause-last-line!)
    
    t))


(defmethod on-new-player ((variant contraband) (player player))
  ;; in contraband we add a quest right away, don't we?

  ;;(warn "cur-win is ~s and has bg ~s" *cur-win* (window.background *cur-win*))
  ;;(%print-imagelist)
  
  (let ((win *cur-win*)
	(text-colour +term-umber+)
	(end-col 95)
	)
    
    (clear-window win)
    (refresh-window win)
    
    (texture-background! win "textures/paper-bg.png" -1)
    (clear-window win)
    (refresh-window win)

    (put-coloured-line! text-colour (format nil "To ~a," (player.name player)) 8 2)

    ;; move this shit somewhere else..
    (when (eq (race.symbol (player.race player)) '<copian>)
      (print-text! 8 4 text-colour
		   "Congratulations on being appointed Imperial Agent, I hear you have impressed your trainers. After the sudden death of King Aequus in the Kingdom of Atrocitas, the Empire of Copia needs updated information about what happens in Atrocitas. Since you've repeatedly asked for a 'real mission', you'll get one at the border areas where a lot of travelers and merchants from Atrocitas pass."
		   :end-col end-col)
      
      (print-text! 8 10 text-colour
		   "You have received two sealed letters. One of them is for Imperial Mereo Ulydes in Bartertown, and he will give you further assignments. The other letter is for Imperial Mereo Junifer in Mont Renuo (Lambda Rock), and she will have further assignments for you. Titulate Mereo Junifer as General, and not as Mereo. In Bartertown you must seek out Procedo Quovis. Agent Quovis has three fingers on his left hand. You will tell him that you're trading silk, but want to learn the copper trade. Agent Quovis will be your contact with the Agency."
		   :end-col end-col)
      
      (print-text! 8 18 text-colour
		   "Your cover story is that you're a former textile trader, but lost your shop. You have little money left and you're looking for a new business.  To support your story the Agency provides you with two hundred florentins.  Running a shop in either Bartertown or Mont Renuo will make you blend in. Do not under any circumstances reveal your identity as an imperial agent to anyone not mentioned in this letter."
		   :end-col end-col)
      
      (print-text! 8 24 text-colour
		   "As an Imperial Agent you have no special privileges and any crimes you're caught for will be punished by the local government. The Agency will not interfere with the local government's right to punish criminals. The Agency reserves the right to punish you in addition for crimes against the Empire's interests."
		   :end-col end-col)
      
      (print-text! 8 29 text-colour
		   "The Most Holy Emperor Sapient wishes you all luck and hopes that you will uncover more of what happens in Atrocitas. Information about what's happening in Bartertown and Mont Renuo is also of great importance to us."
		   :end-col end-col)
      
      (put-coloured-line! +term-blue+ "<signature>" 8 33)
      (put-coloured-line! +term-umber+ "Imperial High Agent Statim Mangrevi" 8 34))

    (when (eq (race.symbol (player.race player)) '<atrocitan>)

      (print-text! 8 4 text-colour "Congratulations on finishing your studies at the Royal Academy of Atroburg. Your professors tell me you finished with honours. As we agreed last february at your father's mansion, I will be your mentor the next two years, training you in the arts of diplomacy, trade and politics. After King Aequus' death last week, things are very hectic and I send you this letter because there is no time to meet in person."
		   :end-col end-col)

      (print-text! 8 10 text-colour "When we last met, you insisted that you wanted to 'prove yourself and your worthiness'. It seems that Fate was listening to our conversation. The Kingdom, myself and our families need you to go to the two border towns Bartertown and Lambda Rock in the Empire of Copia. In Bartertown you will join the Atrocitan Consul Tepesco as a clerk and trainee. Consul Tepesco will undoubtedly give you assignments to secure our trade with Copia and secure the peace."
		   :end-col end-col)

      
      (print-text! 8 17 text-colour "The Kingdom is overrun by agents from the Empire of Copia, trying to create chaos after the King's death. We suspect that the imperial agents are trying to weaken our Kingdom so they can make another of their cowardly attacks on us. It's a bad situation, and we need you to report to us what happens on the border. We want to know about trading, schemes, copian force movements and all other information you find appropriate. With good information we can judge the situation better and avoid living in fear. Fear will only benefit the Copians."
		   :end-col end-col)
           
      (print-text! 8 25 text-colour "Atrocitans in the Empire of Copia you have no special privileges and any crimes you're caught for will be punished by Copians. The Kingdom cannot interfere with the Copian authorities on criminal matters. Be careful."
		   :end-col end-col)

      (print-text! 8 29 text-colour "Prince Callidus, who manages foreign affairs after his father's death, wishes you all luck and hopes that you will uncover more of what the Empire of Copia is up to. Our freedom is at stake."
		    :end-col end-col)
      
      (put-coloured-line! +term-blue+ "<signature>" 8 33)
      (put-coloured-line! +term-umber+ "Duke Larethian, Minister of the Foreign Office" 8 34))

    (pause-at-line! (1- (get-last-console-line)) :msg "[Press any key to destroy letter and continue]"
		    :attr +term-green+)
    ;;(pause-last-line! :attr +term-green+)

    (texture-background! win "" -1)
    (clear-window win)
    )

  ;;(%print-imagelist)
  
  ;; find letter-quest, add to player, activate quest
  ;;(warn "Nuevo playeras")
  (cond ((eq (race.symbol (player.race player)) '<copian>)
	 (dolist (i '("deliver-letter-to-junifer" "deliver-letter-to-ulydes"))
	   (let ((quest (find-quest variant i)))
	     (init-quest variant quest nil player))))
	
	((eq (race.symbol (player.race player)) '<atrocitan>)
	 (dolist (i '("deliver-letter-to-tepesco"))
	   (let ((quest (find-quest variant i)))
	     (init-quest variant quest nil player)))
	 (add-to-inventory player (get-new-object "facts-machine")))
	 
	(t (error "Uknown nationality.")))
  
  player)
