;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/print.lisp - printout code for vanilla
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)

(defmethod print-depth ((level level) (setting bottom-row-locations))
  "prints current depth somewhere"
  (with-frame (+misc-frame+)
    (let ((column (- (get-frame-width +misc-frame+) 8))) ;;(slot-value setting 'depth)))
      (put-coloured-line! +term-l-blue+ (format nil "~d ft" (* 50 (level.depth level))) column 0))))


(defmethod print-depth ((level van-town-level) (setting bottom-row-locations))
  "prints current depth somewhere"
  (let ((column (- (get-frame-width +misc-frame+) 8)));; (slot-value setting 'depth)))
    (with-frame (+misc-frame+)
      (put-coloured-line! +term-l-blue+ "Town"
			 column
			 0))))

(defmethod print-cut ((variant vanilla-variant) (player player) 
                      (setting vanilla-basic-frame-locations))

  (let* ((cuts (get-attribute-value '<cut> (player.temp-attrs player)))
         (loc (slot-value setting 'cut))
         (cut-info (%get-cutlvl cuts)))

    ;;(warn "print cut ~s ~s ~s" cuts cut-info loc) 
    (with-frame (+charinfo-frame+)
      (put-coloured-str! (second cut-info) (third cut-info)
			 (cdr loc) (car loc)))
    ))


(defmethod print-stun ((variant vanilla-variant) (player player) 
                      (setting vanilla-basic-frame-locations))
  
  (let* ((stun (get-attribute-value '<stun> (player.temp-attrs player)))
         (loc (slot-value setting 'stun))
         (stun-info (%get-stunlvl stun)))

    (with-frame (+charinfo-frame+)
      (put-coloured-str! (second stun-info) (third stun-info)
			 (cdr loc) (car loc)))
    ))

(defmethod print-poisoned ((variant vanilla-variant) (player player) 
                           (setting vanilla-bottom-row-locations))
  (with-frame (+misc-frame+)
    (let ((column (slot-value setting 'poisoned))
	  (row 0))

      (if (get-attribute-value '<poisoned> (player.temp-attrs player))
	  (put-coloured-str! +term-orange+ "Poisoned" column row)
	  (put-coloured-str! +term-white+  "        " column row))
      )))

(defmethod print-state ((variant vanilla-variant) (player player) 
			(setting vanilla-bottom-row-locations))

  (with-frame (+misc-frame+)
    (let ((word nil)
	  (colour +term-white+)
	  (temp-attrs (player.temp-attrs player))
	  (column (slot-value setting 'state))
	  (row 0))

      (cond ((get-attribute-value '<paralysed> temp-attrs)
	     (setf word "Paralysed!"
		   colour +term-red+))
	    ;; turn it off
	    (t
	     (setf word "          ")
	     ))
    
      (put-coloured-str! colour word column row))
    
    t))

(defmethod print-afraid ((variant vanilla-variant) (player player) 
			 (setting vanilla-bottom-row-locations))

  (with-frame (+misc-frame+)
    (let ((column (slot-value setting 'afraid))
	  (row 0))
      (if (get-attribute-value '<fear> (player.temp-attrs player))
	  (put-coloured-str! +term-orange+ "Afraid" column row)
	  (put-coloured-str! +term-white+  "      " column row))
      )))

(defmethod print-confused ((variant vanilla-variant) (player player) 
			   (setting vanilla-bottom-row-locations))
  (with-frame (+misc-frame+)
    
    (let ((column (slot-value setting 'confused))
	  (row 0))
      (if (get-attribute-value '<confusion> (player.temp-attrs player))
	  (put-coloured-str! +term-orange+ "Confused" column row)
	  (put-coloured-str! +term-white+  "        " column row))
      )))

(defmethod print-can-study-more ((variant vanilla-variant) (player player) 
				 (setting vanilla-bottom-row-locations))
  
  (with-frame (+misc-frame+)
    (let ((column (slot-value setting 'study))
	  (row 0))
      (if (can-learn-more-spells? variant player)
	  (put-coloured-str! +term-l-green+ "Study" column row)
	  (put-coloured-str! +term-white+  "     " column row))
      )))


(defmethod print-extra-frame-content ((variant vanilla-variant) (dungeon dungeon) (player player))
  (let ((pr-set (get-setting variant :basic-frame-printing))
	(bot-set (get-setting variant :bottom-row-printing)))
	
    (print-cut variant player pr-set)
    (print-poisoned variant player bot-set)
    (print-afraid variant player bot-set)
    (print-confused variant player bot-set)
    ;; more
    (print-speed variant player bot-set)
    ;; more
    (print-can-study-more variant player bot-set)
    t))



(defmethod redraw-stuff ((variant vanilla-variant) (dungeon dungeon) (player player))
  
  (when (= 0 *redraw*) (return-from redraw-stuff nil))

  (let ((retval nil)
	(pr-set nil)
        (bot-set nil))

    (when (bit-flag-set? *redraw* +print-extra+)
      (bit-flag-remove! *redraw* +print-extra+)
      (bit-flag-remove! *redraw* +print-cut+)
      (bit-flag-remove! *redraw* +print-stun+)
      (bit-flag-remove! *redraw* +print-hunger+)
      (bit-flag-remove! *redraw* +print-blind+)
      (bit-flag-remove! *redraw* +print-confused+)
      (bit-flag-remove! *redraw* +print-afraid+)
      (bit-flag-remove! *redraw* +print-poisoned+)
      (bit-flag-remove! *redraw* +print-state+)
      (bit-flag-remove! *redraw* +print-speed+)
      (bit-flag-remove! *redraw* +print-study+)
;;      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))

      (print-extra-frame-content variant dungeon player)
      (setf retval t))

    
    (when (bit-flag-set? *redraw* +print-cut+)
      (bit-flag-remove! *redraw* +print-cut+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-cut variant player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-stun+)
      (bit-flag-remove! *redraw* +print-stun+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-stun variant player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-hunger+)
      (bit-flag-remove! *redraw* +print-hunger+)
      ;; fix
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-blind+)
      (bit-flag-remove! *redraw* +print-blind+)
      ;; fix
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-confused+)
      (bit-flag-remove! *redraw* +print-confused+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-confused variant player bot-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-afraid+)
      (bit-flag-remove! *redraw* +print-afraid+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-afraid variant player bot-set)
      (setf retval t))
    

    (when (bit-flag-set? *redraw* +print-poisoned+)
      (bit-flag-remove! *redraw* +print-poisoned+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-poisoned variant player bot-set)
      (setf retval t))

    ;; state and speed in engine

    (when (bit-flag-set? *redraw* +print-study+)
      (bit-flag-remove! *redraw* +print-study+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-can-study-more variant player bot-set)
      (setf retval t))

    (when (call-next-method)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-mana+)
      (bit-flag-remove! *redraw* +print-mana+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-mana-points variant player pr-set)
      (setf retval t))


    t))


(defmethod print-mana-points ((variant vanilla-variant) player setting)
  "Prints mana-points info to left frame."

  (when (is-spellcaster? player)
    (let ((cur-hp (current-mana player))
	  (max-hp (maximum-mana player))
	  (cur-set (slot-value setting 'cur-mana))
	  (max-set (slot-value setting 'max-mana)))
      
      (output-string! +charinfo-frame+ (cdr max-set) (car max-set) +term-white+ "Max MP")
      (output-string! +charinfo-frame+ (cdr cur-set) (car cur-set) +term-white+ "Cur MP")

      (print-number +charinfo-frame+ +term-l-green+
		    max-hp
		    5
		    (car max-set)
		    (+ (cdr max-set) 7))
      
      
      (print-number +charinfo-frame+ (cond ((>= cur-hp max-hp) +term-l-green+)
					   ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
					   (t +term-red+))
		    
		    cur-hp
		    5
		    (car cur-set)
		    (+ (cdr cur-set) 7))
      
      )))

(defmethod print-basic-frame ((variant vanilla-variant) dungeon player)
  
  (call-next-method)

  (let ((pr-set (get-setting variant :basic-frame-printing)))
    (print-mana-points variant player pr-set)
    
    (bit-flag-remove! *redraw* +print-mana+)

    t))

	 

(defmethod display-player-skills ((variant vanilla-variant) player term settings)
  (declare (ignore term))
  (let* ((row (if settings
		  (slot-value settings 'skills-y)
		  10))
	 (col (if settings
		  (slot-value settings 'skills-x)
		  42))
	 (value-attr (if settings
			 (slot-value settings 'value-attr)
			 +term-l-green+))
	 (sk-attr (if settings
		      (slot-value settings 'title-attr)
		      +term-white+)))

    (flet ((print-skill (skill div row)
	     (declare (ignore div))
	     (let ((val (slot-value (player.skills player) skill)))
	       (put-coloured-str! value-attr
				  (format nil "~9d" val)
				  (+ col 14)
				  row))))
      
      (put-coloured-str! sk-attr "Saving Throw" col (+ row 0))
      (print-skill 'saving-throw 6 (+ row 0))

      (put-coloured-str! sk-attr "Stealth" col (+ row 1))
      (print-skill 'stealth 1 (+ row 1))
      
      (put-coloured-str! sk-attr "Fighting" col (+ row 2))
      (print-skill 'fighting 12 (+ row 2))

      (put-coloured-str! sk-attr "Shooting" col (+ row 3))
      (print-skill 'shooting 12 (+ row 3))

      (put-coloured-str! sk-attr "Disarming" col (+ row 4))
      (print-skill 'disarming 8 (+ row 4))

      (put-coloured-str! sk-attr "Magic Device" col (+ row 5))
      (print-skill 'device 6 (+ row 5))

      (put-coloured-str! sk-attr "Perception" col (+ row 6))
      (print-skill 'perception 6 (+ row 6))

      (put-coloured-str! sk-attr "Searching" col (+ row 7))
      (print-skill 'searching 6 (+ row 7))

      t)))

(defmethod display-player-combat-ratings ((variant vanilla-variant) player term settings)
  (declare (ignore term))

  (let* ((title-attr (if settings
			 (slot-value settings 'title-attr)
			 +term-white+))
	 (value-attr (if settings
			 (slot-value settings 'value-attr)
			 +term-l-blue+))
	 (col (if settings
		  (slot-value settings 'combat-x)
		  26))
	 (row (if settings
		  (slot-value settings 'combat-y)
		  13))
	 (f-col (+ col 7))

	 (perc (player.perceived-abilities player))
	 (p-ac (pl-ability.base-ac perc))
	 (p-acmod (pl-ability.ac-modifier perc))
	 (tohit (pl-ability.to-hit-modifier perc))
	 (dmg (pl-ability.to-dmg-modifier perc))

	 (mewpn (get-melee-weapon player))
	 (miwpn (get-missile-weapon player))

	 (megval (when mewpn (object.game-values mewpn)))
	 (migval (when miwpn (object.game-values miwpn)))

	 (mel-tohit 0)
	 (mel-dmg 0)
	 (miss-tohit 0)
	 (miss-dmg 0)
	 )

    (when megval
      (setf mel-tohit (gval.tohit-modifier megval)
	    mel-dmg (gval.dmg-modifier megval)))
    
    (when migval
      (setf miss-tohit (gval.tohit-modifier migval)
	    miss-dmg (gval.dmg-modifier migval)))
    
    
    
    ;; hack
    (decf row 13)

    (put-coloured-str! title-attr "Armour" col (+ row 13))
          
    (put-coloured-str! value-attr
		       (format nil "~12@a" (format nil "[~d,~@d]" p-ac p-acmod))
		       (1+ f-col) (+ row 13))


      
    (put-coloured-str! title-attr  "Fight" col (+ row 14))
    (put-coloured-str! value-attr (format nil "~13@a" (format nil "(~@d,~@d)" tohit dmg))
		       f-col (+ row 14))
  
    ;; skip weapon+bow specifics now
    (put-coloured-str! title-attr  "Melee" col (+ row 15))
    (put-coloured-str! value-attr (format nil "~13@a" (format nil "(~@d,~@d)" (+ tohit mel-tohit)
							      (+ dmg mel-dmg)))
		       f-col (+ row 15))
  
    (put-coloured-str! title-attr "Shoot" col (+ row 16))
    (put-coloured-str! value-attr (format nil "~13@a" (format nil "(~@d,+0)" (+ tohit miss-tohit) miss-dmg))
		       f-col (+ row 16))
  
  
  
    (put-coloured-str! title-attr  "Blows" col (+ row 17))
    (put-coloured-str! value-attr (format nil "~13@a" "1/turn")
		       f-col (+ row 17))
  
    (put-coloured-str! title-attr  "Shots" col (+ row 18))
    (put-coloured-str! value-attr (format nil "~13@a" "1/turn")
		       f-col (+ row 18))
		   
    (put-coloured-str! title-attr "Infra" col (+ row 19))
  
    (put-coloured-str! value-attr
		       (format nil "~10d ft" (* 10 (player.infravision player)))
		       f-col (+ row 19))

    t))
