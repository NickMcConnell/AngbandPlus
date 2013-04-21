;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/variant.lisp - code related to variant object
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defmethod activate-object :before ((var-obj contraband) &key)
  "Initialises variant-variables that should be there before
the rest of the game is init'ed."
  
  (setf (variant.images var-obj) (make-array 64 :initial-element nil))

  ;; get the images init'ed right away
  (loop for i from 0
	for x across *contraband-images*
	do
	(cond ((and (consp x) (stringp (second x)))
	       (when (plusp (length (second x)))
		 (load-image& var-obj x i #xffffff00)))
	      ((and (stringp x) (= (length x) 0))
	       nil)
	      (t
	       (error "Unknown image-spec ~s" x))))

  
  (pushnew (make-gender :id "male" :symbol '<male> :name "Male" :win-title "King")
	   (variant.genders var-obj) :test #'eql :key #'gender.symbol)
  (pushnew (make-gender :id "female" :symbol '<female> :name "Female" :win-title "Queen")
	   (variant.genders var-obj) :test #'eql :key #'gender.symbol)

  ;; fix a lot of this later..
   (flet ((help-path (file)
	   (concatenate 'string *engine-source-dir* "docs/help/" file)))
  

    (register-help-topic& var-obj
			  (make-help-topic :id "keys" :key #\k
					   :name "Show commands/keys"
					   :data (help-path "keys.txt")))
    
    (register-help-topic& var-obj
			  (make-help-topic :id "general" :key #\g
					   :name "General information"
					   :data (help-path "general.txt")))

    (register-help-topic& var-obj
			  (make-help-topic :id "dungeon" :key #\d
					   :name "Simple information about the dungeons"
					   :data (help-path "dungeon.txt")))

    (register-help-topic& var-obj
			  (make-help-topic :id "birth" :key #\b
					   :name "Information about creating a character"
					   :data (help-path "birth.txt")))

    (register-help-topic& var-obj
			  (make-help-topic :id "playing" :key #\p
					   :name "Tips and hints on how to play langband"
					   :data (help-path "playing.txt")))

    (register-help-topic& var-obj
			  (make-help-topic :id "thanks" :key #\t
					   :name "Who made Langband possible"
					   :data (help-path "THANKS")))
    (register-help-topic& var-obj
			  (make-help-topic :id "version" :key #\v
					   :name "Show version information"
					   :data (help-path "version.txt"))))
 

  ;; register help
  ;; register levels
  ;; register skills
  ;; register combat
  
  (let ((*load-verbose* nil))
    (load-variant-data& var-obj "defines")
    (load-variant-data& var-obj "sound")
    (load-variant-data& var-obj "character")
    (load-variant-data& var-obj "skills")
    (load-variant-data& var-obj "keys")
    (load-variant-data& var-obj "dialogues")
    )

  (contra-init-eq-system var-obj)

  var-obj)

(defmethod activate-object ((var-obj contraband) &key)
  
  (let ((*load-verbose* nil))
    (initialise-monsters& var-obj :file "townies")
    (initialise-monsters& var-obj :file "leaders")
    (initialise-objects& var-obj :file "objects")

    ;; hack, done after objects and monsters are ok'ed.. maybe move to :after
    (load "variants/contraband/tasks/letters.lisp")
    (load "variants/contraband/tasks/dress.lisp")
    )
  
  ;; more hackish
  (let ((pairs '())
	(table (variant.quests var-obj)))
    (maphash #'(lambda (k v) (push (cons k v) pairs)) table)
    (clrhash table)
    (dolist (i pairs)
      (setf (gethash (car i) table) (make-instance (cdr i))))
    (setf (variant.quests var-obj) table))
    
  
  var-obj)

(defmethod activate-object :after ((var-obj contraband) &key)
  "Does post-initialisation of the game with variant tweaking."

  (let ((birth-settings (make-instance 'con-birth :allow-all-classes t))
	(chdisp-settings (make-instance 'chardisplay-settings)))
    
    (when (eq (get-system-type) 'sdl)
      (setf (slot-value birth-settings 'instr-x) 15
	    (slot-value birth-settings 'instr-y) 3
	    (slot-value birth-settings 'instr-attr) +term-blue+
	    (slot-value birth-settings 'instr-w) 45
	    (slot-value birth-settings 'query-x) 15
	    (slot-value birth-settings 'query-y) 24
	    (slot-value birth-settings 'query-reduced) t
	    (slot-value birth-settings 'query-attr) +term-blue+
	    (slot-value birth-settings 'info-x) 15
	    (slot-value birth-settings 'info-y) 16
	    (slot-value birth-settings 'info-attr) +term-umber+
	    (slot-value birth-settings 'choice-x) 52
	    (slot-value birth-settings 'choice-y) 3
	    (slot-value birth-settings 'choice-tattr) +term-blue+
	    (slot-value birth-settings 'choice-attr) +term-l-red+
	    (slot-value birth-settings 'text-x) 52
	    (slot-value birth-settings 'text-y) 7
	    (slot-value birth-settings 'text-w) 35
	    (slot-value birth-settings 'text-attr) +term-umber+
	    (slot-value birth-settings 'altern-cols) 2
	    (slot-value birth-settings 'altern-attr) +term-umber+
	    (slot-value birth-settings 'altern-sattr) +term-l-red+
	    (slot-value birth-settings 'note-colour) +term-white+
	    )

      (setf (slot-value chdisp-settings 'title-x) 15
	    (slot-value chdisp-settings 'title-y) 10
	    (slot-value chdisp-settings 'title-attr) +term-blue+
	    (slot-value chdisp-settings 'picture-x) 25
	    (slot-value chdisp-settings 'picture-y) 2
	    (slot-value chdisp-settings 'extra-x) 15
	    (slot-value chdisp-settings 'extra-y) 18
	    (slot-value chdisp-settings 'elem-x) 15
	    (slot-value chdisp-settings 'elem-y) 24
	    (slot-value chdisp-settings 'value-attr) +term-green+
	    (slot-value chdisp-settings 'value-badattr) +term-red+
	    (slot-value chdisp-settings 'stats-attr) +term-blue+
	    (slot-value chdisp-settings 'statok-attr) +term-umber+
	    (slot-value chdisp-settings 'statbad-attr) +term-l-red+
	    (slot-value chdisp-settings 'stats-x) 53
	    (slot-value chdisp-settings 'skills-x) 53
	    (slot-value chdisp-settings 'combat-x) 53
	    (slot-value chdisp-settings 'combat-y) 20
	    )

      
      (setf (get-setting var-obj :birth) birth-settings
	    (get-setting var-obj :char-display) chdisp-settings
	    )))


  (setf (get-setting var-obj :basic-frame-printing)
	(make-instance 'basic-frame-locations))
  
  (setf (get-setting var-obj :bottom-row-printing)
	(make-instance 'bottom-row-locations))

  
  (register-level-builder! "town-level"
			   (get-late-bind-function 'langband
						   'create-bare-town-level-obj))
  
  
  var-obj)


(defmethod redraw-stuff ((variant contraband) (dungeon dungeon) (player player))
  
  (when (= 0 *redraw*) (return-from redraw-stuff nil))

  (let ((retval nil)
	(pr-set nil)
        (bot-set nil))

    (when (bit-flag-set? *redraw* +print-extra+)
      (bit-flag-remove! *redraw* +print-extra+)
      (setf retval t))
    
    (if (call-next-method)
	t
	retval)
    ))

(defun contra-init-eq-system (var-obj)
  "Initialises values dealing with the equipment (sorting, worn slots)."

  (let ((equip-order '(
		       (eq.lefthand  "Left Hand"     active-object/melee-weapon)
		       (eq.righthand "Right Hand"    active-object/melee-weapon)
		       (eq.l-ring    "On left hand"  active-object/ring)
		       (eq.r-ring    "On right hand" active-object/ring)
		       (eq.neck      "Around neck"   active-object/neckwear)
		       (eq.armour    "On body"       active-object/body-armour)
		       (eq.cloak     "About body"    active-object/cloak)
		       (eq.head      "On head"       active-object/headgear)
		       (eq.glove     "On hands"      active-object/gloves)
		       (eq.feet      "On feet"       active-object/boots)
		       (eq.backpack  "On back"       active-object/container t)
		       )))
    
    (register-slot-order& var-obj equip-order))
  )

(defvar *showing-runes* nil)
(defvar *current-arrow* 0)

;; flaky
(defun rune-select (col row)
  (let ((num (+ row (* 2 col)))
	(win (aref *windows* *map-frame*)))
    (warn "Got number ~s and letter ~s" num  (code-char (+ 65 num)))
    (setf (window-coord win +foreground+ 4 (+ 2 *current-arrow*)) (tile-paint-value 44 num))
    (paint-coord win 4 (+ 2 *current-arrow*))
    ))

(defun display-rune-arrow (win col idx)
  (let ((indent 2))
    (clear-coord win col (+ indent *current-arrow*))
    (paint-coord win col (+ indent *current-arrow*))
    (setf (window-coord win +foreground+ col (+ indent idx)) (tile-paint-value 46 1))
    (paint-coord win col (+ indent idx))
    (setf *current-arrow* idx)

    ;;(warn "disp")
    ;;(flush-coords win col 2 (1+ col) 13)
    ))

(defmethod handle-mouse-click ((variant contraband) window button x y)

  (let ((num-id (window.num-id window))
	(player *player*)
	(dungeon *dungeon*))
  
    (cond ((= num-id +inv-frame+)
	   ;; handle normal case
	   (cond ((not *showing-runes*)
		  (when (eq button :left)
		    (let ((wid (window.pixel-width window))
			  (tile-wid (window.tile-width window)))
		      ;; two button-sets
		      (cond ((> x (- wid tile-wid)) ;; last tile
			     (switch-inventory-view))
			    ((> x (- wid (* 2 tile-wid))) ;; second last tile 
			     (switch-map-mode *dungeon* *player*)))
		      )))
		 ;; runes
		 (*showing-runes*
		  (when (eq button :left)
		    (let* ((tile-wid (window.tile-width window))
			   (tile-hgt (window.tile-height window))
			   (which-x (int-/ x tile-wid))
			   (which-y (if (< y tile-hgt) 0 1)))
		      (rune-select which-x which-y))))
		 ))

	  ((and *showing-runes*
		(= num-id *map-frame*)
		(eq button :left))
	   (let* ((loc-x (int-/ x (window.tile-width window)))
		  (loc-y (int-/ y (window.tile-height window))))

	     (when (and (= loc-x 2) ;; hit a key.. maybe
			(>= loc-y 2)
			(< loc-y 12))
	       (warn "Hit F~d" (1+ (- loc-y 2)))
	       (display-rune-arrow (aref *windows* *map-frame*) 0 (- loc-y 2))
	       )))

	  
	  ((and (= num-id *map-frame*)
		(eq button :right))
	   ;; first get panel coords, then translate to real coords
	   (let* ((loc-x (int-/ x (window.tile-width window)))
		  (loc-y (int-/ y (window.tile-height window)))
		  (rx (+ loc-x (player.view-x player)))
		  (ry (+ loc-y (player.view-y player)))
		  (tgt (%get-target dungeon rx ry)))

	     (when (is-legal-target? dungeon tgt)
	       (when (player.target player)
		 (%remove-target (player.target player)))
	       (%highlight-target dungeon tgt)
	       (setf (player.target player) tgt))

	     
	     ;;(warn "right click in square ~s ~s" loc-x loc-y)
	     ))
	  
	  (t nil))
    
    t))

(defmethod arrange-game-exit& ((variant contraband) player)
  ;; nothing special happens yet
  t)

(defun con/show-intro ()
  (clear-window +full-frame+)
  (refresh-window +full-frame+)

  (print-text! 10 1 +term-l-red+ "[Pictures used in the intro are copyrighted by others 
and are used for illustration during development but will not be in the released product.]"
	       :end-col 80)
  
  (paint-gfx-image& '(variant-gfx "other/aequus.png") 40 8)

  (print-text! 3 6 +term-l-blue+
	       "The news are spreading quickly as we speak..")
  (print-text! 3 11 +term-white+ "The wise King Aequus of Atrocitas has passed away 70 winters  
old.  His rule was long and just, and he secured peace, trade and prosperity for his nation. 
He was also a brilliant but ruthless commander on the battlefield.  He held the 
northern Dakau Dogmen at bay, and he crushed the mighty Empire of Copia's invading 
legions with a decisive victory 30 years ago."
	       :end-col 35)
  
  (pause-last-line!)
  (clear-window +full-frame+)
  (refresh-window +full-frame+)

  (paint-gfx-image& '(variant-gfx "other/adaugeo2.png") 10 3)
  
  (paint-gfx-image& '(variant-gfx "other/vehemen.png") 40 3)

  (paint-gfx-image& '(variant-gfx "other/callidus2.png") 70 3)

  (print-text! 10 14 +term-white+ "King Aequus' three sons are already powerful 
princes and known far outside the kingdom. 
The oldest son Prince Adauego is a pious knight and a missionary for the atrocitan healing god Deus Salveus. 
Prince Vehemen is a legendary knight famous for his valour, battle skills and gallant manners.  The youngest 
son, Prince Callidus, is a learned man whose organisational skills have turned the Kingdom 
from a war nation into a strong trading nation." :end-col 90) 

  (paint-gfx-image& '(variant-gfx "other/sapient.png") 10 22)

  (print-text! 35 23 +term-white+ "The Empire of Copia has prospered on the trade with the Kingdom 
of Atrocitas, much to the benefit of the merchants and the people.  The Holy Emperor Sapient has sent an 
official letter to the Kingdom of Atrocitas with regrets for their loss of King Aequus.  There is 
much uncertainty in the Empire of Copia, if the peace will be upheld by a new king 
or if the kingdom will be divided between the princes.   " :end-col 90)
  
  (pause-last-line!)

  (clear-window +full-frame+)
  (refresh-window +full-frame+)

  (paint-gfx-image& '(variant-gfx "other/map-towns.png") 40 4)

  (print-text! 3 8 +term-white+ "Contraband is a game of intrigue, diplomacy, smuggling and 
good old-fashioned adventure.  It's a time of uncertainty for two mighty nations and the player 
will be tossed into the middle of the nations' playground.  Along the River Ovid lies the two 
small copian towns Bartertown and Lambda Rock.  It's in an area filled with ancient secrets, 
legends and all travelers between the Kingdom and the Empire pass these two towns."
	       :end-col 35)

  (print-text! 3 30 +term-l-blue+ "It's time to decide who you are and what you want to play...")
	       
  
  (pause-last-line!)
    
  t)

(defun define-skill (id slot-name slot-alias &key desc index)
  (let ((skill (make-con/skill :id id :slot slot-name :alias slot-alias
			       :desc desc :idx index)))
    ;;(warn "defining skill ~s with index ~s" slot-alias index)

    (setf (aref (variant.skills *variant*) index) skill)
    ;;(vector-push skill *skills*)
    
    skill))

(defun con/update-gobj-table! (variant key o-table alloc-table-creator)
  "Tries to make an allocation table from a table."
  (declare (ignore key))
;;  (warn "updating on ~a ~a" key o-table)
  
  (let ((okind-table (gobj-table.obj-table o-table)))
    
    (setf (gobj-table.obj-table-by-lvl o-table)
	  (convert-obj okind-table :vector :sort-table-p t
		       :sorted-by-key #'(lambda (x) (slot-value x 'depth))))
    
    (setf (gobj-table.alloc-table o-table)
	  (funcall alloc-table-creator variant (gobj-table.obj-table-by-lvl o-table)))
    ))

(defun %print-runes (win)
  (let ((rune-num 25)
	(frame-wid (get-frame-width +inv-frame+)))
    ;; ugly thing to test runes, remove later"
    (loop for i from 0 below rune-num
	  for col = (int-/ i 2)
	  for row = (mod i 2)
	  do
	  (clear-coord win col row)
	  (setf (window-coord win +background+ col row) (logior (tile-file 44) (tile-number i)))
	  )))
