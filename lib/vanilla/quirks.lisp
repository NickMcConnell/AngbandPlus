;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

vanilla/quirks.lisp - special settings for Vanilla
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

|#

(in-package :langband)

(defun van-init-sorting-values ()
  "Initialises the sorting values for later reading of the vanilla objects."

  ;; highest value listed first..
  
  (let ((table *sort-values*))
    (setf (gethash 1 table)  2000 ;; skeleton
	  (gethash 2 table)  2100 ;; bottle
	  (gethash 3 table)  2200 ;; junk
	  (gethash 5 table)  2300 ;; spike
	  (gethash 7 table)  2400 ;; chest
	  (gethash 16 table) 2500 ;; ammo, shot
	  (gethash 17 table) 2600 ;; ammo, arrow
	  (gethash 18 table) 2700 ;; ammo, bolt
	  (gethash 19 table) 2800 ;; bow
	  (gethash 20 table) 2900 ;; digging tool
	  (gethash 21 table) 3000 ;; hafted weapon
	  (gethash 22 table) 3100 ;; polearm
	  (gethash 23 table) 3200 ;; sword
	  (gethash 30 table) 3300 ;; boots
	  (gethash 31 table) 3400 ;; gloves
	  (gethash 32 table) 3500 ;; helmets
	  (gethash 33 table) 3600 ;; crowns
	  (gethash 34 table) 3700 ;; shields
	  (gethash 35 table) 3800 ;; cloaks
	  (gethash 36 table) 3900 ;; soft armours
	  (gethash 37 table) 4000 ;; hard armours
	  (gethash 38 table) 4100 ;; dragon-scale
	  (gethash 39 table) 4200 ;; light-source
	  (gethash 40 table) 4300 ;; neckwear
	  (gethash 45 table) 4400 ;; rings
	  (gethash 66 table) 4500 ;; rods
	  (gethash 55 table) 4600 ;; staves
	  (gethash 65 table) 4800 ;; wands
	  (gethash 70 table) 5000 ;; scrolls
	  (gethash 75 table) 5500 ;; potions
	  (gethash 77 table) 5700 ;; flasks
	  (gethash 80 table) 6000 ;; food
	  (gethash 90 table) 6900 ;; mage spellbook
	  (gethash 91 table) 7000 ;; priest spellbook
	  (gethash 100 table) 7100 ;; money
	
	  )))

(defun van-pre-init ()
  "Initialises variant-variables that should be there before
the rest of the game is init'ed."
  
  (customise-game-parameters&
   '((:initial-backpack . :backpack)
     (:backpack-constant-p . :yes)
     (:equipment-organisation . :vanilla)
     ))

  (van-init-sorting-values)
  
  (let ((equip-order '(
		       (eq.weapon   "Wielding"      <weapon>)
		       (eq.bow      "Shooting"      <bow>)
		       (eq.l-ring   "On left hand"  <ring>)
		       (eq.r-ring   "On right hand" <ring>)
		       (eq.neck     "Around neck"   <neckwear>)
		       (eq.light    "Light source"  <light-source>)
		       (eq.armour   "On body"       <body-armour>)
		       (eq.cloak    "About body"    <cloak>)
		       (eq.shield   "On arm"        <shield>)
		       (eq.head     "On head"       <headgear>)
		       (eq.glove    "On hands"      <glove>)
		       (eq.feet     "On feet"       <boots>)
		       (eq.backpack "On back"       <container>))))
    
    (register-slot-order& equip-order)))

(defun van-add-basic-equip (player dungeon)
  "Adds basic equipment to player.  Dungeon argument is NIL."

  (declare (ignore dungeon))

  (let* ((backpack (player.inventory player))
	 (inventory (aobj.contains backpack)))

    (dolist (i '((<food> <ration>)
		 (<light-source> <torch>)))
      (let ((objs (objs-that-satisfy i)))
	;; assume first is right
	(when objs
	  (let ((obj (create-aobj-from-kind (car objs))))
	    (setf (aobj.number obj) (rand-range 3 7))
	    (item-table-add! inventory obj)))
	))

    ))

(defun van-post-init ()
  "Does post-initialisation of the game with variant tweaking."

  ;; YES, this is a hack
;;  (warn "Post-init of vanilla..")

  (setf (get-setting :basic-frame-printing) (make-prt-settings))
  (setf (get-setting :birth) (make-birth-settings :allow-all-classes t))

  (register-setting-event& :birth (cons :on-pre-equip #'van-add-basic-equip))
  
  )

(register-variant& :vanilla "Vanilla"
		  :before-game-init #'van-pre-init
		  :after-game-init #'van-post-init
		  )


