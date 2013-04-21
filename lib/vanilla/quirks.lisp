;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/vanilla/quirks.lisp - special settings for Vanilla
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(defun van-init-sorting-values (var-obj)
  "Initialises the sorting values for later reading of the vanilla objects."

  ;; highest value listed first..
  
  (let ((table (variant.sort-values var-obj)))
    
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

(defmethod activate-object :before ((var-obj vanilla-variant) &key)
  "Initialises variant-variables that should be there before
the rest of the game is init'ed."
  
  (customise-game-parameters&
   '((:initial-backpack . :backpack)
     (:backpack-constant-p . :yes)
     (:equipment-organisation . :vanilla)
     ))
  
  ;; max lvl matches size of xp-table
  (setf (variant.max-charlevel var-obj) 50)
  (setf (variant.xp-table var-obj)
	#1A(
	      10
	      25
	      45
	      70
	      100
	      140
	      200
	      280
	      380
	      500
	      650
	      850
	      1100
	      1400
	      1800
	      2300
	      2900
	      3600
	      4400
	      5400
	      6800
	      8400
	      10200
	      12500
	      17500
	      25000
	      35000
	      50000
	      75000
	      100000
	      150000
	      200000
	      275000
	      350000
	      450000
	      550000
	      700000
	      850000
	      1000000
	      1250000
	      1500000
	      1800000
	      2100000
	      2400000
	      2700000
	      3000000
	      3500000
	      4000000
	      4500000
	      5000000))

  ;; registering the levels this variant will use
  (register-level! 'level var-obj)
  (register-level! 'random-level var-obj)
  (register-level! 'town-level var-obj)

  ;; we need to sort out objects/monsters to the right places
  (register-object-filter! 'level
			   #'(lambda (var-obj obj)
			       (let ((table (get-otype-table 'level var-obj)))
				 (setf (gethash (slot-value obj 'id)
						(gobj-table.obj-table table))
				       obj)
				 t))
			   var-obj)

  (register-monster-filter! 'random-level
			    #'(lambda (var-obj obj)
				;; all below 0
				(when (> (slot-value obj 'level) 0)
				  (let ((table (get-mtype-table 'random-level var-obj)))
				    (setf (gethash (slot-value obj 'id)
						   (gobj-table.obj-table table))
					  obj)
				    t)))
			    var-obj)
						

  (register-monster-filter! 'town-level
			    #'(lambda (var-obj obj)
				;; all equal to 0
				(when (= (slot-value obj 'level) 0)
				  (let ((table (get-mtype-table 'town-level var-obj)))
				    (setf (gethash (slot-value obj 'id)
						   (gobj-table.obj-table table))
					  obj)
				    t)))
			    var-obj)
    
  (load-variant-data& var-obj "defines")
  (load-variant-data& var-obj "races")
  (load-variant-data& var-obj "classes")

  
  (van-init-sorting-values var-obj)

  ;; move to variant object
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

;; EVENT function
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

(defmethod activate-object :after ((var-obj vanilla-variant) &key)
  "Does post-initialisation of the game with variant tweaking."

;;  (declare (ignore var-obj))
  ;; YES, this is a hack
;;  (warn "Post-init of vanilla variant..")

  (setf (get-setting :basic-frame-printing) (make-prt-settings))
  (setf (get-setting :birth) (make-birth-settings :allow-all-classes t))
  
  (setf (get-setting :random-level)
	(make-instance 'dungeon-settings
		       :max-width 198
		       :max-height 66
		       ;; ranges
		       :stairs-down '(10 . 20) ;; (3 4)
		       :stairs-up '(10 . 20) ;; (1 2)
		       ))
  
  (register-setting-event& :birth (cons :on-pre-equip #'van-add-basic-equip))

  )

    
(defun update-gobj-table! (key o-table alloc-table-creator)
  ""
  (declare (ignore key))
;;  (warn "updating on ~a ~a" key o-table)
  
  (let ((okind-table (gobj-table.obj-table o-table)))
    
    (setf (gobj-table.obj-table-by-lvl o-table)
	  (htbl-to-vector okind-table t
			  :sorted-by-key #'(lambda (x) (slot-value x 'level))))
    
    (setf (gobj-table.alloc-table o-table)
	  (funcall alloc-table-creator (gobj-table.obj-table-by-lvl o-table)))
    ))



(defmethod initialise-monsters& ((var-obj vanilla-variant) &key old-file)
  
  (if old-file
      (compat-read-monsters& old-file) ;; adds to mkind-table
      (error "No lispy monster-file reader"))
  
  ;; initialise all tables
  (let ((object-tables (variant.monsters var-obj)))
    (maphash #'(lambda (key obj)
		 (update-gobj-table! key obj
			#'create-alloc-table-monsters))
	     object-tables))
  
  )

  

  
(defmethod initialise-features& ((variant vanilla-variant) &key old-file)
  (if old-file
      (compat-read-feature-file& old-file)
      (error "No lispy feature-file reader")))


(defmethod initialise-objects& ((var-obj vanilla-variant) &key old-file)
  
  (if old-file
    (compat-read-obj-kind& old-file) ;; adds to okind-table
    (error "No lispy monster-file reader"))

  ;; initialise all tables
  (let ((object-tables (variant.objects var-obj)))
    (warn "Mapping ~a" object-tables)
    (maphash #'(lambda (key obj)
		 (update-gobj-table! key obj
				     #'create-alloc-table-objects))
	     object-tables))


  ;; skip flavouring ever so long.
;;    (init-flavours& okind-table)
  
  #+langband-debug
  (%output-kinds-to-file "dumps/obj.lisp")
  )


;; The real McCoy
(defmethod activate-object ((var-obj vanilla-variant) &key)

  (initialise-objects&  var-obj :old-file "lib/edit/k_info.txt")
  (initialise-monsters& var-obj :old-file "lib/edit/r_info.txt")
  (initialise-features& var-obj :old-file "lib/edit/f_info.txt")

  )
