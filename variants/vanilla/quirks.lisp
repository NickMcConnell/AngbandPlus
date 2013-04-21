;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/quirks.lisp - special settings for Vanilla
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

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

  (pushnew (make-sex :id "male" :symbol '<male> :name "Male" :win-title "King")
	(variant.sexes var-obj) :test #'eql :key #'sex.symbol)
  (pushnew (make-sex :id "female" :symbol '<female> :name "Female" :win-title "Queen")
	(variant.sexes var-obj) :test #'eql :key #'sex.symbol)

  (register-help-topic& var-obj (make-help-topic :id "keys" :key #\k :name "Show commands/keys"))
  (register-help-topic& var-obj (make-help-topic :id "chlog" :key #\c :name "Show changelog"))
  
  (van-register-levels! var-obj)

  (van-init-skill-system var-obj)
  (van-init-combat-system var-obj)
  
  (let ((*load-verbose* nil))
    (load-variant-data& var-obj "defines")
    (load-variant-data& var-obj "races")
    (load-variant-data& var-obj "classes")
    (load-variant-data& var-obj "flavours")
    (load-variant-data& var-obj "stores"))

  ;; we ensure that any elements in variant are sorted
  (setf (variant.elements var-obj)
	(sort (variant.elements var-obj) #'< :key #'element.index))
  
  (van-init-equipment-values var-obj)

  )

(defun van-register-levels! (var-obj)
  "registering the levels this variant will use."
  
  (register-level! var-obj "level"
		   :object-filter
		   #'(lambda (var-obj obj)
		       (let* ((table (get-otype-table var-obj "level"))
			      (id (slot-value obj 'id))
			      (obj-table (gobj-table.obj-table table)))
			 (multiple-value-bind (val f-p)
			     (gethash id obj-table)
			   (declare (ignore val))
			   (if f-p
			     (error "Object-id ~s already exists in system, obviously not a unique id."
				    id)
			     (setf (gethash id obj-table) obj))))

			 t))
  
  (register-level! var-obj "random-level"
		   :monster-filter
		   #'(lambda (var-obj obj)
		       ;; all below 0
		       (when (> (slot-value obj 'depth) 0)
			 (let* ((which-lvl "random-level")
				(table (get-mtype-table var-obj which-lvl))
				(id (slot-value obj 'id))
				(mon-table (gobj-table.obj-table table)))
			   (multiple-value-bind (val f-p)
			       (gethash id mon-table)
			     (declare (ignore val))
			     (if f-p
				 (error "Monster-id ~s already exist for ~s, not unique id."
					id which-lvl)
				 (setf (gethash id mon-table) obj))))
			 t)))

  (register-level! var-obj "town-level"
		   :monster-filter
		   #'(lambda (var-obj obj)
		       ;; all equal to 0
		       (when (= (slot-value obj 'depth) 0)
			 (let* ((which-lvl "town-level")
				(table (get-mtype-table var-obj which-lvl))
				(id (slot-value obj 'id))
				(mon-table (gobj-table.obj-table table)))
			   (multiple-value-bind (val f-p)
			       (gethash id mon-table)
			     (declare (ignore val))
			     (if f-p
				 (error "Monster-id ~s already exist for ~s, not unique id."
					id which-lvl)
				 (setf (gethash id mon-table) obj))))
			 t)))

  )
			   

;; EVENT function
(defun van-add-basic-equip (state player dungeon)
  "Adds basic equipment to player.  Dungeon argument is NIL."

  (declare (ignore state dungeon))

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

    t))

;;(trace van-add-basic-equip)

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

  ;; trying to make an event and register it. 
  (let ((equip-event (make-event :vanilla-equipment-addition
				   :on-pre-equip
				   #'van-add-basic-equip))
	(birth-settings (get-setting :birth)))
    
    (register-event& (event.id equip-event) equip-event)
    (if (not birth-settings)
	(warn "Unable to find birth-settings, not registering event.")
	(register-object-event! birth-settings equip-event)
	))
  
  ;; register level-constructors
  (register-level-builder! "random-level"
			   (get-late-bind-function 'langband
						   'make-random-level-obj))
  
  (register-level-builder! "town-level"
			   (get-late-bind-function 'langband
						   'van-create-bare-town-level-obj))
  
  )

    
(defun update-gobj-table! (variant key o-table alloc-table-creator)
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



(defmethod initialise-monsters& ((var-obj vanilla-variant) &key old-file (file "monsters"))
  "If old-file is given, use compatibility.  If not use lispy-file."
  #+cmu
  (declare (optimize (ext:inhibit-warnings 3)))

  (cond
    #+compatibility-monsters
    (old-file
     (compat-read-monsters& (variant-data-fname var-obj old-file))) ;; adds to mkind-table
    
    #-compatibility-monsters
    (old-file
     (error "Trying to read legacy angband-file ~s, but no compatibility support loaded." old-file))
    
    (file
     (let ((*load-verbose* nil))
       (load-variant-data& var-obj file)))
    (t
     (error "No file specified for monster-init.")))
    
  ;; initialise all tables
  (let ((object-tables (variant.monsters-by-level var-obj)))
    (maphash #'(lambda (key obj)
		 (update-gobj-table! var-obj key obj
			#'create-alloc-table-monsters))
	     object-tables))
  
  )


  
(defmethod initialise-floors& ((var-obj vanilla-variant) &key old-file (file "floors"))
  #+cmu
  (declare (optimize (ext:inhibit-warnings 3)))
  (cond
    #+compatibility-floors
    (old-file
     (compat-read-floor-file& (variant-data-fname var-obj old-file)))
    #-compatibility-floors
    (old-file
     (error "Trying to read legacy angband-file ~s, but no compatibility support loaded." old-file))
    (file
     (let ((*load-verbose* nil))
       (load-variant-data& var-obj file)))
    (t
     (error "No file specified for floor-init."))))

(defmethod initialise-objects& ((var-obj vanilla-variant) &key old-file (file "objects"))
  #+cmu
  (declare (optimize (ext:inhibit-warnings 3)))
  (cond
    #+compatibility-objects
    (old-file
     (compat-read-obj-kind& (variant-data-fname var-obj old-file))) ;; adds to okind-table
    #-compatibility-objects
    (old-file
     (error "Trying to read legacy angband-file ~s, but no compatibility support loaded." old-file))
    (file
     (let ((*load-verbose* nil))
       (load-variant-data& var-obj file)))
    (t
     (error "No file specified for floor-init.")))


  ;; initialise all tables
;;    (warn "Mapping ~a" object-tables)
  ;; let us find some behaviour
  (van-combine-effects-with-objects! (variant.objects var-obj))

  (let ((object-tables (variant.objects-by-level var-obj)))
    
    (maphash #'(lambda (key obj)
		 (update-gobj-table! var-obj key obj
				     #'create-alloc-table-objects))
	     object-tables))

  
;;  #+langband-debug
;;  (%output-kinds-to-file "dumps/obj.lisp")
  )


;; The real McCoy
(defmethod activate-object ((var-obj vanilla-variant) &key)

;;  (warn "active..")
  (initialise-objects&  var-obj :file "objects")
  (initialise-objects&  var-obj :file "armour")
  (initialise-objects&  var-obj :file "weapons")
  (initialise-objects&  var-obj :file "potions")
  (initialise-objects&  var-obj :file "scrolls")
  (initialise-objects&  var-obj :file "sticks")
  (initialise-objects&  var-obj :file "books")
  (initialise-objects&  var-obj :file "gold")

  (initialise-monsters& var-obj :file "monsters")
  (initialise-monsters& var-obj :file "town-monsters")
  (initialise-monsters& var-obj :file "uniques")

  (initialise-floors& var-obj)

;;  (warn "flav")
  ;; after all objects are in
  (init-flavours& (variant.flavour-types var-obj))

;;  (van-allocate-flavours! (variant.objects var-obj))

  ;;  (initialise-objects&  var-obj :old-file "k_info.txt")
  ;;  (initialise-monsters& var-obj :old-file "r_info.txt")
  ;;  (initialise-floors& var-obj :old-file "f_info.txt")
  
  var-obj)


(defun van-init-equipment-values (var-obj)
  "Initialises values dealing with the equipment (sorting, worn slots)."
  (declare (ignore var-obj))
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
		       (eq.glove    "On hands"      <gloves>)
		       (eq.feet     "On feet"       <boots>)
		       (eq.backpack "On back"       <container>))))
    
    (register-slot-order& equip-order))
  )


(defun van-init-skill-system (var-obj)
  "Tries to init all that deals with the skill-system."

  ;; we more or less assume these to be complete as we use
  ;; the CDR-values as a list of slots in several places
  ;; in the code
  (register-skill-translation& var-obj
			       '((<disarming> . disarming)
				 (<device> . device)
				 (<saving-throw> . saving-throw)
				 (<stealth> . stealth)
				 (<search> . searching)
				 (<perception> . perception)
				 (<fighting> . fighting)
				 (<shooting> . shooting))))

(defun van-init-combat-system (var-obj)
  "Tries to init all that deals with the combat-system."

  (add-attk-desc var-obj '<beg> "begs you for money.")
  (add-attk-desc var-obj '<touch> "touches you.")
  (add-attk-desc var-obj '<claw> "claws you.")
  (add-attk-desc var-obj '<bite> "bites you.")
  (add-attk-desc var-obj '<sting> "stings you.")
  (add-attk-desc var-obj '<drool> "drools on you.")
  
  )

