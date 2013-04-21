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

  (pushnew (make-gender :id "male" :symbol '<male> :name "Male" :win-title "King")
	(variant.genders var-obj) :test #'eql :key #'gender.symbol)
  (pushnew (make-gender :id "female" :symbol '<female> :name "Female" :win-title "Queen")
	(variant.genders var-obj) :test #'eql :key #'gender.symbol)


;;  (assert (eq nil (variant.legal-effects var-obj)))
;;  (setf (variant.legal-effects var-obj) '(:quaff :read :eat :create :add-magic :use)) ;; make :use a meta-effect?

  (flet ((help-path (file)
	   (concatenate 'string *engine-source-dir* "lib/help/" file)))
  

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
					   :name "Who has helped make Langband possible"
					   :data (help-path "THANKS")))
    (register-help-topic& var-obj
			  (make-help-topic :id "version" :key #\v
					   :name "Show version information"
					   :data (help-path "version.txt"))))
 

  
  (van-register-levels! var-obj)

  (van-init-skill-system var-obj)
  (van-init-combat-system var-obj)
  
  (let ((*load-verbose* nil))
    (load-variant-data& var-obj "defines")
    (load-variant-data& var-obj "stats")
    (load-variant-data& var-obj "flavours")
    (load-variant-data& var-obj "traps")

    (load-variant-data& var-obj "spells")
    (load-variant-data& var-obj "races")
    (load-variant-data& var-obj "classes")
    ;; need races
    (load-variant-data& var-obj "stores")

    (load-variant-data& var-obj "combat")
    )

  ;; we ensure that any elements in variant are sorted
  (setf (variant.elements var-obj)
	(sort (variant.elements var-obj) #'< :key #'element.number))
  
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

    (dolist (i '("food-ration" "torch"))
      (let ((obj (create-aobj-from-id i :amount (rand-range 3 7))))
	(item-table-add! inventory obj)))

    t))


(defmethod activate-object :after ((var-obj vanilla-variant) &key)
  "Does post-initialisation of the game with variant tweaking."

;;  (declare (ignore var-obj))
  ;; YES, this is a hack
;;  (warn "Post-init of vanilla variant..")

  (setf (get-setting var-obj :basic-frame-printing)
	(make-instance 'vanilla-basic-frame-locations))
  
  (setf (get-setting var-obj :bottom-row-printing)
	(make-instance 'vanilla-bottom-row-locations))

  (let ((birth-settings (make-instance 'vanilla-birth :allow-all-classes t))
	(chdisp-settings (make-instance 'chardisplay-settings))
	(res-settings (make-instance 'resistdisplay-settings)))
	
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
      (setf (slot-value res-settings 'title-x) 15
	    (slot-value res-settings 'title-y) 3
	    (slot-value res-settings 'title-attr) +term-blue+
	    (slot-value res-settings 'list-x) 15
	    (slot-value res-settings 'list-y) 6
	    (slot-value res-settings 'unres-attr) +term-red+
	    (slot-value res-settings 'res-attr) +term-green+
	    ))

	    
    (setf (get-setting var-obj :birth) birth-settings
	  (get-setting var-obj :char-display) chdisp-settings
	  (get-setting var-obj :resists-display) res-settings
	  ))
	  
  
  (setf (get-setting var-obj :random-level)
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
	(birth-settings (get-setting var-obj :birth)))
    
    (register-event& (event.id equip-event) equip-event :variant var-obj)
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
  (initialise-objects&  var-obj :file "food")
  (initialise-objects&  var-obj :file "armour")
  (initialise-objects&  var-obj :file "weapons")
  (initialise-objects&  var-obj :file "potions")
  (initialise-objects&  var-obj :file "rings")
  (initialise-objects&  var-obj :file "neckwear")
  (initialise-objects&  var-obj :file "scrolls")
  (initialise-objects&  var-obj :file "sticks")
  (initialise-objects&  var-obj :file "books")
  (initialise-objects&  var-obj :file "gold")

  (initialise-monsters& var-obj :file "monsters")
  (initialise-monsters& var-obj :file "town-monsters")
  (initialise-monsters& var-obj :file "uniques")
  
  (initialise-floors& var-obj :file "floors")


  (when (load-gfx-tiles?)
    ;;(warn "Loading graf prefs!")
    (load-variant-data& var-obj "graf-prefs"))
  ;;  (warn "flav")
  ;; after all objects are in
;;  (init-flavours& (variant.flavour-types var-obj))

  (loop for x being the hash-values of (variant.flavour-types var-obj)
	do
	(unless (flavour-type.generator-fn x) ;; no point if generated
	  ;; turn into array, shuffle and put back
	  (let ((an-arr (coerce (flavour-type.unused-flavours x) 'vector)))
	    (shuffle-array! an-arr (length an-arr))
	    (setf (flavour-type.unused-flavours x) (coerce an-arr 'list))
	    t)))

  
  #||
  ;; hack, fix later
  (let ((*load-verbose* nil))
    (load-variant-data& var-obj "ego-items"))
  ;; hack
  (dump-egos)
  ||#
;;  (van-combine-effects-with-objects! var-obj (variant.objects var-obj))

  
  ;; ensure that we have a legal gold-table
  (when (eq (variant.gold-table var-obj) nil)
    (let* ((obj-kinds (loop for x being the hash-values of (variant.objects var-obj) ;; hackish
			    when (typep x 'object-kind/money)
			    collecting x))
	   (obj-len (length obj-kinds))
	   (gold-table (make-array obj-len)))
      (loop for i from 0
	    for obj in obj-kinds
	    do
	    (setf (aref gold-table i) obj))
      
      (setf (variant.gold-table var-obj) gold-table)))
  


  
  
  var-obj)


(defun van-init-equipment-values (var-obj)
  "Initialises values dealing with the equipment (sorting, worn slots)."

  (let ((equip-order '(
		       (eq.weapon   "Wielding"      active-object/melee-weapon)
		       (eq.bow      "Shooting"      active-object/missile-weapon)
		       (eq.l-ring   "On left hand"  active-object/ring)
		       (eq.r-ring   "On right hand" active-object/ring)
		       (eq.neck     "Around neck"   active-object/neckwear)
		       (eq.light    "Light source"  active-object/light-source)
		       (eq.armour   "On body"       active-object/body-armour)
		       (eq.cloak    "About body"    active-object/cloak)
		       (eq.shield   "On arm"        active-object/shield)
		       (eq.head     "On head"       active-object/headgear)
		       (eq.glove    "On hands"      active-object/gloves)
		       (eq.feet     "On feet"       active-object/boots)
		       (eq.backpack "On back"       active-object/container t)
		       )))
    
    (register-slot-order& var-obj equip-order))
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

;;; the rest of the file has a lot of odd functions

(defun get-resistance-level (variant player elm)
  (let ((num (if (integerp elm)
		 elm
		 (get-element-number variant elm))))
    (aref (player.resists player) num)))

(defun resists-element? (player elm &key (variant *variant*))
  (plusp (get-resistance-level variant player elm)))
 

(defun %van-fill-resists (variant resist-table source-resists bit)
  (dolist (x (variant.elements variant))
    (when (bit-flag-set? source-resists (element.bit-flag x))
      (setf (aref resist-table (element.number x)) bit))))



(defun %van-update-resistance (variant resist-array element bit)
;;  (warn "Temporarily resisting ~s" element)
  (let ((elm-number (get-element-number variant element)))
    (bit-flag-add! (aref resist-array elm-number) bit)
    ))


(defun get-stat-row (data val)
  (dolist (j data)
    (cond ((consp j) 
	   (cond ((= val (car j))
		  (return-from get-stat-row j))
		 ((and (integerp (cadr j)) (<= val (cadr j)))
		  (return-from get-stat-row j))))
	  ((stat-field-p j)
	   (let ((lower (stat-field-lower j))
		 (upper (stat-field-upper j)))
	     
	     (cond ((= val lower)
		    (return-from get-stat-row j))
		   ((and (integerp upper) (<= val upper))
		    (return-from get-stat-row j)))))))
  (error "Fell through GET-ROW with ~a val in ~s" val data))

(defun get-stat-info (stat-obj stat-value info-key)
  (let ((the-row (get-stat-row (stat.fields stat-obj) stat-value)))
    (cdr (assoc info-key (stat-field-data the-row)))))

(defun get-stat-info-value (variant player stat info-key)
  (let* ((stats (variant.stats variant))
	 (stat (cond ((symbolp stat)
		      (find stat stats :key #'stat.symbol))
		     (t
		      (error "only syms support for get-stat-info-value"))))
	 (value (svref (player.active-stats player) (stat.number stat))))
    (get-stat-info stat value info-key)))

;;(trace get-stat-info-value)

(defmethod deliver-elemental-damage! ((variant vanilla-variant) source (target player) element damage)

  ;; skip immunity, add later
  (unless (<= damage 0)
    (let ((percentage (cond ((>= damage 60) 3)
			    ((>= damage 30) 2)
			    (t 1)))
	  (res-level (get-resistance-level variant target element)))
      (declare (ignorable percentage)) ;; fix!
      (when (bit-flag-set? res-level +calculated-effect+)
	(setf damage (int-/ (+ 2 damage) 3)))
      (when (bit-flag-set? res-level +temporary-effect+)
	(setf damage (int-/ (+ 2 damage) 3)))

      (deliver-damage! variant source target damage)
      ;; add equipment dmg
      t)))



#||
(defun parse-ego-items& (variant &key file)
  (let ((fname (variant-data-fname variant file)))
    (warn "Reading ego from ~s" fname)

    (compat-read-ego-file& fname)
    
    t))
||#
