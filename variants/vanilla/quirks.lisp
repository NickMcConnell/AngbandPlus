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

  (pushnew (make-gender :id "male" :symbol '<male> :name "Male" :win-title "King")
	(variant.genders var-obj) :test #'eql :key #'gender.symbol)
  (pushnew (make-gender :id "female" :symbol '<female> :name "Female" :win-title "Queen")
	(variant.genders var-obj) :test #'eql :key #'gender.symbol)


  (assert (eq nil (variant.legal-effects var-obj)))
  (setf (variant.legal-effects var-obj) '(:quaff :read :eat :use)) ;; make :use a meta-effect?

  ;; fix these two to something real
  (register-help-topic& var-obj (make-help-topic :id "keys" :key #\k :name "Show commands/keys"))
  (register-help-topic& var-obj (make-help-topic :id "chlog" :key #\c :name "Show changelog"))
  
  (van-register-levels! var-obj)

  (van-init-skill-system var-obj)
  (van-init-combat-system var-obj)
  
  (let ((*load-verbose* nil))
    (load-variant-data& var-obj "defines")
    (load-variant-data& var-obj "stats")
    (load-variant-data& var-obj "races")
    (load-variant-data& var-obj "classes")
    (load-variant-data& var-obj "flavours")
    (load-variant-data& var-obj "stores"))

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

  (setf (get-setting var-obj :basic-frame-printing) (make-prt-settings))
  (setf (get-setting var-obj :birth) (make-birth-settings :allow-all-classes t))
  
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

;; we override to add our own stuff
(defmethod produce-player-object ((variant vanilla-variant))
  (let ((pl-obj (call-next-method)))

    ;; we want the resist table
    (let ((resist-size (length (variant.elements variant))))
      (setf (player.resists pl-obj) (make-array resist-size :initial-element 0))) 
    
    ;; ensure that we have hash-tables in place
    (unless (hash-table-p (player.calc-attrs pl-obj))
      (setf (player.calc-attrs pl-obj) (make-hash-table :test #'eq)))
    (unless (hash-table-p (player.temp-attrs pl-obj))
      (setf (player.temp-attrs pl-obj) (make-hash-table :test #'eq)))

    (flet ((install-attribute (&rest args)
	     (let ((attr (apply #'make-player-attribute args)))
	       (unless (is-legal-effect? variant (attr.key attr))
		 (warn "The attribute ~s does not seem legal" attr))
	     (add-player-attribute pl-obj attr)))
	   )
					      
      
      ;; add attributes to the player-object
      (install-attribute "slow digest" '<slow-digest> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies how slow digestion is, 0 is normal.")
      
      (install-attribute "feather fall" '<feather-fall> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, are we falling like a feather?")
      
      (install-attribute "glowing" '<glowing> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies radius for player-glow.")
      
      (install-attribute "regenerate" '<regenerate> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies regeneration-speed.")
      
      (install-attribute "telepathy" '<telepathy> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies radius for telepathy.")
      
      (install-attribute "see invisible" '<see-invisible> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies radius.")
      
      (install-attribute "infravision" '<infravision> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies radius.")
      
      (install-attribute "free action" '<free-action> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is action free?")
      
      (install-attribute "hold life" '<hold-life> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, has he got a good grasp on his life-force?")
      
      (install-attribute "earthquake" '<earthquake> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, do the blows cause earthquakes?")
      
      (install-attribute "aggravate" '<aggravates> :type :calculated
			 :value 0 :default-value 0
			 :desc "number, specifies radius for aggravation.")
      
      (install-attribute "random teleporting" '<random-teleport> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is the player being randomly teleported?")
      
      (install-attribute "drain xp" '<drain-xp> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is the player being drained of xp?")
      
      (install-attribute "blessed blade" '<blessed-blade> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is the player wielding a blessed blade?")
      
      ;; then those attributes that are "temporary" in nature

      (install-attribute "cut" '<cut> :type :temporary ;; need special code
			 :value 0 :default-value 0
			 :desc "number, size of cuts")

      (install-attribute "stun" '<stun> :type :temporary ;; need special code
			 :value 0 :default-value 0
			 :desc "number, stun-power")

      (install-attribute "see invisible" '<see-invisible> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "Your eyes feel very sensitive!"
			 :turned-off-msg "Your eyes feel less sensitive."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+)
					(bit-flag-add! *update* +pl-upd-monsters+))
			 :desc "boolean, in vanilla see-inv is on/off")

      (install-attribute "infravision" '<infravision> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "Your eyes begin to tingle!"
			 :turned-off-msg "Your eyes stop tingling."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+)
					(bit-flag-add! *update* +pl-upd-monsters+))
			 :desc "boolean, in vanilla infravision is on/off")

      
      (install-attribute "hasted" '<hasted> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel yourself moving faster!"
			 :turned-off-msg "You feel yourself slow down."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla hasted means +10")
      
      (install-attribute "slowed" '<slowed> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel yourself moving slower!"
			 :turned-off-msg "You feel yourself speed up."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla slowed means -10")

      (install-attribute "blindness" '<blindness> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are blind!"
			 :turned-off-msg "You can see again."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-forget-view+ +pl-upd-update-view+ +pl-upd-monsters+)
					(bit-flag-add! *redraw* +print-map+ +print-blind+)
					;; skip window
					)			 
			 :desc "boolean, either blinded or not")
      
      (install-attribute "paralysed" '<paralysed> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are paralysed!"
			 :turned-off-msg "You can move again."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *redraw* +print-state+))
			 :desc "boolean, either paralysed or not")

      (install-attribute "confusion" '<confusion> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are confused!"
			 :turned-off-msg "You feel less confused now."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *redraw* +print-confused+))
			 :desc "boolean, either confused or not")

      (install-attribute "fear" '<fear> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are terrified!"
			 :turned-off-msg "You feel bolder now."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *redraw* +print-afraid+))
			 :desc "boolean, either afraid or not")
      
      (install-attribute "hallucinate" '<hallucinate> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel drugged!"
			 :turned-off-msg "You can see clearly again."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *redraw* +print-map+))
			 :desc "boolean, either hallucinating or not")

      (install-attribute "poisoned" '<poisoned> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are poisoned!"
			 :turned-off-msg "You are no longer poisoned."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *redraw* +print-poisoned+))
			 :desc "boolean, in vanilla you're poisoned or not poisoned")

      (install-attribute "protected from evil" '<prot-from-evil> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel safe from evil!"
			 :turned-off-msg "You no longer feel safe from evil."
			 :update-fun #'%set-simple-effect
			 :desc "boolean, in vanilla you're protected or not protected")
      
      (install-attribute "shielded" '<shielded> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "A mystic shield forms around your body!"
			 :turned-off-msg "Your mystic shield crumbles away."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, you're either under a shield-spell or not in vanilla")

      (install-attribute "invulnerability" '<invulnerable> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel invulnerable!"
			 :turned-off-msg "You feel vulnerable once more."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, either invulnerable or not")

      (install-attribute "heroic" '<heroic> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel like a hero!"
			 :turned-off-msg "The heroism wears off."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, hero or not hero")

      (install-attribute "berserk" '<berserk> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel like a killing machine!"
			 :turned-off-msg "You feel less berserk."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, is he berserk?")

      (install-attribute "blessed" '<blessed> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel righteous!"
			 :turned-off-msg "The prayer has expired."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla you're eiether blessed or not")

      (install-attribute "resist acid" '<resist-acid> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to acid!"
			 :turned-off-msg "You feel less resistant to acid."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "resist elec" '<resist-elec> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to electricity!"
			 :turned-off-msg "You feel less resistant to electricity."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "resist fire" '<resist-fire> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to fire!"
			 :turned-off-msg "You feel less resistant to fire."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla temp-resists are on/off")
      
      (install-attribute "resist cold" '<resist-cold> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to cold!"
			 :turned-off-msg "You feel less resistant to cold."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "resist poison" '<resist-poison> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to poison!"
			 :turned-off-msg "You feel less resistant to poison."
			 :update-fun #'%set-simple-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore player attr))
					(bit-flag-add! *update* +pl-upd-bonuses+))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "recalling" '<recalling> :type :temporary
			 :value nil :default-value nil
			 :desc "boolean, is recalling or not")


    
      pl-obj)))

(defmethod reset-player-object! ((variant vanilla-variant) (player player))
  (call-next-method)
  (flet ((%reset-plattrs (table)
	   (loop for attr being the hash-values of table
		 do
		 (setf (attr.value attr) (attr.default-value attr)))))
    
    (%reset-plattrs (player.calc-attrs player))
;;    (%reset-plattrs (player.temp-attrs player))

    (let ((table (player.resists player)))
      (dotimes (i (length table))
	(setf (aref table i) 0)))
    
    t))

(defun %van-fill-resists (variant resist-table source-resists bit)
  (dolist (x (variant.elements variant))
    (when (bit-flag-set? source-resists (element.bit-flag x))
      (setf (aref resist-table (element.number x)) bit))))


(defmethod calculate-abilities! ((variant vanilla-variant) (player player) (cls character-class))

  ;; do the general stuff first:
  (call-next-method)

  ;; add resists
  ;; currently resists on race is just an integer
  (let ((resist-array (player.resists player)))
    (%van-fill-resists variant resist-array (class.resists cls) +calculated-effect+)

    ;; hack, add fear resist for warriors
    (when (and (eq (class.id cls) '<warrior>) (>= (player.level player) 30))
      (setf (aref resist-array (get-element-number variant '<fear>)) +calculated-effect+))
    ))
  

(defmethod calculate-abilities! ((variant vanilla-variant) (player player) (race character-race))

  ;; do the general stuff first:
  (call-next-method)
  
  (let ((calc-attrs (player.calc-attrs player))
	(race-ab (race.abilities race)))

    ;; add resists
    ;; currently resists on race is just an integer
    (let (;;(resists (race.resists race))
	  (resist-array (player.resists player)))
      (%van-fill-resists variant resist-array (race.resists race) +calculated-effect+))
    
    (dolist (i race-ab)
;;	(warn "checking ~a" i)
	(cond ((consp i)
	       (case (car i)
		 (<infravision>
		  (when (integerp (second i))
		    (alter-attribute! '<infravision> calc-attrs (second i))))
		 (otherwise
		  (warn "Unhandled racial ability ~a" (car i)))))
	      ((symbolp i)
	       (case i
		 (<see-invisible>
		  (alter-attribute! '<see-invisible> calc-attrs +max-sight+)
		  ;;(setf (attr.see-invisible attrs) +max-sight+)
		  ) ;; radius
		 (<free-action>
		  (alter-attribute! '<free-action> calc-attrs t) ;; boolean
;;		  (setf (attr.free-action attrs) t)
		  ) 
		 (otherwise 
		  (warn "Unhandled racial ability ~a" i))))
	      (t
	       (warn "Unhandled racial ability ~a" i)))
	)
    t))

(defmethod calculate-abilities! ((variant vanilla-variant) (player player) (items items-worn))
  ;; get weight and ac first from parent object
  (call-next-method)
  
  (let (;;(actual-abs (player.actual-abilities player))
	;;(perc-abs (player.perceived-abilities player))
	(resist-array (player.resists player))
	)

    (loop for obj across (items.objs items)
	  do
	  (when obj
	    ;; time to get resists
	    (when-bind (gvals (aobj.game-values obj))
	      (%van-fill-resists variant resist-array (gval.resists gvals) +calculated-effect+)
	      )))
    ))
  

(defun %van-update-resistance (variant resist-array element bit)
;;  (warn "Temporarily resisting ~s" element)
  (let ((elm-number (get-element-number variant element)))
    (bit-flag-add! (aref resist-array elm-number) bit)
    ))



(defmethod handle-player-updates! ((variant vanilla-variant) (player player) (old old-player-info))
  (let ((calc-attrs (player.calc-attrs player))
	(temp-attrs (player.temp-attrs player))
	(actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player))
	(resist-array (player.resists player))
	(skills (player.skills player))
	)

    
    ;; go through some temporary effects
    (when (get-attribute-value '<heroic> temp-attrs)
      (incf (pl-ability.ac-modifier actual-abs) +100)
      (incf (pl-ability.ac-modifier perc-abs)   +100))

    (when (get-attribute-value '<blessed> temp-attrs)
      (incf (pl-ability.ac-modifier actual-abs) +5)
      (incf (pl-ability.ac-modifier perc-abs)   +5)
      (incf (pl-ability.to-hit-modifier actual-abs) +10)
      (incf (pl-ability.to-hit-modifier perc-abs)   +10)
      )
    
    (when (get-attribute-value '<shielded> temp-attrs)
      (incf (pl-ability.ac-modifier actual-abs) +50)
      (incf (pl-ability.ac-modifier perc-abs)   +50)
      )

    (when (get-attribute-value '<heroic> temp-attrs)
      (incf (pl-ability.to-hit-modifier actual-abs) +12)
      (incf (pl-ability.to-hit-modifier perc-abs)   +12)

      ;; immune to fear
      (bit-flag-add! (aref resist-array (get-element-number variant '<fear>)) +temporary-effect+))

    (when (get-attribute-value '<berserk> temp-attrs)
      (incf (pl-ability.ac-modifier actual-abs)     -10)
      (incf (pl-ability.ac-modifier perc-abs)       -10)
      (incf (pl-ability.to-hit-modifier actual-abs) +24)
      (incf (pl-ability.to-hit-modifier perc-abs)   +24)
      ;; immune to fear
      (bit-flag-add! (aref resist-array (get-element-number variant '<fear>)) +temporary-effect+))


    (when (get-attribute-value '<hasted> temp-attrs)
      (incf (player.speed player) +10))
    
    (when (get-attribute-value '<slowed> temp-attrs)
      (incf (player.speed player) -10))

    ;; go through all temporary resists

    (when (get-attribute-value '<resist-fire> temp-attrs)
      (%van-update-resistance variant resist-array '<fire> +temporary-effect+))
    (when (get-attribute-value '<resist-cold> temp-attrs)
      (%van-update-resistance variant resist-array '<cold> +temporary-effect+))
    (when (get-attribute-value '<resist-elec> temp-attrs)
      (%van-update-resistance variant resist-array '<electricity> +temporary-effect+))
    (when (get-attribute-value '<resist-acid> temp-attrs)
      (%van-update-resistance variant resist-array '<acid> +temporary-effect+))
    (when (get-attribute-value '<resist-poison> temp-attrs)
      (%van-update-resistance variant resist-array '<poison> +temporary-effect+))
    


    ;; add update of infravision from current facts
    (let ((temp-infra (get-attribute-value '<infravision> temp-attrs))
	  (calc-infra (get-attribute-value '<infravision> calc-attrs)))
      ;; set it to the largest of the two
      (when temp-infra (incf calc-infra)) ;; temporary infra is one better
      (setf (player.infravision player) calc-infra))

    ;; add update of see-inv from current facts
    (let ((temp-see (get-attribute-value '<see-invisible> temp-attrs))
	  (calc-see (get-attribute-value '<see-invisible> calc-attrs)))
      ;; set it to the largest of the two
      ;; hack for vanilla
      (when temp-see
	(setf calc-see +max-sight+)) 
      (setf (player.see-invisible player) calc-see))

    ;;; this way to do lookups _sucks_
    
    ;; time to add modifiers from stats
    (let ((stats (variant.stats variant))
	  (active-stats (player.active-stats player)))
      (flet ((get-row (data val)
	       (dolist (j data)
		 (cond ((= val (car j))
			(return-from get-row j))
		       ((and (integerp (cadr j)) (<= val (cadr j)))
			(return-from get-row j))))
	       (error "Fell through GET-ROW with ~a val in ~s" val data)))
	
      (dolist (i stats)
	(let* ((cur-val (svref active-stats (stat.number i)))
	       (cur-row (get-row (stat.data i) cur-val)))
	  (case (stat.symbol i)
	    (<dex> (let ((to-hit (sixth cur-row))
			 (to-ac (fourth cur-row))
			 (disarm (elt cur-row 7)))
		     ;; + to-hit
		     (incf (pl-ability.to-hit-modifier actual-abs) to-hit)
		     (incf (pl-ability.to-hit-modifier perc-abs)   to-hit)
		     ;; + to ac
		     (incf (pl-ability.ac-modifier actual-abs) to-ac)
		     (incf (pl-ability.ac-modifier perc-abs)   to-ac)
		     ;; better at disarming
		     (incf (skills.disarming skills) disarm)
		     
		     ))
	    
	    (<str> (let ((to-hit (sixth cur-row))
			(to-dmg (fourth cur-row)))
		     (incf (pl-ability.to-hit-modifier actual-abs) to-hit)
		     (incf (pl-ability.to-hit-modifier perc-abs)   to-hit)
		     (incf (pl-ability.to-dmg-modifier actual-abs) to-dmg)
		     (incf (pl-ability.to-dmg-modifier perc-abs)   to-dmg)
		     ;; add digging
		     ))
	    
	    (<int> (let ((disarm (elt cur-row 13))
			 (device (elt cur-row 11)))
		     ;; better at disarming
		     (incf (skills.disarming skills) disarm)
		     ;; better at devices
		     (incf (skills.device skills) device)
		     ))
	    
	    (<wis> (let ((save (elt cur-row 11)))
		     ;; better at saves
		     (incf (skills.saving-throw skills) save)
		     ))
	    )))
      ))

    ;; somehow vanilla-people help players with bonuses
    (incf (skills.stealth skills))

    ;; skip digging modifier
    
    ;; keep stealth within limits
    (cond ((> (skills.stealth skills) 30)
	   (setf (skills.stealth skills) 30))
	  ((minusp (skills.stealth skills))
	   (setf (skills.stealth skills) 0)))

    ;; skip noise
    ;; hold?
    
    (call-next-method)    
    
    t))

;; move later
(defun resists-element? (player elm &key (variant *variant*))
  (let ((num (if (integerp elm)
		 elm
		 (get-element-number variant elm))))
    (plusp (aref (player.resists player) num))))

(defmethod get-creature-state ((player player) state)
  "Very limited so far, but might work for some stuff.  do not rely on it."
  (let* ((temp-attrs (player.temp-attrs player))
	 (attr (gethash state temp-attrs)))

    (unless attr
      (let ((calc-attrs (player.calc-attrs player)))
	(setf attr (gethash state calc-attrs))))

    (when (and attr (typep attr 'player-attribute))
      (attr.value attr))))


(defun %set-simple-effect (player state value)
  "Sets the value of a temorary attribute/effect.  Only supports boolean ones."
  (let* ((attr (gethash state (player.temp-attrs player)))
	 (old-value (attr.value attr))
	 (new-duration (attr.duration attr))
	 (noticed nil))
    
    (check-type attr temp-player-attribute)
    
    (cond ((eq value t)
	   (setf new-duration (incf new-duration (+ 25 (randint 25)))))
	  ((or (eq value nil)
	       (and (integerp value) (= value 0)))
	   (setf new-duration 0))
	  ((integerp value)
	   (incf new-duration value)) ;; both for positive and negative numbers
	  (t
	   (error "Weird value given to ~s: ~s" state value)))

    (when (minusp new-duration) ;; never negative
      (setf new-duration 0))

;;    (warn "going berserk [~a,~a] -> [~a,~a]" old-value (attr.duration attr) value new-duration)
    
    (setf (attr.duration attr) new-duration)
    
    (cond ((plusp new-duration)
	   (setf (attr.value attr) t)
	   (when (eq nil old-value)
	     (setf noticed t)
	     (when (attr.turned-on-msg attr)
	       (print-message! (attr.turned-on-msg attr)))))
	  (t
	   (setf (attr.value attr) nil)
	   (when (eq t old-value)
	     (setf noticed t)
	     (when (attr.turned-off-msg attr)
	       (print-message! (attr.turned-off-msg attr))))))

    (when noticed
      (let ((on-upd (attr.on-update attr)))
	(when (and on-upd (functionp on-upd))
	  (funcall on-upd player attr))))

    ;; call to handle-stuff ?
    t))


;; very hackish, improve/integrate later
(defmethod (setf get-creature-state) (value (player player) state)

  ;; :fear
  ;; :hero
  ;; :blindness
  ;; :heal-cut, <light> (c-10), <serious> ((c/2)-50)
  ;; :confusion
  ;; :poison nil + :slow, num
  ;; :confusion
  ;; :stun
  ;; :cut
  ;; :hallucination
  ;; :berserk

  (let* ((temp-attrs (player.temp-attrs player))
	 (attr (gethash state temp-attrs)))

    (cond ((and attr (attr.update-fun attr))
	   (funcall (attr.update-fun attr) player state value))
	  (t 
	   (case state
	     (otherwise
	      (warn "Not implemented support for creature-state ~s" state)
	      t))
	   ))
    ))
    

