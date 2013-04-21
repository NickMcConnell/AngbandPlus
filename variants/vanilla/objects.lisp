;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/objects.lisp - code related to vanilla object-types
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)

;; possibly add this for potions
(defmethod is-eatable? ((player player) (obj active-object/food))
  t)

(defmethod is-eatable? ((player player) (obj object-kind/food))
  t)

(defun %dummy-eat-fun (dun pl item)
  "A hack to ensure all food-objects can be eaten."
  (declare (ignore dun pl item))
  :used)


(defmethod get-price ((object active-object) situation)
  (declare (ignore situation))
  (let* ((kind (aobj.kind object))
	 (known-p (is-object-known? object)))

    ;; skip broken/cursed

    ;; also ignore discounts
    
    (if known-p
	(object.cost kind)
	(typecase object
	  (active-object/food 5)
	  (active-object/potion 20)
	  (active-object/scroll 20)
	  (active-object/staff 70)
	  (active-object/wand 50)
	  (active-object/rod 90)
	  (active-object/ring 45)
	  (active-object/amulet 45)
	  (otherwise 0)))))


;; store-related method
(defmethod store-mass-produce! ((variant variant) (store store) (object active-object))
  ;; hack

  (let ((number 1)
	(cost (get-price object store)))

    (block increase-number
      (cond ((or (typep object 'active-object/light-source)
		 (typep object 'active-object/food))
	     (when (<= cost 5) (incf number (roll-dice 3 5)))
	     (when (<= cost 20) (incf number (roll-dice 3 5))))
	     
	    ((or (typep object 'active-object/potion)
		 (typep object 'active-object/scroll))
	     (when (<= cost 60) (incf number (roll-dice 3 5)))
	     (when (<= cost 240) (incf number (roll-dice 3 5))))
	    ;; skip food, flask, light
	    ;; skip spellbooks
	    ((or (typep object 'active-object/armour)
		 (typep object 'active-object/weapon))
	     ;; test for artifact
	     (when (<= cost 10) (incf number (roll-dice 3 5)))
	     (when (<= cost 100) (incf number (roll-dice 3 5))))
	    ;; add spike
	    ((typep object 'active-object/ammo)
	     (when (<= cost 5) (incf number (roll-dice 5 5)))
	     (when (<= cost 50) (incf number (roll-dice 5 5))))
	    (t
	     nil)))

    ;; add discount..
     
      
    (setf (aobj.number object) number)))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/ring) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))

  (let* ((o-type (aobj.kind obj))
	 (gvals (object.game-values obj))
	 (known-type (or store (object.aware o-type)))
	 (obj-known (or store (is-object-known? obj)))
	 (flav-obj (if store nil (object.flavour o-type)))
	 )
    
;;    (when flav-obj
;;      (setf flavour (flavour.text-attr flav-obj)))

    
    (write-pluralised-string stream "& #ring~@" (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :flavour flav-obj :numeric-prefix numeric-prefix)
    
    (when obj-known
      
      ;; if it has combat-bonuses, add those
      (let ((tohit-mod (if gvals (gval.tohit-modifier gvals) 0))
	    (dmg-mod (if gvals (gval.dmg-modifier gvals) 0))
	    )
	(cond ((and (/= 0 tohit-mod) (/= 0 dmg-mod))
	       (format stream " (~@d,~@d)" tohit-mod dmg-mod))
	      ((/= tohit-mod 0)
	       (format stream " (~@d)" tohit-mod))
	      ((/= dmg-mod 0)
	       (format stream " (~@d)" dmg-mod))
	      (t
	       nil)))
      
      ;; display armour bonuses
      (let ((ac-bonus (if gvals (gval.ac-modifier gvals) 0)))
	(when (/= 0 ac-bonus)
	  (format stream " [~@d]" ac-bonus)))
      )

    
    ))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/light-source) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))

  (let* ((o-type (aobj.kind obj))
	 ;;(gvals (object.game-values obj))
	 (known-type (or store (object.aware o-type)))
	 ;;(obj-known (or store (is-object-known? obj)))
	 )
    
    (write-pluralised-string stream (object.name obj) (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :numeric-prefix numeric-prefix)

    (when-bind (desc (get-charge-status obj))
      (when (stringp desc)
	(format stream " (~a)" desc)))
    
    
    ))


(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/armour) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  
  (declare (ignore verbosity))
  
  (let* ((o-type (aobj.kind obj))
	 (gvals (object.game-values obj))
	 (obj-known (or store (is-object-known? obj)))
	 (known-type (or store (object.aware o-type))))
    
    (write-pluralised-string stream (object.name obj) (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :numeric-prefix numeric-prefix)
    
    (let ((ac-val (if gvals (gval.base-ac gvals) 0))
	  (ac-bonus (if gvals (gval.ac-modifier gvals) 0))
	  (tohit-mod (if gvals (gval.tohit-modifier gvals) 0))
	  )

      (when-bind (ego (aobj.ego obj))
	(format stream " ~a" (ego.name ego)))
      
      ;; display armour bonuses
      (when (and obj-known (/= tohit-mod 0))
	(format stream " (~@d)" tohit-mod))
      
      (cond (obj-known
	     (format stream " [~d,~@d]" ac-val ac-bonus))
	    ((plusp ac-val)
	     (format stream " [~d]" ac-val)))
      )))


(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/book) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  (declare (ignore verbosity))
  (let ((known-type (or store (object.aware (aobj.kind obj)))))
    (write-pluralised-string stream "& ritual-book~ @" (aobj.number obj)
			     :numeric-prefix numeric-prefix
			     :ident known-type :actual-name (object.name obj))))
  
(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/weapon) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  "this one should be moved out into the variant directories.  it conses"

  (declare (ignore verbosity))
  (let* ((o-type (aobj.kind obj))
	 (number (aobj.number obj))
	 (known-obj (is-object-known? obj))
	 (base (plural-name number (object.name o-type) nil (or store known-obj) nil
			    :numeric-prefix numeric-prefix))
	 (suffix (if (aobj.ego obj) (format nil " ~a" (ego.name (aobj.ego obj))) ""))
	 (gvals (object.game-values obj))
	 (tohit-mod (if gvals (gval.tohit-modifier gvals) 0))
	 (dmg-mod (if gvals (gval.dmg-modifier gvals) 0))
	 )
    (cond (known-obj
	   (format stream "~a~a (~@d,~@d)" base suffix tohit-mod dmg-mod))
	  (t
	   (write-string base stream)))))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))
  (let* ((o-type (aobj.kind obj))
	 (name (object.name obj))
	 (flavour (if store nil (object.flavour o-type)))
	 (known-type (or store (is-object-known? obj)))
	 (number (aobj.number obj))
	 ;;(plural-string nil)
	 )


    (let ((str (typecase obj
		 (active-object/mushroom
		  (plural-name number "& #mushroom~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/potion
		  (plural-name number "& #potion~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/ring
		  (error "Ring should be handled elsewhere"))
		 (active-object/staff
		  (plural-name number "& #staff~@" flavour known-type name :numeric-prefix numeric-prefix))  
		 (active-object/wand
		  (plural-name number "& #wand~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/rod
		  (plural-name number "& #rod~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/scroll
		  (plural-name number "& scroll~ #@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/amulet
		  (plural-name number "& #amulet~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (otherwise
		  (plural-name number name nil known-type nil :numeric-prefix numeric-prefix))  
		 )))
      (write-string str stream))

    ))


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind) keyword-args)

  (call-next-method)

  (when-bind (depth (getf keyword-args :depth))
    (assert (>= depth 0))
    (setf (object.power-lvl new-obj) depth))

  (let ((locale (getf keyword-args :locale))
	(chance (getf keyword-args :chance)))
    (when (or locale chance)
      (check-type chance vector)
      (check-type locale vector)
      (setf (object.locations new-obj) (loop for x across locale
					     for y across chance
					     when (plusp y)
					     collecting (cons x y)))))
  
  ;; add stuff here

  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/ammo) keyword-args)

  (call-next-method)
  
  (when-bind (e-t (getf keyword-args :visual-effect))
    ;; get effect-type
    (when-bind (lookup (gethash e-t (variant.visual-effects var-obj)))
      (setf (object.effect-type new-obj) lookup)))

  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/spellbook) keyword-args)

  (call-next-method)

  (let ((spells (getf keyword-args :spells)))
    (when (consp spells)
      (let ((book (create-spellbook (object.name new-obj) (object.id new-obj) spells)))
	(register-spellbook& var-obj book))))

  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/prayerbook) keyword-args)

  (call-next-method)

  (let ((spells (getf keyword-args :spells)))
    (when (consp spells)
      (let ((book (create-spellbook (object.name new-obj) (object.id new-obj) spells)))
	(register-spellbook& var-obj book))))

  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/bow) keyword-args)

  (call-next-method)

  (let ((multiplier (getf keyword-args :multiplier)))
    ;; get bow multiplier
    (when (and multiplier (numberp multiplier))
;;      (warn "Multiplier for ~s is ~s" new-obj multiplier)
      (setf (object.multiplier new-obj) multiplier)))

  new-obj)


(defmethod get-visual-projectile ((obj active-object/wand))
  (object.effect-type (aobj.kind obj)))

(defmethod get-visual-projectile ((obj active-object/rod))
  (object.effect-type (aobj.kind obj)))

(defmethod get-visual-projectile ((obj active-object/ammo))
  (object.effect-type (aobj.kind obj)))


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/wand) keyword-args)

  (call-next-method)

  (when-bind (e-t (getf keyword-args :effect-type))
    (unless (is-legal-effect-type? e-t)
      (warn "Uknown effect-type ~s for object ~s" e-t new-obj))
    ;; get effect-type
    (when-bind (lookup (gethash e-t (variant.visual-effects var-obj)))
      
      (setf (object.effect-type new-obj) lookup)))

  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/rod) keyword-args)

  (call-next-method)

  (when-bind (e-t (getf keyword-args :effect-type))
    (unless (is-legal-effect-type? e-t)
      (warn "Uknown effect-type ~s for object ~s" e-t new-obj))
    ;; get effect-type
    (when-bind (lookup (gethash e-t (variant.visual-effects var-obj)))
      
      (setf (object.effect-type new-obj) lookup)))

  new-obj)


(defmethod get-charge-status ((obj active-object))
  nil)

;; may need updated with artifacts
(defmethod get-charge-status ((obj active-object/light-source))
  (let ((kind (aobj.kind obj)))
    (when-bind (descs (object.status-descs kind))
      (let ((charges (gval.charges (aobj.game-values obj)))
	    (max-charge (object.max-fuel kind)))
	
	(when (numberp charges)
	  ;; hackish
	  (let ((ratio (int-/ (* 100 charges) max-charge)))
	    ;; (warn "Ratio is ~d/~d -> ~d" charges max-charge ratio)
	    (cond ((> ratio 90) (elt descs 0))
		  ((> ratio 70) (elt descs 1))
		  ((> ratio 30) (elt descs 2))
		  ((> ratio 10) (elt descs 3))
		  ((> ratio 0) (elt descs 4))
		  ((<= ratio 0) (elt descs 5))
		  #-(or cmu sbcl)
		  (t
		   (error "Never fall this far!"))))
	  ))
      )))
      

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/light-source) keyword-args)

  (call-next-method)

  (when-bind (descs (getf keyword-args :status-descs))
    (if (listp descs)
	(setf (object.status-descs new-obj) descs)
	(warn "Status-descriptions for light-source ~a is not a list, but ~s"
	      (object.name new-obj) descs)))

  (when-bind (fuel (getf keyword-args :max-fuel))
    (if (numberp fuel)
	(setf (object.max-fuel new-obj) fuel)
	(warn "Max-fuel for light-source ~a is not a number, but ~s"
	      (object.name new-obj) fuel)))

  
  new-obj)


(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/ring) depth quality)

  (let ((add-magic-effect (get-object-effect variant item :add-magic)))
    (when (and add-magic-effect
	       (effect-entry-p add-magic-effect)
	       (functionp (effect-entry-fun add-magic-effect)))
      (funcall (effect-entry-fun add-magic-effect) item depth quality))
    
    t))


(defun van/add-ego-ability! (variant item depth quality)
  (declare (ignore quality)) ;; use this later
  (block make-ego-item
    ;;(warn "ego ~s ~s" quality (object.id item))
    ;; skip boost

    (let ((total 0)
	  (table (gobj-table.alloc-table (%get-var-table variant "level" 'ego-items-by-level))))

      (loop named counting-area
	    for a-obj across table
	    do
	    (progn
	      (setf (alloc.prob3 a-obj) 0)
	   
	      (when (>= depth (alloc.depth a-obj))
		;;(warn "Checked ~s at ~s vs ~s" (alloc.obj a-obj) (alloc.depth a-obj) depth)
		;; test if the ego-obj can fit
		(let* ((ego (alloc.obj a-obj))
		       (types (ego.obj-types ego)))
		  (dolist (i types)
		    (cond ((symbolp i)
			   (when (satisfies-obj-type? i item)
			     ;;(warn "~s satisfied ~s at ~s" ego item depth)
			     (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))))
			  (t nil)))
		  ))
	      (incf total (alloc.prob3 a-obj))))

      (when (= 0 total)
	(warn "No suitable ego-items at depth ~a for ~a" depth item)
	(return-from make-ego-item nil))

    
      (let ((val (random total)))
	(loop for a-obj across table
	      do
	      (when (< val (alloc.prob3 a-obj))
		(assert (typep item 'active-object/vanilla-object))
		(setf (aobj.ego item) (alloc.obj a-obj))
		(return-from make-ego-item t))
	      (decf val (alloc.prob3 a-obj))))

      nil)))

(defun van/add-magic-to-weapon! (variant item depth quality)
  (block add-magic-to-item!
    (when (eq quality :normal)
      (return-from add-magic-to-item! nil))

    ;; skip ego-check for :great and :broken
    (when (or (eq quality :great) (eq quality :broken))
      (van/add-ego-ability! variant item depth quality)) ;; returns t for success and nil otherwise

    (ensure-game-values! item)
  
    (let ((to-hit (+ (randint 5) (magic-bonus-for-level 5 depth)))
	  (to-dmg (+ (randint 5) (magic-bonus-for-level 5 depth)))
	  (to-hit-extra (+ (magic-bonus-for-level 10 depth)))
	  (to-dmg-extra (+ (magic-bonus-for-level 10 depth)))
	  (gvals (aobj.game-values item))
	  )

      (case quality
	(:good
	 (incf (gval.dmg-modifier gvals) to-dmg)
	 (incf (gval.tohit-modifier gvals) to-hit))
	(:great
	 (incf (gval.dmg-modifier gvals) (+ to-dmg to-dmg-extra))
	 (incf (gval.tohit-modifier gvals) (+ to-hit to-hit-extra)))
	(:cursed
	 (decf (gval.dmg-modifier gvals) to-dmg)
	 (decf (gval.tohit-modifier gvals) to-hit))
	(:broken
	 (decf (gval.dmg-modifier gvals) (+ to-dmg to-dmg-extra))
	 (decf (gval.tohit-modifier gvals) (+ to-hit to-hit-extra)))
	(otherwise
	 (warn "Unknown quality ~s wanted for item ~s" quality item)))

      ;; skip cursed flag
      ;; skip super-charges

      ;;    (warn "Added magic (~s,~s) to ~s" to-hit to-dmg item)
    
      t)))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/weapon) depth quality)
  (van/add-magic-to-weapon! variant item depth quality))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/ammo) depth quality)
  (van/add-magic-to-weapon! variant item depth quality))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/armour) depth quality)

  (when (eq quality :normal)
    (return-from add-magic-to-item! nil))

  ;; skip ego-check for :great and :broken
  (when (or (eq quality :great) (eq quality :broken))
    (van/add-ego-ability! variant item depth quality)) ;; returns t for success and nil otherwise

  (ensure-game-values! item)
  
  (let ((to-ac1 (+ (randint 5) (magic-bonus-for-level 5 depth)))
	(to-ac2 (+ (magic-bonus-for-level 10 depth)))
	(gvals (aobj.game-values item))
	)

    (case quality
      (:good
       (incf (gval.ac-modifier gvals) to-ac1))
      (:great
       (incf (gval.ac-modifier gvals) (+ to-ac1 to-ac2)))
      (:cursed
       (decf (gval.ac-modifier gvals) to-ac1)
       (bit-flag-add! (aobj.identify item) (logior +ident-cursed+)))
      (:broken
       (decf (gval.ac-modifier gvals) (+ to-ac1 to-ac2))
       (bit-flag-add! (aobj.identify item) (logior +ident-cursed+ +ident-broken+)))
      (otherwise
       (warn "Unknown quality ~s wanted for item ~s" quality item)))

    ;; skip cursed flag
    ;; skip super-charges

    ))
  

(defmethod add-magic-to-item! :after ((variant vanilla-variant) (item active-object/vanilla-object) depth quality)
  (declare (ignore depth quality))

  (when-bind (ego (aobj.ego item))
    ;;(warn "Made ego ~s for ~s" ego item)
    ;; skip xtra
    ;; skip broken
    ;; skip cursed
    ;; skip penalties/bonuses
    ;; skip level-feeling
    ))
  

;; all wands should have game-values for charges
(defmethod initialize-instance :after ((obj active-object/wand) &key)
  (ensure-game-values! obj))

;; all staffs should have game-values for charges
(defmethod initialize-instance :after ((obj active-object/staff) &key)
  (ensure-game-values! obj))

;; most rings should have game-values, so we just assume all of them should
(defmethod initialize-instance :after ((obj active-object/ring) &key)
  (ensure-game-values! obj))

(defun add-charges! (item charges)
  (incf (gval.charges (aobj.game-values item)) charges))
