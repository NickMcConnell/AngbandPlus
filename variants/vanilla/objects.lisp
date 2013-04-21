;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/objects.lisp - code related to vanilla object-types
Copyright (c) 2002 - Stig Erik Sandø

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
	 (flavour (if store nil (object.flavour o-type))))
    
    ;; temporary hack
    (when flavour (setf flavour (car flavour)))
    
    (write-pluralised-string stream "& #ring~@" (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :flavour flavour :numeric-prefix numeric-prefix)
    
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
	 (gvals (object.game-values obj))
	 (tohit-mod (if gvals (gval.tohit-modifier gvals) 0))
	 (dmg-mod (if gvals (gval.dmg-modifier gvals) 0))
	 )
    (cond (known-obj
	   (format stream "~a (~@d,~@d)" base tohit-mod dmg-mod))
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

    ;; temporary hack
    (when flavour (setf flavour (car flavour)))
    
;;    (warn "tot-str ~s" tot-str)

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
  
  ;; add stuff here

  new-obj)


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/bow) keyword-args)

  (call-next-method)

  (let ((multiplier (getf keyword-args :multiplier)))
    ;; get bow multiplier
    (when (and multiplier (numberp multiplier))
;;      (warn "Multiplier for ~s is ~s" new-obj multiplier)
      (setf (object.multiplier new-obj) multiplier)))

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


(defmethod add-magic-to-item! ((variant variant) (item active-object/weapon) depth quality)
  
  (when (eq quality :normal)
    (return-from add-magic-to-item! nil))

  ;; skip ego-check for :great and :broken

    (unless (aobj.game-values item)
      (setf (aobj.game-values item) (make-game-values)))
  
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
    
    t))

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
