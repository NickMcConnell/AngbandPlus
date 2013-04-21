;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: core.lisp - core classes, generics and functions
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)
  
(defstruct (game-obj-table (:conc-name gobj-table.))
  (obj-table nil) ;; hash-table with all possible objects for the setting
  (alloc-table nil) 
  (obj-table-by-lvl nil))


(defclass variant (activatable)
  ((id        :accessor variant.id
	      :initform "lithping"
	      :initarg :id)
   
   (name      :accessor variant.name
	      :initform "lithping"
	      :initarg :name)

   (sys-file  :accessor variant.sys-file;; used for?
	      :initform nil
	      :initarg :sys-file)

   (config-path :accessor variant.config-path;; where are the configuration-files?
		:initform nil
		:initarg :config-path)

   ;; the rest can be done lazily

   (sexes     :accessor variant.sexes
	      :initform '()
	      :initarg :sexes
	      :documentation "List of legal sexes for players and monsters.")
   (races     :accessor variant.races
	      :initform (make-hash-table :test #'equal)
	      :initarg :races)
   
   (classes   :accessor variant.classes
	      :initform (make-hash-table :test #'equal)
	      :initarg :classes)

   (effects   :accessor variant.effects
	      :initform '()
	      :initarg :effects
	      :documentation "List of legal effects and effects to handle for variant.")
   
   (elements  :accessor variant.elements
	      :initform '()
	      :initarg :elements
	      :documentation "List of legal elements and elements to handle for variant.")
   
   (turn      :accessor variant.turn
	      :initform 0
	      :initarg :turn)

   (turn-events :accessor variant.turn-events
		:initform (make-hash-table :test #'equal)
		:initarg :turn-events)


   ;; a level builder is a constructor that must be funcalled
   ;; the key is the level-id
   (level-builders :accessor variant.level-builders
		   :initform (make-hash-table :test #'equal)
		   :initarg :level-builders)

   (floor-features :accessor variant.floor-features
		   :initform (make-hash-table :test #'eql)
		   :initarg :floor-features)

   (room-builders  :accessor variant.room-builders
		   :initform (make-hash-table :test #'equal)
		   :initarg :room-builders)

     
   (max-depth      :accessor variant.max-depth
		   :initform 128
		   :initarg :max-depth)
     
   (max-charlevel  :accessor variant.max-charlevel
		   :initform 50
		   :initarg :max-charlevel)
     
   (xp-table  :accessor variant.xp-table
	      :initarg :xp-table
	      ;; maybe have a default? or maybe not
	      ;; it should be an array of size max-charlevel
	      :initform nil)

   ;; these are just monster-types.. not actual monsters
   (monsters :accessor variant.monsters
	     :initform (make-hash-table :test #'equal)
	     :documentation "these are just monster-types.. not active monsters.")

   (objects :accessor variant.objects
	    :initform (make-hash-table :test #'equal)
	    :documentation "these are just object-types.. not active objects.")

   (monsters-by-level :accessor variant.monsters-by-level
		      :initform (make-hash-table :test #'equal)
		      :documentation "these are monster-types organised by levels.")

   (objects-by-level :accessor variant.objects-by-level
		     :initform (make-hash-table :test #'equal)
		     :documentation "these are object-types organised by levels.")
   
   
   (filters :accessor variant.filters
	    :initform (make-hash-table :test #'equal)
	    :initarg :filters)
     
   (flavour-types :accessor variant.flavour-types
		  :initform (make-hash-table :test #'equal)
		  :initarg :flavour-types)

   (house-types :accessor variant.house-types
		:initform (make-hash-table)
		:initarg :store-types)
     
   (house-owners :accessor variant.house-owners
		 :initform (make-hash-table)
		 :initarg :store-owners)

   (skill-translations :accessor variant.skill-translations
		       :initform nil
		       :initarg :skill-translations)

   (attk-descs :accessor variant.attk-descs
	       :initform (make-hash-table :test #'eq)
	       :initarg :attk-descs)

   (day-length      :accessor variant.day-length
		    :initarg :day-length
		    :initform 10000)

   (help-topics :accessor variant.help-topics
		:initarg :help-topics
		:initform (make-hash-table :test #'equal))
   
   ))


(defstruct help-topic
  id
  key
  name
  data)

(defclass effect ()
  ((symbol :reader effect.symbol :initarg :symbol)
   (name   :reader effect.name   :initarg :name)
   (index  :reader effect.index  :initarg :index)
   ))

;;; effects
;; fast, slow, blind, prot-evil, shielded, afraid, cut, stun, blessed,
;; hero, super-hero, berserk, poisoned, slow-digest, invulnerable,
;; hallucinate, confused, paralysed, telepathy, invisibility, see-inv,
;; random-teleport, hold-life, ... 

(defun define-effect (symbol name index)
  (let ((var-obj *variant*))
    (pushnew (make-instance 'effect :symbol symbol :name name :index index)
	     (variant.effects var-obj) :test #'eq :key #'effect.symbol)))

(defun is-legal-effect? (variant effect)
  "Returns T if the given effect is legal."
  (assert (and (symbolp effect) (not (eq nil effect))))
  (if (find effect (variant.effects variant) :test #'eq :key #'effect.symbol)
      t
      nil))


(defclass element ()
  ((symbol :reader element.symbol :initarg :symbol)
   (name   :reader element.name   :initarg :name)
   (index  :reader element.index  :initarg :index)
   ))

;; see variants/vanilla/config/defines.lisp for examples

(defun define-element (symbol name index)
  (let ((var-obj *variant*))
    (pushnew (make-instance 'element :symbol symbol :name name :index index)
	     (variant.elements var-obj) :test #'eq :key #'element.symbol)))



(defun is-legal-element? (variant element)
  "Returns T if the given element is legal."
  (assert (and (symbolp element) (not (eq nil element))))
  (if (find element (variant.elements variant) :test #'eq :key #'element.symbol)
      t
      nil))

(defun get-element-flag (variant element)
  (check-type variant variant)
  (assert (and (symbolp element) (not (eq nil element))))
  (let ((elm (find element (variant.elements variant) :test #'eq :key #'element.symbol)))
    (if (and elm (typep elm 'element))
	(element.index elm)
	(error "The element ~s is not registered for variant '~a'"
	       element (variant.name variant)))))


#||
(defclass temporary-effects ()
  (fast slow blind paralysed confused afraid hallucinating poisoned cut stun
	prot-from-evil invulnerable hero super-hero shielded blessed see-invisible
	infravision
	resists
	immunities))

(defclass calculated-effects ()
  (immunities resists sustains slow-digest feather-fall, light
	      regenerate telepathy see-invisible free-action
	      hold-life aggravate random-teleport))
||#

(defclass temporary-attributes ()
  ((speed-mod      :accessor temp.speed-mod      :initform 0 :documentation "signed number") ;; <hasted> + <slowed>
   (blind-mod      :accessor temp.blind-mod      :initform 0 :documentation "signed number")
   (paralysed      :accessor temp.paralysed      :initform 0 :documentation "signed number")
   (confused       :accessor temp.confused       :initform 0 :documentation "signed number")
   (afraid         :accessor temp.afraid         :initform 0 :documentation "signed number")
   (hallucinate    :accessor temp.hallucinate    :initform 0 :documentation "signed number")
   (poisoned       :accessor temp.poisoned       :initform 0 :documentation "signed number")
   (cut            :accessor temp.cut            :initform 0 :documentation "signed number")
   (stunned        :accessor temp.stunned        :initform 0 :documentation "signed number")
   (prot-from-evil :accessor temp.prot-from-evil :initform 0 :documentation "signed number")
   (invulnerable   :accessor temp.invulnerable   :initform 0 :documentation "signed number")
   (heroic         :accessor temp.heroic         :initform 0 :documentation "signed number")
   (super-heroic   :accessor temp.super-heroic   :initform 0 :documentation "signed number")
   (shield-spell   :accessor temp.shielded       :initform 0 :documentation "signed number")
   (blessed        :accessor temp.blessed        :initform 0 :documentation "signed number")
   (see-invisible  :accessor temp.see-invisible  :initform 0 :documentation "signed number")
   (infravision    :accessor temp.infravision    :initform 0 :documentation "signed number")
   (resists        :accessor temp.resists        :initform 0 :documentation "signed number") ;; split?
   (recalling      :accessor temp.recalling      :initform 0 :documentation "signed number") ;; count till recall

   ))

(defclass calculated-attributes ()
  ((slow-digest      :accessor calc.slow-digest     :initform 0   :documentation "number, specifies how slow")
   (feather-fall     :accessor calc.feather-fall    :initform nil :documentation "boolean")
   (glowing          :accessor calc.glowing         :initform 0   :documentation "number, specifies radius")
   (regenerate       :accessor calc.regenerate      :initform 0   :documentation "number, specifies regenerate-speed")
   (telepathy        :accessor calc.telepathy       :initform 0   :documentation "number, specifies radius")
   (see-invisible    :accessor calc.see-invisible   :initform 0   :documentation "number, specifies radius")
   (free-action      :accessor calc.free-action     :initform nil :documentation "boolean")
   (hold-life        :accessor calc.hold-life       :initform nil :documentation "boolean")
   (earthquake       :accessor calc.earthquake      :initform nil :documentation "boolean, blows cause earthquake")
   (aggravate        :accessor calc.aggravate       :initform 0   :documentation "number, specifies radius")
   (random-teleport  :accessor calc.random-teleport :initform nil :documentation "boolean")
   (drain-xp         :accessor calc.drain-xp        :initform nil :documentation "boolean")
   (blessed-blade    :accessor calc.blessed-blade   :initform nil :documentation "boolean")
   ))

(defclass misc-player-info ()
  ((age    :accessor playermisc.age    :initform 0)
   (status :accessor playermisc.status :initform 0)
   (height :accessor playermisc.height :initform 0)
   (weight :accessor playermisc.weight :initform 0))
  (:documentation "A helper-class for the player-object."))

(defclass player-abilities ()
  ((base-ac         :accessor pl-ability.base-ac     :initform 0 :documentation "integer, >= 0")
   (ac-modifier     :accessor pl-ability.ac-modifier :initform 0 :documentation "integer")
   (to-hit-modifier :accessor pl-ability.to-hit-modifier :initform 0 :documentation "integer")
   (to-dmg-modifier :accessor pl-ability.to-dmg-modifier :initform 0 :documentation "integer"))
  
  (:documentation "A helper-class for the player-object."))
  

(defclass player ()

  (
    ;; === Need Special saving ===
  
   (name  :accessor player.name  :initform nil)
   (class :accessor player.class :initform nil)
   (race  :accessor player.race  :initform nil)
   (sex   :accessor player.sex   :initform nil)
   
   (base-stats    :accessor player.base-stats
		  :initform nil
		  :documentation "This is the base stats")
   (current-statmods :accessor player.cur-statmods
		     :initform nil
		     :documentation "This is the diff (possibly drained or raised values) of the base stats")
   (hp-table      :accessor player.hp-table
		  :initform nil
		  :documentation "Note: should be saved.")
   (equipment     :accessor player.equipment :initform nil)
   
   ;; add save-code for this as well
   (misc          :accessor player.misc
		  :initform nil
		  :documentation "Pointer to an object with misc info about the player character.")

   
   (dead-from     :accessor player.dead-from
		  :initform ""
		  :documentation "who killed the player?")

   
   ;; === Directly savable to binary ===
   
   (loc-x :accessor location-x :initform +illegal-loc-x+)
   (loc-y :accessor location-y :initform +illegal-loc-y+)
   
   (view-x :accessor player.view-x :initform +illegal-loc-x+);; wx
   (view-y :accessor player.view-y :initform +illegal-loc-y+);; wy
   
   (depth     :accessor player.depth     :initform 0)
   (max-depth :accessor player.max-depth :initform 0)
   
   (max-xp      :accessor player.max-xp      :initform 0)
   (cur-xp      :accessor player.cur-xp      :initform 0)
   (fraction-xp :accessor player.fraction-xp :initform 0) 
   
   (cur-hp      :accessor current-hp         :initform 0)
   (fraction-hp :accessor player.fraction-hp :initform 0)
   
   (cur-mana      :accessor player.cur-mana      :initform 0)
   (fraction-mana :accessor player.fraction-mana :initform 0)
   
   (gold        :accessor player.gold   :initform 0)
   (food        :accessor player.food   :initform (1- +food-full+))
   (energy      :accessor player.energy :initform 0)
   
   ;; === The remaining values can be calculated from the above ===
   
   (level     :accessor player.level     :initform 1)  ;; can be calculated from cur-xp
   (max-level :accessor player.max-level :initform 1)  ;; can be calculated from max-xp

   (max-hp    :accessor maximum-hp       :initform 0)   ;; can be calculated
   (max-mana  :accessor player.max-mana  :initform 0)   ;; can be calculated
   (xp-table  :accessor player.xp-table  :initform nil) ;; can be calculated
   
   (energy-use :accessor player.energy-use :initform 0)   ;; is just a temp-variable
   (leaving-p  :accessor player.leaving-p  :initform nil) ;; need to save it?
   (dead-p     :accessor player.dead-p     :initform nil) ;; need to save it?
   (speed      :accessor player.speed      :initform +speed-base+)  ;; does this change?
   

   (burden       :accessor player.burden       :initform 0
		 :documentation "Calculated value for how much player carries.")
   (light-radius :accessor player.light-radius :initform 0)
   
   (infravision :accessor player.infravision
		:initform 0
		:documentation "How far does infravision reach?  0 for no infravision.")
   (inventory   :accessor player.inventory
		:initform nil
		:documentation "quick variable to equipment.backpack.content")
   (skills      :accessor player.skills
		:initform nil)
   
   (modbase-stats :accessor player.modbase-stats
		  :initform nil
		  :documentation "This is the modified base stats (base + race + class + eq)")
   (active-stats :accessor player.active-stats
		 :initform nil
		 :documentation "This is the current active stat-value (base + curstatmods + race + class + eq)")

   (perceived-abilites :accessor player.perceived-abilities
		       :initform nil
		       :documentation "Pointer to a player-abilities object with perceived abilties.")
   (actual-abilites :accessor player.actual-abilities
		    :initform nil
		    :documentation "Pointer to a player-abilities object with actual abilties.")
   

   ;; fix this later
   (resists :accessor creature.resists :initform 0 :documentation "What does the player resist?")
   ))


;;; == variant-related code

(defmethod initialise-monsters& (variant &key)
  (error "No INIT-MONSTERS for ~s" (type-of variant)))
  
(defmethod initialise-floors& (variant &key)
  (error "No INIT-FLOORS for ~s" (type-of variant)))

(defmethod initialise-objects& (variant &key)
  (error "No INIT-OBJECTS for ~s" (type-of variant)))


;; a small closure
(let ((registered-variants (make-hash-table :test #'equal)))
  
  (defun register-variant& (var-obj)
    "Registers a variant-object."
    
    (check-type var-obj variant)
    (setf (gethash (variant.id var-obj) registered-variants) var-obj))

  (defun load-variant& (id &key (verbose t))
    "Tries to load a variant."
    (declare (ignore verbose))
    (let ((var-obj (gethash id registered-variants)))
      (when (and var-obj (typep var-obj 'variant))
	var-obj))))



(defmethod variant-data-fname ((var-obj variant) data-fname)
  "Returns a full pathname for data."
  (let ((file-path (variant.config-path var-obj)))
    (if file-path
	(concatenate 'string file-path "/" data-fname)
	data-fname)))



(defun load-variant-data& (var-obj data-file)
  "Loads variant-data from appropriate directory."

  (let ((fname (variant-data-fname var-obj data-file)))
    (load fname)))


#||
      (let ((sys-file (variant.sys-file var-obj)))
	(when verbose
	  (format t "~&Will try to load variant '~a' in file ~a~%" id sys-file))
	(compile-in-environment
	 #'(lambda ()
	     (load sys-file)
	     (mk:operate-on-system id 'compile :verbose nil)
	     (when verbose
	       (format t "~&Variant '~a' compiled and loaded.~%" id))))
	var-obj))))
||#
	     


(defun execute-turn-events! (var-obj)
  "Executes any turn-events."
  (let* ((turn (variant.turn var-obj))
	 (turn-table (variant.turn-events var-obj))
	 (turn-ev (gethash turn turn-table)))

    (when turn-ev
      (warn "Executing events ~a" turn-ev)
      (remhash turn turn-table))))

(defun register-turn-event! (var-obj wanted-turn event)
  "Adds a turn-event."

  (push event (gethash wanted-turn (variant.turn-events var-obj))))


;;(defun get-monster-filters (type var-obj)
;;  (gethash type (variant.filters var-obj)))


(defun apply-filters-on-obj (type var-obj obj)
  (let ((filters (gethash type (variant.filters var-obj))))
    (dolist (i filters)
      (funcall (cdr i) var-obj obj))))


(defun get-level-builder (id &optional (var-obj *variant*))
  "Returns a level-builder or nil."
  (assert (or (symbolp id) (stringp id)))
  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (gethash key table)))

(defun register-level-builder! (id builder &optional (var-obj *variant*))
  "Registers a level-builder which must be a function."
  (assert (or (symbolp id) (stringp id)))
  (assert (functionp builder))

  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) builder)))

(defmethod get-sex ((variant variant) (key string))
  (find key (variant.sexes variant) :key #'sex.id :test #'equal))

(defmethod get-sex ((variant variant) (key symbol))
  (find key (variant.sexes variant) :key #'sex.symbol :test #'eq))
