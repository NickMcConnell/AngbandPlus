;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/base.lisp - the base variant class for Vanilla
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defclass contraband (variant)
  ((dawn-time     :initarg :dawntime   :initform 0    :accessor variant.dawn)
   (twilight-time :initarg :twilight   :initform 6000 :accessor variant.twilight)
   (town-seed         :initform nil  :accessor variant.town-seed)
   (used-scroll-names :initform (make-hash-table :test #'equal) :accessor variant.used-scroll-names)
   
   (gold-table     :initarg :gold-table
		   :initform nil
		   :accessor variant.gold-table)
   (spells         :initarg :spells
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spells)

   (spellbooks     :initarg :spellbooks
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spellbooks)

   ;; 50 is max-level
   (max-charlevel :initform 50)
   ;; put xp-table here right away
   (xp-table :initform #1A(     10       25       45       70      100
			       140      200      280      380      500
			       650      850     1100     1400     1800
			      2300     2900     3600     4400     5400
			      6800     8400    10200    12500    17500
			     25000    35000    50000    75000   100000
			    150000   200000   275000   350000   450000
			    550000   700000   850000  1000000  1250000
			   1500000  1800000  2100000  2400000  2700000
			   3000000  3500000  4000000  4500000  5000000))

   (legal-effects :initarg :legal-effects
                  :initform '(:quaff :read :eat :create :add-magic :use)
                  :accessor variant.legal-effects)
;;   (object-effects :initarg :object-effects :initform (make-hash-table :test #'equal)
;;		   :accessor variant.object-effects)

   ))


(defclass con-town (themed-level)
  ((id     :initform "town-level")
   (symbol :initform 'town-level)
   (stores        :initarg :stores     :initform nil  :accessor level.stores)
   (num-stores    :initarg :num-stores :initform 8    :accessor level.num-stores)
   (home-num      :initarg :home-num   :initform 7    :accessor level.home-num))

  (:documentation "Contraband has a special (two) town-level with
stores and special behaviour.  The class is used for dispatching."))


;; change to (col row) format
(defclass con-basic-frame-locations (basic-frame-locations)
  ((max-mana :initarg :max-mana :initform '(18 . 0))
   (cur-mana :initarg :cur-mana :initform '(19 . 0))

   (target   :initarg :target  :initform '(20 . 0))
   (cut      :initarg :cut     :initform '(21 . 0))
   (stun     :initarg :stun    :initform '(22 . 0))
   )
  (:documentation "Locations and various settings when printing stuff.
Each location should be a cons with (row . col)."))
  
(defclass con-bottom-row-locations (bottom-row-locations)
  ((blind    :initarg :blind    :initform 7)
   (confused :initarg :confused :initform 13)
   (afraid   :initarg :afraid   :initform 22)
   (poisoned :initarg :poisoned :initform 29)
   (study    :initarg :study    :initform 64))
   
  (:documentation "Locations and various settings when printing stuff.
Each location is a fixnum with column in the last row."))

(defclass con-birth (birth-settings)
  ())

;;; define relevant object-types for contraband

(def-obj-type weapon :key <weapon>)
(def-obj-type armour :key <armour>)
(def-obj-type cloak :is armour :key <cloak>)

#||
;; doesn't work!
;; hacks to let things load
(defmethod get-otype-table ((var-obj contraband) (level level))
  (%get-var-table var-obj "level" 'objects-by-level))

(defmethod get-mtype-table ((var-obj contraband) (level level))
  (%get-var-table var-obj "level" 'monsters-by-level))

(defmethod activate-object :before ((var-obj contraband) &key)
  (register-level! var-obj "level")
  )
||#  
		   

;; path tweaking needed!!!
(defun make-contraband-obj ()
  (make-instance 'contraband
		 :id "contraband"
		 :name "Contraband"

		 :config-path
		 #+langband-development
		 "./variants/contraband/config/"
		 #-langband-development
		 "/var/games/contraband/"))


(register-variant& "contraband" #'make-contraband-obj
		   :desc "Contraband is all about rum, smuggling and pretty girls.")
