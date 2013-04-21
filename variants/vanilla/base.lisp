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


(defclass vanilla-variant (variant)
  ((dawn-time     :initarg :dawntime   :initform 0    :accessor variant.dawn)
   (twilight-time :initarg :twilight   :initform 6000 :accessor variant.twilight)
   (town-seed         :initform nil  :accessor variant.town-seed)
   (used-scroll-names :initform (make-hash-table :test #'equal) :accessor variant.used-scroll-names)
   
   (legal-effects  :initarg :legal-effects  :initform nil
		   :accessor variant.legal-effects)
;;   (object-effects :initarg :object-effects :initform (make-hash-table :test #'equal)
;;		   :accessor variant.object-effects)
   (gold-table     :initarg :gold-table :initform nil
		   :accessor variant.gold-table)
   ))


(defclass van-town-level (themed-level)
  ((id     :initform "town-level")
   (symbol :initform 'town-level)
   (stores        :initarg :stores     :initform nil  :accessor level.stores)
   (num-stores    :initarg :num-stores :initform 8    :accessor level.num-stores)
   (home-num      :initarg :home-num   :initform 7    :accessor level.home-num))

  (:documentation "The Vanilla variant has a special town-level with
stores and special behaviour.  The class is used for dispatching."))


(defclass black-market (store)
  ()
  (:documentation "A store with steep prices, used as a dispatch class."))


(defun van-make-variant-obj ()
  (make-instance 'vanilla-variant
		 :id "langband-vanilla"
		 :name "Vanilla"

		 :stat-length 6
		 
		 ;; only used by development
		 :sys-file "./variants/vanilla/langband-vanilla.system"
		 :config-path
		 #+langband-development
		 "./variants/vanilla/config"
		 #-langband-development
		 "/var/lib/games/langband/vanilla"))


(register-variant& "langband-vanilla" #'van-make-variant-obj)
	   
