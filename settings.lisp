;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: settings.lisp - code for keeping track of settings
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: the code deals with representing various settings
ADD_DESC: for different parts of the code

|#

(in-package :org.langband.engine)

(defclass settings ()
  ((name   :accessor setting.name   :initform "No-name" :initarg :name)
   (events :accessor setting.events :initform nil :initarg nil)))

(defclass birth-settings (settings)
  ((allow-all-classes :accessor birth.allow-classes :initform nil))
  (:documentation "Settings when creating characters."))

(defclass dungeon-settings (settings)
  ((max-width   :initarg :max-width   :initform 198)
   (max-height  :initarg :max-height  :initform 66)
   ;; how many rooms
   (room-number :initarg :room-number :initform 50)
   ;; ranges
   (stairs-down :initarg :stairs-down :initform '(3 4))
   (stairs-up   :initarg :stairs-up   :initform '(1 2)))
  (:documentation "A class I will be expanding later.."))

(defclass printing-settings (settings)
  ((race   :initarg :race)
   (class  :initarg :class)
   (title  :initarg :title)
   (level  :initarg :level)
   (xp     :initarg :xp)
   (gold   :initarg :gold)
   (food   :initarg :food)
   (energy :initarg :energy)
   (stat   :initarg :stat)
   (ac     :initarg :ac)
   (max-hp   :initarg :max-hp)
   (cur-hp   :initarg :cur-hp)
   (max-mana :initarg :max-mana)
   (cur-mana :initarg :cur-mana))
  
  (:documentation "Locations and various settings when printing stuff.
Each location should be a cons with (row . col)."))
  

(defmethod trigger-event ((obj settings) event arg-list)
  "trigger events registered for the settings."
  (apply-event event (setting.events obj) arg-list))

(defmethod register-object-event! ((obj settings) event)
  (push event (setting.events obj)))

(defun get-setting (key)
  "Returns the setting or NIL."
  (gethash key *game-settings*))

(defun (setf get-setting) (setting key)
  "Ensures that the setting is accesible from key."
  (when setting
    (unless (keywordp key)
      (warn "Registered setting without a keyword as key [~s]"
	    key))
    (setf (gethash key *game-settings*) setting)))
  

(defun make-prt-settings ()
  "Creates and returns appropriate default printing-settings."
  (make-instance 'printing-settings
		 :name "Printing Settings"
		 :race     '(1 . 0)
		 :class    '(2 . 0)
		 :title    '(3 . 0)
		 :level    '(4 . 0)
		 :xp       '(5 . 0)
		 :gold     '(6 . 0)
		 :stat     '(8 . 0)
		 :ac       '(15 . 0)
		 :max-hp   '(16 . 0)
		 :cur-hp   '(17 . 0)
		 :max-mana '(18 . 0)
		 :cur-mana '(19 . 0)

		 :food     '(21 . 0)
		 :energy   '(22 . 0)

		 ))


(defun make-birth-settings (&key allow-all-classes)
  "Returns a birth-settings object."
  (let ((settings (make-instance 'birth-settings :name "Birth settings")))
    (when allow-all-classes
      (setf (birth.allow-classes settings) t))
    settings))
