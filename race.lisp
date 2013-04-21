;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: race.lisp - code for the character races
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defclass race ()
  ((id           :accessor race.id           :initform nil)
   (symbol       :accessor race.symbol       :initform nil)
   (name         :accessor race.name         :initform nil)
   (desc         :accessor race.desc         :initform nil)
   (xp-extra     :accessor race.xp-extra     :initform 0)
   (hit-dice     :accessor race.hit-dice     :initform 10)
   (stat-changes :accessor race.stat-changes :initform nil)
   (abilities    :accessor race.abilities    :initform nil)
   (classes      :accessor race.classes      :initform nil)
   (start-eq     :accessor race.start-eq     :initform nil)
   (skills       :accessor race.skills       :initform nil))
  (:documentation "Representation for a character race."))


(defun (setf get-char-race) (race id)
  "Puts the race in appropriate tables id'ed by id."
  (check-type race race)
  (assert (or (stringp id) (symbolp id)))
  (setf (gethash id (variant.races *variant*)) race))

(defun get-char-race (id)
  "Fetches the race id'ed by id from the appropriate table."
  (assert (or (stringp id) (symbolp id)))
  (let ((table (variant.races *variant*)))
    (etypecase id
      (string (gethash id table))
      (symbol (gethash id table)))))

(defun register-race& (race)
  "Registers a race in the right places."
  ;; add it to table
  (check-type race race) 
  (setf (get-char-race (race.id race)) race
	(get-char-race (race.symbol race)) race))
  

(defun get-races-as-a-list ()
  "Returns a fresh list of all races."
  (let* ((table (variant.races *variant*))
	 (hash-vals (loop for v being each hash-value of table
			  collecting v)))
    (remove-duplicates hash-vals)))


(defun define-race (id name &key symbol desc xp-extra stat-changes abilities
		    hit-dice classes starting-equipment skills)
  "defines a race and updates global race-list.  Both id and symbol will be
in the global race-table for easy access."
  
  (let ((race (make-instance 'race)))

    (unless (stringp id)
      (warn "Id ~s for race ~s must be a string, use symbol for race-symbol."))
    ;;      (warn "Creating race ~a [~a]" name desc)

    (unless (and (symbolp symbol) (not (eq symbol nil)))
      (warn "Symbol for race ~s is ~s, please use a normal symbol" name symbol))
    
    (setf (race.id race) id
	  (race.symbol race) symbol)
    
    (setf (race.name race) name)
    (when desc
      (setf (race.desc race) desc))
    (when xp-extra
      (setf (race.xp-extra race) xp-extra))
    (when classes
      (setf (race.classes race) classes))
    
    (if stat-changes
	(setf (race.stat-changes race) (build-stat-table-from-symlist stat-changes))
	(setf (race.stat-changes race) (make-stat-array)))

    (when abilities
      (setf (race.abilities race) abilities))

    (when hit-dice
      (setf (race.hit-dice race) hit-dice))
    
    (when starting-equipment
      (setf (race.start-eq race) starting-equipment))
    
    (register-race& race)
    
    (setf (race.skills race) (build-skills-obj-from-list *variant* skills))
    
    ;; return the race
    race))

(defmethod print-object ((inst race) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (race.id inst)
	   (race.name inst)))
  inst)
