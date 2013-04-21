;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: race.lisp - code for the character races
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(defclass l-race ()
  (
   (id           :accessor race.id           :initform nil)
   (name         :accessor race.name         :initform nil)
   (desc         :accessor race.desc         :initform nil)
   (xp-extra     :accessor race.xp-extra     :initform 0)
   (hit-dice     :accessor race.hit-dice     :initform 10)
   (stat-changes :accessor race.stat-changes :initform nil)
   (abilities    :accessor race.abilities    :initform nil)
   (classes      :accessor race.classes      :initform nil)
   (start-eq     :accessor race.start-eq     :initform nil)
   )
  (:documentation "Representation for a character race."))

(defmethod print-object ((inst l-race) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (race.id inst)
	   (race.name inst)))
  inst)


(defun (setf get-char-race) (race id)
  "Puts the race in appropriate tables id'ed by id."
  (setf (gethash id *race-table*) race))

(defun get-char-race (id)
  "Fetches the race id'ed by id from the appropriate table."
  (gethash id *race-table*))

(defun get-races-as-a-list ()
  "Returns a fresh list of all races."
  (loop for v being each hash-value of *race-table*
	collecting v))

(defun define-race (id name &key desc xp-extra stat-changes abilities
		    hit-dice classes starting-equipment)
  "defines a race and updates global race-list"
  
  (let ((race (make-instance 'l-race)))
    
    ;;      (warn "Creating race ~a [~a]" name desc)
    (setf (race.id race) id)
    (setf (race.name race) name)
    (when desc
      (setf (race.desc race) desc))
    (when xp-extra
      (setf (race.xp-extra race) xp-extra))
    (when classes
      (setf (race.classes race) classes))
    
    (if stat-changes
	(setf (race.stat-changes race) (build-stat-table-from-symlist stat-changes))
	(setf (race.stat-changes race) #1A(0 0 0 0 0 0)))

    (when abilities
      (setf (race.abilities race) abilities))

    (when hit-dice
      (setf (race.hit-dice race) hit-dice))
    
    (when starting-equipment
      (setf (race.start-eq race) starting-equipment))
    
    ;; add it to table
    (setf (get-char-race id) race)
    
    ;; return the race
    race))

#||
(defun print-all-races ()
  "Prints to stdout all races with their key."
  (let ((*print-readably* nil))
    (maphash #'(lambda (k v)
		 (format t "~a -> ~a~%" k v))
	     *race-table*)))
||#
;; (print-all-races)
