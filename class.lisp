;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: class.lisp - character class code
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains basics for dealing with character classes

|#

(in-package :org.langband.engine)


(defclass character-class ()
    (
     (id           :accessor class.id           :initform nil)
     (name         :accessor class.name         :initform nil)
     (desc         :accessor class.desc         :initform nil)
     (hit-dice     :accessor class.hit-dice     :initform 0)
     (xp-extra     :accessor class.xp-extra     :initform 0)
     (stat-changes :accessor class.stat-changes :initform nil)
     (abilities    :accessor class.abilities    :initform nil)
     (titles       :accessor class.titles       :initform nil)
     (starting-eq  :accessor class.start-eq     :initform nil)
     (skills       :accessor class.skills       :initform nil)
     )
    (:documentation "Information about a character class."))


(defmethod print-object ((inst character-class) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (class.id inst)
	   (class.name inst)))
  inst)

(defun (setf get-char-class) (class id)
  "Adds class to appropriate tables id'ed by id."
  (assert (or (stringp id) (symbolp id)))
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.classes *variant*)))
    (setf (gethash key table) class)))

(defun get-char-class (id)
  "Gets the class id'ed by id."
  (assert (or (stringp id) (symbolp id)))
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.classes *variant*)))
    (gethash key table)))

(defun get-classes-as-a-list ()
  "Returns all available classes."
  (let ((table (variant.classes *variant*)))
    (loop for v being each hash-value of table
	  collecting v)))

(defun get-classes-in-list (the-list)
  "Returns the classes for id's in the the-list."
  (let ((table (variant.classes *variant*)))
    (loop for i in the-list
	  collecting (gethash i table))))

(defun define-class (id name &key desc xp-extra stat-changes abilities titles
		     starting-equipment hit-dice skills)
  "Defines and establishes a class."

  (declare (ignore abilities))
  
  (let ((my-class (make-instance 'character-class)))
;;    (warn "Creating class ~a [~a]" name desc)

    (setf (class.id my-class) id)
    (setf (class.name my-class) name)
    (when desc
      (setf (class.desc my-class) desc))
    (when xp-extra
      (setf (class.xp-extra my-class) xp-extra))
    (if stat-changes
      (setf (class.stat-changes my-class)
	    (build-stat-table-from-symlist stat-changes))
      (setf (class.stat-changes my-class)
	    #1A(0 0 0 0 0 0)))

    (when hit-dice
      (setf (class.hit-dice my-class) hit-dice))
    (when titles
      (setf (class.titles my-class) titles))

    (when starting-equipment
;;      (warn "Start-eq for ~a is ~a" name starting-equipment)
      (setf (class.start-eq my-class) starting-equipment))

    (setf (class.skills my-class)
	  (build-skills-obj-from-list *variant* skills))
    
    ;; adding it to the table
    (setf (get-char-class id) my-class)
    ;; returning the class
    my-class))


(defun get-title-for-level (class level)
  "Returns the title for a given class of the given level."
  (declare (type u-fixnum level))
  (let ((titles (class.titles class)))
    (elt titles (int-/ (1- level) 5))))

