;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: character.lisp - code related to character class, race, ...
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)



(defun (setf get-char-class) (class id)
  "Adds class to appropriate tables id'ed by id."
  (assert (or (stringp id) (symbolp id)))
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.classes *variant*)))
    (setf (gethash key table) class)))

(defun get-char-class (id &key (variant *variant*))
  "Gets the class id'ed by id."
  (assert (or (stringp id) (symbolp id)))
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.classes variant)))
    (gethash key table)))

(defun register-class& (class)
  "Registers a race in the right places."
  ;; add it to table
  (check-type class character-class) 
  (setf (get-char-class (class.id class)) class
	(get-char-class (class.symbol class)) class))


(defun get-classes-as-a-list ()
  "Returns all available classes."
  (remove-duplicates (loop for v being each hash-value of (variant.classes *variant*)
			   collecting v)))

(defun get-classes-in-list (the-list)
  "Returns the classes for id's in the the-list."
  (let ((table (variant.classes *variant*)))
    (loop for i in the-list
	  collecting (gethash i table))))

(defmethod produce-character-class ((variant variant) id name &key &allow-other-keys)
;;  (warn "Spells is ~s" spells)
  (make-instance 'character-class))

(defun define-character-class (id name &rest args &key symbol desc xp-extra stat-changes (resists :unspec)
			       (abilities :unspec) titles
			       starting-equipment hit-dice skills &allow-other-keys)
  "Defines and establishes a class."

;;  (warn "Defining class ~s with args ~s" id args)
	    
  (let* ((var-obj *variant*)
	 (my-class (apply #'produce-character-class var-obj id name args)))
	
    ;;    (warn "Creating class ~a [~a]" name desc)

    (check-type var-obj variant)
    
    (when (or (not (eq resists :unspec))
	      (not (eq abilities :unspec)))
      #+langband-extra-checks
      (warn "Unhandled resists/abilities ~s/~s for class ~a"
	    resists abilities name))

    
    (unless (and (stringp id) (verify-id id))
      (warn "Id ~s for class ~s must be a string, use symbol for class-symbol."))
    ;;      (warn "Creating race ~a [~a]" name desc)

    (unless (and (symbolp symbol) (not (eq symbol nil)))
      (warn "Symbol for class ~s is ~s, please use a normal symbol" name symbol))

    (setf (class.id my-class) id
	  (class.symbol my-class) symbol)
    
    (setf (class.name my-class) name)
    (when desc
      (setf (class.desc my-class) desc))
    (when xp-extra
      (setf (class.xp-extra my-class) xp-extra))
    (if stat-changes
	(setf (class.stat-changes my-class)
	      (build-stat-table-from-symlist var-obj stat-changes))
	(setf (class.stat-changes my-class)
	      (make-stat-array var-obj)))

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
    (register-class& my-class)
    ;; returning the class
    my-class))


(defun get-title-for-level (class level)
  "Returns the title for a given class of the given level."
  (declare (type u-fixnum level))
  (let ((titles (class.titles class)))
    (elt titles (int-/ (1- level) 5))))



(defun (setf get-char-race) (race id)
  "Puts the race in appropriate tables id'ed by id."
  (check-type race character-race)
  (assert (or (stringp id) (symbolp id)))
  (setf (gethash id (variant.races *variant*)) race))

(defun get-char-race (id &key (variant *variant*))
  "Fetches the race id'ed by id from the appropriate table."
  (assert (or (stringp id) (symbolp id)))
  (let ((table (variant.races variant)))
    (etypecase id
      (string (gethash id table))
      (symbol (gethash id table)))))

(defun register-race& (race)
  "Registers a race in the right places."
  ;; add it to table
  (check-type race character-race) 
  (setf (get-char-race (race.id race)) race
	(get-char-race (race.symbol race)) race))
  

(defun get-races-as-a-list ()
  "Returns a fresh list of all races."
  (let* ((table (variant.races *variant*))
	 (hash-vals (loop for v being each hash-value of table
			  collecting v)))
    (remove-duplicates hash-vals)))


(defun define-character-race (id name &key symbol desc xp-extra stat-changes (abilities :unspec)
			      (resists :unspec)
			      (hit-dice :unspec)
			      classes starting-equipment skills)
  "defines a race and updates global race-list.  Both id and symbol will be
in the global race-table for easy access."
  
  (let ((race (make-instance 'character-race))
	(var-obj *variant*))

    (unless (and (stringp id) (verify-id id))
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
	(setf (race.stat-changes race) (build-stat-table-from-symlist var-obj stat-changes))
	(setf (race.stat-changes race) (make-stat-array var-obj)))

    (cond ((eq abilities :unspec))
	  ((listp abilities)
	   (dolist (i abilities)
	     (cond ((and (consp i) (eq (car i) '<resist>))
		    (when (> (length i) 2)'
		      (warn "FIX: resist with extra unhandled info: ~s" i))
		    (bit-flag-add! (race.resists race) (get-element-flag var-obj (cadr i))))
		   ;; fix the one below later
		   (t
		    (push i (race.abilities race))))))

	  (t
	   (error "Unknown format ~s for race-abilities for ~a"
		  abilities (race.name race))))

    (cond ((eq resists :unspec))
	  ((listp resists)
	   (dolist (i resists)
	     (cond ((and (symbolp i) (not (eq nil i)))
		    (bit-flag-add! (race.resists race) (get-element-flag var-obj i)))
		   (t
		    (error "Unknown resist argument ~s for race ~a"
			   i (race.name race))))))
	  (t
	   (error "Unknown resist argument ~s for race ~a"
			   resists (race.name race))))
	   

    (cond ((eq hit-dice :unspec))
	  ((and (integerp hit-dice) (plusp hit-dice))
	   (setf (race.hit-dice race) hit-dice))
	  (t
	   (error "Hit-dice ~s for race ~a is invalid." hit-dice (race.name race))))
    
    (when starting-equipment
      (setf (race.start-eq race) starting-equipment))
    
    (register-race& race)
    
    (setf (race.skills race) (build-skills-obj-from-list var-obj skills))

;;    (warn "Race ~a resists ~s" (race.name race) (race.resists race))
    
    ;; return the race
    race))
