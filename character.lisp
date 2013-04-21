;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: character.lisp - code related to character class, race, ...
Copyright (c) 2002-2003 - Stig Erik Sandø

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
  (make-instance 'character-class :id id :name name))

(defun define-character-class (id name &rest args &key symbol desc xp-extra
			       (mod-age :unspec)
			       (mod-status :unspec)
			       stat-changes (resists :unspec)
			       (abilities :unspec) titles (stat-sustains :unspec)
			       starting-equipment hit-dice skills &allow-other-keys)
  "Defines and establishes a class."

;;  (warn "Defining class ~s with args ~s" id args)

  (unless (and (stringp id) (verify-id id))
    (warn "Id ~s for class ~s must be a string, use symbol for class-symbol."))
  
  (let* ((var-obj *variant*)
	 (my-class (apply #'produce-character-class var-obj id name args)))
	
    ;;    (warn "Creating class ~a [~a]" name desc)

    (check-type var-obj variant)
    (check-type my-class character-class)
    
    (when (or (not (eq resists :unspec))
	      (not (eq abilities :unspec))
	      (not (eq stat-sustains :unspec))
	      )
      #+langband-extra-checks
      (warn "Unhandled resists/abilities/sustains ~s/~s/~s for class ~a"
	    resists abilities stat-sustains name))

    (unless (and (symbolp symbol) (not (eq symbol nil)))
      (warn "Symbol for class ~s is ~s, please use a normal symbol" name symbol))

    (setf (class.symbol my-class) symbol)

    (when desc
      (setf (class.desc my-class) desc))
    (when xp-extra
      (setf (class.xp-extra my-class) xp-extra))
    (if stat-changes
	(setf (class.stat-changes my-class)
	      (build-stat-table-from-symlist var-obj stat-changes))
	(setf (class.stat-changes my-class)
	      (make-stat-array var-obj)))

    (when (or (integerp mod-age) (consp mod-age) (functionp mod-age))
      (setf (class.mod-age my-class) mod-age))
	
    (when (or (integerp mod-status) (consp mod-status) (functionp mod-status))
      (setf (class.mod-status my-class) mod-status))
	 

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

(defmethod produce-character-race ((variant variant) id name &key &allow-other-keys)
;;  (warn "Spells is ~s" spells)
  (make-instance 'character-race :id id :name name))


(defun define-character-race (id name &rest args &key symbol desc xp-extra
			      (base-age :unspec)
			      (mod-age :unspec)
			      (base-status :unspec)
			      (mod-status :unspec)
			      stat-changes (abilities :unspec)
			      (resists :unspec)
			      (stat-sustains :unspec)
			      (hit-dice :unspec)
			      (m-height :unspec) (m-height-mod :unspec)
			      (f-height :unspec) (f-height-mod :unspec)
			      (m-weight :unspec) (m-weight-mod :unspec)
			      (f-weight :unspec) (f-weight-mod :unspec)
 			      classes starting-equipment skills &allow-other-keys)
  "defines a race and updates global race-list.  Both id and symbol will be
in the global race-table for easy access."

  (unless (and (stringp id) (verify-id id))
    (warn "Id ~s for race ~s must be a string, use symbol for race-symbol."))
  ;;      (warn "Creating race ~a [~a]" name desc)

  
  (let* ((var-obj *variant*)
	 (race (apply #' produce-character-race var-obj id name args)))

    (check-type race character-race)

    (unless (and (symbolp symbol) (not (eq symbol nil)))
      (warn "Symbol for race ~s is ~s, please use a normal symbol" name symbol))
    
    (setf (race.symbol race) symbol)
    
    (when desc
      (setf (race.desc race) desc))
    (when xp-extra
      (setf (race.xp-extra race) xp-extra))
    (when classes
      (setf (race.classes race) classes))

    (when (integerp base-age)
      (setf (race.base-age race) base-age))

    (when (or (integerp mod-age) (consp mod-age) (functionp mod-age))
      (setf (race.mod-age race) mod-age))
    
    (when (integerp base-status)
      (setf (race.base-status race) base-status))
	
    (when (or (integerp mod-status) (consp mod-status) (functionp mod-status))
      (setf (race.mod-status race) mod-status))
	

    (loop for i in (list m-height m-height-mod f-height f-height-mod
			 m-weight m-weight-mod f-weight f-weight-mod)
	  for j in '(m-height m-height-mod f-height f-height-mod
		     m-weight m-weight-mod f-weight f-weight-mod)
	  do
	  (when (integerp i)
	    (setf (slot-value race j) i)))
	  
    
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

    ;; add better error-checking vs bad elements?
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

    ;; add better error-checking vs bad elements?
    (cond ((eq stat-sustains :unspec))
	  ((eq stat-sustains nil))
	  ((consp stat-sustains)
	   (let ((the-sustains (make-array (variant.stat-length var-obj) :initial-element nil)))
	     (dolist (i stat-sustains)
	       (cond ((and (symbolp i) (not (eq nil i)))
		      (setf (aref the-sustains (get-stat-num-from-sym i)) t))
		     (t
		      (error "Unknown sustain argument ~s for race ~a"
			     i (race.name race)))
		     ))
	     (setf (race.stat-sustains race) the-sustains)))
	   (t
	   (error "Unknown sustain argument ~s for race ~a"
			   stat-sustains (race.name race))))


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
