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


(defun get-classes-as-a-list (variant)
  "Returns all available classes."
  (remove-duplicates (loop for v being each hash-value of (variant.classes variant)
			   collecting v)))

#||
(defun get-classes-in-list (the-list variant)
  "Returns the classes for id's in the the-list."
  (let ((table (variant.classes variant)))
    (loop for i in the-list
	  collecting (gethash i table))))
||#

(defmethod produce-character-class ((variant variant) id name &key &allow-other-keys)
;;  (warn "Spells is ~s" spells)
  (make-instance 'character-class :id id :name name))

(defmethod initialise-character-class! ((var-obj variant) (my-class character-class) kwd-args)

  (let ((id (class.id my-class)))

    ;; handle name
    (when-bind (symbol (getf kwd-args :symbol))
      (when (eq symbol t)
	(warn "Symbol for class ~s is ~s, please use a normal symbol" id symbol))
      (setf (class.symbol my-class) symbol))

    ;; handle desc
    (when-bind (desc (getf kwd-args :desc))
      (check-type desc string)
      (setf (class.desc my-class) desc))

    ;; handle xp-extra
    (when-bind (xp-extra (getf kwd-args :xp-extra))
      (check-type xp-extra integer)
      (setf (class.xp-extra my-class) xp-extra))

    ;; handle hit-dice
    (when-bind (hit-dice (getf kwd-args :hit-dice))
      (check-type hit-dice integer)
      (setf (class.hit-dice my-class) hit-dice))

    ;; handle titles
    (when-bind (titles (getf kwd-args :titles))
      (unless (and (consp titles) (= (length titles) 10))
	(signal-condition 'illegal-char-class-data :id id :desc "Titles not a list of length 10."))
      (dolist (i titles)
	(when (> (length i) 15)
	  (signal-condition 'illegal-char-class-data :id id
			    :desc (format nil "Title ~s (~s) longer than 15 chars." i (length i)))))
      (setf (class.titles my-class) titles))

    ;; handle starting equipment
    (when-bind (starting-equipment (getf kwd-args :starting-equipment))
      (unless (consp starting-equipment)
	(signal-condition 'illegal-char-class-data :id id :desc "starting-equipment not a list."))
      (setf (class.start-eq my-class) starting-equipment))

    ;; handle abilities
    (when-bind (abilities (getf kwd-args :abilities))
      (assert (consp abilities))
      (dolist (i abilities)
	(cond ((consp i)
	       (warn "Don't know how to handle class-ability: ~s" i))
	      ((symbolp i)
	       (pushnew i (class.abilities my-class)))
	      (t
	       (warn "Unknown form of class-ability: ~s" i)))))
    
    
    (destructuring-bind (&key (mod-age :unspec)
			      (mod-status :unspec) stat-changes (resists :unspec)
			      (stat-sustains :unspec) &allow-other-keys)
	kwd-args
      (when (or (not (eq resists :unspec))
		(not (eq stat-sustains :unspec))
		)
	#+langband-extra-checks
	(warn "Unhandled resists/abilities/sustains ~s/~s/~s for class ~a"
	      resists abilities stat-sustains id))

      (if stat-changes
	  (setf (class.stat-changes my-class)
		(build-stat-table-from-symlist var-obj stat-changes))
	  (setf (class.stat-changes my-class)
		(make-stat-array var-obj)))

      (when (or (integerp mod-age) (consp mod-age) (functionp mod-age))
	(setf (class.mod-age my-class) mod-age))
	
      (when (or (integerp mod-status) (consp mod-status) (functionp mod-status))
	(setf (class.mod-status my-class) mod-status))
	 
      my-class)))
  
(defun define-character-class (id name &rest keyword-args &key &allow-other-keys)
  "Defines and establishes a class."

  (handler-case
      (let ((my-class nil))
	(unless (and (stringp id) (verify-id id))
	  (signal-condition 'illegal-char-class-data :id id
			    :desc "Id for class must be a string, use symbol for class-symbol."))

	(setf my-class (apply #'produce-character-class *variant* id name keyword-args))

	;; (warn "Creating class ~a [~a]" name desc)
	
	(check-type my-class character-class)

	(initialise-character-class! *variant* my-class keyword-args)
	
	;; adding it to the table
	(register-class& my-class)
    
	;; returning the class
	my-class)
    
    (illegal-char-class-data (co)
      (warn "Failed to initialise character-class [~a]: ~a"
	    (illegal-data.id co) (illegal-data.desc co))
      nil)))




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
  

(defun get-races-as-a-list (variant)
  "Returns a fresh list of all races."
  (let* ((table (variant.races variant))
	 (hash-vals (loop for v being each hash-value of table
			  collecting v)))
    (remove-duplicates hash-vals)))

(defmethod produce-character-race ((variant variant) id name &key &allow-other-keys)
;;  (warn "Spells is ~s" spells)
  (make-instance 'character-race :id id :name name))

(defmethod initialise-character-race! ((var-obj variant) (race character-race) keyword-args)
  (let ((id (race.id race)))

    (when-bind (symbol (getf keyword-args :symbol))
      (cond ((nonboolsym? symbol)
	     (setf (race.symbol race) symbol))
	    (t
	     (signal-condition 'illegal-char-race-data :id id :desc "Illegal symbol for character-race"))))

    (when-bind (desc (getf keyword-args :desc))
      (cond ((stringp desc)
	     (unless (plusp (length desc))
	       (signal-condition 'illegal-char-race-data :id id :desc "empty desc for char-race"))
	     (setf (race.desc race) desc))
	    (t
	     (signal-condition 'illegal-char-race-data :id id :desc "Unknown format for desc data for char-race"))))

    (when-bind (xp-extra (getf keyword-args :xp-extra))
      (cond ((non-negative-integer? xp-extra)
	     (setf (race.xp-extra race) xp-extra))
	    (t
	     (signal-condition 'illegal-char-race-data :id id :desc "xp-extra argument not non-negative integer"))))

    (when-bind (base-age (getf keyword-args :base-age))
      (cond ((non-negative-integer? base-age)
	     (setf (race.base-age race) base-age))
	    (t
	     (signal-condition 'illegal-char-race :id id :desc "base-age argument not non-negative integer"))))


    
    (destructuring-bind (&key (mod-age :unspec) (base-status :unspec)
			      (mod-status :unspec) stat-changes (abilities :unspec)
			      (resists :unspec) (stat-sustains :unspec)
			      (hit-dice :unspec)
			      (m-height :unspec) (m-height-mod :unspec)
			      (f-height :unspec) (f-height-mod :unspec)
			      (m-weight :unspec) (m-weight-mod :unspec)
			      (f-weight :unspec) (f-weight-mod :unspec)
			      classes starting-equipment &allow-other-keys)
	keyword-args

      (when classes
	(setf (race.classes race) classes))

      (when (or (integerp mod-age) (consp mod-age) (functionp mod-age))
	(setf (race.mod-age race) mod-age))
    
      (when (non-negative-integer? base-status)
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


      race)))

(defun define-character-race (id name &rest keyword-args &key &allow-other-keys)
  "defines a race and updates global race-list.  Both id and symbol will be
in the global race-table for easy access."

  (handler-case
      (let ((race nil))
	(unless (and (stringp id) (verify-id id))
	  (signal-condition 'illegal-char-race-data "Id for race must be a string, use symbol for race-symbol."))

	(setf race (apply #' produce-character-race *variant* id name keyword-args))

	(check-type race character-race)
	
	(initialise-character-race! *variant* race keyword-args)
	
	(register-race& race)
	
	;;    (warn "Race ~a resists ~s" (race.name race) (race.resists race))
    
	;; return the race
	race)
    
    (illegal-char-class-data (co)
      (warn "Failed to initialise character-class [~a]: ~a"
	    (illegal-data.id co) (illegal-data.desc co))
      nil)))



(defmethod get-character-picture ((variant variant) (player player))
  nil)
