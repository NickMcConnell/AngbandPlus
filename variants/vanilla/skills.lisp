;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/skills.lisp - skill-related code for vanilla variant
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defmethod produce-skills-object ((variant vanilla-variant) &key (default-value 0))
  "Returns a skills object."
  (let ((obj (make-instance 'vanilla-skills)))
    (unless (and (numberp default-value) (= 0 default-value))
      (let ((skill-list (variant.skill-translations variant)))
	(dolist (i skill-list)
	  (setf (slot-value obj (cdr i)) default-value))))
    obj))

(defmethod get-melee-attack-skill ((variant vanilla-variant) (player player))
  (skills.fighting (player.skills player)))

(defmethod get-ranged-attack-skill ((variant vanilla-variant) (player player))
  (skills.shooting (player.skills player)))

(defmethod get-search-skill ((variant vanilla-variant) (player player))
  (skills.searching (player.skills player)))

(defun van/reset-skills! (variant skills-obj reset-val)
  "Sets all skills to RESET-VAL."
  (dolist (i (variant.skill-translations variant))
    (setf (slot-value skills-obj (cdr i)) reset-val)))

(defun van/add-to-a-skill! (which the-skills-obj player-lvl source)
  (declare (type fixnum player-lvl))
  (when source
    (let ((obj (slot-value source which)))
      (if (not obj)
	  (warn "Skill-slot ~s does have NIL value, please fix." which)
	  (incf (slot-value the-skills-obj which)
		(cond ((eq obj nil) 0)
		      ((numberp obj) obj)
		      ((van/skill-p obj)
		       (the fixnum (+ (the fixnum (van/skill.base obj))
				      (int-/ (* player-lvl (the fixnum (van/skill.lvl-gain obj)))
					     10))))
		      (t
		       (error "Unknown skill-obj ~a" obj)))
		)))))

(defmethod register-skill-translation& ((variant variant) translation)
  "Registers a translation (single cons) or a list
of translations."
  (flet ((add-single-translation (translation)
	   (pushnew translation (variant.skill-translations variant)
		    :test #'eql)))
    
    (when (consp translation) 
      (cond ((and (atom (car translation))
		  (atom (cdr translation)))
	     (warn "pushing a single one..")
	     (add-single-translation translation))
	  
	    (t
	     ;; we have a list
	     (dolist (i translation)
	       (assert (and (atom (car i))
			    (atom (cdr i))))
	       (add-single-translation i)))))
    
    translation))
  

(defmethod get-skill-translation ((variant variant) key)
  "Returns a symbol in the appropriate skills-class or nil."
  (let ((search (assoc key (variant.skill-translations variant))))
    (when search
      (cdr search))))


(defmethod build-skills-obj-from-list ((variant variant) skills)
  "Tries to build a skills-obj and include all possible
information from the list skills whose content depends on variant."
  
  (let ((skill-obj (produce-skills-object variant :default-value nil)))

    (unless (consp skills)
      (return-from build-skills-obj-from-list skill-obj))
    
    (dolist (i skills)
      (if (not (consp i))
	  (warn "Skill argument ~s must be a list: (skill base-val lvl-val)"
		i)
	  (let* ((skill-sym (first i))
		 (the-name (get-skill-translation variant skill-sym)))
	    (if (eq the-name nil)
		(warn "Unable to find skill-translation from ~s" skill-sym)
		(setf (slot-value skill-obj the-name)
		      (make-van/skill :name (string-downcase (string the-name))
				      :base (let ((base-arg (second i)))
					      (if base-arg base-arg 0))
				      :lvl-gain (let ((lvl-arg (third i)))
						  (if lvl-arg lvl-arg 0))))
		))))
    skill-obj))

#||
(defmethod produce-skills-object ((variant variant) &key (default-value 0))
  "Returns a skills object."
  (let ((obj (make-instance 'vanilla-skills)))
    (unless (and (numberp default-value) (= 0 default-value))
      (let ((skill-list (variant.skill-translations variant)))
	(dolist (i skill-list)
	  (setf (slot-value obj (cdr i)) default-value))))
    obj))
||#

