;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: monster.lisp - monster-code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code which deals with critters you can meet in the dungeon.

||#

(in-package :org.langband.engine)

;; check the initform values here more thoroughly
(defclass monster-kind ()
  ((id        :initarg :id   :accessor monster.id        :initform "")
   (name      :initarg :name :accessor monster.name      :initform "")
   (desc      :accessor monster.desc      :initform "") ;; string 
   (symbol    :accessor monster.symbol    :initform nil) ;; character
   (colour    :accessor monster.colour    :initform nil) ;; varies with implementation
   (alignment :accessor monster.alignment :initform nil) ;; symbols/list
   (type      :accessor monster.type      :initform nil) ;; symbols/list
   (depth     :accessor monster.depth     :initform 0) ;; positive int
   (rarity    :accessor monster.rarity    :initform 0) ;; positive int
   (hitpoints :accessor monster.hitpoints :initform nil) ;; cons or a number I guess
   (armour    :accessor monster.armour    :initform nil) ;; integer
   (speed     :accessor monster.speed     :initform 0) ;; positive integer
   (xp        :accessor monster.xp        :initform 0) ;; poistive integer
   (sex       :accessor monster.sex       :initform nil) ;; symbol? 

   (abilities  :accessor monster.abilities  :initform nil)
   ;;   (resists :accessor monster.resists :initform nil)
   (immunities :accessor monster.immunities :initform nil)

   (alertness  :accessor monster.alertness  :initform 0) ;; how sleepy
   (vision     :accessor monster.vision     :initform 0) ;; how far can it see?
   (attacks    :accessor monster.attacks    :initform '()) ;; a list
   (treasures  :accessor monster.treasures  :initform '()) ;; a list
   
   (vulnerabilities   :accessor monster.vulnerabilities :initform nil)
   (special-abilities :accessor monster.sp-abilities    :initform nil)
   )) 

(defmethod has-ability? ((mon monster-kind) ability)
  (dolist (i (monster.abilities mon))
    (cond ((and (symbolp i) (eq ability i))
	   (return-from has-ability? i))
	  ((and (consp i) (eq (car i) ability))
	   (return-from has-ability? i))))
  nil)
	  
(defmethod has-ability? ((mon active-monster) ability)
  (has-ability? (amon.kind mon) ability))

(defmethod produce-monster-kind ((variant variant) id name &key the-kind)
  (declare (ignore the-kind))
  (assert (stringp id))
  (assert (stringp name))
  (make-instance 'monster-kind :id id :name name))

(defmethod produce-active-monster ((variant variant) mon-type)

  (assert (not (eq mon-type nil)))

  
  (let ((the-kind (cond ((symbolp mon-type)
			 (get-monster-kind variant (symbol-name mon-type)))
			((stringp mon-type)
			 (get-monster-kind variant mon-type))
			 ((typep mon-type 'monster-kind)
			  mon-type)
			 (t
			  (error "Mon-type argument to produce-active-monster is not {symbol,string,mkind}, but is ~s"
				 mon-type)))))

    (unless (typep the-kind 'monster-kind)
      (warn "Unable to find the monster-kind ~s" mon-type)
      (return-from produce-active-monster nil))

    (let ((amon (make-instance 'active-monster :kind the-kind))
	  (num-hitdice (car (monster.hitpoints the-kind)))
	  (hitdice (cdr (monster.hitpoints the-kind))))
    
      (if (has-ability? the-kind '<max-hitpoints>)
	  (setf (current-hp amon) (* num-hitdice hitdice))
	  (setf (current-hp amon) (roll-dice num-hitdice hitdice)))
      
      (setf (maximum-hp amon) (current-hp amon))
      (setf (get-creature-speed amon) (monster.speed the-kind))
      ;;    (warn "Monster ~a got ~a hp from ~a dice" (get-creature-name amon)
      ;;	  (current-hp amon) (monster.hitpoints kind))
      ;; blah
      amon)))



(defmethod get-xp-value ((creature active-monster))
  (monster.xp (amon.kind creature)))

(defmethod get-creature-name ((creature active-monster))
  (monster.name (amon.kind creature)))

(defmethod get-creature-ac ((creature active-monster))
  (monster.armour (amon.kind creature)))


(defmethod monster.name ((creature active-monster)) ;; remove eventually
  (monster.name (amon.kind creature)))

(defmethod get-creature-name ((creature player))
  (player.name creature))

(defmethod alter-xp! ((mon active-monster) amount)
  (declare (ignore amount))
  nil)

(defmethod monster.attacks ((mon active-monster))
  (monster.attacks (amon.kind mon)))

(defmethod get-mkind-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-mtype-table var-obj level))
	 (table (gobj-table.obj-table o-table)))
    table))

(defmethod get-mkind-alloc-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-mtype-table var-obj level))
	 (table (gobj-table.alloc-table o-table)))
    table))
 

(defmethod get-monster-kind ((variant variant) id)
  "Returns monster-kind or nil.  I think this one is quite limited."
  (assert (or (stringp id) (symbolp id)))
  (let ((table (get-mkind-table variant *level*))
	(key (if (symbolp id) (symbol-name id) id)))
;;    (warn "htbl has test ~a" (hash-table-test table))
    (gethash key table)))


(defmethod convert-obj (attacks (to (eql :attk-list)) &key)
  (mapcar #'(lambda (x)
	      (list (attack.kind x) :type (attack.dmg-type x) :damage (attack.damage x)))
	  attacks))

(defmethod convert-obj (attk-list (to (eql :attacks)) &key)
  "Converts attacks in list-form to a list of attack instances."
  (let ((attacks '()))
    (dolist (i attk-list)
      (cond ((consp i)
	     (assert (symbolp (car i)))
	     (let ((attk (make-instance 'attack :kind (car i))))
	       (do ((x (cdr i) (cddr x)))
		   ((null x))
		 (ecase (car x)
		   (:type (setf (attack.dmg-type attk) (cadr x)))
		   (:damage (setf (attack.damage attk) (cadr x)))))

	       (push attk attacks)))
	    (t
	     (warn "Unknown attack-info ~s" i))))
    (nreverse attacks)))


(defmethod get-monster-list ((var-obj variant))
  "returns a fresh list.  Remove me!"
  (let ((table (get-mkind-table var-obj *level*)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 #'string<
		 :key #'monster.id)))

(defun get-all-monsters (&key (var-obj *variant*))
  "Returns a fresh list."
  (let ((total-list '()))
    (loop for v being the hash-values of (variant.monsters var-obj)
	  do (push v total-list))
    (stable-sort total-list #'string< :key #'monster.id)))
   

(defun %parse-treasure-spec (spec)
  "Parses the spec and returns data in a good format."

  (assert (consp spec))

  (let ((drops '())
	(quality :normal)
	(type :any))
    
    (dolist (i spec)
      (assert (or (consp i) (symbolp i)))
      
      (cond ((eq i '<drop-good>)
	     (setf quality :good))
	    ((eq i '<drop-great>)
	     (setf quality :great))
	    ((eq i '<drop-planned>)
	     ;; fix this later
	     )
	    ((eq i '<only-drop-items>)
	     (setf type :item))
	    ((eq i '<only-drop-gold>)
	     (setf type :gold))
	    ((consp i)
	     (ecase (car i)
	       (<drop>
		(check-type (cadr i) string)
		(push (make-instance 'treasure-drop :amount (parse-dice (cadr i)))
		      drops))
	       (<drop-chance>
		(check-type (cadr i) number)
		(push (make-instance 'treasure-drop :chance (cadr i))
		      drops))))
	     
	    (t
	     (error "Unknown treasure-argument ~s" i)))
	    

      )

      (dolist (i drops)
	(setf (drop.quality i) quality)
	(setf (drop.type i) type))
      
      drops))


(defun define-monster-kind (id name &key desc symbol colour
			    alignment type depth ;;level
			    rarity hitpoints armour
			    speed xp abilities
			    immunities alertness
			    vulnerabilities
			    vision attacks special-abilities
			    (treasures :unspec)
			    sex appear-in-group?)
  "Defines a critter you might bump into when you least expect it. It uses
the *VARIANT* object so it has to be properly initialised."
  
;;  (lang-warn "Creating monster ~a [~a]" name id)

  (declare (ignore appear-in-group?))
  
  (assert (or (stringp id) (symbolp id)))
  (assert (stringp name))
  (check-type *variant* variant)
  
  (when (symbolp id)
    (warn "Deprecated id ~s for object ~s, use a legal string" id name))

  (unless (verify-id id)
    (error "Id ~s is not valid for a monster (~a)" id name)) 
  
  (let* ((var-obj *variant*)
	 (m-obj (produce-monster-kind var-obj id name)))

    
    (if (stringp desc)
	(setf (monster.desc m-obj) desc)
	(warn "No description for monster ~a found" id))

    (when symbol
      (setf (monster.symbol m-obj) symbol))
    (when colour
      (setf (monster.colour m-obj) (etypecase colour
				     (number (charify-number colour))
				     (character (convert-obj colour :colour-code))
				     )))
    (when xp
      (setf (monster.xp m-obj) xp))
    (when speed
      (setf (monster.speed m-obj) speed))
    (when hitpoints
      (setf (monster.hitpoints m-obj) hitpoints))
    (when armour
      (setf (monster.armour m-obj) armour))
    (when type
      (setf (monster.type m-obj) type))
    (when alignment
      (setf (monster.alignment m-obj) alignment))
    (when sex
      (setf (monster.sex m-obj) sex))

;;    (when (and level depth)
;;      (error "Both level and depth-arguments to define-mkind, please use only :depth"))

    ;; hack for bwards-compatibility
;;    (when level
;;      (setf depth level))
    
    (cond ((and depth (typep depth '(integer 0 *)))
	   (setf (monster.depth m-obj) depth))
	  (t
	   (lang-warn "Given illegal depth-value ~s for monster ~s" depth name)
	   (setf (monster.depth m-obj) 1))) ;; hack

    (if (and rarity (typep rarity '(integer 0 *))) 
	(setf (monster.rarity m-obj) rarity)
	(progn
	  (lang-warn "Given illegal rarity-value ~s for monster ~s" rarity name)
	  (setf (monster.rarity m-obj) 1))) ;; hack

    (when abilities
      (setf (monster.abilities m-obj) abilities))
    
    (when alertness
      (setf (monster.alertness m-obj) alertness))
    (when immunities
      (setf (monster.immunities m-obj) immunities))
    (when vulnerabilities
      (setf (monster.vulnerabilities m-obj) vulnerabilities))
    (when vision
      (setf (monster.vision m-obj) vision))
    (when attacks
      (cond ((consp attacks)
	     (setf (monster.attacks m-obj) (convert-obj attacks :attacks)))
	    (t
	     (lang-warn "Unknown form of attacks-argument ~s for monster ~s" attacks name))))

    (cond ((eq treasures :unspec))
	  ((consp treasures)
	   (setf (monster.treasures m-obj) (%parse-treasure-spec treasures)))
	  (t
	   (error "Illegal format for monster-treasures ~s" treasures)))

    ;; tough demand
    (when-bind (treasures (monster.treasures m-obj))
      (assert (every #'(lambda (x) (typep x 'treasure-drop)) treasures)))
    
;;    (when treasures
;;      (setf (monster.treasures m-obj) treasures))
    (when special-abilities
      (setf (monster.sp-abilities m-obj) special-abilities))
    

    ;; hackish addition to big object-table
    (let ((main-obj-table (variant.monsters var-obj))
	  (obj-id (monster.id m-obj)))
      (multiple-value-bind (val found-p)
	  (gethash obj-id main-obj-table)
	(declare (ignore val))
	(when found-p
	  (warn "Replacing monster with id ~s" obj-id))
	(setf (gethash obj-id main-obj-table) m-obj)))

    ;; applies the filters registered for newly read monsters
    (apply-filters-on-obj :monsters var-obj m-obj)
    ;;(add-new-mkind! m-obj id)
    
    m-obj))

(defmethod get-loadable-form ((object monster-kind) &key)
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-val val)))))))
      
      (setf the-form (list 'define-monster-kind 
			   (monster.id object)
			   (monster.name object)))
      
      (possibly-add :desc (monster.desc object))
      (possibly-add :symbol (monster.symbol object))
      (possibly-add :colour (convert-obj (monster.colour object) :letter))
      (possibly-add :alignment (monster.alignment object))
      (possibly-add :type (monster.type object))
      (possibly-add :depth (monster.depth object))
      (possibly-add :rarity (monster.rarity object))
      (possibly-add :hitpoints (monster.hitpoints object))
      (possibly-add :armour (monster.armour object))
      (possibly-add :speed (monster.speed object) 0)
      (possibly-add :xp (monster.xp object) 0)
      (possibly-add :abilities (monster.abilities object))
      (possibly-add :immunities (monster.immunities object))
      (possibly-add :vulnerabilities (monster.vulnerabilities object))
      (possibly-add :alertness (monster.alertness object))
      (possibly-add :vision (monster.vision object))
      (possibly-add :attacks (convert-obj (monster.attacks object) :attk-list))
      (possibly-add :treasures (monster.treasures object))
      (possibly-add :sex (monster.sex object))
      (possibly-add :special-abilities (monster.sp-abilities object))

      the-form)))

      
(defun dump-monsters (out-file &key (monster-list nil) (var-obj *variant*))
  (let ((mon-list (if monster-list
		       monster-list
		       (get-monster-list var-obj)))
	(*print-case* :downcase)
	(*print-right-margin* 120))
    
    (with-open-file (ffile (pathname out-file)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (pprint '(in-package :langband) ffile)
      (terpri ffile)
      (dolist (x mon-list)
	(print (get-loadable-form x) ffile)
	(terpri ffile))
      (terpri ffile))))


(defmethod print-object ((inst monster-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (monster.id inst)
	   (monster.name inst)))
  inst)


(defmethod print-object ((inst active-monster) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~S, (~s,~s)]" (class-name (class-of inst)) 
	   (amon.kind inst) (location-x inst) (location-y inst))
  inst))

(defmethod print-object ((inst treasure-drop) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a ~a ~a]" (class-name (class-of inst)) 
	   (drop.chance inst) (drop.quality inst) (drop.amount inst) (drop.type inst))
  inst))

