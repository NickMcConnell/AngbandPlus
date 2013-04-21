;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/monsters.lisp - code related to monsters
Copyright (c) 2003 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)


(defclass spab ()
  ((id         :accessor spab.id         :initform "dummy"  :initarg :id)
   (type       :accessor spab.type       :initform 'generic :initarg :type)
   (mana-cost  :accessor spab.mana-cost  :initform 0        :initarg :mana-cost)
   (power      :accessor spab.power      :initform 0        :initarg :power)
   (effect     :accessor spab.effect     :initform nil)
   (vis-effect :accessor spab.vis-effect :initform nil)
   ))

(defclass summon-spab (spab)
  ((what :accessor spab.what :initform nil :initarg :what)))

(defclass ranged-spab (spab)
  ((range :accessor spab.range :initform 10 :initarg :range)
   (desc   :accessor spab.desc :initform "an arrow")
   (vis-effect :initform "arrow")
   ))

(defclass spell-spab (spab)
  ())

(defclass ball-spell-spab (spell-spab)
  ((desc   :accessor spab.desc :initform "ball")
   (damage :accessor spab.damage :initform 10)
   ))

(defclass bolt-spell-spab (spell-spab)
  ((desc   :accessor spab.desc :initform "bolt")
   (damage :accessor spab.damage :initform 10)
   ))

(defclass dmg-spell-spab (spell-spab)
  ())

(defclass breath-spab (spab)
  ((desc   :accessor spab.desc :initform "breath")
   (damage :accessor spab.damage :initform 10)
   ))

(defmethod get-visual-projectile ((obj spab))
  (spab.vis-effect obj))


(defvar *registered-spabs* (make-hash-table :test #'equal))

(defun define-monster-spabs (type ident spab-types)
  "Defines a number of spabs."

  (dolist (i spab-types)
    (let ((obj (make-instance type)))
      
      (ecase type
	(summon-spab (destructuring-bind (key &key what) i
		       (assert (nonboolsym? what))
		       (setf (spab.what obj) what)
		       (setf (gethash (list ident key) *registered-spabs*) obj)))
	(spell-spab  (destructuring-bind (key &key effect visual) i
		       (setf (spab.id obj) (format nil "~A" key)
			     (spab.type obj) key)
		       (when (functionp effect)
			 ;;(warn "Assign for ~s" key)
			 (setf (spab.effect obj) effect))

		       (when visual
			 (unless (is-legal-effect-type? visual)
			   (warn "Unknown visual ~s for spab ~s" visual key))
			 
			 (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
			   (setf (spab.vis-effect obj) lookup)))
			 
		       (setf (gethash (list ident key) *registered-spabs*) obj)))
	
	(breath-spab  (destructuring-bind (key &key breath-type desc damage visual) i
			(assert (is-legal-element? *variant* breath-type))
			(setf (spab.type obj) breath-type
			      (spab.desc obj) desc)
			(when (or (functionp damage) (positive-integer? damage))
			      (spab.damage obj) damage)
			
			(when visual
			  (unless (is-legal-effect-type? visual)
			    (warn "Unknown visual ~s for spab ~s" visual key))

			  (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
			    ;;(warn "visual lookup is ~s" lookup)
			    (setf (spab.vis-effect obj) lookup)))

			(setf (gethash (list ident key) *registered-spabs*) obj)))
	
	(bolt-spell-spab  (destructuring-bind (key &key type desc damage visual) i
			    (assert (is-legal-element? *variant* type))
			    
			    (setf (spab.type obj) type
				  (spab.desc obj) desc)
			    (when (or (functionp damage) (positive-integer? damage))
			      (spab.damage obj) damage)
			    
			    (when visual
			      (unless (is-legal-effect-type? visual)
				(warn "Unknown visual ~s for spab ~s" visual key))
			      
			      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
				(setf (spab.vis-effect obj) lookup)))

			    (setf (gethash (list ident key) *registered-spabs*) obj)))
	
	(ball-spell-spab  (destructuring-bind (key &key type damage desc visual) i
			    (assert (is-legal-element? *variant* type))

			    (setf (spab.type obj) type
				  (spab.desc obj) desc)
			    (when (or (functionp damage) (positive-integer? damage))
			      (spab.damage obj) damage)
			    
			    (when visual
			      (unless (is-legal-effect-type? visual)
				(warn "Unknown visual ~s for spab ~s" visual key))
			      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
				(setf (spab.vis-effect obj) lookup)))
			    
			    (setf (gethash (list ident key) *registered-spabs*) obj)))
	(dmg-spell-spab  (destructuring-bind (key &key power) i
			   (setf (spab.power obj) power)
			   (setf (gethash (list ident key) *registered-spabs*) obj)))
	(ranged-spab  (destructuring-bind (key &key power desc range) i
			(setf (spab.power obj) power
			      (spab.range obj) range
			      (spab.desc obj) desc)
			
			(when-bind (visual (spab.vis-effect obj))
			  (unless (is-legal-effect-type? visual)
			    (warn "Unknown visual ~s for spab ~s" visual key))
			  
			  (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
			    ;;(warn "visual lookup is ~s" lookup)
			    (setf (spab.vis-effect obj) lookup)))


			
			(setf (gethash (list ident key) *registered-spabs*) obj)))
	))
    ))

(defmethod print-object ((inst spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.id inst)))

  inst)

(defmethod print-object ((inst breath-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.type inst)))
  inst)

(defmethod print-object ((inst bolt-spell-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.type inst)))
  inst)

(defmethod print-object ((inst ball-spell-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.type inst)))
  inst)

(defmethod print-object ((inst summon-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.what inst)))
  inst)

(defmacro spab-effect (arguments &body body)
  (assert (= (length arguments) 4))
  (let ((def `(lambda ,arguments
               (declare (ignorable ,@arguments))
	       (block spab-effect
		 ,@body))))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability spab) target dungeon)
  (declare (ignore target dungeon))
  (warn "Vanilla special ability ~s not implemented" ability)
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability spell-spab) target dungeon)

  (when (functionp (spab.effect ability))
    (warn "FUNCALL: ~A" (spab.id ability))
    (return-from trigger-special-ability
      (funcall (spab.effect ability) creature ability target dungeon)))
  
  ;;(describe ability)

  (case (spab.type ability)
    (otherwise
     (warn "Vanilla spell ability ~s not implemented" ability)))
  
  
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability ball-spell-spab) target dungeon)
  (declare (ignore dungeon))
  ;;(warn "Ball-spell ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? *player*)
      (format-message! "~@(~A~) mumbles."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) casts a ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (spab.damage ability))
	(spell-effect (get-spell-effect (spab.type ability))))
    (when (functionp dmg)
      (setf dmg (funcall dmg (get-power-lvl creature))))
    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-fire-ball! creature target spell-effect dmg 3
		    :projected-object ability))
  
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability breath-spab) target dungeon)
  (declare (ignore dungeon))
  
  ;;(warn "Breath ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? *player*)
      (format-message! "~@(~A~) breathes."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) breathes ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (spab.damage ability))
	(spell-effect (get-spell-effect (spab.type ability))))
    (when (functionp dmg)
      (setf dmg (funcall dmg (current-hp creature))))
    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-breath! creature target spell-effect dmg 3
		 :projected-object ability))
  
  t)



(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability bolt-spell-spab) target dungeon)
  (declare (ignore dungeon))
  ;;(warn "Bolt-spell ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? *player*)
      (format-message! "~@(~A~) mumbles."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) casts a ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (spab.damage ability))
	(spell-effect (get-spell-effect (spab.type ability))))
    (when (functionp dmg)
      (setf dmg (funcall dmg (get-power-lvl creature))))
    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-fire-bolt! creature target spell-effect dmg
		    :projected-object ability))
  
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability ranged-spab) target dungeon)
  (declare (ignore dungeon))
  ;;(warn "Bolt-spell ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? *player*)
      (format-message! "~@(~A~) makes a strange noise."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) fires ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (roll-dice (spab.power ability) 6))
	(spell-effect (get-spell-effect '<arrow>)))

    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-fire-bolt! creature target spell-effect dmg
		    :projected-object ability))
  
  t)


(defun analyse-special-abilities! (variant mon-kind)
  "Analyses and updates special-abilities info."
  (declare (ignore variant))
  
  (let ((id (monster.id mon-kind))
	(sp-abilities (monster.sp-abilities mon-kind))
	(result '())
	(frequency 1))

    (dolist (i sp-abilities)
      (when (and (consp i) (eq (first i) '<frequency>))
	(unless (and (numberp (second i))
		     (> (second i) 0)
		     (<= (second i) 1))
	  (error-condition 'illegal-monster-data :id id
			   :desc "Frequency arg to special-abilities not between <0,1]."))
	(setf frequency (second i))))

    (dolist (ab sp-abilities)

      (cond ((and (consp ab) (eq (first ab) '<frequency>)))
	    (t
	     (let ((lookup (gethash ab *registered-spabs*)))
	       (cond (lookup
		      (push lookup result))
		     (t
		      (warn "~s gave ~s" ab lookup)))))
	    ))

    (when result
      ;; frequency first!
      (setf (monster.sp-abilities mon-kind) (cons (floor (* 100 frequency))
						  result)))
      
 
    mon-kind))

(defmethod initialise-monster-kind! ((var-obj vanilla-variant) (m-obj monster-kind) keyword-args)

  (call-next-method)

  (let ((id (monster.id m-obj)))

    (when-bind (depth (getf keyword-args :depth))
      (cond ((integerp depth)
	     (unless (non-negative-integer? depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative depth/power-lvl argument for monster"))
	     (setf (monster.power-lvl m-obj) depth))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown depth/power-lvl argument for monster-kind"))))
    
    (let ((depth (getf keyword-args :depth))
	  (rarity (getf keyword-args :rarity)))

      (cond ((and depth rarity)
	     (unless (integerp depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Non-integer depth argument for monster-kind"))
	     (unless (integerp rarity)
	       (signal-condition 'illegal-monster-data :id id :desc "Non-integer rarity argument for monster-kind"))
	     (unless (non-negative-integer? depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative depth argument for monster-kind"))
	     (unless (non-negative-integer? rarity)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative rarity argument for monster-kind"))
	     (push (cons depth rarity) (alloc-locations m-obj)))
	    ((and (eq depth nil) (eq depth rarity)))
	    (t
	     (signal-condition 'illegal-monster-data :id id
			       :desc "Unknown depth + rarity argument for monster-kind"))))

    #||
    (when (or (= (monster.power-lvl m-obj) 0)
	      (monster.power-lvl m-obj) 1)
      (setf (monster.sp-abilities m-obj)
	    '(;;(<breath> <nexus>) (<breath> <gravity>) (<breath> <shards>) (<breath> <confusion>)
	      ;;(<spell> <missile>) (<breath> <shards>)
	      ;;(<ball-spell> <cold>) (<breath> <cold>)
	      ;;(<arrow> 2) (<arrow> 3)
	      ;;(<spell> <darkness>)
	      ;;(<ball-spell> <mana>) (<ball-spell> <fire>) (<ball-spell> <acid>)
	      ;;(<breath> <sound>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
	      (<frequency> 1/4))))
    ||#
    
    (analyse-special-abilities! var-obj m-obj)
    
    m-obj))


(defun van-group-chance (id mon-depth lvl-depth)
  (declare (ignore id))
  (let* ((diff (- lvl-depth mon-depth))
	 (chance (if (plusp diff)
		     (* 10 diff)
		     0)))
      
      (when (> chance 60)
	(setq chance 60))

;;      (warn "Group chance for ~a (~a) at depth ~a is ~a%" id mon-depth lvl-depth chance)
      
      (if (plusp chance)
	  (< (random 100) chance)
	  nil)))

;; seems to be original depth + 4 which is the basis for when groups appear
(defun van-novice-appears-in-group? (level mon)

  (when (typep mon 'active-monster)
    (setq mon (amon.kind mon)))

  (unless (typep mon 'monster-kind)
    (error "Unknown object ~s given to grouping-function, should be a monster."
	   mon))
  
  (when (typep mon 'unique-monster)
    (error "A unique-monster ~s should not have a grouping-function."
	   (monster.id mon)))

  (let ((mon-depth (monster.power-lvl mon)) ;; a bit more tricky than vanilla, but gets increasingly worse
	(lvl-depth (level.depth level)))
    (van-group-chance (monster.id mon)
		      mon-depth
		      lvl-depth
		      )))


(defvar *primitive-attacker-ai* (make-instance 'primitive-melee-attacker))

;; we override to add our own stuff
(defmethod produce-active-monster ((variant vanilla-variant) mon-type)
  
  (let ((amon (call-next-method)))

    (setf (amon.strategies amon) (list *primitive-attacker-ai*))
    
    (flet ((install-attribute (&rest args)
	     (let ((attr (apply #'make-creature-attribute args)))
	       (unless (is-legal-effect? variant (attr.key attr))
		 (warn "The attribute ~s does not seem legal" attr))
	       (add-creature-attribute amon attr))))
      
      (install-attribute "stun" '<stun> :type :temporary ;; stun has special code
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "number, stun-power")
            
      (install-attribute "hasted" '<hasted> :type :temporary
			 :value nil :default-value nil
			 :update-fun #'%modify-boolean-effect
			 :desc "boolean, in vanilla hasted means +10")
      
      (install-attribute "slowed" '<slowed> :type :temporary
			 :value nil :default-value nil
			 :update-fun #'%modify-boolean-effect
			 :desc "boolean, in vanilla slowed means -10")
      
      (install-attribute "sleeping" '<sleeping> :type :temporary
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "integer, how sound asleep")

      (install-attribute "confusion" '<confusion> :type :temporary
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "integer, how confused")

      (install-attribute "fear" '<fear> :type :temporary
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-boolean-effect
			 :desc "integer, how afraid")
      
	   )
      
      amon))

(defmethod roll-saving-throw ((mon active-monster) attack-power)
  "Returns T if saving throw was made."
  ;;(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
  (> (get-power-lvl mon)
     (+ 10 (randint attack-power))))

#||
    (dolist (ab sp-abilities)
      (cond ((consp ab)
	     (ecase (first ab)
	       (<frequency> nil)
	       (<summon> (push (make-instance 'summon-spab :what (second ab)) result))
	       (<arrow>
		(unless (positive-integer? (second ab))
		  (error-condition 'illeagl-monster-data :id id
				   :desc "Non-positive power-argument to <arrow> sp-ab."))
		(push (make-instance 'ranged-spab :power) (second ab)))
	       
	       (<spell>
		(cond ((nonboolsym? (second ab))
		       (cond ((member (second ab) '(<confusion> <scare> <blindness> <teleport> <heal> <slow>
						    <forget> <mind-blast> <traps> <haste> <paralysis>
						    <darkness> <teleport-player> <blink> <missile>
						    <drain-mana> <brain-smash> <teleport-away> <teleport-level>))
			      (push (make-instance 'spell-spab :type (second ab)) result))
			     (t
			      (warn "Unknown spell ~s." (second ab))
			      )))
		      ((consp (second ab))
		       (let ((arg (second ab)))
			 (cond ((eq (first arg) '<ball>)
				(assert (and (nonboolsym? (second arg))
					     (is-legal-element? variant (second arg))))
				(push (make-instance 'ball-spell-spab :type (second arg)) result))
			       ((eq (first arg) '<bolt>)
				(assert (and (nonboolsym? (second arg))
					     (is-legal-element? variant (second arg))))
				(push (make-instance 'bolt-spell-spab :type (second arg)) result))
			       ((eq (first arg) '<cause>)
				(assert (positive-integer? (second arg)))
				(push (make-instance 'spell-spab :power (second arg) :symbol '<cause>) result))
			       (t
				(warn "Other spell ~s" arg)))))
		      (t
		       (warn "nasty spell ~s" ab))
		      ))
	       
	       (<breath>
		(cond ((nonboolsym? (second ab))
		       (push (make-instance 'breath-spab :type (second ab)) result))
		      (t
		       (warn "Have breath ~s" (cdr ab)))))
	       ))
	    (t
	     (warn "Ab ~s" ab))))
    ||#
