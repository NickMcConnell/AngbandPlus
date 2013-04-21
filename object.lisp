;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: object.lisp - code for object-kinds
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code for object-kinds which is basic and should be widely
ADD_DESC: available in the game.

|#

(in-package :org.langband.engine)


(defclass object-kind ()
    ((id         :accessor object.id
		 :initarg :id
		 :initform nil)
   
     (numeric-id :accessor object.numeric-id
		 :initarg :numeric-id
		 :initform nil)
   
     (name       :accessor object.name
		 :initarg :name
		 :initform nil)
   
     (x-attr     :accessor object.x-attr
		 :initarg :x-attr
		 :initform nil)
   
     (x-char     :accessor object.x-char
		 :initarg :x-char
		 :initform nil)
   
     (level      :accessor object.level
		 :initarg :level
		 :initform 0);; fix later
   
     (rarity     :accessor object.rarity
		 :initarg :rarity
		 :initform nil)
   
     (chance     :accessor object.chance
		 :initarg :chance
		 :initform (make-array 4 :initial-element 0))
   
     (locale     :accessor object.locale
		 :initarg :locale
		 :initform (make-array 4 :initial-element 0))
   
     (weight     :accessor object.weight
		 :initarg :weight
		 :initform nil)
   
     (cost       :accessor object.cost
		 :initarg :cost
		 :initform nil)
   
     (obj-type   :accessor object.obj-type
		 :initarg :obj-type
		 :initform nil);; replaces type-val/subtype-val

     (flags      :accessor object.flags
		 :initarg :flags
		 :initform nil)
   
     (game-values :accessor object.game-values
		  :initarg :game-values
		  :initform nil)

     (easy-know   :accessor object.easy-know
		  :initarg :easy-know
		  :initform nil
		  :documentation "Is it easy to understand what the object
is all about?")
     
     (aware :accessor object.aware
	    :initform nil
	    :documentation "The player is 'aware' of the item's effects")
   
     (tried      :accessor object.tried
		 :initform nil
		 :documentation "The player has 'tried' one of the items")
   
     (flavour    :accessor object.flavour
		 :initform nil)

     (sort-value :accessor object.sort-value
		 :initarg :sort-value
		 :initform 0)
   
     ;; should be a list of conses (event . function-obj)
     (events     :accessor object.events
		 :initarg :events
		 :initform nil)

     ;; list of conses
     (effects   :accessor object.effects
		:initarg :events
		:initform nil)

     (the-kind  :accessor object.the-kind
		:initarg :the-kind
		:initform nil)
     
     ))

(defmacro def-obj-type (name &key is key kind-slots aobj-slots)
  "Creates necessary objects and registers them."
  (let* ((ok-name (concat-pnames 'object-kind/ name))
	 (act-name (concat-pnames 'active-object/ name))
	 (ok-par-name (if is (concat-pnames 'object-kind/ is) 'object-kind))
	 (act-par-name (if is (concat-pnames 'active-object/ is) 'active-object))
	 (reg-call (when key `(setf (gethash ',key *obj-type-mappings*) (cons ',ok-name ',act-name)))))
    
    (let ((retval `(progn
		    (defclass ,ok-name (,ok-par-name) ,kind-slots)
		    (defclass ,act-name (,act-par-name) ,aobj-slots)
		    ,reg-call)))
      
;;      (warn "Ret: ~s" retval)
      
      retval)))


(def-obj-type weapon :key <weapon>)
(def-obj-type missile-weapon :is weapon)
(def-obj-type bow :is missile-weapon :key <bow>
	      :kind-slots ((multiplier :accessor object.multiplier :initform 1 :initarg :multiplier)))
(def-obj-type digger :is weapon :key <digger>)

(def-obj-type armour)
(def-obj-type body-armour :is armour :key <body-armour>)
(def-obj-type boots :is armour :key <boots>)
(def-obj-type gloves :is armour :key <gloves>)
(def-obj-type shield :is armour :key <shield>)
(def-obj-type headgear :is armour :key <headgear>)
(def-obj-type cloak :is armour :key <cloak>)

(def-obj-type potion :key <potion>)
(def-obj-type money :key <money>)
(def-obj-type scroll :key <scroll>)
(def-obj-type wand :key <wand>)
(def-obj-type staff :key <staff>)
(def-obj-type rod :key <rod>)
(def-obj-type book :key <book>)
(def-obj-type ring :key <ring>)
(def-obj-type chest :key <chest>)
(def-obj-type light-source :key <light-source>)
(def-obj-type ammo :key <ammo>)
(def-obj-type container :key <container>)

(def-obj-type food :key <food>)
(def-obj-type mushroom :is food :key <mushroom>)
(def-obj-type neckwear :key <neckwear>)
(def-obj-type amulet :is neckwear :key <amulet>)

(defmethod learn-about-object! (player object what)
  (error "Fell through learn with ~s ~s ~s" player object what))

(defmethod learn-about-object! (player (object active-object) (what (eql :aware)))
  (learn-about-object! player (aobj.kind object) what))

(defmethod learn-about-object! (player (object object-kind) (what (eql :aware)))
  (declare (ignore player))
  (setf (object.aware object) t))

(defmethod learn-about-object! (player (object active-object) (what (eql :tried)))
  (learn-about-object! player (aobj.kind object) what))

(defmethod learn-about-object! (player (object object-kind) (what (eql :tried)))
  (declare (ignore player))
  (setf (object.tried object) t))

(defmethod learn-about-object! (player (object active-object) (what (eql :known)))
  (declare (ignore player))
  (let ((flag (aobj.identify object)))
    (bit-flag-remove! flag +ident-sense+)
    (bit-flag-remove! flag +ident-empty+)
    (bit-flag-add! flag +ident-known+)
    (setf (aobj.identify object) flag)
    flag))

(defmethod learn-about-object! (player (object object-kind) (what (eql :known)))
  (declare (ignore player))
  (error "Learn :known must be called on an active-object.."))

(defmethod is-object-known? ((object active-object))
  (or (bit-flag-set? (aobj.identify object) +ident-known+)
      (and (object.easy-know (aobj.kind object))
	   (object.aware (aobj.kind object)))))

(defmethod get-attribute ((obj active-object))
  (get-attribute (aobj.kind obj)))

(defmethod get-attribute ((kind object-kind))
  (let ((flavour (object.flavour kind)))
    (if flavour
	(cadr flavour)
	(object.x-attr kind))))

(defmethod get-okind-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-otype-table var-obj level))
	 (table (gobj-table.obj-table o-table)))
    table))

(defmethod get-okind-alloc-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-otype-table var-obj level))
	 (table (gobj-table.alloc-table o-table)))
    table))

(defmethod get-object-kind ((variant variant) id)
  "Returns the object-kind for the given id."
  (assert (or (stringp id) (symbolp id)))
  
  (let ((table (get-okind-table variant *level*))
	(key (if (symbolp id) (symbol-name id) id)))
    (gethash key table)))


(defmethod object.name ((obj active-object))
  (object.name (aobj.kind obj)))

(defmethod object.obj-type ((obj active-object))
  "Forwards to the right place.."
  (object.obj-type (aobj.kind obj)))


(defmethod object.game-values ((obj active-object))
  (let ((gvals (aobj.game-values obj)))
    (if gvals
	gvals
	(object.game-values (aobj.kind obj)))))

(defmethod object.x-attr ((obj active-object))
  (object.x-attr (aobj.kind obj)))

(defmethod object.x-char ((obj active-object))
  (object.x-char (aobj.kind obj)))

(defun obj-is? (obj type-to-check)
  "Checks if given object satisfies given type"
  (if (find type-to-check (object.obj-type obj))
      t
      nil))

(defun obj-is-in? (obj type-list)
  "Checks if obj satisifes any of the types in type-list.
Uses OBJ-IS? to check. Returns the given type if succesful,
and NIL if unsuccesful."
  (dolist (x type-list)
    (when (obj-is? obj x)
      (return-from obj-is-in? x)))
  nil)

(defun objs-that-satisfy (demand &key (var-obj *variant*) (level *level*))
  "Returns a list of objects that satisfies the list of demands.
Returns NIL on failure."
  
  (let ((retval nil)
	(demand-list (if (listp demand) demand (list demand)))
	(table (get-okind-table var-obj level)))

    (loop for x being the hash-values of table
	  do
	  (let ((type-list (object.obj-type x))
		(satisfy-p t))
	    (dolist (i demand-list)
	      (unless (find i type-list :test #'eq)
		(setq satisfy-p nil)))
	    (when satisfy-p
	      (push x retval))))
    retval))

(defun create-aobj-from-id (id &key (amount 1) (variant *variant*))
  "Creates an active object from object-kind identified by id.
Amount specifies how many objects the active-object is, e.g for arrows.
Uses *VARIANT*."
  (let* ((kind (get-object-kind variant id))) ;; fix later
    (unless kind
      (return-from create-aobj-from-id nil))
    (create-aobj-from-kind kind :amount amount :variant variant)))


(defun create-aobj-from-kind-num (num &key (amount 1) (variant *variant*))
  "This is a hackish function which is backward compatible
with k-info.txt numbers. NUM is the numeric id."
  (create-aobj-from-id num :amount amount :variant variant))

(defun create-aobj-from-kind (kind &key (amount 1) (variant *variant*))
  "Creates an aobj from a given kind.  Uses *VARIANT*."
  (let ((obj (produce-active-object variant kind)))
    ;; assume dice
    (setf (aobj.number obj)
	  (cond ((and (stringp amount) (position #\d amount))
		 (parse-and-roll-dice amount))
		((stringp amount)
		 (parse-integer amount))
		((and (numberp amount) (plusp amount))
		 amount)
		(t
		 (warn "Invalid amount ~s for object ~s, assuming 1 instead."
		       amount (object.name kind))
		 1)))
	
    (trigger-event obj :on-create (list nil nil))
    (activate-object obj)
    obj))

(defmethod trigger-event ((obj active-object) event arg-list)
  "trigger any kind events first (add this obj as last argument).. then active object"
  (trigger-event (aobj.kind obj) event (append arg-list (list obj)))
  (apply-event event (aobj.events obj) arg-list))

(defmethod trigger-event ((obj object-kind) event arg-list)
  "trigger events registered for the kind."
  (apply-event event (object.events obj) arg-list))
  

(defun write-pluralised-string (stream plural-string number &key (flavour nil) (ident nil) (actual-name nil))
  (declare (type u-16b number)
	   (type simple-base-string plural-string))
  (let ((plural (> number 1)))
    (loop for i of-type u-16b from 0 to (1- (length plural-string))
	  for x = (schar plural-string i)
	  do
	  (case x
	    (#\~ (when plural
		   ;; hackish
		   (when (find (schar plural-string (1- i)) '(#\h #\s))
		     (write-char #\e stream))
		   (write-char #\s stream)))
	    
	    (#\& (if plural
		     (write-string (format nil "~a" number) stream)
		     (if (find (schar plural-string (+ i 2)) '(#\a #\e #\i #\o #\u #\y))
			 (write-string "an" stream)
			 (write-char #\a stream))))
	    (#\# (when flavour
		   (write-string flavour stream)
		   ;;(write-char #\Space s)
		   ))
	    
	    (#\@ (when ident
		   (write-string " of " stream)
		   (write-string actual-name stream)))
	    
	    (otherwise
	     (write-char x stream))))
    ))

(defun plural-name (number name flavour ident actual-name)
  "Returns a name with plurality fixed as in normal Angband.  FIX ME"
  (with-output-to-string (s)
    (write-pluralised-string s name number :flavour flavour :ident ident :actual-name actual-name)))


(defmethod write-obj-description ((variant variant) (obj active-object) stream &key (store nil))
  
  (let* ((o-type (aobj.kind obj))
	 (name (object.name obj))
	 (flavour (if store nil (object.flavour o-type)))
	 (known-type (or store (object.aware o-type)))
	 (number (aobj.number obj))
	 ;;(o-tlist (object.obj-type o-type))
	 ;;(plural-string nil)
	 )

    ;; temporary hack
    (when flavour (setf flavour (car flavour)))
    
;;    (warn "tot-str ~s" tot-str)

    (write-string 
    (cond ((obj-is? o-type '<mushroom>)
	   (plural-name number "& #mushroom~@" flavour known-type name))
	  ((obj-is? o-type '<potion>)
	   (plural-name number "& # potion~@" flavour known-type name))
	  ((obj-is? o-type '<ring>)
	   (plural-name number "& #ring~@" flavour known-type name))
	  ((obj-is? o-type '<potion>)
	   (plural-name number "& #amulet~@" flavour known-type name))
	  ((obj-is? o-type '<staff>)
	   (plural-name number "& #staff~@" flavour known-type name))
	  ((obj-is? o-type '<wand>)
	   (plural-name number "& #wand~@" flavour known-type name))
	  ((obj-is? o-type '<rod>)
	   (plural-name number "& #rod~@" flavour known-type name))
	  ((obj-is? o-type '<scroll>)
	   (plural-name number "& scroll~ #@" flavour known-type name))
	  ((obj-is? o-type '<amulet>)
	   (plural-name number "& #amulet~@" flavour known-type name))
	  (t
;;	   (warn "Fell through with object ~a ~s" name (object.obj-type o-type)) 
	   (plural-name number name nil known-type nil)))
    stream)

    ))

(defmethod write-obj-description ((variant variant) (obj active-object/book) stream &key store)
  (let ((known-type (or store (object.aware (aobj.kind obj)))))
    (write-pluralised-string stream "& ritual-book~ @" (aobj.number obj)
			     :ident known-type :actual-name (object.name obj))))
  
(defmethod write-obj-description ((variant variant) (obj active-object/weapon) stream &key store)
  "this one should be moved out into the variant directories.  it conses"
  (let* ((o-type (aobj.kind obj))
	 (number (aobj.number obj))
	 (known-obj (is-object-known? obj))
	 (base (plural-name number (object.name o-type) nil (or store known-obj) nil))
	 (gvals (object.game-values obj))
	 (tohit-bonus (if gvals (gval.tohit-bonus gvals) 0))
	 (dmg-bonus (if gvals (gval.dmg-bonus gvals) 0))
	 )
    (cond (known-obj
	   (format stream "~a (~@d,~@d)" base tohit-bonus dmg-bonus))
	  (t
	   (write-string base stream)))))

(defmethod is-eatable? (player obj)
  (declare (ignore obj player))
  nil)

;; possibly add this for potions
(defmethod is-eatable? (player (obj active-object/food))
  (declare (ignore player))
  t)

(defmethod is-eatable? (player (obj object-kind/food))
  (declare (ignore player))
  t)


(defmethod get-price ((object active-object) situation)
  (declare (ignore situation))
  (let* ((kind (aobj.kind object))
	 (id (is-object-known? object)))
    
    (if id
	(object.cost kind)
	20) ;; just a default
    ))

(defun get-object-list (&key (var-obj *variant*) (level *level*))
  "returns a fresh list.  Remove me!"
  (let ((table (get-okind-table var-obj level)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 #'<
		 :key #'object.numeric-id)))



(defun define-object-kind (id name
			   &key numeric-id x-attr x-char level rarity
			   chance locale weight cost obj-type sort-value
			   events game-values flags flavour desc the-kind
			   multiplier)
  "creates and establishes an object corresponding to parameters.  It uses
the *VARIANT* object so it has to be properly initialised."

  (declare (ignore flavour desc))
  (let* ((var-obj *variant*)
	 (new-obj (produce-object-kind var-obj id name obj-type :the-kind the-kind))
	 (key (if (symbolp id) (symbol-name id) id)))
    
    (when flags
      (when (find '<easy-know> flags)
	(setf (object.easy-know new-obj) t)
	(setf flags (remove '<easy-know> flags)))
      (setf (object.flags new-obj) flags))


    (setf (object.numeric-id new-obj) (if numeric-id
					  numeric-id
					  key)
	  (object.x-attr new-obj) (etypecase x-attr
				    (character (convert-obj x-attr :colour-code))
				    (number (charify-number x-attr)))
	  (object.x-char new-obj) x-char
	  (object.level new-obj) level
	  (object.rarity new-obj) rarity
	  (object.chance new-obj) chance
	  (object.locale new-obj) locale
	  (object.weight new-obj) weight
	  (object.cost new-obj) cost
	  (object.sort-value new-obj) (if (numberp sort-value)
					  sort-value
					  0) ;; hack
	  (object.events new-obj) (get-legal-events events)
	  (object.game-values new-obj) game-values)

    ;; hack, move away later
    (when (and multiplier (numberp multiplier) (typep new-obj 'object-kind/bow))
      (setf (object.multiplier new-obj) multiplier))

    
    ;; apply object-filters on the new object.
    (apply-filters-on-obj :objects var-obj new-obj)
    
    new-obj))

(defmethod get-loadable-form ((object object-kind) &key (full-dump nil))
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-val val)))))))
    (setf the-form (list 'define-object-kind 
			 (object.id object)
			 (object.name object)))
    (possibly-add :numeric-id (object.numeric-id object))
;;    (possibly-add :desc (object.desc object))
    (possibly-add :x-attr (convert-obj (object.x-attr object) :letter))
    (possibly-add :x-char (object.x-char object))
    (possibly-add :level (object.level object))
    (possibly-add :rarity (object.rarity object))
    (possibly-add :chance (object.chance object) #(0 0 0 0))
    (possibly-add :locale (object.locale object) #(0 0 0 0))
    (possibly-add :weight (object.weight object))
    (possibly-add :cost (object.cost object))
    (possibly-add :obj-type (object.obj-type object))
    (possibly-add :flags (object.flags object))
    (possibly-add :identified (object.tried object))
    (possibly-add :sort-value (object.sort-value object) 0)
    (possibly-add :easy-know (object.easy-know object))
    (possibly-add :the-kind (object.the-kind object))
    
    (when full-dump
      (possibly-add :flavour (object.flavour object)))


    (when-bind (gval (object.game-values object))
      (setf the-form (append the-form (list :game-values (get-loadable-form gval)))))
    
    the-form)))

(defun dump-objects (out-file &optional object-list)
  (let ((obj-list (if object-list
		      object-list
		      (get-object-list)))
	(*print-case* :downcase)
	(*print-right-margin* 120))
    
    (with-open-file (ffile (pathname out-file)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (pprint '(in-package :langband)
	      ffile)
      (terpri ffile)
      (dolist (x obj-list)
	(print (get-loadable-form x) ffile)
	(terpri ffile))
      (terpri ffile))))

(defmethod produce-active-object ((variant variant) (okind object-kind))
  "Returns an active-object based on the given okind."

  
  (let* ((wanted-kind (object.the-kind okind))
	 (mapping (when wanted-kind (gethash wanted-kind *obj-type-mappings*)))
	 (gvals (when (object.game-values okind) (copy-game-values variant (object.game-values okind))))
	 )
    (cond ((and mapping (consp mapping))
	   (make-instance (cdr mapping) :obj okind :game-values gvals))
	  (t 
	   (make-instance 'active-object :obj okind :game-values gvals)))))



(defmethod produce-object-kind ((variant variant) id name obj-type &key the-kind)
  "Produces a suitable object of type object-kind"

  (assert (or (stringp id) (symbolp id)))
  (assert (or (symbolp obj-type)
	      (and (consp obj-type)
		   (every #'symbolp obj-type))))
  (let* ((key (if (symbolp id) (symbol-name id) id))
	 (listed-obj-type (if (listp obj-type)
			      obj-type
			      (list obj-type)))

	 (has-mapping (gethash the-kind *obj-type-mappings*)))

    (cond ((consp has-mapping)
	   (make-instance (car has-mapping) :id key :name name
			  :obj-type listed-obj-type
			  :the-kind the-kind))
	  (t
	   ;;(warn "making ~s" obj-type)
	   (make-instance 'object-kind :id key :name name
			  :obj-type listed-obj-type))
	  )))



(defmethod print-object ((inst object-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (object.name inst) (object.level inst)))
  inst)

(defmethod print-object ((inst active-object) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~a ~S (~a,~a)]" (class-name (class-of inst)) 
	   (aobj.number inst) (aobj.kind inst) (location-x inst) (location-y inst))
  inst))
