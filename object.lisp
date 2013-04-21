;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: object.lisp - code for object-kinds
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code for object-kinds which is basic and should be widely
ADD_DESC: available in the game.

|#

(in-package :org.langband.engine)


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

(defmethod get-colour ((obj active-object))
  (get-colour (aobj.kind obj)))

(defmethod get-colour ((kind object-kind))
  (let ((flavour (object.flavour kind)))
    (if flavour
	(cdr flavour)
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

(defmethod object.weight ((obj active-object))
  (* (aobj.number obj) (object.weight (aobj.kind obj))))

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
	 (known-type (or store (is-object-known? obj)))
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
	   (plural-name number "& # ring~@" flavour known-type name))
	  ((obj-is? o-type '<staff>)
	   (plural-name number "& # staff~@" flavour known-type name))
	  ((obj-is? o-type '<wand>)
	   (plural-name number "& # wand~@" flavour known-type name))
	  ((obj-is? o-type '<rod>)
	   (plural-name number "& # rod~@" flavour known-type name))
	  ((obj-is? o-type '<scroll>)
	   (plural-name number "& scroll~ #@" flavour known-type name))
	  ((obj-is? o-type '<amulet>)
	   (plural-name number "& # amulet~@" flavour known-type name))
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
	 (tohit-mod (if gvals (gval.tohit-modifier gvals) 0))
	 (dmg-mod (if gvals (gval.dmg-modifier gvals) 0))
	 )
    (cond (known-obj
	   (format stream "~a (~@d,~@d)" base tohit-mod dmg-mod))
	  (t
	   (write-string base stream)))))

(defmethod is-eatable? ((player player) (obj active-object))
  nil)

;; possibly add this for potions
(defmethod is-eatable? ((player player) (obj active-object/food))
  t)

(defmethod is-eatable? ((player player) (obj object-kind/food))
  t)

(defmethod is-magical? ((obj active-object))
  nil)

(defmethod is-artifact? ((obj active-object))
  nil)

(defmethod get-price ((object active-object) situation)
  (declare (ignore situation))
  (let* ((kind (aobj.kind object))
	 (known-p (is-object-known? object)))

    ;; skip broken/cursed

    ;; also ignore discounts
    
    (if known-p
	(object.cost kind)
	(typecase object
	  (active-object/food 5)
	  (active-object/potion 20)
	  (active-object/scroll 20)
	  (active-object/staff 70)
	  (active-object/wand 50)
	  (active-object/rod 90)
	  (active-object/ring 45)
	  (active-object/amulet 45)
	  (otherwise 0)))))


(defun get-object-list (&key (var-obj *variant*) (level *level*))
  "returns a fresh list.  Remove me!"
  (let ((table (get-okind-table var-obj level)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 #'<
		 :key #'object.numeric-id)))


(defmethod copy-active-object ((variant variant) (obj active-object))
  "Copies the given OBJ and returns a new object that is equal."
  
  (let ((new-obj (make-instance (class-of obj))))
;;    (warn "Old ~s and new ~s" (class-of obj) (class-of new-obj))
    ;; needs improvement
    (dolist (i '(kind inscription number loc-x loc-y identify marked))
      ;; doesn't handle shared-structures well
      (setf (slot-value new-obj i) (slot-value obj i)))
    (when-bind (gvals (aobj.game-values obj))
      (setf (aobj.game-values new-obj) (copy-game-values variant gvals)))
    ;; skip contains
    ;; skip events
    new-obj))

(defun is-object-effect? (arg)
  (functionp arg))

;; hack
(defmacro object-effect (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defun define-object-kind (id name
			   &key numeric-id x-attr x-char depth rarity
			   chance locale weight cost obj-type sort-value
			   events game-values flags flavour desc the-kind
			   multiplier (on-quaff :unspec)
			   (on-read :unspec) (on-eat :unspec))
  "creates and establishes an object corresponding to parameters.  It uses
the *VARIANT* object so it has to be properly initialised."

  (declare (ignore flavour desc))
  (let* ((var-obj *variant*)
	 (new-obj (produce-object-kind var-obj id name obj-type :the-kind the-kind))
	 (key (if (symbolp id)
		  (string-downcase (symbol-name id))
		  id)))

    (when (symbolp id)
      (warn "Deprecated id for object ~s" id))
    
    (when flags
      (when (find '<easy-know> flags)
	(setf (object.easy-know new-obj) t)
	(setf flags (remove '<easy-know> flags)))
      (setf (object.flags new-obj) flags))

;;    (when (and depth level)
;;      (error "Object ~s given both level and depth." key))

;;    (when level
;;      (setf depth level))

    (cond ((and depth (typep depth '(integer 0 *)))
	   (setf (object.depth new-obj) depth))
	  (t
	   (lang-warn "Given illegal depth-value ~s for object ~s" depth key)
	   (setf (object.depth new-obj) 1))) ;; hack


    (setf (object.numeric-id new-obj) (if numeric-id
					  numeric-id
					  key)
	  (object.x-attr new-obj) (etypecase x-attr
				    (character (convert-obj x-attr :colour-code))
				    (number (charify-number x-attr)))
	  (object.x-char new-obj) x-char
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

    (flet ((possible-add-effect (effect var &optional (energy +energy-normal-action+))
	     (cond ((eq :unspec var))
		   ((is-object-effect? var)
		    (let ((entry (make-effect-entry :type effect
						    :fun var
						    :energy-use energy)))
		      (pushnew entry (object.effects new-obj) :key #'effect-entry-type)))
		   (t
		    (error "Unknown value ~s for ~s for ~s" var effect key)))))
      
      (possible-add-effect :quaff on-quaff)
      (possible-add-effect :read on-read)
      (possible-add-effect :eat on-eat)
      )
    
    ;; hack, move away later
    (when (and multiplier (numberp multiplier) (typep new-obj 'object-kind/bow))
      (setf (object.multiplier new-obj) multiplier))
    
    ;; hackish addition to big object-table
    (let ((main-obj-table (variant.objects var-obj))
	  (obj-id (object.id new-obj)))
      (multiple-value-bind (val found-p)
	  (gethash obj-id main-obj-table)
	(declare (ignore val))
	(when found-p
	  (warn "Replacing object with id ~s" obj-id))
	(setf (gethash obj-id main-obj-table) new-obj)))
    
    ;; apply object-filters on the new object.    
    (apply-filters-on-obj :objects var-obj new-obj)
    
    new-obj))


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


(defun define-flavour-type (symbol &optional generator-fn)
  "Defines a flavour-type"
  (let* ((var-obj *variant*)
	 (ft-obj (make-flavour-type :symbol symbol
				    :generator-fn generator-fn))
	 (table (variant.flavour-types var-obj)))
    (setf (gethash symbol table) ft-obj)
    ft-obj))

(defun legal-flavour-obj? (flav)
  (and (consp flav)
       (stringp (car flav))
       (atom (car flav)) ;; integer 0..16 ?
       ))
  
(defun establish-flavour& (table name colour)
  (setf (gethash name table) (cons name colour)))

(defun find-flavour-type (variant-obj type)
  "Tries to find given flavour-type in given variant-obj."
  (gethash type (variant.flavour-types variant-obj)))

;;(trace find-flavour-type)

(defun define-basic-flavour (type name colour)
  "Defines a basic flavour.."
  
  (let ((ft-obj (find-flavour-type *variant* type)))
    (unless ft-obj
      (warn "Unable to find flavour-type ~s" type)
      (return-from define-basic-flavour nil))
    (let ((table (flavour-type.table ft-obj)))
      (establish-flavour& table name colour))))
	


(defun use-flavour-table (flavour-to-use used-by &key (variant *variant*))
  "a handy way to re-use a flavour-table for another kind
of objects.  all entries are copied, not shared."
  
  (let* ((var-obj variant)
	 (used-by-type (find-flavour-type var-obj used-by))
	 (type-to-use (find-flavour-type var-obj flavour-to-use))
	 (old-table (flavour-type.table type-to-use))
	 (new-table (flavour-type.table used-by-type)))
	 
    
    (maphash #'(lambda (key val)
		 (setf (gethash key new-table) val))
	     old-table)
    
    ;;(warn "~s will use ~s" used-by-type type-to-use)
    
    used-by-type))

(defmethod flavour-object! ((variant variant) (obj object-kind))
  ;; do nothing
  (warn "Not added flavouring to ~a" obj)
  nil)

(defun %flavour-obj-kind! (obj)
  "Flavours the given object OBJ."
  (let* ((kind obj) ;; hack
	 (var-obj *variant*)
	 (f-type (gethash (object.the-kind obj) (variant.flavour-types var-obj))))
    (when f-type
      (let ((gen-fn (flavour-type.generator-fn f-type))
	    (table (flavour-type.table f-type)))
	(cond (gen-fn
	       (setf (object.flavour kind) (funcall gen-fn var-obj kind)))
	      ((and table (typep table 'array))
	       (let ((next-flavour (aref table (fill-pointer table))))
		 (setf (object.flavour kind) next-flavour)
		 (incf (fill-pointer table))))
	      (t
	       (describe f-type)
	       (error "Unable to flavour object kind ~a with ~s" kind (flavour-type.symbol f-type))
	       ))
    
	(assert (legal-flavour-obj? (object.flavour kind)))))
    obj))
