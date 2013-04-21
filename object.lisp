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

     (identified :accessor object.identified
		 :initform nil)
   
     (tried      :accessor object.tried
		 :initform nil)
   
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
     
     ))

(defun get-okind-table ()
  (let* ((o-table (get-otype-table *level* *variant* ))
	 (table (gobj-table.obj-table o-table)))
    table))

(defun %get-okind-alloc-tbl ()
  (let* ((o-table (get-otype-table *level* *variant*))
	 (table (gobj-table.alloc-table o-table)))
    table))

(defun get-obj-kind (id)
  "Returns the obj-kind for the given id."
  (assert (or (stringp id) (symbolp id)))
  (let ((table (get-okind-table))
	(key (if (symbolp id) (symbol-name id) id)))
    (gethash key table)))

#||
(defun (setf get-obj-kind) (obj id)
  ;;  (warn "Adding ~a" obj)
  "Adds the obj to the obj-kind table identified by given id."
  (error "do not use me")
  (let ((table (get-okind-table)))
    (setf (gethash id table) obj)) )
||#

(defun add-new-okind! (obj id)
  ""
  (declare (ignore id))
;;  (warn "Adding obj with id ~s" id)
  (apply-filters-on-obj :objects *variant* obj))


(defun define-object-kind (dummy-arg id name
			   &key numeric-id x-attr x-char level rarity
			   chance locale weight cost obj-type sort-value
			   events)
  "creates and establishes an object corresponding to parameters"
  (declare (ignore dummy-arg))
  
  (assert (or (stringp id) (symbolp id)))
  
  (let* ((key (if (symbolp id) (symbol-name id) id))
	 (new-obj (make-instance 'object-kind
				:id key
				:numeric-id (if numeric-id
						numeric-id
						key)
				:name name
				:x-attr x-attr
				:x-char x-char
				:level level
				:rarity rarity
				:chance chance
				:locale locale
				:weight weight
				:cost cost
				:sort-value sort-value
				:obj-type (if (listp obj-type)
					      obj-type
					      (list obj-type))
				:events (get-legal-events events)
				)))
    
    (add-new-okind! new-obj id)
;;    (apply-filters-on-obj :objects *variant* new-obj)
;;    (setf (get-obj-kind id) new-obj)
    
    new-obj))

(defmethod object.obj-type ((obj active-object))
  "Forwards to the right place.."
  (object.obj-type (aobj.kind obj)))


(defmethod object.game-values ((obj active-object))
  (object.game-values (aobj.kind obj)))


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

(defun objs-that-satisfy (demand)
  "Returns a list of objects that satisfies the list of demands.
Returns NIL on failure."
  
  (let ((retval nil)
	(demand-list (if (listp demand) demand (list demand)))
	(table (get-okind-table)))

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

(defun create-aobj-from-id (id &optional (amount 1))
  "Creates an active object from object-kind identified by id.
Amount specifies how many objects the active-object is, e.g for arrows."
  (let ((kind (get-obj-kind id))) ;; fix later
    (unless kind
      (return-from create-aobj-from-id nil))
    (create-aobj-from-kind kind amount)))


(defun create-aobj-from-kind-num (num &optional (amount 1))
  "This is a hackish function which is backward compatible
with k-info.txt numbers. NUM is the numeric id."
  (create-aobj-from-id num amount))

(defun create-aobj-from-kind (kind &optional (amount 1))
  "Creates an aobj from a given kind."
  (let ((obj (make-instance 'active-object :obj kind :number amount)))
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
  

(defun get-obj-kind-by-level (level)
  "Returns an object-kind for the given level.."

  ;; skipping boost
  
  (let ((total 0)
	(counter 0)
	(table (%get-okind-alloc-tbl)))

;;    (warn "O-Table[~a,~a] is ~a" *level* level (length table))

    (loop named counting-area
	  for a-obj across table
	  do
;;	  (warn "Checking ~a" a-obj)
	  (when (> (alloc.level a-obj) level)
	    (return-from counting-area nil))
	  ;; skip chest-check
	  (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))
	  (incf total (alloc.prob3 a-obj))
	  (incf counter))


    (when (= 0 total)
      (warn "No suitable objects at level ~a [~a]" level  *level*)
      (return-from get-obj-kind-by-level nil))

    (let ((val (random total)))
;;      (warn "Counter is ~a and total is ~a and we got ~a" counter total val)

      ;; loop through objects and find one
      (loop for a-obj across table
	    do
	    (when (< val (alloc.prob3 a-obj))
	      (return-from get-obj-kind-by-level (alloc.obj a-obj)))
	    (decf val (alloc.prob3 a-obj)))
      
      ))
  
  nil)

;;(trace get-obj-kind-by-level)

(defun get-obj-by-level (level)
  "Returns an (active) object by level."
  (let ((the-kind (get-obj-kind-by-level level)))
    (if (not the-kind)
	nil
	(create-aobj-from-kind the-kind))))


(defun plural-name (number name flavour ident actual-name)
  "Returns a name with plurality fixed as in normal Angband.  FIX ME"
  (declare (type u-16b number)
	   (type simple-base-string name))
  (let ((plural (> number 1)))
    (with-output-to-string (s)
      (loop for i of-type u-16b from 0 to (1- (length name))
	    for x = (schar name i)
	    do
	    (case x
	      (#\~ (when plural
		     ;; hackish
		     (when (find (schar name (1- i)) '(#\h #\s))
		       (write-char #\e s))
		     (write-char #\s s)))
	      
	      (#\& (if plural
		       (write-string (format nil "~a" number) s)
		       (if (find (schar name (+ i 2)) '(#\a #\e #\i #\o #\u #\y))
			   (write-string "an" s)
			   (write-char #\a s))))
	      (#\# (when flavour
		     (write-string flavour s)
		     (write-char #\Space s)
		     ))

	      (#\@ (when ident
		     (write-string " of " s)
		     (write-string actual-name s)))
	      
	      (otherwise
	       (write-char x s))))
      )))

(defmethod description ((item active-object) &key store (number nil))
  (description (aobj.kind item) :store store :number (if number number (aobj.number item))))

(defmethod description ((o-type object-kind) &key store (number 1))
  "this one should be moved out into the variant directories.  it conses"
  
  (let ((name (object.name o-type))
	(flavour (if store nil (object.flavour o-type)))
	(known-type (or store (object.identified o-type)))
	;;(o-tlist (object.obj-type o-type))
	;;(plural-string nil)
	)

    ;; temporary hack
    (when flavour (setf flavour (car flavour)))
    
;;    (warn "tot-str ~s" tot-str)

    (cond ((obj-is? o-type '<mushroom>)
	   (plural-name number "& #mushroom~@" flavour known-type name))
	  ((obj-is? o-type '<potion>)
	   (plural-name number "& #potion~@" flavour known-type name))
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
	  ((obj-is? o-type '<spellbook>)
	   (plural-name number "& ritual-book~ #@" flavour known-type name))
	  (t
;;	   (warn "Fell through with object ~a ~s" name (object.obj-type o-type)) 
	   (plural-name number name nil known-type nil)))

    ))

;;(trace description)

(defmethod print-object ((inst object-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (object.name inst) (object.level inst)))
  inst)

(defmethod print-object ((inst active-object) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S (~a,~a)]" (class-name (class-of inst)) 
	   (aobj.kind inst) (location-x inst) (location-y inst))
  inst))

(defun is-eatable? (o-type)
  (or (obj-is? o-type '<food>)
      (obj-is? o-type '<potion>)
      (obj-is? o-type '<mushroom>)))
      
