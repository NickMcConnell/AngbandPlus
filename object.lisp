;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: object.lisp - code for object-kinds
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code for object-kinds which is basic and should be widely
ADD_DESC: available in the game.

|#

(in-package :langband)


(eval-when (:compile-toplevel :load-toplevel :execute)
 
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
   
     )))

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

    (gethash id (get-okind-table)))



(defun (setf get-obj-kind) (obj id)
  ;;  (warn "Adding ~a" obj)
  "Adds the obj to the obj-kind table identified by given id."
  (warn "do not use me")
  (let ((table (get-okind-table)))
    (setf (gethash id table) obj)) )

(defun add-new-okind! (obj id)
  ""
  (declare (ignore id))
  (apply-filters-on-obj :objects *variant* obj))


(defun define-object-kind (id name &key numeric-id x-attr x-char level rarity
			   chance locale weight cost obj-type sort-value
			   events)
  "creates and establishes an object corresponding to parameters"
  (let ((new-obj (make-instance 'object-kind
				:id id
				:numeric-id (if numeric-id
						numeric-id
						id)
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
				:events events
				)))

    (add-new-okind! new-obj id)
;;    (apply-filters-on-obj :objects *variant* new-obj)
;;    (setf (get-obj-kind id) new-obj)
    
    new-obj))

(defmethod object.obj-type ((obj active-object))
  "Forwards to the right place.."
  (object.obj-type (aobj.kind obj)))

(defmethod dump-object ((obj object-kind) stream (style (eql :lispy)))
  "Dumps the object in a lispy-style which can be evaluated later.
Loses information."
  
  (let ((clause `(define-object-kind
		  ,(object.id obj)
		  ,(object.name obj)
		  :numeric-id ,(object.numeric-id obj)
		  :x-attr ,(get-letter-from-colour-code (object.x-attr obj))
		  :x-char ,(object.x-char obj)
		  :level ,(object.level obj)
		  :rarity ,(object.rarity obj)
		  :chance ,(object.chance obj)
		  :locale ,(object.locale obj)
		  :weight ,(object.weight obj)
		  :sort-value ,(object.sort-value obj)
		  :cost ,(object.cost obj)
		  :obj-type ,(symbolify (object.obj-type obj))
		  )))
		  
    (pprint clause stream)))

(defmethod print-object ((inst object-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (object.name inst) (object.level inst)))
  inst)

(defmethod print-object ((inst active-object) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	   (aobj.kind inst))
  inst))

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
	(make-instance 'active-object :obj the-kind))))


(defun plural-name (number name)
  "Returns a name with plurality fixed as in normal Angband.  FIX ME"
  (let ((plural (> number 1)))
    (with-output-to-string (s)
      (loop for i from 0 to (1- (length name))
	    for x = (schar name i)
	    do
	    (case x
	      (#\~ (when plural
		     (write-char #\s s)))
	      
	      (#\& (if plural
		       (write-string (format nil "~a" number) s)
		       (if (find (schar name (+ i 2)) '(#\a #\e #\i #\o #\u #\y))
			   (write-string "an" s)
			   (write-char #\a s))))
	      
	      
	      (otherwise
	       (write-char x s))))
      )))


(defun object-description (item &key store)
  "this one should be moved out into the variant directories"
  
  (declare (ignore store))


  
  (let* ((o-type (aobj.kind item))
	 (name (object.name o-type))
;;	 (num (aobj.number item))
;;	 (plural (> num 1))
	 (tot-str (plural-name (aobj.number item) name))
	 (o-tlist (object.obj-type o-type))
	 (flavour (object.flavour o-type)))
	 
    ;; temporary hack
    (when flavour (setf flavour (car flavour)))
    
    (dolist (x o-tlist)
      (case x
	('<mushroom> (setf tot-str (concatenate 'string flavour " mushroom of " tot-str)))
	('<ring> (setf tot-str (concatenate 'string flavour " ring of " tot-str)))
	('<potion> (setf tot-str (concatenate 'string flavour " potion of " tot-str)))
	('<staff> (setf tot-str (concatenate 'string flavour " staff of " tot-str)))
	('<wand> (setf tot-str (concatenate 'string flavour " wand of " tot-str)))
	('<rod> (setf tot-str (concatenate 'string flavour " rod of " tot-str)))
	('<rod> (setf tot-str (concatenate 'string flavour " amulet of " tot-str)))
	('<scroll> (setf tot-str (concatenate 'string "scroll [" flavour "] of " tot-str)))
	(otherwise
	 ;;(warn "Object fell through type-list")
	 )))

    tot-str))



(defun %output-kinds-to-file (fname)
  "Dumps the obj-kind table to the given filename."
  
  (with-open-file (s (pathname fname)
		     :direction :output 
		     :if-exists :supersede)
    (let ((*print-case* :downcase)
	  (*print-escape* t)
	  (table (get-okind-table)))

      (loop for v being the hash-values of table
	    do
	    (dump-object v s :lispy)
	    (write-char #\newline s)
	    ))))
