;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: stores.lisp - code which deals with stores and their owners
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass store (house)
    ((id     :accessor store.id     :initform nil :initarg :id)
     (name   :accessor store.name   :initform nil :initarg :name)
     (number :accessor store.number :initform nil :initarg :number)
     
     (poss-owners :accessor store.poss-owners
		  :initform nil ;;(make-hash-table :test #'eq)
		  :initarg :poss-owners)
     
     ;; unsure on this
     (items  :accessor store.items  :initform nil :initarg :items)
     ))

 
  (defclass store-owner (owner)
    ((purse      :accessor owner.purse      :initform nil :initarg :purse)
     (max-greed  :accessor owner.max-greed  :initform nil :initarg :max-greed)
     (min-greed  :accessor owner.min-greed  :initform nil :initarg :min-greed)
     (haggle-num :accessor owner.haggle-num :initform nil :initarg :haggle-num)
     (tolerance  :accessor owner.tolerance  :initform nil :initarg :tolerance)
     (race       :accessor owner.race       :initform nil :initarg :race)
     ))

  
  )

(defmethod find-owner-for-house (level (house store)
				       &key
				       (var-obj *variant*)
				       (selection :random))
  
  (declare (ignore level))
  
  (let ((poss-owners (store.poss-owners house))
	(the-owner nil))
    
    (unless poss-owners
      (warn "Unable to find any possible owners for store ~a" house)
      (return-from find-owner-for-house nil))

    (ecase selection
      (:random
       (setf the-owner (elt poss-owners (random (length poss-owners)))))
      )
    
    (assert the-owner)

    (get-owner the-owner var-obj)
    ))


(defun add-owner-to-store-type! (owner store-type-id
				 &optional (var-obj *variant*))
  "The OWNER argument should be an owner, the STORE-TYPE-ID
should be an exisiting id."
  
  (let* ((v-obj var-obj)
	 (store-type (get-house store-type-id v-obj)))
    
    (if (and store-type (typep store-type 'store))
	(pushnew (owner.id owner) (store.poss-owners store-type))
	(warn "Unable to find store-type ~a" store-type-id))))
	 

(defun define-store (id &key name number x-attr x-char)
  "creates a store object and adds it to the appropriate table"
;;  (declare (ignore args))
  (let ((var-obj *variant*)
	(store (make-instance 'store :id id :name name :number number
			      :x-attr x-attr :x-char x-char)))

    (establish-house& var-obj store)

    (when (and number (numberp number))
      ;; add to numbered position
      (establish-house& var-obj store :house-key number))
    
    store))

(defun define-store-owner (&key store-type id name purse max-greed
			   min-greed haggle-num tolerance race
			   special-owner)
  "creates an owner and adds him or her to appropriate tables"
  
  (let ((owner (make-instance 'store-owner :id id :name name
			      :purse purse :max-greed max-greed
			      :min-greed min-greed :haggle-num haggle-num
			      :tolerance tolerance :race race))
	(var-obj *variant*))


    ;; we add it to the owner-table
    (establish-owner& var-obj owner)
    
    ;; we just want generic owners to the relation table
    (unless special-owner
      (add-owner-to-store-type! owner store-type var-obj))

    owner))



(defun show-store (num)
  "Shows the store number num."
  (with-new-screen ()
   
    ;;  (clear-the-screen)
    
;;    (store-display-inventory num)
    (c-pause-line *last-console-line*)
    ;;(clear-the-screen)
    ))


(defun create-store (type
		     &key
		     (owner :random)
		     (var-obj *variant*)
		     (level *level*))
  "The TYPE argument can be either an id (preferred)
or a number.  Returns NIL on failure."

  (unless (or (typep type 'number)
	      (typep type 'symbol))
    (warn "Store type should be a number or symbol, was given ~s [~a]."
	  type (type-of type))
    (return-from create-store nil))
  
  (let* ((wanted-house (get-house type var-obj)))
    
    (when (or (eq nil wanted-house)
	      (not (typep wanted-house 'store)))
      (warn "Unable to find store ~a" type)
      (return-from create-store nil))
    

    (let ((its-owner (if (eq owner :random)
			 (find-owner-for-house level wanted-house :var-obj var-obj :selection :random)
			 (get-owner owner var-obj))))
      
      (unless its-owner
	(warn "Unable to find owner ~a for ~a" owner wanted-house)
	(return-from create-store nil))

      (activate-object wanted-house :owner its-owner)
      (activate-object its-owner)
      
      wanted-house)))
    
#||
(defun establish-store-type& (var-obj store)
  "adds a store to the table of possible stores.
Is added by both id and number."
  (warn "establish-store-type& deprecated")
  (establish-house& var-obj store))

(defun get-store-type (id &optional var-obj)
  "gets a store from the table of possible stores"
  (warn "get-store-type& deprecated")
  (get-house id var-obj))


  (let ((table (variant.house-types var-obj)))
    (setf (gethash (house.id store) table) store)
    (setf (gethash (store.number store) table) store)
    ))

  (let* ((v-obj (if var-obj var-obj *variant*))
	 (table (variant.house-types v-obj)))
    
    (gethash id table)))


(defun establish-owner&% (var-obj owner)
  "adds an owner to the table of possible owners"
  (let ((table (variant.house-owners var-obj)))
    (setf (gethash (owner.id owner) table) owner)))

(defun get-owner% (id &optional var-obj)
  "gets an owner from the table of possible owners"
  (let* ((v-obj (if var-obj var-obj *variant*))
	 (table (variant.house-owners v-obj)))

    (gethash id table)))

  (let* ((v-obj (if var-obj var-obj *variant*))
	 (table (variant.store-types v-obj)))
    (loop for x being the hash-values of table
	  do
	  (when (= (store.number x) number)
	    (return-from get-store-type-by-num x)))
    nil))



(defun get-store-type-by-num (number &optional var-obj)
  "Returns a store-type matching the given number. O(n)"
  (get-store-type number var-obj))

  

(defun get-owner-by-store (store-type &optional (var-obj *variant*))
  "Returns an owner based on store-type."

  (let* ((v-obj var-obj) 
	 (s-type (etypecase store-type
		   (store store-type)
		   (symbol (get-house store-type v-obj)))))
    
    (unless (and s-type (typep s-type 'store))
      (warn "Unable to find store ~a" store-type)
      (return-from get-owner-by-store nil))
    
    (let ((poss-owners (store.poss-owners s-type)))
      
      (unless poss-owners
	(warn "Unable to find any possible owners for store ~a" store-type)
	(return-from get-owner-by-store nil))

      (let ((the-owner (elt poss-owners (random (length poss-owners)))))
	(assert the-owner)

	(get-owner the-owner v-obj)))))


||#
