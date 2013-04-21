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
     (items      :accessor store.items      :initform nil :initarg :items)
     (turnover   :accessor store.turnover   :initform 9   :initarg :turnover)
     (min-items  :accessor store.min-items  :initform 6   :initarg :min-items)
     (max-items  :accessor store.max-items  :initform 18  :initarg :max-items)
     (item-limit :accessor store.item-limit :initform 24 :initarg :item-limit)
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
    
    (assert (not (eq the-owner nil)))

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
	 

(defun define-store (id &key name number x-attr x-char (owner :random) (no-items nil))
  "creates a store object and adds it to the appropriate table"
;;  (declare (ignore args))
  (let ((var-obj *variant*)
	(store (make-instance 'store :id id :name name :number number
			      :x-attr x-attr :x-char x-char :owner owner)))

        ;; hackish
    (unless no-items
      (setf (house.items store) (make-container (store.max-items store)
						'items-in-store)))

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


(defmethod visit-house (level (house store))
  "Visits the given store."
  (declare (ignore level))

  (unless (activated? house)
    (activate-object house))
  
  (with-new-screen ()
    (clear-the-screen)
    (item-table-print (house.items house))
    (c-pause-line *last-console-line*)
    ))
    

;; hackish  create/delete/maint
(defun store-create-obj! (the-store)
  "fix me.. works only as black market."
  ;; this is just a black market
  (let ((some-obj (get-obj-by-level (+ 25 (randint 25)))))

    (when some-obj
;;      (warn "pushing ~a" some-obj)
      (item-table-add! (store.items the-store) some-obj)
      some-obj)))


(defun store-delete-obj! (the-store)
  "just wipes an object.."
  (item-table-remove! (store.items the-store) 0))
  
(defmethod activate-object ((obj store) &key)
  
  (let ((res-obj (call-next-method)))
    (unless (eq res-obj obj)
      (warn "Something fu with store-obj ~a" res-obj)
      (return-from activate-object res-obj)))

  ;; hackish
  (dotimes (j 10) (store-maint! obj))
  
  obj)


(defun store-maint! (store)
  "hackish, fix later."
  (store-create-obj! store))


(defmethod item-table-print ((table items-in-store) &key show-pause start-x start-y)
  
  (let ((x (if start-x start-x 0))
	(y (if start-y start-y 6))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table key))
	     (c-prt "" (+ i y) (- x 2))
	     (c-put-str (format nil "~a) ~a" (i2a i)
				(object-description val :store t))
			(+ i y) x)
	     (let* ((weight (object.weight (aobj.kind val)))
		    (full-pounds (int-/ weight 10))
		    (fractions (mod weight 10)))
	       (c-prt (format nil "~3d.~d lb~a" full-pounds fractions (if (> full-pounds 1)
									  "s"
									  ""))
		      (+ i y) 61))

	     (incf i)))
      
;;      (describe table)
      
      (item-table-iterate! table #'iterator-fun)
    
      (when show-pause
	(c-pause-line *last-console-line*))
      )))
  
#||
;; blah blah

  (let* ((max-items (store.max-items store))
	 (min-items (store.min-items store))
	 (turn-over (store.turnover store))
	 (len (length (store.items store)))
	 (j len))
    
    ;; silly calc
    (setq j (- j (randint turn-over)))
    (if (> j max-items)
	(setq j max-items)
	(if (< j min-items)
	    (setq j min-items)))
    
      ;; wipe objects
    ;;      (warn "Comparing ~a with ~a" (length (store.items the-store)) j)
      
    (while (> (length (store.items store)) j)
      (store-delete-obj! store))
    
    (setq j (length (store.items store)))
    
    (setq j (- j (randint turn-over)))
    (if (> j max-items)
	(setq j max-items)
	(if (< j min-items)
	    (setq j min-items)))


||#