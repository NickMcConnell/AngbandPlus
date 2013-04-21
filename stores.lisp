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
  (defclass store ()
    ((id :accessor store.id :initform nil)
     (name :accessor store.name :initform nil)
     (owner :accessor store.owner :initform nil)
     (number :accessor store.number :initform nil)
     (items :accessor store.items :initform nil)
     ))

  (defclass store-owner ()
    ((id :accessor owner.id :initform nil)
     (name :accessor owner.name :initform nil)
     (purse :accessor owner.purse :initform nil)
     (max-greed :accessor owner.max-greed :initform nil)
     (min-greed :accessor owner.min-greed :initform nil)
     (haggle-num :accessor owner.haggle-num :initform nil)
     (tolerance :accessor owner.tolerance :initform nil)
     (race :accessor owner.race :initform nil)

     )))


(defmethod print-object ((inst store) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (class-name (class-of inst))
	   (slot-value inst 'name)
	   (slot-value inst 'owner)))
	   
  inst)

(defmethod print-object ((inst store-owner) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A]" (class-name (class-of inst))
	   (slot-value inst 'name)))
	   
  inst)


;; array with the active stores, indexed by number
(defvar *active-stores* (make-array +max-stores+))

;; array with store id's, indexed by number
(defvar *store-ids* (make-array +max-stores+))

;; the different store-types indexed by id
(defvar *store-types* (make-hash-table :test #'eql))

;; the different owners indexed by id
(defvar *owners* (make-hash-table :test #'eql))

;; a mapping from store id to a list of possible owners
(defvar *store-owners* (make-hash-table :test #'eql))



(defun store-init& (num)

;;  (warn "init on ~a" num)
  
  ;; let's find an owner for this joint
  (let* ((store-id (aref *store-ids* num))
	 (possible-owners (get-store-owners store-id))
	 (chosen-owner (nth (random (length possible-owners)) possible-owners))
	 (the-store (get-store-type store-id)))

    (setf (store.owner the-store) chosen-owner)
    (setf (aref *active-stores* num) the-store)
    
    the-store))

(defun store-delete-obj (num )
  (let ((the-store (aref *active-stores* num)))
    (pop (store.items the-store)))
  (values))

(defun store-create-obj (num)

  (let ((some-obj (get-obj-by-level (+ 25 (randint 25))))
	(the-store (aref *active-stores* num)))

    (when some-obj
	(push some-obj (store.items the-store))))
  
  (values))

(defun store-display-inventory (num)
  (let ((the-store (aref *active-stores* num)))

    (loop for i from 0
	  for item in (store.items the-store)
	  do
	  (store-display-entry num i item))))

(defun store-display-entry (num item-num item)
  (declare (ignore num))
  
  (let ((row (+ item-num 6))
	(o-type (aobj.kind item)))

	(c-prt (format nil "~c) " (code-char (+ (char-code #\a) item-num))) row 0)

	(c-prt (format nil "~a " (object-description item :store t)) row 5)
	
	;; weight
	(let ((weight (object.weight o-type)))
	  (c-prt (format nil "~d.~d" (int-/ weight 10) (mod weight 10))
		 row 61))

	))
	

(defun store-maint! (num)

;;  (warn "maint on ~a" num)
;;  (warn "Active ~a" *active-stores*)
  (let ((the-store (aref *active-stores* num))
	;; fix level-rating to use new system
	;;(*level-rating* 0)
	)
    ;; we need to sell some stuff
    (let* ((len (length (store.items the-store)))
	   (j len))
      ;; silly calc
      (setq j (- j (randint +store-turnover+)))
      (if (> j +store-maximum-items+)
	  (setq j +store-maximum-items+)
	  (if (< j +store-minimum-items+)
	      (setq j +store-minimum-items+)))
      
      ;; wipe objects
;;      (warn "Comparing ~a with ~a" (length (store.items the-store)) j)
      
      (while (> (length (store.items the-store)) j)
	(store-delete-obj num))

      (setq j (length (store.items the-store)))
      
      (setq j (- j (randint +store-turnover+)))
      (if (> j +store-maximum-items+)
	  (setq j +store-maximum-items+)
	  (if (< j +store-minimum-items+)
	      (setq j +store-minimum-items+)))

      (store-create-obj num)
      )))


(defun establish-store-type& (store)
  "adds a store to the table of possible stores"
  (setf (gethash (store.id store) *store-types*) store))

(defun get-store-type (id)
  "gets a store from the table of possible stores"
  (gethash id *store-types*))

(defun establish-owner& (owner)
  "adds an owner to the table of possible owners"
  (setf (gethash (owner.id owner) *owners*) owner))

(defun get-owner (id)
  "gets an owner from the table of possible owners"
  (gethash id *owners*))

(defun establish-store-owner-rel& (store-id owner)
  "adds a relation from a store-type to an owner"
  (push owner (gethash store-id *store-owners*)))

(defun get-store-owners (id)
  "returns a list of possible owners for given store-id/store-type"
  (gethash id *store-owners*))


(defun define-store (id &key name number)
  "creates a store object and adds it to the appropriate table"
;;  (declare (ignore args))
  (let ((store (make-instance 'store)))
    (setf (store.id store) id
	  (store.name store) name
	  (store.number store) number)

    (establish-store-type& store)
    ;; add to numbered position
    (setf (aref *store-ids* (1- (store.number store))) (store.id store))
    
    store))

(defun define-store-owner (&key store-type id name purse max-greed
			   min-greed haggle-num tolerance race
			   special-owner)
  "creates an owner and adds him or her to appropriate tables"
  
  (let ((owner (make-instance 'store-owner)))
    (setf (owner.id owner) id
	  (owner.name owner) name
	  (owner.purse owner) purse
	  (owner.max-greed owner) max-greed
	  (owner.min-greed owner) min-greed
	  (owner.haggle-num owner) haggle-num
	  (owner.tolerance owner) tolerance
	  (owner.race owner) race)

    ;; we add it to the owner-table
    (establish-owner& owner)
    
    ;; we just want generic owners to the relation table
    (unless special-owner
      (establish-store-owner-rel& store-type owner))

    owner))


(defun store-build! (dungeon number xx yy)
  "more or less taken directly"
  
   (let* ((qy +screen-height+)
	  (qx +screen-width+)
	  (y0 (+ qy (* yy 9) 6))
	  (x0 (+ qx (* xx 14) 12))
	  (y1 (- y0 (randint (if (= 0 yy) 3 2))))
	  (y2 (+ y0 (randint (if (= 1 yy) 3 2))))
	  (x1 (- x0 (randint 5)))
	  (x2 (+ x0 (randint 5))))

     (loop for y from y1 to y2 do
	   (loop for x from x1 to x2 do
		 (setf (cave-feature dungeon x y) +feature-perm-extra+)))


     ;; add doors
     (let ((tmp (random 4))
	   (x 0)
	   (y 0))

       ;; skip relocating annoying doors
       
       (case tmp
	 ;; bottom
	 (0 (setq y y2
		  x (rand-range x1 x2)))
	 ;; top
	 (1 (setq y y1
		  x (rand-range x1 x2)))
	 ;; right
	 (2 (setq y (rand-range y1 y2)
		  x x2))
	 ;; left
	 (3 (setq y (rand-range y1 y2)
		  x x1))
	 
	 (t
	  (warn "Fall-through in door placement")
	  (setq y y2
		x x2)))

       ;; fake it
       (setf (cave-feature dungeon x y) (+ +feature-shop-head+ number)))
       
     
     ))
	  

(defun initialise-stores& ()
  (dotimes (i +max-stores+)

    (unless (eql i +the-home+)
      (store-init& i)
      ;; maint it ten times
      (dotimes (j 10) (store-maint! i)))

;;    (warn "Store: ~a" (aref *active-stores* i))
    ))

;;(trace store-maint)

(defun show-store (num)
  "Shows the store number num."
  (clear-the-screen)
  (store-display-inventory num)
  (c-pause-line *last-console-line*)
  (clear-the-screen))

