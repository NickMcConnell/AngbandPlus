;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: stores.lisp - code which deals with stores and their owners
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defclass store (house)
  ((id     :accessor store.id     :initform nil :initarg :id)
   (name   :accessor store.name   :initform nil :initarg :name)
   (number :accessor store.number :initform nil :initarg :number)
     
   (poss-owners :accessor store.poss-owners
		:initform nil;;(make-hash-table :test #'eq)
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


;;; Current implementation ignores haggling, selling-season, buying-season, etc

(defgeneric store-generate-object (the-store)
  (:documentation "Returns an object appropriate for the store, no side-effects."))

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
	 

(defun define-store (id &key (type 'store) name number x-attr x-char (owner :random) (no-items nil))
  "creates a store object and adds it to the appropriate table"
;;  (declare (ignore args))
  (let ((var-obj *variant*)
	(store (make-instance type :id id :name name :number number
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
  
  (let* ((race-obj (if (and race (or (symbolp race)
				     (stringp race)))
		       (get-char-race race)
		       race))
	 (owner (make-instance 'store-owner :id id :name name
			      :purse purse :max-greed max-greed
			      :min-greed min-greed :haggle-num haggle-num
			      :tolerance tolerance :race race-obj))
	 (var-obj *variant*))


    ;; we add it to the owner-table
    (establish-owner& var-obj owner)
    
    ;; we just want generic owners to the relation table
    (unless special-owner
      (add-owner-to-store-type! owner store-type var-obj))

    owner))

(defmethod get-price ((object active-object) (store store))
  (let* ((okind (aobj.kind object))
	 (default-price (object.cost okind)))

    (if (and (numberp default-price) (>= default-price 0))
	(floor (* 1.1 default-price) 1)
	0)))


(defmethod get-offer ((object active-object) (store store))
  (int-/ (get-price object store) 2)) ;; decent value, eh?
   
(defun %store-select-item (low top)
  (let ((the-char (read-one-character)))
    (cond ((eql the-char #\Escape)
	   nil)
	  ((characterp the-char)
	   (let ((char-num (a2i the-char)))
	     (cond ((and (<= low char-num)
			 (>= top char-num))
		    char-num)
		   (t
		    (c-prt! "Illegal selection!" 0 0)
		    nil))))
	  (t
	   (c-prt! "Odd return-value!" 0 0)
	   nil))))
		 

(defun %store-buy-item (player level store)
  (declare (ignore level))
  (block buying
    (let* ((items (store.items store))
	   (item-len (items.cur-size items)))
  
      (c-col-put-str! +term-white+
		      (format nil "(Items ~a-~a, ESC to exit) Which item are you interested in?"
			      (i2a 0) (i2a (1- item-len)))
		      0 0)
      (let ((selected (%store-select-item 0 (1- item-len))))
	(when (and selected (numberp selected))
	  (let* ((act-obj (item-table-find items selected))
		 (the-price (get-price act-obj store))
		 (backpack (aobj.contains (player.inventory player))))
	    ;;(warn "Buying ~s for ~s" act-obj the-price)
	    (unless (< the-price (player.gold player))
	      (c-prt! "You cannot afford that item!" 0 0)
	      (return-from buying nil))

	    (unless (item-table-more-room? backpack)
	      (c-prt! "No room in backpack!" 0 0)
	      (return-from buying nil))

	    (item-table-add! backpack act-obj)
	    (item-table-remove! items act-obj)
	    ;; add identify for it
	    (decf (player.gold player) the-price)
	    
	    act-obj)
	  )))))

;;(trace %store-buy-item)

(defun %store-sell-item (player level store)
  (declare (ignore level))
  (block selling

    (when-bind (selection (select-item nil player '(:backpack :equip)
				       :prompt "Sell which item?"
				       :where :backpack))
      

      (let* ((the-table (get-item-table nil player (car selection)))
	     (removed-obj (item-table-remove! the-table (cdr selection) :only-single-items t)))

	(when removed-obj

	  (let ((price (get-offer removed-obj store))
		(shop-items (store.items store)))

	    (cond ((and (plusp price)
			(item-table-more-room? shop-items))
		   
		   (item-table-add! shop-items removed-obj)
		   ;;	       (item-table-remove! the-table removed-obj)
		   (incf (player.gold player) price)
		   (return-from %store-sell-item t))
		  ;; something screwed up, put it back.
		  (t 
		   (item-table-add! the-table removed-obj)))
	    ))
	))
      
    nil))

(defun %store-display (player store)

  (let ((store-name (store.name store))
	(store-limit 50000)
	(the-owner (house.owner store))
	(owner-name "Bob")
	(owner-race "Human"))

    (when (and the-owner (typep the-owner 'store-owner))
      (setf owner-name (owner.name the-owner))
      (let ((the-race (owner.race the-owner)))
	(when (and the-race (typep the-race 'race))
	  (setf owner-race (race.name the-race))))
      (let ((poss-limit (owner.purse the-owner)))
	(when (and poss-limit (plusp poss-limit))
	  (setf store-limit poss-limit))))

    (c-clear-from! 0) ;; hack
    
    (with-foreign-str (s)
      (lb-format s "~a (~a)" owner-name owner-race)
      (c-col-put-str! +term-white+ s 3 10))

    (with-foreign-str (s)
      (lb-format s "~a (~a)" store-name store-limit)
      (c-col-put-str! +term-white+ s 3 50))

    
    (c-col-put-str! +term-white+ "Item Description" 5 3)
    (c-col-put-str! +term-white+ "Weight" 5 60)
    (c-col-put-str! +term-white+ "Price" 5 72)

    (c-col-put-str! +term-white+ "Gold Remaining:" 19 53)
    
    (c-col-put-str! +term-white+
		    (format nil "~9d" (player.gold player))
		    19 68)

    
    (item-table-print (house.items store) :store store)

    (c-col-put-str! +term-white+ " ESC) Exit from building." 22 0)
    (c-col-put-str! +term-white+ " g) Get/purchase item." 22 31)
    (c-col-put-str! +term-white+ " d) Drop/sell item." 23 31)

    (c-col-put-str! +term-white+ "You may: " 21 0)

   
    t))

(defun %shop-input-loop (player level store)
  (loop
   (c-term-gotoxy! 10 21)       
   (let ((val (read-one-character)))
     (cond ((eql val #\g)
	    (let ((retval (%store-buy-item player level store)))
	      (when retval
		(%store-display player store))))
	    ((eql val #\d)
	     (when-bind (retval (%store-sell-item player level store))
	       (%store-display player store)))
	    ((or (eql val #\Escape)
		 (eql val #\Q))
	     (return-from %shop-input-loop t))
	    (t
	     (warn "Unknown key read: ~s" val)))
     
     (c-prt! "" 0 0)
     )))


(defmethod visit-house (level (house store))
  "Visits the given store."

  (unless (activated? house)
    (activate-object house))
  
  (with-new-screen ()
    (clear-the-screen!)
    (%store-display *player* house)
    (%shop-input-loop *player* level house)
    
    
;;    (c-pause-line! *last-console-line*)
    ))
    

;; hackish  create/delete/maint
(defmethod store-generate-object ((the-store store))
  "fix me.. works only as black market."
  ;; this is just a black market, fix for a regular store!
  (let* ((some-obj (get-obj-by-level (+ 25 (randint 25))))
	 (o-type (when some-obj (aobj.kind some-obj))))

    (when (and some-obj (plusp (get-price some-obj the-store))
	       (not (obj-is? o-type '<chest>))) ;; hack
      some-obj)))


(defun store-delete-obj! (the-store &optional obj-key)
  "just wipes an object.."
  (let ((store-items (store.items the-store)))
    (item-table-remove! store-items (if obj-key
					obj-key
					(random (items.cur-size store-items)))
			)))
  
(defmethod activate-object ((obj store) &key)
  
  (let ((res-obj (call-next-method)))
    (unless (eq res-obj obj)
      (warn "Something fu with store-obj ~a" res-obj)
      (return-from activate-object res-obj)))

  ;; hackish
  (dotimes (j 10) (store-maint! obj))
  
  obj)


(defun store-maint! (the-store)
  "hackish, fix later."
  (when-bind (new-obj (store-generate-object the-store))
    (item-table-add! (store.items the-store) new-obj)))


(defmethod item-table-print ((table items-in-store)
			     &key
			     show-pause
			     start-x start-y
			     (store t))
  
  (let ((x (if start-x start-x 0))
	(y (if start-y start-y 6))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table key))
	     (let ((attr (get-attribute val))
		   (price (get-price val store))
		   (desc (with-output-to-string (s)
			   (write-obj-description val s :store t))))
	       (c-prt! "" (+ i y) (- x 2))
	       (c-col-put-str! +term-white+ (format nil "~a) " (i2a i)) (+ i y) x)
	       
	       (c-col-put-str! attr desc (+ i y) (+ x 4))
	       
	       (let* ((weight (object.weight (aobj.kind val)))
		      (full-pounds (int-/ weight 10))
		      (fractions (mod weight 10)))
		 (c-prt! (format nil "~3d.~d lb~a" full-pounds fractions
				 (if (> full-pounds 1)
				     "s"
				     ""))
			 (+ i y) 61))
	       
	       (c-col-put-str! +term-white+ (format nil "~9d " price)
			       (+ i y) 70)

	       (incf i))))
      
;;      (describe table)
      
      (item-table-iterate! table #'iterator-fun)
    
      (when show-pause
	(c-pause-line! *last-console-line*))
      )))

;;(trace get-price)
