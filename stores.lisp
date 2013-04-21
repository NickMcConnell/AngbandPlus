;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: stores.lisp - code which deals with stores and their owners
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;;; Current implementation ignores haggling, selling-season, buying-season, etc

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
	 
(defun %make-priorities (variant store-id sale-list)
  "Returns a list of sale-priorities, given a sale-spec."
  (let ((ret-list '()))
    (dolist (i sale-list)
      (cond ((and (consp i) (eq (car i) 'obj))
	     (destructuring-bind (dummy &key id type (weight 1))
		 i
	       (declare (ignore dummy))
	       (cond (id
		      (let ((k (get-object-kind variant id)))
			(if k
			    (dotimes (j weight)
			      (push k ret-list))
			    (warn "Unable to find kind for obj-id ~s" id))))
		     (type
		      (warn "Type-spec for store-objs not implemented."))
		     (t
		      (warn "Neither type or id is mentioned for obj-spec ~s for store ~s" i store-id)))
	       ))
	    (t
	     (warn "Unknown format for spec ~s for store ~s" i store-id))))
    
    ret-list))


(defun define-store (id &key (type 'store) name number
		     (sells nil) 
		     x-attr x-char (owner :random) (no-items nil))
  "creates a store object and adds it to the appropriate table"
;;  (declare (ignore args))
  (let ((var-obj *variant*)
	(store (make-instance type :id id :name name :number number
			      :x-attr x-attr :x-char x-char :owner owner)))

    
    (when (and (eq type 'store) sells)
      (setf (store.sells store) sells))
    
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

    ;; skip storekeeper, charisma, ...
    (if (and (numberp default-price) (>= default-price 0))
	(floor (* 1.5 default-price) 1)
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
	   #-cmu
	   (c-prt! "Odd return-value!" 0 0)
	   nil))))
		 

(defun %store-buy-item (player level store)
  (declare (ignore level))
  (block buying
    (let* ((var-obj *variant*)
	   (items (store.items store))
	   (item-len (items.cur-size items)))
  
      (put-coloured-str! +term-white+
			 (format nil "(Items ~a-~a, ESC to exit) Which item are you interested in?"
				 (i2a 0) (i2a (1- item-len)))
			 0 0)
      (let ((selected (%store-select-item 0 (1- item-len))))
	(when (and selected (numberp selected))
	  (let* ((retval nil)
		 (act-obj (item-table-find items selected))
		 (the-price (get-price act-obj store))
		 (backpack (aobj.contains (player.inventory player))))
	    ;;(warn "Buying ~s for ~s" act-obj the-price)
	    (unless (< the-price (player.gold player))
	      (c-prt! "You cannot afford that item!" 0 0)
	      (return-from buying nil))

	    (unless (item-table-more-room? backpack)
	      (c-prt! "No room in backpack!" 0 0)
	      (return-from buying nil))

	    (cond ((= 1 (aobj.number act-obj))
		   (possible-identify! player act-obj) ;; fix?
		   (item-table-add! backpack act-obj)
		   (item-table-remove! items act-obj)
		   (setf retval act-obj))
		  
		  ((> (aobj.number act-obj) 1)
		   (let ((new-obj (copy-active-object var-obj act-obj)))
		     (decf (aobj.number act-obj))
		     (setf (aobj.number new-obj) 1)
		     (item-table-add! backpack new-obj)
		     (possible-identify! player new-obj)
		     (setf retval new-obj))
		   ))

	    ;; add identify for it
	    (decf (player.gold player) the-price)
	    (bit-flag-add! *redraw* +print-gold+)
	    
	    
	    retval)
	  )))))

;;(trace %store-buy-item)

(defun %store-sell-item (player level store)
  
  (let ((dungeon (level.dungeon level)))
    (block selling

      (when-bind (selection (select-item dungeon player '(:backpack :equip)
					 :prompt "Sell which item?"
					 :where :backpack))
      

	(let* ((the-table (get-item-table dungeon player (car selection)))
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
      
      nil)))

(defun %store-display (player store)

  (let ((store-name (store.name store))
	(store-limit 50000)
	(the-owner (house.owner store))
	(owner-name "Bob")
	(owner-race "Human"))

    (when (and the-owner (typep the-owner 'store-owner))
      (setf owner-name (owner.name the-owner))
      (let ((the-race (owner.race the-owner)))
	(when (and the-race (typep the-race 'character-race))
	  (setf owner-race (race.name the-race))))
      (let ((poss-limit (owner.purse the-owner)))
	(when (and poss-limit (plusp poss-limit))
	  (setf store-limit poss-limit))))

    (c-clear-from! 0) ;; hack
    
    (with-foreign-str (s)
      (lb-format s "~a (~a)" owner-name owner-race)
      (put-coloured-str! +term-white+ s 10 3))

    (with-foreign-str (s)
      (lb-format s "~a (~a)" store-name store-limit)
      (put-coloured-str! +term-white+ s 50 3))

    
    (put-coloured-str! +term-white+ "Item Description" 3 5)
    (put-coloured-str! +term-white+ "Weight" 60 5)
    (put-coloured-str! +term-white+ "Price" 72 5)

    (put-coloured-str! +term-white+ "Gold Remaining:" 53 19)
    
    (put-coloured-str! +term-white+
		       (format nil "~9d" (player.gold player))
		       68 19)

    
    (item-table-print (house.items store) :store store)

    (put-coloured-str! +term-white+ " ESC) Exit from building." 0 22)
    (put-coloured-str! +term-white+ " g) Get/purchase item." 31 22)
    (put-coloured-str! +term-white+ " d) Drop/sell item." 31 23)

    (put-coloured-str! +term-white+ "You may: " 0 21)

   
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
    
    ;;(pause-last-line!)
    ))



;; hackish  create/delete/maint
(defmethod store-generate-object ((variant variant) (the-store store))
  "this is just for a regular store, not a black market"

  (when-bind (sells (store.sells the-store))
    (when-bind (kind (rand-elm sells))
      (when (typep kind 'object-kind)
	(let ((aobj (create-aobj-from-kind kind :variant variant)))
	  ;; possibly add magic
	  (apply-magic! variant aobj (store.object-depth the-store) :allow-artifact nil)
	  (store-mass-produce! variant the-store aobj)
	  (return-from store-generate-object aobj)))))

  (warn "Fell through in obj-generation for store ~s" the-store)
    
  nil)


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
  (let ((var-obj *variant*)
	(sells (store.sells obj)))
    ;; late-init basically
    (when sells
      (setf (store.sells obj) (%make-priorities var-obj (store.id obj) sells)))
 										 
    (dotimes (j 10) (store-maintenance! var-obj obj)))
  
  obj)


(defmethod store-maintenance! ((variant variant) (the-store store))
  "hackish, fix later."
  (when-bind (new-obj (store-generate-object variant the-store))
    (item-table-add! (store.items the-store) new-obj)))


(defmethod item-table-print ((table items-in-store)
			     &key
			     show-pause
			     start-x start-y
			     print-selection
			     (store t))

  (declare (ignore print-selection))
  (let ((x (if start-x start-x 0))
	(y (if start-y start-y 6))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (declare (ignore a-table key))
	     (let ((attr (get-colour val))
		   (price (get-price val store))
		   (desc (with-output-to-string (s)
			   (write-obj-description *variant* val s :store t))))
	       (c-prt! "" (- x 2) (+ i y))
	       (put-coloured-str! +term-white+ (format nil "~a) " (i2a i)) x (+ i y))
	       
	       (put-coloured-str! attr desc (+ x 4) (+ i y))
	       
	       (let* ((weight (object.weight (aobj.kind val)))
		      (full-pounds (int-/ weight 10))
		      (fractions (mod weight 10)))
		 (c-prt! (format nil "~3d.~d lb~a" full-pounds fractions
				 (if (> full-pounds 1)
				     "s"
				     ""))
			 61 (+ i y)))
	       
	       (put-coloured-str! +term-white+ (format nil "~9d " price)
			       70 (+ i y))

	       (incf i))))
      
      
      (item-table-iterate! table #'iterator-fun)
    
      (when show-pause
	(pause-last-line!))

      )))
