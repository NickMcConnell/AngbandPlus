;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: building.lisp - code which deals with buildings
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defmethod visit-house (level (house house))
  (declare (ignore level))
  ;;(warn "simple house..")
  )
  
(defmethod find-owner-for-house (level house &key var-obj selection)
  (declare (ignore level house var-obj selection))
  nil)

(defun define-door-type (id name &key x-char x-attr text-char text-attr
			 numeric-id effect min-depth max-depth rarity
			 cave-flags-on cave-flags-off)
  "Defines and registers a door-type."
  
  (declare (ignore numeric-id effect min-depth max-depth rarity))
  
  (unless (verify-id id)
    (warn "door-id ~s not valid" id)
    (return-from define-door-type nil))
  
  (let ((trap-obj (make-instance 'door-type :id id :name name
				  ;;:min-depth min-depth :max-depth max-depth
				  ;;:rarity rarity
				  ))
	(table (variant.doors *variant*)))

    (when (integerp cave-flags-on)
      (setf (slot-value trap-obj 'cave-flags-on) cave-flags-on))
    (when (integerp cave-flags-off)
      (setf (slot-value trap-obj 'cave-flags-off) cave-flags-off))

    (handle-gfx-visual trap-obj x-attr x-char)
    (handle-text-visual trap-obj text-attr text-char)

    ;; removed effects
    (setf (gethash id table) trap-obj)

    trap-obj))

(defmethod x-attr ((obj active-door))
  (x-attr (decor.type obj)))

(defmethod x-char ((obj active-door))
  (x-char (decor.type obj)))

(defmethod gfx-sym ((obj active-door))
  (gfx-sym (decor.type obj)))

(defmethod text-attr ((obj active-door))
  (text-attr (decor.type obj)))

(defmethod text-char ((obj active-door))
  (text-char (decor.type obj)))

(defmethod text-sym ((obj active-door))
  (text-sym (decor.type obj)))

(defmethod get-decor-name ((decor active-door))
  (door.name (decor.type decor)))


(defun is-door? (obj)
  (typep obj 'active-door))

(defmethod get-door (variant key &key (visible t))
  (let ((door-type (gethash key (variant.doors variant))))
    (when door-type
      (make-instance 'active-door :type door-type :visible? visible))))

(defun get-door-type (key &optional (variant *variant*))
  (gethash key (variant.doors variant)))

(defmethod place-door! ((variant variant) dungeon x y (door active-door))
  (let ((type (decor.type door)))
    (setf (cave-decor dungeon x y) door)
    (bit-flag-add! (cave-flags dungeon x y) (slot-value type 'cave-flags-on))
    (bit-flag-remove! (cave-flags dungeon x y) (slot-value type 'cave-flags-off))
    (pushnew door (dungeon.decor dungeon))) ;; this one might trip badly
  door)

(defmethod activate-object ((obj house) &key (owner nil) (var-obj *variant*) (level *level*))
  "Wakes a house(-type) from slumber and returns a usable house."

  (when (and owner (typep owner 'owner))
    (setf (house.owner obj) owner))

  (when (eq (house.owner obj) :random)
    (let ((new-owner (find-owner-for-house level obj
					   :var-obj var-obj :selection :random)))
      (when (acceptable-owner? obj new-owner)
	(setf (house.owner obj) new-owner))))

  (cond ((acceptable-owner? obj)
	 (return-from activate-object obj))
	(t
	 (warn "House ~a does not have an owner." obj)
	 nil)))

(defun acceptable-owner? (house &optional (owner nil))
  "Returns t or nil."
  (let ((the-owner (if owner owner (house.owner house))))
    (when (and the-owner (or (typep the-owner 'owner)
			 (eq the-owner :player)))
      t)))

(defun establish-house& (var-obj house &key (house-key nil))
  "Establish a house as available type.  Returns NIL
on failure, and the house on success."

  (when (and var-obj house
	     (is-variant? var-obj)
	     (typep house 'house))
    
    (let ((table (variant.house-types var-obj))
	  (house-id (if house-key
			house-key
			(house.id house))))

      (setf (gethash house-id table) house)
      
      house)))


(defun establish-owner& (var-obj owner)
  "Establish an owner as available. Returns NIL on
failure and owner on success."
  
  (when (and var-obj owner
	     (is-variant? var-obj)
	     (typep owner 'owner))
    
    (let ((table (variant.house-owners var-obj))
	  (owner-id (owner.id owner)))
      
      (setf (gethash owner-id table) owner)
      
      owner)))


(defun get-house (id &optional (var-obj *variant*))
  "Returns a house-type (non-activated) or NIL."

  (when (and var-obj id (is-variant? var-obj))
    
    (let ((table (variant.house-types var-obj)))
      
      (gethash id table))))

(defun get-owner (id &optional (var-obj *variant*))
  "Returns an owner (non-activated) or NIL."
  
  (when (and var-obj id (is-variant? var-obj))
    
    (let ((table (variant.house-owners var-obj)))
      
      (gethash id table))))



(defmethod build-house! (level (house house) topleft-x topleft-y
			       &key
			       (door-feature nil)
			       (door-trigger nil)
			       &allow-other-keys)

  (when level
;;    (warn "building house ~a on level ~a at [~a,~a]" house level topleft-x topleft-y)

    (let* ((dungeon (level.dungeon level))
	   (*dungeon* dungeon)
	   (variant *variant*)
	   (y0 topleft-y)
	   (x0 topleft-x)
	   (y1 (- y0 (randint 3)))
	   (y2 (+ y0 (randint 3)))
	   (x1 (- x0 (randint 5)))
	   (x2 (+ x0 (randint 5))))

      (let ((ft (get-floor *variant* "stone-building")))
	(assert (not (eq ft nil)))
      
	(loop for y from y1 to y2 do
	      (loop for x from x1 to x2 do
		    (setf (cave-floor dungeon x y) ft))))
      

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

       
       ;; time to place house number
       
       ;; first check if we got decor
       (cond ((and door-feature (typep door-feature 'active-door))
	      (setf (location-x door-feature) x
		    (location-y door-feature) y)
	      
	      (decor-operation variant door-feature :open :value t)
	      (pushnew door-feature (dungeon.decor dungeon))
	      (setf (cave-decor dungeon x y) door-feature))

	     ;; we got a floor-type
	     ((and door-feature (typep door-feature 'floor-type))
	      (setf (cave-floor dungeon x y) door-feature))
	     ;; we don't know how to do things
	     (t
	      (warn "Built house without a proper door/feature: ~s" door-feature)))
	      
       
       (when door-trigger
	 (setf (get-coord-trigger dungeon x y) door-trigger))
       
       ))

    house))

(defun define-house (id &key name (type 'house) number owner (no-items nil))
  "defines a house and adds it to the appropriate table."
  
  (let ((var-obj *variant*)
	(house (make-instance type :id id :name name :owner owner)))

    ;; hackish
    (unless no-items
      (setf (house.items house) (make-container 24 ;; fix later
						'items-in-house)))
    
    (establish-house& var-obj house)

    (when (and number (numberp number))
      ;; add to numbered position
      (establish-house& var-obj house :house-key number))
    
    house))


(defmethod item-table-print ((table items-in-house)
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
	     (let ((attr (get-text-colour val))
		   (desc (with-output-to-string (s)
			   (write-obj-description *variant* val s :store store))))
	       (put-coloured-line! +term-white+ "" (- x 2) (+ i y))
	       (put-coloured-str! +term-white+ (format nil "~a) " (i2a i)) x (+ i y))
	       
	       (put-coloured-str! attr desc (+ x 4) (+ i y))
	       
	       (let* ((weight (object.weight (aobj.kind val)))
		      (full-pounds (int-/ weight 10))
		      (fractions (mod weight 10)))
		 (put-coloured-line! +term-white+ (format nil "~3d.~d lb~a" full-pounds fractions
				 (if (> full-pounds 1)
				     "s"
				     ""))
			 61 (+ i y)))

	       (when store
		 (let ((price (get-price val store)))
		   (put-coloured-str! +term-white+ (format nil "~9d " price)
				      70 (+ i y))))

	       (incf i))))
      
      
      (item-table-iterate! table #'iterator-fun)
    
      (when show-pause
	(pause-last-line!))

      )))
