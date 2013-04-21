;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: allocate.lisp - allocation and placing of "things" in dungeon
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defmethod allocate-monster! ((variant variant) (dungeon dungeon) player distance sleeping)
  "distance is a fixnum, sleeping is either NIL or T."

  (let ((x nil)
	(y nil)
	(dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon))
	(px (location-x player))
	(py (location-y player)))

    (loop named search-for-spot
	  do
	  (setq x (random dungeon-width)
		y (random dungeon-height))

	  (when (and (can-place? dungeon x y :creature)
		     (> (distance x y px py) distance))
	    (return-from search-for-spot nil)))
    
  (if (place-monster! variant dungeon player x y sleeping t)
      t
      nil)
  ))

(defun place-single-monster! (dungeon player mon x y sleeping)
  "places a single monster MON at (X,Y) in dungeon DUN."
  (declare (ignore player))

  ;; add checks that it is ok to place something at this spot..
  (let ((kind (amon.kind mon)))
    
    (when (and sleeping (plusp (monster.alertness kind)))
      (let ((alert (monster.alertness kind)))
	(modify-creature-state! mon '<sleeping>
				:new-value  (+ (* 2 alert) (* 10 (randint alert))))))
  
  
    (setf (cave-monsters dungeon x y) (cons mon nil)
	  (location-x mon) x
	  (location-y mon) y)
    (push mon (dungeon.monsters dungeon))


    (incf (get-creature-energy mon) (random 10))
    (lb-ds:pq-insert mon (get-creature-energy mon) (dungeon.action-queue dungeon))
    
    t))

(defun place-monster-group! (dungeon player mon-kind x y sleeping)
  "Tries to scatter a bunch of monsters in the dungeon of the given kind."

  (let ((number (randint (dungeon.depth dungeon))) ;; hack
	(var-obj *variant*)
	(last-x x)
	(last-y y)
	(ddx-ddd *ddx-ddd*)
	(ddy-ddd *ddy-ddd*))
	
    
    (check-type mon-kind monster-kind)

    ;; ugly hack
    (when (> number 20)
      (setf number 20))
    
;;    (warn "Trying to place group (~a) of ~a monsters at (~a,~a)."
;;	  (monster.id mon-kind) number x y)
    
    (dotimes (i number)
      (loop named placement
	    for x from 0 below +normal-direction-number+
	    do
	    (let ((poss-x (+ last-x (svref ddx-ddd x)))
		  (poss-y (+ last-y (svref ddy-ddd x))))
	      (when (can-place? dungeon poss-x poss-y :creature)
		(place-single-monster! dungeon player (produce-active-monster var-obj mon-kind)
				       poss-x poss-y sleeping)
		(setf last-x poss-x
		      last-y poss-y)
		(return-from placement t)))))
	      
	      
		  
	    
    t))

(defmethod place-monster! ((variant variant) (dungeon dungeon) player x y sleeping group)
  "Tries to place a monster at given coordinates.."
  
  (let* ((var-obj *variant*)
	 (level *level*)
	 (possible-monster (get-active-monster-by-level var-obj level
							:depth (dungeon.depth dungeon))))
    (when possible-monster
      (place-single-monster! dungeon player possible-monster x y sleeping))
    
    (when (and group possible-monster)
      (when (appears-in-group? variant *level* possible-monster)
	(place-monster-group! dungeon player (amon.kind possible-monster) x y sleeping)
;;	(warn "grouped ~s" possible-monster)
	))

    ))



(defmethod get-monster-kind-by-level ((variant variant) (level level) &key depth)
  "Returns a monster-kind for the given level."

  ;; skipping boost
  
  (let ((total 0)
	(depth (if (numberp depth) depth (level.depth level)))
	(counter 0)
	;; fix later
	(table (get-mkind-alloc-table variant level)))

;;    (warn "M-Table[~a,~a] is ~a" *level* level (length table))

    (loop named counting-area
	  for a-obj across table
	  do
;;	  (warn "Checking ~a" a-obj)
	  (when (> (alloc.depth a-obj) depth)
	    (return-from counting-area nil))
	  ;; skip chest-check
	  (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))
	  (incf total (alloc.prob3 a-obj))
	  (incf counter))


    (when (= 0 total)
      (lang-warn "No suitable monsters at depth ~a [~a]" depth level)
      (return-from get-monster-kind-by-level nil))

    (let ((val (random total)))
;;      (warn "Counter is ~a and total is ~a and we got ~a" counter total val)

      ;; loop through objects and find one
      (loop for a-obj across table
	    do
	    (when (< val (alloc.prob3 a-obj))
	      (return-from get-monster-kind-by-level (alloc.obj a-obj)))
	    (decf val (alloc.prob3 a-obj)))
      
      ))
  
  nil)


(defmethod get-active-monster-by-level ((variant variant) (level level) &key depth)
  "Returns an (active) object by level.  Returns NIL on failure."

  (when-bind (the-kind (get-monster-kind-by-level variant level :depth depth))
    (produce-active-monster variant the-kind)))


(defmethod get-object-kind-by-level ((variant variant) (level level) &key depth)
  "Returns an object-kind for the given depth.."

  ;; skipping boost
  
  (let ((total 0)
	(counter 0)
	(depth (if (numberp depth) depth (level.depth level)))
	(table (get-okind-alloc-table variant level)))

;;    (warn "O-Table[~a,~a] is ~a" *level* level (length table))

    (loop named counting-area
	  for a-obj across table
	  do
	  (progn
	    ;;	  (warn "Checking ~a" a-obj)
	    (when (> (alloc.depth a-obj) depth)
	      (return-from counting-area nil))
	    ;; skip chest-check
	    (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))
	    (incf total (alloc.prob3 a-obj))
	    (incf counter)))


    (when (= 0 total)
      (warn "No suitable objects at depth ~a [~a]" depth level)
      (return-from get-object-kind-by-level nil))

    (let ((val (random total)))
;;      (warn "Counter is ~a and total is ~a and we got ~a" counter total val)

      ;; loop through objects and find one
      (loop for a-obj across table
	    do
	    (when (< val (alloc.prob3 a-obj))
	      (return-from get-object-kind-by-level (alloc.obj a-obj)))
	    (decf val (alloc.prob3 a-obj)))
      
      ))
  
  nil)

(defmethod get-active-object-by-level ((variant variant) (level level) &key depth (amount 1))
  "Returns an (active) object by level."
  (when-bind (the-kind (get-object-kind-by-level variant level :depth depth))
    (create-aobj-from-kind the-kind :variant variant :amount amount)))

(defun create-basic-allocation-table (variant obj-table)
  "Creates and returns a basic allocation table for the given objects in a table."
  
  ;; first we should scan the obj-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((org-size (variant.max-depth variant))
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
    (loop for k-obj across obj-table
	  do
	  (dolist (j (slot-value k-obj 'locations))
	    (unless (= 0 (cdr j))
	      (incf alloc-sz)
	      (incf (aref level-org (car j)))
	      )))

    ;; then we sum up in level-org (init2.c)
    (loop for lvl-num from 1 below org-size
	  do
	  (setf (aref level-org lvl-num) (+ (aref level-org lvl-num)
					    (aref level-org (1- lvl-num)))))

    
    (let ((table (make-array alloc-sz))
	  (counter 0))
      (loop for k-obj across obj-table
	    for k-idx from 0
	    do
	    (dolist (j (slot-value k-obj 'locations))
	      (let ((chance (cdr j)))
		(unless (= 0 chance)
		  (let* ((p (int-/ 100 chance))
			 (alloc-obj (make-alloc-entry :index k-idx
						      :obj k-obj
						      :depth (car j)
						      :prob1 p
						      :prob2 p
						      :prob3 p)))
			
		    
		    (setf (svref table counter) alloc-obj)
		    (incf counter))))))


      (setq table (sort table #'< :key #'alloc.depth))
      
      table)))
  

(defmethod create-alloc-table-objects ((variant variant) obj-table)
  "Creates an allocation table for objects and returns it."
  (create-basic-allocation-table variant obj-table))

(defmethod create-alloc-table-monsters ((variant variant) mon-table)
  "Creates an allocation table for monsters and returns it."
  (create-basic-allocation-table variant mon-table))
