;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: allocate.lisp - allocation and placing of "things" in dungeon
Copyright (c) 2000-2002 - Stig Erik Sandø

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

	  (when (and (cave-boldly-naked? dungeon x y)
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
    
    t))

(defun place-monster-group! (dungeon player mon-kind x y sleeping)
  "Tries to scatter a bunch of monsters in the dungeon of the given kind."

  (let ((number (randint 13)) ;; hack
	(var-obj *variant*)
	(last-x x)
	(last-y y)
	(ddx-ddd *ddx-ddd*)
	(ddy-ddd *ddy-ddd*))
	
    
    (check-type mon-kind monster-kind)

;;    (warn "Trying to place group (~a) of ~a monsters at (~a,~a)."
;;	  (monster.id mon-kind) number x y)
    
    (dotimes (i number)
      (loop named placement
	    for x from 0 below +normal-direction-number+
	    do
	    (let ((poss-x (+ last-x (svref ddx-ddd x)))
		  (poss-y (+ last-y (svref ddy-ddd x))))
	      (when (cave-boldly-naked? dungeon poss-x poss-y)
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
;;	  (warn "Checking ~a" a-obj)
	  (when (> (alloc.depth a-obj) depth)
	    (return-from counting-area nil))
	  ;; skip chest-check
	  (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))
	  (incf total (alloc.prob3 a-obj))
	  (incf counter))


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


(defmethod create-alloc-table-objects ((variant variant) obj-table)
  "Creates an allocation table for objects and returns it."
  
  ;; first we should scan the obj-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((var-obj *variant*)
	 (org-size (variant.max-depth var-obj))
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
;;    (warn "Scanning..")

    #+langband-debug
    (with-open-file (s (pathname (concatenate 'string *dumps-directory* "bar.txt"))
		       :direction :output 
		       :if-exists :supersede)
      (loop for i across obj-table
	    do (print i s)))
    
    (loop for k-obj across obj-table
	  do
	  (dotimes (j 4)
	    (unless (= 0 (svref (object.chance k-obj) j))
	      (incf alloc-sz)
	      (incf (aref level-org (svref (object.locale k-obj) j)))
	      )))

;;    (warn "Summing..")
    ;; then we sum up in level-org (init2.c)
    (loop for lvl-num from 1 below org-size
	  do
	  (setf (aref level-org lvl-num) (+ (aref level-org lvl-num)
					    (aref level-org (1- lvl-num)))))

    #||
    (loop for v across level-org
	  for i from 0
	  do
	  (format t "~&~a: ~a~%" i v))
    ||#

;;    (warn "doing table ~a ~a ~a" alloc-sz (svref obj-table 0) (svref obj-table 1))
    
    (let ((table (make-array alloc-sz))
	  (counter 0))
      (loop for k-obj across obj-table
	    for k-idx from 0
	    do
	    (dotimes (j 4)
	      (let ((chance (svref (object.chance k-obj) j)))
		(unless (= 0 chance)
		  (let* ((p (int-/ 100 chance))
			 (alloc-obj (make-alloc-entry :index k-idx
						      :obj k-obj
						      :depth (svref (object.locale k-obj) j)
						      :prob1 p
						      :prob2 p
						      :prob3 p)))
			
		    
		    (setf (svref table counter) alloc-obj)
		    (incf counter))))))


      (setq table (sort table #'< :key #'alloc.depth))

      #+langband-debug
      (dump-alloc-table table (concatenate 'string *dumps-directory* "foo.txt"))
      
      table)))


(defmethod create-alloc-table-monsters ((variant variant) mon-table)
  "Creates an allocation table for monsters and returns it."
  
  ;; first we should scan the mon-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((org-size (variant.max-depth variant))
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
    (loop for k-obj across mon-table
	  do
	  (when (< 0 (monster.rarity k-obj))
	    (incf alloc-sz)
	    (incf (aref level-org (monster.depth k-obj)))
	    ))
      
    ;; then we sum up in level-org (init2.c)
    (loop for lvl-num from 1 below org-size
	  do
	  (setf (aref level-org lvl-num) (+ (aref level-org lvl-num)
					    (aref level-org (1- lvl-num)))))

   
    (let ((table (make-array alloc-sz))
	  (counter 0))

;;      (warn "going second table.")
      
      (loop for k-obj across mon-table
	    for k-idx from 0
	    do
	    ;;(warn "i")
	    (when (< 0 (monster.rarity k-obj))
	      
	      (let* ((x (monster.depth k-obj))
		     (p (int-/ 100 (monster.rarity k-obj)))
		     (alloc-obj (make-alloc-entry :index k-idx
						  :obj k-obj
						  :depth x
						  :prob1 p
						  :prob2 p
						  :prob3 p)))
;;		(warn "k")
		(setf (svref table counter) alloc-obj)
		(incf counter))))

	    

      (setq table (sort table #'< :key #'alloc.depth))
    

      #+langband-debug
      (dump-alloc-table table (concatenate 'string *dumps-directory* "formosa.txt"))
      
      table)))
