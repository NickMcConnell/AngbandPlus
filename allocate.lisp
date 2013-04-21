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

(defun place-single-monster! (dun pl mon x y sleeping)
  "places a single monster MON at (X,Y) in dungeon DUN."
  (declare (ignore pl ))

  ;; add checks that it is ok to place something at this spot..
  (let ((kind (amon.kind mon))
	(mstatus (amon.status mon)))
    
    (when (and sleeping (plusp (monster.alertness kind)))
      (let ((alert (monster.alertness kind)))
	(setf (status.sleeping mstatus) (+ (* 2 alert) (* 10 (randint alert))))))
  
  
    (setf (cave-monsters dun x y) (cons mon nil)
	  (location-x mon) x
	  (location-y mon) y)
    (push mon (dungeon.monsters dun))
    
    t))

(defun place-monster-group! (dun pl mon-kind x y sleeping)
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
	    for x from 0 to 7
	    do
	    (let ((poss-x (+ last-x (svref ddx-ddd x)))
		  (poss-y (+ last-y (svref ddy-ddd x))))
	      (when (cave-boldly-naked? dun poss-x poss-y)
		(place-single-monster! dun pl (produce-active-monster var-obj mon-kind)
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


;;(trace get-monster-kind-by-level)
;;(trace print-map swap-monsters!)
