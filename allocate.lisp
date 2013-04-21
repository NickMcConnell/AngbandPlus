;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: allocate.lisp - allocation and placing of "things" in dungeon
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun allocate-monster! (dungeon player distance sleeping)
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
    
  (if (place-monster! dungeon player x y sleeping t)
      t
      nil)
  ))

(defun place-single-monster! (dun pl mon x y sleeping)
  "places a single monster MON at (X,Y) in dungeon DUN."
  (declare (ignore pl sleeping))

  ;; add checks that it is ok to place something at this spot..
  
  (setf (cave-monsters dun x y) mon
	(location-x mon) x
	(location-y mon) y)
  (push mon (dungeon.monsters dun))
  t)

(defun place-monster! (dungeon player x y sleeping group)
  "Tries to place a monster at given coordinates.."

  (declare (ignore group))
  
  (let ((possible-monster (get-monster-by-level (dungeon.depth dungeon))))
    (when possible-monster
      (place-single-monster! dungeon player possible-monster x y sleeping)))
  
  )

(defun swap-monsters! (dun pl from-x from-y to-x to-y)
  "swaps two monsters or move one"

  (let ((mon-1 (cave-monsters dun from-x from-y))
	(mon-2 (cave-monsters dun to-x to-y)))

    (setf (cave-monsters dun from-x from-y) mon-2
	  (cave-monsters dun to-x to-y) mon-1)

    
    ;; hack, move to swap later
    (when (and (= from-x (location-x pl))
	       (= from-y (location-y pl)))
      (setf (location-x pl) to-x
	    (location-y pl) to-y)
      ;; add more stuff here
;;      (warn "move pl (~s ~s) -> (~s ~s)" from-x from-y to-x to-y)
      (bit-flag-add! *update* +update-view+)
      (bit-flag-add! *redraw* +print-map+)
      ) ;; hack, fix later


    (when mon-1
      (dolist (i mon-1)
	(setf (location-x i) to-x
	      (location-y i) to-y)))


    (when mon-2
      (dolist (i mon-2)
	(setf (location-x i) to-x
	      (location-y i) to-y)))

    
    (light-spot! dun from-x from-y)
    (light-spot! dun to-x to-y)))



(defun get-monster-kind-by-level (level)
  "Returns a monster-kind for the given level."

  ;; skipping boost
  
  (let ((total 0)
	(counter 0)
	;; fix later
	(table (%get-mkind-alloc-tbl)))

;;    (warn "M-Table[~a,~a] is ~a" *level* level (length table))

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
      (lang-warn "No suitable monsters at level ~a [~a]" level *level*)
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

;;(trace get-monster-kind-by-level)


(defun get-monster-by-level (level)
  "Returns an (active) object by level.  Returns NIL on failure."
  (let ((the-kind (get-monster-kind-by-level level)))
    (when the-kind
      (create-active-monster the-kind))))

;;(trace print-map swap-monsters!)
