;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/vanilla/quirks.lisp - special settings for Vanilla
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass van-town-level (level)
    ()))

(defmethod level-ready? ((level van-town-level))
  (when (level.dungeon level)
    t))


(defmethod generate-level! ((level van-town-level) player dun)
  "Generates a town and returns it.  If the dungeon
argument is non-NIL it will be re-used and returned as
part of the new level."

  (let* ((settings (get-setting :random-level)) ;; hack
	 (max-dungeon-width  (slot-value settings 'max-width))
	 (max-dungeon-height (slot-value settings 'max-height))
	 (dungeon (if dun dun (create-dungeon max-dungeon-width
					      max-dungeon-height
					      :its-depth 0)))
	 (qy +screen-height+)
	 (qx +screen-width+))

    (declare (type u-fixnum qy qx))

;;    (warn "Generating town with random state equal to ~s" *random-state*)
    
;;    (setf (player.map player) (make-map dungeon))
    
    (fill-dungeon-with-feature! dungeon +feature-perm-solid+)
    
    (fill-dungeon-part-with-feature! dungeon +feature-floor+
				     (cons (1+ +screen-width+)  (+ +screen-width+ qx -1))
				     (cons (1+ +screen-height+) (+ +screen-height+ qy -1)))

    ;; we need stores
    (let ((room-numbers (stable-sort (loop for x of-type u-fixnum from 0 to (1- +max-stores+)
					   collecting x)
				     #'(lambda (x y)
					 (declare (ignore x y))
					 (let ((val (random 100)))
					   (oddp val))))))
      (dotimes (y 2)
	(dotimes (x 4)
	  (store-build! dungeon (pop room-numbers) x y))))



    (loop named place-the-player
	  for x of-type u-fixnum = (with-type u-fixnum (+ qx (rand-range 3 (- +screen-width+ 4))))
	  for y of-type u-fixnum = (with-type u-fixnum (+ qy (rand-range 3 (- +screen-height+ 4))))
	  do
	  (when (cave-boldly-naked? dungeon x y)
	    (setf (cave-feature dungeon x y) +feature-more+)
	    (place-player! dungeon player x y)
	    (return-from place-the-player nil)))

    
    ;; add some inhabitants
    (dotimes (i 3)
      (allocate-monster! dungeon player 3 t))

    (setf (level.dungeon level) dungeon)

    level))


(defvar *van-saved-town-seed* nil)

(defun van-make-town-level-obj (old-level player)
  "A sucky function which should be simplified greatly."
  
  ;; we already have a saved value
  (cond (*van-saved-town-seed*
	 (let* ((town (make-instance 'van-town-level :depth 0))
		(cl:*random-state* (cl:make-random-state *van-saved-town-seed*)))
	
;;	   (warn "reusing state ~a" *van-saved-town-seed*)
	   (generate-level! town player
			    (if old-level (level.dungeon old-level) nil))))
	;; this is the first time
	(t
	 (let* ((town (make-instance 'van-town-level :depth 0))
		(new-state (cl:make-random-state t)) 
		(cl:*random-state* (cl:make-random-state new-state)))

;;	   (warn "Assigning state ~a" new-state)
	   (setf *van-saved-town-seed* new-state)
	   (generate-level! town player
			    (if old-level (level.dungeon old-level) nil))
	   ))))

(defmethod create-appropriate-level ((variant vanilla-variant) old-level player depth)

  (let ((level (if (= depth 0)
		   (van-make-town-level-obj old-level player)
		   (make-random-level-obj depth))))

    (unless (level-ready? level)
      (warn "Generating level ~a" level)
      (generate-level! level player
		       (if old-level (level.dungeon old-level) nil)))

    (assert (level-ready? level))
    
    level))


(defmethod print-depth ((level van-town-level) setting)
  "prints current depth somewhere"
  (declare (ignore setting))
  (c-prt "Town" *last-console-line* 70)) ;;fix 



(defun van-town-illuminate! (dungeon player time-of-day)
  "Illuminates the town according to the time-of-day."

  (declare (ignore dungeon player time-of-day))
  
  (warn "illuminate town.")

  )
	 

