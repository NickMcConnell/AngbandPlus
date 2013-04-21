;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/vanilla/levels.lisp - level customisation for Vanilla
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass van-town-level (themed-level)
    ((id :initform 'town-level)
     (stores        :initarg :stores     :initform nil  :accessor variant.stores)
     (num-stores    :initarg :num-stores :initform 8 :accessor variant.num-stores)
     (home-num      :initarg :home-num   :initform 7 :accessor variant.home-num))

    ))

#||
(defun van-store-build! (dungeon number xx yy
			 &key
			 (y-offset +screen-height+)
			 (x-offset +screen-width+))
			 
  "more or less taken directly"

  (let ((the-house (get-house number)))
    (when the-house
      (build-house! *level* the-house xx yy))))
  
       ;; fake it
;;       (setf (get-coord-trigger dungeon x y) #'(lambda (dun x y)
;;						 (warn "Entering shop ~a" number)))
;;       (setf (cave-feature dungeon x y) (+ +feature-shop-head+ number)))

||#
     


(defmethod level-ready? ((level van-town-level))
  (when (level.dungeon level)
    t))


(defmethod generate-level! ((level van-town-level) player)
  "Generates a town and returns it.  If the dungeon
argument is non-NIL it will be re-used and returned as
part of the new level."

  (let* ((*level* level)
	 (var-obj *variant*)
	 (settings (get-setting :random-level)) ;; hack
	 (max-dungeon-width  (slot-value settings 'max-width))
	 (max-dungeon-height (slot-value settings 'max-height))
	 (dungeon (create-dungeon max-dungeon-width
				  max-dungeon-height
				  :its-depth 0))
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
    (let* ((y-offset +screen-height+)
	   (x-offset +screen-width+)
	   (store-num (variant.num-stores level))
	   (store-numbers (shuffle-array! (get-array-with-numbers store-num :fill-pointer t)
					 store-num))
;;	   (stores (loop for x from 0 to (1- store-num) collecting (create-store (1+ x))))
	   )

      ;; move actual stores to level object
;;      (warn "Stores is ~a" stores)

      (setf (level.dungeon level) dungeon)

	    
      (dotimes (y 2)
	(dotimes (x 4)
	  (let* ((house-num (vector-pop store-numbers))
		 (the-house (get-house house-num)))
	    (when the-house
	      (let ((x0 (+ x-offset (* x 14) 12))
		    (y0 (+ y-offset (* y 9) 6)))
		(warn "building ~s [~a]" the-house house-num)
		(build-house! level the-house x0 y0
			      :door-feature (1- (+ +feature-shop-head+ house-num))
			      :door-trigger  #'(lambda (dun x y)
						 (warn "Entering shop ~a" (1+ house-num))))
		)))
	  )))

    

    (loop named place-the-player
	  for x of-type u-fixnum = (with-type u-fixnum (+ qx (rand-range 3 (- +screen-width+ 4))))
	  for y of-type u-fixnum = (with-type u-fixnum (+ qy (rand-range 3 (- +screen-height+ 4))))
	  do
	  (when (cave-boldly-naked? dungeon x y)
	    (setf (cave-feature dungeon x y) +feature-more+)
	    (place-player! dungeon player x y)
	    (return-from place-the-player nil)))

    
;;    (setf (level.dungeon level) dungeon)

    level))


(defvar *van-saved-town-seed* nil)

(defun van-make-town-level-obj (player)
  "A sucky function which should be simplified greatly."

  (flet ((do-generation (seed)
	   (let* ((town (make-instance 'van-town-level :depth 0))
		  (cl:*random-state* (cl:make-random-state seed)))

	     (generate-level! town player))))
  
    ;; we already have a saved value
    (cond (*van-saved-town-seed*
	   (do-generation *van-saved-town-seed*))
	 
	  ;; this is the first time
	  (t
	   (let* ((new-state (cl:make-random-state t)))
	     (setf *van-saved-town-seed* new-state)
	     (do-generation new-state)))
	  )))


(defmethod create-appropriate-level ((variant vanilla-variant) old-level player depth)

  (declare (ignore old-level))
  (let ((level (if (= depth 0)
		   (van-make-town-level-obj player)
		   (make-random-level-obj depth))))

    (unless (level-ready? level)
      ;; (warn "Generating level ~a" level)
      (generate-level! level player))


    (assert (level-ready? level))
    
    level))


(defmethod print-depth ((level van-town-level) setting)
  "prints current depth somewhere"
  (declare (ignore setting))
  (c-prt "Town" *last-console-line* 70)) ;;fix 


(defmethod activate-object :after ((level van-town-level) &key)
  
  (let* ((dungeon (level.dungeon level))
	 (player *player*)
	 (var-obj *variant*)
	 (turn (variant.turn var-obj))
	 (mod-time (mod turn (variant.day-length var-obj)))
	 ;; quick hack
	 (time-of-day (if (< mod-time (variant.twilight var-obj))
			  'day
			  'night)))
	 

    ;; hackish
    (let ((resident-num (if (eq time-of-day 'day) 4 8)))
	
      ;; add some inhabitants
      (dotimes (i resident-num)
	(allocate-monster! dungeon player 3 t)))

    (van-town-illuminate! dungeon player time-of-day)
    ;;(warn "post activate")
    ))


(defun van-town-illuminate! (dungeon player time-of-day)
  "Illuminates the town according to the time-of-day."
  (declare (ignore player))
  
    (with-dungeon (dungeon (coord x y))
      (declare (ignore x y))
      (let ((feat (coord.feature coord)))
	;; slightly interesting grids
	(cond ((> feat +feature-invisible+)
	       (bit-flag-add! (coord.flags coord) #.(logior +cave-glow+ +cave-mark+)))
	      ;; day-time
	      ((eq time-of-day 'day)
	       (bit-flag-add! (coord.flags coord) +cave-glow+))
	      ;; at night
	      (t
	       (bit-flag-remove! (coord.flags coord) +cave-glow+))
	      ))
      ;; skip doorways yet
      )

    (bit-flag-add! *update* +forget-view+ +update-view+)
    (bit-flag-add! *redraw* +print-map+)


  )
	 

(defmethod get-otype-table ((level random-level) var-obj)
  (%get-var-table var-obj 'level 'objects))

(defmethod get-otype-table ((level van-town-level) var-obj)
  (%get-var-table var-obj 'level 'objects))

(defmethod get-mtype-table ((level random-level) var-obj)
  (%get-var-table var-obj level 'monsters))

(defmethod get-mtype-table ((level van-town-level) var-obj)
  (%get-var-table var-obj level 'monsters))

(defmethod get-mtype-table ((level (eql 'random-level)) var-obj)
  (%get-var-table var-obj level 'monsters))

(defmethod get-mtype-table ((level (eql 'town-level)) var-obj)
  (%get-var-table var-obj level 'monsters))
