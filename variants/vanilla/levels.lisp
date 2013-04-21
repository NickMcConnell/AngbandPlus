;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/levels.lisp - level customisation for Vanilla
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(defun van-create-bare-town-level-obj ()
  "Returns a bare town-level."
  (make-instance 'van-town-level :depth 0 :rating 0))


(defmethod level-ready? ((level van-town-level))
  (when (level.dungeon level)
    t))


(defun %van-visit-shop (dungeon x y house-num)
  "Visits shop.."
  (declare (ignore dungeon x y))
;;  (warn "triggered..")
  (let ((house (get-house house-num)))
    (visit-house *level* house)))

(defmethod display-house ((player player) (home players-home) &key (offset 0))

  (declare (ignore offset))
  (let ((store-name "Your home")
	(left-col 1)
	(desc-line 3)
	(last-line (get-last-console-line)))

    (c-clear-from! 0) ;; hack

    (with-foreign-str (s)
      (lb-format s "~a" store-name)
      (put-coloured-str! +term-yellow+ s left-col 1))
      
      (put-coloured-str! +term-white+ "Item Description" 3 desc-line)
      (put-coloured-str! +term-white+ "Weight" 60 desc-line)
;;      (put-coloured-str! +term-white+ "Price" 72 line)
      
      ;; should have max here too
      
      (item-table-print (house.items home) :store nil :start-y (+ 2 desc-line))

      ;; make these relative to last line
      (put-coloured-str! +term-yellow+ "ESC" 1 last-line)
      (put-coloured-str! +term-white+ ") Exit from building." 4 last-line)
      (put-coloured-str! +term-yellow+ "g" 31 last-line)
      (put-coloured-str! +term-white+ ") Get item." 32 last-line)
      (put-coloured-str! +term-yellow+ "d" 51 last-line)
      (put-coloured-str! +term-white+ ") Drop item." 52 last-line)
      
      (put-coloured-str! +term-l-red+ "You may: " 0 (- last-line 1))
   
      t))

(defun %home-input-loop (player level home)
  (let ((dungeon (level.dungeon level)))
    
  (block input-loop
    (flet ((drop-item ()
	     (when-bind (selection (select-item dungeon player '(:backpack :equip)
						:prompt "Drop which item? "
						:where :backpack))
	       
	       (let* ((the-table (get-item-table dungeon player (car selection)))
		      (removed-obj (item-table-remove! the-table (cdr selection) :only-single-items t)))
		 (when removed-obj
		   ;; check if full!
		   (item-table-add! (house.items home) removed-obj)
		   t))))
	   
	   (get-item ()
	     (let ((item-len (items.cur-size (house.items home))))
	       (put-coloured-str! +term-white+
				  (format nil "(Items ~a-~a, ESC to exit) Which item are you interested in?"
					  (i2a 0) (i2a (1- item-len)))
				  0 0)
	       (let* ((selected (%store-select-item 0 (1- item-len))))
		 (when (and selected (numberp selected))
		   (let* ((items (house.items home))
			  (act-obj (item-table-find items selected))
			  (backpack (aobj.contains (player.inventory player))))
		   
		   (cond ((= 1 (aobj.number act-obj))
			  
			  (item-table-add! backpack act-obj)
			  (item-table-remove! items act-obj)
			  t)
			 
			 ((> (aobj.number act-obj) 1)
			  (let ((new-obj (copy-active-object *variant* act-obj)))
			    (decf (aobj.number act-obj))
			    (setf (aobj.number new-obj) 1)
			    (item-table-add! backpack new-obj)
			    t))
			 )))
		 ))))

      (loop
       (c-term-gotoxy! 10 21)       
       (let ((val (read-one-character)))
	 (cond ((or (eql val #\g)
		    (eql val #\p))
		(when (get-item)
		  (display-house player home)))
	       
	       ((or (eql val #\d)
		    (eql val #\s))
		(when (drop-item)
		  (display-house player home)))
	       
	       ((or (eql val #\Escape)
		    (eql val #\Q))
		
	      (return-from input-loop t))
	       
	       (t
		(warn "Unknown key read: ~s" val)))
	 
	 ;;     (c-prt! "" 0 0)
	 )))
    )))
 

(defmethod visit-house (level (house players-home))
  "Visit the player home."

  (unless (activated? house)
    (activate-object house))
  
  (let ((player *player*))
    (with-new-screen ()
      (clear-the-screen!)
      (display-house player house :offset 0)
      (%home-input-loop player level house))
    
    ;;(pause-last-line!)
    ))


(defmethod generate-level! ((variant vanilla-variant) (level van-town-level) player)
  "Generates a town and returns it.  If the dungeon
argument is non-NIL it will be re-used and returned as
part of the new level."

  (let* ((*level* level)
;;	 (var-obj *variant*)
	 (settings (get-setting variant :random-level)) ;; hack
	 (max-dungeon-width  (slot-value settings 'max-width))
	 (max-dungeon-height (slot-value settings 'max-height))
	 (dungeon (create-dungeon max-dungeon-width
				  max-dungeon-height
				  :its-depth 0))
	 (screen-height *screen-height*)
	 (screen-width *screen-width*)
	 (town-height 22)
	 (town-width 66)
	 (qy 0)
	 (qx 0)
;;	 (build-stores nil) ;; for testing
	 (build-stores t)
	 )

    (declare (type u-fixnum qy qx))

;;    (warn "Generating town with random state equal to ~s" *random-state*)
    
;;    (setf (player.map player) (make-map dungeon))
    
    (fill-dungeon-with-floor! dungeon +floor-perm-solid+)
    
    (fill-dungeon-part-with-floor! dungeon +floor-regular+
				   (cons 1 (1- town-width))
				   (cons 1 (1- town-height)))

    (setf (level.dungeon level) dungeon)
    
    (when build-stores
      ;; we need stores
      (let* ((y-offset 1)
	     (x-offset 1)
	     (store-num (level.num-stores level))
	     (store-numbers (shuffle-array! (get-array-with-numbers store-num :fill-pointer t)
					    store-num))
	     ;;	   (stores (loop for x from 0 to (1- store-num) collecting (create-store (1+ x))))
	     )

	;; move actual stores to level object
	;;      (warn "Stores is ~a" store-numbers)

	    
	(dotimes (y 2)
	  (dotimes (x 4)
	    (let* ((house-num (1+ (vector-pop store-numbers)))
		   (the-house (get-house house-num)))
	      (when the-house
		(let ((x0 (+ x-offset (* x 14) 12))
		      (y0 (+ y-offset (* y 9) 6)))
		  ;;(warn "building ~s [~a]" the-house house-num)
		  (build-house! level the-house x0 y0
				:door-feature (1- (+ +floor-shop-head+ house-num))
				:door-trigger (make-coord-event (format nil "house-event-~d" house-num)
								#'%van-visit-shop house-num))
		  ;;	(warn "Entering shop ~a, ~a" house-num the-house)))
		  )))
	    ))))

    

    (loop named place-the-player
	  for x of-type u-fixnum = (with-type u-fixnum (+ qx (rand-range 3 (- screen-width 4))))
	  for y of-type u-fixnum = (with-type u-fixnum (+ qy (rand-range 3 (- screen-height 4))))
	  do
	  (when (cave-boldly-naked? dungeon x y)
	    (setf (cave-floor dungeon x y) +floor-more+)
	    (place-player! dungeon player x y)
	    (return-from place-the-player nil)))

    
;;    (setf (level.dungeon level) dungeon)

    level))


(defun van-make-town-level-obj (variant player)
  "A sucky function which should be simplified greatly."

  (flet ((do-generation (seed)
	   (let* ((builder (get-level-builder "town-level"))
		  (town (funcall builder))
		  (cl:*random-state* (cl:make-random-state seed)))

	     (generate-level! variant town player))))
  
    ;; we already have a saved value
    (cond ((variant.town-seed variant)
	   (do-generation (variant.town-seed variant)))
	 
	  ;; this is the first time
	  (t
	   (let* ((new-state (cl:make-random-state t)))
	     (setf (variant.town-seed variant) new-state)
	     (do-generation new-state)))
	  )))


(defmethod create-appropriate-level ((variant vanilla-variant) old-level player depth)

  (declare (ignore old-level))
  (let ((level nil))

    (cond ((= depth 0)
	   (setf level (van-make-town-level-obj variant player)))
	  (t ;; the rest
	   (let ((builder (get-level-builder "random-level")))
	     (unless builder
	       (error "Can't find random-level builder"))
	     (setf level (funcall builder)))))
    
    ;; we set the depth now.
    (setf (level.depth level) depth)

    (unless (level-ready? level)
      ;; (warn "Generating level ~a" level)
      (generate-level! variant level player))


    (assert (level-ready? level))
    
    level))



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
	(allocate-monster! var-obj dungeon player 3 t)))

    (van-town-illuminate! dungeon player time-of-day)
    ;;(warn "post activate")
    ))


(defun van-town-illuminate! (dungeon player time-of-day)
  "Illuminates the town according to the time-of-day."
  (declare (ignore player))
  
    (with-dungeon (dungeon (coord x y))
      (declare (ignore x y))
      (let ((feat (coord.floor coord)))
	;; slightly interesting grids
	(cond ((> feat +floor-invisible-trap+)
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

    (bit-flag-add! *update* +pl-upd-forget-view+ +pl-upd-update-view+)
    (bit-flag-add! *redraw* +print-map+)


  )
	 

;; this one does the job for all.. only one table for objects in vanilla
(defmethod get-otype-table ((var-obj vanilla-variant) level)
  (declare (ignore level))
  (%get-var-table var-obj "level" 'objects-by-level))

(defmethod get-mtype-table ((var-obj vanilla-variant) (level random-level))
  (%get-var-table var-obj level 'monsters-by-level))

(defmethod get-mtype-table ((var-obj vanilla-variant) (level van-town-level))
  (%get-var-table var-obj level 'monsters-by-level))

;; when we pass a string
(defmethod get-mtype-table ((var-obj vanilla-variant) (level string))
  (%get-var-table var-obj level 'monsters-by-level))
