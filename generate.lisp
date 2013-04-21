;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DEC: generate.lisp - generation of dungeon levels
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with generation of dungeon levels.

|#

(in-package :langband)

(defun generate-town! (player &optional dun)
  "Generates a town and returns it.  If the optional dungeon
argument is passed it will be used as new dungeon and returned."
  (let ((dungeon (if dun dun (create-dungeon *dungeon-width*
					     *dungeon-height*
					     :its-level 0)))
	(qy +screen-height+)
	(qx +screen-width+))

    (declare (type u-fixnum qy qx))
    
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
            
    ;; an error..
    dungeon))

(defstruct dun-data
  room-centres doors walls tunnels row-rooms col-rooms room-map crowded)


#||
;; dy1 dy2 dx1 dx2 level
(defconstant +room-data+ #1A(
			     ( 0  0  0 0  0)
			     ( 0  0 -1 1  1)   ;; 1 = Simple (33x11)
			     ( 0  0 -1 1  1)   ;; 2 = Overlapping (33x11)
			     ( 0  0 -1 1  3)   ;; 3 = Crossed (33x11)
			     ( 0  0 -1 1  3)   ;; 4 = Large (33x11) 
			     ( 0  0 -1 1  5)   ;; 5 = Monster nest (33x11)
			     ( 0  0 -1 1  5)   ;; 6 = Monster pit (33x11) 
			     ( 0  1 -1 1  5)   ;; 7 = Lesser vault (33x22) 
			     (-1  2 -2 3 10)   ;; 8 = Greater vault (66x44)
			     ))
||#

(defun correct-direction (x1 y1 x2 y2)
  "Returns two values, first the x-dir, then the y-dir"
  (declare (type fixnum x1 x2 y1 y2))
  
  (let ((rdir (if (= y1 y2) 0 (if (< y1 y2) 1 -1)))
	(cdir (if (= x1 x2) 0 (if (< x1 x2) 1 -1))))
    (declare (type fixnum rdir cdir))
    
    (if (and (/= 0 rdir)
	     (/= 0 cdir))
	(if (= 0 (random 1))
	    (values cdir 0)
	    (values 0 rdir))
	(values cdir rdir))))

(defun in-bounds? (dungeon x y)
  "Checks that the coordinate is well within the dungeon"
  (and (< x (dungeon.width dungeon))
       (< y (dungeon.height dungeon))))


(defun in-bounds-fully? (dungeon x y)
  "Checks that the coordinate is well within the dungeon"
  (and (> x 0)
       (> y 0)
       (< x (1- (dungeon.width dungeon)))
       (< y (1- (dungeon.height dungeon)))))

(defun place-closed-door! (dungeon x y)
  "Places a closed door at the given coordinate"
  (let ((val (random 400)))
    ;; normal closed door
    (cond ((< val 300)
	   (setf (cave-feature dungeon x y) +feature-door-head+))
	  ;; locked door
	  ((< val 399)
	   (setf (cave-feature dungeon x y) (+ +feature-door-head+ (randint 7))))
	  ;; stuck door
	  (t
	   (setf (cave-feature dungeon x y) (+ +feature-door-head+ #x08 (random 8)))))))



(defun place-random-door! (dungeon x y)
  "Places a door at the given coordinate"
  (let ((val (random 1000)))
    ;; open door
    (cond ((< val 300)
	   (setf (cave-feature dungeon x y) +feature-open+))
	  ;; broken door
	  ((< val 400)
	   (setf (cave-feature dungeon x y) +feature-broken+))
	  ;; secret door
	  ((< val 600)
	   (setf (cave-feature dungeon x y) +feature-secret+))
	  ;; closed door
	  (t
	   (place-closed-door! dungeon x y)))))


(defun next-to-corridor (dungeon x y)
  "returns a fixnum"
  
  (let ((retval 0))
    
    (dotimes (i 4)
      (let ((tmp-x (+ x (svref +ddx-ddd+ i)))
	    (tmp-y (+ y (svref +ddy-ddd+ i))))
	(declare (type u-fixnum tmp-x tmp-y))
	
	(unless (or (not (cave-floor-bold? dungeon tmp-x tmp-y))
		    (/= (cave-feature dungeon tmp-x tmp-y) +feature-floor+)
		    (cave-is-room? dungeon tmp-x tmp-y))
	  (incf retval))))
    retval))

(defun possible-doorway? (dungeon x y)
  (when (>= (next-to-corridor dungeon x y) 2)
    ;; check vertical
    (when (and (<= +feature-magma+ (cave-feature dungeon x (1- y)))
	       (<= +feature-magma+ (cave-feature dungeon x (1+ y))))
      (return-from possible-doorway? t))
    ;; check horizontal
    (when (and (<= +feature-magma+ (cave-feature dungeon (1- x) y))
	       (<= +feature-magma+ (cave-feature dungeon (1+ x) y)))
      (return-from possible-doorway? t)))
  nil)
    

(defun try-door! (dungeon x y)
  "attempts to place a door"
  (unless (in-bounds? dungeon x y)
    ;; ignore walls or rooms
    (unless (or (>= (cave-feature dungeon x y) +feature-magma+)
		(cave-is-room? dungeon x y))
      ;; chance and allowed
      (when (and (< (random 100) +tunnel-junction+)
		 (possible-doorway? dungeon x y))
	(place-random-door! dungeon x y)))))

(defun new-player-spot! (dungeon player)
  (let ((x 0)
	(y 0)
	(max-x (- (dungeon.width dungeon) 2))
	(max-y (- (dungeon.height dungeon) 2)))
    
    (declare (type u-fixnum x y max-x max-y))
    
    (loop
     (setq x (rand-range 1 max-x)
	   y (rand-range 1 max-y))

     (when (and (cave-boldly-naked? dungeon x y)
		(not (cave-icky? dungeon x y)))
       (place-player! dungeon player x y)
       (return-from new-player-spot! nil)))))

(defun place-rubble! (dungeon x y)
  (setf (cave-feature dungeon x y) +feature-rubble+))

(defun place-trap! (dungeon x y)
  ;; add later
  (values))

(defun let-floor-carry! (dungeon x y obj)

  ;; fix later
;;  (warn "carry ~a" obj)
  (let ((place (cave-objects dungeon x y)))
    (unless place
      (setf (cave-objects dungeon x y) (make-floor-container)))
    
    (item-table-add! (cave-objects dungeon x y) obj))

  (values))

(defun create-gold (dungeon)
  
  (let ((gold-obj (create-aobj-from-kind-num 480)))
    (setf (aobj.number gold-obj) (+ 10 (random 100))) ;; hack
    gold-obj))

(defun create-object (dungeon good-p great-p)

  ;; skip good and great
  (let* ((depth (dungeon.level dungeon))
	 (obj (get-obj-by-level depth)))

    obj))


(defun place-gold! (dungeon x y)
  ;; skip paranoia
  (let ((gold (create-gold dungeon)))
    (when gold
      (let-floor-carry! dungeon x y gold)))
  
  (values))

(defun place-object! (dungeon x y good-p great-p)
 ;; skip paranoia
  (let ((obj (create-object dungeon good-p great-p)))
    (when obj
      (let-floor-carry! dungeon x y obj)))
  
  (values))

(defun next-to-walls (dungeon x y)
  "Returns number of close walls in the four dirs."
  
  (let ((k 0))
    (when (>= (cave-feature dungeon (1+ x) y) +feature-wall-extra+) (incf k))
    (when (>= (cave-feature dungeon (1- x) y) +feature-wall-extra+) (incf k))
    (when (>= (cave-feature dungeon x (1+ y)) +feature-wall-extra+) (incf k))
    (when (>= (cave-feature dungeon x (1- y)) +feature-wall-extra+) (incf k))
    k))

(defun allocate-object! (dungeon set type number)
  "Allocate an object.. blah."

  (let ((dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon))
	(x 0)
	(y 0))
    
    (dotimes (i number)

    ;; find legal spot

      
      (tagbody restart-loop
	 (loop named legal-spot-finder
	       do
	       (setq x (random dungeon-width)
		     y (random dungeon-height))
	       (unless (cave-boldly-naked? dungeon x y)
		 (go restart-loop))
	       (let ((room-p (cave-is-room? dungeon x y)))
		 (when (or (and (eq set 'alloc-set-corr) room-p)
			   (and (eq set 'alloc-set-room) (not room-p)))
		   (go restart-loop)))
	       (return-from legal-spot-finder)))
      
      (case type
	('alloc-type-rubble (place-rubble! dungeon x y))
	('alloc-type-trap   (place-trap! dungeon x y))
	('alloc-type-gold   (place-gold! dungeon x y))
	('alloc-type-object (place-object! dungeon x y nil nil))))
  
    (values)))

(defun allocate-stairs! (dungeon dir how-many walls)
  "Allocates stairs."
  
  (let ((dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon)))

    (dotimes (i how-many)

      (block placed-stair

	(dotimes (j 3000)
	  (let ((y (random dungeon-height))
		(x (random dungeon-width)))

	    (when (and (cave-boldly-naked? dungeon x y)
		       (>= (next-to-walls dungeon x y) walls))
	    
	      (setf (cave-feature dungeon x y) (if (eq dir :up)
						   +feature-less+
						   +feature-more+))
	      (return-from placed-stair))))
      
	(when (> walls 0)
	  (decf walls))
	    
      
	))))

(defun build-tunnel! (dungeon x1 y1 x2 y2)
  "builds a tunnel"
  
  (let ((row1 y1)
	(col1 x1)
	(row2 y2)
	(col2 x2)
	(loop-counter 0)
	(door-flag nil)
	(start-row y1)
	(start-col x1))

    (multiple-value-bind (col-dir row-dir)
	(correct-direction col1 row1 col2 row2)

      (tagbody start-of-loop
;;	 (warn "continue..")
	 (loop named while-loop
	       ;; loop for a long time
	       while (or (/= row1 row2)
			 (/= col1 col2))
	       do
	       
	       (when (> (incf loop-counter) 2000)
		 (return-from while-loop))

	       
	       ;; bends
	       (when (< (random 100) +tunnel-change+)
		 (multiple-value-setq (col-dir row-dir)
		   (correct-direction col1 row1 col2 row2))
		 (when (< (random 100) +tunnel-random+)
		   (multiple-value-setq (col-dir row-dir)
		     (random-direction))))

	       ;; get next location
	       (let ((tmp-col (+ col1 col-dir))
		     (tmp-row (+ row1 row-dir)))
		 (declare (type u-fixnum tmp-col tmp-row))
		 
		 ;; do not leave the dungeon
		 (loop until (in-bounds-fully? dungeon tmp-col tmp-row)
		       do
		       (multiple-value-setq (col-dir row-dir)
			 (correct-direction col1 row1 col2 row2))
		       (when (< (random 100) +tunnel-random+)
			 (multiple-value-setq (col-dir row-dir)
			   (random-direction)))
		       
		       (setq tmp-col (+ col1 col-dir)
			     tmp-row (+ row1 row-dir)))

		 ;; get the feature in question
		 (let ((feature (cave-feature dungeon tmp-col tmp-row)))

		   ;; mae sure we're not at the map-edge, vault-edge or at solid granite
		   (cond ((or (eql feature +feature-perm-solid+)
			      (eql feature +feature-perm-outer+)
			      (eql feature +feature-wall-solid+))
			  (go start-of-loop))

			 ;; pierce normal outer walls
			 ((eql feature +feature-wall-outer+)
			  (let* ((y (+ tmp-row row-dir))
				 (x (+ tmp-col col-dir))
				 (feat (cave-feature dungeon x y)))

			    (when (or (eql feat +feature-perm-solid+)
				      (eql feat +feature-perm-outer+)
				      (eql feat +feature-wall-solid+)
				      (eql feat +feature-wall-outer+))
			      (go start-of-loop))

			    (setq row1 tmp-row
				  col1 tmp-col)

			    (push (cons col1 row1) (dun-data-walls *cur-dun*))

			    ;; make sure walls around piercing is solid
			    (loop for wy of-type u-fixnum from (1- row1) to (1+ row1)
				  do
				  (loop for wx of-type u-fixnum from (1- col1) to (1+ col1)
					do
					(when (eql (cave-feature dungeon wx wy) +feature-wall-outer+)
					  (setf (cave-feature dungeon wx wy) +feature-wall-solid+))))
			    
			    ))

			
			 ;; travel through rooms
			 ((bit-flag-set? (cave-flags dungeon tmp-col tmp-row)
					 +cave-room+)
			  (setq col1 tmp-col
				row1 tmp-row))
			 
			 
			 ;; tunnel through other walls
			 ((>= feature +feature-wall-extra+)
			  ;;(warn "g.")
			  (setq col1 tmp-col
				row1 tmp-row)
			  
			  ;; fix, inefficient
;;			  (when (< (length (dun-data-tunnels *cur-dun*)) +tunnel-max+)

			  (push (cons tmp-col tmp-row) (dun-data-tunnels *cur-dun*))
			  (setq door-flag nil))

			 ;; corridor intersections.. et al
			 (t

			  (setq row1 tmp-row
				col1 tmp-col)

			  (unless door-flag
			    ;; skip max-check
			    (push (cons col1 row1) (dun-data-doors *cur-dun*))
			    (setq door-flag t))

			  ;; end in nowhere
			  (when (>= (random 100) +tunnel-extra+)
			    (setq tmp-row (abs (- row1 start-row))
				  tmp-col (abs (- col1 start-col)))
			    (when (or (> tmp-col 10)
				      (> tmp-row 10))
			      (return-from while-loop)))))

		   ))))

      ;; turn it into corridor
      (dolist (i (dun-data-tunnels *cur-dun*))
;;	(warn "tunnel..~a" i)
	(setf (cave-feature dungeon (car i) (cdr i)) +feature-floor+))
      

      ;; do piercing
      (dolist (i (dun-data-walls *cur-dun*))
	(setf (cave-feature dungeon (car i) (cdr i)) +feature-floor+)
	(when (< (random 100) +tunnel-door+)
	  (place-random-door! dungeon (car i) (cdr i))))
      
      )))
		   
			 
		   

      

(defun random-direction ()
  "Returns two values with a random nsew direction"
  (let ((val (random 4)))
    (values (svref +ddx-ddd+ val)
	    (svref +ddy-ddd+ val))))
	    

(defun generate-room (dungeon x1 y1 x2 y2 light)
  ;; fix me
  (loop for y of-type fixnum from y1 to y2
	do
	(loop for x of-type fixnum from x1 to x2
	      do
	      (bit-flag-add! (cave-flags dungeon x y)
			     +cave-room+
			     (if light +cave-glow+ 0)))))


(defun generate-fill (dungeon x1 y1 x2 y2 feat)
  
  (assert (and (< y1 y2) (< x1 x2)))
  
  (loop for y from y1 to y2
	do
	(loop for x from x1 to x2
	      do
	      ;;(warn "fish [~a,~a] -> ~a" x y feat)
	      (setf (cave-feature dungeon x y) feat))))


(defun generate-draw (dungeon x1 y1 x2 y2 feat)
  "draws a box matching the coordinates"
  
  (loop for y from y1 to y2
	do
	(setf (cave-feature dungeon x1 y) feat)
	(setf (cave-feature dungeon x2 y) feat))

  (loop for x from x1 to x2
	do
	(setf (cave-feature dungeon x y1) feat)
	(setf (cave-feature dungeon x y2) feat))

  )



(defun generate-dungeon! (player &optional dun)
  "Generates a dungeon level and returns it.  If the optional dungeon
argument is passed it will be used as new dungeon and returned."
 
  (let* ((dungeon-height *dungeon-height*)
	 (dungeon-width *dungeon-width*)
	 (dungeon (if dun dun (create-dungeon dungeon-width
					      dungeon-height)))
	 (*cur-dun* (make-dun-data))
	 (qy +screen-height+)
	 (qx +screen-width+))

;;    (setf (player.map player) (make-map dungeon))
    
    ;; start with granite
    (fill-dungeon-with-feature! dungeon +feature-wall-extra+)

    ;; skip destroyed levels
    
    (setf (dun-data-row-rooms *cur-dun*) (int-/ dungeon-height +block-height+)
	  (dun-data-col-rooms *cur-dun*) (int-/ dungeon-width +block-width+)

	  ;; init room-table
	  (dun-data-room-map *cur-dun*) (make-array (list (dun-data-col-rooms *cur-dun*)
							  (dun-data-row-rooms *cur-dun*))
						    :initial-element nil)
	  (dun-data-crowded *cur-dun*) nil)

    ;;    (warn "Roomie ~a" (eql *dungeon* dungeon))
    
    ;; build rooms
    (loop for i of-type u-fixnum from 0 to +dungeon-rooms+;; fix
	  do

	  (let ((by (random (dun-data-row-rooms *cur-dun*)))
		(bx (random (dun-data-col-rooms *cur-dun*))))

	    (when +dungeon-align+
	      ;; slide left
	      (when (= (mod bx 3) 0) (incf bx))
	      ;; slide right
	      (when (= (mod bx 3) 2) (decf bx)))
	      
	    
	    ;; skip destroy
	    ;; skip unusual
	    (build-room! player dungeon bx by 1)
		
	    ;; fill in
	  
	    ))

    ;; perm walls on top and bottom
    (loop for y in (list 0 (1- dungeon-height))
	  for x from 0 to (1- dungeon-width)
	  do
	  (setf (cave-feature dungeon x y) +feature-perm-solid+))
    
    ;; perm walls on left and right
    (loop for x in (list 0 (1- dungeon-width))
	  for y from 0 to (1- dungeon-height)
	  do
	  (setf (cave-feature dungeon x y) +feature-perm-solid+))
    
    
    ;; make list into an array
    (let* ((centre-list (dun-data-room-centres *cur-dun*))
	   (len (length centre-list))
	   (centres (make-array len)))
      
      (loop for i from 0
	    for c in centre-list
	    do
	    (setf (svref centres i) c))

      ;; scramble
      (loop for i from 0 to (1- len)
	    do
	    (let* ((pick1 (random len))
		   (pick2 (random len))
		   (temp (svref centres pick1)))
	      (setf (svref centres pick1) (svref centres pick2))
	      (setf (svref centres pick2) temp)))

      (setf (dun-data-room-centres *cur-dun*) centres)

      (let* ((last (svref centres (1- len)))
	     (x (car last))
	     (y (cdr last)))
	(loop for c across centres
	      for this-x = (car c)
	      for this-y = (cdr c)
	      do
	      (build-tunnel! dun this-x this-y x y)
	      (setf x this-x
		    y this-y)))

      
      )

    ;; place unplaced doors
    (let ((doors (dun-data-doors *cur-dun*)))
      (dolist (i doors)
	(let ((x (car i))
	      (y (cdr i)))
	  (try-door! dungeon (1- x) y)
	  (try-door! dungeon (1+ x) y)
	  (try-door! dungeon x (1- y))
	  (try-door! dungeon x (1+ y)))))
    


    (allocate-stairs! dungeon :down (rand-range 3 4) 3)
    (allocate-stairs! dungeon :up   (rand-range 1 2) 3)
   

    (let ((monster-amount (int-/ (dungeon.level dungeon) 3)))
      (when (> monster-amount 10) (setq monster-amount 10))
      (when (< monster-amount 2)  (setq monster-amount 2))

      (setq monster-amount (+ monster-amount 14 (random 8)))

      (dotimes (i monster-amount)
	(allocate-monster! dungeon player 0 t)))
    
    
    (new-player-spot! dungeon player)


    ;; in rooms
    (allocate-object! dungeon 'alloc-set-room 'alloc-type-object 9)
	
    (allocate-object! dungeon 'alloc-set-both 'alloc-type-gold 4)
    (allocate-object! dungeon 'alloc-set-both 'alloc-type-object 4)
    ;;    (fill-dungeon-part-with-feature dungeon +feature-floor+
    ;;				    (cons (1+ +screen-width+)  (+ +screen-width+ qx -1))
    ;;				    (cons (1+ +screen-height+) (+ +screen-height+ qy -1)))

    dungeon))


(defun generate-cave! (player &optional dungeon)
  "Returns a dungeon object.  If the optional dungeon is
passed, this object will be used instead of a new one generated."

  (if (> (player.depth player) 0)
      (generate-dungeon! player dungeon)
      (generate-town! player dungeon)))
  

