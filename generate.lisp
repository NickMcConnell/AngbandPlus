;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: generate.lisp - generation of dungeon levels
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with generation of dungeon levels.

|#

(in-package :org.langband.engine)

;; old table of room-data, see at the end

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


(defun place-closed-door! (dungeon x y)
  "Places a closed door at the given coordinate"
  (let ((val (random 400)))
    ;; normal closed door
    (cond ((< val 300)
	   (setf (cave-floor dungeon x y) +floor-door-head+))
	  ;; locked door
	  ((< val 399)
	   (setf (cave-floor dungeon x y) (+ +floor-door-head+ (randint 7))))
	  ;; stuck door
	  (t
	   (setf (cave-floor dungeon x y) (+ +floor-door-head+ #x08 (random 8)))))))



(defun place-random-door! (dungeon x y)
  "Places a door at the given coordinate"
  (let ((val (random 1000)))
    ;; open door
    (cond ((< val 300)
	   (setf (cave-floor dungeon x y) +floor-open-door+))
	  ;; broken door
	  ((< val 400)
	   (setf (cave-floor dungeon x y) +floor-broken-door+))
	  ;; secret door
	  ((< val 600)
	   (setf (cave-floor dungeon x y) +floor-secret-door+))
	  ;; closed door
	  (t
	   (place-closed-door! dungeon x y)))))


(defun next-to-corridor (dungeon x y)
  "returns a fixnum"
  (declare (type fixnum x y))
  (let ((retval 0)
	(ddx-ddd *ddx-ddd*)
	(ddy-ddd *ddy-ddd*))
    
    (dotimes (i 4)
      (let ((tmp-x (+ x (svref ddx-ddd i)))
	    (tmp-y (+ y (svref ddy-ddd i))))
	(declare (type u-fixnum tmp-x tmp-y))
	
	(unless (or (not (cave-floor-bold? dungeon tmp-x tmp-y))
		    (/= (cave-floor dungeon tmp-x tmp-y) +floor-regular+)
		    (cave-is-room? dungeon tmp-x tmp-y))
	  (incf retval))))
    retval))

(defun possible-doorway? (dungeon x y)
  (declare (type fixnum x y))
  (when (>= (next-to-corridor dungeon x y) 2)
    ;; check vertical
    (when (and (<= +floor-magma+ (cave-floor dungeon x (1- y)))
	       (<= +floor-magma+ (cave-floor dungeon x (1+ y))))
      (return-from possible-doorway? t))
    ;; check horizontal
    (when (and (<= +floor-magma+ (cave-floor dungeon (1- x) y))
	       (<= +floor-magma+ (cave-floor dungeon (1+ x) y)))
      (return-from possible-doorway? t)))
  nil)
    

(defun try-door! (dungeon x y)
  "attempts to place a door"
  (declare (type fixnum x y))
  (unless (in-bounds? dungeon x y)
    ;; ignore walls or rooms
    (unless (or (>= (cave-floor dungeon x y) +floor-magma+)
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
;;       (warn "placing player at ~s,~s" x y)
       (place-player! dungeon player x y)
       (return-from new-player-spot! nil)))))

(defmethod place-rubble! ((variant variant) dungeon x y)
  (declare (type fixnum x y))
  (setf (cave-floor dungeon x y) +floor-rubble+))

(defun make-trap-visible (the-trap dungeon x y)
  (setf (decor.visible? the-trap) t)
  (light-spot! dungeon x y))

(defun %execute-trap (the-trap dungeon x y)
  (cond ((and (typep the-trap 'active-trap) (typep (trap.type the-trap) 'trap-type))
	 (let ((trap-type (trap.type the-trap)))
	   (cond ((functionp (trap.effect trap-type))
		  (funcall (trap.effect trap-type) the-trap dungeon x y)
		  ;; assuming everything went ok
		  (make-trap-visible the-trap dungeon x y))
		 (t
		  (warn "Trap ~s does not have a funcallable effect." (trap.id trap-type))))))
	(t
	 (warn "%EXECUTE-TRAP was not passed a TRAP-argument, but ~s" the-trap))
	))
			
;;  (warn "Executing trap ~s" the-trap)


;; should be reimplemented, slow and ignores rarity
(defmethod find-random-trap (variant dungeon x y)
  (declare (ignore dungeon))
  (let* ((table (variant.traps variant))
	 (num-traps (hash-table-count table))
	 (which-trap (random num-traps)))
    
    (loop for i from 0
	  for v being the hash-values of table
	  do
	  (when (= i which-trap)
	    (let ((trap (make-instance 'active-trap :type v :loc-x x :loc-y y)))
	      (return-from find-random-trap trap))))
    nil))
 

(defmethod place-trap! ((variant variant) dungeon x y)
  "Tries to place a trap at given location."
  (when (and (in-bounds? dungeon x y)
	     (cave-boldly-naked? dungeon x y))

    (when-bind (the-trap (find-random-trap variant dungeon x y))
;;      (warn "Making trap (~s) at ~s,~s" (trap.name (trap.type the-trap)) x y)
      (push (make-coord-event "step" #'%execute-trap nil) (decor.events the-trap))
      (setf (cave-decor dungeon x y) the-trap)
      ;; hack I guess
      (setf (cave-floor dungeon x y) +floor-invisible-trap+))
    ))


(defun let-floor-carry! (dungeon x y obj)

  ;; fix later
;;  (warn "carry ~a" obj)
  (let ((place (cave-objects dungeon x y)))
    (unless place
      (setf (cave-objects dungeon x y) (make-floor-container dungeon x y)))
    
    (item-table-add! (cave-objects dungeon x y) obj))

  (values))

(defmethod create-gold ((variant variant) (dungeon dungeon))
  (error "Please make CREATE-GOLD for variant.. this depends heavily on variant."))

(defmethod add-magic-to-item! ((variant variant) item depth quality)
  (declare (ignore item depth quality))
  ;; do nothing, not magical
  )

;; :good is +1, :great is +2, :cursed is -1 and :broken is -2

(defmethod apply-magic! ((variant variant) obj base-level &key good-p great-p (allow-artifact t))
  
  (let* ((base-good-chance (+ 10 base-level))
	 (base-great-chance (int-/ base-good-chance 2))
	 (status (if great-p
		     :great
		     (if good-p
			 :good
			 :normal)))
	 (rolls 0) ;; rolls for artifact
	 )

    (when (> base-good-chance 75) (setq base-good-chance 75))
    (when (> base-great-chance 20) (setq base-great-chance 20))

    ;; good or great?
    (cond ((or (eq status :good) (< (random 100) base-good-chance))
	   (setq status :good)
	   (when (or (eq status :great) (< (random 100) base-great-chance))
	     (setq status :great)))
	  ;; cursed?
	  ((< (random 100) base-good-chance)
	   (setq status :cursed)
	   (when (< (random 100) base-great-chance)
	     (setq status :broken))))

    (when allow-artifact
      (cond ((eq status :great)
	     (setq rolls 1)) ;; one roll
	    (great-p
	     (setq rolls 4)))) ;; 3 more rolls

    (dotimes (i rolls)
      ;; try for artifact
      )

    ;; skip analysis of artifact

    
    (add-magic-to-item! variant obj base-level status)

    ;; skip analysis of ego-items

    ;; skip last part
    ))



(defmethod create-object ((variant variant) (dungeon dungeon) good-p great-p)
  "Creates an object for the given dungeon-object."

  (declare (ignore great-p))
  ;; skip good and great
  (let* (;;(prob-sp-object (if good-p 10 1000))
	 (depth (dungeon.depth dungeon))
	 (level *level*)
	 (base-obj-depth (if good-p (+ depth 10) depth))
	 
	 (obj (get-active-object-by-level variant level :depth base-obj-depth)))

    (apply-magic! variant obj depth)
    
    obj))


(defmethod place-gold! ((variant variant) (dungeon dungeon) x y)
  (declare (type fixnum x y))
  ;; skip paranoia
  (let ((gold (create-gold variant dungeon)))
    (when gold
      (let-floor-carry! dungeon x y gold)))
  
  (values))

(defmethod place-object! ((variant variant) (dungeon dungeon) x y good-p great-p)
  (declare (type fixnum x y))
 ;; skip paranoia
  (let ((obj (create-object variant dungeon good-p great-p)))
    (when obj
      (let-floor-carry! dungeon x y obj)))
  
  (values))

(defun next-to-walls (dungeon x y)
  "Returns number of close walls in the four dirs."
  (declare (type fixnum x y))  
  (let ((k 0))
    (when (>= (cave-floor dungeon (1+ x) y) +floor-wall-extra+) (incf k))
    (when (>= (cave-floor dungeon (1- x) y) +floor-wall-extra+) (incf k))
    (when (>= (cave-floor dungeon x (1+ y)) +floor-wall-extra+) (incf k))
    (when (>= (cave-floor dungeon x (1- y)) +floor-wall-extra+) (incf k))
    k))

(defmethod allocate-object! ((variant variant) (dungeon dungeon) set type number)
  "Allocate an object.. blah."

  (let ((dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon))
	(x 0)
	(y 0))
    (declare (type fixnum x y))
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
	(alloc-type-rubble (place-rubble! variant dungeon x y))
	(alloc-type-trap   (place-trap! variant dungeon x y))
	;; add again later
	(alloc-type-gold   (place-gold! variant dungeon x y))
	(alloc-type-object (place-object! variant dungeon x y nil nil))))
  
    (values)))

(defun allocate-stairs! (dungeon dir how-many walls)
  "Allocates stairs."
  
  (let ((dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon)))
    (declare (type fixnum dungeon-height dungeon-width))
    (dotimes (i how-many)

      (block placed-stair

	(dotimes (j 3000)
	  (let ((y (random dungeon-height))
		(x (random dungeon-width)))
	    (declare (type fixnum x y))
	    (when (and (cave-boldly-naked? dungeon x y)
		       (>= (next-to-walls dungeon x y) walls))
	    
	      (setf (cave-floor dungeon x y) (if (eq dir :up)
						   +floor-less+
						   +floor-more+))
	      (return-from placed-stair))))
      
	(when (> walls 0)
	  (decf walls))
	    
      
	))))

(defun build-tunnel! (dungeon x1 y1 x2 y2)
  "builds a tunnel"
  (declare (type fixnum x1 y1 x2 y2))
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
		 (let ((feature (cave-floor dungeon tmp-col tmp-row)))

		   ;; mae sure we're not at the map-edge, vault-edge or at solid granite
		   (cond ((or (eql feature +floor-perm-solid+)
			      (eql feature +floor-perm-outer+)
			      (eql feature +floor-wall-solid+))
			  (go start-of-loop))

			 ;; pierce normal outer walls
			 ((eql feature +floor-wall-outer+)
			  (let* ((y (+ tmp-row row-dir))
				 (x (+ tmp-col col-dir))
				 (feat (cave-floor dungeon x y)))

			    (when (or (eql feat +floor-perm-solid+)
				      (eql feat +floor-perm-outer+)
				      (eql feat +floor-wall-solid+)
				      (eql feat +floor-wall-outer+))
			      (go start-of-loop))

			    (setq row1 tmp-row
				  col1 tmp-col)

			    (push (cons col1 row1) (dun-data.walls *cur-dun*))

			    ;; make sure walls around piercing is solid
			    (loop for wy of-type u-fixnum from (1- row1) to (1+ row1)
				  do
				  (loop for wx of-type u-fixnum from (1- col1) to (1+ col1)
					do
					(when (eql (cave-floor dungeon wx wy) +floor-wall-outer+)
					  (setf (cave-floor dungeon wx wy) +floor-wall-solid+))))
			    
			    ))

			
			 ;; travel through rooms
			 ((bit-flag-set? (cave-flags dungeon tmp-col tmp-row)
					 +cave-room+)
			  (setq col1 tmp-col
				row1 tmp-row))
			 
			 
			 ;; tunnel through other walls
			 ((>= feature +floor-wall-extra+)
			  ;;(warn "g.")
			  (setq col1 tmp-col
				row1 tmp-row)
			  
			  ;; fix, inefficient
;;			  (when (< (length (dun-data.tunnels *cur-dun*)) +tunnel-max+)

			  (push (cons tmp-col tmp-row) (dun-data.tunnels *cur-dun*))
			  (setq door-flag nil))

			 ;; corridor intersections.. et al
			 (t

			  (setq row1 tmp-row
				col1 tmp-col)

			  (unless door-flag
			    ;; skip max-check
			    (push (cons col1 row1) (dun-data.doors *cur-dun*))
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
      (dolist (i (dun-data.tunnels *cur-dun*))
;;	(warn "tunnel..~a" i)
	(setf (cave-floor dungeon (car i) (cdr i)) +floor-regular+))
      

      ;; do piercing
      (dolist (i (dun-data.walls *cur-dun*))
	(setf (cave-floor dungeon (car i) (cdr i)) +floor-regular+)
	(when (< (random 100) +tunnel-door+)
	  (place-random-door! dungeon (car i) (cdr i))))
      
      )))
		   
			 
		   

      

(defun random-direction ()
  "Returns two values with a random nsew direction"
  (let ((val (random 4)))
    (values (svref *ddx-ddd* val)
	    (svref *ddy-ddd* val))))
	    

(defun generate-room (dungeon x1 y1 x2 y2 light)
  "simple helper function which marks an area as a room.  The
light argument is a boolean."
  
  (declare (type u-fixnum x1 y1 x2 y2))
  
  (loop for y of-type fixnum from y1 to y2
	do
	(loop for x of-type fixnum from x1 to x2
	      do
	      (bit-flag-add! (cave-flags dungeon x y)
			     +cave-room+
			     (if light +cave-glow+ 0)))))


(defun generate-fill (dungeon x1 y1 x2 y2 feat)
  
  (assert (and (< y1 y2) (< x1 x2)))
  
  (loop for y of-type fixnum from y1 to y2
	do
	(loop for x of-type fixnum from x1 to x2
	      do
	      ;;(warn "fish [~a,~a] -> ~a" x y feat)
	      (setf (cave-floor dungeon x y) feat))))


(defun generate-draw (dungeon x1 y1 x2 y2 feat)
  "draws a box matching the coordinates"

  (declare (type fixnum x1 y1 x2 y2 ))
  
  (loop for y of-type fixnum from y1 to y2
	do
	(setf (cave-floor dungeon x1 y) feat)
	(setf (cave-floor dungeon x2 y) feat))

  (loop for x of-type fixnum from x1 to x2
	do
	(setf (cave-floor dungeon x y1) feat)
	(setf (cave-floor dungeon x y2) feat))

  )



(defmethod generate-level! ((variant variant) (level random-level) player)
  "Generates a dungeon level and returns it.  If the optional dungeon
argument is passed it will be used as new dungeon and returned."

  (let* ((*level* level)
	 (settings (get-setting variant :random-level))
	 (dungeon-height (slot-value settings 'max-height))
	 (dungeon-width (slot-value settings 'max-width))
	 (dungeon (create-dungeon dungeon-width dungeon-height
				  :its-depth (level.depth level)))
	 (*cur-dun* (make-dun-data))
	 )

    
    ;; start with granite
    (fill-dungeon-with-floor! dungeon +floor-wall-extra+)

    ;; skip destroyed levels
    
    (setf (dun-data.row-rooms *cur-dun*) (int-/ dungeon-height +block-height+)
	  (dun-data.col-rooms *cur-dun*) (int-/ dungeon-width +block-width+)

	  ;; init room-table
	  (dun-data.room-map *cur-dun*) (make-array (list (dun-data.col-rooms *cur-dun*)
							  (dun-data.row-rooms *cur-dun*))
						    :initial-element nil)
	  (dun-data.crowded *cur-dun*) nil)

    ;;    (warn "Roomie ~a" (eql *dungeon* dungeon))
    
    ;; build rooms
    (block room-building
      (let ((dungeon-rooms (slot-value settings 'room-number)))
;;	(warn "ROOMS ~s" dungeon-rooms)
	(loop for i of-type u-fixnum from 0 to dungeon-rooms;; fix
	      do

	      (let ((by (random (dun-data.row-rooms *cur-dun*)))
		    (bx (random (dun-data.col-rooms *cur-dun*))))

		(when +dungeon-align+
		  ;; slide left
		  (when (= (mod bx 3) 0) (incf bx))
		  ;; slide right
		  (when (= (mod bx 3) 2) (decf bx)))
	      
	    
		;; skip destroy
		;; skip unusual
		(let ((the-room-type (find-appropriate-room *variant* level player)))
		  (construct-room! the-room-type dungeon player bx by))
		
		;; fill in
	  
		))))

    ;; perm walls on top and bottom
    (loop for y in (list 0 (1- dungeon-height))
	  for x from 0 to (1- dungeon-width)
	  do
	  (setf (cave-floor dungeon x y) +floor-perm-solid+))
    
    ;; perm walls on left and right
    (loop for x in (list 0 (1- dungeon-width))
	  for y from 0 to (1- dungeon-height)
	  do
	  (setf (cave-floor dungeon x y) +floor-perm-solid+))
    
    
    ;; make list into an array
    (let* ((centre-list (dun-data.room-centres *cur-dun*))
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

      (setf (dun-data.room-centres *cur-dun*) centres)

;;      (warn "tunnel..")
      (let* ((last (svref centres (1- len)))
	     (x (car last))
	     (y (cdr last)))
	(loop for c across centres
	      for this-x = (car c)
	      for this-y = (cdr c)
	      do
	      (build-tunnel! dungeon this-x this-y x y)
	      (setf x this-x
		    y this-y)))

      
      )
    

    ;; place unplaced doors
    (let ((doors (dun-data.doors *cur-dun*)))
      (dolist (i doors)
	(let ((x (car i))
	      (y (cdr i)))
	  (try-door! dungeon (1- x) y)
	  (try-door! dungeon (1+ x) y)
	  (try-door! dungeon x (1- y))
	  (try-door! dungeon x (1+ y)))))
    
    (let ((stairs-up (slot-value settings 'stairs-up))
	  (stairs-down (slot-value settings 'stairs-down)))

;;      (warn "UP: ~s" stairs-up)
      (allocate-stairs! dungeon :down (rand-range (car stairs-down)
						  (cdr stairs-down)) 3)
      (allocate-stairs! dungeon :up   (rand-range (car stairs-up)
						  (cdr stairs-up)) 3))

    (let ((depth-constant (int-/ (dungeon.depth dungeon) 3)))
      (when (> depth-constant 10) (setq depth-constant 10))
      (when (< depth-constant 2)  (setq depth-constant 2))

      ;; allocate rubble
      ;; allocate traps
      (dotimes (i 10) ;; hack
	(allocate-object! variant dungeon 'alloc-set-both 'alloc-type-trap (randint depth-constant)))

      ;; we want monsters
      (dotimes (i (+ depth-constant 14 (randint 8)))
	(allocate-monster! variant dungeon player 0 t)))

    
    (new-player-spot! dungeon player)

    
    ;; in rooms
    (allocate-object! variant dungeon 'alloc-set-room 'alloc-type-object 9)
	
    (allocate-object! variant dungeon 'alloc-set-both 'alloc-type-gold 4)
    (allocate-object! variant dungeon 'alloc-set-both 'alloc-type-object 4)

    (setf (level.dungeon level) dungeon)
    
    level))


(defmethod activate-object :after ((obj random-level) &key leave-method player)

;;  (warn "post-init of random level, ~a" leave-method)

  (when leave-method
    (let* ((dun (level.dungeon obj))
	   (pl (if player player *player*))
	   (px (location-x pl))
	   (py (location-y pl))
	   (feat (case leave-method
		   (:down-stair +floor-less+)
		   (:up-stair +floor-more+)
		   (otherwise nil))))

      
      (when feat
;;	(warn "placing feature at ~s,~s" px py) 
	(setf (cave-floor dun px py) feat))
      ))
  

  
  obj)

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
