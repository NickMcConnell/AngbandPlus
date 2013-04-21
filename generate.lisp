;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: generate.lisp - generation of dungeon levels
Copyright (c) 2000-2003 - Stig Erik Sandø

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

(defun %retrieve-door (variant key operation x y)
  (let ((door (get-door variant key)))
    
    (unless door
      (warn "Unable to find door with key ~s" key)
      (return-from %retrieve-door nil))

    (setf (location-x door) x
	  (location-y door) y
	  (decor.visible? door) t) ;; default

    ;; always closed
    (decor-operation variant door operation :value t)
    
    door))


(defun place-random-door! (variant dungeon x y)
  "Places a door at the given coordinate"
  (let* ((val (random 1000))
	 (*dungeon* dungeon)
	 (the-door nil))

    (cond ((< val 300)
	   ;; normal open door
	   (setf the-door (%retrieve-door variant "open-door" :open x y)))
	  
	  ((< val 400)
	   ;; broken door
	   (setf the-door (%retrieve-door variant "destroyed-door" :open x y))
	   (setf (door.broken? the-door) t)
	   )

	  ((< val 600)
	   ;; secret door
	   (setf the-door (%retrieve-door variant "closed-door" :close x y))
	   (setf (decor.visible? the-door) nil)
	   )
	  
	  ((< val 850)
	   ;; normal closed door
	   (setf the-door (%retrieve-door variant "closed-door" :close x y)))

	  ((< val 980)
	   ;; locked (and closed) door
	   (setf the-door (%retrieve-door variant "closed-door" :close x y))
	   (setf (door.lock the-door) (randint 7)))

	  ;; the remaining are stuck and closed doors
	  (t
	   (setf the-door (%retrieve-door variant "closed-door" :close x y))
	   (setf (door.stuck the-door) (randint 8))))

    (when the-door
      (place-door! variant dungeon x y the-door))


    the-door))
 

(defun next-to-corridor (dungeon x y)
  "returns a fixnum"
  (declare (type fixnum x y))
  (let ((retval 0)
	(ddx-ddd *ddx-ddd*)
	(ddy-ddd *ddy-ddd*))

    (loop for i below +simple-direction-number+
	  for tmp-x of-type u-fixnum = (+ x (svref ddx-ddd i))
	  for tmp-y of-type u-fixnum = (+ y (svref ddy-ddd i))
	  do
	  (unless (or (not (cave-floor-bold? dungeon tmp-x tmp-y))
		      (not (bit-flag-set? (floor.flags (cave-floor dungeon tmp-x tmp-y))
					  +floor-flag-floor+))
		      (cave-is-room? dungeon tmp-x tmp-y))
	    (incf retval)))
    
    retval))

(defun is-real-wall? (variant floor)
  (declare (ignore variant))
  (let ((ftype (etypecase floor
		 (floor-type floor)
		 (integer (get-floor-type floor)))))
    (bit-flag-set? (floor.flags ftype)
		   +floor-flag-wall+)))


(defun possible-doorway? (variant dungeon x y)
  (declare (type fixnum x y))

  (when (>= (next-to-corridor dungeon x y) 2)
    ;; check vertical
    (when (and (is-real-wall? variant (cave-floor dungeon x (1- y)))
	       (is-real-wall? variant (cave-floor dungeon x (1+ y))))
      (return-from possible-doorway? t))
    ;; check horizontal
    (when (and (is-real-wall? variant (cave-floor dungeon (1- x) y))
	       (is-real-wall? variant (cave-floor dungeon (1+ x) y)))
      (return-from possible-doorway? t)))
  nil)


(defun try-door! (variant dungeon x y)
  "attempts to place a door"
  (declare (type fixnum x y))
  (unless (in-bounds? dungeon x y)
    ;; ignore walls or rooms
    (unless (or (is-real-wall? variant (cave-floor dungeon x y))
		(cave-is-room? dungeon x y))
      ;; chance and allowed
      (when (and (< (random 100) +tunnel-junction+)
		 (possible-doorway? variant dungeon x y))
	(place-random-door! variant dungeon x y)))))


(defun new-player-spot! (dungeon player)
  (let ((x 0)
	(y 0)
	(max-x (- (dungeon.width dungeon) 2))
	(max-y (- (dungeon.height dungeon) 2)))
    
    (declare (type u-fixnum x y max-x max-y))
    
    (loop
     (setq x (rand-range 1 max-x)
	   y (rand-range 1 max-y))

     (when (and (can-place? dungeon x y :creature)
		(not (cave-icky? dungeon x y))
		(eq nil (cave-objects dungeon x y)))
;;       (warn "placing player at ~s,~s" x y)
       (place-player! dungeon player x y)
       (return-from new-player-spot! nil)))))

(defmethod place-rubble! ((variant variant) dungeon x y)
  (declare (type fixnum x y))
  (setf (cave-floor dungeon x y) "rubble"))

(defun %execute-trap (the-trap dungeon x y)
  (cond ((and (typep the-trap 'active-trap) (typep (decor.type the-trap) 'trap-type))
	 (let ((trap-type (decor.type the-trap)))
	   (cond ((functionp (trap.effect trap-type))
		  (funcall (trap.effect trap-type) the-trap dungeon x y)
		  ;; assuming everything went ok
		  (disturbance *variant* *player* the-trap :max)
		  (decor-operation *variant* the-trap :visible))
		 (t
		  (warn "Trap ~s does not have a funcallable effect." (trap.id trap-type))))))
	(t
	 (warn "%EXECUTE-TRAP was not passed a TRAP-argument, but ~s" the-trap))
	))
			
;;  (warn "Executing trap ~s" the-trap)

(defvar *cached-trap-event* nil)

(defun create-simple-trap (type x y)
  (let ((trap (make-instance 'active-trap :type type :loc-x x :loc-y y)))
    (unless *cached-trap-event*
      (setf *cached-trap-event* (make-coord-event "step" #'%execute-trap nil)))
    (push *cached-trap-event* (decor.events trap))
    trap))

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
	    (return-from find-random-trap (create-simple-trap v x y))))

    nil))

(defmethod place-trap! ((variant variant) dungeon x y (the-trap active-trap))
;;  (warn "trying to place trap ~s at (~s,~s) where we have ~s,~s"
;;	the-trap x y (cave-floor dungeon x y) (dungeon.decor dungeon))
  (when (in-bounds? dungeon x y)
    (let ((decor (cave-decor dungeon x y)))
      (when  (or (can-place? dungeon x y :trap)
		 ;; this is to allow us put a trap in an old spot!
		 (and (typep decor 'active-trap)
		      (not (decor.visible? decor))))
	(setf (cave-decor dungeon x y) the-trap)
	(pushnew the-trap (dungeon.decor dungeon))

	))))


(defmethod place-random-trap! ((variant variant) dungeon x y)
  "Tries to place a trap at given location."
  (when (and (in-bounds? dungeon x y)
	     (can-place? dungeon x y :trap))

    (when-bind (the-trap (find-random-trap variant dungeon x y))
      (if (is-trap? the-trap)
	  (place-trap! variant dungeon x y the-trap)
	  (warn "Did not find a proper trap but got ~s" the-trap)))))


(defun let-floor-carry! (dungeon x y obj)

  ;; fix later
;;  (warn "carry ~a" obj)
  (let ((place (cave-objects dungeon x y)))
    (unless place
      (setf (cave-objects dungeon x y) (make-floor-container dungeon x y)))
    
    (item-table-add! (cave-objects dungeon x y) obj))

  (values))

(defmethod create-gold ((variant variant) (dungeon dungeon) &key originator)
  (declare (ignore originator))
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
      (cond (great-p
	     (setq rolls 4)) ;; 4 rolls
	    ((eq status :great)
	     (setq rolls 1)) ;; one roll
	    ))

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

  ;; skip good and great
  (let* (;;(prob-sp-object (if good-p 10 1000))
	 (depth (dungeon.depth dungeon))
	 (level *level*)
	 (base-obj-depth (if good-p (+ depth 10) depth))
	 
	 (obj (get-active-object-by-level variant level :depth base-obj-depth)))

    (apply-magic! variant obj depth :good-p good-p :great-p great-p)

    (attempt-multi-creation! variant obj depth)
    
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
  
  (flet  ((%is-wall? (nx ny)
	    (and (bit-flag-set? (floor.flags (cave-floor dungeon nx ny))
				+floor-flag-wall+)
		 (eq (cave-decor dungeon nx ny) nil))))
    (let ((k 0))
      (when (%is-wall? (1+ x) y) (incf k))
      (when (%is-wall? (1- x) y) (incf k))
      (when (%is-wall? x (1+ y)) (incf k))
      (when (%is-wall? x (1- y)) (incf k))
      k)))

(defmethod allocate-object! ((variant variant) (dungeon dungeon) set type number)
  "Allocate an object.. blah."

  (let ((dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon))
	(x 0)
	(y 0))
    (declare (type fixnum x y))
    (dotimes (i number)

    ;; find legal spot
      ;; FIX
;;      (return-from allocate-object! nil) 
      
      (tagbody restart-loop
	 (loop named legal-spot-finder
	       do
	       (setq x (random dungeon-width)
		     y (random dungeon-height))
	       (unless (can-place? dungeon x y :object)
		 (go restart-loop))
	       (let ((room-p (cave-is-room? dungeon x y)))
		 (when (or (and (eq set 'alloc-set-corr) room-p)
			   (and (eq set 'alloc-set-room) (not room-p)))
		   (go restart-loop)))
	       (return-from legal-spot-finder)))
      
      (case type
	(alloc-type-rubble (place-rubble! variant dungeon x y))
	(alloc-type-trap   (place-random-trap! variant dungeon x y))
	;; add again later
	(alloc-type-gold   (place-gold! variant dungeon x y))
	(alloc-type-object (place-object! variant dungeon x y nil nil))))
  
    (values)))

(defun allocate-stairs! (dungeon dir how-many walls)
  "Allocates stairs in the given dungeon."
  
  (let ((dungeon-height (dungeon.height dungeon))
	(dungeon-width  (dungeon.width dungeon)))
    (declare (type fixnum dungeon-height dungeon-width))
    (dotimes (i how-many)

      (block placed-stair

	(dotimes (j 3000)
	  (let ((y (random dungeon-height))
		(x (random dungeon-width)))
	    (declare (type fixnum x y))
	    (when (and (can-place? dungeon x y :stair)
		       (>= (next-to-walls dungeon x y) walls))

	      (let ((fl-type (if (eq dir :up)
				 (get-floor-type "stair-up")
				 (get-floor-type "stair-down"))))
		(setf (cave-floor dungeon x y) fl-type))
	      (return-from placed-stair))))
      
	(when (> walls 0)
	  (decf walls))
	    
      
	))))

(defun build-tunnel! (variant dungeon x1 y1 x2 y2)
  "builds a tunnel"
  (declare (type fixnum x1 y1 x2 y2))
  (let ((row1 y1)
	(col1 x1)
	(row2 y2)
	(col2 x2)
	(loop-counter 0)
	(door-flag nil)
	(start-row y1)
	(start-col x1)
	(room-wall-ptr (get-floor-type "room-wall")))

    (check-type room-wall-ptr floor-type)
    
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
		 (let* ((feature (cave-floor dungeon tmp-col tmp-row))
			(flags (floor.flags feature))
			)

		   (check-type feature floor-type)
		   (check-type flags fixnum)
		   
		   ;; make sure we're not at the map-edge, vault-edge or at solid granite
		   (cond ((or (bit-flag-set? flags +floor-flag-permanent+)
			      (bit-flag-set? (cave-flags dungeon tmp-col tmp-row) +cave-no-tunnel+))
			  (go start-of-loop))

			 ;; pierce normal room walls
			 ((eql feature room-wall-ptr) ;; check id instead, in case variant returns random?
			  (let* ((y (+ tmp-row row-dir))
				 (x (+ tmp-col col-dir))
				 (feat (cave-floor dungeon x y))
				 (flgs (floor.flags feat)))
			    (check-type feat floor-type)
			    (check-type flgs fixnum)

			    #||
			    (warn "room wall -> ~s ~s ~s ~s ~s" feat (bit-flag-set? flgs +floor-flag-permanent+)
				  (cave-flags dungeon x y)
				  (bit-flag-set? (cave-flags dungeon x y) +cave-no-tunnel+)
				  (eql feat room-wall-ptr))
			    ||#

			    ;; don't go in this direction if next is a nasty wall?
			    (when (or (bit-flag-set? flgs +floor-flag-permanent+)
				      (bit-flag-set? (cave-flags dungeon x y) +cave-no-tunnel+)
				      (eql feat room-wall-ptr))
			      (go start-of-loop))
			    

			    
			    (setq row1 tmp-row
				  col1 tmp-col)

			    (push (cons col1 row1) (dun-data.walls *cur-dun*))

			    ;; make sure walls around piercing is solid
			    (loop for wy of-type u-fixnum from (1- row1) to (1+ row1)
				  do
				  (loop for wx of-type u-fixnum from (1- col1) to (1+ col1)
					do
					(when (eql (cave-floor dungeon wx wy) room-wall-ptr)
					  (bit-flag-add! (cave-flags dungeon wx wy) +cave-no-tunnel+))))
			    
			    ))

			
			 ;; travel through rooms
			 ((bit-flag-set? (cave-flags dungeon tmp-col tmp-row)
					 +cave-room+)
			  (setq col1 tmp-col
				row1 tmp-row))
			 
			 
			 ;; tunnel through other walls
			 ((bit-flag-set? flags +floor-flag-wall+)
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
	(setf (cave-floor dungeon (car i) (cdr i)) "normal-floor"))
      

      ;; do piercing
      (dolist (i (dun-data.walls *cur-dun*))
	;;(setf (cave-floor dungeon (car i) (cdr i)) "normal-floor")
	(when (< (random 100) +tunnel-door+)
	  (place-random-door! variant dungeon (car i) (cdr i))))
      
      )))
		   
			 
		   

      

(defun random-direction ()
  "Returns two values with a random nsew direction"
  (let ((val (random +simple-direction-number+)))
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
	 (dungeon-height (setting-lookup settings "max-height"))
	 (dungeon-width (setting-lookup settings "max-width"))
	 (dungeon (create-dungeon dungeon-width dungeon-height
				  :its-depth (level.depth level)))
	 (*cur-dun* (make-dun-data))
	 (cave-wall (get-floor-type "cave-wall"))
	 )


    (check-type cave-wall floor-type)
     
    ;; start with granite
    (fill-dungeon-with-floor! dungeon cave-wall)

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
      (let ((dungeon-rooms (setting-lookup settings "room-number")))
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
		(let ((the-room-type (find-appropriate-room variant level player)))
		  (construct-room! the-room-type dungeon player bx by))
		
		;; fill in
	  
		))))

    (let ((perm-outer (get-floor-type "permanent-outer-wall")))
    ;; perm walls on top and bottom
      (loop for y in (list 0 (1- dungeon-height))
	    for x from 0 below dungeon-width
	    do
	    (setf (cave-floor dungeon x y) perm-outer))
      
      ;; perm walls on left and right
      (loop for x in (list 0 (1- dungeon-width))
	    for y from 0 below dungeon-height
	    do
	    (setf (cave-floor dungeon x y) perm-outer)))
    
    
    ;; make list into an array
    (let* ((centre-list (dun-data.room-centres *cur-dun*))
	   (len (length centre-list))
	   (centres (make-array len)))
      
      (loop for i from 0
	    for c in centre-list
	    do
	    (setf (svref centres i) c))

      ;; scramble
      (loop for i from 0 below len
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
	      (build-tunnel! variant dungeon this-x this-y x y)
	      (setf x this-x
		    y this-y)))

      
      )
    

    ;; place unplaced doors
    (let ((doors (dun-data.doors *cur-dun*)))
      (dolist (i doors)
	(let ((x (car i))
	      (y (cdr i)))
	  (try-door! variant dungeon (1- x) y)
	  (try-door! variant dungeon (1+ x) y)
	  (try-door! variant dungeon x (1- y))
	  (try-door! variant dungeon x (1+ y)))))
    
    (let ((stairs-up (setting-lookup settings "stairs-up"))
	  (stairs-down (setting-lookup settings "stairs-down")))

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
      (dotimes (i (randint depth-constant))
	(allocate-object! variant dungeon 'alloc-set-both 'alloc-type-trap (randint depth-constant)))

      ;; we want monsters
      (dotimes (i (+ depth-constant 14 (randint 8)))
	(allocate-monster! variant dungeon player 0 t)))

    
    ;; in rooms
    (allocate-object! variant dungeon 'alloc-set-room 'alloc-type-object 9)
	
    (allocate-object! variant dungeon 'alloc-set-both 'alloc-type-gold 4)
    (allocate-object! variant dungeon 'alloc-set-both 'alloc-type-object 4)

    (new-player-spot! dungeon player)
    
    (setf (level.dungeon level) dungeon)
    
    level))


(defmethod activate-object :after ((obj random-level) &key leave-method player)

;;  (warn "post-init of random level, ~a" leave-method)

  (when leave-method
    (let* ((dungeon (level.dungeon obj))
	   (the-player (if player player *player*))
	   (px (location-x the-player))
	   (py (location-y the-player))
	   (feat (case leave-method
		   (:down-stair (get-floor-type "stair-up"))
		   (:up-stair (get-floor-type "stair-down"))
		   (otherwise nil))))

      
      (when feat
;;	(warn "placing feature at ~s,~s" px py) 
	(setf (cave-floor dungeon px py) feat))
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
