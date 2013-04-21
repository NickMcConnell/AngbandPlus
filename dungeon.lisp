;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: dungeon.lisp - basic code for the dungeon
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

ADD_DESC: Simple code to access the dungeon object(s) 

|#

(in-package :langband)

(defstruct (dungeon-coord (:conc-name coord.))
  (feature 0 :type fixnum)
  (flags 0 :type fixnum)  ;; info-flag in angband
  (objects nil)
  (monsters nil)
  )

(defstruct (dungeon (:conc-name dungeon.))
  (table nil)
  (height *dungeon-height* :type fixnum)
  (width *dungeon-width* :type fixnum)

  (up-stairs-p nil)
  (down-stairs-p nil)

  (level nil)
  (monsters nil))
  

#||
(defmethod print-object ((inst dungeon) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	   (dungeon.level inst))
  inst))
||#

(defun create-dungeon (width height &key its-level)
  "Creates and returns a dungeon of specified size"
  
  (let ((d (make-dungeon :height height :width width))
	(table (make-array (list width height))))
    
;;    (setf (dungeon.height d) height)
;;    (setf (dungeon.width d) width)
    
    (dotimes (j (dungeon.width d))
      (dotimes (i (dungeon.height d))
	(setf (aref table j i) (make-dungeon-coord))))

    (when its-level
      (setf (dungeon.level d) its-level))
    
    (setf (dungeon.table d) table)
    
    d))

(defun clean-coord! (coord)
  "Cleands the given coordinate."
  
  (setf (coord.feature  coord) 0
	(coord.flags    coord) 0
	(coord.objects  coord) nil
	(coord.monsters coord) nil)
  coord)

(defun clean-dungeon! (dungeon)
  "Clears all flags and everything from the dungeon."

  (dotimes (y (dungeon.height dungeon))
    (dotimes (x (dungeon.width dungeon))
      (clean-coord! (aref (dungeon.table dungeon) x y))))
  
  dungeon)
      


(defun make-map (dungeon)
  "Returns a map based on a given dungeon."
  (make-array (list (dungeon.width dungeon)
		    (dungeon.height dungeon))
	      :element-type 'fixnum :initial-element 0))

(defun cave-coord (dungeon x y)
  (aref (dungeon.table dungeon) x y))

(defun cave-feature (dungeon x y)
  (coord.feature (cave-coord dungeon x y)))

(defun cave-flags (dungeon x y)
  (coord.flags (cave-coord dungeon x y)))

(defun cave-objects (dungeon x y)
  (coord.objects (cave-coord dungeon x y)))

(defun cave-monsters (dungeon x y)
  (coord.monsters (cave-coord dungeon x y)))

(defun (setf cave-monsters) (val dungeon x y)
  ;; add smart stuff later..
  (setf (coord.monsters (aref (dungeon.table dungeon) x y)) (if (listp val)
								val
								(list val))))


(defun delete-monster! (dungeon monster)
  "Wipes the monster.."
  (warn "Deleting monster ~a" monster))

(defun (setf cave-objects) (val dungeon x y)
  (setf (coord.objects (aref (dungeon.table dungeon) x y)) val)
  )

(defun (setf cave-flags) (val dungeon x y)
  (setf (coord.flags (aref (dungeon.table dungeon) x y)) val)
  )
 

(defun (setf cave-feature) (val dungeon x y)
;;  (warn "Setting ~a ~a to ~a" x y val)
  (declare (type fixnum val))
  (let ((coord (aref (dungeon.table dungeon) x y)))

    (setf (coord.feature coord) val)

  ;; fix me
  
  (if (>= val +feature-door-head+)
      ;; this is a wall or a door..
      (bit-flag-add! (coord.flags coord) +cave-wall+)
;;      (setf (coord.flags coord) (logior (coord.flags coord) +cave-wall+))
      ;; we're not a a wall or a door.. I don't get this code..
      (bit-flag-remove! (coord.flags coord) +cave-wall+)
      )
;;      (setf (cave-flags dungeon x y) (logandc2 (cave-flags dungeon x y) +cave-wall+))

  
  ;; redraw
  ))



(defun fill-dungeon-with-feature! (dungeon feature)
  "Cleans and then fills the dungeon with a given feature.
Returns nothing."
  (dotimes (y (dungeon.height dungeon))
      (dotimes (x (dungeon.width dungeon))
	(clean-coord! (aref (dungeon.table dungeon) x y))
	(setf (cave-feature dungeon x y) feature)))
 
  (values))

(defun fill-dungeon-part-with-feature! (dungeon feature width-range height-range)
  "height-range and width-range should be conses where
car is start and cdr is the non-included end  (ie [start, end> )"

  (declare (type (cons fixnum fixnum) width-range height-range))
  
;;  (warn "Filling h: ~a and w: ~a" height-range width-range)
  
  (loop for i of-type fixnum from (car height-range)
	      to (the fixnum (1- (cdr height-range)))
	do
	(loop for j of-type fixnum from (car width-range)
	            to (the fixnum (1- (cdr width-range)))
	      do
	      (setf (cave-feature dungeon j i) feature)))
  (values))

(defun %map-info (dungeon x y)
  "Returns a CONS with the map-info (attr . char)"
;;  (warn "info for {~a,~a}" x y)
  
  (cond ((let ((monsters (cave-monsters dungeon x y)))
	   (when monsters
	     (let ((kind (amon.kind (car monsters))))
	       (cons (monster.colour kind)
		     (monster.symbol kind))))))
	 
	 ;; remove this entry later..
	 ((and (eql x (player.loc-x *player*))
	       (eql y (player.loc-y *player*)))
;;	  (warn "returning player at {~a,~a}" x y)
	  (cons +term-white+ #\@))
	 
	 ((let ((obj (cave-objects dungeon x y)))
	    (cond ((and obj (typep obj 'item-table))
		  ;; do we have objects..
		  (if (> (items.cur-size obj) 1)
		      ;; pile symbol
		      (cons +term-white+
			    #\&)
		      ;; single object
		      (let* ((kind (aobj.kind (item-table-find obj 0)))
			     (flavour (object.flavour kind)))
			(if flavour
			    (cons (cadr flavour)
				  (object.x-char kind))
			    (cons (object.x-attr kind)
				  (object.x-char kind))))))
		 (t
		  ;; no objects
		  nil))))
	
	;; otherwise something else
	(t
	 (let* ((feature-id (cave-feature dungeon x y))
		(feature-obj (get-feature feature-id)))
	   (cons (feature.x-attr feature-obj)
		 (feature.x-char feature-obj))))))

(defun map-info (dungeon x y)
  
  ;; maybe get the coord in one go..
  (let ((mon (cave-monsters dungeon x y))
	(feat (cave-feature dungeon x y))
	(flags (cave-flags dungeon x y))
	(obj (cave-objects dungeon x y))
	(f-obj nil)
	(ret-obj nil))

    ;; skip hallucination
    
    ;; boring grids
    (cond ((<= feat +feature-invisible+)
	   ;; something we see
	   (cond ((or (bit-flag-set? flags +cave-seen+)
		     (bit-flag-set? flags +cave-view+))

		  (setf f-obj (get-feature +feature-floor+))
		  (setf ret-obj (cons (feature.x-attr f-obj)
				      (feature.x-char f-obj)))

		    ;; do tricky handling here
		    
		  )

		 
		 ;; otherwise darkness
		 (t
		  (setf f-obj (get-feature +feature-none+))
		  (setf ret-obj (cons (feature.x-attr f-obj)
				      (feature.x-char f-obj)))
		  ))
	     
	   )
	  ;; interesting grids
	  (t

	   (cond ((bit-flag-set? flags +cave-mark+)
		  (setf f-obj (get-feature feat))
		  (setf ret-obj (cons (feature.x-attr f-obj)
				      (feature.x-char f-obj))))

		 ;; not noted
		 (t
		  (setf f-obj (get-feature +feature-none+))
		  (setf ret-obj (cons (feature.x-attr f-obj)
				      (feature.x-char f-obj))))
		 
		) 
	   
	   ))

    ;; let's see if any objects are on top

    (when (and obj (typep obj 'item-table) (or (bit-flag-set? flags +cave-seen+)
					       (bit-flag-set? flags +cave-view+)))
      ;; do we have objects..
      (if (> (items.cur-size obj) 1)
	  ;; pile symbol
	  (setf ret-obj (cons +term-white+ #\&))
	  ;; single object
	  (let* ((kind (aobj.kind (item-table-find obj 0)))
		 (flavour (object.flavour kind)))
	    (if flavour
		(setf ret-obj (cons (cadr flavour)
				    (object.x-char kind)))
		(setf ret-obj (cons (object.x-attr kind)
				    (object.x-char kind)))))))

    
    ;; do we have monsters?
    (when (and mon (or (bit-flag-set? flags +cave-seen+)
		       (bit-flag-set? flags +cave-view+)))
      (let ((kind (amon.kind (car mon))))
	(setf ret-obj (cons (monster.colour kind)
			    (monster.symbol kind)))))

    
    ;; remove this entry later..
    (when (and (eql x (player.loc-x *player*))
	       (eql y (player.loc-y *player*)))
;;      (warn "returning player at {~a,~a}" x y)
      (setf ret-obj (cons +term-white+ #\@)))

    
    
    ret-obj))

(defun cave-is-room? (dungeon x y)
  (bit-flag-set? (cave-flags dungeon x y)
	     +cave-room+))

(defun cave-boldly-naked? (dungeon x y)
  (and (= (cave-feature dungeon x y) +feature-floor+)
       (eq nil (cave-objects dungeon x y))
       (eq nil (cave-monsters dungeon x y))))

(defun cave-floor-bold? (dungeon x y)
  (not (bit-flag-set? (cave-flags dungeon x y)
		  +cave-wall+)))

(defun cave-icky? (dungeon x y)
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-icky+))


(defun place-player! (dungeon player x y)
  (declare (ignore dungeon))
  ;; fix me later
  (setf (player.loc-y player) y)
  (setf (player.loc-x player) x)

  t)


(defun move-viewport! (dungeon player direction)
  "moves the viewport in a direction"
;;  (warn "moving viewport..")
  
  (let* ((vp-jump 12)
	 (dungeon-height (dungeon.height dungeon))
	 (dungeon-width (dungeon.width dungeon))
	 (cur-view-x (player.view-x player))
	 (cur-view-y (player.view-y player))
	 (wanted-x cur-view-x)
	 (wanted-y cur-view-y))
    
    (case direction
      (8 (decf wanted-y vp-jump))
      (6 (incf wanted-x vp-jump))
      (2 (incf wanted-y vp-jump))
      (4 (decf wanted-x vp-jump))
      (otherwise (warn "Unknown direction")))

    ;; don't go left or top
    (when (< wanted-x 0) (setq wanted-x 0))
    (when (< wanted-y 0) (setq wanted-y 0)) 

    ;; don't go right or bottom
    (let ((max-x (- dungeon-width +screen-width+))
	  (max-y (- dungeon-height +screen-height+)))
      
      (when (> wanted-x max-x) (setq wanted-x max-x))
      (when (> wanted-y max-y) (setq wanted-y max-y)))

    (setf (player.view-x player) wanted-x
	  (player.view-y player) wanted-y)

    ;; redraw
    (print-map dungeon player)
    
    ))
   

(defun print-map (dungeon player)
  "Prints a map of the given dungeon to the screen"
  
  (let ((ty (+ +start-row-of-map+ +screen-height+))
	(tx (+ +start-column-of-map+ +screen-width+))
;;	(dungeon-height (dungeon.height dungeon))
;;	(dungeon-width (dungeon.width dungeon))
	)

    (declare (type fixnum ty tx))

    (warn "printing map")

    
    (loop for y of-type fixnum from (player.view-y player)
	  for vy of-type fixnum from +start-row-of-map+ to (the fixnum (1- ty))
	  do
	  
	  (loop for x of-type fixnum from (player.view-x player)
		for vx of-type fixnum from +start-column-of-map+ to (the fixnum (1- tx))
		do
		(let ((point-info (map-info dungeon x y)))
;;		  (warn "Q at ~a ~a with ~a" vx vy (cdr point-info))
		  (%loc-queue-cons vx vy point-info))))



    ;; add a printing of depth here.. remove later
    (print-depth (player.depth *player*))
    ))

(defun %loc-queue-cons (vx vy point-info)
  (c-term-queue-char vx vy
		     (car point-info)
		     #+allegro
		     (cdr point-info)
		     #+cmu
		     (char-code (cdr point-info))
		     ))

(defun verify-panel (dungeon pl)

  "verifies that the panel is correct and scrolls as needed"
  
  (let* ((d-y (player.view-y pl))
	 (d-x (player.view-x pl))
	 (dungeon-height (dungeon.height dungeon))
	 (dungeon-width (dungeon.width dungeon))
	 (pl-depth (player.depth pl))
	 (new-y d-y)
	 (new-x d-x)
	 (p-y (player.loc-y pl))
	 (p-x (player.loc-x pl))
	 (scroll-p nil))

    (declare (type fixnum d-y d-x new-y new-x p-y p-x pl-depth))
    ;; skip center-code

    (when (or (< p-y (+ d-y 2))
	      (>= p-y (+ d-y +screen-height+ -2)))
      
      (setq new-y (the fixnum (* (int-/ (the fixnum (- p-y (int-/ +panel-height+ 2)))
					+panel-height+)
				 +panel-height+)))

      (if (< new-y 0)
	(setq new-y 0)
	(let ((extreme-point (- dungeon-height +screen-height+)))
	  (when (> new-y extreme-point)
	    (setq new-y extreme-point)))))

    ;; hack for town
    (when (= 0 pl-depth)
      (setq new-y +screen-height+))

    (unless (= d-y new-y)
      (setf (player.view-y pl) new-y)
      (setq scroll-p t))


    ;; skipping center
    (when (or (< p-x (the fixnum (+ d-x 4)))
	      (>= p-x (the fixnum (+ d-x +screen-width+ -4))))
	  
      (setq new-x (* (int-/ (- p-x (int-/ +panel-width+ 2))
			+panel-width+)
		     +panel-width+))
      
      (if (< new-x 0)
	(setq new-x 0)
	(let ((extreme-point (- dungeon-width +screen-width+)))
	  (when (> new-x extreme-point)
	    (setq new-x extreme-point)))))
    
    ;; hack for town
    (when (= 0 (player.depth pl))
      (setq new-x +screen-width+))

    ;; add x
    
    (unless (= d-x new-x)
      (setf (player.view-x pl) new-x)
      (setq scroll-p t))


    (when scroll-p
      (bit-flag-add! *redraw* +print-map+)
      ;; more stuff
      )
    
    t))

(defun distance (x1 y1 x2 y2)
  "returns a fixnum"
  (declare (type fixnum x1 x2 y1 y2))
  (declare (optimize (speed 3) (safety 0)))
  
  (let ((ay (if (> y1 y2)
		(the fixnum (- y1 y2))
		(the fixnum (- y2 y1))))
	(ax (if (> x1 x2)
		(the fixnum (- x1 x2))
		(the fixnum (- x2 x1)))))
    
    (declare (type fixnum ax ay))
    
    (if (> ay ax)
	(the fixnum (+ ay (the fixnum (int-/ ax 2))))
	(the fixnum (+ ax (the fixnum (int-/ ay 2)))))
    ))
	     
  

(defun note-spot! (dungeon x y)
  "noting the spot.."

  (let* ((coord (cave-coord dungeon x y))
	 (flag (coord.flags coord)))

    ;; require it to be seen
    (when (bit-flag-set? flag +cave-seen+)

      ;; skip obj
      
      (unless (bit-flag-set? flag +cave-mark+)
	(let ((feat (coord.feature coord)))
	  (if (<= feat +feature-invisible+)
	      ;; skip save of certain floors
	      nil
	      (bit-flag-add! (coord.flags coord) +cave-mark+))))
  
      )))

(defun light-spot! (dungeon x y)
  "lighting up the spot.."

  (let* ((info (map-info dungeon x y))
	 (pvx (player.view-x *player*))
	 (pvy (player.view-y *player*))
	 (kx (- x pvx))
	 (ky (- y pvy)))

    #||
    ;; debugging
    (when (or (minusp kx)
	      (minusp ky))
      (error "a value became negative.."))
    ||#
    
    ;, sometimes we cross a screen
    (when (and (<= 0 kx)
	       (<= 0 ky)
	       (< ky +screen-height+)
	       (< kx +screen-width+))
	   
	   (%loc-queue-cons
	    (+ +start-column-of-map+ kx)
	    (+ +start-row-of-map+ ky)
	    info))
    )))





(defun print-map-as-ppm (dungeon fname)
  "prints a PPM file"
  
  (let ((str-orange-colour "2 1 0")
	(str-burgund-colour "1 0 0")
	(str-ltblue-colour "0 1 2")
	(str-dblue-colour "0 0 1")
	(str-white-colour "2 2 2")
	(str-gray-colour "1 1 1")
	(str-black-colour "0 0 0"))
	
    
    (with-open-file (s (pathname fname)
		       :direction :output 
		       :if-exists :supersede)

      (let* ((cnt 0)
	     (jump 0)
	     (sec-jump 0)
	     (dungeon-height (dungeon.height dungeon))
	     (dungeon-width (dungeon.width dungeon))
	     (arr (make-array (* 4 dungeon-width dungeon-height))))

	(format s "P3 ~a ~a 2~%" (* 2 dungeon-width) (* 2 dungeon-height))
	
	(loop for y from 0 to (1- dungeon-height)
	      do
	      ;;    (when (> y 0) (format s "~%"))
	      (setq jump (* 4 y dungeon-width))
	      (setq sec-jump (+ jump (* 2 dungeon-width)))
	      (setq cnt 0)
	      ;;	    (warn "jump is ~a and will also set ~a" jump sec-jump)
	    
	      (loop for x from 0 to (1- dungeon-width)
		    do
		    (let* ((point-info (map-info dungeon x y))
			   (val (case (cdr point-info)
				  (#\# (if (eql +feature-secret+
						(cave-feature dungeon x y))
					   str-orange-colour
					   str-gray-colour))
				  (#\. str-white-colour)
				  (#\+ str-burgund-colour)
				  (#\@ str-dblue-colour)
				  (#\' str-ltblue-colour)
				  (otherwise "0 1 0"))))
		    
		      ;;	  (warn "Q at ~a ~a with ~a" vx vy (cdr point-info))
		      ;; set x,y
		      (setf (svref arr (+ jump cnt)) val)
		      ;; set x, y+1
		      (setf (svref arr (+ sec-jump cnt)) val)

		      (incf cnt)
		    
		      ;; set x+1, y
		      (setf (svref arr (+ jump cnt)) val)
		      ;; set x+1, y+1
		      (setf (svref arr (+ sec-jump cnt)) val)

		      (incf cnt)
		    
		      )))

      
	(loop for x across arr
	      for i from 0
	      do
	      (if (stringp x)
		  (format s "~a~%" x)
		  (warn "not number at ~a" i)))
      
	))      

    ))


(defun print-map-to-file (dungeon fname)
  ;; print ascii
    
  (with-open-file (s (pathname fname)
		     :direction :output 
		     :if-exists :supersede)
    (let ((dungeon-height (dungeon.height dungeon))
	  (dungeon-width (dungeon.width dungeon)))
      
	  (loop for y from 0 to (1- dungeon-height)
		do
		(when (> y 0) (format s "~%"))
	    
		(loop for x from 0 to (1- dungeon-width)
		      do
		      (let ((point-info (map-info dungeon x y)))
			(format s "~a" (cdr point-info)))))
	  (format s "~%"))))

