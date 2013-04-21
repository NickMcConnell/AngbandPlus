;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: dungeon.lisp - basic code for the dungeon
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

---
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
  (height 0 :type u-fixnum)
  (width  0 :type u-fixnum)

  (up-stairs-p nil)
  (down-stairs-p nil)

  (depth nil)
  (monsters nil)
  (rooms nil) ;; unused
  (triggers nil)
  )
  


(defun invoke-on-dungeon (dungeon fun)
  "calls given FUN with three arguments:
a COORD
the X-coordinate of the COORD
the Y-coordinate of the COORD
"
  (let ((table (dungeon.table dungeon)))

    (dotimes (y (dungeon.height dungeon))
      (dotimes (x (dungeon.width dungeon))
	(funcall fun (aref table x y) x y)))
    ))


(defmacro with-dungeon (parameters &body code)
  "a WITH- construct.  parameters should be: (dungeon-variable (arg-names to INVOKE))."

 `(invoke-on-dungeon ,(car parameters) #'(lambda ,(cadr parameters) ,@code))) 

(defun invoke-on-dungeon-monsters (dun fun)
  "Calls FUN with TWO arguments:
the dungeon object
the monster
"

  (let ((monster-list (dungeon.monsters dun)))
    (dolist (i monster-list)
      (when (creature-alive? i)
	(funcall fun dun i)))))
	
(defmacro with-dungeon-monsters ((dungeon-var monster-var) &body code)
  `(invoke-on-dungeon-monsters ,dungeon-var #'(lambda (,dungeon-var ,monster-var) ,@code)))


(defun create-dungeon (width height &key its-depth) 
  "Creates and returns a dungeon of specified size"

;;  (warn "Creating dungeon of size [~s ~s]" width height)
  
  (let ((d (make-dungeon :height height :width width))
	(table (make-array (list width height))))
    
;;    (setf (dungeon.height d) height)
;;    (setf (dungeon.width d) width)
    
    (dotimes (j (dungeon.width d))
      (dotimes (i (dungeon.height d))
	(setf (aref table j i) (make-dungeon-coord))))

    (when its-depth
      (setf (dungeon.depth d) its-depth))
    
    (setf (dungeon.table d) table)
    
    d))

;; hackish
(defun (setf coord-feature) (val coord)
  "this is an evil hack."

  ;; fix me
  (setf (coord.feature coord) val)
  
  (if (>= val +feature-door-head+)
      ;; this is a wall or a door..
      (bit-flag-add! (coord.flags coord) +cave-wall+)
      ;; we're not a a wall or a door.. 
      (bit-flag-remove! (coord.flags coord) +cave-wall+)
      )

  )

(defun clean-coord! (coord)
  "Cleands the given coordinate."
  
  (setf (coord-feature  coord) 0
	(coord.flags    coord) 0
	(coord.objects  coord) nil
	(coord.monsters coord) nil)
  coord)



(defun clean-dungeon! (dungeon)
  "Clears all flags and everything from the dungeon."

  (warn "!!! cleaning dungeon..")
  
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (clean-coord! coord))

  (setf (dungeon.rooms dungeon) nil
	(dungeon.monsters dungeon) nil
	(dungeon.triggers dungeon) nil)

  dungeon)


(defun make-map (dungeon)
  "Returns a map based on a given dungeon."
  (make-array (list (dungeon.width dungeon)
		    (dungeon.height dungeon))
	      :element-type 'fixnum :initial-element 0))

(defun cave-coord (dungeon x y)
  (assert (and (< x (dungeon.width dungeon))
	       (< y (dungeon.height dungeon))))
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
;;  (setf (dungeon.monsters dungeon) (remove monster (dungeon.monsters dungeon)))
  (warn "Deleting monster ~a from ~a" monster (dungeon.monsters dungeon)))

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

    ;; hack
    (setf (coord-feature coord) val)

  
  ;; redraw
  ))



(defun fill-dungeon-with-feature! (dungeon feature)
  "Cleans and then fills the dungeon with a given feature.
Returns nothing."
  
;;  (warn "filling up dungeon with feature ~s.." feature)

  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (clean-coord! coord)
    (setf (coord-feature coord) feature))

  ;; check
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (assert (and (= (coord.feature coord) feature)
		 ;;(= (coord.flags coord) 0)
		 (eq (coord.monsters coord) nil)
		 (eq (coord.objects coord) nil))))
  
  (values))

(defun fill-dungeon-part-with-feature! (dungeon feature width-range height-range)
  "height-range and width-range should be conses where
car is start and cdr is the non-included end  (ie [start, end> )"

  #-allegro
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

(defun map-info (dungeon x y)
  "Returns two values, attr and char, use M-V-B to get the values."
  
  ;; maybe get the coord in one go..
  (let* (;;(coord (cave-coord
	 (mon (cave-monsters dungeon x y))
	 (feat (cave-feature dungeon x y))
	 (flags (cave-flags dungeon x y))
	 (obj (cave-objects dungeon x y))
	 (f-obj nil)
	 (ret-attr +term-white+)
	 (ret-char #\X)
	 ;;(name-return nil)
	 )


    ;; skip hallucination
    
    ;; boring grids
    (cond ((<= feat +feature-invisible+)
	   ;; something we see
	   (cond ((or (bit-flag-set? flags +cave-mark+)
		      (bit-flag-set? flags +cave-seen+))

		  (setf f-obj (get-feature +feature-floor+))
		  (setf ret-attr (feature.x-attr f-obj)
			ret-char (feature.x-char f-obj))

		  ;; do tricky handling here
		  ;; torch-light
		  (when (and (eql ret-attr +term-white+)
			     (bit-flag-set? flags +cave-seen+)
			     (not (bit-flag-set? flags +cave-glow+)))
		    (setf ret-attr +term-yellow+))
		  
		  )

		 
		 ;; otherwise darkness
		 (t
		  (setf f-obj (get-feature +feature-none+))
		  (setf ret-attr (feature.x-attr f-obj)
			ret-char (feature.x-char f-obj))
		  ))
	     
	   )
	  ;; interesting grids
	  (t

	   (cond ((bit-flag-set? flags +cave-mark+)
		  (setf f-obj (get-feature feat))
		  (setf ret-attr (feature.x-attr f-obj)
			ret-char (feature.x-char f-obj)))

		 ;; not noted
		 (t
		  (setf f-obj (get-feature +feature-none+))
		  (setf ret-attr (feature.x-attr f-obj)
			ret-char (feature.x-char f-obj)))
		 
		)
	   
	   ))

    ;; let's see if any objects are on top

    (when (and obj (typep obj 'item-table) (or (bit-flag-set? flags +cave-seen+)
					       (bit-flag-set? flags +cave-view+)))
      ;; do we have objects..
      (if (> (items.cur-size obj) 1)
	  ;; pile symbol
	  (setf ret-attr +term-white+
		ret-char #\&)
	  ;; single object
	  (let* ((kind (aobj.kind (item-table-find obj 0)))
		 (flavour (object.flavour kind)))
	    ;;(warn "Object ~s of kind ~s has flavour ~s" obj kind flavour)
	    ;;(warn "-> (~s,~s) / (~s,~s)"
		;;  (if flavour (cadr flavour) nil) (object.x-char kind)
		;;  (object.x-attr kind) (object.x-char kind) )
	    ;;(setq name-return t)
	    (if flavour
		(setf ret-attr (cadr flavour)
		      ret-char (object.x-char kind))
		(setf ret-attr (object.x-attr kind)
		      ret-char (object.x-char kind)))
	    )))
    
;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))

    
    ;; do we have monsters?
    (when (and mon (and (bit-flag-set? flags +cave-seen+) 
			(bit-flag-set? flags +cave-view+)))
      (let ((kind (amon.kind (car mon))))
	(setf ret-attr (monster.colour kind)
	      ret-char (monster.symbol kind))))

;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))
    
    ;; remove this entry later..
    (when (and (eql x (location-x *player*))
	       (eql y (location-y *player*)))
;;      (warn "returning player at {~a,~a}" x y)
      (setf ret-attr +term-white+
	    ret-char #\@))

;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))
    
    (values ret-attr ret-char)))

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


(defun player-has-los-bold? (dungeon x y)
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-view+))

(defun player-can-see-bold? (dungeon x y)
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-seen+))

(defun place-player! (dungeon player x y)
  (declare (ignore dungeon))
  ;; fix me later
  (setf (location-y player) y
	(location-x player) x)

  player)




(defun print-map (dungeon player)
  "Prints a map of the given dungeon to the screen"
  
  (let ((ty (+ +start-row-of-map+ +screen-height+))
	(tx (+ +start-column-of-map+ +screen-width+))
;;	(dungeon-height (dungeon.height dungeon))
;;	(dungeon-width (dungeon.width dungeon))
	)

    (declare (type fixnum ty tx))

;;    (warn "printing map")
    
    (loop for y of-type fixnum from (player.view-y player)
	  for vy of-type fixnum from +start-row-of-map+ to (the fixnum (1- ty))
	  do
	  
	  (loop for x of-type fixnum from (player.view-x player)
		for vx of-type fixnum from +start-column-of-map+ to (the fixnum (1- tx))
		do
		(multiple-value-bind (the-attr the-char)
		    (map-info dungeon x y)
	  ;;		  (warn "Q at ~a ~a with ~a" vx vy (cdr point-info))
		  (%loc-queue-cons vx vy the-attr the-char))))



    ;; add a printing of depth here.. remove later
    (print-depth *level* nil)
    ))

(defun %loc-queue-cons (vx vy the-attr the-char)
  "Misleading name.. no cons involved anymore."
  (c-term-queue-char! vx vy
		      the-attr
		      #-handle-char-as-num
		      the-char
		      #+handle-char-as-num
		      (char-code the-char)))


(defun verify-panel (dungeon pl)

  "verifies that the panel is correct and scrolls as needed"
  
  (let* ((d-y (player.view-y pl))
	 (d-x (player.view-x pl))
	 (dungeon-height (dungeon.height dungeon))
	 (dungeon-width (dungeon.width dungeon))
	 (pl-depth (player.depth pl))
	 (new-y d-y)
	 (new-x d-x)
	 (p-y (location-y pl))
	 (p-x (location-x pl))
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
  (declare (type u-fixnum x1 x2 y1 y2))
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (let ((ay (if (> y1 y2)
		(the u-fixnum (- y1 y2))
		(the u-fixnum (- y2 y1))))
	(ax (if (> x1 x2)
		(the u-fixnum (- x1 x2))
		(the u-fixnum (- x2 x1)))))
    
    (declare (type u-fixnum ax ay))
    
    (if (> ay ax)
	(the u-fixnum (+ ay (the u-fixnum (int-/ ax 2))))
	(the u-fixnum (+ ax (the u-fixnum (int-/ ay 2)))))
    ))
	     
  

(defun note-spot! (dungeon x y)
  "noting the spot, does not remember objects yet."

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

  (let* ((pvx (player.view-x *player*))
	 (pvy (player.view-y *player*))
	 (kx (- x pvx))
	 (ky (- y pvy)))


    ;; debugging
;;    (when (or (minusp kx) (minusp ky))
;;    (error "a value became negative.."))

    
    ;, sometimes we cross a screen
    (when (and (<= 0 kx)
	       (<= 0 ky)
	       (< ky +screen-height+)
	       (< kx +screen-width+))

      (multiple-value-bind (the-attr the-char)
	  (map-info dungeon x y)
	(%loc-queue-cons (+ +start-column-of-map+ kx)
			 (+ +start-row-of-map+ ky)
			 the-attr the-char)))

    ))


(defun print-map-as-ppm (dungeon fname)
  "prints a PPM file"
  
  (let ((str-orange-colour "2 1 0")
	(str-burgund-colour "1 0 0")
	(str-ltblue-colour "0 1 2")
	(str-dblue-colour "0 0 1")
	(str-white-colour "2 2 2")
	(str-gray-colour "1 1 1")
	;;(str-black-colour "0 0 0")
	)
	
    
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
		    (multiple-value-bind (the-attr the-char)
			(map-info dungeon x y)
		      (declare (ignore the-attr))
		      (let ((val (case the-char
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
		    
		      ))))

      
	(loop for x across arr
	      for i from 0
	      do
	      (if (stringp x)
		  (format s "~a~%" x)
		  (warn "not number at ~a" i)))
      
	))      

    ))


(defun print-map-to-file (dungeon fname)
  "prints the feature-map to a file"
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
		      (let ((point-info (cave-feature dungeon x y)))
			(format s "~a" (feature.x-char (get-feature point-info))))))
	  (format s "~%"))))

(defun get-coord-trigger (dun x y)
  "Tries to find a trigger at a given point."
  (dolist (i (dungeon.triggers dun))
    (let ((place (car i)))
      (when (and (= (car place) x)
		 (= (cdr place) y))
	(return-from get-coord-trigger i))))
  nil)

(defun (setf get-coord-trigger) (val dun x y)
  "Adds a trigger to the given dungeon."
  (dolist (i (dungeon.triggers dun))
    (let ((place (car i)))
      (when (and (= (car place) x)
		 (= (cdr place) y))
	(setf (cdr i) val)
	(return-from get-coord-trigger i))))
  
  (push (cons (cons x y) val) (dungeon.triggers dun)))


(defun apply-possible-coord-trigger (dun x y)
  "This is a hack.. fix me later.."
  (let ((trigger (get-coord-trigger dun x y)))
    (when (and trigger (consp trigger) (functionp (cdr trigger)))
      (funcall (cdr trigger) dun x y))))

(defun put-cursor-relative! (dun x y)
  "Tries to put the cursor relative to the window."
  (declare (ignore dun))
  
  (let* ((pl *player*)
	 (pwy (player.view-y pl)) 
	 (ky (- y pwy))
	 (pwx (player.view-x pl))
	 (kx (- x pwx)))
    
    (when (and (<= 0 kx)
               (<= 0 ky)
               (< ky +screen-height+)
               (< kx +screen-width+))

      (c-term-gotoxy! (+ +start-column-of-map+ kx)
		      (+ +start-row-of-map+ ky)))))


	 
