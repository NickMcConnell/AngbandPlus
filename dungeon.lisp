;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: dungeon.lisp - basic code for the dungeon
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

---
ADD_DESC: Simple code to access the dungeon object(s) 

|#

(in-package :org.langband.engine)


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

(defun invoke-on-dungeon-monsters (dungeon fun)
  "Calls FUN with TWO arguments:
the dungeon object
the monster
"

  (let ((monster-list (dungeon.monsters dungeon)))
    (dolist (i monster-list)
      (when (creature-alive? i)
	(funcall fun dungeon i)))))
	
(defmacro with-dungeon-monsters ((dungeon-var monster-var) &body code)
  `(invoke-on-dungeon-monsters ,dungeon-var
    #'(lambda (,dungeon-var ,monster-var) ,@code)))

(defun legal-coord? (dungeon x y)
  (and (> x 0)
       (> y 0)
       (< x (dungeon.width dungeon))
       (< y (dungeon.height dungeon))))


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
(defun (setf coord-floor) (val coord)
  "this is an evil hack."

  ;; fix me
  (setf (coord.floor coord) val)
  
  (if (>= val +floor-door-head+)
      ;; this is a wall or a door..
      (bit-flag-add! (coord.flags coord) +cave-wall+)
      ;; we're not a a wall or a door.. 
      (bit-flag-remove! (coord.flags coord) +cave-wall+)
      )
  )

(defun clean-coord! (coord)
  "Cleans the given coordinate."
  
  (setf (coord-floor    coord) 0
	(coord.flags    coord) 0
	(coord.objects  coord) nil
	(coord.monsters coord) nil
	(coord.decor    coord) nil)
  coord)



(defun clean-dungeon! (dungeon)
  "Clears all flags and everything from the dungeon."

;;  (warn "!!! cleaning dungeon..")
  
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

#||
Warning:
apparently ACL expands e.g (setf (cave-flags dungeon x y) foo)
with CAVE-fLAGS first as a macro, ending up with
(setf (coord ....) foo)
and not using the (SETF CAVE-FLAGS) function which does some
extra tricks.
||#

#+compiler-that-inlines
(defsubst cave-coord (dungeon x y)
  (declare (type fixnum x y))
;;  (assert (and (< x (dungeon.width dungeon))
;;	       (< y (dungeon.height dungeon))))
  (aref (dungeon.table dungeon) x y))

#-compiler-that-inlines
(defmacro cave-coord (dungeon x y)
  `(aref (dungeon.table ,dungeon) ,x ,y))

;;#+compiler-that-inlines
(defun cave-floor (dungeon x y)
  (declare (type fixnum x y))
  (coord.floor (cave-coord dungeon x y)))

(defun cave-decor (dungeon x y)
  (declare (type fixnum x y))
  (coord.decor (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-floor (dungeon x y)
;;  `(coord.floor (cave-coord ,dungeon ,x ,y)))

;;#+compiler-that-inlines
(defun cave-flags (dungeon x y)
  (declare (type fixnum x y))
  (coord.flags (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-flags (dungeon x y)
;;  `(coord.flags (cave-coord ,dungeon ,x ,y)))

;;#+compiler-that-inlines
(defun cave-objects (dungeon x y)
  (declare (type fixnum x y))
  (coord.objects (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-objects (dungeon x y)
;;  `(coord.objects (cave-coord ,dungeon ,x ,y)))

;;#+compiler-that-inlines
(defun cave-monsters (dungeon x y)
  (declare (type fixnum x y))
  (coord.monsters (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-monsters (dungeon x y)
;;  `(coord.monsters (cave-coord ,dungeon ,x ,y)))

(defun (setf cave-monsters) (val dungeon x y)
  (declare (type fixnum x y))
  ;; add smart stuff later..
  (setf (coord.monsters (aref (dungeon.table dungeon) x y)) (if (listp val)
								val
								(list val))))


(defun delete-monster! (dungeon monster)
  "Wipes the monster.."
;;  (setf (dungeon.monsters dungeon) (remove monster (dungeon.monsters dungeon)))
  (warn "Deleting monster ~a from ~a" monster (dungeon.monsters dungeon)))

(defun (setf cave-objects) (val dungeon x y)
  (setf (coord.objects (aref (dungeon.table dungeon) x y)) val))

(defun (setf cave-flags) (val dungeon x y)
  (setf (coord.flags (aref (dungeon.table dungeon) x y)) val))

(defun (setf cave-decor) (val dungeon x y)
  (setf (coord.decor (aref (dungeon.table dungeon) x y)) val))


(defun (setf cave-floor) (val dungeon x y)
;;  (warn "Setting ~a ~a to ~a" x y val)
  (declare (type fixnum val))
  (let ((coord (aref (dungeon.table dungeon) x y)))

    ;; hack
    (setf (coord-floor coord) val)
    
    (when (dungeon.active dungeon)
      (light-spot! dungeon x y)
      (note-spot! dungeon x y))

  
  ;; redraw
  ))



(defun fill-dungeon-with-floor! (dungeon floor)
  "Cleans and then fills the dungeon with a given floor.
Returns nothing."
  
;;  (warn "filling up dungeon with floor ~s.." floor)

;;  (check-type floor floor-type)
  
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (clean-coord! coord)
    (setf (coord-floor coord) floor))

  ;; check
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (assert (and (= (coord.floor coord) floor)
		 ;;(= (coord.flags coord) 0)
		 (eq (coord.monsters coord) nil)
		 (eq (coord.objects coord) nil))))
  
  (values))

(defun fill-dungeon-part-with-floor! (dungeon floor width-range height-range)
  "height-range and width-range should be conses where
car is start and cdr is the non-included end  (ie [start, end> )"

  #-allegro
  (declare (type (cons fixnum fixnum) width-range height-range))
  
;;  (warn "Filling h: ~a and w: ~a" height-range width-range)
  
  (loop for i of-type fixnum from (car height-range)
	      below (the fixnum (cdr height-range))
	do
	(loop for j of-type fixnum from (car width-range)
	      below (the fixnum (cdr width-range))
	      do
	      (setf (cave-floor dungeon j i) floor)))
  (values))

(defun map-info (dungeon x y)
  "Returns two values, attr and char, use M-V-B to get the values."
  (declare (type fixnum x y))
  ;; maybe get the coord in one go..
  (let* ((coord (cave-coord dungeon x y)) ;; faster
	 (mon (coord.monsters coord))
	 (feat (coord.floor coord))
	 (flags (coord.flags coord))
	 (obj-table (coord.objects coord))
	 (f-obj nil)
	 (decor (coord.decor coord))
	 (ret-attr +term-white+)
	 (ret-char #\X)
	 (pl-obj *player*)
	 (trans-attr 0)
	 (trans-char 0)
	 ;;(name-return nil)
	 )

    ;; hackish, fix later
    (when (consp mon)
      (setf mon (car mon)))
    
    ;; skip hallucination
    (cond ;; places with decor
	  ((and (typep decor 'decor) (decor.visible? decor))
	   ;; hackish
	   (setf f-obj (get-floor-type feat)) ;; ok?
	   (setf ret-attr (x-attr decor)
		 ret-char (x-char decor))
	   )
	  ;; boring grids
	  ((<= feat +floor-invisible-trap+)
	   ;; something we see
	   (cond ((or (bit-flag-set? flags +cave-mark+)
		      (bit-flag-set? flags +cave-seen+))

		  (setf f-obj (get-floor-type +floor-regular+))
		  (setf ret-attr (x-attr f-obj)
			ret-char (x-char f-obj))

		  ;; do tricky handling here
		  ;; torch-light
		  (when (and (eql ret-attr +term-white+)
			     (bit-flag-set? flags +cave-seen+)
			     (not (bit-flag-set? flags +cave-glow+)))
		    (setf ret-attr +term-yellow+))
		  
		  )

		 
		 ;; otherwise darkness
		 (t
		  (setf f-obj (get-floor-type +floor-none+))
		  (setf ret-attr (x-attr f-obj)
			ret-char (x-char f-obj))
		  ))
	     
	   )
	  
		
	  ;; interesting grids
	  (t

	   (cond ((bit-flag-set? flags +cave-mark+)
		  (setf f-obj (get-floor-type feat))
		  (setf ret-attr (x-attr f-obj)
			ret-char (x-char f-obj)))

		 ;; not noted
		 (t
		  (setf f-obj (get-floor-type +floor-none+))
		  (setf ret-attr (x-attr f-obj)
			ret-char (x-char f-obj)))
		 
		)
	   
	   ))

    ;; hackish save of transparency
    (setf trans-attr ret-attr
	  trans-char ret-char)
    
    ;; let's see if any objects are on top

    (when (and obj-table (typep obj-table 'item-table))

      (let (;;(objs (items.objs obj-table))
	    (obj-len (items.cur-size obj-table))
	    (first-obj (item-table-find obj-table 0)))

	(cond ((and (> obj-len 1) (aobj.marked first-obj))
	       ;; pile symbol
	       (setf ret-attr +term-white+
		     ret-char #.(char-code #\&)))
	      ((aobj.marked first-obj)
	       ;; single object
	       (let* ((kind (aobj.kind first-obj))
		      (flavour (object.flavour kind)))
		 ;;(warn "Object ~s of kind ~s has flavour ~s" first-obj kind flavour)
		 ;;(warn "-> (~s,~s) / (~s,~s)"
		 ;;  (if flavour (cadr flavour) nil) (x-char kind)
		 ;;  (x-attr kind) (x-char kind) )
		 ;;(setq name-return t)
		 (if flavour
		     (setf ret-attr (x-attr flavour)
			   ret-char (x-char flavour))
		     (setf ret-attr (x-attr kind)
			   ret-char (x-char kind)))
		 ))
	      (t
	       ))
	))
    
    
	  
;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))

    
    ;; do we have monsters and can see it?
    (when (and mon (amon.seen-by-player? mon))
      (let* ((kind (amon.kind mon))
	     (wanted-attr (x-attr kind))
	     (wanted-char (x-char kind))
	     )

	(cond (nil
	       ;; skip hallucinate
	       )
	      ;; skip avoid-other ???, option that's not supported
	      
	      ;; bizarre monster-attr and char
	      ((and (>= wanted-attr +graphics-start+)
		    (>= wanted-char +graphics-start+))
	       ;;(warn "Fell to double.. ~s ~s" wanted-attr wanted-char)
	       (setf ret-attr wanted-attr
		     ret-char wanted-char))
	      
	      ;; multi-hued monster
	      ((has-ability? kind '<colour-changing>)
	       (setf ret-attr (charify-number (randint 15))
		     ret-char wanted-char))

	      ;; normal monster, not clear in any way
	      ((not (or (has-ability? kind '<see-through>)
			(has-ability? kind '<absorbs-symbol>)))
	       (setf ret-attr wanted-attr
		     ret-char wanted-char))
	      
	      ;; bizarre grid under
	      ((or (>= ret-attr +graphics-start+)
		   (>= ret-char +graphics-start+))
	       ;;(warn "bizarre grid ~s  ~s" ret-attr ret-char)
	       (setf ret-attr wanted-attr
		     ret-char wanted-char))
	      
	      ;; normal char, clear attr
	      ((not (has-ability? kind '<absorbs-symbol>))
	       (setf ret-char wanted-char))
	      ;; normal attr, clear char
	      ((not (has-ability? kind '<see-through>))
	       (setf ret-attr wanted-attr))
	      (t
	       (warn "Fell through monster-check for monster ~s" (monster.name kind)))
	      
	      )))

;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))
    
    ;; remove this entry later..
    (when (and pl-obj ;; only when we have a player
	       (eql x (location-x pl-obj))
	       (eql y (location-y pl-obj)))
;;      (warn "returning player at {~a,~a}" x y)
      (if *use-graphics*
	  (setf ret-attr (+ +graphics-start+ 6)
		ret-char (+ +graphics-start+ 7))
	  (setf ret-attr (x-attr pl-obj)
		ret-char (x-char pl-obj))))

;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))
    
    (values ret-attr ret-char trans-attr trans-char)))

(defun cave-is-room? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-room+))

(defun cave-boldly-naked? (dungeon x y)
  (declare (type fixnum x y))
  (let ((coord (cave-coord dungeon x y)))
    (and (= (coord.floor coord) +floor-regular+)
	 (eq nil (coord.objects coord))
	 (eq nil (coord.monsters coord)))))

(defun cave-floor-bold? (dungeon x y)
  (declare (type fixnum x y))
  (not (bit-flag-set? (cave-flags dungeon x y)
		      +cave-wall+)))

(defun cave-empty-bold? (dungeon x y)
  (and (cave-floor-bold? dungeon x y)
       (eq (cave-monsters dungeon x y) nil)))

(defun cave-icky? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-icky+))


(defun player-has-los-bold? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-view+))

(defun player-can-see-bold? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-seen+))

(defun in-bounds? (dungeon x y)
  "Checks that the coordinate is well within the dungeon"
  (declare (type fixnum x y))
  ;; strict check, fails some places
;;  (legal-coord? dungeon x y)
  ;; liberal check
  (and (< y (dungeon.height dungeon))
       (< x (dungeon.width dungeon)))
  )



(defun in-bounds-fully? (dungeon x y)
  "Checks that the coordinate is well within the dungeon"
  (declare (type fixnum x y))
  (and (> x 0)
       (> y 0)
       (< x (1- (dungeon.width dungeon)))
       (< y (1- (dungeon.height dungeon)))))


(defun is-closed-door? (dungeon x y)
  (let ((feat (cave-floor dungeon x y)))
    (and (>= feat +floor-door-head+)
	 (< feat +floor-door-tail+))))

;; hackish.. not very good
(defun is-open-door? (dungeon x y)
  (let ((feat (cave-floor dungeon x y)))
    (= feat +floor-open-door+)))


(defun panel-contains? (player x y)
  "Returns T if the panel contains the coordinate."
  (let ((pvx (player.view-x player))
	(pvy (player.view-y player))
	(screen-width *screen-width*)
	(screen-height *screen-height*))
	
    (and (< (- x pvx) screen-width)
	 (< (- y pvy) screen-height))))


(defun place-player! (dungeon player x y)
  (declare (ignore dungeon))
  (declare (type fixnum x y))
  ;; fix me later
  (setf (location-y player) y
	(location-x player) x)

  player)


(defun print-map (dungeon player)
  "Prints a map of the given dungeon to the screen"
;;  (declare (optimize (safety 0) (speed 3) (debug 0)
;;		     #+cmu (ext:inhibit-warnings 3)))


  (let* (;;(*player* player)
	 (wy (player.view-y player))
	 (wx (player.view-x player))
	 (ty (+ wy *screen-height*))
	 (tx (+ wx *screen-width*))
	
;;	(dungeon-height (dungeon.height dungeon))
;;	(dungeon-width (dungeon.width dungeon))
	)

    (declare (type fixnum ty tx))

;;    (warn "printing map (~s ~s)" (location-x player) (location-y player))

    (assert (eq player *player*))
    
    (loop for vy of-type fixnum from +start-row-of-map+
	  for y of-type fixnum from wy below ty
	  do
	  
	  (loop for vx of-type fixnum from +start-column-of-map+
		for x of-type fixnum from wx below tx
		do
		(when (in-bounds? dungeon x y)
		  (multiple-value-bind (the-attr the-char trans-attr trans-char)
		      (map-info dungeon x y)
		    (when *use-graphics* ;; remove when stable
		      (assert (oddp vx)))
		    (c-term-queue-char! vx vy the-attr the-char trans-attr trans-char)
		    (when *use-graphics*
			(incf vx)
			(if (>= the-attr +graphics-start+)
			    (c-term-queue-char! vx vy -1 -1 0 0)
			    (c-term-queue-char! vx vy +term-white+ #.(char-code #\Space)
						+term-white+ #.(char-code #\Space))))
		    )))
	  )))


(defun modify-panel! (dungeon player vx vy)
  "maybe let variants override this one to avoid the town-hack below?"
  (let ((town-width 66)
	(town-height 22) ;; hacks
	(pl-depth (dungeon.depth dungeon))
	(screen-height *screen-height*)
	(screen-width *screen-width*)
	(dun-height (dungeon.height dungeon))
	(dun-width (dungeon.width dungeon)))
  
    (cond ((= pl-depth 0)
	   (cond ((> vy (- town-height screen-height)) ;; hack
		  (setf vy (- town-height screen-height)))
		 ((minusp vy)
		  (setf vy 0))))
	  ((> vy (- dun-height screen-height))
	   (setf vy (- dun-height screen-height))))
  
    (cond ((= pl-depth 0)
	   (cond ((> vx (- town-width screen-width)) ;; hack
		  (setf vx (- town-width screen-width)))
		 ((minusp vx)
		  (setf vx 0))))
	
	  ((> vx (- dun-width screen-width))
	   (setf vx (- dun-width screen-width))))

    (when (minusp vy)
      (setf vy 0))

    (when (minusp vx)
      (setf vx 0))

    (when (or (/= (player.view-x player) vx)
	      (/= (player.view-y player) vy))
      ;;    (warn "modify panel to ~s ~s" vx vy)
      (setf (player.view-x player) vx
	    (player.view-y player) vy)
      (bit-flag-add! *redraw* +print-map+)
      ;; skip window

      t)
    ))


(defun verify-panel (dungeon player)
  "verifies that the panel is correct and scrolls as needed"

  (let ((p-y (location-y player))
	(p-x (location-x player))
	(v-y (player.view-y player))
	(v-x (player.view-x player))
	(screen-height *screen-height*)
	(screen-width *screen-width*)
	(panel-height *panel-height*)
	(panel-width *panel-width*)
	(centre-on-player nil)
	)

    ;; hackish, update when running arrives
    (cond ((and centre-on-player
		(/= p-y (+ v-y (int-/ screen-height 2))))
	   (setf v-y (- p-y (int-/ screen-height 2))))
	  
	  ((or (< p-y (+ v-y 2))
	       (>= p-y (+ v-y screen-height -2)))
	   (setf v-y (* (int-/ (- p-y (int-/ panel-height 2))
			       panel-height)
			panel-height))))

    (cond ((and centre-on-player
		(/= p-x (+ v-x (int-/ screen-width 2))))
	   (setf v-x (- p-x (int-/ screen-width 2))))
	  
	  ((or (< p-x (+ v-x 4))
	       (>= p-x (+ v-x screen-width -4)))
	   (setf v-x (* (int-/ (- p-x (int-/ panel-width 2))
			       panel-width)
			panel-width))))

    (modify-panel! dungeon player v-x v-y)))



(defun distance (x1 y1 x2 y2)
  "returns a fixnum"
  (declare   (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3))
	     (type fixnum x1 x2 y1 y2))
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
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
  "noting the spot."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3))
	   (type fixnum x y))

  (let* ((coord (cave-coord dungeon x y))
	 (flag (coord.flags coord)))

    ;; require it to be seen
    (when (bit-flag-set? flag +cave-seen+)

      (when-bind (objs (cave-objects dungeon x y))
	(dolist (i (items.objs objs))
	  (setf (aobj.marked i) t)))
      
      (unless (bit-flag-set? flag +cave-mark+)
	(let ((feat (coord.floor coord)))
	  (if (<= feat +floor-invisible-trap+)
	      ;; skip save of certain floors
	      nil
	      (bit-flag-add! (coord.flags coord) +cave-mark+))))
  
      )))

(defun print-relative! (dungeon x y the-char the-attr)
  "Prints the-char and the-attr at relative coordinates, on the map."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3))
	   (type fixnum x y))
  
  (let ((player *player*))
    
    (unless (and player dungeon *dungeon*)
      (return-from print-relative! nil))

    (when (characterp the-char)
      (setf the-char (char-code the-char))) ;; hack!

    
    (let* ((pvx (player.view-x player))
	   (pvy (player.view-y player))
	   (kx (- x pvx))
	   (ky (- y pvy)))
      
      (declare (type fixnum pvx pvy kx ky))

      ;; debugging
      ;;    (when (or (minusp kx) (minusp ky))
      ;;    (error "a value became negative.."))

;;      (warn "drawing ~s,~s at ~s,~s" the-attr the-char x y)
    
      ;; sometimes we cross a screen
      (when (and (<= 0 kx)
		 (<= 0 ky)
		 (< ky *screen-height*)
		 (< kx *screen-width*))

	(let ((vx (+ +start-column-of-map+ kx (if *use-graphics* kx 0)))
	      (vy (+ +start-row-of-map+ ky)))

	  (when *use-graphics* ;; remove later
	    (assert (oddp vx)))
	  (c-term-queue-char! vx vy the-attr the-char 0 0)

	  (when *use-graphics*
	    (if (>= the-attr +graphics-start+)
		(c-term-queue-char! (1+ vx) vy -1 -1 0 0)
		(c-term-queue-char! (1+ vx) vy +term-white+ #.(char-code #\Space) 0 0)))
	  ))
      )))


(defun light-spot! (dungeon x y)
  "lighting up the spot.."
  (declare (type fixnum x y))
  
  (let ((player *player*))
    
    (unless (and player dungeon *dungeon*) ;; hackish!
      (return-from light-spot! nil))
    
    (multiple-value-bind (the-attr the-char trans-attr trans-char)
	(map-info dungeon x y)

      ;; eventually fix these at origin
      (when (characterp the-char)
	(setf the-char (char-code the-char))) ;; hack!
      (when (characterp trans-char)
	(setf trans-char (char-code trans-char))) ;; hack!
      
    
    (let* ((pvx (player.view-x player))
	   (pvy (player.view-y player))
	   (kx (- x pvx))
	   (ky (- y pvy)))
      
      (declare (type fixnum pvx pvy kx ky))

      ;; sometimes we cross a screen
      (when (and (<= 0 kx)
		 (<= 0 ky)
		 (< ky *screen-height*)
		 (< kx *screen-width*))

      
      (let ((vx (+ +start-column-of-map+ kx (if *use-graphics* kx 0)))
	    (vy (+ +start-row-of-map+ ky)))

;;	(warn "Light spot ~s ~s,~s with ~s,~s" (if dungeon (dungeon.depth dungeon) nil) x y
;;	      the-attr the-char)
	(when *use-graphics*  ;; remove later
	  (assert (oddp vx)))
	(c-term-queue-char! vx vy the-attr the-char trans-attr trans-char)
	(when *use-graphics*
	  (incf vx)
;;	  (warn "next step")
	  (if (>= the-attr +graphics-start+)
	      (c-term-queue-char! vx vy -1 -1 0 0)
	      (c-term-queue-char! vx vy +term-white+ #.(char-code #\Space)
				  +term-white+ #.(char-code #\Space))))
      ))
  ))))


(defun get-coord-trigger (dungeon x y)
  "Tries to find a trigger at a given point."
;;  (warn "looking for trigger at ~d,~d -> ~s" x y (dungeon.triggers dungeon))
  (dolist (i (dungeon.triggers dungeon))
    (let ((place (car i)))
      (when (and (= (car place) x)
		 (= (cdr place) y))
	(return-from get-coord-trigger i))))
  nil)

;;(trace get-coord-trigger)

(defun (setf get-coord-trigger) (val dungeon x y)
  "Adds a trigger to the given dungeon."

;;  (warn "Adding ~s at ~d,~d" val x y)
  (block setf-coord-trigger
    (dolist (i (dungeon.triggers dungeon))
      (let ((place (car i)))
	(when (and (= (car place) x)
		   (= (cdr place) y))
	  (setf (cdr i) val)
	  (return-from setf-coord-trigger i))))
    
    (push (cons (cons x y) val) (dungeon.triggers dungeon))))


(defun apply-possible-coord-trigger (dungeon x y)
  "This is a hack.. fix me later.."
  (declare (type fixnum x y))

  (when-bind (decor (cave-decor dungeon x y))
    (when-bind (events (decor.events decor))
      (dolist (i events)
	(when (typep i 'l-event)
	  (funcall (event.function i) decor dungeon x y)
	  (return-from apply-possible-coord-trigger t)))))

    
  (let ((trigger (get-coord-trigger dungeon x y)))
    (when (and trigger (consp trigger) (is-event? (cdr trigger)))
      (let ((the-event (cdr trigger)))
	(apply (event.function the-event) dungeon x y (event.state the-event)))
      )))

(defun put-cursor-relative! (dungeon x y)
  "Tries to put the cursor relative to the window."
  (declare (ignore dungeon))
  (declare (type fixnum x y))

  (let* ((player *player*)
	 (pwy (player.view-y player)) 
	 (ky (- y pwy))
	 (pwx (player.view-x player))
	 (kx (- x pwx)))
    
    (declare (type fixnum pwx pwy kx ky))
    
    (when (and (<= 0 kx)
               (<= 0 ky)
               (< ky *screen-height*)
               (< kx *screen-width*))

      
      (c-term-gotoxy! (+ +start-column-of-map+ kx (if *use-graphics* kx 0)) ;; hack
		      (+ +start-row-of-map+ ky)))))

	 
(defun remove-item-from-dungeon! (dungeon item)
;;  (lang-warn "Removing item ~s from dungeon" item)
  (setf (dungeon.objects dungeon) (delete item (dungeon.objects dungeon))))

(defun add-room-to-dungeon! (dungeon room)
  (push room (dungeon.rooms dungeon)))


;; alters the dungeon object.
(defmethod activate-object :around ((obj level) &key)
   (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find ACTIVATE-OBJECT for type ~a" (type-of obj))
     (return-from activate-object nil))

   ;; we pass along the same arguments.. 
   (let* ((result (call-next-method))
	  (dungeon (level.dungeon result)))
     (cond ((and dungeon (typep dungeon 'dungeon))
	    (setf (dungeon.active dungeon) t)
	    result)
	   (t
	    (lang-warn "Activation of object ~a failed, return was ~a" obj result)
	    nil))
     ))


(defun define-room (id constructor)
  "First argument should be an integer.. fix this later.."
  (assert (or (stringp id) (symbolp id)))
  (assert (functionp constructor))

  (let ((table (variant.room-builders *variant*))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) constructor)))

(defun get-room (id)
  "Returns the constructor to build the given room, or NIL."

  (assert (or (stringp id) (symbolp id)))
  
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.room-builders *variant*)))
    (gethash key table)))

(defmethod find-appropriate-room (variant level player)
  (declare (ignore variant level player))
  (error "find-appropriate-room not implemented."))

(defun construct-room! (room-type dungeon player bx0 by0)
  "Constructs and returns an active-room."
  
  ;;  (declare (ignore player))
  ;;  (warn "Build room ~a ~a" by0 bx0)

  (assert (and (>= bx0 0) (>= by0 0) (< bx0 18) (< by0 6)))

  (let ((returned-room nil))
  
    (block room-construction
      
      (let* (;;(room-builder (get-room-builder num))
	     (room-info (room-type.size-mod room-type))
	     (room-map (dun-data.room-map *cur-dun*))
	     (by1 (+ by0 (svref room-info 0)))
	     (by2 (+ by0 (svref room-info 1)))
	     (bx1 (+ bx0 (svref room-info 2)))
	     (bx2 (+ bx0 (svref room-info 3))))
	
	(when (or (< by1 0)
		  (< bx1 0)
		  (>= by2 (dun-data.row-rooms *cur-dun*))
		  (>= bx2 (dun-data.col-rooms *cur-dun*)))
	  (warn "off the screen...")
	  (return-from room-construction nil))
	
	;; verify open space
	(loop for i from by1 to by2
	      do
	      (loop for j from bx1 to bx2
		    do
		    (when (aref room-map j i)
		      (return-from room-construction nil))))

    
	(let (;;(fun (cdr room-builder))
	      (y (int-/ (* (+ by1 by2 1) +block-height+) 2))
	      (x (int-/ (* (+ bx1 bx2 1) +block-width+) 2)))

	  (build-room! room-type dungeon player x y)

	  (let ((aroom (make-instance 'active-room :type room-type
				      :loc-x x
				      :loc-y y)))
	    (add-room-to-dungeon! dungeon aroom)
	    (setq returned-room aroom))


	  (push (cons x y) (dun-data.room-centres *cur-dun*))

	  ;; reserve space in the room map
      
	  (loop for i from by1 to by2
		do
		(loop for j from bx1 to bx2
		      do
		      (setf (aref room-map j i) t)))

	  ;; skip crowd


	  returned-room)))))

 
;; a simple builder, register it in your variant as 'random-level
(defun make-random-level-obj ()
  (make-instance 'random-level :depth 0 :rating 0))

(defmethod level-ready? ((level random-level))
  (when (level.dungeon level)
    t))


(defmethod register-level! ((var-obj variant) (id string) &key object-filter monster-filter &allow-other-keys)
;;  (assert (not (eq nil var-obj)))
;;  (assert (symbolp id))
;;  #+langband-extra-checks
;;  (assert (ok-object? var-obj))

  (let ((mon-table (make-game-obj-table))
	(obj-table (make-game-obj-table)))
    
    (setf (gobj-table.obj-table mon-table) (make-hash-table :test #'equal)
	  (gobj-table.obj-table obj-table) (make-hash-table :test #'equal))

    (setf (gethash id (variant.monsters-by-level var-obj)) mon-table
	  (gethash id (variant.objects-by-level var-obj))  obj-table)

    ;; fix
    (when object-filter
      (if (not (functionp object-filter))
	  (lang-warn "Object-filter ~s for level ~s is not a function." object-filter id)
	  (pushnew (cons id object-filter)
		   (gethash :objects (variant.filters var-obj))
		   :key #'car)))
    
    (when monster-filter
      (if (not (functionp monster-filter))
	  (lang-warn "Monster-filter ~s for level ~s is not a function." monster-filter id)
	  (pushnew (cons id monster-filter)
		   (gethash :monsters (variant.filters var-obj))
		   :key #'car)))
    
    ))

(defun %get-var-table (var-obj key slot)
  ""
  (let ((id (etypecase key
	      (level (level.id key))
	      (string key))))
    (let ((mon-table (slot-value var-obj slot)))
      (when mon-table
	(gethash id mon-table)))))

