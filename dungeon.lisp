;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: dungeon.lisp - basic code for the dungeon
Copyright (c) 2000-2003 - Stig Erik Sandø

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

  (let* ((variant *variant*)
	 (ft (etypecase val
	       (integer (gethash val (variant.floor-types variant)))
	       (floor-type val)
	       (string (gethash val (variant.floor-types variant)))
	       ))
	 #||
	 (numeric-id (etypecase val
	 (integer val)
	 (floor-type (floor.numeric-id val))
	 (string (floor.numeric-id ft))
	 ))
	 ||#
	 )
		    
		     
    ;; fix me
    ;;(setf (coord.floor coord) numeric-id)
    (setf (coord.floor coord) ft)
  
    (if (bit-flag-set? (floor.flags ft) +floor-flag-wall+)
	;; this is a wall or a door..
	(bit-flag-add! (coord.flags coord) +cave-wall+)
	;; we're not a a wall or a door.. 
	(bit-flag-remove! (coord.flags coord) +cave-wall+)
	)
    ))

(defun clean-coord! (coord)
  "Cleans the given coordinate."
  
  (setf (coord-floor    coord) (get-floor-type "nothing")
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
  (unless (or (eq val nil) (typep val 'decor))
    (warn "Assigning odd value ~s to ~s,~s as decor" val x y))
  (setf (coord.decor (aref (dungeon.table dungeon) x y)) val))


(defun (setf cave-floor) (val dungeon x y)
;;  (warn "Setting ~a ~a to ~a" x y val)
;;  (declare (type fixnum val))
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
  #+never
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

(defun draw-to-map (dungeon x y tx ty)
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
	 ;;(ret-attr +term-white+)
	 ;;(ret-char #\X)
	 (pl-obj *player*)
	 ;;(trans-attr 0)
	 ;;(trans-char 0)
	 (using-gfx (use-gfx-tiles? *map-frame*))
	 ;;(name-return nil)
;;	 (attr-fun (if using-gfx #'x-attr #'text-attr)) ;; gradually remove
;;	 (char-fun (if using-gfx #'x-char #'text-char)) ;; gradually remove
	 (sym-fun (if using-gfx #'gfx-sym #'text-sym))
	 (win (aref *windows* *map-frame*))
	 ;;(fg-attr 0)
	 ;;(fg-char 0)
	 ;;(mid-attr 0)
	 ;;(mid-char 0)
	 (floor-sym 0)
	 (mid-sym 0)
	 (mon-sym 0)
	 )

    (clear-coord win tx ty)
    ;; hackish, fix later
    (when (consp mon)
      (setf mon (car mon)))

    ;; first of all we need to decide if we can see the floor at all
    (cond ((or (bit-flag-set? flags +cave-mark+)
	       (bit-flag-set? flags +cave-seen+))

	   ;;(assert (plusp feat))
	   ;;; then we need to find out what the floor is
	   ;;(setf f-obj (get-floor-type feat))
	   (setf f-obj feat)

	   (setf floor-sym (funcall sym-fun f-obj))
	   ;;(setf ret-attr (funcall attr-fun f-obj)
		;; ret-char (funcall char-fun f-obj))
	   #||
	   (when (and (bit-flag-set? flags +cave-seen+)
		      (not (bit-flag-set? flags +cave-glow+))
		      (bit-flag-set? (floor.flags f-obj) +floor-flag-use-light-effect+))
	     (cond (using-gfx
		    (decf ret-char))
		   ((eql ret-attr +term-white+)
		    (setf ret-attr +term-yellow+))))
	   ||#
	   )


	  ;; otherwise not seen
	  (t
	   (setf f-obj (get-floor-type "nothing"))
	   (setf floor-sym (funcall sym-fun f-obj))
	   ;;(warn "floor at ~s,~s is nothing" x y)
	   ;;(setf ret-attr (funcall attr-fun f-obj)
		;; ret-char (funcall char-fun f-obj))
	   ))

    (when (plusp floor-sym)
      (setf (window-coord win +background+ tx ty) floor-sym))
    ;;(unless (plusp floor-sym)
    ;;  (error "No real floor-sym"))
	   
	;; we paint floor at the back!
    ;;(setf (window-coord win +background+ tx ty) (tile-paint-value ret-attr ret-char)))

    ;;(warn "done floor for ~s, ~s" x y)
    ;; removed code here, see older versions

    ;; hackish save of transparency
;;    (setf trans-attr ret-attr
;;	  trans-char ret-char)

    ;; this code should check if it has been seen
    ;; places with decor, but must be seen
    (when (and (or (bit-flag-set? flags +cave-mark+)
		   (bit-flag-set? flags +cave-seen+))
	       (typep decor 'decor) (decor.visible? decor))
      ;; hackish
      ;;(setf f-obj (get-floor-type feat)) ;; ok?
      ;;(setf mid-attr (funcall attr-fun decor)
      ;;mid-char (funcall char-fun decor))
      (setf mid-sym (funcall sym-fun decor))
      ;; this is a hack to use a different tile for the floor
      ;;(decf trans-char 6)
      )
	    
    ;; we need a way to make all these effects transparent atop each other!!!
    
    
    ;; let's see if any objects are on top

    (when (and obj-table (typep obj-table 'item-table))

      (let (;;(objs (items.objs obj-table))
	    (obj-len (items.cur-size obj-table))
	    (first-obj (item-table-find obj-table 0)))

	(cond ((and (> obj-len 1) (aobj.marked first-obj))
	       ;; bad hack
	       (setf mid-sym (if using-gfx
				 (tile-paint-value 10 54)
				 (text-paint-value +term-white+ #\&)))
	       ;; pile symbol
	       ;;(setf mid-attr +term-white+
		;;     mid-char #.(char-code #\&))
	      )
	      ((aobj.marked first-obj)
	       (setf mid-sym (funcall sym-fun first-obj))
	       ;;(setf mid-attr (funcall attr-fun first-obj)
		;;     mid-char (funcall char-fun first-obj))
	       )
	      (t
	       ))
	))

    (cond ((plusp mid-sym)
	   (setf (window-coord win +decor+ tx ty) mid-sym))
	  ;;((or (plusp mid-attr) (plusp mid-char))
	  ;; (setf (window-coord win +decor+ tx ty) (tile-paint-value mid-attr mid-char)))
	  )

	  
;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))

    
    ;; do we have monsters and can see it?
    (when (and mon (amon.seen-by-player? mon))
      (let* ((kind (amon.kind mon))
	     ;;(wanted-attr (funcall attr-fun kind))
	     ;;(wanted-char (funcall char-fun kind))
	     )

	(setf mon-sym (funcall sym-fun kind))

	#||
	(cond (nil
	       ;; skip hallucinate
	       )
	      ;; skip avoid-other ???, option that's not supported
	      
	      ;; bizarre monster-attr and char
	      ((and (>= wanted-attr +graphics-start+)
		    (>= wanted-char +graphics-start+))
	       ;;(warn "Fell to double for ~s = ~s ~s ~s" mon mon-sym wanted-attr wanted-char)
	       (setf fg-attr wanted-attr
		     fg-char wanted-char))
	      
	      ;; multi-hued monster
	      ((has-ability? kind '<colour-changing>)
	       (setf fg-attr (charify-number (randint 15))
		     fg-char wanted-char))

	      ;; normal monster, not clear in any way
	      ((not (or (has-ability? kind '<see-through>)
			(has-ability? kind '<absorbs-symbol>)))
	       (setf fg-attr wanted-attr
		     fg-char wanted-char))
	      
	      
	      ;; bizarre grid under
	      ((or (>= ret-attr +graphics-start+)
		   (>= ret-char +graphics-start+))
	       ;;(warn "bizarre grid ~s  ~s" ret-attr ret-char)
	       (setf fg-attr wanted-attr
		     fg-char wanted-char))
	      
	      ;; normal char, clear attr
	      ((not (has-ability? kind '<absorbs-symbol>))
	       (setf fg-char wanted-char))
	      ;; normal attr, clear char
	      ((not (has-ability? kind '<see-through>))
	       (setf fg-attr wanted-attr))
	      
	      (t
	       (warn "Fell through monster-check for monster ~s" (monster.name kind)))
	      
	      )
	||#
	))

    
;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))
    
    ;; remove this entry later..
    (when (and pl-obj ;; only when we have a player
	       (eql x (location-x pl-obj))
	       (eql y (location-y pl-obj)))
;;      (warn "returning player at {~a,~a}" x y)
      ;;(setf fg-attr (funcall attr-fun pl-obj)
	;;    fg-char (funcall char-fun pl-obj))
      (setf mon-sym (funcall sym-fun pl-obj)))

    (cond ((plusp mon-sym)
	   ;;(warn "mon sym ~s" mon-sym)
	   (setf (window-coord win +foreground+ tx ty) mon-sym))
	  ;;((or (plusp fg-attr) (plusp fg-char))
	  ;; (setf (window-coord win +foreground+ tx ty) (tile-paint-value fg-attr fg-char)))
	  )
    

    ;; hack to bring a long target
    (when-bind (target (player.target pl-obj))
      (when (eq mon (target.obj target))
	(setf (window-coord win +effect+ tx ty) (tile-paint-value 40 0))))
    
;;    (when name-return
;;      (warn "Returns (~s,~s)" ret-attr ret-char))
    
    ;;(values ret-attr ret-char trans-attr trans-char)
    nil))


(defun cave-is-room? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-room+))

;; this is just crap!
#||
(defun cave-boldly-naked? (dungeon x y)
  (declare (type fixnum x y))
  (let* ((coord (cave-coord dungeon x y))
	 (fl-type (get-floor-type (coord.floor coord)))
	 (fl-flags (floor.flags fl-type)))
    (and (bit-flag-set? fl-flags +floor-flag-floor+)
	 (eq nil (coord.objects coord))
	 (eq nil (coord.monsters coord)))))
||#

(defun can-place? (dungeon x y type &optional allow-existing)
  (let* ((coord (cave-coord dungeon x y))
	 (fl-type (coord.floor coord)))

    (ecase type
      (:object (cond (allow-existing
		      (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-items+))
		     (t
		      (and (eq nil (coord.objects coord))
			   (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-items+)))))
      (:creature (and (eq nil (coord.monsters coord))
		     (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-creatures+)))
      (:trap (and (eq nil (coord.monsters coord))
		  (eq nil (coord.objects coord))
		  (eq nil (coord.decor coord))))
      (:stair (and (eq nil (coord.monsters coord))
		   (eq nil (coord.objects coord))
		   (eq nil (coord.decor coord))
		   (bit-flag-set? (floor.flags fl-type) +floor-flag-floor+)))
      
      )))
	  
       
       
    


;; just crap, use a better predicate
(defun cave-floor-bold? (dungeon x y)
  (declare (type fixnum x y))
  (not (bit-flag-set? (cave-flags dungeon x y)
		      +cave-wall+)))

;; just crap, use a better predicate
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
  (and (>= y 0)
       (>= x 0)
       (< y (dungeon.height dungeon))
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
  (when-bind (decor (cave-decor dungeon x y))
    (when (and (typep decor 'active-door)
	       (decor.visible? decor))
      (door.closed? decor))))


;; hackish.. not very good
(defun is-open-door? (dungeon x y)
  (when-bind (decor (cave-decor dungeon x y))
    (when (and (typep decor 'active-door)
	       (decor.visible? decor)) 
      (not (door.closed? decor)))))


(defun panel-contains? (player x y)
  "Returns T if the panel contains the coordinate."
  (let ((x-off (- x (player.view-x player)))
	(y-off (- y (player.view-y player))))
	
    (and (>= x-off 0)
	 (>= y-off 0)
	 (< x-off (get-frame-width *map-frame*))
	 (< y-off (get-frame-height *map-frame*)))))


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
	 (ty (+ wy (get-frame-height *map-frame*)))
	 (tx (+ wx (get-frame-width *map-frame*)))
	 )

    (declare (type fixnum ty tx))

;;    (warn "printing map (~s ~s)" (location-x player) (location-y player))
    
    (assert (eq player *player*))
    
    (loop for vy of-type fixnum from 0
	  for y of-type fixnum from wy below ty
	  do
	  
	  (loop for vx of-type fixnum from 0
		for x of-type fixnum from wx below tx
		do
		(when (in-bounds? dungeon x y)
		  (draw-to-map dungeon x y vx vy)))
	  
	  )))
 

(defun modify-panel! (dungeon player vx vy)
  "maybe let variants override this one to avoid the town-hack below?"
  (let ((town-width 66)
	(town-height 22) ;; hacks
	(pl-depth (dungeon.depth dungeon))
	(term-height (get-frame-height *map-frame*))
	(term-width (get-frame-width *map-frame*))
	(dun-height (dungeon.height dungeon))
	(dun-width (dungeon.width dungeon)))

    ;; hack
    (incf pl-depth)
    
    (cond ((= pl-depth 0)
	   (cond ((> vy (- town-height term-height)) ;; hack
		  (setf vy (- town-height term-height)))
		 ((minusp vy)
		  (setf vy 0))))
	  ((> vy (- dun-height term-height))
	   (setf vy (- dun-height term-height))))
  
    (cond ((= pl-depth 0)
	   (cond ((> vx (- town-width term-width)) ;; hack
		  (setf vx (- town-width term-width)))
		 ((minusp vx)
		  (setf vx 0))))
	
	  ((> vx (- dun-width term-width))
	   (setf vx (- dun-width term-width))))

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
	(term-height (get-frame-height *map-frame*))
	(term-width (get-frame-width *map-frame*))
	;;(centre-on-player nil)
	;;(centre-on-player t)
	)

    ;; FIX!!! This code is _very_ broken!
    
    ;; hackish, update when running arrives
    (cond #+centre-on-player
	  ((/= p-y (+ v-y (int-/ term-height 2)))
	   (setf v-y (- p-y (int-/ term-height 2))))
	  
	  ((or (< p-y (+ v-y 3))
	       (>= p-y (+ v-y term-height -3)))
	   (setf v-y (- p-y (int-/ term-height 3)))))
    ;;* (int-/ (- p-y (int-/ panel-height 2))
;;			       panel-height)
;;			panel-height))))

    (cond #+centre-on-player
	  ((/= p-x (+ v-x (int-/ term-width 2)))
	   (setf v-x (- p-x (int-/ term-width 2))))
	  
	  ((or (< p-x (+ v-x 3))
	       (>= p-x (+ v-x term-width -3)))
	   (setf v-x (- p-x (int-/ term-width 3)))))
;;	   (setf v-x (* (int-/ (- p-x (int-/ panel-width 2))
;;			       panel-width)
;;			panel-width))))

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
	(let ((decor (coord.decor coord))
	      (ft (coord.floor coord)))
	  (cond ((bit-flag-set? (floor.flags ft) +floor-flag-floor+)
		 (when (bit-flag-set? flag +cave-glow+)
		   (bit-flag-add! (coord.flags coord) +cave-mark+)))
		 
		(t
		 (bit-flag-add! (coord.flags coord) +cave-mark+)))))
  
      )))

(defun light-spot! (dungeon x y)
  "lighting up the spot.."
  (declare (type fixnum x y))

  (let ((player *player*))
    
    (unless (and player dungeon *dungeon*) ;; hackish!
      (return-from light-spot! nil))

    (let* ((pvx (player.view-x player))
	   (pvy (player.view-y player))
	   (kx (- x pvx))
	   (ky (- y pvy))
	   (win (aref *windows* *map-frame*)))
      
      (declare (type fixnum pvx pvy kx ky))

      ;; sometimes we cross a screen
      (when (and (<= 0 kx)
		 (<= 0 ky)
		 (< ky (get-frame-height *map-frame*))
		 (< kx (get-frame-width  *map-frame*)))

	(draw-to-map dungeon x y kx ky)
	;; maybe add paint here too
	(paint-coord win kx ky)
	;;(queue-map-char! kx ky the-attr the-char trans-attr trans-char)
      ))
  ))



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

(defun get-relative-coords (x y)
  "Returns relative x and relative y, or NIL if they're not visible on map."
  (let* ((player *player*)
	 (pwy (player.view-y player)) 
	 (ky (- y pwy))
	 (pwx (player.view-x player))
	 (kx (- x pwx)))
    
    (declare (type fixnum pwx pwy kx ky))

    (values (if (and (<= 0 kx) (< kx (get-frame-width *map-frame*))) kx nil)
	    (if (and (<= 0 ky) (< ky (get-frame-width *map-frame*))) ky nil))))


(defun put-cursor-relative! (dungeon x y &optional (cursor :map-cursor))
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
               (< ky (get-frame-height *map-frame*))
               (< kx (get-frame-width *map-frame*)))

      (set-cursor-to *map-frame* cursor kx ky))))


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


(defun read-map (variant fname)

  (let ((map-data '()))
    (with-open-file (s (pathname fname)
		       :direction :input)
      (loop for x = (read s nil 'eof)
	    until (eq x 'eof)
	    do
	    (push x map-data)))
    
    (let ((legal-syms (make-hash-table :test #'equal))
	  (height -1)
	  (width -1)
	  (dummy-cnt 400)
	  (dun-rows '()))
      
      (dolist (i map-data)
	
	(ecase (car i)
	  (define-map-symbol
	      (destructuring-bind (symbol id name &key text-attr text-char x-char x-attr (flags 0))
		  (cdr i)
		(let ((ft (make-instance 'floor-type :id id :name name :flags (if (nonboolsym? flags)
										  (eval flags)
										  flags)
					 :numeric-id (incf dummy-cnt))))
		  (handle-gfx-visual ft (eval x-attr) (eval x-char))
		  (handle-text-visual ft (eval text-attr) (eval text-char))
		  
		  #||
					 :text-attr (if (nonboolsym? text-attr)
							(eval text-attr)
							text-attr)
					 :text-char text-char
					 :x-attr (eval x-attr) :x-char (eval x-char))))
		  
		  ;; hacks
		  (when (characterp (text-char ft))
		    (setf (text-char ft) (char-code text-char)))
		  (when (eq (x-attr ft) nil)
		    (setf (x-attr ft) (text-attr ft)))
		  (when (eq (x-char ft) nil)
		    (setf (x-char ft) (text-char ft)))
		  ||#
		  
		  (setf (gethash symbol legal-syms) ft))))
	  (map
	   (with-input-from-string (str (second i))
	     (loop for x = (read-line str nil 'eof)
		   until (eq x 'eof)
		   do
		   (incf height)
		   (setf width (length x))
		   (when (plusp (length x))
		     (push x dun-rows))
		   ))
	   )))

      (let ((real-table (variant.floor-types variant)))
	(loop for v being the hash-values of legal-syms
	      do
	      (setf (gethash (floor.numeric-id v) real-table) v)
	      (setf (gethash (floor.id v) real-table) v)))
	      
      (warn "H ~s W ~s" height width)
      
      (let ((dungeon (create-dungeon width height)))
	(loop for i from 0
	      for row in (nreverse dun-rows)
	      do
	      (loop for j from 0
		    for x across row
		    do
		    (let ((floor (gethash x legal-syms)))
		      ;;(when (eql x #\,)
			;;(warn "floor is ~s" floor))
		      (cond (floor
			     (setf (cave-floor dungeon j i) floor))
			    (t
			     (warn "Could not find ~s" x)))
		      )))

      
	dungeon))))

#||
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
	   (ky (- y pvy))
	   (win (aref *windows* *map-frame*)))
      
      (declare (type fixnum pvx pvy kx ky))

      ;; sometimes we cross a screen
      (when (and (<= 0 kx)
		 (<= 0 ky)
		 (< ky (get-frame-height *map-frame*))
		 (< kx (get-frame-width  *map-frame*)))

	(warn "fix print-relative!")
	;;(setf (window-coord win +foreground+ kx ky) 
	
	;;(queue-map-char! kx ky the-attr the-char 0 0)
	  ))
      ))
||#
