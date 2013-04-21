;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: view.lisp - code for figuring out what is seen
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(defconstant +view-max+ 1536)
(defconstant +vinfo-max-grids+ 161)
(defconstant +vinfo-max-slopes+ 126)

   
(defconst +vinfo-bits-3+ vinfo-bit-type #x3FFFFFFF "")
(defconst +vinfo-bits-2+ vinfo-bit-type #xFFFFFFFF "")
(defconst +vinfo-bits-1+ vinfo-bit-type #xFFFFFFFF "")
(defconst +vinfo-bits-0+ vinfo-bit-type #xFFFFFFFF "")


(defconstant +scale+ 100000)

(defstruct (vinfo-type (:conc-name vinfo-type.))
  grids ;; 8
  bits  ;; 4
  next-0
  next-1
  x
  y
  d
  r)

(defstruct (vinfo-hack (:conc-name vinfo-hack.))
  num-slopes
  slopes
  slopes-min
  slopes-max)

(defvar *vinfo* (make-array +vinfo-max-grids+))
    
;;(defvar *view-size* 0)
;;(defvar *view-array* (make-array +view-max+))

(defun create-vinfo-hack ()
  (let ((hack (make-vinfo-hack)))
    (setf (vinfo-hack.num-slopes hack) 0
	  (vinfo-hack.slopes hack) (make-array +vinfo-max-slopes+)
	  (vinfo-hack.slopes-max hack) (make-array (list (1+ +max-sight+)
							 (1+ +max-sight+)))
	  (vinfo-hack.slopes-min hack) (make-array (list (1+ +max-sight+)
							 (1+ +max-sight+))))
    hack))
	  

(defun vinfo-init-aux (hack x y m)
  (let ((i 0)
	(slope-num (vinfo-hack.num-slopes hack)))
    
    (when (and (> m 0)
	       (<= m +scale+))
      (loop named inner
	    for j of-type u-fixnum from 0 to (1- slope-num)
	    do
	    (incf i)
	    (when (= m (svref (vinfo-hack.slopes hack) j))
	      ;;(warn "hit on ~a" j)
	      (setq i j)
	      (return-from inner nil)))

;;      (warn "compare ~a ~a" i slope-num) 
      (when (= i slope-num)
	(assert (< slope-num +vinfo-max-slopes+))
	(setf (svref (vinfo-hack.slopes hack) slope-num) m)
	(incf (vinfo-hack.num-slopes hack))))

    (when (< m (aref (vinfo-hack.slopes-min hack) x y))
      (setf (aref (vinfo-hack.slopes-min hack) x y) m))

    (when (> m (aref (vinfo-hack.slopes-max hack) x y))
      (setf (aref (vinfo-hack.slopes-max hack) x y) m))))

;;(trace vinfo-init-aux)

#+allegro
(defmacro grid (x y)
  `(the fixnum (+ (the fixnum (* 256 ,y)) ,x)))

#-allegro
(defun grid (x y)
  (the fixnum (+ (* 256 y) x)))

#+allegro
(defmacro grid-y (g)
  `(the fixnum (int-/ ,g 256)))

#-allegro
(defun grid-y (g)
  (the fixnum (int-/ g 256)))

#+allegro
(defmacro grid-x (g)
  `(the fixnum (prog1 (mod ,g 256))))

#-allegro
(defun grid-x (g)
  (the fixnum (prog1 (mod g 256))))

(defun vinfo-init ()
  (let ((hack (create-vinfo-hack))
	(vinfo *vinfo*)
	(num-grids 0))

    (loop for y from 0 to +max-sight+
	  do
	  ;;(warn "Going ~a" y)
	  (loop for x from y to +max-sight+
		do
		(unless (> (distance 0 0 x y) +max-sight+)
		  ;; set wild values
		  (setf (aref (vinfo-hack.slopes-max hack) x y) 0
			(aref (vinfo-hack.slopes-min hack) x y) 999999999)

		  (assert (< num-grids +vinfo-max-grids+))

		  (incf num-grids)
		  (let ((ty (* 1000 y))
			(tx (* 1000 x)))
	      
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (- ty 500))
					   (+ tx 500)))
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (- ty 500))
					   (- tx 500)))
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (+ ty 500))
					   (+ tx 500)))
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (+ ty 500))
					   (- tx 500))))
		  )))


    (assert (>= num-grids +vinfo-max-grids+))
    (assert (>= (vinfo-hack.num-slopes hack) +vinfo-max-slopes+))

    (setf (vinfo-hack.slopes hack)
	  (stable-sort (vinfo-hack.slopes hack) #'<))

;;    (print (vinfo-hack.slopes hack))

;;    (warn "last part")
    (loop for i from 0 to (1- +vinfo-max-grids+)
	  do
	  (setf (svref vinfo i) (make-vinfo-type :grids (make-array 8 :initial-element 0)
						 :bits (make-array 4 :element-type 'vinfo-bit-type
								   :initial-element 0)
						 :x 0
						 :y 0
						 :d 0
						 :r 0
						 )))
    
    (let ((queue-head 0)
	  (queue-tail 0)
	  (queue (make-array (* 2 +vinfo-max-grids+) :initial-element nil))
	  (num-slopes (vinfo-hack.num-slopes hack)))

      (setf (svref queue queue-tail) (svref vinfo 0))
      (incf queue-tail)

      (while (< queue-head queue-tail)
;;	(warn "~a vs ~a -> ~s" queue-head queue-tail (vinfo-type.grids (svref vinfo queue-head)))
;;	(when (> (incf counter) 160)
;;	  (error "blah"))
	(let* ((e queue-head)
	       ;; get next
	       ;;(p (svref queue queue-head))
	       (vinfo-obj (svref vinfo e))  ;; vinfo(e)
	       (grid-array (vinfo-type.grids vinfo-obj))
	       (g (svref grid-array 0))
	       (x (grid-x g))
	       (y (grid-y g)))

	  (incf queue-head)
	  
;;	  (warn "Looping e=~d qh=~d qt=~d g=~d y=~d x=~d"
;;		e queue-head queue-tail g y x)
	  
	  (setf (svref grid-array 0) (grid x y)
		(svref grid-array 1) (grid y x)
		(svref grid-array 2) (grid (- y) x)
		(svref grid-array 3) (grid (- x) y)
		(svref grid-array 4) (grid (- x) (- y))
		(svref grid-array 5) (grid (- y) (- x))
		(svref grid-array 6) (grid y (- x))
		(svref grid-array 7) (grid x (- y)))

	  ;; do slopes
	  (dotimes (i num-slopes)
	    (let ((m (svref (vinfo-hack.slopes hack) i)))
	      (when (and (> e 0)
			 (> m (aref (vinfo-hack.slopes-min hack) x y))
			 (< m (aref (vinfo-hack.slopes-max hack) x y)))
		
		(bit-flag-add! (aref (vinfo-type.bits vinfo-obj) (int-/ i 32))
			       (expt 2 (mod i 32)))
		)))
	  
	  (setf (vinfo-type.next-0 vinfo-obj) (svref vinfo 0))

	  (when (<= (distance 0 0 (1+ x) y)
		    +max-sight+)
	    (let* ((g (grid (1+ x) y))
		   (last-qobj (svref queue (1- queue-tail)))
		   (f-grdval (svref (vinfo-type.grids last-qobj)
						      0)))
	      (unless (= g f-grdval)
		(let ((some-vinfo (svref vinfo queue-tail)))
		  (setf (svref (vinfo-type.grids some-vinfo) 0)
			g)
		 
		  (setf (svref queue queue-tail) some-vinfo)
		  (incf queue-tail)))

	      (setf (vinfo-type.next-0 vinfo-obj) (svref vinfo (1- queue-tail)))
	      ))
	  
	  (setf (vinfo-type.next-1 vinfo-obj) (svref vinfo 0))
	  
	  (when (<= (distance 0 0 (1+ x) (1+ y))
		    +max-sight+)
	    (let* ((g (grid (1+ x) (1+ y)))
		   (last-qobj (svref queue (1- queue-tail)))
		   (f-grdval (svref (vinfo-type.grids last-qobj)
				    0)))
	      (unless (= g f-grdval)
		(let ((some-vinfo (svref vinfo queue-tail)))
		  (setf (svref (vinfo-type.grids some-vinfo) 0)
			g)
		 
		(setf (svref queue queue-tail) some-vinfo)
		(incf queue-tail)))

	      (setf (vinfo-type.next-1 vinfo-obj) (svref vinfo (1- queue-tail)))
	      ))

	  ;; hack
	  (when (= y x)
	    (setf (vinfo-type.next-0 vinfo-obj)
		  (vinfo-type.next-1 vinfo-obj)))
	  
	  
	  (setf (vinfo-type.y vinfo-obj) y
		(vinfo-type.x vinfo-obj) x
		(vinfo-type.d vinfo-obj) (if (> y x)
					(+ y (int-/ x 2))
					(+ x (int-/ y 2)))
		(vinfo-type.r vinfo-obj) (if (= y 0)
					x
					(if (= x 0)
					    y
					    (if (= x y)
						y
						0))))
	  
					
	  )))

    ;;(%hidden-dump)
    
    (values)))


(defvar *array-view* (make-array 1536 :fill-pointer 0))
(defvar *temp-view* (make-array 1536 :fill-pointer 0))

;; seems to be a list, despite the name
;;(defvar *view-hack-arr* nil)

(defun update-view! (dun pl)
  "Updates the view from the given player."

  (declare (optimize (safety 0) (speed 3) (debug 0)))

  (let* ((py (player.loc-y pl))
	 (px (player.loc-x pl))
	 (pg (grid px py))
	 (radius (player.light-radius pl))
	 (fast-view *array-view*)
	 (fast-temp *temp-view*)
	 (vinfo *vinfo*)
	 ;;(fast-view (make-map dun))
	 )

    (declare (type u-fixnum py px pg radius))
    (declare (type (vector t 1536) fast-temp fast-view))
    
;;    (declare (:explain :boxing))

    (setf (fill-pointer fast-temp) 0)
    
    (flet ((add-coord-info (grid flag)
	     ;; remove cons'ing later
	     (declare (ignore flag)
		      (type fixnum grid)
		      (type u-fixnum flag))
	     (vector-push grid fast-view)
;;	     (push (cons grid flag) fast-view)
	     )
	   (add-temp-info (grid flag)
	     (declare (ignore flag)
		      (type fixnum grid))
	     (vector-push grid fast-temp) 
;;	     (push (cons grid flag) fast-temp)
	     )
	   (get-real-flag (grid)
	     (declare (type fixnum grid))
	     (cave-flags dun (grid-x grid) (grid-y grid)))
	   (update-real-flags (grid flag)
	     (setf (cave-flags dun (grid-x grid) (grid-y grid)) flag)))
      
;;      (warn "player at {~a,~a}" px py)
    
      ;; step 0

      (block save-old-flags
	(loop for grid of-type fixnum across fast-view do

	  (let* (;;(grid (car i))
		 (flag (get-real-flag grid)))
	    (declare (type u-fixnum flag))
	    (when (bit-flag-set? flag +cave-seen+)
	      (bit-flag-add! flag +cave-temp+)
	      (add-temp-info grid flag))

	    ;; clear
	    (bit-flag-remove! flag #.(logior +cave-view+ +cave-seen+))
	    (update-real-flags grid flag))))

      (setf (fill-pointer fast-view) 0)
      
      ;; step 1 - player grid
      
      (block player-grid
	(let* ((coord (cave-coord dun px py))
	       (flag (coord.flags coord))
	       )

	  (declare (type u-fixnum flag))
	  ;; assume viewed
	  (bit-flag-add! flag +cave-view+)

	  (cond ((< 0 radius)
		 ;; torch
		 ;;(warn "pl seen.")
		 (bit-flag-add! flag +cave-seen+))
		
		((bit-flag-set? flag +cave-glow+)
		 (bit-flag-add! flag +cave-seen+)))

	  (add-coord-info pg flag)
	  ;;(warn "fast view is ~a" fast-view)
	  (setf (coord.flags coord) flag)))

      
      
      ;; skip blindness

      (block octant-run
	;; octants
	(loop for o2 of-type u-fixnum from 0 to 7;; size of the grid array
	      do
	    
	      ;; (warn "octant ~a" o2)
	    
	      (let ((queue-head 0)
		    (queue-tail 0)
		    (queue (make-array (* 2 +vinfo-max-grids+) :initial-element nil))
		    ;;(num-slopes (vinfo-hack.num-slopes hack))
		    (bit-arr (make-array 4 :element-type 'vinfo-bit-type))
		    (last-v (svref vinfo 0))
		    ;;(count 0)
		    )

		(declare (type (simple-array vinfo-bit-type (4)) bit-arr))
		(declare (type vinfo-type last-v))
		
;;		(warn "fish")
		(setf (aref bit-arr 0) +vinfo-bits-0+
		      (aref bit-arr 1) +vinfo-bits-1+
		      (aref bit-arr 2) +vinfo-bits-2+
		      (aref bit-arr 3) +vinfo-bits-3+)
		
;;		(warn "go..")
		
		(setf (svref queue queue-tail) (svref vinfo 1))
		(incf queue-tail)
		(setf (svref queue queue-tail) (svref vinfo 2))
		(incf queue-tail)
	      
		(while (< queue-head queue-tail)
		
;;		  (when (= (incf count) 40)
;;		    (return))
		  (assert (< queue-head (length queue)))
		  
		  (let* ((e queue-head)
			 ;; get next
			 ;;(p (svref queue queue-head))
			 (vinfo-obj (svref queue e))
			 (its-d (vinfo-type.d vinfo-obj))
			 (bits (vinfo-type.bits vinfo-obj))
			 (grid-array (vinfo-type.grids vinfo-obj))
			 (g (+ pg (the fixnum (svref grid-array o2))))
			 (x (grid-x g))
			 (y (grid-y g))
			 )
		    
		    (declare (type u-fixnum g x y its-d))
		    (declare (type (simple-array vinfo-bit-type (4)) bits))
		    
		    (incf queue-head)

		    ;;(warn "Bit-arr[~a]: ~s vs ~s" queue-head bit-arr bits)
		  
		    (when (or (bit-flag-and (aref bit-arr 0)
					    (aref bits 0))
			      (bit-flag-and (aref bit-arr 1)
					    (aref bits 1))
			      (bit-flag-and (aref bit-arr 2)
					    (aref bits 2))
			      (bit-flag-and (aref bit-arr 3)
					    (aref bits 3)))

		      (when (minusp g)
			(error "{pg=~s,px=~s,py=~s} -> {g=~s,x=~s,y=~s} -> {o2=~s,e=~s} -> ~s"
			       pg px py g x y o2 e grid-array))
		      
		      (let* ((coord (cave-coord dun x y))
			     (info (coord.flags coord)))

			(declare (type u-fixnum info))

;;			(warn "bit ok at {~a,~a} -> ~a" x y info)
		      

			;; we have a wall!
			(cond ((bit-flag-set? info +cave-wall+)

			       (dotimes (i 4)
				 (bit-flag-remove! (aref bit-arr i)
						   (aref bits i)))
			       ;;(warn "{~a,~a} -> a wall" x y)

			       ;; we just do stuff to new walls
			       (unless (bit-flag-set? info +cave-view+)
				 (bit-flag-add! info +cave-view+)

				 (cond ((< its-d radius)
					;;(warn "see")
					(bit-flag-add! info +cave-seen+))
				     
				       ((bit-flag-set? info +cave-glow+)
					(let* ((x (grid-x g))
					       (y (grid-y g))
					       (xx (if (< x px)
						       (1+ x)
						       (if (> x px)
							   (1- x)
							   x)))
					       (yy (if (< y py)
						       (1+ y)
						       (if (> y py)
							   (1- y)
							   y))))
					  
					  (when (bit-flag-set? (cave-flags dun xx yy) +cave-glow+)
					    (bit-flag-add! info +cave-seen+)))))
					      
			       

				 (add-coord-info g info)
				 (setf (coord.flags coord) info)))
				 
			       
			      
			      (t
			       ;; no wall
			       (let ((n-0 (vinfo-type.next-0 vinfo-obj))
				     (n-1 (vinfo-type.next-1 vinfo-obj)))
				 ;; n-0 et al is of type VINFO-TYPE
				 (declare (type vinfo-type n-0 n-1))
				 (when (not (eql last-v n-0))
				   (setf (svref queue queue-tail) n-0
					 last-v n-0)
				   (incf queue-tail))
			       
				 (when (not (eql last-v n-1))
				   (setf (svref queue queue-tail) n-1
					 last-v n-1)
				   (incf queue-tail))


				 ;; newly found non-wall
				 (unless (bit-flag-set? info +cave-view+)
				   (bit-flag-add! info +cave-view+)
				   (cond ((< its-d radius)
					  (bit-flag-add! info +cave-seen+))
					 
					 ((bit-flag-set? info +cave-glow+)
					  (bit-flag-add! info +cave-seen+)))

				   (add-coord-info g info)
				   (setf (coord.flags coord) info))
				 )))

		      
			)))

		  )))
	)

      (block handle-blindness
	;; later
	)

      (block process-new-grids
      
	;; fix the new view
	(loop for the-grid across fast-view do

	  (let* (;;(the-grid (car i))
		 (the-flag (get-real-flag the-grid)))
	    (declare (type u-fixnum the-flag))
	    ;; was not seen, is now seen
	    (when (and (bit-flag-set? the-flag +cave-seen+)
		       (not (bit-flag-set? the-flag +cave-temp+)))
	      (let ((x (grid-x the-grid))
		    (y (grid-y the-grid)))
		(note-spot! dun x y)
		(light-spot! dun x y))))))


      (block process-old-grids

	(loop for grid across fast-temp do
	  (let* (;;(grid (car i))
		 (x (grid-x grid))
		 (y (grid-y grid))
		 (coord (cave-coord dun x y))
		 (flag (coord.flags coord)))

	    (declare (type u-fixnum flag))

	    ;; remove temp-flag
	    (bit-flag-remove! flag +cave-temp+)
	    (setf (coord.flags coord) flag)

	    ;; was seen, no more
	    (unless (bit-flag-set? flag +cave-seen+)
	      (light-spot! dun x y))
	    
	    )))
    

;;      (setf *view-hack-arr* fast-view)
    
      (values))))
	      
				       
		  
	
(defun forget-view! (dun pl)
  "Clears everything currently viewed."
  (declare (ignore pl))
  
;;  (warn "(forget-view!)")
  (let ((the-array *array-view*))
  
    (loop for the-grid across the-array
	  do
	  (let* (;;(the-grid (car i))
		 (x (grid-x the-grid))
		 (y (grid-y the-grid))
		 (flag (cave-flags dun x y)))
	    
	    (setf (cave-flags dun x y) (bit-flag-remove! flag #.(logior +cave-view+ +cave-seen+)))
	    (light-spot! dun x y)
	    
      
	    ))
    
    (setf (fill-pointer the-array) 0)))



(defun %hidden-dump ()
  
    (let ((str *standard-output*))
      (loop for i from 0
	    for x across *vinfo*
;;	    until (> i 30)
#||	    (format str "~d: x=~d y=~d d=~d r=~d~%"
		    i
		    (vinfo-type.x x)
		    (vinfo-type.y x)
		    (vinfo-type.d x)
		    (vinfo-type.r x))||#
	    do
	    (format str "~d: ~s~%" i (vinfo-type.bits x))
;;	    (format str "~d: ~s~%" i (vinfo-type.grids x))
;;	    (format str "~d: ~s~%" i (vinfo-type.d x))
	    ))
    )

(in-package :cl-user)
(defun kl ()
  (lb::vinfo-init))

