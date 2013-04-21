;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: rooms.lisp - contains room-builders
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with generation of rooms

|#

(in-package :langband)


(defun define-room-builder (key room-data function)
  "First argument should be an integer.. fix this later.."
  (setf (gethash key *room-builders*) (cons room-data function)))

(defun build-room! (player dungeon bx0 by0 num)
  
  (declare (ignore player))
;;  (warn "Build room ~a ~a" by0 bx0)

  (assert (and (>= bx0 0) (>= by0 0) (< bx0 18) (< by0 6)))
  
  (let* ((room-builder (gethash num *room-builders*))
	 (room-info (car room-builder))
	 (room-map (dun-data-room-map *cur-dun*))
	 (by1 (+ by0 (svref room-info 0)))
	 (by2 (+ by0 (svref room-info 1)))
	 (bx1 (+ bx0 (svref room-info 2)))
	 (bx2 (+ bx0 (svref room-info 3))))

    (when (or (< by1 0)
	      (< bx1 0)
	      (>= by2 (dun-data-row-rooms *cur-dun*))
	      (>= bx2 (dun-data-col-rooms *cur-dun*)))
      (warn "off the screen...")
      (return-from build-room! nil))

    ;; verify open space
    (loop for i from by1 to by2
	  do
	  (loop for j from bx1 to bx2
		do
		(when (aref room-map j i)
		  (return-from build-room! nil))))

    
    (let ((fun (cdr room-builder))
	  (y (int-/ (* (+ by1 by2 1) +block-height+) 2))
	  (x (int-/ (* (+ bx1 bx2 1) +block-width+) 2)))

      (funcall fun dungeon x y)

      (push (cons x y) (dun-data-room-centres *cur-dun*))

      ;; reserve space in the room map
      
      (loop for i from by1 to by2
	  do
	  (loop for j from bx1 to bx2
		do
		(setf (aref room-map j i) t)))

      ;; skip crowd


      t)))


(defun build-simple-room (dungeon x0 y0)

  (let ((light t) ;; fix later
	(y1 (- y0 (randint 4)))
	(y2 (+ y0 (randint 3)))
	(x1 (- x0 (randint 11)))
	(x2 (+ x0 (randint 11))))

;;    (warn "Point (~a ~a)  --> (~a ~a) (~a ~a)" x0 y0 x1 y1 x2 y2) 
    
    (generate-room dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) light)
    (generate-draw dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) +feature-wall-outer+)
    (generate-fill dungeon x1 y1 x2 y2 +feature-floor+)

;;    (generate-draw dungeon 4 4 20 20 +feature-wall-outer+)
;;    (generate-fill dungeon 4 4 20 20 +feature-floor+)
    
    ;; skip hacks
    ))


(define-room-builder 1 #1A(0 0 -1 1 1)
		     #'build-simple-room)
