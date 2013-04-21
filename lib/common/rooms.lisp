;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/common/rooms.lisp - room-builders that should be common
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

---
ADD_DESC: The rooms should be registered in the individual variant

|#

(in-package :langband)

(defclass simple-room (room-type)
  ())

(defclass shop-room (room-type)
  ((shop :accessor room.shop :initform nil :initarg :shop)))


(defun common-make-simple-room ()
  "constructor for the simple room."
  (let ((room (make-instance 'simple-room :id "simple-room"
			     :name "simple room"
			     :size-mod #1A(0 0 -1 1)
			     :min-level 1)))

    room))
    
(defun common-make-shop-room ()
  "constructor for the shop room."
  (let ((room (make-instance 'shop-room :id "shop-room"
			     :name "shop room"
			     :size-mod #1A(0 0 -1 1)
			     :min-level 1)))

    room))

(defmethod build-room! ((room simple-room) dungeon player x0 y0)

  (declare (ignore player))
  
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


(defmethod build-room! ((room shop-room) dungeon player x0 y0)

  (declare (ignore player))
  
  (let ((light t) ;; fix later
	(y1 (- y0 (randint 4)))
	(y2 (+ y0 (randint 3)))
	(x1 (- x0 (randint 11)))
	(x2 (+ x0 (randint 11))))

;;    (warn "Point (~a ~a)  --> (~a ~a) (~a ~a)" x0 y0 x1 y1 x2 y2) 
    
    (generate-room dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) light)
    (generate-draw dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) +feature-wall-outer+)
    (generate-fill dungeon x1 y1 x2 y2 +feature-floor+)

    (let ((dx1 (+ 2 x1))
	  (dy1 (+ 2 y1))
	  (dx2 (- x2 2))
	  (dy2 (- y2 2)))
      
      (when (and (> (- dy2 dy1) 2)
		 (> (- dx2 dx1) 2))
	(warn "Generating shop at ~s,~s" dx1 dy1)
	(generate-fill dungeon dx1 dy1 dx2 dy2 +feature-perm-extra+)

	(setf (cave-feature dungeon (1+ dx1) dy1) +feature-shop-head+)))
    
;;    (generate-draw dungeon 4 4 20 20 +feature-wall-outer+)
;;    (generate-fill dungeon 4 4 20 20 +feature-floor+)
    
    ;; skip hacks
    ))

