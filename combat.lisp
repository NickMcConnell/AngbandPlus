;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: combat.lisp - the combat-system
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(defun check-for-hits ()

  )

(defun attack-location! (dun pl x y)
  "attacks a given location.."

  (let ((monsters (cave-monsters dun x y)))
    (when monsters
      (let ((the-monster (car monsters)))
;;	(describe the-monster)
	(if (/= (random 2) 1)
	    (c-print-message (format nil "You miss the ~a." (monster.name the-monster)))
	    (progn
	      (decf (amon.cur-hp the-monster) (random 10))
	      (c-print-message (format nil "You hit the ~a." (monster.name the-monster)))
	      
	      (when (< (amon.cur-hp the-monster) 0)
		
		(c-print-message (format nil "The ~a dies.." (monster.name the-monster)))
		
		(let ((monster-xp (monster.xp (amon.kind the-monster))))
		  (increase-xp! pl (if monster-xp monster-xp 0)))
		
		(setf (cave-monsters dun x y) nil)
		(light-spot! dun x y)
		)))
	      
	))))
