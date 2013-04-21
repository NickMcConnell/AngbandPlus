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

(defun kill-monster! (dun x y monster)
  "Tries to remove the monster at the location."
  (setf (amon.alive? monster) nil
	(dungeon.monsters dun) (delete monster (dungeon.monsters dun))
	(cave-monsters dun x y) nil)
  nil)

(defun cmb-describe-miss (pl the-monster)
  (declare (ignore pl))
  (c-print-message! (format nil "You miss the ~a." (monster.name the-monster)))
  nil)

(defun cmb-hit-monster? (pl the-monster)
  (declare (ignore pl the-monster))
  (/= (random 2) 1))

(defun cmb-inflict-damage (pl the-monster)
  (declare (ignore pl))
  (let ((amount (random 10)))
    (decf (amon.cur-hp the-monster) amount)
    amount))

(defun cmb-describe-hit (pl the-monster)
  (declare (ignore pl))
  (c-print-message! (format nil "You hit the ~a." (monster.name the-monster)))
  nil)

(defun cmb-describe-death (pl the-monster)
  (declare (ignore pl))
  (c-print-message! (format nil "The ~a dies.." (monster.name the-monster)))
  nil)


(defun attack-location! (dun pl x y)
  "attacks a given location.."

  (let ((monsters (cave-monsters dun x y)))
    (when monsters
      (let ((the-monster (car monsters)))
	;; hack
	(play-sound 1)
;;	(describe the-monster)
	(if (cmb-hit-monster? pl the-monster)
	    (cmb-describe-miss pl the-monster)
	    (progn
	      (cmb-inflict-damage pl the-monster)
	      (cmb-describe-hit pl the-monster)
	      	      
	      (when (< (amon.cur-hp the-monster) 0)
		(cmb-describe-death pl the-monster)
		(let ((monster-xp (monster.xp (amon.kind the-monster))))
		  (increase-xp! pl (if monster-xp monster-xp 0)))
		
		(kill-monster! dun x y the-monster)
		;; repaint
		(light-spot! dun x y)
		)))
	      
	))))


