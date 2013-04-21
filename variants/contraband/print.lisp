;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/print.lisp - various printing methods
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defun print-quests (var-obj player settings)
;;  (declare (ignore player))
  (clear-window *cur-win*)
  (let ((title-row 2)
	(title-col 15)
	(title-attr +term-blue+))
    
    (put-coloured-str! title-attr "Quests" title-col title-row)
    (put-coloured-str! title-attr "======" title-col (1+ title-row))

    (let ((row (+ title-row 3)))
      (loop for x being the hash-values of (variant.quests var-obj)
	    do
	    (when (eq (quest.state x) :active)
	      (put-coloured-str! title-attr (quest.name x) title-col (incf row))
	      (setf row (print-text! title-col (incf row) +term-green+ (quest.desc x) :end-col 45))
	      (incf row))))
    
    (pause-last-line!)
    ))


(defmethod display-player-skills ((variant contraband) player term settings)

  (let* ((row (if settings
		  (slot-value settings 'skills-y)
		  10))
	 (col (if settings
		  (slot-value settings 'skills-x)
		  42))
	 (value-attr (if settings
			 (slot-value settings 'value-attr)
			 +term-l-green+))
	 (sk-attr (if settings
		      (slot-value settings 'title-attr)
		      +term-white+))
	 (counter 0))

    ;; maybe sort on score
    (loop for x across (variant.skills variant)
	  for disp-row = (+ row counter)
	  do
	  (when (and x (< counter 20)) ;; display max 20
	    (let ((score (gethash (con/skill.slot x) (player.skills player))))
	      (when (plusp score)
		(incf counter)
		(put-coloured-str! +term-green+ (format nil "~2,'0d" score)
				   col disp-row)
		(put-coloured-str! sk-attr (con/skill.id x) (+ 4 col) disp-row)
		
		))))
    
    player))

(defmethod display-player-combat-ratings ((variant contraband) player term settings)
  ;; nothing
  (declare (ignore player term settings))
  t)
    

(defmethod print-object ((inst quest) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
           (quest.id inst) (quest.state inst))
   inst))

