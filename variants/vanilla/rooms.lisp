;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/rooms.lisp - room-builders that should be common
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(defmethod find-appropriate-room ((variant vanilla-variant)
				  (level random-level)
				  player)
  (declare (ignore player))
  ;; hack
  (let* ((some-val (random 100))
	 (the-room nil))

    ;; might surprise someone 
    (cond ((< some-val 15)
	   (setf the-room (funcall (get-room "overlapping-room"))))
	  (t
	   (setf the-room (funcall (get-room "simple-room")))
	   ))
    
    the-room))
