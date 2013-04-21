;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: util.lisp - utility-code dependant on other code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Convenient utilities which is based on several
ADD_DESC: classes and must be loaded late.

|#

(in-package :langband)

(defun get-item-table (dungeon player which-table)
  "Returns item-table or NIL."
  
  (ecase which-table
    (:floor (cave-objects dungeon 
			  (player.loc-x player)
			  (player.loc-y player)))
    (:backpack (aobj.contains (player.inventory player)))
    (:equip (player.eq player))))

(defun save-game (dun pl &optional (filename "save.file"))
  "Tries to save a game."

  (with-open-file (s (pathname filename)
		     :direction :output
		     :if-exists :supersede)
    (let ((*print-case* :downcase))

      (dump-object pl s :save)
      (dump-object dun s :save)
      ))
  
  (values))

(defmethod dump-object ((obj player) stream style)
  (declare (ignore stream style))
  (format t "~a~%" (get 'player 'struct-slots)))

(defmethod dump-object ((obj dungeon) stream style)
  (declare (ignore stream style))
  (format t "~a ~a~%" (dungeon.rooms obj) (dungeon.monsters obj)))
