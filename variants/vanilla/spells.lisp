;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: variants/vanilla/spells.lisp - spell-effects
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(defun light-room! (dungeon x y)
  "Lights the room."
  (let ((coord (cave-coord dungeon  x y)))
    (bit-flag-add! (coord.flags coord) +cave-glow+))

  ;; light neighbours
  t)

(defun light-area! (dungeon pl dmg radius)
  "Lights the area."
  (declare (ignore dmg radius))
  ;; unless blind
  (c-print-message! "You are surrounded by a white light.")
  ;; skip dmg
  (light-room! dungeon (location-x pl) (location-y pl))
  t)
