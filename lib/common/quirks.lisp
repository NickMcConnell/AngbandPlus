;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

common/quirks.lisp - various variant settings which should be common.
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

#||
(defun comm-pre-init ()
  
  ;; hacks descriptions for equipment.  Should be in vanilla
  (setf (get 'eq.weapon   'description) "Wielding"
	(get 'eq.bow      'description) "Shooting"
	(get 'eq.l-ring   'description) "On left hand"
	(get 'eq.r-ring   'description) "On right hand"
	(get 'eq.neck     'description) "Around neck"
	(get 'eq.light    'description) "Light source"
	(get 'eq.armour   'description) "On body"
	(get 'eq.cloak    'description) "About body"
	(get 'eq.shield   'description) "On arm"
	(get 'eq.head     'description) "On head"
	(get 'eq.glove    'description) "On hands"
	(get 'eq.feet     'description) "On feet"
	(get 'eq.backpack 'description) "On back")
  )

(register-variant-common& :before-game-init #'comm-pre-init) 
||#