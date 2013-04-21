;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/common/parameters.lisp - parameters common to all variants
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

;; langband default parameters
    
(define-parameter :initial-backpack
    "Initial backpack"
  "This value sets which backpack which is default for starting
characters"
  :backpack  ;; default value
  (list (make-parameter-alternative :backpack
				    "A normal backpack"
				    "This is the vanilla backpack with room for 23 objects.")
	(make-parameter-alternative :small-backpack
				    "A small backpack"
				    "This is a smaller backpack with only room for 10 objects.")))

(define-parameter :backpack-constant-p
    "Can the backpack be changed?"
  "This value sets whether the character can choose appropriate
backpack himself  or if it is set in stone, ie vanilla."
  :yes  ;; default value
  (list (make-parameter-alternative :yes
				    "Backpack is constant"
				    "You cannot change backpack, just like in vanilla.")
	(make-parameter-alternative :no
				    "Backpack is not constant"
				    "You can buy and change backpack like other equipment.")))

(define-parameter :equipment-organisation
    "How are equipment slots organised?"
  "With this parameter you can change how equipment is handled.
Currently only available in vanilla mode."
  :vanilla  ;; default value
  (list (make-parameter-alternative :vanilla
				    "Vanilla equipment org."
				    "weapon, bow, armour, shield, light and cloak slots.")))


	
	