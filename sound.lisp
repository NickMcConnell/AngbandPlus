;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: sound.lisp - simple functions that deals with sound
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(defconstant +sound-hit+ 1)
(defconstant +sound-miss+ 2)
(defconstant +sound-flee+ 3)
(defconstant +sound-drop+ 4)
(defconstant +sound-kill+ 5)
(defconstant +sound-level+ 6)
(defconstant +sound-death+ 7)
(defconstant +sound-study+ 8)
(defconstant +sound-teleport+ 9)
(defconstant +sound-shoot+ 10)
(defconstant +sound-quaff+ 11)
(defconstant +sound-zap+ 12)
(defconstant +sound-walk+ 13)
(defconstant +sound-tpother+ 14)
(defconstant +sound-hitwall+ 15)
(defconstant +sound-eat+ 16)
(defconstant +sound-store1+ 17)
(defconstant +sound-store2+ 18)
(defconstant +sound-store3+ 19)
(defconstant +sound-store4+ 20)
(defconstant +sound-dig+ 21)
(defconstant +sound-opendoor+ 22)
(defconstant +sound-shutdoor+ 23)
(defconstant +sound-tplevel+ 24)

(defun play-sound (type)
  "Plays the given sound(s) for type."
  (when (using-sound?)
    (let ((sound-num 8))
      (c-term-xtra& sound-num type))))


(defun define-sound (num &rest sounds)
  "Defines a sound.  Returns nil when sound is not loaded."
  #-using-sound
  (declare (ignore num sounds))
  #-using-sound
  nil
  
  #+using-sound
  (when (using-sound?)
    (let ((base-path "./lib/sound/"))
  
      (dolist (i sounds)
	(c-load-sound& num (concatenate 'string base-path i))))
    num))
  

(defun using-sound? ()
  "Returns either T or NIL."
  #+using-sound
  t
  #-using-sound
  nil)
