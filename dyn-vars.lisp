;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: dyn-vars.lisp - global dynamic variables for the game code
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the variables (DEFVAR) in the game.
ADD_DESC: The file should be kept small.

|#

(in-package :org.langband.engine)


(defvar *game-parameters* (make-hash-table :test #'eq)
  "a table with keyword game-parameters")

(defvar *game-settings* (make-hash-table :test #'eq)
  "a table with settings for various parts of the game.")


;; four very important variables :-)
(defvar *variant* nil "variant in use.  one should not rebind this
too frequently.")
(defvar *level* nil "The current level, for good and bad.")
(defvar *dungeon* nil "global dungeon object")
(defvar *player* nil "the player object")


(defcustom *redraw* u-fixnum 0 "what to redraw, bitfield")
(defcustom *update* u-fixnum 0 "what to update, bitfield")

(defvar *cur-dun* nil
  "a dynamic variable which is set to an object
of type DUN-DATA (see: dungeon.lisp) and is valid
throughout dungeon-generation")

(defvar *hitpoint-warning* 3
  "Value in [0..9] of when to warn about hitpoint-losses")

(defvar *last-console-line* 23 "just a dummy for later use.")

(defvar *global-event-table* (make-hash-table :test #'equal))

(defvar *engine-source-dir* #+langband-development "./"
	#-langband-development (translate-logical-pathname "langband:"))
(defvar *engine-config-dir*
  #+unix
  (progn
    #+langband-development (pathname "./config/")
    #-langband-development "/var/lib/games/langband/")
  #+win32
  (pathname "c:/cygwin/home/default/langband/config/")
  #-(or unix win32)
  (pathname "./config/"))
