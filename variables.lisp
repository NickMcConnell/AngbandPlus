;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variables.lisp - global variables for the game code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the variables (DEFVAR) in the game.
ADD_DESC: The file should be kept small.

|#

(in-package :langband)


(defvar *game-parameters* (make-hash-table :test #'eq)
  "a table with keyword game-parameters")

(defvar *game-settings* (make-hash-table :test #'eq)
  "a table with settings for various parts of the game.")

(defcustom *turn* u-fixnum 0
  "Keeps track of which turn it is.")

(defvar *level-rating* 0
  "not used.. will be replaced by better system")


(defvar *alloc-table-objects* nil
  "table used for figuring out objects to allocate")
(defvar *alloc-table-monsters* nil
  "table used for figuring out monsters to allocate")


(defvar *object-kind-table*
  (make-hash-table :test #'eql)
  "hash-table with all object kinds")

(defvar *object-kind-table-numeric*
  (make-hash-table :test #'eql)
  "hash-table with all object kinds")


(defvar *monster-kind-table*
  (make-hash-table :test #'equal)
  "table with all monsters (numeric ids)")

(defvar *monster-kind-table-numeric*
  (make-hash-table :test #'equal)
  "table with all monsters (numeric ids)")

(defvar *floor-feature-table*
  (make-hash-table :test #'eql)
  "read from the floor info file")

(defvar *objects-by-level* nil "all objects sorted by level")
(defvar *monsters-by-level* nil "all monsters sorted by level")


(defvar *class-table*
  (make-hash-table :test #'equal)
  "the allowed character classes")
(defvar *race-table*
  (make-hash-table :test #'equal)
  "the table with possible races")


;; two very important variables :-)
(defvar *dungeon* nil "global dungeon object")
(defvar *player* nil "the player object")

;; move these later.. 
(defcustom *dungeon-height* u-fixnum 66 "settable size of dungeon.")
(defcustom *dungeon-width* u-fixnum 198 "settable size of dungeon.")

(defcustom *redraw* u-fixnum 0 "what to redraw, bitfield")
(defcustom *update* u-fixnum 0 "what to update, bitfield")

(defvar *room-builders*
  (make-hash-table :test #'eql)
  "table of functions to build rooms")

(defvar *cur-dun* nil
  "a dynamic variable which is set to an object
of type DUN-DATA (see: dungeon.lisp) and is valid
throughout dungeon-generation")

(defvar *hitpoint-warning* 3
  "Value in [0..9] of when to warn about hitpoint-losses")

;; fix me later..
(defvar *sort-values* (make-hash-table :test #'eql))

(defun get-sort-value (key)
  "Returns a number for the key, or NIL."
  (gethash key *sort-values*))

(defvar *last-console-line* 23 "just a dummy for later use.")
