;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)


(define-room "simple-room" #'common-make-simple-room)
(define-room "overlapping-room" #'common-make-overlapping-room)

(register-information& "status-roll" 100 ;; what's the roll of status
		       "status-cap" 100) ;; what's max status

(register-information& "which-town" "bartertown")

(define-floor-type* "nothing" "nothing"
  :numeric-id 73
  :text-attr +term-white+
  :text-char #\Space
  :flags 0
  :x-attr (tile-file 0) :x-char (tile-number 0))
