;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variants/vanilla/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(define-normal-event ()
    :backpack-creation
  :on-create
  #'common-creating-backpack)


(define-object-kind 
    :backpack "backpack" :numeric-id 750
    :x-attr #\w :x-char #\&
    :depth 0 :rarity nil :chance #(0 0 0 0)
    :locale #(0 0 0 0) :weight nil
    :cost 1200 :obj-type '(<container> <backpack>)
    :the-kind '<container>
    :events (list :backpack-creation))

(define-room "simple-room" #'common-make-simple-room)
(define-room "overlapping-room" #'common-make-overlapping-room)
