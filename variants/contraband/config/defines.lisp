;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/contraband/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defconstant +common-backpack-size+ 23)

(define-object-kind 
    "backpack" "backpack" :numeric-id 750
    :x-attr #\w :x-char #\&
    :depth 0 :rarity nil :chance #(0 0 0 0)
    :locale #(0 0 0 0) :weight nil
    :cost 1200 :the-kind '<container>
    :on-create #'(lambda (item)
		   (let ((container (make-container +common-backpack-size+)))
		     (setf (aobj.contains item) container)
		     t))
    ;;:events (list :backpack-creation)
    )

(define-room "simple-room" #'common-make-simple-room)
(define-room "overlapping-room" #'common-make-overlapping-room)

(register-information& "status-roll" 100 ;; what's the roll of status
		       "status-cap" 100) ;; what's max status

;;(register-information& "which-town" "vanilla")
(register-information& "which-town" "bartertown")
