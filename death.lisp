;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: death.lisp - the boggling concept: death
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun %centred-string (str max-len)
  "Tries to return a centred version of the string."
  (format nil (format nil "~~~a:@<~a~~>" max-len str)))

;;(trace %centred-string)

(defun %pretty-date-line (univ-time)
  "Returns a pretty printed date-line."
  (let ((decoded-vals (cl:decode-universal-time univ-time)))
    (format nil "~a" decoded-vals)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (bt:define-binary-struct (hs-entry (:conc-name hs-entry.)) ()
    
    (version nil) ;; string
    (variant nil) ;; string
    
    (name nil)    ;; string
    (race nil)    ;; string-id
    (class nil)   ;; string-id
    (sex nil)     ;; string
    
    ;; directly savable
    (max-xp      0 :bt bt:u32)
    (depth       0 :bt bt:u16)
    (max-depth   0 :bt bt:u16)
    (turn        0 :bt bt:u32)
    (gold        0 :bt bt:u32)
    (score       0 :bt bt:u32)  
    
    (date        0 :bt u64) ;; time of death
    )
)

(defun make-high-score (variant player)
  "Returns a high-score entry for the player."

  (check-type variant variant)
  (check-type player player)
  
  (let ((hs (make-hs-entry)))
    (setf (hs-entry.name hs) (player.name player)
	  (hs-entry.max-xp hs) (player.max-xp player)
	  (hs-entry.depth hs) (player.depth player)
	  (hs-entry.max-depth hs) (player.max-depth player)
	  (hs-entry.turn hs) (variant.turn variant)
	  (hs-entry.gold hs) (player.gold player)
	  (hs-entry.score hs) (calculate-score variant player)
	  (hs-entry.date hs) (cl:get-universal-time))
    hs))
	  

(defun print-tomb (player &optional (dead-from "three mad dogs on steroids"))
  "Prints a tombstone."

  (check-type player player)
  ;; temporary
  (let ((hs (make-high-score *variant* player)))
    (declare (ignore hs))
    (with-open-file (s (game-data-path "dead.txt")
		       :direction :input)
      (loop for x = (read-line s nil 'eof)
	    for i from 0
	    until (eq x 'eof)
	    do
	    ;;	  (warn "Printing ~s on ~d" x i)
	    (c-put-str! x i 0)))

    ;; now let's make things perty
    (let* ((title (get-title-for-level (player.class player) (player.level player)))
	   (class-name (class.name (player.class player)))
	   (name (player.name player))
	   (max-width 31))
    
      (flet ((dump-str (str y x)
	       (c-put-str! (%centred-string str max-width) y x)))

	(dump-str name  6 11)
	(dump-str "the"  7 11)
	(dump-str title  8 11)
	(dump-str class-name 10 11)
	(dump-str (format nil "Level: ~a" (player.level player))
		  11 11)
	(dump-str (format nil "Xp: ~a" (player.max-xp player))
		  12 11)
	(dump-str (format nil "Au: ~a" (player.max-xp player))
		  13 11)
	(dump-str (format nil "Killed on level: ~a" (player.depth player))
		  14 11)
	(dump-str (format nil "by ~a" dead-from)
		  15 11)
	;; add time
	;;      (dump-str (%pretty-date-line (hs-entry.date hs)) 17 11)
    
	nil))))
 
  