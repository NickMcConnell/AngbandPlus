;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: death.lisp - the boggling concept: death (and high-scores)
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
    (cause-of-death nil) ;; string
    
    ;; directly savable
    (xp          0 :bt bt:u32)
    (max-xp      0 :bt bt:u32)
    (level       0 :bt bt:u16)
    (depth       0 :bt bt:u16)
    (max-depth   0 :bt bt:u16)
    (turn        0 :bt bt:u32)
    (gold        0 :bt bt:u32)
    (score       0 :bt bt:u32)  
    
    (date        0 :bt u64) ;; time of death
    )
)

(defmethod make-high-score (variant player)
  "Returns a high-score entry for the player."

  (check-type variant variant)
  (check-type player player)

  (flet ((to-string (arg)
	   (string-downcase (string arg))))
  
    (let ((race-key (to-string (race.id (player.race player))))
	  (class-key (to-string (class.id (player.class player))))
	  (sex-key (to-string (player.sex player)))
	  (death-reason (if (and (stringp (player.dead-from player)) (> (length (player.dead-from player)) 1))
			    (player.dead-from player)
			    "Unknown"))
	  (hs (make-hs-entry)))
      
      (setf
       (hs-entry.version hs) "0.1p"
       (hs-entry.variant hs) (variant.name variant)
       (hs-entry.name hs) (player.name player)
       (hs-entry.race hs) race-key
       (hs-entry.class hs) class-key
       (hs-entry.sex hs) sex-key
       (hs-entry.cause-of-death hs) death-reason

       (hs-entry.xp hs) (player.cur-xp player)
       (hs-entry.max-xp hs) (player.max-xp player)
       (hs-entry.level hs) (player.level player)
       (hs-entry.depth hs) (player.depth player)
       (hs-entry.max-depth hs) (player.max-depth player)
       (hs-entry.turn hs) (variant.turn variant)
       (hs-entry.gold hs) (player.gold player)
       (hs-entry.score hs) (calculate-score variant player)
       (hs-entry.date hs) (cl:get-universal-time))
      hs)))

(defun save-hs-entry (hs stream)
  "Saves the given high-score entry to the given stream."
  (check-type hs hs-entry)
  (check-type stream stream)
  
  (bt:write-binary 'hs-entry stream hs)
  (%bin-save-string (hs-entry.version hs) stream)
  (%bin-save-string (hs-entry.variant hs) stream)

  (%bin-save-string (hs-entry.name hs) stream)
  (%bin-save-string (hs-entry.race hs) stream)
  (%bin-save-string (hs-entry.class hs) stream)
  (%bin-save-string (hs-entry.sex hs) stream)
  (%bin-save-string (hs-entry.cause-of-death hs) stream)
  )

(defun load-hs-entry (stream)
  "Loads a high-score entry from a stream and returns the entry."
  (let ((obj (bt:read-binary 'hs-entry stream)))
    (setf (hs-entry.version obj) (%bin-read-string stream)
	  (hs-entry.variant obj) (%bin-read-string stream)
	  (hs-entry.name obj) (%bin-read-string stream)
	  (hs-entry.race obj) (%bin-read-string stream)
	  (hs-entry.class obj) (%bin-read-string stream)
	  (hs-entry.sex obj) (%bin-read-string stream)
	  (hs-entry.cause-of-death obj) (%bin-read-string stream)
	  )
    
    obj))

(defmethod get-high-scores (variant fname)
  (check-type variant variant)
  
  (let ((hscores '())
	(bt:*endian* :little-endian)
	(the-path (pathname fname)))
    (when (probe-file the-path)
      (handler-case 
	  (bt:with-binary-file (str the-path
				    :direction :input)
	    (loop for obj = (load-hs-entry str)
		  when obj
		  do (push obj hscores)))
	(end-of-file (co)
	  (declare (ignore co))
	  nil)))
    
    (nreverse hscores)))

(defmethod display-high-scores (variant highscore-list &key (current 0) (use-term t))
  ;; FIX: when dealing with several screenfuls
  
  (check-type variant variant)
  (check-type current number)
  (assert (listp highscore-list))

  (flet ((get-str (hs num)
	   (format nil "~a. ~a (~a) at dlvl ~a got ~a points"
		   (1+ num)
		   (hs-entry.name hs)
		   (hs-entry.cause-of-death hs)
		   (hs-entry.depth hs)
		   (hs-entry.score hs))))
    
    (declare (dynamic-extent #'get-str))
  
    (let ((len (length highscore-list)))
      
      (loop for i in highscore-list
	    for cnt from 0
	    do
	    (progn
	      (check-type i hs-entry)
	      (if use-term
		  (c-col-put-str! (if (= cnt current) +term-l-blue+ +term-l-green+)
				  (get-str i cnt)
				  cnt 0)
		  (write-string (get-str i cnt) *standard-output*))))
      ;; returns how many entries it printed. 
      len)))

	
  
(defmethod save-high-score& (variant hs fname)
  "Returns number of hs."
  
  (check-type variant variant)

  (let ((hscores (get-high-scores variant fname))
	(bt:*endian* :little-endian)
	(the-path (pathname fname)))

    (let* ((sorted-hscores (stable-sort (cons hs hscores) #'> :key #'hs-entry.score))
	   (pos (position hs sorted-hscores :test #'eq)))

;;      (warn "bob ~s ~s" hs sorted-hscores)
          
      (bt:with-binary-file (str the-path
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	
	(dolist (i sorted-hscores)
	  (check-type i hs-entry)
	  (save-hs-entry i str)))

;;    (display-high-scores variant hscores :current hs)

      pos)))

(defmethod print-tomb (variant player)
  "Prints a tombstone."

  (check-type variant variant)
  (check-type player player)
  ;; temporary
  (let ((hs (make-high-score variant player)))
;;    (declare (ignore hs))
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
	(dump-str (format nil "Level: ~a" (hs-entry.level hs))
		  11 11)
	(dump-str (format nil "Xp: ~a" (hs-entry.xp hs))
		  12 11)
	(dump-str (format nil "Au: ~a" (hs-entry.gold hs))
		  13 11)
	(dump-str (format nil "Killed on level: ~a" (hs-entry.depth hs))
		  14 11)
	(dump-str (format nil "by ~a" (hs-entry.cause-of-death hs))
		  15 11)
	;; add time
	;;      (dump-str (%pretty-date-line (hs-entry.date hs)) 17 11)
    
	nil))))

;;(trace save-high-score&)
