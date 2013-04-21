;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: death.lisp - the boggling concept: death (and high-scores)
Copyright (c) 2001-2003 - Stig Erik Sandø

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

(defmethod produce-high-score-object ((variant variant) (player player))
  "Returns a high-score entry for the player."

  (flet ((to-string (arg)
	   (string-downcase (string arg))))
  
    (let ((race-key (to-string (race.id (player.race player))))
	  (class-key (to-string (class.id (player.class player))))
	  (gender-key (gender.id (player.gender player)))
	  (death-reason (if (and (stringp (player.dead-from player)) (> (length (player.dead-from player)) 1))
			    (player.dead-from player)
			    "Unknown"))
	  (hs (make-hs-entry)))
      
      (setf
       (hs-entry.version hs) *engine-version*
       (hs-entry.variant hs) (variant.name variant)
       (hs-entry.name hs) (player.name player)
       (hs-entry.race hs) race-key
       (hs-entry.class hs) class-key
       (hs-entry.gender hs) gender-key
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

(defmethod save-object ((variant variant) (hs hs-entry) (str l-binary-stream) indent)
  "Saves the given high-score entry to the given stream."
  (declare (ignore indent))

  (let ((stream (lang.stream str)))
    (check-type stream stream)
  
    (bt:write-binary 'hs-entry stream hs)
    (%bin-save-string (hs-entry.version hs) stream)
    (%bin-save-string (hs-entry.variant hs) stream)
    
    (%bin-save-string (hs-entry.name hs) stream)
    (%bin-save-string (hs-entry.race hs) stream)
    (%bin-save-string (hs-entry.class hs) stream)
    (%bin-save-string (hs-entry.gender hs) stream)
    (%bin-save-string (hs-entry.cause-of-death hs) stream)
    ))

(defmethod load-object ((variant variant) (type (eql :hs-entry)) (str l-binary-stream))
  "Loads a high-score entry from a stream and returns the entry."
  (let* ((stream (lang.stream str))
	 (obj (bt:read-binary 'hs-entry stream)))
    (setf (hs-entry.version obj) (%bin-read-string stream)
	  (hs-entry.variant obj) (%bin-read-string stream)
	  (hs-entry.name obj) (%bin-read-string stream)
	  (hs-entry.race obj) (%bin-read-string stream)
	  (hs-entry.class obj) (%bin-read-string stream)
	  (hs-entry.gender obj) (%bin-read-string stream)
	  (hs-entry.cause-of-death obj) (%bin-read-string stream)
	  )
    
    obj))

(defmethod get-high-scores (variant fname)
  (check-type variant variant)
  
  (let ((hscores '())
	(the-path (pathname fname)))
    (when (probe-file the-path)
      (handler-case 
	  (bt:with-binary-file (str the-path
				    :direction :input)
	    (let ((bt:*endian* :little-endian)
		  (the-lang-stream (make-instance 'l-binary-stream :stream str)))

	    (loop for obj = (load-object variant :hs-entry the-lang-stream)
		  when obj
		  do (push obj hscores))))
	(end-of-file (co)
	  (declare (ignore co))
	  nil)))
    
    (nreverse hscores)))

(defmethod display-high-scores ((variant variant) highscore-list &key (current 0) (use-term t))
  ;; FIX: when dealing with several screenfuls
  
  (check-type current number)
  (assert (listp highscore-list))

  ;; this line provokes a bug, the line is cut off!
  (put-coloured-line! +term-l-green+ "---------------------------------------------------------------------" 0 0)
  
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
	    (when (and (typep i 'hs-entry) (< cnt 20)) ;; limit to best 20.
;;	      (warn "printing ~s" (get-str i cnt)) 
	      (if use-term
		  (put-coloured-line! (if (= cnt current) +term-l-blue+ +term-l-green+)
				      (get-str i cnt)
				      0 (1+ cnt))
		  (write-string (get-str i cnt) *standard-output*))))
      ;; returns how many entries it printed. 
      len)))


(defun %sort-hscores (hlist)
  (stable-sort hlist #'> :key #'hs-entry.score))
  
(defmethod save-high-score& ((variant variant) hs fname)
  "Returns number of hs."

  (let ((hscores (get-high-scores variant fname))
	(the-path (pathname fname)))

    (let* ((sorted-hscores (%sort-hscores (cons hs hscores)))
	   (pos (position hs sorted-hscores :test #'eq)))

;;      (warn "bob ~s ~s" hs sorted-hscores)
          
      (bt:with-binary-file (str the-path
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	(let ((bt:*endian* :little-endian)
	      (the-lang-stream (make-instance 'l-binary-stream :stream str)))
	  
	  (dolist (i sorted-hscores)
	    (check-type i hs-entry)
	    (save-object variant i the-lang-stream nil))))

;;    (display-high-scores variant hscores :current hs)

      pos)))

(defmethod print-tomb ((variant variant) (player player))
  "Prints a tombstone."

  ;; temporary
  (let ((hs (produce-high-score-object variant player)))

    (unless (eq (get-system-type) 'sdl)
      
;;    (declare (ignore hs))
      (with-open-file (s (game-data-path "dead.txt")
			 :direction :input)
	(loop for x = (read-line s nil 'eof)
	      for i from 0
	      until (eq x 'eof)
	      do
	      ;;	  (warn "Printing ~s on ~d" x i)
	      (put-coloured-str! +term-white+ x 0 i))))
    

    ;; now let's make things perty
    (let* ((title (get-title-for-level (player.class player) (player.level player)))
	   (class-name (class.name (player.class player)))
	   (name (player.name player))
	   (max-width 31))

      (unless (eq (get-system-type) 'sdl)
      (flet ((dump-str (str y x)
	       (put-coloured-str! +term-white+ (%centred-string str max-width) x y)))

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
    
	nil))

      (when (eq (get-system-type) 'sdl)
	(let* ((is-male? (if (eq (gender.symbol (player.gender player)) '<male>)
			     t nil))
	       (pronoun (if is-male? "he" "she"))
	       (owning (if is-male? "his" "her"))
	       (text (format nil
			     #.(concatenate 'string "And so it has come to pass, ~a the ~a ~a has died.  "
					    "The might of Morgoth's armies has so far proven too strong "
					    "for ~a and ~a many ancestors. "
					    "Only ~a winters old, but already of rank ~a, ~a fought the "
					    "evils of Angband valiantly.  Ancestors mourn the loss of yet "
					    "another hapless adventurer.  ~a was killed "
					    "by a ~a.  The main claim to fame was ~a brutal killing of "
					    "innocent townspeople, and ~a will not be missed.")
			     name title class-name
			     name
			     owning
			     17 (hs-entry.level hs) pronoun
			     name
			     (hs-entry.cause-of-death hs) owning pronoun)))
	  (print-text! 10 27 +term-white+ text
		       :end-col (- (get-frame-width +full-frame+) 10))))
      
      )))

(defmethod arrange-game-exit& ((variant variant) player)
  "Organises things dealing with death of a player..
Thanks for all the fish."


  (let* ((hs (produce-high-score-object variant player))
	 (home-path (variant-home-path variant))
	 (fname (concatenate 'string home-path "high-scores"))
	 (enter-high-score (if (eq (player.leaving? player) :quit) nil t))
	 (hs-pos 12)
	 (alive? (not (player.dead? player)))
	 ) ;; must do something smart here
   
    (lbsys/make-sure-dirs-exist& home-path)

    ;; first we save quitting or dead character
    (when-bind (func (get-late-bind-function 'langband 'save-the-game))
      #-langband-release
      (funcall func variant player (if alive? *level* nil) :format :readable)
      (funcall func variant player (if alive? *level* nil) :format :binary))
    
    (when enter-high-score
	;;    (warn "writing to ~s" fname)
	(setf hs-pos (save-high-score& variant hs fname)))

    (with-full-frame ()
      (when (not alive?)
	(put-coloured-line! +term-white+ "Oops.. you died.. " 0 0)
    
	(clear-window-from +full-frame+ 0)
	(paint-gfx-image& "other/thedead.png" 0 0)
	
	(print-tomb variant player)
      
	(pause-last-line!))
      
      (let ((hlist (get-high-scores variant fname)))
	(unless enter-high-score
	  (setf hlist (%sort-hscores (cons hs hlist)))
	  (setf hs-pos (position hs hlist)))
	(clear-window-from +full-frame+ 0)
	(display-high-scores variant hlist :current hs-pos)
	)
      
      (pause-last-line!))
    
    t))
