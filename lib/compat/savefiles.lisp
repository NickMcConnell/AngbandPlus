
#|

DESC: lib/compat/savefiles.lisp - reads standard vanilla savefiles
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#

(in-package :org.langband.engine)


(bt:define-binary-struct vanilla-save-header ()
  (major nil :binary-type bt:u8)
  (minor nil :binary-type bt:u8)
  (patch nil :binary-type bt:u8)
  (extra nil :binary-type bt:u8)
  )

(let ((xor-byte 0)
      (checking nil))
  (defun compat-get-byte (stream)
    (let* ((ch (logand (common-lisp:read-byte stream) #xff))
	   (retval (logxor ch xor-byte)))
      (setf xor-byte ch)
      (when checking
	(warn "c is ~s, retval is ~s, xor is ~s" ch retval xor-byte))
      
      retval))
  (defun set-checking (val)
    (setf checking val)))

(defun compat-skip-bytes (stream number)
  (when (plusp number)
    (loop for i from 1 to number
	  do (compat-get-byte stream))))

(defun compat-read-u16b (stream)
  (logior (compat-get-byte stream)
	  (ash (compat-get-byte stream) 8)))

(defun compat-read-u32b (stream)
  (logior (compat-get-byte stream)
	  (ash (compat-get-byte stream) 8)
	  (ash (compat-get-byte stream) 16)
	  (ash (compat-get-byte stream) 24)))

(defun compat-read-string (stream max-len)

  (let ((collected '())
	(count 0))
    (block collecting
      (loop
       (let ((next (compat-get-byte stream)))
	 (when (< count max-len)
	   (push next collected)
	   (incf count))
	 (when (= 0 next)
	   (return-from collecting)))))
    (coerce (nreverse (mapcar #'code-char collected)) 'string)
    ))
    
  

(defun compat-read-options (stream)

  (compat-skip-bytes stream 16)

  (let* ((delay (compat-get-byte stream))
	 (warning (compat-get-byte stream)))
    
    (warn "delay ~s"delay)
    (warn "warn ~s" warning))

  (compat-read-u16b stream)
  
  (compat-skip-bytes stream 128))

(defun compat-read-random-info (stream)

  (let* ((rand-tmp (compat-read-u16b stream))
	(rand-place (compat-read-u16b stream)))
    (warn "Rand-tmp ~s" rand-tmp)
    (warn "Place ~s" rand-place))

  (compat-skip-bytes stream 252))

(defun compat-read-msgs (stream)
  (let ((num-msgs (compat-read-u16b stream)))
    (loop for i from 1 to num-msgs
	  do
	  (let* ((msg (compat-read-string stream 128))
		 (msg-type (compat-read-u16b stream)))
;;	    (warn "Message ~s: ~s" msg-type msg)
	    ))
    ))

(defun compat-read-lore (stream)
  (compat-skip-bytes stream 48))

(defun compat-read-savefile& (fname)
  "Reads savefiles from 2.9.3"
  
  (flet ((read-u16 (stream)
	   (compat-read-u16b stream))
	 (read-u32 (stream)
	   (compat-read-u32b stream))
	 (read-byte (stream)
	   (compat-get-byte stream)))
		   
  
  (bt:with-binary-file (s (pathname fname)
			  :direction :input)
    (let ((bt:*endian* :little-endian))
    (let* ((header (bt:read-binary 'vanilla-save-header s)))
      (warn "Header says ~s ~s ~s ~s"
	    (vanilla-save-header-major header)
	    (vanilla-save-header-minor header)
	    (vanilla-save-header-patch header)
	    (vanilla-save-header-extra header)
	    ))

    (warn "Extra: ~s" (read-u32 s))
    (warn "When: ~s"  (read-u32 s))
    (warn "Lives: ~s" (read-u16 s))
    (warn "Saves: ~s" (read-u16 s))
    (warn "Tmp: ~s"   (read-u32 s))
    (warn "Tmp: ~s"   (read-u32 s))

    ;; skip random stuff
    (compat-read-random-info s)

    ;; options
    (compat-read-options s)

    ;; messages
    (compat-read-msgs s)

    (let ((mon-mem-number (read-u16 s)))
      (warn "Knows of ~s races" mon-mem-number)
      (loop for i from 0 to (1- mon-mem-number)
	    do (compat-read-lore s)))

    (let ((obj-mem-number (read-u16 s)))
      (loop for i from 0 to (1- obj-mem-number)
	    do (read-byte s)))

    (let ((quest-num (read-u16 s)))
      (warn "Quests ~s" quest-num)
      (loop for i from 0 to (1- quest-num)
	    do (compat-skip-bytes s 4)))
      

    (let ((quest-num (read-u16 s)))
      (warn "Artifacts ~s" quest-num)
      (loop for i from 0 to (1- quest-num)
	    do (compat-skip-bytes s 4)))
      
    (let* ((full-name (compat-read-string s 32))
	   (died-from (compat-read-string s 80)))
      
      ;; history
      (dotimes (i 4)
	(compat-read-string s 60))

      (let* ((race (compat-get-byte s))
	     (class (compat-get-byte s))
	     (gender (compat-get-byte s))
	     (tmp (compat-get-byte s)) ;; ignore
	     (hit-die (compat-get-byte s))
	     (xp-fact (compat-get-byte s))
	     (age (read-u16 s))
	     (height (read-u16 s))
	     (weight (read-u16 s))
	     )

	;; read stats here
	(compat-skip-bytes s 24)
	;; skip other bytes
	(compat-skip-bytes s 24)

	(let* ((gold (read-u32 s))
	       (max-xp (read-u32 s))
	       (cur-xp (read-u32 s))
	       (frac-frac (read-u16 s))
	       (lvl (read-u16 s))
	       (max-hp (read-u16 s))
	       (cur-hp (read-u16 s))
	       (frac-hp (read-u16 s))
	       (max-mana (read-u16 s))
	       (cur-mana (read-u16 s))
	       (frac-mana (read-u16 s))
	       (max-lvl (read-u16 s))
	       (max-depth (read-u16 s))
	       (social-class 0)
	       )
	  ;; skip repair

	  (compat-skip-bytes s 8)
	  (setf social-class (read-u16 s))
	  (compat-skip-bytes s 2)

	  (warn "Player ~s (r: ~s, c: ~s, g: ~s) xp: ~s lvl: ~s, (max hp: ~s, hp: ~s), (max mana: ~s, mana ~s)"
		full-name race class gender cur-xp lvl max-hp cur-hp max-mana cur-mana)
	  
	  ;; skip old rest
	  (compat-skip-bytes s 2)


	  )))
    ;; start of many many flags
    (let* ((flag-blind (read-u16 s))
	   (flag-paralysed (read-u16 s))
	   (flag-confused (read-u16 s))
	   (flag-food (read-u16 s))
	  )

      ;; skip some flags
      (compat-skip-bytes s 4)
      
      (let* ((flag-energy (read-u16 s))
	     (flag-fast (read-u16 s))
	     (flag-slow (read-u16 s))
	     (flag-afraid (read-u16 s))
	     (flag-cut (read-u16 s))
	     (flag-stun (read-u16 s))
	     (flag-poison (read-u16 s))
	     (flag-image (read-u16 s))
	     (flag-protevil (read-u16 s))
	     (flag-invuln (read-u16 s))
	     (flag-hero (read-u16 s))
	     (flag-shero (read-u16 s))
	     (flag-shield (read-u16 s))
	     (flag-blessed (read-u16 s))
	     (flag-tim-invis (read-u16 s))
	     (flag-word-recall (read-u16 s))
	     (flag-see-infra (read-u16 s))
	     (flag-time-infra (read-u16 s))
	     (flag-opp-fire (read-u16 s))
	     (flag-opp-cold (read-u16 s))
	     (flag-opp-acid (read-u16 s))
	     (flag-opp-elec (read-u16 s))
	     (flag-opp-poison (read-u16 s))
	     (flag-confuse 0)
	     (flag-search 0)
	     (seed-flavour 0)
	     (seed-town 0)
	     (turn 0)
	     )

	;; more stuff
	(setf flag-confuse (compat-get-byte s))
	(compat-skip-bytes s 3)
	(setf flag-search (compat-get-byte s))
	(compat-skip-bytes s 3)

	;; skip more
	(compat-skip-bytes s 40)

	;; skip randarts
	(compat-skip-bytes s 8)
	(compat-skip-bytes s 12)

	(setf seed-flavour (read-u32 s))
	(setf seed-town (read-u32 s))

	;; skip weird stuff
	(compat-skip-bytes s 6)

	;; skip death + feeling
	(compat-skip-bytes s 2)

	;; skip feeling-turn
	(compat-skip-bytes s 4)

	(setf turn (read-u32 s))

	(let ((hp-arr-len (read-u16 s)))
	  (dotimes (i hp-arr-len)
	    (compat-skip-bytes s 2)))

	;; skip spell-info
	(compat-skip-bytes s 24)
	(compat-skip-bytes s 64)
	
	
	))
    
    
    ))
  ))


(pushnew :compatibility-savefiles cl:*features*)
