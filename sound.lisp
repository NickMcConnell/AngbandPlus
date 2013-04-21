;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: sound.lisp - simple functions that deals with sound
Copyright (c) 2001-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;;; these constants were snatched from the old defines.h
;;; feel free to change them

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

(defconstant +sound-intro+ 30)

(defvar *sound-table* (make-hash-table :test #'equal))
(defvar *music-table* nil)

(defun init-sound-system& (size)
  ;; put init here later
  (c-init-sound-system& size)
  (setf *music-table* (make-array size :fill-pointer 0))
  t)

(defun play-sound (type)
  "Plays the given sound(s) for type."
  (when (using-sound?)
    (let (;;(xtra-code-sound 8)
	  (sounds (gethash type *sound-table*)))
      (when sounds
	;; add sounds back in
	(c-play-sound-effect (rand-elm sounds))

	))))


(defun load-music (fname)
  "Tries to load the fname."
  (when (using-sound?)
    (let ((path (concatenate 'string *engine-data-dir* "audio/music/" fname)))
      (when (probe-file path)
	(let ((idx (vector-push path *music-table*)))
	  (lb-ffi:c-load-music-file& path idx)
	  idx)))))


(defun play-music (music)
  "Tries to play given music."

  (let ((idx (etypecase music
	       (integer music)
	       (string (find music *music-table* :test #'equal)))))

    (when (and (integerp idx) (>= idx 0) (< idx (length *music-table*)))
      (warn "Play ~s" idx)
      (lb-ffi:c-play-music-file idx))))
 

(defun define-sound-effect (key &rest sounds)
  "Defines a sound.  Returns nil when sound is not loaded."

  (when (using-sound?)
    (let ((base-path (concatenate 'string *engine-data-dir* "audio/effects/"))
	  (current-sounds (gethash key *sound-table*)))
      
  
      (dolist (i sounds)
	(let ((idx (org.langband.ffi:c-load-sound-effect& (concatenate 'string base-path i) -1)))
	  (pushnew idx current-sounds :test #'equal)))

      (setf (gethash key *sound-table*) current-sounds)
      
      (length current-sounds))
    ))
  

(defun using-sound? ()
  "Returns either T or NIL."
  ;; fix: cache value to avoid ffi-call
  (if (= 0 (c-get-sound-status))
      nil
      t))

