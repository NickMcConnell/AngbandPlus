;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: config/prefs.lisp - main file for user preferences.

this file defines preferences done the lispy way

|#

(in-package :org.langband.engine)

;; read when engine is loaded, can contain preferences
;; may be obsolete
#||
(when (or (eq (get-system-type) 'xaw)
	  (eq (get-system-type) 'x11)
	  (eq (get-system-type) 'gtk))
	    
  (read-pref-file "x11-keys.lisp"))

(when (eq (get-system-type) 'gcu)
  (read-pref-file "gcu-keys.lisp"))

(when (eq (get-system-type) 'win)
  (read-pref-file "win-keys.lisp"))

(when (eq (get-system-type) 'sdl)
  (read-pref-file "sdl-keys.lisp"))

(when (using-sound?)
  (read-pref-file "sound.lisp"))

||#
