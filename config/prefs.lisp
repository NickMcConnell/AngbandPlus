;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: config/prefs.lisp - main file for user preferences.

this file defines preferences done the lispy way

|#

(when (or (eq (get-system-type) 'xaw)
	  (eq (get-system-type) 'x11))
  
  (read-pref-file "x11-keys.lisp"))

(when (eq (get-system-type) 'gcu)
  (read-pref-file "gcu-keys.lisp"))

(when (eq (get-system-type) 'win)
  (read-pref-file "win-keys.lisp"))

(when (using-sound?)
  (read-pref-file "sound.lisp"))
