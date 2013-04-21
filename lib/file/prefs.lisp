;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/file/prefs.lisp - main file for user preferences.

this file defines preferences done the lispy way

|#

(when (or (eq (get-system-type) 'xaw)
	  (eq (get-system-type) 'x11))
  
  (read-pref-file "lib/file/x11-keys.lisp"))

(when (using-sound?)
  (read-pref-file "lib/file/sound.lisp"))
