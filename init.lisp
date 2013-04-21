;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: init.lisp - initialisation code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Code which makes sure all tables and settings are as they should 
ADD_DESC: at the start.

||#

(in-package :org.langband.engine)

(defun %to-a-string (obj)
  (etypecase obj
    (string obj)
    (symbol (symbol-name obj))))

(defun game-init& (&optional (ui #+win32 "win" #-win32 "x11"))
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."
  
  (setq cl:*random-state* (cl:make-random-state t))
  (vinfo-init&) ;; init line-of-sight arrays

  (with-open-file (alternative-errors #p"/tmp/langband-warn.txt"
				      :direction :output
				      :if-exists :append
				      :if-does-not-exist :create)
    
    (let (#+hide-warnings
	  (cl:*error-output* alternative-errors)
	  #+hide-warnings
	  (cl:*trace-output* alternative-errors)
	  #+hide-warnings
	  (cl:*standard-output* alternative-errors))

      ;;      (warn "Writing to warn-file")
      ;;      (format t "~&Writing to file~%")


  ;; time to register our lisp
  #+(or cmu allegro clisp lispworks sbcl cormanlisp)
  (c-set-lisp-system! #+cmu 0 #+allegro 1 #+clisp 2 #+lispworks 3 #+sbcl 4 #+cormanlisp 5)
  
  #-(or cmu allegro clisp lispworks sbcl cormanlisp)
  (error "lisp-system ~s unknown for C-side." (lisp-implementation-type))

  #+(and lispworks win32)
  (c-set-hinst! (win32:get-active-window))
;;  (c-set-hinst! (win32:get-window-long (win32:get-active-window) -6))
  
  #+use-callback-from-c
  (arrange-callbacks)

  #+cmu
  (pushnew 'arrange-callbacks ext:*after-gc-hooks*)

  #+sbcl
  (pushnew 'arrange-callbacks sb-ext:*after-gc-hooks*)
  
  (handler-case
      (let ((flag 0))
	(when *use-graphics*
	  (bit-flag-add! flag 1)) ;; graphics
	(init-c-side& (%to-a-string ui)
		      (namestring *engine-config-dir*)
		      flag) ;; no debug, possible gfx
	
	;;(warn "return..")
	#-use-callback-from-c
	(play-game&))
    (langband-quit ()
      (format t "~&Thanks for helping to test Langband.~2%")))
  
  )))

(defun %adjust-screen-size (width height)
  (let ((adjusted-width (- width +start-column-of-map+ 1))
	(adjusted-height (- height +start-row-of-map+ 1)))

    (when *use-graphics*
      (setf adjusted-width (int-/ adjusted-width 2)))
    
;;    (warn "Adjusting to ~s ~s from ~s ~s" adjusted-width adjusted-height *screen-width* *screen-height*)
    (when (or (/= *screen-width* adjusted-width)
	      (/= *screen-height* adjusted-height))
      (setf *screen-width* adjusted-width
	    *screen-height* adjusted-height)
      (when *player*
	(verify-panel *dungeon* *player*))
      t)))

(defun %mouse-clicked (button x y)
  (warn "Button ~s clicked at (~s,~s)" button x y)
  nil)

(defun arrange-callbacks ()
  "Assures that the C-side has necessary callbacks to the Lisp-side."

  ;; not sure if this allegro code is 110% correct
  #+allegro
  (let ((play-ptr  (ff:register-foreign-callable `c-callable-play nil t))
	(size-ptr  (ff:register-foreign-callable `c-callable-resize nil t))
	(mouse-ptr (ff:register-foreign-callable `c-callable-mouseclick nil t))
	)
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
    (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
    )
  
  #+cmu
  (let ((play-ptr  (kernel:get-lisp-obj-address #'play-game&))
	(size-ptr  (kernel:get-lisp-obj-address #'%adjust-screen-size))
	(mouse-ptr (kernel:get-lisp-obj-address #'%mouse-clicked))
	)
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
    (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
    )

  #+sbcl
  (let ((play-ptr  (sb-kernel:get-lisp-obj-address #'play-game&))
	(size-ptr  (sb-kernel:get-lisp-obj-address #'%adjust-screen-size))
	(mouse-ptr (sb-kernel:get-lisp-obj-address #'%mouse-clicked))
	)
;;    (warn "setting callbacks ~d ~d" play-ptr size-ptr)
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
    (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
    )

  #+lispworks
  (let ((play-ptr  (fli:make-pointer :symbol-name "LB_PlayGame"))
	(size-ptr  (fli:make-pointer :symbol-name "LB_AdjustSize"))
	(mouse-ptr (fli:make-pointer :symbol-name "LB_MouseClicked"))
	)
    
    (fli:with-foreign-string (name elm-count byte-count :external-format :ascii)
      "play-game"
      (declare (ignore elm-count byte-count))
      (org.langband.ffi:c-set-lisp-callback! name play-ptr))
    
    (fli:with-foreign-string (name elm-count byte-count :external-format :ascii)
      "adjust-size"
      (declare (ignore elm-count byte-count))
      (org.langband.ffi:c-set-lisp-callback! name size-ptr))
    
    (fli:with-foreign-string (name elm-count byte-count :external-format :ascii)
      "mouse-clicked"
      (declare (ignore elm-count byte-count))
      (org.langband.ffi:c-set-lisp-callback! name mouse-ptr))
      )
  
  #-(or sbcl cmu allegro lispworks)
  (error "No callback arranged for implementation..")
  
  )

;;; hackish thing to start the game ever so long.
(defun a (&optional (ui #+win32 "win" #-win32 "x11") &key (gfx nil))
  ;; to make sure dumps look pretty
  (let ((*package* (find-package :org.langband.engine))
	#+(or cmu) (extensions:*gc-verbose* nil)
	#+(or cmu sbcl) (*compile-print* nil)
	)

    (unless (or (eq ui 'x11) (and (stringp ui)
				  (string-equal ui "x11")))
      (setf gfx nil)) ;; hack
    
    (when gfx
      (setf *use-graphics* t))
    ;; still get problems as ffi locks up thread system, but a bit better.
    #+lispworks
    (mp:process-run-function "langband" '() #'game-init& ui)
    #-lispworks
    (game-init& ui)
;;    (format t "~&Thanks for helping to test Langband.~2%")
    ))

(defun b (&optional (ui "x11"))
  (a ui :gfx t))


(setf (symbol-function 'cl-user::langband)
      #'a)

(setf (symbol-function 'cl-user::gfx-langband)
      #'b)
