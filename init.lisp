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

(defun lookup-var (theme var)

  (cond ((eq var 'window.width)
	 (cond ((string-equal (theme.system theme) "gcu")
		80) ;; replace with a better size a bit later
	       ((string-equal (theme.system theme) "sdl")
		800)
	       (t
		(error "Uknown system for theme: ~s" (theme.system theme)))))

	((eq var 'window.height)
	 (cond ((string-equal (theme.system theme) "gcu")
		24) ;; replace with a better size a bit later
	       ((string-equal (theme.system theme) "sdl")
		600)
	       (t
		(error "Uknown system for theme: ~s" (theme.system theme)))))
	
	((eq var 'gfxtiles.height)
	 32)
	((eq var 'gfxtiles.width)
	 32)
	((eq var 'true)
	 t)
	((eq var 'false)
	 nil)
	((eq var  '+gfxmap-frame+)
	 +gfxmap-frame+)
	((eq var  '+asciimap-frame+)
	 +asciimap-frame+)
	((eq var '+inv-frame+)
	 +inv-frame+)
	((eq var '+misc-frame+)
	 +misc-frame+)
	((eq var '+message-frame+)
	 +message-frame+)
	((eq var '+charinfo-frame+)
	 +charinfo-frame+)
	((eq var '+full-frame+)
	 +full-frame+)
	((eq var '+query-frame+)
	 +query-frame+)
	((eq var '+dialogue-frame+)
	 +dialogue-frame+)
	
	(t
	 var)))

(defun process-subwindow-data! (theme data)
  (unless (nonboolsym? (car data))
    (warn "Illegal subwindow-data, should have specifier first, not ~s"
	  (car data))
    (return-from process-subwindow-data! nil))

  (let ((sub (make-instance 'subwindow :name (car data))))

    (destructuring-bind (&key key x y width height background font tile-width tile-height gfx-tiles?)
	(cdr data)

      (cond ((numberp key)
	     (setf (subwindow.key sub) key))
	    ((and (nonboolsym? key)
		  (numberp (lookup-var theme key)))
	     (setf (subwindow.key sub) (lookup-var theme key)))
	    (t
	     (warn "Can't handle key ~s yet" key)))
      
      (cond ((numberp x)
	     (setf (subwindow.x sub) x))
	    ((numberp (lookup-var theme x))
	     (setf (subwindow.x sub) (lookup-var theme x)))
	    (t
	     (warn "Can't handle x ~s yet" x)))

      (cond ((numberp y)
	     (setf (subwindow.y sub) y))
	    ((numberp (lookup-var theme y))
	     (setf (subwindow.y sub) (lookup-var theme y)))
	    (t
	     (warn "Can't handle y ~s yet" y)))

      (cond ((numberp width)
	     (setf (subwindow.width sub) width))
	    ((numberp (lookup-var theme width))
	     (setf (subwindow.width sub) (lookup-var theme width)))
	    (t
	     (warn "Can't handle width ~s yet" width)))

      (cond ((numberp height)
	     (setf (subwindow.height sub) height))
	    ((numberp (lookup-var theme height))
	     (setf (subwindow.height sub) (lookup-var theme height)))
	    (t
	     (warn "Can't handle height ~s yet" height)))

      (cond ((eq tile-width nil)
	     nil)
	    ((numberp tile-width)
	     (setf (subwindow.tile-width sub) tile-width))
	    ((numberp (lookup-var theme tile-width))
	     (setf (subwindow.tile-width sub) (lookup-var theme tile-width)))
	    (t
	     (warn "Can't handle tile-width ~s yet" tile-width)))

      (cond ((eq tile-height nil)
	     nil)
	    ((numberp tile-height)
	     (setf (subwindow.tile-height sub) tile-height))
	    ((numberp (lookup-var theme tile-height))
	     (setf (subwindow.tile-height sub) (lookup-var theme tile-height)))
	    (t
	     (warn "Can't handle tile-height ~s yet" tile-height)))

      
      (cond ((eq nil background)
	     nil)
	    ((stringp background)
	     (setf (subwindow.background sub) background))
	    (t
	     (warn "Can't handle background ~s yet" background)))

      (cond ((eq nil font)
	     nil)
	    ((stringp font)
	     (setf (subwindow.font sub) font))
	    (t
	     (warn "Can't handle font ~s yet" font)))
      
      (cond ((or (eq gfx-tiles? t) (eq gfx-tiles? nil))
	     (setf (subwindow.gfx-tiles? sub) gfx-tiles?))
	    ((or (eq (lookup-var theme gfx-tiles?) t)
		 (eq (lookup-var theme gfx-tiles?) nil))
	     (setf (subwindow.gfx-tiles? sub) (lookup-var theme gfx-tiles?)))
	    (t
	     (warn "Can't handle gfx-tiles? ~s yet" gfx-tiles?)))

      )
    
    sub))

  
  
(defun process-theme-data! (theme data)
  (let ((subwindows '())
	(rest '()))
    
    (loop for i in data
	  do
	  (if (consp i)
	      (push i subwindows)
	      (push i rest)))

    (setf rest (nreverse rest))
    
;;    (warn "Sub ~s and rest ~s" subwindows rest)

    (destructuring-bind (&key system default-font)
	rest
      (when system
	(setf (theme.system theme) system))
      (when default-font
	(setf (theme.font theme) default-font)))

    (dolist (i subwindows)
      (let ((result (process-subwindow-data! theme i)))
	(when (typep result 'subwindow)
	  (push result (theme.windows theme)))))

    ;; make sure all subwindows have a font
    (when (stringp (theme.font theme))
      (dolist (i (theme.windows theme))
	(unless (stringp (subwindow.font i))
	  (setf (subwindow.font i) (theme.font theme)))))

    ;; ensure there are no duplicate keys or names
    (let ((names '())
	  (keys '()))
      (dolist (i (theme.windows theme))
	(if (find (subwindow.name i) names :test #'string-equal)
	    (warn "Duplicate name of window ~s" (subwindow.name i))
	    (push (subwindow.name i) names))
	(if (find (subwindow.key i) keys :test #'equal)
	    (warn "Duplicate key for window ~s" (subwindow.key i))
	    (push (subwindow.key i) keys))))
    
    theme))

(defun find-theme (system-type)
  "Tries to read in a theme."
  (let ((wanted-file (game-data-path "theme.lisp"))
	(sys-str (string-downcase (%to-a-string system-type))))

    (warn "Looking for system ~s" sys-str)
    
    (when (probe-file wanted-file) ;; is it there?
      (with-open-file (s wanted-file
			 :direction :input)
	(loop for x = (read s nil 'eof)
	      until (eq x 'eof)
	      do
	      (cond ((and (consp x) (eq (car x) 'theme))
		     (unless (stringp (second x))
		       (warn "The name/key to the theme is not a string, bailing.")
		       (return-from find-theme nil))

		     (let ((the-theme (make-instance 'theme :key (second x))))
		       (process-theme-data! the-theme (cddr x))
		       (when (string-equal sys-str (theme.system the-theme))
			 (return-from find-theme the-theme))))

		    (t
		     (warn "Don't know how to handle data ~s" x))
		    ))

	))
    
    nil))

(defun install-theme& (theme)

  (check-type theme theme)
  (warn "Installing theme ~s" (theme.key theme))
  
  (dolist (i (theme.windows theme))
    (c-add-frame! (subwindow.key i) (string-downcase (string (subwindow.name i))))
    (c-add-frame-coords! (subwindow.key i)
			 (subwindow.x i) (subwindow.y i)
			 (subwindow.width i) (subwindow.height i))
    (c-add-frame-tileinfo! (subwindow.key i)
			   (subwindow.tile-width i) (subwindow.tile-height i)
			   (subwindow.font i)
			   (if (subwindow.background i)
			       (subwindow.background i)
			       ""))
    (c-add-frame-gfxinfo! (subwindow.key i)
			  (if (subwindow.gfx-tiles? i)
			      1 0))
    )
  
  t)

(defun game-init& (&optional (ui "sdl"))
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."
  
  (setq cl:*random-state* (cl:make-random-state t))
  (vinfo-init&) ;; init line-of-sight arrays

  (with-open-file (alternative-errors #+win32 #p"lb-warn.txt"
				      #-win32 #p"langband-warn.txt"
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

      ;; fix paths
      #-langband-development
      (progn
	(setf *engine-source-dir* "/usr/share/common-lisp/source/langband-engine/")
	#+unix
	(setf *engine-config-dir* "/var/games/langband-engine/")
	#+unix
	(setf *engine-graphics-dir* "/usr/share/games/langband-engine/graphics/")
	)
      #+win32
      (setf *engine-config-dir* (concatenate 'string (lbsys/ensure-dir-name (namestring (lbsys/get-current-directory)))
					     "config/"))
      

      ;; time to register our lisp
      #+(or cmu allegro clisp lispworks sbcl cormanlisp)
      (c-set-lisp-system! #+cmu 0 #+allegro 1 #+clisp 2 #+lispworks 3 #+sbcl 4 #+cormanlisp 5)
      
      #-(or cmu allegro clisp lispworks sbcl cormanlisp)
      (error "lisp-system ~s unknown for C-side." (lisp-implementation-type))
      
      (c-init-frame-system& +max-frames+ +predefined-frames+)
      
      (let ((theme (find-theme ui)))
	(unless (typep theme 'theme)
	  (warn "No usable theme found, using defaults."))
	(install-theme& theme))
      
      #+use-callback-from-c
      (arrange-callbacks)
      
      #+cmu
      (pushnew 'arrange-callbacks ext:*after-gc-hooks*)
      
      #+sbcl
      (pushnew 'arrange-callbacks sb-ext:*after-gc-hooks*)
      
      (handler-case
	  (let ((flag 0)
		(retval -1))
	    (when *graphics-supported*
	      (bit-flag-add! flag 1)) ;; graphics
	    ;; add sound?
	    #-disable-sound
	    (bit-flag-add! flag 2)
	    
	    (setf retval (init-c-side& (%to-a-string ui)
				       *engine-source-dir*
				       *engine-config-dir*
				       *engine-graphics-dir*
				       flag)) ;; no debug, possible gfx

	    (when (/= retval 0) ;; only success is accepted
	      (warn "Problems init'ing UI, please check term-window for error-messages")
	      (return-from game-init& nil))
	    ;;(warn "return..")
	    #-use-callback-from-c
	    (play-game&))
	(langband-quit ()
	  (format t "~&Thanks for helping to test Langband.~2%")))
  
      t)))

(defun %adjust-screen-size (width height)
  (declare (ignore width height))
  ;; call the update-term-sizes in global.lisp
  ;; but only when we're absolutely sure all terms are properly created!!
  ;;(update-term-sizes!)
  
  ;;(warn "ADJUST DISABLED!!")

  t)
    

(defun %mouse-clicked (button x y)
  (declare (ignore button))
  (when (eq (get-system-type) 'sdl)
    ;; this is a hardcoded hack
    ;; bottom-right buttons
    (cond ((and (> x 768) (> y 536))
	   (switch-inventory-view))
	  ;; add when you figure out how to handle inkey()
	  ((and (> x 736) (> y 536))
	   ;;(switch-map-mode)
	   (c-term-keypress 20))
	  )
    ;;(warn "Button ~s clicked at (~s,~s)" button x y)
    nil))

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
(defun a (&optional (ui "sdl") &key (gfx nil))
  ;; to make sure dumps look pretty
  (let ((*package* (find-package :org.langband.engine))
	#+(or cmu) (extensions:*gc-verbose* nil)
	#+(or cmu sbcl) (*compile-print* nil)
	)

    (unless (or (string-equal "sdl" (string ui))
		(string-equal "x11" (string ui)))
      (setf gfx nil)) ;; hack      
    
    (when gfx
      (setf *graphics-supported* t))
    ;; still get problems as ffi locks up thread system, but a bit better.
    #+lispworks
    (mp:process-run-function "langband" '() #'game-init& ui)
    #-lispworks
    (game-init& ui)
;;    (format t "~&Thanks for helping to test Langband.~2%")
    ))

(defun b (&optional (ui "gcu"))
  (a ui :gfx nil))

(defun c (&optional (ui "sdl"))
  (a ui :gfx t))


(setf (symbol-function 'cl-user::langband)
      #'b)

(setf (symbol-function 'cl-user::gfx-langband)
      #'c)
