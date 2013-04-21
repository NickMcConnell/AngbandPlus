;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: init.lisp - initialisation code
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Code which makes sure all tables and settings are as they should 
ADD_DESC: at the start.

||#

(in-package :org.langband.engine)

(defstruct delayed
  expr)

(defun %to-a-string (obj)
  (etypecase obj
    (string obj)
    (symbol (symbol-name obj))))

(defun lookup-ui-theme-var (theme var)
  "tries to lookup a theme-variable."
  (cond ((eq var 'window.width)
	 (cond ((string-equal (theme.system theme) "gcu")
		(warn "asking for wid, returning for ~s" *screen-width*)
		(if (eq *screen-width* :wait)
		    (make-delayed :expr 'window.width)
		    *screen-width*))
	       
	       ((string-equal (theme.system theme) "sdl")
		*screen-width*)
	       (t
		(error "Uknown system for theme: ~s" (theme.system theme)))))

	((eq var 'window.height)
	 (cond ((string-equal (theme.system theme) "gcu")
		(warn "asking for hgt, returning for ~s" *screen-width*)
		(if (eq *screen-height* :wait)
		    (make-delayed :expr 'window.height)
		    *screen-height*)) ;; replace with a better size a bit later
	       ((string-equal (theme.system theme) "sdl")
		*screen-height*)
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
	((eq var '+infodisp-frame+)
	 +infodisp-frame+)
	
	(t
	 var)))

;;(trace lookup-ui-theme-var)

(defun fetch-theme-var (theme var-arg)
  "Tries to return a dynamic variable in the theme."

  (let ((win nil))
    ;; first arg should be window
    ;;(warn "looking for ~s" (first var-arg))
    (dolist (i (theme.windows theme))
      ;;(warn "Comparing ~s and ~s" i (first var-arg))
      (when (eq (window.id i) (first var-arg))
	(setf win i)))
    
    (unless win
      ;;(warn "Unable to find window (var ~s)" (first var-arg))
      (return-from fetch-theme-var (make-delayed :expr (cons 'var var-arg))))

    ;;(warn "now look for ~s in ~s" (cdr var-arg) win)
    
    (ecase (second var-arg)
      (tile-height 
       (let ((size (window.tile-height win)))
	 ;;(warn "returning font-size in ~s to be ~s" var-arg size)
	 (if (plusp size)
	     size
	     (make-delayed :expr (cons 'var var-arg)))))
      
      (tile-width
       (let ((size (window.tile-width win)))
	 (if (plusp size)
	     size
	     (make-delayed :expr (cons 'var var-arg)))))

      (height (possible-expand theme (window.pixel-height win)))
      (width (possible-expand theme (window.pixel-width win)))
      (x-offset (possible-expand theme (window.x-offset win)))
      (y-offset (possible-expand theme (window.y-offset win)))
      )))

(defun possible-expand (theme expr)
  "If EXPR is a delayed expression, it tries to expand it, if not it returns the expression."
  (if (delayed-p expr)
      (handle-ui-theme-calculation theme (delayed-expr expr))
      expr))
    

(defun handle-ui-theme-calculation (theme calc)
  "Tries to calculate value for CALC."
  (cond ((integerp calc)
	 calc)
	((integerp (lookup-ui-theme-var theme calc))
	 (lookup-ui-theme-var theme calc))
	((delayed-p calc)
	 (handle-ui-theme-calculation theme (delayed-expr calc)))
	((delayed-p (lookup-ui-theme-var theme calc))
	 (lookup-ui-theme-var theme calc))
	((consp calc)
	 (cond ((find (car calc) '(- + * /))
		(let ((temp-results (mapcar #'(lambda (x) (handle-ui-theme-calculation theme x)) (cdr calc))))
		  (if (every #'integerp temp-results)
		      (reduce (car calc) temp-results)
		      (make-delayed :expr (cons (car calc) temp-results)))
		  ))
	       ((eq (car calc) 'var)
		(fetch-theme-var theme (cdr calc)))
	       (t
		(error "Don't know how to calculate value from ~s" calc))))
	(t
	 (error ">Don't know how to calculate value from ~s" calc))))

;;(trace handle-ui-theme-calculation)

(defun process-subwindow-data! (theme data)
  (unless (nonboolsym? (car data))
    (warn "Illegal subwindow-data, should have specifier first, not ~s"
	  (car data))
    (return-from process-subwindow-data! nil))

  (let ((sub (make-instance 'window :id (car data))))

    (destructuring-bind (&key key x y width height background font tile-width tile-height gfx-tiles?)
	(cdr data)

      (cond ((numberp key)
	     (setf (window.num-id sub) key))
	    ((and (nonboolsym? key)
		  (numberp (lookup-ui-theme-var theme key)))
	     (setf (window.num-id sub) (lookup-ui-theme-var theme key)))
	    (t
	     (warn "Can't handle key ~s yet" key)))
      
      (cond ((or (nonboolsym? x) (integerp x) (consp x))
	     (let ((val (handle-ui-theme-calculation theme x)))
	       (setf (window.x-offset sub) val)))
	    (t
	     (warn "Can't handle x ~s yet" x)))

      (cond ((or (nonboolsym? y) (integerp y) (consp y))
	     (let ((val (handle-ui-theme-calculation theme y)))
	       (setf (window.y-offset sub) val)))
	    (t
	     (warn "Can't handle y ~s yet" y)))

      (cond ((or (nonboolsym? width) (integerp width) (consp width))
	     (let ((val (handle-ui-theme-calculation theme width)))
	       (setf (window.pixel-width sub) val)))
	    (t
	     (warn "Can't handle width ~s yet" width)))

      (cond ((or (integerp height) (nonboolsym? height) (consp height))
	     (let ((val (handle-ui-theme-calculation theme height)))
	       (setf (window.pixel-height sub) val)))
	    (t
	     (warn "Can't handle height ~s yet" height)))

      (cond ((eq tile-width nil)
	     nil)
	    ((numberp tile-width)
	     (setf (window.tile-width sub) tile-width))
	    ((numberp (lookup-ui-theme-var theme tile-width))
	     (setf (window.tile-width sub) (lookup-ui-theme-var theme tile-width)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle tile-width ~s" tile-width))))

      (cond ((eq tile-height nil)
	     nil)
	    ((numberp tile-height)
	     (setf (window.tile-height sub) tile-height))
	    ((numberp (lookup-ui-theme-var theme tile-height))
	     (setf (window.tile-height sub) (lookup-ui-theme-var theme tile-height)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle tile-height ~s" tile-height))))

      
      (cond ((eq nil background)
	     nil)
	    ((stringp background)
	     (setf (window.backgroundfile sub) background))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle background ~s" background))))

      (cond ((eq nil font)
	     nil)
	    ((stringp font)
	     (setf (window.font sub) font))
	    ((and (consp font) (stringp (first font)))
	     (setf (window.font sub) font))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle font ~s" font))))
      
      (cond ((or (eq gfx-tiles? t) (eq gfx-tiles? nil))
	     (setf (window.gfx-tiles? sub) gfx-tiles?))
	    ((or (eq (lookup-ui-theme-var theme gfx-tiles?) t)
		 (eq (lookup-ui-theme-var theme gfx-tiles?) nil))
	     (setf (window.gfx-tiles? sub) (lookup-ui-theme-var theme gfx-tiles?)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle gfx-tiles? argument ~s" gfx-tiles?))))

      )
    
    sub))

(defun process-ui-theme-data! (theme data &optional wanted-name)
  "Tries to parse the given data and set correct values in the ui-theme object
given as argument."
  
  (let ((subwindows '())
	(rest '()))
    
    (loop for i in data
	  do
	  (if (consp i)
	      (push i subwindows)
	      (push i rest)))

    (setf rest (nreverse rest))
    
    ;;(warn "Sub ~s and rest ~s" subwindows rest)

    (destructuring-bind (&key system default-font)
	rest
      (when system
	(setf (theme.system theme) system))
      (when default-font
	(setf (theme.font theme) default-font)))

    (when (and wanted-name (not (string-equal wanted-name (theme.system theme))))
      (return-from process-ui-theme-data! theme))
    
    (dolist (i subwindows)
      (let ((result (process-subwindow-data! theme i)))
	(when (typep result 'window)
	  (push result (theme.windows theme)))))

    ;; make sure all subwindows have a font
    (when (stringp (theme.font theme))
      (dolist (i (theme.windows theme))
	(unless (or (stringp (window.font i))
		    (and (consp (window.font i))
			 (stringp (first (window.font i)))))
	  (setf (window.font i) (theme.font theme)))))

    ;; ensure there are no duplicate keys or names
    (let ((names '())
	  (keys '()))
      (dolist (i (theme.windows theme))
	(if (find (window.id i) names :test #'string-equal)
	    (signal-condition 'illegal-ui-theme-data :id (window.id i)
			      :desc "Duplicate name of window")
	    (push (window.id i) names))
	(if (find (window.num-id i) keys :test #'equal)
	    (signal-condition 'illegal-ui-theme-data :id (window.id i)
			      :desc "Duplicate key of window")
	    (push (window.num-id i) keys))))
    
    theme))

(defun find-ui-theme (system-type)
  "Tries to read in a theme."
  (let ((wanted-file (game-data-path "theme.lisp"))
	(sys-str (string-downcase (%to-a-string system-type))))

    ;;(warn "Looking for system ~s" sys-str)
    
    (when (probe-file wanted-file) ;; is it there?
      (with-open-file (s wanted-file
			 :direction :input)
	(loop for x = (read s nil 'eof)
	      until (eq x 'eof)
	      do
	      (cond ((and (consp x) (eq (car x) 'ui-theme))
		     (unless (stringp (second x))
		       (signal-condition 'illegal-ui-theme-data :id (second x)
					 :desc "The name/key to the theme is not a string, bailing.")
		       (return-from find-ui-theme nil))

		     (let ((the-theme (make-instance 'ui-theme :key (second x))))
		       (process-ui-theme-data! the-theme (cddr x) system-type)
		       (when (string-equal sys-str (theme.system the-theme))
			 (return-from find-ui-theme the-theme))))

		    (t
		     (signal-condition 'illegal-ui-theme-data :id "<none>"
				       :desc (format nil "Don't know how to handle data ~s" x)))
		    ))

	))
    
    nil))

(defun establish-ui-theme-size-calculations& (theme)
  "Tells the C-side of wanted sizes for subwindows."
  ;; fonts already installed
  (dolist (i (theme.windows theme))
    
    (when (delayed-p (window.x-offset i))
      (setf (window.x-offset i) (possible-expand theme (window.x-offset i))))
    (when (delayed-p (window.y-offset i))
      (setf (window.y-offset i) (possible-expand theme (window.y-offset i))))
    (when (delayed-p (window.pixel-width i))
      (setf (window.pixel-width i) (possible-expand theme (window.pixel-width i))))
    (when (delayed-p (window.pixel-height i))
      (setf (window.pixel-height i) (possible-expand theme (window.pixel-height i))))

    (unless (non-negative-integer? (window.x-offset i))
      (signal-condition 'illegal-ui-theme-data :id (theme.key theme) :desc "x-offset not legal"))
    (unless (non-negative-integer? (window.y-offset i))
      (signal-condition 'illegal-ui-theme-data :id (theme.key theme) :desc "y-offset not legal"))
    (unless (positive-integer? (window.pixel-width i))
      (error-condition 'illegal-ui-theme-data :id (theme.key theme)
		       :desc (format nil "pixel-width ~s for ~s not legal"
				     (window.pixel-width i) (window.id i))))
    (unless (positive-integer? (window.pixel-height i))
      (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			:desc (format nil "pixel-height ~s not legal for ~s" (window.pixel-height i) (window.id i))))
    
    #||
    (warn "Passing ~s ~s ~s ~s ~s" (window.num-id i)
	  (window.x-offset i)
	  (window.y-offset i)
	  (window.pixel-width i)
	  (window.pixel-height i))
    ||#
    
    (org.langband.ffi:c-add-frame-coords! (window.num-id i)
					  (window.x-offset i)
					  (window.y-offset i)
					  (window.pixel-width i)
					  (window.pixel-height i))
    ))


(defun install-ui-theme& (theme)
  "Tries to install given ui-theme as the wanted ui-theme to use."
  (check-type theme ui-theme)
;;  (warn "Installing theme ~s" (theme.key theme))

  (loop ;;for cnt from 0
   for i in (theme.windows theme)
	do
	(progn
	  ;;(warn "Installing ~s" i)
	  ;;(describe i)
	  (org.langband.ffi:c-add-frame! (window.num-id i) (string-downcase (string (window.id i))))

	  (org.langband.ffi:c-add-frame-tileinfo! (window.num-id i)
						  (window.tile-width i) (window.tile-height i))
	  
	  (org.langband.ffi:c-add-frame-gfxinfo! (window.num-id i)
						 (if (window.gfx-tiles? i)
						     1 0))
	  
	  ;; the more complex stuff will be added when things are running
	  
	  ;; shouldn't this work?
	  (setf (aref *windows* (window.num-id i)) i)))
  
  t)

;; hackish
(defun %assign-debian-dirs ()
  (setf *engine-source-dir* "/usr/share/common-lisp/source/langband-engine/")
  #+unix
  (setf *engine-config-dir* "/var/games/langband-engine/")
  #+unix
  (setf *engine-data-dir* "/usr/share/games/langband-data/")
  )

(defun %assign-win-dirs ()
  (let ((dir (concatenate 'string (lbsys/ensure-dir-name (namestring (lbsys/get-current-directory)))
			  "config/")))
    (setf *engine-config-dir* dir)))

(defun load-user-preference-file& ()

  (let ((wanted-file (merge-pathnames (pathname "prefs.lisp") (home-langband-path)))
	(*package* (find-package :org.langband.engine))
	(*load-verbose* nil))
    (when (probe-file wanted-file)
      (load wanted-file))))


(defun load-user-variant-prefs& (variant)
  (let ((wanted-file (merge-pathnames (pathname "prefs.lisp") (variant-home-path variant)))
	(*package* (find-package :org.langband.engine))
	(*load-verbose* nil))
    (when (probe-file wanted-file)
      (load wanted-file))))



(defun game-init& (&optional (ui "sdl") &key (size :800x600) (full-screen nil))
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."

  (setq cl:*random-state* (cl:make-random-state t))
  (vinfo-init&) ;; init line-of-sight arrays

  (lb-ds:init-pq-pool 60) ;; random number
  
  ;; ensure that we always have a valid input-event
  (unless (and *input-event* (input-event-p *input-event*))
    (setf *input-event* (make-input-event :keypress (make-keyboard-event) :mouseclick (make-mouse-event))))

  (when (string-equal ui "gcu")
    (setf size :wait
	  *screen-height* :wait
	  *screen-width* :wait))
  
  (let ((alternative-errors (open #+win32 #p"lb-warn.txt"
				  #-win32 #p"warnings-langband.txt"
				  :direction :output
				  :if-exists :append
				  :if-does-not-exist :create)))
    (unwind-protect
	 (let* ((hide-warnings (or #+win32 t
				   #+langband-release t
				   (string-equal ui "gcu")))
		
		(cl:*error-output* (if hide-warnings
				       alternative-errors
				       cl:*error-output*))
		
		(cl:*trace-output* (if hide-warnings
				       alternative-errors
				       cl:*trace-output*))
		(cl:*standard-output* (if hide-warnings
					  alternative-errors
					  cl:*standard-output*))
		
		)
	   

      ;; fix paths
      #-langband-development
      (%assign-debian-dirs)
      #+win32
      (%assign-win-dirs)

      ;; time to register our lisp
      #+(or cmu allegro clisp lispworks sbcl cormanlisp)
      (org.langband.ffi:c-set-lisp-system! #+cmu 0 #+allegro 1 #+clisp 2 #+lispworks 3
					   #+sbcl 4 #+cormanlisp 5 #+openmcl 6)
      
      #-(or cmu allegro clisp lispworks sbcl cormanlisp openmcl)
      (error "lisp-system ~s unknown for C-side." (lisp-implementation-type))
      
      (org.langband.ffi:c-init-frame-system& +max-frames+ +predefined-frames+)

      (handler-case
	  (let* (;; hacks
		 (*screen-width* (ecase size
				   (:wait :wait)
				   (:800x600 800)
				   (:1024x768 1024)
				   (:1280x1024 1280)))
		 (*screen-height* (ecase size
				    (:wait :wait)
				    (:800x600 600)
				    (:1024x768 768)
				    (:1280x1024 1024)))
		 (theme (find-ui-theme ui)))
	    (unless (typep theme 'ui-theme)
	      (warn "No usable theme found, using defaults."))
	    (install-ui-theme& theme)
	    (setf *current-ui-theme* theme))
	
	(illegal-ui-theme-data (co)
	    (warn "UI-Theme problems for ~a: ~a" (illegal-data.id co)
		  (illegal-data.desc co))
	    (return-from game-init& nil)))
      
      #+use-callback-from-c
      (arrange-callbacks)
      
      #+(and cmu use-callback-from-c)
      (pushnew 'arrange-callbacks ext:*after-gc-hooks*)
      
      #+(and sbcl use-callback-from-c)
      (pushnew 'arrange-callbacks sb-ext:*after-gc-hooks*)

      (handler-case
	  (let ((flag 0)
		(retval -1))
	    (when *graphics-supported*
	      (bit-flag-add! flag #x01)) ;; graphics
	    ;; add sound?
	    #-disable-sound
	    (bit-flag-add! flag #x02)

	    (ecase size
	      (:wait nil)
	      (:800x600 nil)
	      (:1024x768 (bit-flag-add! flag #x04))
	      (:1280x1024 (bit-flag-add! flag #x08)))
	    
	    (when full-screen
	      (bit-flag-add! flag #x10))

	    ;;(warn "init flag ~s" flag)
	    (setf retval (org.langband.ffi:c-init-c-side& (%to-a-string ui)
							  *engine-source-dir*
							  *engine-config-dir*
							  *engine-data-dir*
							  flag)) ;; no debug, possible gfx
	    (cond ((= retval -1)
		   #-use-callback-from-c
		   (play-game&)
		   #+use-callback-from-c
		   (progn
		     (warn "Problems init'ing UI, please check term-window for error-messages")
		     (return-from game-init& nil)))
		  ((/= retval 0) ;; only success is accepted
		   (warn "Problems init'ing UI, please check term-window for error-messages")
		   (return-from game-init& nil))
		  ;;(warn "return..")
		  ))
	    
	(langband-quit ()
	  (format t "~&Thanks for helping to test Langband.~2%")
	  (org.langband.ffi:c-cleanup-c-side&)
	  ))
  
      t)
      ;; cleanup
      (close alternative-errors)
      )))

(defun %adjust-screen-size (width height)
  (declare (ignore width height))
  ;; call the update-term-sizes in global.lisp
  ;; but only when we're absolutely sure all terms are properly created!!
  ;;(update-term-sizes!)
  
  ;;(warn "ADJUST DISABLED!!")

  t)
    
#+use-callback-from-c
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
    
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
    (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
    )
  
  #-(or sbcl cmu allegro lispworks)
  (error "No callback arranged for implementation..")
  
  )

;;; hackish thing to start the game ever so long.
(defun a (&optional (ui "sdl") &key (gfx nil) (size :800x600) (full-screen nil))
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
    #||
    #+lispworks
    (mp:process-run-function "langband" '() #'game-init& ui)
    #-lispworks
    ||#
    (game-init& ui :size size :full-screen full-screen)
;;    (format t "~&Thanks for helping to test Langband.~2%")
    ))

(defun b (&optional (ui "gcu"))
  ;;(warn "Curses/GCU not supported in this version.")
  (a ui :gfx nil))

(defun c (&key (ui "sdl") (size :800x600) (full-screen nil))
  (a ui :gfx t :size size :full-screen full-screen))

(defun d (&optional (ui "sdl"))
  (a ui :gfx t :size :1024x768))

(defun e (&optional (ui "sdl"))
  (a ui :gfx t :size :1280x1024 :full-screen t))

(setf (symbol-function 'cl-user::langband)
      #'c)

(setf (symbol-function 'cl-user::gcu-langband)
      #'b)


(setf (symbol-function 'cl-user::gfx-langband)
      #'c)

(setf (symbol-function 'cl-user::sdl-langband)
      #'c)
