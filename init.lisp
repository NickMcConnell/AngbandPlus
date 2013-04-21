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

(defun init-flavours& (flavour-type-table)
  "initiates flavours and updates symbols relating to flavours"

  (assert (hash-table-p flavour-type-table))

  (loop for x being the hash-values of flavour-type-table
	do
    
    ;; we wish to avoid helping flavours with their own
    ;; functions
    (unless (flavour-type.generator-fn x)

      ;; make a shuffled vector of the flavour-htbl
	  
      (let* ((flavour-htbl (flavour-type.table x))
	     (flavour-array (convert-obj flavour-htbl :vector :fill-pointer t))
	     (len (hash-table-count flavour-htbl)))

	(cond ((> len 0)
	       (setf (fill-pointer flavour-array) len)
	       
	       ;; shuffle
	       (shuffle-array! flavour-array len))
	      
	      (t
	       (warn "The flavour-type ~s has no flavours" (flavour-type.symbol x))))

	(setf (fill-pointer flavour-array) 0) ;; set fill-pointer to start

;;	(warn "Val is ~a" (aref flavour-array 0))
	(setf (flavour-type.table x) flavour-array)))
    ))



(defmethod create-alloc-table-objects ((variant variant) obj-table)
  "Creates an allocation table for objects and returns it."
  
  ;; first we should scan the obj-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((var-obj *variant*)
	 (org-size (variant.max-depth var-obj))
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
;;    (warn "Scanning..")

    #+langband-debug
    (with-open-file (s (pathname (concatenate 'string *dumps-directory* "bar.txt"))
		       :direction :output 
		       :if-exists :supersede)
      (loop for i across obj-table
	    do (print i s)))
    
    (loop for k-obj across obj-table
	  do
	  (dotimes (j 4)
	    (unless (= 0 (svref (object.chance k-obj) j))
	      (incf alloc-sz)
	      (incf (aref level-org (svref (object.locale k-obj) j)))
	      )))

;;    (warn "Summing..")
    ;; then we sum up in level-org (init2.c)
    (loop for lvl-num from 1 to (1- org-size)
	  do
	  (setf (aref level-org lvl-num) (+ (aref level-org lvl-num)
					    (aref level-org (1- lvl-num)))))

    #||
    (loop for v across level-org
	  for i from 0
	  do
	  (format t "~&~a: ~a~%" i v))
    ||#

;;    (warn "doing table ~a ~a ~a" alloc-sz (svref obj-table 0) (svref obj-table 1))
    
    (let ((table (make-array alloc-sz))
	  (counter 0))
      (loop for k-obj across obj-table
	    for k-idx from 0
	    do
	    (dotimes (j 4)
	      (let ((chance (svref (object.chance k-obj) j)))
		(unless (= 0 chance)
		  (let* ((p (int-/ 100 chance))
			 (alloc-obj (make-alloc-entry :index k-idx
						      :obj k-obj
						      :depth (svref (object.locale k-obj) j)
						      :prob1 p
						      :prob2 p
						      :prob3 p)))
			
		    
		    (setf (svref table counter) alloc-obj)
		    (incf counter))))))


      (setq table (sort table #'< :key #'alloc.depth))

      #+langband-debug
      (dump-alloc-table table (concatenate 'string *dumps-directory* "foo.txt"))
      
      table)))


(defmethod create-alloc-table-monsters ((variant variant) mon-table)
  "Creates an allocation table for monsters and returns it."
  
  ;; first we should scan the mon-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((org-size (variant.max-depth variant))
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
    (loop for k-obj across mon-table
	  do
	  (when (< 0 (monster.rarity k-obj))
	    (incf alloc-sz)
	    (incf (aref level-org (monster.depth k-obj)))
	    ))
      
    ;; then we sum up in level-org (init2.c)
    (loop for lvl-num from 1 to (1- org-size)
	  do
	  (setf (aref level-org lvl-num) (+ (aref level-org lvl-num)
					    (aref level-org (1- lvl-num)))))

   
    (let ((table (make-array alloc-sz))
	  (counter 0))

;;      (warn "going second table.")
      
      (loop for k-obj across mon-table
	    for k-idx from 0
	    do
	    ;;(warn "i")
	    (when (< 0 (monster.rarity k-obj))
	      
	      (let* ((x (monster.depth k-obj))
		     (p (int-/ 100 (monster.rarity k-obj)))
		     (alloc-obj (make-alloc-entry :index k-idx
						  :obj k-obj
						  :depth x
						  :prob1 p
						  :prob2 p
						  :prob3 p)))
;;		(warn "k")
		(setf (svref table counter) alloc-obj)
		(incf counter))))

	    

      (setq table (sort table #'< :key #'alloc.depth))
    

      #+langband-debug
      (dump-alloc-table table (concatenate 'string *dumps-directory* "formosa.txt"))
      
      table)))

(defun dump-alloc-table (table fname)
  "Dumps an alloc-table to the given file."
  (with-open-file (s (pathname fname)
		     :direction :output 
		     :if-exists :supersede)
    (loop for i across table
	  do
	  (format s "~&~a: ~a~%" (alloc.depth i) (alloc.obj i)))))

(defun %to-a-string (obj)
  (etypecase obj
    (string obj)
    (symbol (symbol-name obj))))

(defun game-init& (&optional (ui #+win32 "win" #-win32 "x11"))
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."
  
  (setq cl:*random-state* (cl:make-random-state t))

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
  
  ;; so far we just load vanilla
  (let* ((var-key "langband-vanilla")
	 (var-obj (load-variant& var-key :verbose t)))
    (cond ((not var-obj)
	   (warn "Unable to find variant ~s" var-key)
	   (return-from game-init& nil))
	  (t
	   (setf *variant* var-obj)
	   (activate-object var-obj))))

  (vinfo-init&)
    
  ;; run tests after variant has been loaded
  #+xp-testing
  (do-a-test :post)
  
  (unless *current-key-table*
    (setf *current-key-table* *ang-keys*))

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
      (progn
	(init-c-side& (%to-a-string ui)
		      (namestring *engine-config-dir*)
		      0) ;; no debug
	
	;;(warn "return..")
	#-use-callback-from-c
	(play-game&))
    (langband-quit ()
      (format t "~&Thanks for helping to test Langband.~2%")))
  
  )))

(defun %adjust-screen-size (width height)
  (let ((adjusted-width (- width +start-column-of-map+ 1))
	(adjusted-height (- height +start-row-of-map+ 1)))
	
;;    (warn "Adjusting to ~s ~s from ~s ~s" adjusted-width adjusted-height *screen-width* *screen-height*)
    (when (or (/= *screen-width* adjusted-width)
	      (/= *screen-height* adjusted-height))
      (setf *screen-width* adjusted-width
	    *screen-height* adjusted-height)
      (verify-panel *dungeon* *player*)
      t)))

(defun arrange-callbacks ()
  "Assures that the C-side has necessary callbacks to the Lisp-side."
  
  #+allegro
  (let ((play-ptr (ff:register-foreign-callable `c-callable-play nil t))
	(size-ptr (ff:register-foreign-callable `%adjust-screen-size nil t)))
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr))
  
  #+cmu
  (let ((play-ptr (kernel:get-lisp-obj-address #'play-game&))
	(size-ptr (kernel:get-lisp-obj-address #'%adjust-screen-size)))
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr))

  #+sbcl
  (let ((play-ptr (sb-kernel:get-lisp-obj-address #'play-game&))
	(size-ptr (sb-kernel:get-lisp-obj-address #'%adjust-screen-size)))
    (warn "setting callbacks ~d ~d" play-ptr size-ptr)
    (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
    (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr))

  #-(or sbcl cmu allegro)
  (error "No callback arranged for implementation..")
  
  )

;;; hackish thing to start the game ever so long.
(defun a (&optional (ui #+win32 "win" #-win32 "x11"))
  ;; to make sure dumps look pretty
  (let ((*package* (find-package :org.langband.engine))
	#+(or cmu) (extensions:*gc-verbose* nil)
	#+(or cmu sbcl) (*compile-print* nil)
	)
    (game-init& ui)
;;    (format t "~&Thanks for helping to test Langband.~2%")
    ))
