;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#||

DESC: init.lisp - initialisation code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Code which makes sure all tables and settings are as they should 
ADD_DESC: at the start.

||#

(in-package :langband)

(defun init-objects& (&key old-file) 
  (when old-file
    (read-old-obj-kind& old-file) ;; adds to *object-kind-table*
    (setq *objects-by-level* (htbl-to-vector *object-kind-table* t
					     :sorted-by-key #'object.level))
    (setq *alloc-table-objects* (create-alloc-table-objects *objects-by-level*))
    (init-flavours& *object-kind-table*)

    (%output-kinds-to-file "dumps/obj.lisp")
    ))

(defun init-monsters& (&key old-file)
  (when old-file
    (read-old-monsters& old-file) ;; adds to *monster-kind-table*
    (setq *monsters-by-level* (htbl-to-vector *monster-kind-table* t
					      :sorted-by-key #'monster.level))
    (setq *alloc-table-monsters* (create-alloc-table-monsters *monsters-by-level*))
    ))

(defun init-flavours& (obj-table)
  "initiates flavours and updates symbols relating to flavours"
    
  
  (dolist (x *flavour-types*)
    
    ;; we wish to avoid helping flavours with their own
    ;; functions
    (unless (get x 'flavour-generator)

      ;; make a shuffled flavour-selector and change the flavour-table
      ;; into a vector
	  
      (let* ((flavour-htbl (get x 'flavour-table))
	     (len (hash-table-count flavour-htbl))
	     (tmp-arr (make-array len :initial-element 0 :fill-pointer t)))
	
	  ;; make numbered array and assign htbl values to vector
	  (loop for i from 0 to (1- len)
		do
		(setf (aref tmp-arr i) i))

	  ;; shuffle
	  (shuffle-array! tmp-arr len)

	  (setf (fill-pointer tmp-arr) 0) ;; set fill-pointer to start
	  (setf (get x 'flavour-selector) tmp-arr)

	  ;; make sure the flavour-table is a vector now
	  (setf (get x 'flavour-table) (htbl-to-vector flavour-htbl))
	  
	  )))

  ;; go through the object table
  (loop for x being the hash-values of obj-table
	do
	(let ((which-sym (obj-is-in? x *flavour-types*)))
	  (when which-sym
	    
	    (let ((flavour-generator (get which-sym 'flavour-generator))
		  (flavour-vector (get which-sym 'flavour-table))
		  (flavour-selector (get which-sym 'flavour-selector)))

	      ;; if we have a function, call it and use return value
	      (cond (flavour-generator
		     (setf (object.flavour x) (funcall flavour-generator x)))
		    
		  ;; otherwise use tables and selector
		    (t
		     (setf (object.flavour x)
			   (svref flavour-vector
				  (aref flavour-selector (fill-pointer flavour-selector))))
		     (incf (fill-pointer flavour-selector))))

	      )))

	))


(defun create-alloc-table-objects (obj-table)
  "Creates an allocation table for objects and returns it."
  
  ;; first we should scan the obj-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((org-size +max-depth+)
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
;;    (warn "Scanning..")

    #+langband-debug
    (with-open-file (s (pathname "dumps/bar.txt")
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
						      :level (svref (object.locale k-obj) j)
						      :prob1 p
						      :prob2 p
						      :prob3 p)))
			
		    
		    (setf (svref table counter) alloc-obj)
		    (incf counter))))))


      (setq table (sort table #'< :key #'alloc.level))

      #+langband-debug
      (with-open-file (s (pathname "dumps/foo.txt")
			 :direction :output 
                         :if-exists :supersede)
	(loop for i across table
	      do
	      (format s "~&~a: ~a~%" (alloc.level i) (alloc.obj i))))

      
      table)))


(defun create-alloc-table-monsters (mon-table)
  "Creates an allocation table for monsters and returns it."
  
  ;; first we should scan the mon-table and figure
  ;; out level-organisation and number of allocation
  ;; slots

  (let* ((org-size +max-depth+)
	 (level-org (make-array org-size :initial-element 0))
	 (alloc-sz 0))
    
    (loop for k-obj across mon-table
	  do
	  (when (< 0 (monster.rarity k-obj))
	    (incf alloc-sz)
	    (incf (aref level-org (monster.level k-obj)))
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
	      
	      (let* ((x (monster.level k-obj))
		     (p (int-/ 100 (monster.rarity k-obj)))
		     (alloc-obj (make-alloc-entry :index k-idx
						  :obj k-obj
						  :level x
						  :prob1 p
						  :prob2 p
						  :prob3 p)))
;;		(warn "k")
		(setf (svref table counter) alloc-obj)
		(incf counter))))

	    

      (setq table (sort table #'< :key #'alloc.level))
    

      #+langband-debug
      (with-open-file (s (pathname "dumps/formosa.txt")
			 :direction :output 
                         :if-exists :supersede)
	(loop for i across table
	      do
	      (format s "~&~a: ~a~%" (alloc.level i) (alloc.obj i))))
      
      table)))


(defun initialise-langband& ()
  "This initialises the langband-game. No low-level
stuff please."
  
  (let ((variant-pre-init (get 'variant 'pre-init))
	(variant-post-init (get 'variant 'post-init))
	(common-pre-init (get 'common 'pre-init))
	(common-post-init (get 'common 'post-init))
	)

    (when common-pre-init
      (funcall common-pre-init))
    
    (when variant-pre-init
      (funcall variant-pre-init))
    
    
    ;;(dump-all-monsters "new-mon.lisp" (get-monster-list) :lispy)

    ;; read monsters later
    ;;(load "new-mon.lisp")

    (init-monsters& :old-file "lib/edit/r_info.txt")
    (read-floor-file& "lib/edit/f_info.txt")
    (init-objects& :old-file "lib/edit/k_info.txt")

    ;; hackish
    (unless *current-key-table*
      (setf *current-key-table* *ang-keys*))
    
    (when common-post-init
      (funcall common-post-init))
    
    (when variant-post-init
      (funcall variant-post-init))))


(defun game-init& ()
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."
  
  (setq *random-state* (make-random-state t))

  (vinfo-init)
  (initialise-langband&)

  #+allegro
  (set_lisp_callback (ff:register-foreign-callable `c-callable-play nil t))
  
  ;;  (c-init-x11! 0 0)
  (c-init-gui! 0 +c-null-value+)
  
  #+cmu
  (play-game&)
  
  )
