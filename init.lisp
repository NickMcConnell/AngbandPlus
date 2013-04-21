;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#||

DESC: init.lisp - initialisation code
Copyright (c) 2000-2001 - Stig Erik Sand�

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Code which makes sure all tables and settings are as they should 
ADD_DESC: at the start.

||#

(in-package :langband)

(defun init-flavours& (flavour-type-list)
  "initiates flavours and updates symbols relating to flavours"
    
  
  (dolist (x flavour-type-list)
    
    ;; we wish to avoid helping flavours with their own
    ;; functions
    (unless (flavour-type.generator-fn x)

      ;; make a shuffled vector of the flavour-htbl
	  
      (let* ((flavour-htbl (flavour-type.table x))
	     (flavour-array (htbl-to-vector flavour-htbl :fill-pointer t))
	     (len (hash-table-count flavour-htbl)))

	(cond ((> len 0)
	       (setf (fill-pointer flavour-array) len)
	       
	       ;; shuffle
	       (shuffle-array! flavour-array len))
	      
	      (t
	       (warn "The flavour-type ~s has no flavours" (flavour-type.symbol x))))

	(setf (fill-pointer flavour-array) 0) ;; set fill-pointer to start

;;	(warn "Val is ~a" (aref flavour-array 0))
	(setf (flavour-type.table x) flavour-array))
      )))


(defun create-alloc-table-objects (obj-table)
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

  (let* ((var-obj *variant*)
	 (org-size (variant.max-depth var-obj))
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


(defun game-init& ()
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."
  
  (setq cl:*random-state* (cl:make-random-state t))

  (vinfo-init)

  ;; so far we just load vanilla
  (let ((var-obj (load-variant& 'vanilla-variant :verbose t)))

    (when var-obj
      (setf *variant* var-obj)
      (activate-object var-obj)))

  ;; run tests after variant has been loaded
  #+xp-testing
  (do-a-test :post)
  
  (unless *current-key-table*
    (setf *current-key-table* *ang-keys*))

  #+allegro
  (set_lisp_callback (ff:register-foreign-callable `c-callable-play nil t))
  
  ;;  (c-init-x11! 0 0)
  (c-init-gui! 0 +c-null-value+)
  
  #+cmu
  (play-game&)
  
  )