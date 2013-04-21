;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: keys.lisp - keypressing code
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with keyboard input.

|#

(in-package :org.langband.engine)

(defvar *key-operations* (make-hash-table :test #'eq))

(defun define-key-operation (key operation)
  "defines a key-operation which later can be bound."
  (setf (gethash key *key-operations*) operation))

(defun find-key-operation (key)
  "returns an operation or NIL."
  (gethash key *key-operations*))

(defun get-key-operations ()
  "Returns an alist of keys and their operations."

  (let ((collected nil))
    (maphash #'(lambda (k v)
		 (push (cons k v) collected))
	     *key-operations*)
    (nreverse collected)))

(defun define-key-table (name)
  "Returns a key-table."
  (declare (ignore name))
  (make-hash-table :test #'eql))

(defun make-inner-key-table ()
  (make-hash-table :test #'eql))


(defvar *current-key-table* nil)
(defvar *ang-keys* (define-key-table "angband"))

(defun define-keypress (key-table where key operation)
  "Defines a keypress and ties it to the appropriate
operation."

  (let ((table (gethash where key-table))
	(oper (find-key-operation operation)))

    (unless oper
      (error "Unable to find operation '~a' in ~a for key '~a'"
	    operation (cons key-table where) key)
      #-cmu
      (return-from define-keypress nil))
    
    (unless table
      (setf table (make-inner-key-table))
      (setf (gethash where key-table) table))
    
    (setf (gethash key table) oper)
    ))
    

(defun check-keypress (table key)
  "checks a keypress vs the given table"
  (let ((poss-fun (gethash key table)))
    poss-fun))


(defun get-and-process-command! (dun pl table)
  "remove me later"

  (let ((loc-table (gethash table *current-key-table*)))

    (loop
     (let* ((ch (read-one-character))
	    (fun (check-keypress loc-table ch)))
       
       (cond ((and fun (functionp fun))
	      (let ((retval (funcall fun dun pl)))
		(return-from get-and-process-command! retval)))
	     (t
	      (warn "fell through key with ~a ~a" ch (char-code ch))))
       ))
    ))

(defun is-closed-door? (dun x y)
  (let ((feat (cave-feature dun x y)))
    (and (>= feat +feature-door-head+)
	 (< feat +feature-door-tail+))))

(defun open-door! (dun x y)
  "hackish, fix me later.."
  (setf (cave-feature dun x y) +feature-open+)
  (light-spot! dun x y))


(defun open-all! (dun pl)
  "opens all doors around.."
  (let ((x (location-x pl))
	(y (location-y pl))
	(collected '()))
    
    (dolist (i (list (cons x (1- y))
		     (cons x (1+ y))
		     (cons (1- x) y)
		     (cons (1+ x) y)))
      (when (is-closed-door? dun (car i) (cdr i))
	(open-door! dun (car i) (cdr i))))))

(defun print-key-table (table fname)
  "Prints a key-table to the given file."
  
  (with-open-file (s (pathname fname)
                     :direction :output 
                     :if-exists :supersede)
    (let ((collected nil))
      (maphash #'(lambda (k v)
		   (push (cons k v) collected))

	       table)
      ;; hackish
      (let ((key-ops (get-key-operations)))
	(dolist (i key-ops)
	  (dolist (j collected)
	    (when (eq (cdr i) (cdr j))
	      (setf (cdr j) (car i))))))
      
      (let ((sorted (sort (mapcar #'(lambda (k)
				      (format nil "key ~a -> ~a" (car k) (cdr k)))
				  collected)
			  #'string-lessp)))
	(dolist (i sorted)
	  (format s "~a~%" i))))))

