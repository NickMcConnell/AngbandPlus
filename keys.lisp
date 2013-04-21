;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: keys.lisp - keypressing code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Most of the code which deals with keyboard input.

|#

(in-package :langband)

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
  (make-hash-table :test #'eq))

(defun make-inner-key-table ()
  (make-hash-table :test #'eq))

(defvar *current-key-table* nil)

(defun define-keypress (key-table where key operation)
  "Defines a keypress and ties it to the appropriate
operation."

  (let ((table (gethash where key-table))
	(oper (find-key-operation operation)))

    (unless oper
      (error "Unable to find operation '~a' in ~a for key '~a'"
	    operation (cons key-table where) key)
      
      (return-from define-keypress nil))
    
    (unless table
      (setf table (make-inner-key-table))
      (setf (gethash where key-table) table))
    
    (setf (gethash key table) oper)
    ))
    
#||    


(defun define-keypress (where ang-key rogue-key function)
  "Defines a keypress and creates the necessary tables"

  (declare (ignore rogue-key))
  
  (let ((table (gethash where *keypress-tables* )))

    (unless table
      (setf table (make-hash-table))
      (setf (gethash where *keypress-tables*) table))

    ;; ignore rogue-keys
    (setf (gethash ang-key table) function)

    ang-key))
||#

(defun check-keypress (table key)
  "checks a keypress vs the given table"
  (let ((poss-fun (gethash key table)))
    poss-fun))


(defun get-and-process-command! (dun pl table)
  "remove me later"

;;  (setq c-inkey-flag 1)
  (let* ((table (gethash table *current-key-table*))
;;	 (the-str (make-string 100))
;;	 (retval (c-term-inkey& the-str 1 0))
	 (ch (read-one-character))
	 (fun (check-keypress table ch)))

;;    (warn "Got ~a" retval)
    
    (if fun
	(funcall fun dun pl)
	(warn "fell through key with ~a ~a" ch (char-code ch)))
    ))

(defun is-closed-door? (dun x y)
  (let ((feat (cave-feature dun x y)))
    (and (>= feat +feature-door-head+)
	 (< feat +feature-door-tail+))))

(defun open-door! (dun x y)
  (setf (cave-feature dun x y) +feature-open+)
  ;;(light-spot! dun x y)
  )

(defun open-all! (dun pl)
  "opens all doors around.."
  (let ((x (player.loc-x pl))
	(y (player.loc-y pl)))
    
    (dolist (i (list (cons x (1- y))
		     (cons x (1+ y))
		     (cons (1- x) y)
		     (cons (1+ x) y)))
      (when (is-closed-door? dun (car i) (cdr i))
	(open-door! dun (car i) (cdr i))))))


(defun key-test ()
  (dotimes (i 5)
    (print (c-read-some-key& 0 0))))
