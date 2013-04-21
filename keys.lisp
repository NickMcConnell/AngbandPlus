;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: keys.lisp - keypressing code
Copyright (c) 2000-2002 - Stig Erik Sandø

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
  ;; to trigger warnings early
;;  (when (functionp operation)
;;    (setf operation (compile nil operation)))
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
      (warn "Unable to find operation '~a' in ~a for key '~a'"
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


(defun get-and-process-command! (dungeon player table)
  "remove me later"

  (let ((loc-table (gethash table *current-key-table*)))

    (loop
     (let* ((ch (read-one-character))
	    (fun (check-keypress loc-table ch)))
       
       (cond ((and fun (functionp fun))
	      (let ((retval (funcall fun dungeon player)))
		(return-from get-and-process-command! retval)))
	     (t
	      (warn "fell through key with ~a ~a ~s" ch (char-code ch) ch)))
       ))
    ))
