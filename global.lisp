;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: global.lisp - globally available functions/classes
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: common langband-specific code of interest for larger
ADD_DESC: parts of the code.  Small classes, functions, et.al

|#

(in-package :langband)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass game-values ()
    ((base-ac       :accessor gval.base-ac       :initform 0)
     (ac-bonus      :accessor gval.ac-bonus      :initform 0)
     (base-dice     :accessor gval.base-dice     :initform 0)
     (num-dice      :accessor gval.num-dice      :initform 0)
     (tohit-bonus   :accessor gval.tohit-bonus   :initform 0)
     (dmg-bonus     :accessor gval.dmg-bonus     :initform 0)
     (mana          :accessor gval.mana          :initform 0)
     (charges       :accessor gval.charges       :initform 0)
     (food-val      :accessor gval.food-val      :initform 0)
     (tunnel        :accessor gval.tunnel        :initform 0)
     (speed         :accessor gval.speed         :initform 0)
     (skill-bonuses :accessor gval.skill-bonuses :initform nil)
     (stat-bonuses  :accessor gval.stat-bonuses  :initform nil)
     (ignores       :accessor gval.ignores       :initform nil)
     (resists       :accessor gval.resists       :initform nil)
     (immunities    :accessor gval.immunities    :initform nil)
     (abilities     :accessor gval.abilities     :initform nil)
     (sustains      :accessor gval.sustains      :initform nil)
     (slays         :accessor gval.slays         :initform nil)
     )
  
    (:documentation "necessary game-values for an object.")))
   

(defun make-game-values ()
  "Returns an object of type game-values."
  (make-instance 'game-values))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass active-object (activatable)
    ((kind        :accessor aobj.kind
		  :initarg :obj
		  :initform nil)
     (inscription :accessor aobj.inscr
		  :initform nil)
     (number      :accessor aobj.number
		  :initarg :number
		  :initform 1)
     (contains    :accessor aobj.contains
		  :initarg :contains
		  :initform nil)
     (events      :accessor aobj.events
		  :initarg :events
		  :initform nil)
     )))



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass active-monster (activatable)
    ((kind   :accessor amon.kind   :initarg :obj   :initform nil)
     (cur-hp :accessor amon.cur-hp :initarg :hp    :initform nil)
     (speed  :accessor amon.speed  :initarg :speed :initform nil)
     (mana   :accessor amon.mana   :initarg :mana  :initform nil)
     )))
   

(defstruct (alloc-entry (:conc-name alloc.))
  (obj nil)
  (index nil)
  (level nil)
  (prob1 nil)
  (prob2 nil)
  (prob3 nil))


(defstruct (dun-data (:conc-name dun-data.))
  (room-centres nil)
  (doors nil)
  (walls nil)
  (tunnels nil)
  (row-rooms nil)
  (col-rooms nil)
  (room-map nil)
  (crowded nil))

#||
(defmethod print-object ((inst l-alloc-entry) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (alloc.obj inst) (alloc.level inst)))
  inst)
||#


(defun get-colour-code-from-letter (letter)
  "Returns a code which can be sent to C-functions as colour."
  
  (case letter
    (#\d +term-dark+)
    (#\w +term-white+)
    (#\s +term-slate+)
    (#\o +term-orange+)
    (#\r +term-red+)
    (#\g +term-green+)
    (#\b +term-blue+)
    (#\u +term-umber+)

    (#\D +term-l-dark+)
    (#\W +term-l-white+)
    (#\v +term-violet+)
    (#\y +term-yellow+)
    (#\R +term-l-red+)
    (#\G +term-l-green+)
    (#\B +term-l-blue+)
    (#\U +term-l-umber+)

    (otherwise
     (error "Fell through get-colour-code-from-letter.. ~a" letter)
     #-cmu
     +term-white+)))

(defun get-letter-from-colour-code (code)
  "Returns a char for the appropriate colour-code."
  #||
  (warn "compare ~s vs ~s, ~a vs ~a, ~a vs ~a, ~a vs ~a, ~a"
	code +term-dark+
	(type-of code) (type-of +term-dark+) (char-code code) (char-code +term-dark+)
	(eq code +term-dark+) (eql code +term-dark+) (case code (+term-dark+ t) (t nil)))
  ||#

  (cond  ((eq code +term-dark+)    #\d) 
	 ((eq code +term-white+)   #\w)
	 ((eq code +term-slate+)   #\s) 
	 ((eq code +term-orange+)  #\o) 
	 ((eq code +term-red+)     #\r) 
	 ((eq code +term-green+)   #\g) 
	 ((eq code +term-blue+)    #\b) 
	 ((eq code +term-umber+)   #\u) 
	 
	 ((eq code +term-l-dark+)  #\D) 
	 ((eq code +term-l-white+) #\W) 
	 ((eq code +term-violet+)  #\v) 
	 ((eq code +term-yellow+)  #\y) 
	 ((eq code +term-l-red+)   #\R) 
	 ((eq code +term-l-green+) #\G) 
	 ((eq code +term-l-blue+)  #\B) 
	 ((eq code +term-l-umber+) #\U) 

	 (t
	  (error "Fell through get-letter-from-colour-code.. ~a" (char-code code))
	  #-cmu
	  #\w)))
    
(defun screen-save ()
  ;; flush
  (c-print-message +c-null-value+)
  (c-term-save))

(defun screen-load ()
    ;; flush
  (c-print-message +c-null-value+)
  (c-term-load))


(defmacro with-new-screen (arg &body body)
  (declare (ignore arg))
  `(unwind-protect
    (prog2
	(screen-save)
	,@body)
    (screen-load)))

;; move later

(defun get-system-type ()
  'x11)

(defun read-pref-file (fname)
  (load fname))

(defun define-key-macros (key &rest macros)
  (dolist (i macros)
    (let ((macro (text-to-ascii i)))
;;      (loop for x across macro do (format t "~a " (char-code x)))
;;      (format t "~%")
;;      (warn "macro ~s" macro)
      (c-macro-add& macro (string key))))
  key)
