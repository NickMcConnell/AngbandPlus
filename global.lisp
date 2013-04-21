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

;; these might be commented out
#||
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric location-x (obj)
    (:documention "Generic function for all things that have a location
in the game at some point."))
  
  (defgeneric (setf location-x) (value obj)
    (:documentation "Sets the x-location for the object whenever possible."))
  
  (defgeneric location-y (obj)
    (:documention "Generic function for all things that have a location
in the game at some point."))
  
  (defgeneric (setf location-y) (value obj)
    (:documentation "Sets the y-location for the object whenever possible.")))
||#  

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
     (light-radius  :accessor gval.light-radius  :initform 0)
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
  
    (:documentation "necessary game-values for an object."))

    (defclass attack ()
    (
     (kind :accessor attack.kind :initform nil)
     (dmg-type :accessor attack.dmg-type :initform nil)
     (damage :accessor attack.damage :initform nil)
     ))
   
    
   

    (defclass active-object (activatable)
      ((kind        :accessor aobj.kind
		    :initarg :obj
		    :initform nil)
       (inscription :accessor aobj.inscr
		    :initform "")
       (number      :accessor aobj.number
		  :initarg :number
		  :initform 1)
       (contains    :accessor aobj.contains
		    :initarg :contains
		    :initform nil)
       (events      :accessor aobj.events
		    :initarg :events
		    :initform nil)
       (loc-x       :accessor location-x
		    :initarg :loc-x
		    :initform +illegal-loc-x+)
       (loc-y       :accessor location-y
		    :initarg :loc-y
		    :initform +illegal-loc-y+)
       ))



    (defclass active-monster (activatable)
      ((kind    :accessor amon.kind
		:initarg :kind
		:initform nil)
       (cur-hp  :accessor current-hp
		:initarg :hp
		:initform 0)
       (max-hp  :accessor get-creature-max-hp
		:initarg :max-hp
		:initform 0)
       (speed   :accessor get-creature-speed
		:initarg :speed
		:initform 0)
       (energy  :accessor get-creature-energy
		:initarg :energy
		:initform 0)
       (mana    :accessor get-creature-mana
		:initarg :mana
		:initform 0)
       
       (loc-x   :accessor location-x      :initarg :loc-x :initform nil)
       (loc-y   :accessor location-y      :initarg :loc-y :initform nil)
       (alive?  :accessor creature-alive? :initarg :alive? :initform t)
       
       ))

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

    ;; this is a dummy for classes, not objects.. the player will have numbers
    (defstruct (skill (:conc-name skill.))
      (name "")
      (base 0)
      (lvl-gain 0)) ;; this is for 10 levels, to allow for fractions


    ;; move this to variants later
    (defclass skills ()
      ((saving-throw :accessor skills.saving-throw  :initform 0)
       (stealth      :accessor skills.stealth       :initform 0)
       (fighting     :accessor skills.fighting      :initform 0)
       (shooting     :accessor skills.shooting      :initform 0)
       (disarming    :accessor skills.disarming     :initform 0)
       (device       :accessor skills.device        :initform 0)
       (perception   :accessor skills.perception    :initform 0)
       (searching    :accessor skills.searching     :initform 0))
      (:documentation "Various skills..")
      ;;    #+cmu
      ;;    (:metaclass pcl::standard-class)
      ))


(defun make-game-values ()
  "Returns an object of type game-values."
  (make-instance 'game-values))



(defun make-skills (&key (default-value 0))
  "Returns a skills object."
  (let ((obj (make-instance 'skills)))
    (unless (and (numberp default-value) (= 0 default-value))
      (let ((skill-list (variant.skill-translations *variant*)))
	(dolist (i skill-list)
	  (setf (slot-value obj (cdr i)) default-value))))
    obj))

(defun register-skill-translation& (variant translation)
  "Registers a translation (single cons) or a list
of translations."
  (flet ((add-single-translation (translation)
	   (pushnew translation (variant.skill-translations variant)
		    :test #'eql)))
    
    (when (consp translation) 
      (cond ((and (atom (car translation))
		  (atom (cdr translation)))
	     (warn "pushing a single one..")
	     (add-single-translation translation))
	  
	    (t
	     ;; we have a list
	     (dolist (i translation)
	       (assert (and (atom (car i))
			    (atom (cdr i))))
	       (add-single-translation i)))))
    
    translation))
  

(defun get-skill-translation (variant key)
  "Returns a symbol in the appropriate skills-class or nil."
  (let ((search (assoc key (variant.skill-translations variant))))
    (when search
      (cdr search))))


(defun build-skills-obj-from-list (variant skills)
  "Tries to build a skills-obj and include all possible
information from the list skills whose content depends on variant."
  
  (let ((skill-obj (make-skills :default-value nil)))

      (when (listp skills)
	(dolist (i skills)
	  (if (not (consp i))
	      (warn "Skill argument ~s must be a list, like: (skill base-val lvl-val)"
		    i)
	      (let* ((skill-sym (first i))
		     (the-name (get-skill-translation variant skill-sym)))
		(if (eq the-name nil)
		    (warn "Unable to find skill-translation from ~s" skill-sym)
		    (setf (slot-value skill-obj the-name)
			  (make-skill :name (string-downcase (string the-name))
				      :base (let ((base-arg (second i)))
					      (if base-arg base-arg 0))
				      :lvl-gain (let ((lvl-arg (third i)))
						  (if lvl-arg lvl-arg 0))))
		    )))))
      skill-obj))
	
#||
;; comment this out when code is working.. it conses and isn't needed
(defmethod print-object ((inst skills) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) ~{ ~S~}" (class-name (class-of inst)) 
	   (mapcar #'(lambda (x) (slot-value inst (cdr x))) (variant.skill-translations *variant*))))
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

(let ((screen-lock nil))
  
  (defun screen-save ()
    
    (when screen-lock
      (error "Screen already locked, please fix execution-path."))

    ;; flush
    (c-print-message! +c-null-value+)
    (c-term-save!)
    (setf screen-lock t))
  
  (defun screen-load ()
    (unless screen-lock
      (error "Trying to load a screen, but none is locked."))
    ;; flush
    (c-print-message! +c-null-value+)
    (c-term-load!)
    (setf screen-lock nil)))


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
  "Tries to read a named preference file."
  (load fname))

(defun define-key-macros (key &rest macros)
  (dolist (i macros)
    (let ((macro (text-to-ascii i)))
;;      (loop for x across macro do (format t "~a " (char-code x)))
;;      (format t "~%")
;;      (warn "macro ~s" macro)
      (c-macro-add& macro (string key))))
  key)


#-compiler-that-inlines
(defmacro grid (x y)
  `(the fixnum (+ (the fixnum (* 256 ,y)) ,x)))

#+compiler-that-inlines
(defun grid (x y)
  (the fixnum (+ (* 256 y) x)))

#-compiler-that-inlines
(defmacro grid-y (g)
  `(the fixnum (int-/ ,g 256)))

#+compiler-that-inlines
(defun grid-y (g)
  (the fixnum (int-/ g 256)))

#-compiler-that-inlines
(defmacro grid-x (g)
  `(the fixnum (prog1 (mod ,g 256))))

#+compiler-that-inlines
(defun grid-x (g)
  (the fixnum (prog1 (mod g 256))))
