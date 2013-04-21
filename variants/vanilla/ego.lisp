;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/ego.lisp - ego-item code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; Ego code

(defclass ego-item ()
  ((id          :accessor ego.id          :initform ""  :initarg :id)
   (name        :accessor ego.name        :initform ""  :initarg :name)
   (numeric-id  :accessor ego.numeric-id  :initform -1  :initarg :numeric-id)
   (rating      :accessor ego.rating      :initform 0   :initarg :rating)
   (xtra        :accessor ego.xtra        :initform 0   :initarg :xtra)
   (max-to-hit  :accessor ego.max-to-hit  :initform 0   :initarg :max-to-hit)
   (max-to-dmg  :accessor ego.max-to-dmg  :initform 0   :initarg :max-to-dmg)
   (max-to-ac   :accessor ego.max-to-ac   :initform 0   :initarg :max-to-ac)
   (pval        :accessor ego.pval        :initform 0   :initarg :pval)
   (depth       :accessor ego.depth       :initform 0   :initarg :depth)
   (rarity      :accessor ego.rarity      :initform 0   :initarg :rarity)
   (cost        :accessor ego.cost        :initform 0   :initarg :cost)
   (tval        :accessor ego.tval        :initform 0   :initarg :tval)
   (min-sval    :accessor ego.min-sval    :initform 0   :initarg :min-sval)
   (max-sval    :accessor ego.max-sval    :initform 0   :initarg :max-sval)
   (flags       :accessor ego.flags       :initform '() :initarg :flags)
   (game-values :accessor ego.game-values :initform nil :initarg :game-values)
   ))


;; remove later
(defparameter *egos* (make-hash-table :test #'equal))

(defun get-ego (numeric-id)
  (gethash numeric-id *egos*))

(defun (setf get-ego) (value numeric-id)
  (setf (gethash numeric-id *egos*) value))


(defun define-ego-item (id name &key (numeric-id :unspec) (rating :unspec)
			(xtra :unspec) (max-to-ac :unspec) (max-to-hit :unspec)
			(max-to-dmg :unspec) (pval :unspec) (depth :unspec) (rarity :unspec)
			(cost :unspec) (tval :unspec) (min-sval :unspec)
			(max-sval :unspec) (flags :unspec) (game-values :unspec))
  "Attempts to define an ego-item."


  (check-type id string)
  (check-type name string)
  
  (assert (verify-id id))
  (assert (> (length name) 0))
  
  (let ((variant *variant*)
	(ego-item (make-instance 'ego-item :name name :id id))
	(gvals (if (or (eq game-values nil) (eq game-values :unspec))
		   (make-game-values)
		   game-values)))

    (cond ((integerp numeric-id)
	   (setf (ego.numeric-id ego-item) numeric-id))
	  ((eq numeric-id :unspec))
	  (t
	   (warn "Unknown value for ego-numeric-id: ~s" numeric-id)))

    
    (cond ((integerp rating)
	   (setf (ego.rating ego-item) rating))
	  ((eq rating :unspec))
	  (t
	   (warn "Unknown value for ego-rating: ~s" rating)))
    
    (cond ((integerp xtra)
	   ;; check this later
	   ;;(warn "Ego ~s has xtra-parameter: ~s" id xtra)
	   (setf (ego.xtra ego-item) xtra))
	  ((eq xtra :unspec))
	  (t
	    (warn "Unknown value for ego-xtra: ~s" xtra)))


    (cond ((integerp max-to-hit)
	   (setf (ego.max-to-hit ego-item) max-to-hit))
	  ((eq max-to-hit :unspec))
	  (t
	   (warn "Unknown value for ego-max-to-hit: ~s" max-to-hit)))

    (cond ((integerp max-to-dmg)
	   (setf (ego.max-to-dmg ego-item) max-to-dmg))
	  ((eq max-to-dmg :unspec))
	  (t
	   (warn "Unknown value for ego-max-to-dmg: ~s" max-to-dmg)))
    
    (cond ((integerp max-to-ac)
	   (setf (ego.max-to-ac ego-item) max-to-ac))
	  ((eq max-to-ac :unspec))
	  (t
	   (warn "Unknown value for ego-max-to-ac: ~s" max-to-ac)))

    (cond ((integerp pval)
	   (setf (ego.pval ego-item) pval))
	  ((eq pval :unspec))
	  (t
	   (warn "Unknown value for ego-pval: ~s" pval)))

    (cond ((integerp depth)
	   (setf (ego.depth ego-item) depth))
	  ((eq depth :unspec))
	  (t
	   (warn "Unknown value for ego-depth: ~s" depth)))

    
    (cond ((integerp rarity)
	   (setf (ego.rarity ego-item) rarity))
	  ((eq rarity :unspec))
	  (t
	   (warn "Unknown value for ego-rarityt: ~s" rarity)))

        
    (cond ((integerp cost)
	   (setf (ego.cost ego-item) cost))
	  ((eq cost :unspec))
	  (t
	   (warn "Unknown value for ego-cost: ~s" cost)))

    (cond ((integerp tval)
	   (setf (ego.tval ego-item) tval))
	  ((eq tval :unspec))
	  (t
	   (warn "Unknown value for ego-tval: ~s" tval)))

    (cond ((integerp min-sval)
	   (setf (ego.min-sval ego-item) min-sval))
	  ((eq min-sval :unspec))
	  (t
	   (warn "Unknown value for ego-min-sval: ~s" min-sval)))
    
    (cond ((integerp max-sval)
	   (setf (ego.max-sval ego-item) max-sval))
	  ((eq max-sval :unspec))
	  (t
	   (warn "Unknown value for ego-max-sval: ~s" max-sval)))
    
    (cond ((consp flags)
	   (let ((ok-flags '()))
	     (dolist (i flags)
	       (cond ((consp i)
		      (cond ((eq (car i) '<slay>)
			     (pushnew (cadr i) (gval.slays gvals)))
			    ((eq (car i) '<sustain>)
			     (pushnew (cadr i) (gval.sustains gvals)))
			    ((eq (car i) '<resist>)
			     (if (is-legal-element? variant (cadr i))
				 (bit-flag-add! (gval.resists gvals) (get-element-flag variant (cadr i)))
				 (error "Tried to add ~s, but illegal" i)))
			    ((eq (car i) '<ignore>)
			     (if (is-legal-element? variant (cadr i))
				 (bit-flag-add! (gval.ignores gvals) (get-element-flag variant (cadr i)))
				 (error "Tried to add ~s, but illegal" i)))
			    (t
;;			     (warn "~s fell through" i)
			     (push i ok-flags)
			     )))
		     (t
		      (push i ok-flags))))
;;	     (when ok-flags
;;	       (warn "Flags ~s" ok-flags))
	   (setf (ego.flags ego-item) (nreverse ok-flags))))
	  ((eq flags :unspec))
	  ((eq flags '()))
	  (t
	   (warn "Unknown value for ego-flags: ~s" flags)))

    (setf (ego.game-values ego-item) gvals)
    
    (setf (get-ego id) ego-item)
    
    ego-item))


(defmethod get-loadable-form ((variant variant) (object ego-item) &key (full-dump nil))

;;(defun %dump-form (variant object)
  (declare (ignore full-dump))
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
             (unless (equal val def-val)
               (setf the-form (nconc the-form (list initarg (loadable-val val)))))))

    (setf the-form (list 'define-ego-item
                         (ego.id object)
                         (ego.name object)))

;;    (possibly-add :id (ego.id object) "dummy-id")
    (possibly-add :numeric-id (ego.numeric-id object) -1)
    (possibly-add :rating (ego.rating object) 0)
    (possibly-add :xtra (ego.xtra object) 0)
    
    (possibly-add :max-to-hit (ego.max-to-hit object) 0)
    (possibly-add :max-to-dmg (ego.max-to-dmg object) 0)
    (possibly-add :max-to-ac (ego.max-to-ac object) 0)
    (possibly-add :pval (ego.pval object) 0)
    
    (possibly-add :depth (ego.depth object) 0)
    (possibly-add :rarity (ego.rarity object) 0)
    (possibly-add :cost (ego.cost object) 0)

    (possibly-add :tval (ego.tval object) 0)
    (possibly-add :min-sval (ego.min-sval object) 0)
    (possibly-add :max-sval (ego.max-sval object) 0)
    (possibly-add :flags (ego.flags object) '())

    (when-bind (gval (ego.game-values object))
      (setf the-form (append the-form (list :game-values (get-loadable-form variant gval)))))

    
    the-form)))


(defun dump-egos ()
  (let* ((vals (loop for x being the hash-values of *egos*
		     collecting x))
	 (sorted-vals (sort vals #'< :key #'ego.numeric-id))
	 (*print-case* :downcase)
	 (*print-right-margin* 120))

    (with-open-file (s #p"ego.dump"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (loop for x in sorted-vals
	    do
	    (pprint (get-loadable-form *variant* x) s))
      )))

(defmethod print-object ((inst ego-item) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (ego.name inst) ))
  inst)
