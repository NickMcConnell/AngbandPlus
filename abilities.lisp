(defpackage :langband)

(in-package :langband)

;;; this file isn't used.. please ignore

(defmacro define-ability (name &rest args)
;;  (print args)
  `(defconstant ,name t))

(defmacro define-hazard  (name &rest args)
;;  (print args)
  `(defconstant ,name t))


(defmacro define-attack (the-name &rest args)
;;  (print args)
  `(defun ,the-name (&key type freq)
    ',the-name))

(define-hazard <FIRE>)
(define-hazard <COLD>)
(define-hazard <ACID>)
(define-hazard <LIGHTNING>)
(define-hazard <POISON>)


(define-hazard <SOUND>)
(define-hazard <LIGHT>)
(define-hazard <DARKNESS>)
(define-hazard <NETHER>)  ;; find better name
(define-hazard <NEXUS>)
(define-hazard <CONFUSION>)
(define-hazard <FEAR>)
(define-hazard <BLINDNESS>)
(define-hazard <CHAOS>)
(define-hazard <SHARDS>)
(define-hazard <DISENCHANT>)

(define-hazard <PLASMA>)
(define-hazard <TIME>)
(define-hazard <GRAVITY>)
(define-hazard <INERTIA>)
    
;; new ones
(define-hazard <DISEASE>)


(define-attack <BITE>)
(define-attack <CLAW>)
(define-attack <ARROW>)
(define-attack <BREATH>)


(define-ability <OPEN-DOOR>)
(define-ability <BASH-DOOR>)

;;(defun <BREATH> (&key type freq)
;;  'breath)

(defconstant <NATURAL> t)
(defconstant <NEUTRAL> t)
(defconstant <GOOD> t)
(defconstant <EVIL> t)

;; Colours:
;; <BLUE>, <WHITE>, <RED>, <GREEN>, ...


;; monster type
(defconstant <DRAGON> t)

#||
(clrhash *monsters*)
(load "new-mon.lisp")

(dump-all-monsters "new-mon2.lisp" (get-monster-list) :lispy)
||#

