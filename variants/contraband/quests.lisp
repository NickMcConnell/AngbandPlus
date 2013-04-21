;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/quests.lisp - code to handle quests
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defstruct con/coord-event
  x
  y
  quest
  trigger)

(defun %make-init-method (quest-name init)
  (let ((var-arg (first (second init)))
	(quest-arg (second (second init)))
	(giver (third (second init)))
	(taker (fourth (second init))))

    `(defmethod init-quest ((,var-arg contraband) (,quest-arg ,quest-name) ,giver ,taker)
      ,@(cddr init))))

;;(trace %make-init-method)

(defmacro defquest (classname superclass &key id name desc steps init)
  (let ((init-fun (when init (%make-init-method classname init))))
  ;; fix superclass later
  `(eval-when (:execute :load-toplevel :compile-toplevel)
    (defclass ,classname (quest)
      ((id :initform ,id)
       (name :initform ,name)
       (desc :initform ,desc)
       ;; fix steps later
       ))

    ,init-fun
    
    (register-quest& ,id ',classname))))

(defun register-quest& (id name)
  ;;(warn "Registering quest ~s ~s" id name)
  (setf (gethash id (variant.quests *variant*)) name))

(defun find-quest (variant id)
  (gethash id (variant.quests variant)))


(defun add-to-inventory (creature object &key identified)
  (check-type creature player)
  (let* ((backpack (player.inventory creature))
	 (inventory (aobj.contains backpack)))
    (when identified
      (learn-about-object! creature object :aware)
      (learn-about-object! creature object :known))
    ;; should do error-checking
    (item-table-add! inventory object)
    (update-inventory-row creature)
    object))

(defun remove-from-inventory (creature key)
  (check-type creature player)
  (let ((pos (has-object? creature key)))
    (when pos
      (item-table-remove! (aobj.contains (player.inventory creature)) pos))))

(defun get-new-object (id)
  (check-type id string)
  (create-aobj-from-id id))

;; hack hack hack
(defvar *coord-events* (make-hash-table :test #'equal))

(defun add-quest-event (quest condition event)

  (when (consp condition)
    (when (eq (car condition) 'on-move-to-coord)
      (let ((x (second condition))
	    (y (third condition)))
	(setf (gethash (cons x y) *coord-events*) (make-con/coord-event :quest quest :x x :y y :trigger event)))))
	
  
  (warn "Adding event for ~s" condition)
  nil)

(defun has-object? (creature obj)

  ;; handle '(object "id") case
  (when (and (consp obj) (eq (car obj) 'object))
    (setf obj (second obj)))
  
  (check-type creature player)
  (check-type obj string)
  (let ((objs (items.objs (aobj.contains (player.inventory creature)))))
    (loop for i from 0
	  for x across objs
	  do
	  (when (typep x 'active-object)
	    (when (equal obj (object.id (aobj.kind x)))
	      (return-from has-object? i)))))
  
  nil)

(defmacro quest-event (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
               (declare (ignorable ,@arguments))
               ,@body)))
    `(function ,def)))

(defmethod on-move-to-coord ((variant contraband) (player player) x y)

  ;; bad consing
  (when-bind (ev (gethash (cons x y) *coord-events*))
    ;;(warn "found coord event ~s" ev)
    (when (functionp (con/coord-event-trigger ev))
      (funcall (con/coord-event-trigger ev) variant (con/coord-event-quest ev) player)))

  (let* ((win (aref *windows* +charinfo-frame+))
	 (row (- (window.height win) 2)))
    (output-string! win 0 row +term-l-blue+ "        ")
    (output-string! win 0 row +term-l-blue+ (format nil "~3d,~3d" (location-x player) (location-y player) )))
    
  
  player)

(defmethod quest-available? ((variant contraband) quest quest-giver quest-taker)
  nil)

(defmethod quest-status ((variant contraband) quest taker)
  (quest.state quest))

(defmethod advance-quest ((variant contraband) quest taker)
  quest)
