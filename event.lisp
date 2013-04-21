;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: event.lisp - general event-classes and functionality
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.engine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; the types that return-action of an event may have.
  (deftype return-actions ()
    `(member :remove-event :keep-event))

  (deftype event-types ()
    '(member :on-create :on-pre-equip :on-post-equip :step-on-coord))
  
  )

(defclass l-event ()
  ((id            :reader event.id
		  :initform nil
		  :initarg :id)
   (type          :reader event.type
		  :initform nil
		  :initarg :type)
   ;; the function when called should return T when ok and NIL when not ok
   (function      :reader event.function
		  :initform nil
		  :initarg :function)
   (state         :reader event.state
		  :initform nil
		  :initarg :state)
   (return-action :reader event.return
		  :initform :remove-event
		  :initarg :return)
   ))

(defgeneric trigger-event (obj event arg-list)
  (:documentation "Triggers a given event-type on the object. Recursive."))
(defgeneric register-object-event! (obj event)
  (:documentation "Registers an event on the object."))

(defun is-event? (obj)
  (typep obj 'l-event))

(defun register-event& (id event)
  "Registers an event-id and connects it to a function."
  (unless (equal id (event.id event))
    (warn "registration id ~s of event ~s aren't equal")) 
  (let ((key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key *global-event-table*) event)))

(defun find-event-for-key (id)
  "Tries to find an event for the given id."
  (let ((key (if (symbolp id) (symbol-name id) id)))
    (gethash key *global-event-table*)))

(defun make-event (id type function &key (state nil) (return-action :remove-event))
  "Returns an event-object that can be used."
  (check-type return-action return-actions)
  (check-type type event-types)
  (make-instance 'l-event :id id :type type :function function :state state
		 :return return-action))

(defun define-normal-event (dummy-arg id type function)
  "establishes an event basically."
  (declare (ignore dummy-arg))
  
  (let ((the-event (make-event id type function)))
    (register-event& id the-event)
    the-event))

(defmethod trigger-event (obj event arg-list)
  (declare (ignore obj event arg-list))
  (values))

(defun apply-event (event-type event-list arg-list)
  "Iterates through event-list and funcalls any events
with given arg-list if any events match."
  (dolist (i event-list)
    (when (eq event-type (event.type i))
      (apply (event.function i) (event.state i) arg-list)
      )))

(defun get-legal-events (event-list)
  "Goes through the list and ensures that all events are
legal, and if they're not they will be replaced with a legal-event
or removed.  Conses up a new list."
  (let ((new-list nil))
    (dolist (i event-list)
      (cond ((typep i 'l-event)
	     (push i new-list))
	    ((or (symbolp i) (stringp i))
	     (let ((find-attempt (find-event-for-key i)))
	       (if (and find-attempt (typep find-attempt 'l-event))
		   (push find-attempt new-list)
		   (warn "Unable to find an event for key ~s" i))))

	    (t
	     (warn "Do not know how to handle possible event ~s" i))))
    (nreverse new-list)))
  