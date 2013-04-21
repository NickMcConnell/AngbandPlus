;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/verify.lisp - verification of objects, specific to vanilla
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defmethod ok-object? ((obj object-kind/wand) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (let ((zap (get-object-effect *variant* obj :zap)))
    (unless zap
      (warn "No zap-effect on wand '~a'" (object.name obj))))
  (when (get-object-effect *variant* obj :quaff)
    (warn "Weird: a quaffable ~a" obj))
  (when (get-object-effect *variant* obj :read)
    (warn "Weird: a readable ~a" obj))
  (when (get-object-effect *variant* obj :eat)
    (warn "Weird: an eatable ~a" obj))
  t)

(defmethod ok-object? ((obj object-kind/staff) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (let ((zap (get-object-effect *variant* obj :zap)))
    (unless zap
      (warn "No-zap-effect on staff '~a'" (object.name obj))))
  (when (get-object-effect *variant* obj :quaff)
    (warn "Weird: a quaffable ~a" obj))
  (when (get-object-effect *variant* obj :read)
    (warn "Weird: a readable ~a" obj))
  (when (get-object-effect *variant* obj :eat)
    (warn "Weird: an eatable ~a" obj))
  t)

(defmethod ok-object? ((obj object-kind/rod) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (let ((zap (get-object-effect *variant* obj :zap)))
    (unless zap
      (warn "No zap-effect on rod '~a'" (object.name obj))))
  
  (when (get-object-effect *variant* obj :quaff)
    (warn "Weird: a quaffable ~a" obj))
  (when (get-object-effect *variant* obj :read)
    (warn "Weird: a readable ~a" obj))
  (when (get-object-effect *variant* obj :eat)
    (warn "Weird: an eatable ~a" obj))
  t)

(defmethod ok-object? ((obj object-kind/potion) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (let ((zap (get-object-effect *variant* obj :quaff)))
    (unless zap
      (warn "No quaff-effect on potion '~a'" (object.name obj))))
  (when (get-object-effect *variant* obj :zap)
    (warn "Weird: a zappable ~a" obj))
  (when (get-object-effect *variant* obj :read)
    (warn "Weird: a readable ~a" obj))
  t)

(defmethod ok-object? ((obj object-kind/scroll) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (let ((zap (get-object-effect *variant* obj :read)))
    (unless zap
      (warn "No read-effect on scroll '~a'" (object.name obj))))
  (when (get-object-effect *variant* obj :quaff)
    (warn "Weird: a quaffable ~a" obj))
  (when (get-object-effect *variant* obj :zap)
    (warn "Weird: a zappable ~a" obj))
  (when (get-object-effect *variant* obj :eat)
    (warn "Weird: an eatable ~a" obj))
  t)

;; mushrooms should have extra effect
(defmethod ok-object? ((obj object-kind/mushroom) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (let ((eat (get-object-effect *variant* obj :eat)))
    (unless eat
      (warn "No eat-effect on mushroom '~a'" (object.name obj))))
  (when (get-object-effect *variant* obj :quaff)
    (warn "Weird: a quaffable ~a" obj))
  (when (get-object-effect *variant* obj :read)
    (warn "Weird: a readable ~a" obj))
  (when (get-object-effect *variant* obj :zap)
    (warn "Weird: a zappable ~a" obj))
  t)

(defmethod ok-object? ((obj active-object/wand) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (%ok-check (typep (aobj.game-values obj) 'game-values))
  t)

(defmethod ok-object? ((obj active-object/staff) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (call-next-method))
  (%ok-check (typep (aobj.game-values obj) 'game-values))
  t)

(defmethod ok-object? ((obj magic-spell) &key context warn-on-failure)
  (declare (ignore context))
  (%ok-check (stringp (spell.id obj)))
  (unless (spell.effect obj)
    (warn "The spell '~a' has no effect yet" (spell.name obj)))
  t)
