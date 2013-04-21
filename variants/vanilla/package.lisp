;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: variants/vanilla/package.lisp - package def for vanilla

|#

(in-package :cl-user)

(defpackage :org.langband.vanilla
  (:nicknames :lb-vanilla :lbv)
  (:use :common-lisp :org.langband.engine))
