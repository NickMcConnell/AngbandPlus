;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: variants/vanilla/scrolls.lisp - scroll-effects
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)

(define-object-effect (<scroll> <phase-door>)
    (dun pl item)
  (declare (ignore dun pl item))
  (warn "phase door.")
;;  (when  (set-creature-state! pl :fear nil)
;;    (possible-identify! pl (aobj.kind item)))
  :used)
