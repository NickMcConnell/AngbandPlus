;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.testing -*-

#|

DESC: tests/player.lisp - testing code for player-obj

|#

(in-package :org.langband.testing)

(def-lb-fixture in-player-fixture (in-game)
  ()
  (:documentation "Simple fixture for testing save/load."))


(defmethod perform-test ((fix in-player-fixture))
  (test-assert (lb::ok-object? lb::*player* :context :in-game :warn-on-failure t)))
