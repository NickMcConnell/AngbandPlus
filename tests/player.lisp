;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND-TEST -*-

#|

DESC: tests/player.lisp - testing code for player-obj

|#

(in-package :lb-test)

(def-lb-fixture in-player-fixture (in-game)
  ()
  (:documentation "Simple fixture for testing save/load."))


(defmethod perform-test ((fix in-player-fixture))
  (test-assert (lb::ok-object? lb::*player*)))
