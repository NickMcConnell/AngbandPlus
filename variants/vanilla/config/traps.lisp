;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

					#|

DESC: variants/vanilla/config/traps.lisp - trap-types for vanilla variant
Copyright (c) 2002 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;; traps should be better at finding out who the victim is.. right now the player is hurt automagically

(define-trap-type "trapdoor" "trap-door"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (print-message! "You fall through a trap door!");
	      (let ((variant *variant*)
		    (player *player*)) ;; fix later

		(cond ((eq t (get-creature-state player '<feather-fall>))
		       (print-message! "You float gently down to the next level."))
		      (t
		       (deliver-damage! variant the-trap player (roll-dice 2 8))))
		(warn "Add depth-change for trap-door!")
		;; increase depth and set leave to true
		))
  :min-depth 1
  :max-depth nil
  :x-char #\^
  :x-attr +term-white+
  :rarity 1)

(define-trap-type "pit" "pit"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (print-message! "You fall into a pit!")
	      
	      (let ((variant *variant*)
		    (player *player*))
		(cond ((eq t (get-creature-state player '<feather-fall>))
		       (print-message! "You float gently down to the bottom of the pit."))
		      (t
		       (deliver-damage! variant the-trap player (roll-dice 2 6))))
		))
  :x-attr +term-slate+
  :x-char #\^
  )


(define-trap-type "spiked-pit" "spiked pit"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (print-message! "You fall into a spiked pit!")
	      (let ((variant *variant*)
		    (player *player*))
		(cond ((eq t (get-creature-state player '<feather-fall>))
		       (print-message! "You float gently to the floor of the pit.")
		       (print-message! "You carefully avoid touching the spikes."))
		      (t
		       (let ((dmg (roll-dice 2 6)))
			 (when (< (randint 100) 50)
			   (print-message! "You are impaled!")
			   (incf dmg dmg) ;; * 2
			   ;; add cut (randint dmg)
			   )
			 (deliver-damage! variant the-trap player dmg))
		       ))
		))
  :x-attr +term-slate+
  :x-char #\^
  )


(define-trap-type "spiked-pit-poison" "spiked pit"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (print-message! "You fall into a spiked pit!")
	      (let ((variant *variant*)
		    (player *player*))
		(cond ((eq t (get-creature-state player '<feather-fall>))
		       (print-message! "You float gently to the floor of the pit.")
		       (print-message! "You carefully avoid touching the spikes."))
		      (t
		       (let ((dmg (roll-dice 2 6)))
			 (when (< (randint 100) 50)
			   (print-message! "You are impaled on poisonous spikes!")
			   (incf dmg dmg) ;; * 2
			   ;; add cut (randint dmg)
			   (cond ((resists-element? player '<poison>)
				  (print-message! "The poison does not affect you!"))
				 (t
				  (incf dmg dmg) ;; * 2
				  ;; add poison (randint dmg)
				  )))
			 (deliver-damage! variant the-trap player dmg))
		       ))
		))
  :x-attr +term-slate+
  :x-char #\^
  )


(define-trap-type "summon-trap" "summoning trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore the-trap dungeon x y))
	      (print-message! "You are enveloped in a cloud of smoke!")
	      ;;; ...
	      )
  :x-attr +term-orange+
  :x-char #\^
  )

(define-trap-type "teleport-trap" "teleport trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore the-trap x y))
	      (print-message! "You hit a teleport trap!")
	      (let ((player *player*))
		(teleport-creature! dungeon player player 100)))
  :x-attr +term-orange+
  :x-char #\^
  )

(define-trap-type "fire-trap" "fire trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (print-message! "You are enveloped in flames!")
	      (deliver-elemental-damage! *variant* the-trap *player* '<fire> (roll-dice 4 6)))
  :x-attr +term-umber+
  :x-char #\^
  )


(define-trap-type "acid-trap" "acid trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (print-message! "You are splashed with acid!")
	      (deliver-elemental-damage! *variant* the-trap *player* '<acid> (roll-dice 4 6)))
  :x-attr +term-umber+
  :x-char #\^
  )


(define-trap-type "dart-trap-slow" "dart trap (slow)"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (let ((target *player*))
		(cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		       (print-message! "A small dart hits you!")
		       (deliver-damage! *variant* the-trap target (roll-dice 1 4)) 
		       ;; add damage + poison
		       )
		      (t
		       (print-message! "A small dart barely misses you.")))
		))

  :x-attr +term-red+
  :x-char #\^
  )

(define-trap-type "dart-trap-red-str" "dart trap (red. str)"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (let ((target *player*))
		(cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		       (print-message! "A small dart hits you!")
		       (deliver-damage! *variant* the-trap target (roll-dice 1 4)) 
		       ;; add damage + poison
		       )
		      (t
		       (print-message! "A small dart barely misses you.")))
		))
  :x-attr +term-red+
  :x-char #\^
  )


(define-trap-type "dart-trap-red-dex" "dart trap (red. dex)"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (let ((target *player*))
		(cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		       (print-message! "A small dart hits you!")
		       (deliver-damage! *variant* the-trap target (roll-dice 1 4)) 
		       ;; add damage + poison
		       )
		      (t
		       (print-message! "A small dart barely misses you.")))
		))
  :x-attr +term-red+
  :x-char #\^
  )


(define-trap-type "dart-trap-red-con" "dart trap (red. con)"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore dungeon x y))
	      (let ((target *player*))
		(cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		       (print-message! "A small dart hits you!")
		       (deliver-damage! *variant* the-trap target (roll-dice 1 4)) 
		       ;; add damage + poison
		       )
		      (t
		       (print-message! "A small dart barely misses you.")))
		))
  :x-attr +term-red+
  :x-char #\^
  )


(define-trap-type "blind-trap" "blindness trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore the-trap dungeon x y))
	      (print-message! "You are surrounded by a black gas!")
	      (unless (resists-element? *player* '<blindness>)
		;; add blindness (randint 50) + 25
		))
  :x-attr +term-green+
  :x-char #\^
  )



(define-trap-type "confusion-trap" "confusion trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore the-trap dungeon x y))
	      (print-message! "You are surrounded by a gas of scintillating colors!")
	      (unless (resists-element? *player* '<confusion>)
		;; add confusion (randint 20) + 10
		))
  :x-attr +term-green+
  :x-char #\^
  )


(define-trap-type "poison-trap" "poison-gas trap"
  :effect (trap-effect (the-trap dungeon x y)
	      (declare (ignore the-trap dungeon x y))
	      (print-message! "You are surrounded by a pungent green gas!")
	      (unless (resists-element? *player* '<poison>)
		;; add poison (randint 20) + 10
		))
  :x-attr +term-green+
  :x-char #\^
  )


(define-trap-type "paralysis-trap" "paralysis trap"
  :effect (trap-effect (the-trap dungeon x y)
		       (declare (ignore the-trap dungeon x y))
		       (print-message! "You are surrounded by a strange white mist!")
		       (unless (eq t (get-creature-state *player* '<free-action>))
			 ;; add paralysis (randint 10) + 5
			 ))
  :x-attr +term-green+
  :x-char #\^
  )
