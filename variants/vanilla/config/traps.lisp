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
	    (print-message! "You fall through a trap door!");
	    (let ((variant *variant*)
		  (player *player*) ;; fix later
		  (level-num (randint 3)))
	      
	      (cond ((eq t (get-creature-state player '<feather-fall>))
		     (setf level-num 1)
		     (print-message! "You float gently down to the next level."))
		    (t
		     (deliver-damage! variant the-trap player (roll-dice 2 8))))

	      ;; increase depth and set leave to true
	      (change-depth! dungeon player :direction :down :amount level-num :type :trapdoor)
	      ))
  :min-depth 1
  :max-depth nil
  :text-char #\^
  :text-attr +term-white+
  :x-attr (tile-file 10)
  :x-char (tile-number 38)
  :rarity 1)

(define-trap-type "pit" "pit"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You fall into a pit!")
	    
	    (let ((variant *variant*)
		  (player *player*))
	      (cond ((eq t (get-creature-state player '<feather-fall>))
		     (print-message! "You float gently down to the bottom of the pit."))
		    (t
		       (deliver-damage! variant the-trap player (roll-dice 2 6))))
	      ))
  :text-attr +term-slate+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 36)
  )


(define-trap-type "spiked-pit" "spiked pit"
  :effect (trap-effect (the-trap dungeon x y)
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
  :text-attr +term-slate+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 37)
  )


(define-trap-type "spiked-pit-poison" "spiked pit"
  :effect (trap-effect (the-trap dungeon x y)
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
				  (modify-creature-state! player '<poisoned> :add (randint dmg))
				  )))
			 (deliver-damage! variant the-trap player dmg))
		       ))
		))
  :text-attr +term-slate+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 67)
  )


(define-trap-type "summon-trap" "summoning trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are enveloped in a cloud of smoke!")
	      ;;; ...
	    )
  :text-attr +term-orange+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 39)
  )

(define-trap-type "teleport-trap" "teleport trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You hit a teleport trap!")
	    (let ((player *player*))
	      (teleport-creature! dungeon player player 100)))
  :text-attr +term-orange+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 40)
  )

(define-trap-type "fire-trap" "fire trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are enveloped in flames!")
	    (deliver-elemental-damage! *variant* the-trap *player* '<fire> (roll-dice 4 6)))
  :text-attr +term-umber+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 35)
  )


(define-trap-type "acid-trap" "acid trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are splashed with acid!")
	    (deliver-elemental-damage! *variant* the-trap *player* '<acid> (roll-dice 4 6)))
  :text-attr +term-umber+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 35)
  )


(define-trap-type "dart-trap-slow" "dart trap (slow)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (modify-creature-state! target '<slowed> :add (+ 20 (randint 20)))
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
		))
  
  :text-attr +term-red+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 33)
  )

(define-trap-type "dart-trap-red-str" "dart trap (red. str)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (update-player-stat! target '<str> '<reduce>)
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
	      ))
  :text-attr +term-red+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 57)
  )


(define-trap-type "dart-trap-red-dex" "dart trap (red. dex)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (update-player-stat! target '<dex> '<reduce>)
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
	      ))
  :text-attr +term-red+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 58)
  )


(define-trap-type "dart-trap-red-con" "dart trap (red. con)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (update-player-stat! target '<con> '<reduce>)
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
	      ))
  :text-attr +term-red+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 60)
  )


(define-trap-type "blind-trap" "blindness trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a black gas!")
	    (unless (resists-element? *player* '<blindness>)
	      (modify-creature-state! *player* '<blindness> :add (+ 25 (randint 50)))
	      ))
  :text-attr +term-green+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 61)
  )



(define-trap-type "confusion-trap" "confusion trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a gas of scintillating colors!")
	    (unless (resists-element? *player* '<confusion>)
	      (modify-creature-state! *player* '<confusion> :add (+ 10 (random 20)))
	      ))
  :text-attr +term-green+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 59)
  )


(define-trap-type "poison-trap" "poison-gas trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a pungent green gas!")
	    (unless (resists-element? *player* '<poison>)
	      (modify-creature-state! *player* '<poisoned> :add (+ 20 (randint 20)))

	      ))
  :text-attr +term-green+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 56)
  )


(define-trap-type "paralysis-trap" "paralysis trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a strange white mist!")
	    (unless (eq t (get-creature-state *player* '<free-action>))
	      (modify-creature-state! *player* '<paralysed> :add (+ 5 (randint 10)))
	      ))
  :text-attr +term-green+
  :text-char #\^
  :x-attr (tile-file 10)
  :x-char (tile-number 61)
  )
