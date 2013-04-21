;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

vanilla/races.lisp - races for vanilla variant
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

Contains definition of races in sexp.

|#

(in-package :langband)


(define-race '<human> "Human"
  :desc "
               The human is the base character.  All other races are com-
               pared to them.  Humans can choose any class and are average
               at everything.  Humans tend to go up levels faster than any
               other race because of their shorter life spans.  No racial
               adjustments or intrinsics occur to characters choosing human."
  :xp-extra 0
  :hit-dice 10
  :stat-changes nil
  :abilities nil
  :classes t
  )

(define-race '<half-elf> "Half-elf"
  :desc "
               Half-elves tend to be smarter and faster than a  human,  but
               not as strong.  Half-elves are slightly better at searching,
               disarming, saving throws, stealth, bows, and magic, but they
               are not as good at hand weapons.  Half-elves may choose any
               class and do not receive any intrinsic abilities."

  :xp-extra 10
  :hit-dice 9
  :stat-changes '((<str> -1) (<int> +1) (<wis> 0)
		  (<dex> +1) (<con> -1) (<chr> +1))
  :abilities '(
	       (<infravision> 2)
	       )
  :classes t
  )

(define-race '<elf> "Elf"
  :desc "
               Elves are better magicians then humans, but not as good at
               fighting.  They tend to be smarter and faster than either
               humans or half-elves and also have better wisdom.  Elves
               are better at searching, disarming, perception, stealth, bows,
               and magic, but they are not as good at hand weapons.  Elves
               may choose any class except Paladin.  They resist light
               effects intrinsically."

  :xp-extra 20
  :hit-dice 8
  :stat-changes '((<str> -1) (<int> +2) (<wis> +1)
		  (<dex> +1) (<con> -1) (<chr> +1))
  :abilities '(
	       (<resist> <light>)
	       (<infravision> 3)
	       )
  :classes '(<paladin> <warrior> <mage>)
  )

(define-race '<hobbit> "Hobbit"
  :desc "
               Hobbits, or Halflings, are very good at bows, throwing,  and
               have good saving throws.  They also are very good at search-
               ing, disarming, perception, and stealth; so they make excel-
               lent rogues, but prefer to be called burglars.  They will be
               much weaker than humans, and no good at melee fighting.
               Halflings have fair infravision, so they can detect warm
               creatures at a distance.  Hobbits can choose between being a
               warrior, mage, or rogue.  They have their dexterity sustained."

  :xp-extra 10
  :hit-dice 7
  :stat-changes '((<str> -2) (<int> +2) (<wis> +1)
		  (<dex> +3) (<con> +2) (<chr> +1))
  :abilities '(
	       (<sustain> <dex>)
	       (<infravision> 4)
	       )
  :classes '(<warrior>)
  )

(define-race '<gnome>  "Gnome"
  :desc "
               Gnomes are smaller than dwarves but larger than halflings.
               They, like  the halflings, live in the earth in burrow-like
               homes.  Gnomes make excellent mages, and have very good saving
               throws.  They are good at searching, disarming, perception,
               and stealth.  They have lower strength than humans so they
               are not very good at fighting with hand weapons.  Gnomes have
               fair infra-vision, so they can detect warm-blooded creatures
               at a distance.  A gnome may choose between being a warrior,
               mage, priest, or rogue.  Gnomes are intrinsically protected
               against paralysis and some slowing effects."

  :xp-extra 25
  :hit-dice 8
  :stat-changes '((<str> -1) (<int> +2) (<wis> 0)
		  (<dex> +2) (<con> +1) (<chr> -2))
  :abilities '(
	       <free-action>
	       (<infravision> 4)
	       )
  :classes '(<warrior>)
  )

(define-race '<dwarf> "Dwarf"
  :desc "
               Dwarves are the headstrong miners and fighters of legend.
               Since dungeons are the natural home of a dwarf, they are
               excellent choices for a warrior or priest.  Dwarves tend to
               be stronger and tougher but slower and less intelligent than
               humans.  Because they are so headstrong and are somewhat wise,
               they resist spells which are cast on them.  Dwarves also have
               very good infra-vision because they live underground.  They
               do have one big drawback, though.  Dwarves are loudmouthed and
               proud, singing in loud voices, arguing with themselves for no
               good reason, screaming out challenges at imagined foes.  In
               other words, dwarves have a miserable stealth.  They can never
               be blinded. "

  :xp-extra 20
  :hit-dice 11
  :stat-changes '((<str> +2) (<int> -3) (<wis> +2)
		  (<dex> -2) (<con> +2) (<chr> -3))
  :abilities '(
	       (<resist> <blindness>)
	       (<infravision> 5)
	       )
  :classes '(<warrior>)
  )

(define-race '<half-orc> "Half-orc"
  :desc "
               Half-Orcs make excellent warriors and decent priests, but
               are terrible at magic.  They are as bad as dwarves at stealth,
               and horrible at searching, disarming, and perception.
               Half-Orcs are, let's face it, ugly.  They tend to pay more for
               goods in town.  Half-Orcs do make good warriors and rogues,
               for the simple reason that Half-Orcs tend to have great
               constitutions and lots of hit points.  Because of their
               preference to living underground to on the surface, half-orcs
               resist darkness attacks."

  :xp-extra 10
  :hit-dice 10
  :stat-changes '((<str> +2) (<int> -1) (<wis> 0)
		  (<dex> 0) (<con> +1) (<chr> -4))
  :abilities '(
	       (<resist> <dark>)
	       (<infravision> 3)
	       )
  :classes '(<warrior>)
  )

(define-race '<half-troll> "Half-troll"
  :desc "
               Half-Trolls are incredibly strong, and have more hit points
               than any other character race.  They are also very stupid and
               slow.  They will make great warriors and iffy priests.  They
               are bad at searching, disarming, perception, and stealth.
               They are so ugly that a Half-Orc grimaces in their presence.
               They also happen to be fun to run...  Half-trolls always have
               their strength sustained."
  
  :xp-extra 20
  :hit-dice 12
  :stat-changes '((<str> +4) (<int> -4) (<wis> -2)
		  (<dex> -4) (<con> +3) (<chr> -6))
  :abilities '(
	       (<sustain> <str>)
	       (<infravision> 3)
	       )
  :classes '(<warrior>)
  )

(define-race '<dunedan> "Dunedan"
  :desc "
               Dunedain are a race of hardy men from the West.  This elder
               race surpasses human abilities in every field, especially
               constitution.  However, being men of the world, very little is
               new to them, and levels are very hard to gain...  They can
               play all classes.  Their constitution cannot be reduced."

  :xp-extra 80
  :hit-dice 10
  :stat-changes '((<str> +1) (<int> +2) (<wis> +2)
		  (<dex> +2) (<con> +3) (<chr> +2))
  :abilities '(
	       (<sustain> <con>)
	       )
  :classes '(<warrior>)
  )

(define-race '<high-elf> "High-elf"
  :desc "
               High-elves are a race of immortal beings dating from the
               beginning of time.  Levels are even harder for them to gain 
               than to Dunedain.  They are masters of all skills, and are
               strong and intelligent, although their wisdom is sometimes
               suspect.  They can play all classes except Paladin, and very
               well at that.  High-elves begin their lives able to see the
               unseen, and resist light effects just like regular elves."

  :xp-extra 100
  :hit-dice 10
  :stat-changes '((<str> +1) (<int> +3) (<wis> -1)
		  (<dex> +3) (<con> +1) (<chr> +5))
  :abilities '(
	       <see-invisible>
	       (<resist> <light>)
	       (<infravision> 4)
	       )
  :classes '(<warrior> <ranger>)
  )
