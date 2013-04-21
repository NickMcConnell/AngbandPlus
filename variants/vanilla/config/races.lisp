;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/races.lisp - races for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(define-character-race "human" "Human"
  :symbol '<human>
  :desc "
               The human is the base character.  All other races are compared
               to them.  Humans can choose any class and are average at
               everything.  Humans tend to go up levels faster than any
               other race because of their shorter life spans.  No racial
               adjustments or intrinsics occur to characters choosing human."
  :xp-extra 0
  :hit-dice 10
  :base-age 18
  :mod-age '(1 . 10)
  :base-status 0

  ;; metric
  :m-height 170 :m-height-mod 15
  :f-height 160 :f-height-mod 15
  :m-weight 80  :m-weight-mod 20
  :f-weight 68  :f-weight-mod 15
  
  :classes t
  :skills '((<disarming> 0 0)
	    (<device> 0 0)
	    (<saving-throw> 0 0)
	    (<stealth> 0 0)
	    (<search> 0 0)
	    (<perception> 10 0)
	    (<fighting> 0 0)
	    (<shooting> 0 0))

  )

(define-character-race "half-elf" "Half-elf"
  :symbol '<half-elf>
  :desc "
               Half-elves tend to be smarter and faster than a  human,  but
               not as strong.  Half-elves are slightly better at searching,
               disarming, saving throws, stealth, bows, and magic, but they
               are not as good at hand weapons.  Half-elves may choose any
               class and do not receive any intrinsic abilities."

  :xp-extra 10
  :hit-dice 9
  :base-age 24
  :mod-age '(1 . 10)
  :base-status 0

  ;; metric
  :m-height 165 :m-height-mod 15
  :f-height 155 :f-height-mod 15
  :m-weight 65  :m-weight-mod 15
  :f-weight 60  :f-weight-mod 15
  
  :stat-changes '((<str> -1) (<int> +1) (<wis> 0)
		  (<dex> +1) (<con> -1) (<chr> +1))
  :abilities '(
	       (<infravision> 2)
	       )
  :classes t
  
  :skills '((<disarming> 2 0)
	    (<device> 3 0)
	    (<saving-throw> 3 0)
	    (<stealth> 1 0)
	    (<search> 6 0)
	    (<perception> 11 0)
	    (<fighting> -1 0)
	    (<shooting> 5 0))

  )

(define-character-race "elf" "Elf"
  :symbol '<elf>
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
  :base-age 75
  :mod-age '(1 . 60)
  :base-status 10
  
  ;; metric
  :m-height 160 :m-height-mod 15
  :f-height 150 :f-height-mod 15
  :m-weight 50  :m-weight-mod 12
  :f-weight 45  :f-weight-mod 10
 
  :stat-changes '((<str> -1) (<int> +2) (<wis> +1)
		  (<dex> +1) (<con> -1) (<chr> +1))
  :abilities '(
	       (<resist> <light>)
	       (<infravision> 3)
	       )
  :classes '(<priest> <rogue> <ranger> <warrior> <mage>)

  :skills '((<disarming> 5 0)
	    (<device> 6 0)
	    (<saving-throw> 6 0)
	    (<stealth> 1 0)
	    (<search> 8 0)
	    (<perception> 12 0)
	    (<fighting> -5 0)
	    (<shooting> 15 0))

  )

(define-character-race "hobbit" "Hobbit"
  :symbol '<hobbit>
  :desc "
               Hobbits, or Halflings, are very good at bows, throwing,  and
               have good saving throws.  They also are very good at searching,
               disarming, perception, and stealth; so they make excellent
               rogues, but prefer to be called burglars.  They will be
               much weaker than humans, and no good at melee fighting.
               Halflings have fair infravision, so they can detect warm
               creatures at a distance.  Hobbits can choose between being a
               warrior, mage, or rogue.  They have their dexterity sustained."

  :xp-extra 10
  :hit-dice 7
  :base-age 21
  :mod-age '(1 . 12)
  :base-status 0

  ;; metric
  :m-height 90 :m-height-mod 10
  :f-height 85 :f-height-mod 10
  :m-weight 45  :m-weight-mod 7
  :f-weight 40  :f-weight-mod 7

  :stat-changes '((<str> -2) (<int> +2) (<wis> +1)
		  (<dex> +3) (<con> +2) (<chr> +1))
  :stat-sustains '(<dex>)
  :abilities '((<infravision> 4))
  :classes '(<warrior> <rogue> <mage>)
  
  :skills '((<disarming> 15 0)
	    (<device> 18 0)
	    (<saving-throw> 18 0)
	    (<stealth> 4 0)
	    (<search> 12 0)
	    (<perception> 15 0)
	    (<fighting> -10 0)
	    (<shooting> 20 0))

  )

(define-character-race "gnome"  "Gnome"
  :symbol '<gnome>
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
  :base-age 50
  :mod-age '(1 . 40)
  :base-status 0

  ;; metric
  :m-height 110 :m-height-mod 10
  :f-height 105 :f-height-mod 10
  :m-weight 55  :m-weight-mod 7
  :f-weight 50  :f-weight-mod 7

  :stat-changes '((<str> -1) (<int> +2) (<wis> 0)
		  (<dex> +2) (<con> +1) (<chr> -2))
  :abilities '(
	       <free-action>
	       (<infravision> 4)
	       )
  :classes '(<warrior> <priest> <rogue> <mage>)
  
  :skills '((<disarming> 10 0)
	    (<device> 12 0)
	    (<saving-throw> 12 0)
	    (<stealth> 3 0)
	    (<search> 6 0)
	    (<perception> 13 0)
	    (<fighting> -8 0)
	    (<shooting> 12 0))

  )

(define-character-race "dwarf" "Dwarf"
  :symbol '<dwarf>
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
  :base-age 35
  :mod-age '(1 . 15)
  :base-status 0

  ;; metric
  :m-height 120 :m-height-mod 10
  :f-height 110 :f-height-mod 10
  :m-weight 70  :m-weight-mod 20
  :f-weight 60  :f-weight-mod 15

  
  :stat-changes '((<str> +2) (<int> -3) (<wis> +2)
		  (<dex> -2) (<con> +2) (<chr> -3))
  :abilities '(
	       (<resist> <blindness>)
	       (<infravision> 5)
	       )
  :classes '(<warrior> <rogue> <priest>)
  
  :skills '((<disarming> 2 0)
	    (<device> 9 0)
	    (<saving-throw> 9 0)
	    (<stealth> -1 0)
	    (<search> 7 0)
	    (<perception> 10 0)
	    (<fighting> 15 0)
	    (<shooting> 0 0))
  
  )

(define-character-race "half-orc" "Half-orc"
  :symbol '<half-orc>
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
  :base-age 14
  :mod-age '(1 . 6)
  :base-status -10

  ;; metric
  :m-height 165 :m-height-mod 15
  :f-height 160 :f-height-mod 15
  :m-weight 70  :m-weight-mod 20
  :f-weight 65  :f-weight-mod 15


  :stat-changes '((<str> +2) (<int> -1) (<wis> 0)
		  (<dex> 0) (<con> +1) (<chr> -4))
  :abilities '(
	       (<resist> <darkness>)
	       (<infravision> 3)
	       )
  :classes '(<warrior> <priest> <rogue>)
  
  :skills '((<disarming> -3 0)
	    (<device> -3 0)
	    (<saving-throw> -3 0)
	    (<stealth> -1 0)
	    (<search> 0 0)
	    (<perception> 7 0)
	    (<fighting> 12 0)
	    (<shooting> -5 0))

  )

(define-character-race "half-troll" "Half-troll"
  :symbol '<half-troll>
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
  :base-age 20
  :mod-age '(1 . 10)
  :base-status -15

  ;; metric
  :m-height 240 :m-height-mod 20
  :f-height 225 :f-height-mod 20
  :m-weight 115 :m-weight-mod 20
  :f-weight 100 :f-weight-mod 15
  
  :stat-changes '((<str> +4) (<int> -4) (<wis> -2)
		  (<dex> -4) (<con> +3) (<chr> -6))
  :stat-sustains '(<str>)
  :abilities '((<infravision> 3))
  :classes '(<warrior> <rogue> <priest>)
  
  :skills '((<disarming> -5 0)
	    (<device> -8 0)
	    (<saving-throw> -8 0)
	    (<stealth> -2 0)
	    (<search> -1 0)
	    (<perception> 5 0)
	    (<fighting> 20 0)
	    (<shooting> -10 0))

  )

(define-character-race "dunedan" "Dunedan"
  :symbol '<dunedan>
  :desc "
               Dunedain are a race of hardy men from the West.  This elder
               race surpasses human abilities in every field, especially
               constitution.  However, being men of the world, very little is
               new to them, and levels are very hard to gain...  They can
               play all classes.  Their constitution cannot be reduced."

  :xp-extra 80
  :hit-dice 10
  :base-age 50
  :mod-age '(1 . 20)
  :base-status 10

  ;; metric
  :m-height 200 :m-height-mod 10
  :f-height 190 :f-height-mod 10
  :m-weight 85 :m-weight-mod 20
  :f-weight 80 :f-weight-mod 15
 
  :stat-changes '((<str> +1) (<int> +2) (<wis> +2)
		  (<dex> +2) (<con> +3) (<chr> +2))
  :abilities '(
	       (<sustain> <con>)
	       )
  :classes t
  
  :skills '((<disarming> 4 0)
	    (<device> 5 0)
	    (<saving-throw> 5 0)
	    (<stealth> 2 0)
	    (<search> 3 0)
	    (<perception> 13 0)
	    (<fighting> 15 0)
	    (<shooting> 10 0))

  )

(define-character-race "high-elf" "High-elf"
  :symbol '<high-elf>
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
  :base-age 100
  :mod-age '(2 . 20)
  :base-status 20

  ;; metric
  :m-height 215 :m-height-mod 10
  :f-height 200 :f-height-mod 10
  :m-weight 85 :m-weight-mod 20
  :f-weight 80 :f-weight-mod 15
 
 
  :stat-changes '((<str> +1) (<int> +3) (<wis> -1)
		  (<dex> +3) (<con> +1) (<chr> +5))
  :abilities '(
	       <see-invisible>
	       (<resist> <light>)
	       (<infravision> 4)
	       )
  :classes '(<warrior> <mage> <priest> <rogue> <ranger>)
  
  :skills '((<disarming> 4 0)
	    (<device> 20 0)
	    (<saving-throw> 20 0)
	    (<stealth> 3 0)
	    (<search> 3 0)
	    (<perception> 14 0)
	    (<fighting> 10 0)
	    (<shooting> 25 0))

  )
