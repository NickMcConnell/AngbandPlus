;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variants/vanilla/config/classes.lisp - classes for vanilla variant
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: Contains definition of classes in sexp.

|#

(in-package :langband)


(define-class '<warrior> "Warrior"
  :desc "
               A Warrior is a hack-and-slash character, who solves most  of
               his  problems  by cutting them to pieces, but will occasionally
               fall back  on the help of a magical device.  His  prime
               stats  are  Strength  and Constitution, and a good Dexterity
               can really help at times.  A Warrior will be good at  Fighting
               and Throwing/Bows, but bad at most other skills."

  :xp-extra 0
  :stat-changes '((<str> +5) (<int> -2) (<wis> -2)
		  (<dex> +2) (<con> +2) (<chr> -1))
  :abilities '(
	       (<resist> <fear> :level 30)
	       )
  :hit-dice 9
  :titles '(
	    "Rookie"
	    "Soldier"
	    "Mercenary"
	    "Veteran"
	    "Swordsman"
	    "Champion"
	    "Hero"
	    "Baron"
	    "Duke"
	    "Lord")

  :starting-equipment '((obj :type <broad-sword>)
			(obj :type (<body-armour> <chain>))
			(obj :type (<potion> <berserk-strength>)))

  
  :skills '((<disarming> 25 10)
	    (<device> 18 7)
	    (<saving-throw> 18 10)
	    (<stealth> 1 0)
	    (<search> 14 0)
	    (<perception> 2 0)
	    (<fighting> 70 45)
	    (<shooting> 55 45))
  )

(define-class '<paladin> "Paladin"
  :desc "
               A Paladin is a warrior/priest.  He is a very  good  fighter,
               second  only to the warrior class, but not very good at missile
               weapons.  He receives prayers at a slower pace then the
               priest,  but  can even  receive the most  powerful  prayers.
               Because a paladin is  really  a  dual  class  character,  it
               requires  more  experience  to advance him.  A paladin lacks
               much in the way of abilities.  He is poor at  stealth,
               perception, searching,  and  magical devices.  He has a decent
               saving throw due to his divine alliance.  His primary  stats
               are Strength and Charisma."
 
  :xp-extra 35
  :hit-dice 6
  :stat-changes '((<str> +3) (<int> -3) (<wis> +1)
		  (<dex> 0) (<con> +2) (<chr> +2))
  :abilities nil
  :titles '(
	   "Gallant"
	   "Keeper"
	   "Protector"
	   "Defender"
	   "Warder"
	   "Knight"
	   "Guardian"
	   "Low Paladin"
	   "High Paladin"
	   "Paladin Lord"
	   )
  
    :starting-equipment '((obj :id "broad-sword")
			  (obj :type (<scroll> <protection> <evil>))
			  (obj :type (<spellbook> <priest> <beginner>))
			  )

    :skills '((<disarming> 20 7)
	      (<device> 24 10)
	      (<saving-throw> 25 11)
	      (<stealth> 1 0)
	      (<search> 12 0)
	      (<perception> 2 0)
	      (<fighting> 68 35)
	      (<shooting> 40 30))

    )

(define-class '<ranger> "Ranger"
  :desc "
               A Ranger is a warrior/mage.  He is a good fighter,  and  the
               best  of  the  classes  with a missile weapon such as a bow.
               The ranger learns spells much more slowly than a  mage,  but
               is  capable  of  learning all  but the most powerful spells.
               Because a ranger is really  a  dual  class  character,  more
               experience  is  required for him to advance.  A ranger has a
               good stealth, good perception, good searching, a good saving
               throw,  and is good with magical devices.  His primary stats
               are Intelligence and Dexterity."

  :xp-extra 30
  :hit-dice 4
  :stat-changes '((<str> +2) (<int> +2) (<wis> 0)
		  (<dex> +1) (<con> +1) (<chr> +1))
  :abilities nil
  :titles '(
	    "Runner"
	    "Strider"
	    "Scout"
	    "Courser"
	    "Tracker"
	    "Guide"
	    "Pathfinder"
	    "Low Ranger"
	    "High Ranger"
	    "Ranger Lord"
	    )
  
    :starting-equipment '((obj :type (<spellbook> <mage> <beginner>))
			  (obj :type (<weapon> <short-sword>))
			  (obj :type (<bow> <long>))
			  (obj :type (<normal> <ammo> <arrow>) :amount "6d6")
			  )
    
    :skills '((<disarming> 30 8)
	      (<device> 32 10)
	      (<saving-throw> 28 10)
	      (<stealth> 3 0)
	      (<search> 24 0)
	      (<perception> 16 0)
	      (<fighting> 56 30)
	      (<shooting> 72 45))

    )

(define-class '<priest> "Priest"
  :desc "
               A Priest is a character of holy devotion.  They explore  the
               dungeon  only  to destroy the evil that lurks within, and if
               treasure just happens to fall into  their  packs,  well,  so
               much  more  to  the  glory of their church!  Priests receive
               their spells from a deity, and therefore do not choose which
               spells they will learn.  They are familiar with magical devices,
               preferring to call them instruments of  god,  but  are
               not  as good as a mage in their use.  Priests have good saving
               throws,  and  make  decent  fighters,  preferring  blunt
               weapons  over  edged  ones.  Wisdom  and  Charisma  are  the
               priest's primary stats."

  :xp-extra 20
  :hit-dice 2
  :stat-changes '((<str> -1) (<int> -3) (<wis> +3)
		  (<dex> -1) (<con> 0) (<chr> +2))
  :abilities nil
  :titles '(
	    "Believer"
	    "Acolyte"
	    "Adept"
	    "Curate"
	    "Canon"
	    "Lama"
	    "Patriarch"
	    "Priest"
	    "High Priest"
	    "Priest Lord"
	    )
  
  :starting-equipment '((obj :type <mace>)
			(obj :type (<spellbook> <priest> <beginner>))
			(obj :id "potion-healing")
			)

  :skills '((<disarming> 25 7)
	    (<device> 30 10)
	    (<saving-throw> 32 12)
	    (<stealth> 2 0)
	    (<search> 16 0)
	    (<perception> 8 0)
	    (<fighting> 48 20)
	    (<shooting> 35 20))
  
)

(define-class '<mage> "Mage"
  :desc "
               A Mage must live by his wits.  He cannot hope to simply hack
               his  way  through the dungeon, and so must therefore use his
               magic to defeat,  deceive, confuse, and escape.  A  mage  is
               not really complete without an assortment of magical devices
               to use in addition to his spells.  He can master the  higher
               level  magical  devices far easier than anyone else, and has
               the best saving throw to resist effects of  spells  cast  at
               him.  Intelligence  and   Dexterity  are  his primary stats.
               There is no rule that says  a  mage  cannot  become  a  good
               fighter, but spells are his true realm."

  :xp-extra 30
  :hit-dice 0
  :stat-changes '((<str> -5) (<int> +3) (<wis> 0)
		  (<dex> +1) (<con> -2) (<chr> +1))
  :abilities nil
  :titles '(
	    "Novice"
	    "Apprentice"
	    "Trickster"
	    "Illusionist"
	    "Spellbinder"
	    "Evoker"
	    "Conjurer"
	    "Warlock"
	    "Sorcerer"
	    "Mage Lord"
	    )
  
    :starting-equipment '((obj :id "dagger")
			  (obj :type (<spellbook> <mage> <beginner>))
			  (obj :type (<scroll> <word-of-recall>))
			  ;; hacks
			  (obj :type (<scroll> <illuminate>) :amount 2)
			  (obj :type (<scroll> <enchant> <weapon> <to-hit>))
			  (obj :type (<scroll> <enchant> <weapon> <powerful>))
			  (obj :type (<scroll> <phase-door>))
			  )

    :skills '((<disarming> 30 7)
	      (<device> 36 13)
	      (<saving-throw> 30 9)
	      (<stealth> 2 0)
	      (<search> 16 0)
	      (<perception> 20 0)
	      (<fighting> 34 15)
	      (<shooting> 20 15))
    )

(define-class '<rogue> "Rogue"
  :desc "
               A Rogue is a character that prefers to live by his  cunning,
               but  is capable of fighting his way out of a tight spot.  He
               is the master of traps and locks, no device being impossible
               for  him  to  overcome.  A rogue has a high stealth allowing
               him to sneak around many creatures without having to  fight,
               or sneak up and get the first blow.  A rogue's perception is
               higher than any other class, and many times he will notice a
               trap  or  secret  door  before having to search.  A rogue is
               better than warriors or paladins with magical  devices,  but
               still  can  not  rely on their performance.  Rogues can also
               learn a few spells, but not the  powerful  offensive  spells
               magi  can use.  A rogue's primary stats are Intelligence and
               Dexterity."

  :xp-extra 25
  :hit-dice 6
  :stat-changes '((<str> +2) (<int> +1) (<wis> -2)
		  (<dex> +3) (<con> +1) (<chr> -1))
  :abilities nil
  :titles '(
	    "Vagabond"
	    "Cutpurse"
	    "Robber"
	    "Burglar"
	    "Filcher"
	    "Sharper"
	    "Low Thief"
	    "High Thief"
	    "Master Thief"
	    "Assassin"
	    )
  
  :starting-equipment '((obj :type (<spellbook> <mage> <beginner>))
			(obj :type <small-sword>)
			(obj :type (<body-armour> <soft-leather>))
			)
  
  :skills '((<disarming> 45 15)
	    (<device> 32 10)
	    (<saving-throw> 28 10)
	    (<stealth> 5 0)
	    (<search> 32 0)
	    (<perception> 24 0)
	    (<fighting> 60 40)
	    (<shooting> 66 30))
  
)


