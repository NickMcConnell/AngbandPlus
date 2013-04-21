;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/vanilla/classes.lisp - classes for vanilla variant
Copyright (c) 2000-2001 - Stig Erik Sand�

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
               his  problems  by cutting them to pieces, but will occasion-
               ally fall back  on the help of a magical device.  His  prime
               stats  are  Strength  and Constitution, and a good Dexterity
               can really help at times.  A Warrior will be good at  Fight-
               ing and Throwing/Bows, but bad at most other skills."

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

  :starting-equipment '(<broad-sword>
			(<body-armour> <chain>)
			(<potion> <berserk-strength>))
  
  )

(define-class '<paladin> "Paladin"
  :desc "
               A Paladin is a warrior/priest.  He is a very  good  fighter,
               second  only to the warrior class, but not very good at mis-
               sile weapons.  He receives prayers at a slower pace then the
               priest,  but  can even  receive the most  powerful  prayers.
               Because a paladin is  really  a  dual  class  character,  it
               requires  more  experience  to advance him.  A paladin lacks
               much in the way of abilities.  He is poor at  stealth,  per-
               ception,  searching,  and  magical devices.  He has a decent
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
  
    :starting-equipment '(<broad-sword>
			  (<scroll> <protection> <evil>)
			  (<spellbook> <priest> < beginner>))
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
  
    :starting-equipment '((<spellbook> <mage> <beginner>)
			  (<broad-sword>)
			  (<bow> <long>))
    )

(define-class '<priest> "Priest"
  :desc "
               A Priest is a character of holy devotion.  They explore  the
               dungeon  only  to destroy the evil that lurks within, and if
               treasure just happens to fall into  their  packs,  well,  so
               much  more  to  the  glory of their church!  Priests receive
               their spells from a deity, and therefore do not choose which
               spells they will learn.  They are familiar with magical dev-
               ices, preferring to call them instruments of  god,  but  are
               not  as good as a mage in their use.  Priests have good sav-
               ing throws,  and  make  decent  fighters,  preferring  blunt
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
  
  :starting-equipment '(<mace>
			(<spellbook> <priest> <beginner>)
			(<potion> <cure> <healing> <normal>))
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
  
    :starting-equipment '(<dagger>
			  (<spellbook> <mage> <beginner>)
			  (<scroll> <word-of-recall>)) 

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
  
  :starting-equipment '((<spellbook> <mage> <beginner>)
			<small-sword>
			(<body-armour> <soft-leather>))
)

