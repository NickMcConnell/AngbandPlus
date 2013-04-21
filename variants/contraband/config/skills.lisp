(in-package :org.langband.contraband)

(define-skill "smithing" 'smithing '<smithing>
	      :desc "Ability to fix your metal weapons or armour, and even make your own weapons and armour."
	      :index 0
	      )
(define-skill "leatherwork" 'leatherwork '<leatherwork>
	      :desc "Allows you to fix your leather sack, armour, pouch, shoes, etc."
	      :index 1
	      )

(define-skill "woodcraft" 'woodcraft '<woodcraft>
	      :desc "Abilitiy to carve out bows, know weak spots of wooden doors, fix broken wagons, etc."
	      :index 2
	      )

(define-skill "gemcutting" 'gemcutting '<gemcutting>
	      :desc "Ability to separate gems, gold and silver apart, and craft it into jewelry, or decorate weapons and armour."
	      :index 3
	      )

(define-skill "mining" 'mining '<mining>
	      :desc "Knowledge of mining, ore, tunnels, underground safety, and also the tecnhiques used to run a mine.  Useful e.g to get good ore when that is needed for weapon smithing."
	      :index 4
	      )


(define-skill "appraising" 'appraising '<appraising>
	      :desc "Estimate value of objects.  Essential for traders and adventurers who can't carry much but want to get rich."
	      :index 5
	      )

(define-skill "economy" 'economy '<economy>
	      :desc "Ability to trade, get good prices, make a business run with profit and overall accounting skill."
	      :index 6
	      )

(define-skill "conversation" 'conversation '<conversation>
	      :desc "Ability to use proper etiquette, sweet-talk, bribe, trick people into giving out secret information and persuade people to do what you want."
	      :index 7
	      )

(define-skill "monster-lore" 'monster-lore '<monster-lore>
	      :desc "Know about monsters and foes, their special attacks and their weak spots.  Increases the chance of critical hits in combat."
	      :index 8
	      )

(define-skill "object-lore" 'object-lore '<object-lore>
	      :desc "Know the history of objects you find, the legends and even simple stuff like figuring out the purpose and use of the object."
	      :index 9
	      )

(define-skill "history" 'history '<history>
	      :desc "Knowing the history and culture of the land is essential for understanding behaviour, and understanding the events that may occur."
	      :index 10
	      )

(define-skill "intuition" 'intuition '<intuition>
	      :desc "A weird skill that lets you pick mysteriously know of hidden dangers, when people are lying, events that might need your attention, etc."
	      :index 11
	      )

(define-skill "languages" 'languages '<languages>
	      :desc "Understand nuances between languages, understand ancient and foreign languages. Be able to write the language Atrocitas and Copia share."
	      :index 12
	      )

(define-skill "animal-handling" 'animal-handling '<animal-handling>
	      :desc "Ability to calm down animals, control a caravan, understand animal behaviour and to know the weak spots of animals."
	      :index 13
	      )
(define-skill "alchemy" 'alchemy '<alchemy>
	      :desc "Find and identify herbs, know effects of herbs, be able to make potions and to identify potions."
	      :index 14
	      )

#||
(define-skill "riding" 'riding '<riding>
	      :desc "When animals you can ride are implemented, this one can be important."
	      :index 14
	      )

(define-skill "tracking" 'tracking '<tracking>
	      :desc "Will be useful in later versions."
	      :index 15
	      )
||#

(define-skill "magic-devices" 'magic-devices '<magic-devices>
	      :desc "Ability to use magical devices, like wands and rods or even activate the Hat of All-Seeing..."
	      :index 16
	      )

(define-skill "pick-pocket" 'pick-pocket '<pick-pocket>
	      :desc "Abilitiy to steal from other people, and do sleight-of-hand tricks."
	      :index 17
	      )

(define-skill "locksmithing" 'locksmithing '<locksmithing>
	      :desc "Ability to identify and operate locks, traps, etc.  Also allows a skilled player to make, install and disable locks and traps."
	      :index 18
	      )

(define-skill "mechanical" 'mechanical '<mechanical>
	      :desc "Ability to understand mechanical devices, and to build them.  Can be combined with other skills, e.g woodwork+mechanical -> catapult."
	      :index 19
	      )

;; combat related skills

(define-skill "unarmed" 'unarmed '<unarmed>  
	      :desc "Ability to fight with bare hands and feet (martial artists, monks, boxers)."
	      :index 20
	      )

(define-skill "short-blades" 'short-blades '<short-blades> 
	      :desc "Ability to use short blades in combat."
	      :index 21
	      )
   
(define-skill "long-blades" 'long-blades '<long-blades>
	      :desc "Ability to use long blades in combat."
	      :index 22
	      )

(define-skill "polearms" 'polearms '<polearms>
	      :desc "Ability to use polearms or axes in combat."
	      :index 23
	      )

(define-skill "bludgeoning" 'bludgeoning '<bludgeoning> 
	      :desc "Ability to use bludgeoning weapons in combat."
	      :index 24
	      )

(define-skill "archery" 'archery '<archery> 
	      :desc "Ability to use bows, crossbows and thrown weapons."
	      :index 25
	      )

(define-skill "shield" 'shield '<shield> 
	      :desc "Ability to use shields efficiently in combat."
	      :index 26
	      )

(define-skill "heavy-armour" 'heavy-armour '<heavy-armour>
	      :desc "Ability to wear heavy armour efficiently in combat."
	      :index 27
	      )

(define-skill "light-armour" 'light-armour '<light-armour> 
	      :desc "Ability to wear light armour efficiently in combat."
	      :index 28
	      )
   
(define-skill "evasion" 'evasion '<evasion>  
	      :desc "Ability to read an opponent and evade attacks altogteher."
	      :index 29
	      )

;; add magic-related skills later
