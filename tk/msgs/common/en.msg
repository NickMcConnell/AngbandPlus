set loc en

# This is the Angband(name) variable used for window titles etc
::msgcat::mcset $loc AngbandTk
::msgcat::mcset $loc KAngbandTk
::msgcat::mcset $loc OAngbandTk
::msgcat::mcset $loc ZAngbandTk

::msgcat::mcset $loc Browse...
::msgcat::mcset $loc Cancel
::msgcat::mcset $loc Close
::msgcat::mcset $loc Find
::msgcat::mcset $loc Find...
::msgcat::mcset $loc "Find Again"
::msgcat::mcset $loc OK
::msgcat::mcset $loc "Set Font"
::msgcat::mcset $loc Yes
::msgcat::mcset $loc No
::msgcat::mcset $loc Accept
::msgcat::mcset $loc All

if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
::msgcat::mcset $loc Human
::msgcat::mcset $loc Half-Elf
::msgcat::mcset $loc Elf
::msgcat::mcset $loc Hobbit
::msgcat::mcset $loc Gnome
::msgcat::mcset $loc Dwarf
::msgcat::mcset $loc Half-Orc
::msgcat::mcset $loc Half-Troll
::msgcat::mcset $loc Dunadan
::msgcat::mcset $loc High-Elf

# ANGBANDTK, KANGBANDTK, OANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc Kobold
::msgcat::mcset $loc Maia
::msgcat::mcset $loc Ent
::msgcat::mcset $loc Wraith
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc Maia
::msgcat::mcset $loc Shadow*Fairy "Shadow Fairy"
::msgcat::mcset $loc Ent

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc Human
::msgcat::mcset $loc Half-Elf
::msgcat::mcset $loc Elf
::msgcat::mcset $loc Hobbit
::msgcat::mcset $loc Gnome
::msgcat::mcset $loc Dwarf
::msgcat::mcset $loc Half-Orc
::msgcat::mcset $loc Half-Troll
::msgcat::mcset $loc Amberite
::msgcat::mcset $loc High-Elf
::msgcat::mcset $loc Barbarian
::msgcat::mcset $loc Half-Ogre
::msgcat::mcset $loc Half-Giant
::msgcat::mcset $loc Half-Titan
::msgcat::mcset $loc Cyclops
::msgcat::mcset $loc Yeek
::msgcat::mcset $loc Klackon
::msgcat::mcset $loc Kobold
::msgcat::mcset $loc Nibelung
::msgcat::mcset $loc Dark-Elf
::msgcat::mcset $loc Draconian
::msgcat::mcset $loc Mindflayer
::msgcat::mcset $loc Imp
::msgcat::mcset $loc Golem
::msgcat::mcset $loc Skeleton
::msgcat::mcset $loc Zombie
::msgcat::mcset $loc Vampire
::msgcat::mcset $loc Spectre
::msgcat::mcset $loc Sprite
::msgcat::mcset $loc Beastman

# ZANGBANDTK
}

if {[variant ANGBANDTK KANGBANDTK]} {

::msgcat::mcset $loc Warrior 
::msgcat::mcset $loc Mage 
::msgcat::mcset $loc Priest 
::msgcat::mcset $loc Rogue 
::msgcat::mcset $loc Ranger 
::msgcat::mcset $loc Paladin 

# ANGBANDTK, KANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc Illusionist 
::msgcat::mcset $loc Druid 
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc Warrior 
::msgcat::mcset $loc Mage 
::msgcat::mcset $loc Priest 
::msgcat::mcset $loc Rogue 
::msgcat::mcset $loc Ranger 
::msgcat::mcset $loc Paladin 
::msgcat::mcset $loc Druid 
::msgcat::mcset $loc Necromancer 
::msgcat::mcset $loc Assassin 

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc Warrior 
::msgcat::mcset $loc Mage 
::msgcat::mcset $loc Priest 
::msgcat::mcset $loc Rogue 
::msgcat::mcset $loc Ranger 
::msgcat::mcset $loc Paladin 
::msgcat::mcset $loc Warrior-Mage 
::msgcat::mcset $loc Chaos-Warrior 
::msgcat::mcset $loc Monk 
::msgcat::mcset $loc Mindcrafter 
::msgcat::mcset $loc High-Mage 

# ZANGBANDTK
}

# Book names
if {[variant ANGBANDTK]} {
::msgcat::mcset $loc Magic
::msgcat::mcset $loc Prayer
}
if {[variant KANGBANDTK]} {
::msgcat::mcset $loc Magic
::msgcat::mcset $loc Prayer
::msgcat::mcset $loc Illusion
::msgcat::mcset $loc Nature
}
if {[variant OANGBANDTK]} {
::msgcat::mcset $loc Magic
::msgcat::mcset $loc Prayer
::msgcat::mcset $loc Druid
::msgcat::mcset $loc Necro
}
if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc Life
::msgcat::mcset $loc Sorcery
::msgcat::mcset $loc Arcane
::msgcat::mcset $loc Trump
::msgcat::mcset $loc Nature
::msgcat::mcset $loc Chaos
::msgcat::mcset $loc Death
}

# Stats
::msgcat::mcset $loc strength Strength
::msgcat::mcset $loc intelligence Intelligence
::msgcat::mcset $loc wisdom Wisdom
::msgcat::mcset $loc dexterity Dexterity
::msgcat::mcset $loc constitution Constitution
::msgcat::mcset $loc charisma Charisma

