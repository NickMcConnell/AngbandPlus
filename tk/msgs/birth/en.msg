set loc en

# Window title
::msgcat::mcset $loc "Character Creation"

# Font
::msgcat::mcset $loc font,prompt "{MS Sans Serif} 10 bold underline"

# Button label
::msgcat::mcset $loc "Options"
::msgcat::mcset $loc "< Back"
::msgcat::mcset $loc "Next >"
::msgcat::mcset $loc "Quit"

#Gender
::msgcat::mcset $loc "Choose your character's gender"

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc D_Gender \
	"Females start with more money, and weigh\
	less, but otherwise your character's gender does not have any\
	significant gameplay effects."
} else {
::msgcat::mcset $loc D_Gender \
	"Your character's gender does not have\
	any significant gameplay effects."
}

::msgcat::mcset $loc Female
::msgcat::mcset $loc Male

#Race
::msgcat::mcset $loc "Choose your character's race"
::msgcat::mcset $loc D_Race \
		"Each race has different abilities\
		and bonuses to stats. Any race can play any class, although\
		some races are better at certain classes."

# HTML
::msgcat::mcset $loc "Stat Modifiers"
::msgcat::mcset $loc Skills
::msgcat::mcset $loc Attributes
::msgcat::mcset $loc "Recommended Classes"
::msgcat::mcset $loc attribute "%s (level %d)"

#Class
::msgcat::mcset $loc "Choose your character's class"
::msgcat::mcset $loc D_Class \
		"Your character's class is like his\
		or her profession. Each class has special abilities and \
		weaknesses that make playing them a different challenge."

#Plot
::msgcat::mcset $loc "Choose a plot"
::msgcat::mcset $loc D_Plot \
		"The plot defines all the quests your\
		character must complete to be a total winner."


#Realm
::msgcat::mcset $loc "Choose your character's first realm of magic"
::msgcat::mcset $loc "Choose your character's second realm of magic"
::msgcat::mcset $loc D_Realm \
"The realm of magic will determine which spells your character can learn."

#Race Description

if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
::msgcat::mcset $loc D_Human \
"The human is the base character.  All other races are\
compared to them. Humans can choose any class and are average at everything. \
Humans tend to go up levels faster than any other race because of their\
shorter life spans.  No racial adjustments or intrinsics occur to\
characters choosing human."

::msgcat::mcset $loc D_Half-Elf\
"Half-elves tend to be smarter and faster than a\
human, but not as strong.  Half-elves are slightly better at searching,\
disarming, saving throws, stealth, bows, and magic, but they\
are not as good at hand weapons.  Half-elves may choose any\
class and do not receive any intrinsic abilities."

::msgcat::mcset $loc D_Elf\
"Elves are better magicians then humans, but not as good at\
fighting.  They tend to be smarter and faster than either\
humans or half-elves and also have better wisdom.  Elves\
are better at searching, disarming, perception, stealth, bows,\
and magic, but they are not as good at hand weapons.  Elves\
may choose any class except Paladin.  They resist light\
effects intrinsically."

::msgcat::mcset $loc D_Hobbit\
"Hobbits, or Halflings, are very good at bows, throwing,\
and have good saving throws.  They also are very good at searching,\
disarming, perception, and stealth; so they make excellent rogues, but\
prefer to be called burglars.  They will be much weaker than humans,\
and no good at melee fighting.  Halflings have fair infravision, so\
they can detect warm creatures at a distance.  Hobbits can choose\
between being a warrior, mage, or rogue.  They have their dexterity\
sustained."

::msgcat::mcset $loc D_Gnome\
"Gnomes are smaller than dwarves but larger than\
halflings.  They, like the halflings, live in the earth in burrow-like\
homes.  Gnomes make excellent mages, and have very good saving\
throws.  They are good at searching, disarming, perception,\
and stealth.  They have lower strength than humans so they\
are not very good at fighting with hand weapons.  Gnomes have\
fair infra-vision, so they can detect warm-blooded creatures\
at a distance.  A gnome may choose between being a warrior,\
mage, priest, or rogue.  Gnomes are intrinsically protected\
against paralysis and some slowing effects."

::msgcat::mcset $loc D_Dwarf\
"Dwarves are the headstrong miners and fighters of\
legend.  Since dungeons are the natural home of a dwarf, they are\
excellent choices for a warrior or priest.  Dwarves tend to\
be stronger and tougher but slower and less intelligent than\
humans.  Because they are so headstrong and are somewhat wise,\
they resist spells which are cast on them.  Dwarves also have\
very good infra-vision because they live underground.  They\
do have one big drawback, though.  Dwarves are loudmouthed and\
proud, singing in loud voices, arguing with themselves for no\
good reason, screaming out challenges at imagined foes.  In\
other words, dwarves have a miserable stealth.  They can never\
be blinded."

::msgcat::mcset $loc D_Half-Orc\
"Half-Orcs make excellent warriors and decent priests,\
but are terrible at magic.  They are as bad as dwarves at stealth,\
and horrible at searching, disarming, and perception.  Half-Orcs are,\
let's face it, ugly.  They tend to pay more for goods in town. \
Half-Orcs do make good warriors and rogues, for the simple reason that\
Half-Orcs tend to have great constitutions and lots of hit points. \
Because of their preference to living underground to on the surface,\
half-orcs resist darkness attacks."

::msgcat::mcset $loc D_Half-Troll\
"Half-Trolls are incredibly strong, and have more\
hit points than any other character race.  They are also very stupid and\
slow.  They will make great warriors and iffy priests.  They\
are bad at searching, disarming, perception, and stealth. \
They are so ugly that a Half-Orc grimaces in their presence.\
They also happen to be fun to run...  Half-trolls always have\
their strength sustained."

::msgcat::mcset $loc D_Dunadan\
"Dunadan are a race of hardy men from the West. \
This elder race surpasses human abilities in every field, especially\
constitution.  However, being men of the world, very little is\
new to them, and levels are very hard to gain...  They can\
play all classes.  Their constitution cannot be reduced."

::msgcat::mcset $loc D_High-Elf\
"High-elves are a race of immortal beings dating\
from the beginning of time.  Levels are even harder for them to gain\
than to Dunadan.  They are masters of all skills, and are\
strong and intelligent, although their wisdom is sometimes\
suspect.  They can play all classes except Paladin, and very\
well at that.  High-elves begin their lives able to see the\
unseen, and resist light effects just like regular elves."

# ANGBANDTK, KANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc D_Kobold ""
::msgcat::mcset $loc D_Maia ""
::msgcat::mcset $loc D_Ent ""
::msgcat::mcset $loc D_Wraith ""
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc D_Maia\
"Special race for players new to Oangband.  Score is reduced by 50%.\
The Maiar, lesser deities of the created world, often\
take tangible form and mingle among earthly beings.  They can be found\
among the ranks of wise counselors, peerless warriors, mighty wizards and\
necromancers; indeed they can play any class with credit.  Their vital\
statistics put all other races to shame, their standard abilities are\
always good and sometimes superlative, and they recover divinely\
quickly from all sorts of wounds, poison, stunning, confusion, blindness,\
and paralyzation.  (They do not, however, get any innate resistances\
or special abilities).  Despite all these qualities, Maiar advance fairly\
quickly, making them the perfect race for those desiring a character able\
to survive the perils of an unfamiliar game."

::msgcat::mcset $loc D_Shadow*Fairy\
"Also known as Fays, these beings were created\
neither by the powers of good nor of evil but, for all anyone knows,\
appeared when the world was formed out of primeval darkness and void.  They\
are physically weak and perilously frail, but possess considerable powers\
of intellect and understanding.  They know little of combat, and only have\
modest advantages in disarming and magical device skill - but all this is\
offset by incomparable concealment.  Not merely can Shadow Fairies\
wreath themselves in murk and gloomy night, thus gaining by far the\
highest stealth of any race, but they never aggravate monsters.  (If\
they normally would, they suffer a penalty of three to stealth).\
Shadow Fairies must beware of bright light."

::msgcat::mcset $loc D_Ent\
"Clad in bark-like skin, and with bodies like the trunks\
of trees, Ents can nevertheless move about perfectly well.  They have poor\
dungeon survival skills, and are bad at archery and with polearms, but can\
fight better than average with other weapons.  As long as they stay\
away from fire (fire damage is increased by 33%.  Resistances operate\
normally after this.), Ents will find themselves well capable of surviving\
many a bitter fight.  As they age, Ents slowly lose the relative\
suppleness of their sapling youth, trading dexterity for increased\
strength and constitution.  Members of this wise race can never gain\
feather fall by any means, but will slowly acquire the ability needed\
to tear rock apart with their weaponless hands."

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc D_Human\
"The human is the base character.  All other races are\
compared to them. Humans can choose any class and are average at everything. \
Humans tend to go up levels faster than any other race because of their\
shorter life spans.  No racial adjustments or intrinsics occur to\
characters choosing human."

::msgcat::mcset $loc D_Half-Elf\
"Half-elves tend to be smarter and faster than a\
human, but not as strong.  Half-elves are slightly better at searching,\
disarming, saving throws, stealth, bows, and magic, but they\
are not as good at hand weapons.  Half-elves may choose any\
class and do not receive any intrinsic abilities."

::msgcat::mcset $loc D_Elf\
"Elves are better magicians then humans, but not as good at\
fighting.  They tend to be smarter and faster than either\
humans or half-elves and also have better wisdom.  Elves\
are better at searching, disarming, perception, stealth, bows,\
and magic, but they are not as good at hand weapons.  Elves\
may choose any class except Paladin.  They resist light\
effects intrinsically."

::msgcat::mcset $loc D_Hobbit\
"Hobbits, or Halflings, are very good at bows, throwing,\
and have good saving throws.  They also are very good at searching,\
disarming, perception, and stealth; so they make excellent rogues, but\
prefer to be called burglars.  They will be much weaker than humans,\
and no good at melee fighting.  Halflings have fair infravision, so\
they can detect warm creatures at a distance.  Hobbits can choose\
between being a warrior, mage, or rogue.  They have their dexterity\
sustained."

::msgcat::mcset $loc D_Gnome\
"Gnomes are smaller than dwarves but larger than\
halflings.  They, like the halflings, live in the earth in burrow-like\
homes.  Gnomes make excellent mages, and have very good saving\
throws.  They are good at searching, disarming, perception,\
and stealth.  They have lower strength than humans so they\
are not very good at fighting with hand weapons.  Gnomes have\
fair infra-vision, so they can detect warm-blooded creatures\
at a distance.  A gnome may choose between being a warrior,\
mage, priest, or rogue.  Gnomes are intrinsically protected\
against paralysis and some slowing effects."

::msgcat::mcset $loc D_Dwarf\
"Dwarves are the headstrong miners and fighters of\
legend.  Since dungeons are the natural home of a dwarf, they are\
excellent choices for a warrior or priest.  Dwarves tend to\
be stronger and tougher but slower and less intelligent than\
humans.  Because they are so headstrong and are somewhat wise,\
they resist spells which are cast on them.  Dwarves also have\
very good infra-vision because they live underground.  They\
do have one big drawback, though.  Dwarves are loudmouthed and\
proud, singing in loud voices, arguing with themselves for no\
good reason, screaming out challenges at imagined foes.  In\
other words, dwarves have a miserable stealth.  They can never\
be blinded."

::msgcat::mcset $loc D_Half-Orc\
"Half-Orcs make excellent warriors and decent priests,\
but are terrible at magic.  They are as bad as dwarves at stealth,\
and horrible at searching, disarming, and perception.  Half-Orcs are,\
let's face it, ugly.  They tend to pay more for goods in town. \
Half-Orcs do make good warriors and rogues, for the simple reason that\
Half-Orcs tend to have great constitutions and lots of hit points. \
Because of their preference to living underground to on the surface,\
half-orcs resist darkness attacks."

::msgcat::mcset $loc D_Half-Troll\
"Half-Trolls are incredibly strong, and have more\
hit points than any other character race.  They are also very stupid and\
slow.  They will make great warriors and iffy priests.  They\
are bad at searching, disarming, perception, and stealth. \
They are so ugly that a Half-Orc grimaces in their presence.\
They also happen to be fun to run...  Half-trolls always have\
their strength sustained."

::msgcat::mcset $loc D_Amberite\
"The Amberites are a reputedly immortal race, who are endowed\
with numerous advantages, not only their longevity.  They are\
very tough and endurant; their constitution cannot be reduced,\
and their ability to heal wounds far surpasses that of any\
other race.  Having seen virtually everything, very little\
is new to them, and they gain levels much slower than the\
other races.  But should they advance high enough, they will\
learn the innate Amberite powers of Pattern Mindwalking and\
Shadow Shifting."

::msgcat::mcset $loc D_High-Elf\
"High-elves are a race of immortal beings dating\
from the beginning of time.  Levels are even harder for them to gain\
than to Dunadan.  They are masters of all skills, and are\
strong and intelligent, although their wisdom is sometimes\
suspect.  They can play all classes except Paladin, and very\
well at that.  High-elves begin their lives able to see the\
unseen, and resist light effects just like regular elves."

::msgcat::mcset $loc D_Barbarian\
"Barbarians are hardy men of the north. They are fierce in\
combat, and their wrath is feared throughout the world.\
Combat is their life: they feel no fear, and they learn\
to enter battle frenzy at will even sooner than half-trolls.\
Barbarians are, however, suspicious of magic, which makes\
magic devices fairly hard for them to use, and also makes\
it impossible for them to play Mages. They cannot play\
Paladins, either, because their wild bloodlust is totally\
incompatible with the strict discipline of knighthood."

::msgcat::mcset $loc D_Half-Ogre\
"Half-Ogres are like Half-Orcs, but only more so. They\
are big, bad, and stupid. For warriors, they have all\
the necessary attributes, and they can even become\
wizards: after all, they are related to Ogre Magi,\
from whom they have learned the skill of setting\
trapped runes once their level is high enough. Like Half-\
Orcs, they resist darkness, and like Half-Trolls, they\
have their strength sustained."

::msgcat::mcset $loc D_Half-Giant\
"Half-Giants are not too unusual, as there has been a\
tradition according to which it is a noble and brave thing\
to do to consort a giant (especially a giant-maid). Never-\
theless, the poor offspring of such a union is seldom very\
popular in the world of men. Their limited intelligence\
makes it impossible for them to become full spellcasters,\
but with their huge strength they make excellent warriors.\
They also love to wander in the wilderness, and can become\
rangers. No ordinary wall can withstand the fury of a giant,\
or a half-giant, and at higher levels they can learn the\
power of magical digging. Their thick skin makes them\
resistant to shards, and like Half-Ogres and Half-Trolls,\
they have their strength sustained."

::msgcat::mcset $loc D_Half-Titan\
"Half-mortal descendants of the mighty titans, these\
immensely powerful creatures put almost any other race\
to shame. They may lack the fascinating special powers\
of certain other races, but their enhanced attributes\
more than make up for that. They learn to estimate the\
strengths of their foes, and their love for law and\
order makes them resistant to the effects of Chaos."

::msgcat::mcset $loc D_Cyclops\
"With but one eye, a cyclops can see more than many\
with two eyes. They are headstrong, and loud noises\
bother them very little. They are not quite qualified\
for the magic using professions, but as a certain Mr\
Ulysses can testify, their accuracy with thrown rocks\
can be deadly..."

::msgcat::mcset $loc D_Yeek\
"Yeeks are among the most pathetic creatures. Fortunately,\
their horrible screams can scare away less confident foes,\
and their skin becomes more and more resistant to acid,\
as they gain experience. But having said that, even a\
mediocre monster can proverbially wipe the floor with\
an unwary yeek."

::msgcat::mcset $loc D_Klackon\
"Klackons are bizarre semi-intelligent ant-like insectoid\
creatures. They make great servants and warriors, but\
their mental abilities are severely limited. Obedient\
and well-ordered, they can never be confused. They are\
also very nimble, and become faster as they advance\
levels. They are also very acidic, inherently resisting\
acid, and capable of spitting acid at higher levels."

::msgcat::mcset $loc D_Kobold\
"Kobolds are a weak goblin race. They love poisoned weapons,\
and can learn to throw poisoned darts (of which they carry\
an unlimited supply). They are also inherently resistant to\
poison, and can become adequate fighters, although they are\
not one of the more powerful races."

::msgcat::mcset $loc D_Nibelung\
"The hated and persecuted race of nocturnal dwarves, these\
cave-dwellers are not much bothered by darkness. Their\
natural inclination to magical items has made them immune\
to effects which could drain away magical energy, and like\
ordinary dwarves, they can examine the dungeon to discover\
traps and secret doors."

::msgcat::mcset $loc D_Dark-Elf\
"Another dark, cave-dwelling race, likewise unhampered by\
darkness attacks, the Dark Elves have a long tradition\
and knowledge of magic. With their intelligence they can\
become superb mages or priests, and they have an inherent\
magic missile attack available to them at a low level. With\
their keen sight, they also learn to see invisible things\
as their relatives High-Elves do, but at a higher level."

::msgcat::mcset $loc D_Draconian\
"A humanoid race with dragon-like attributes. As they advance\
levels, they gain new elemental resistances (up to Poison\
Resistance), and they also have a breath weapon, which\
becomes more powerful with experience. The exact type of\
the breath weapon depends on the draconian's class and\
level. With their wings, they can easily escape any pit\
trap unharmed."

::msgcat::mcset $loc D_Mindflayer\
"A secretive and mysterious ancient race. Their civilization\
may well be older than any other on our planet, and their\
intelligence and wisdom are naturally sustained, and are so\
great that they enable Mind Flayers to become more powerful\
spellcasters than any other race, even if their physical\
attributes are a good deal less admirable. As they advance\
levels, they gain the powers of See Invisible, Telepathy\
and a mind blast attack."

::msgcat::mcset $loc D_Imp\
"A demon-creature from the nether-world, naturally resistant\
to fire attacks, and capable of learning fire bolt and fire\
ball attacks. They are little loved by other races, but\
can perform fairly well in most professions."

::msgcat::mcset $loc D_Golem\
"A golem is an artificial creature, built from a lifeless\
raw material like clay, and awakened to life. They are\
nearly mindless, making them useless for professions which\
rely on magic, but as warriors they are very tough. They\
are resistant to poison, they can see invisible things,\
and move freely. At higher levels, they also become resistant\
to attacks which threaten to drain away their life force.\
They also learn to temporarily turn their skin into a very\
hard, stonelike substance. In the most recent version, golems\
gain very little nutrition from ordinary food. They need to\
collect scrolls of satisfy hunger, or perish of exhaustion\
when the life force animating their body runs out. In the\
most recent version golems also gain natural armor class\
bonus from their tough body."

::msgcat::mcset $loc D_Skeleton\
"There are two types of skeletons: the ordinary, warrior-\
like skeletons, and the spell-using skeletons, which are\
also called liches. As undead beings, skeletons need to\
worry very little about poison or attacks that can drain\
life. They do not really use eyes for perceiving things,\
and are thus not fooled by invisibility. Their bones are\
resistant to sharp shrapnels (not much to cut there),\
and they will quickly become resistant to cold. Should\
a skeleton be unlucky enough to lose some of his or her\
remaining life, he or she will learn to restore it at\
will. It is very hard for skeletons to eat food or drink\
potions. Although the magical effects of these will affect\
the skeleton even without entering the skeleton's (non-\
existent) belly, the potion / food itself will fall through\
the skeleton's jaws, giving no nutritional benefit."

::msgcat::mcset $loc D_Zombie\
"Much like Skeletons, Zombies too are undead horrors: they\
are resistant to life-draining attacks, and can learn to\
restore their life-force. Like skeletons, they become\
resistant to cold-based attacks (actually earlier than\
skeletons), resist poison and can see invisible. While\
still vulnerable to cuts (unlike skeletons), Zombies are\
resistant to Nether. Like golems, they gain very little\
nutrition from the food of mortals. However, Zombies are,\
as the name implies, practically mindless: in this company,\
Groo would seem a genius."

::msgcat::mcset $loc D_Vampire\
"One of the mightier undead creatures, the Vampire is an\
awe-inspiring sight. Yet this mighty creature has a serious\
weakness: the bright rays of sun are its bane, and it will\
need to flee the surface to the deep recesses of earth until\
the sun finally sets. Darkness, on the other hand, only makes\
the Vampire stronger. As undead, the Vampire has a firm hold\
on its life force, and resists nether attacks. The Vampire\
also resists cold and poison based attacks. It is, however,\
susceptible to its perpetual hunger for fresh blood, which\
can only be satiated by sucking the blood from a nearby\
monster, which is the Vampire's special power. It should be\
noted that the vampires are so sensitive to daylight that\
even certain artifact light items which are filled with\
daylight will hurt them if they try to wield the items.\
Fortunately, the vampires do not really need these items,\
since they radiate an aura of 'dark light' of their own.\
Light resistance will, in any case, protect the vampire\
from the adverse effects of sunlight."

::msgcat::mcset $loc D_Spectre\
"Another powerful undead creature: the spectre is a ghastly\
apparition, surrounded by an unearthly green glow. They exist\
only partially on our plane of existence: half-corporeal,\
they can pass through walls, although the density of the\
wall will hurt them in the process of doing this. The spectre\
can scream an eldritch howl, which is enough to scare lesser\
monsters witless. As undead, they have a firm hold on their\
life force, see invisible, and resist poison and cold. They\
also resist nether; in fact, their half-corporeal form\
actually grows stronger from the effects of nether. At\
higher levels they develop telepathic abilities. Spectres\
make superb spellcasters, but their physical form is very\
weak. Like Golems and Zombies, Spectres gain almost no\
nutrition from ordinary food."

::msgcat::mcset $loc D_Sprite\
"One of the several fairy races, sprites are very small.\
They have tiny wings, and can fly over traps that may open\
up beneath them. They enjoy sunlight intensely, and need\
worry little about light based attacks. Although physically\
among the weakest races, Sprites are very talented in magic,\
and can become highly skilled wizards. Sprites have the\
special power of spraying Sleeping Dust, and at higher\
levels they learn to fly faster."

::msgcat::mcset $loc D_Beastman\
"This race is a blasphemous abomination produced by Chaos.\
It is not an independent race but rather a humanoid creature,\
most often a human, twisted by the Chaos, or a nightmarsh\
crossbreed of a human and a beast. All Beastmen are accustomed\
to Chaos so much that they are untroubled by confusion and\
sound, although raw Logrus can still have effects on them.\
Beastmen revel in chaos, as it twists them more and more.\
Beastmen are subject to mutations: when they have been\
created, they receive a random mutation. After that,\
every time they advance a level they have a little chance\
of gaining yet another mutation."

# ZANGBANDTK
}

#
# Descriptions of classes
#

if {[variant ANGBANDTK KANGBANDTK]} {

::msgcat::mcset $loc D_Warrior\
"A Warrior is a hack-and-slash character, who solves\
most of his problems by cutting them to pieces, but will occasionally\
fall back on the help of a magical device.  His prime stats are Strength\
and Constitution, and a good Dexterity can really help at times. \
A Warrior will be good at Fighting and Throwing/Bows, but bad at most\
other skills."

::msgcat::mcset $loc D_Mage\
"A Mage must live by his wits.  He cannot hope to simply\
hack his way through the dungeon, and so must therefore use his\
magic to defeat, deceive, confuse, and escape.  A mage is\
not really complete without an assortment of magical devices\
to use in addition to his spells.  He can master the higher\
level magical devices far easier than anyone else, and has\
the best saving throw to resist effects of spells cast at\
him.  Intelligence and Dexterity are his primary stats.\
There is no rule that says a mage cannot become a good\
fighter, but spells are his true realm."

::msgcat::mcset $loc D_Priest\
"A Priest is a character of holy devotion. \
They explore the dungeon only to destroy the evil that lurks within,\
and if treasure just happens to fall into their packs, well, so\
much more to the glory of their church!  Priests receive\
their spells from a deity, and therefore do not choose which\
spells they will learn.  They are familiar with magical devices,\
preferring to call them instruments of god, but are\
not as good as a mage in their use.  Priests have good\
saving throws, and make decent fighters, preferring blunt\
weapons over edged ones.  Wisdom and Charisma are the\
priest's primary stats."

::msgcat::mcset $loc D_Rogue\
"A Rogue is a character that prefers to live by his
cunning, but is capable of fighting his way out of a tight spot.  He\
is the master of traps and locks, no device being impossible\
for him to overcome.  A rogue has a high stealth allowing\
him to sneak around many creatures without having to fight,\
or sneak up and get the first blow.  A rogue's perception is\
higher than any other class, and many times he will notice a\
trap or secret door before having to search.  A rogue is\
better than warriors or paladins with magical devices, but\
still can not rely on their performance.  Rogues can also\
learn a few spells, but not the powerful offensive spells\
magi can use.  A rogue's primary stats are Intelligence and\
Dexterity."

::msgcat::mcset $loc D_Ranger\
"A Ranger is a warrior/mage.  He is a good fighter,\
and the best of the classes with a missile weapon such as a bow.\
The ranger learns spells much more slowly than a mage, but\
is capable of learning all but the most powerful spells.\
Because a ranger is really a dual class character, more\
experience is required for him to advance.  A ranger has a\
good stealth, good perception, good searching, a good saving\
throw, and is good with magical devices.  His primary stats\
are Intelligence and Dexterity."

::msgcat::mcset $loc D_Paladin\
"A Paladin is a warrior/priest.  He is a very good\
fighter, second only to the warrior class, but not very good at missile\
weapons.  He receives prayers at a slower pace then the\
priest, but can even receive the most powerful prayers. \
Because a paladin is really a dual class character, it\
requires more experience to advance him.  A paladin lacks\
much in the way of abilities.  He is poor at stealth, perception,\
searching, and magical devices.  He has a decent\
saving throw due to his divine alliance.  His primary stats\
are Strength and Charisma."

# ANGBANDTK, KANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc D_Illusionist ""
::msgcat::mcset $loc D_Druid ""
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc D_Warrior\
"The great melee fighter, capable of doing more damage and surviving\
longer at close quarters than any other class.  So skilled in combat\
can a warrior become that he gains permanent protection from fear at\
level 30, can spread attacks out among weaker monsters at level 35, and\
regenerates intrinsically at level 45.  He is unchallengable with melee\
weapons, and is also good with all missile launchers, getting an extra\
shot with bows (level 35) and crossbows (level 40).  His biggest\
drawback is his ignorance of spells and many magical items.  Most of\
his dungeon survival skills are mediocre.  A Warrior learns no magical\
spells, and has poor magical defences.  He has very fast, strong\
pseudo-ID.  His prime stat is Strength, and a good Dexterity and\
Constitution can really help at times."

::msgcat::mcset $loc D_Mage\
"Master of sorcery.  A mage is not really complete without an assortment\
of magical devices to use in addition to his spells.  He can master\
the higher level magical devices far more easily than anyone else.\
He fights very poorly, except with the lightest of weapons.  He is bad\
at archery.  A mage is stealthy at low levels, but his magic betrays\
his presence at high.  Most of his other skills are mediocre.  He has\
excellent magical defences, but dangerously poor physical ones.  He has\
very slow, weak pseudo-ID.  Intelligence is his primary stat, and a\
high Constitution is very useful."

::msgcat::mcset $loc D_Priest\
"Covenented with a mighty and generous god. He explores the dungeon only\
to destroy the evil that lurks within, and if treasure just happens to\
fall into his pack, well, so much more to the glory of his church!\
A priest is spoilt for choice with protection and healing prayers, and\
posesses an unequalled collection of knowledge gaining prayers.  He is\
a fair fighter with blunt weapons and miserable with edged.  He is poor\
at archery.  His magical defences are excellent, and his physical ones\
acceptable.  Priests are familiar with magic items.  He has merely\
mediocre stealth and dungeon survival skills.  Priests gain +1 to light\
radius at level 35 and above and permanant protection against evil at\
level 50.  He has very fast but weak pseudo-ID.  Wisdom is a Priest's\
primary stat."

::msgcat::mcset $loc D_Rogue\
"The flexible dungeon sneaker.  First in line when dungeon survival\
skills were handed out.  He can fight very well with light weapons and\
is dangerous with a sling (gets extra shots at 20th and 40th level).\
He is clumsy with heavy weapons.  He has decent magical skills.  A\
Rogue will have a fairly decent collection of spells to improve his\
natural skills, aid in battle, and gain information, but almost no\
ability to deal magical damage.  He can fight hand-to-hand more\
quietly than can any other class, and gets the most effective sneak\
attacks against sleeping monsters.  Rogues have the unique capacity of\
filching purses and setting traps (use the '+' command).  See the help\
file on combat for more details.  Unfortunately, no shopkeeper trusts\
a rogue, and so members of this class will pay dearly for goods in\
town.  A Rogue has acceptable physical and magical defences.  He has\
moderately fast, strong pseudo-ID.  His primary stats are Intelligence\
and Dexterity."

::msgcat::mcset $loc D_Ranger\
"An archer skilled in nature lore.  He is better than any other class\
with any missile weapon except slings.  A Ranger gets one extra shot\
with any missile weapon (25th level for slings, 45th for xbows), and\
two with bows (20th and 40th level).  He is a merely acceptable melee\
fighter, yet not as limited to light weapons as is a Rogue.  A ranger\
can use magical items effectively, and finds his spells handy but not\
critical.  A Ranger has decent dungeon survival skills, and good\
stealth.  He has acceptable magical and physical defences.  He has\
fairly slow, weak pseudo-ID.  His primary stats are Wisdom and\
Dexterity."

::msgcat::mcset $loc D_Paladin\
"A champion of the Faith.  A paladin is the second best melee fighter\
available, as long as he sticks to blunt weapons.  Missile weapons\
confuse him.  He has little stealth, and poor dungeon survial skills, but\
can survive as well or better than any other class in battle or against\
magic.  He has a sub-set of the priestly prayers.  Plus one to light\
radius at level 45 and above.  He has moderately-paced, strong\
pseudo-ID.  His primary stats are Wisdom and Strength."

::msgcat::mcset $loc D_Druid\
"At one with nature.  He posesses a wide and varied range of healing, \
dungeon alteration, and knowledge spells, plus a large number of spells \
to hinder and reduce foes.  Can fight well bare-handed and acceptably \
with slings, but not with other missile weapons or any melee weapon.  \
Although a druid can do a lot of damage, his spells are more random \
than are those of other realms.  He has decent magical skill, fairly \
high stealth, and good magical defences.  A Druids will have decent \
dungeon survival skills.  He has extremely slow, weak pseudo-ID.  His \
primary stat is Wisdom."

::msgcat::mcset $loc D_Necromancer\
"Dealer in death.  He has an unsurpassed collection of highly deadly\
combat spells, and can dish out damage like nobody's business.  His\
magical item ability is acceptable, but virtually all of his other\
skills are poor, and he has a hard time protecting himself.  Fortuately,\
at higher levels his magic can help with some of these problems.  He\
has slow, weak pseudo-ID.  His primary stat is Intelligence,\
and he needs to watch his Constitution."

::msgcat::mcset $loc D_Assassin\
"The death-dealing stalker.  He is better than a Rogue at melee, but is\
still limited to reasonably light weapons.  An Assassin is deadly with\
lighter missile launchers, and can eventually poison missiles, but gets\
no innate extra shots.  He is a master at using thrown weapons against\
sleeping monsters.  He has rather good stealth, but lacks protection\
if caught.  His other dungeon survival skills are good, and he has\
access to a sub-set of the necromantic spells.  He can shoot missiles\
and throw objects more quietly than any other class, and makes little\
noise in melee as well.  An Assassin will have mediocre magic skill,\
and poor magical defences.  He has moderately fast, strong pseudo-ID.\
His primary stats are Intelligence and Strength."

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc D_Warrior\
"A Warrior is a hack-and-slash character, who solves\
most of his problems by cutting them to pieces, but will occasionally\
fall back on the help of a magical device.  His prime stats are Strength\
and Constitution, and a good Dexterity can really help at times. \
A Warrior will be good at Fighting and Throwing/Bows, but bad at most\
other skills."

::msgcat::mcset $loc D_Mage\
"A Mage must live by his wits.  He cannot hope to simply\
hack his way through the dungeon, and so must therefore use his\
magic to defeat, deceive, confuse, and escape.  A mage is\
not really complete without an assortment of magical devices\
to use in addition to his spells.  He can master the higher\
level magical devices far easier than anyone else, and has\
the best saving throw to resist effects of spells cast at\
him.  Intelligence and Dexterity are his primary stats.\
There is no rule that says a mage cannot become a good\
fighter, but spells are his true realm."

::msgcat::mcset $loc D_Priest\
"A Priest is a character of holy devotion. \
They explore the dungeon only to destroy the evil that lurks within,\
and if treasure just happens to fall into their packs, well, so\
much more to the glory of their church!  Priests receive\
their spells from a deity, and therefore do not choose which\
spells they will learn.  They are familiar with magical devices,\
preferring to call them instruments of god, but are\
not as good as a mage in their use.  Priests have good\
saving throws, and make decent fighters, preferring blunt\
weapons over edged ones.  Wisdom and Charisma are the\
priest's primary stats."

::msgcat::mcset $loc D_Rogue\
"A Rogue is a character that prefers to live by his
cunning, but is capable of fighting his way out of a tight spot.  He\
is the master of traps and locks, no device being impossible\
for him to overcome.  A rogue has a high stealth allowing\
him to sneak around many creatures without having to fight,\
or sneak up and get the first blow.  A rogue's perception is\
higher than any other class, and many times he will notice a\
trap or secret door before having to search.  A rogue is\
better than warriors or paladins with magical devices, but\
still can not rely on their performance.  Rogues can also\
learn a few spells, but not the powerful offensive spells\
magi can use.  A rogue's primary stats are Intelligence and\
Dexterity."

::msgcat::mcset $loc D_Ranger\
"A Ranger is a warrior/mage.  He is a good fighter,\
and the best of the classes with a missile weapon such as a bow.\
The ranger learns spells much more slowly than a mage, but\
is capable of learning all but the most powerful spells.\
Because a ranger is really a dual class character, more\
experience is required for him to advance.  A ranger has a\
good stealth, good perception, good searching, a good saving\
throw, and is good with magical devices.  His primary stats\
are Intelligence and Dexterity."

::msgcat::mcset $loc D_Paladin\
"A Paladin is a warrior/priest.  He is a very good\
fighter, second only to the warrior class, but not very good at missile\
weapons.  He receives prayers at a slower pace then the\
priest, but can even receive the most powerful prayers. \
Because a paladin is really a dual class character, it\
requires more experience to advance him.  A paladin lacks\
much in the way of abilities.  He is poor at stealth, perception,\
searching, and magical devices.  He has a decent\
saving throw due to his divine alliance.  His primary stats\
are Strength and Charisma."

::msgcat::mcset $loc D_Warrior-Mage\
"A Warrior-Mage is precisely what the name suggests: a cross\
between the warrior and mage classes. While their brothers,\
the Rangers, specialize in Nature magic and survival skills,\
true Warrior-Mages attempt to reach the best of both worlds:\
as magicians, they are trained in Arcane magic and can\
select another realm, eventually learning every spell in\
both of them (although slower than true magicians); as\
warriors they are much superior to the usual Mage class. The\
Warrior-Mage is a truly powerful class, which is\
recommended for the players who want to cast spells but\
whose mages tend to die too quickly.  However, the power\
does not come without a price: Warrior-Mages require\
more experience to advance levels than any other class."

::msgcat::mcset $loc D_Chaos-Warrior\
"Chaos Warriors are the feared servants of the terrible Demon\
Lords of Chaos. Every Chaos Warrior has a Patron Demon, such\
as Arioch, Mabelode or Balaan.  Whenever a Chaos Warrior\
advances a level, his Patron Demon may reward him with a\
Gift of Chaos: he might be healed or polymorphed, his stats\
could be increased, or he might be rewarded with an awesome\
weapon.  Or his Patron Demon might, for some reason, get\
annoyed with him and do something fairly nasty to him, like\
surround him with monsters, drain his stats or wreck his\
equipment. Or they might simply ignore him. The Demon Lords\
of Chaos are chaotic and unpredictable indeed.  The exact\
type of reward depends on the Patron Demon Lord (different\
Demons give different rewards) and chance.\
Chaos Warriors are very good fighters, but they are also\
trained in Chaos magic, and can learn every Chaos spell."

::msgcat::mcset $loc D_Monk\
"The Monk character class is very different from all other\
classes. Although they can use weapons and armour just like\
any other class, their training in martial arts makes them\
much more powerful with no armour nor weapons.  To gain the\
resistances necessary for survival at higher levels a monk\
may need to wear some kind of armour, but if the armour he\
wears is too heavy, it will severely disturb his martial\
arts maneuvers. As the monk advances levels, new, powerful\
forms of attack become available. His defensive capabilities\
(blocking and dodging) increase likewise, but if armour is\
being worn, their effect decreases. It is rumoured that the\
monastic training even makes monks faster than any other\
character class!  Fortunately, also the amount of armour a\
monk can wear, while still fighting efficiently, also\
increases with experience.\
There are three kinds of monasteries: those devoted to Life,\
those devoted to Nature, and those devoted to Death.  These\
are the three possible realms of magic avaiilable to a monk,\
who can learn every prayer in the realm of his choice."

::msgcat::mcset $loc D_Mindcrafter\
"The Mindcrafter is character who uses the powers of mind\
instead of magic. These powers are unique to the Mindcrafter\
class, and vary from simple extrasensory powers to mental\
domination of others.  Since these powers are developed by\
the practice of certain disciplines, a Mindcrafter requires\
no spellbooks to use them.  The available powers are simply\
determined by the character level.  The Mindcrafter uses the\
wisdom score to determine how well he / she can perform the\
psychic powers, and in combat a Mindcrafter is roughly the\
equivalent of a priest, although a Mindcrafter is never\
penalized for wielding an edged weapon (unlike a priest)."

::msgcat::mcset $loc D_High-Mage\
"High Mages are specialist Mages of the highest order. In\
most respects they are exactly like ordinary Mages, although\
their spell stat is slightly higher.  Instead of selecting\
two realms of magic, a High Mage selects only one realm of\
magic and studies it extremely well, gaining substantial\
benefits in mana costs, minimum levels and failure rates."

#
# Description of magic realms
#

::msgcat::mcset $loc D_Life\
"Life magic is 'good' magic; it relies mostly on healing\
and protective spells. It does have a few attack spells as well,\
but these are mostly used for harming and banishing foul minions\
of evil. It is rumoured that there is a secret high level prayer\
which will make the priest (or paladin) completely impervious\
to all forms of hostile action."

::msgcat::mcset $loc D_Sorcery\
"Sorcery is a `meta` realm, including enchantment and\
general spells.  It provides superb protection spells (such as\
Teleport spells for fleeing and even Globe of Invulnerability),\
spells to enhance your odds in combat (Slow Monster, Haste Self,\
Confuse Monster) and, most importantly, a vast selection of\
spells for gathering information: in addition to the usual\
detection and identify spells, one of the standard spellbooks\
has a spell called Identify True, which gives you full knowledge\
of a given object!  In the rare books, there are spells with\
which you can enchant your items or turn unwanted items to\
gold.  However, Sorcery has one weakness: it has no spells \
to deal direct damage to your enemies."

::msgcat::mcset $loc D_Arcane\
"Even more than Sorcery, Arcane magic is a general purpose\
realm of magic. It attempts to encompass all 'useful' spells from\
all realms, and almost succeeds, with the probable exception of\
*Identify*.  This is the downside of Arcane magic: while Arcane\
does have all the necessary 'tool' spells for a dungeon delver,\
it has no ultra-powerful high level spells. As a consequence, all\
Arcane spellbooks can be bought in town.  It should also be noted\
that the 'specialized' realms (i.e. other than Arcane) usually\
offer the same spell at a lower level and cost.  Arcane magic is\
therefore perhaps not recommendable as one's only realm of magic,\
but it should be a very nice addition to fill up the gaps in the\
selection of tools spells in another realm."

::msgcat::mcset $loc D_Trump\
"Trump magic seems an independent source of power, although\
its supposed association with Chaos magic has been mentioned in\
several places. Although it lacks the unpredictable chaotic side-\
effects of Chaos magic, it has a few spells whose exact effets\
seem more or less random.  One such spell is Shuffle: the Trump\
spellbooks actually consist of decks of trumps, and the Shuffle\
spell allows the caster to shuffle the deck and pick one card at\
random.  The effect depends on the card picked, and is not always\
pleasant.  In the Amber universe, the Trump gateways are also a\
major method of transportation: Trump magic has, indeed, an\
admirable selection of teleportation spells.  Since the Trump\
gateways can also be used to summon other creatures, Trump magic\
has an equally impressive selection of summoning spells. However,\
not all monsters appreciate being drawn to another place by Trump\
user. The only summoned creatures whose loyalty is guaranteed are\
the Phantasmal Servants, who lack a will of their own (but can\
develop one, if you treat them badly)."

::msgcat::mcset $loc D_Nature\
"Early levels may be rather difficult for a spellcaster\
relying on Nature magic, as the early spells offer only limited\
protection, detection and curing capabilities.  However, at \
higher levels there are very useful offensive spells available,\
especially should the spellcaster be lucky enough to find an\
extremely rare spellbook called \"Nature`s Wrath\".  Nature also \
has a spell of Herbal Healing, which is the only powerful healing\
spell outside the realm of Life magic."

::msgcat::mcset $loc D_Chaos\
"There are few types of magic more unpredictable and\
difficult to control than Chaos magic.  Chaos is the very \
element of unmaking, and the Chaos spells are the most \
terrible weapons of destruction imaginable.  From Magic\
Missile and Acid Bolt to the medium level Fire Ball and\
and Doom Bolt, and finally to the awesome spells of \
Invoke Logrus, Mana Storm and Call the Void, Chaos offers \
an unsurpassable arsenal of attack spells.  The caster can \
also call on the primal forces of Chaos to induce \
mutations in his / her opponents and even him/herself, but \
otherwise, Chaos has no protective spells. Beware, though, \
Chaos spells are known to backfire easily and product \
undesired effects. This is especially true in the version\
2.1.0 of Zangband and later, where the forces of Chaos can\
easily twist the hapless individual foolish enough to\
invoke them, turning them horrendous spawns of Chaos."

::msgcat::mcset $loc D_Death\
"There is no fouler nor more evil category of spells\
than the necromantic spells of Death Magic.  These spells \
are relatively hard to learn, but at higher levels the\
spells give the caster power over living and the (un)dead.\
Poison, vampirism, death spells and even hellfire can be\
directed by the caster, but the most powerful spells need\
his / her own blood as the focus, often hurting the caster\
in the process of casting.  Should a Death wizard find the\
legendary tome Necronomicon, he can expect to gain very\
great powers indeed, but at a cost: few that have studied\
the accursed tome have retained their sanity."

# ZANGBANDTK
}

# Autoroller
::msgcat::mcset $loc "Autoroller"
::msgcat::mcset $loc "> Hit any key to abort <"

::msgcat::mcset $loc "    Strength:"
::msgcat::mcset $loc "Intelligence:"
::msgcat::mcset $loc "      Wisdom:" 
::msgcat::mcset $loc "   Dexterity:"
::msgcat::mcset $loc "Constitution:"
::msgcat::mcset $loc "    Charisma:"

::msgcat::mcset $loc "Round: %d"
::msgcat::mcset $loc "(Max of %s)"
::msgcat::mcset $loc "Total: %2d/%d"

::msgcat::mcset $loc "Enter minimum desired stats for the 'auto-roller'"
::msgcat::mcset $loc D_auto-roller1\
	"The auto-roller will attempt create a character\
	with stats that are at least as high as you enter here.  It is\
	possible the auto-roller will be unable to roll a character if you\
	set the minimums too high. You can leave any or all of the stats\
	empty."
::msgcat::mcset $loc D_auto-roller2\
	"The auto-roller will attempt create a character\
	with stats that are at least as high as you enter here.  You may leave\
	any or all of the stats empty."

::msgcat::mcset $loc Prompt_Points \
	"Choose your character's starting stats"
if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
	set amount 100
}
if {[variant OANGBANDTK]} {
	set amount 50
}
::msgcat::mcset $loc Info_Points \
	"You may distribute up to 48 points amongst your character's stats. Each\
	unused point is worth $amount gold pieces in starting money."
::msgcat::mcset $loc "(Cost %d)"

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc GHOSTLY "Pass through walls"
::msgcat::mcset $loc HURT_FIRE "Hurt by fire"
::msgcat::mcset $loc HURT_LITE "Hurt by light"
}

if {[variant OANGBANDTK]} {
# player_race.flags_special
::msgcat::mcset $loc SWORD_SKILL "Skill with swords"
::msgcat::mcset $loc POLEARM_SKILL "Skill with polearms"
::msgcat::mcset $loc HAFTED_SKILL "Skill with hafted weapons"
::msgcat::mcset $loc SLING_SKILL "Skill with slings"
::msgcat::mcset $loc BOW_SKILL "Skill with bows"
::msgcat::mcset $loc XBOW_SKILL  "Skill with crossbows"
::msgcat::mcset $loc SWORD_UNSKILL "Clumsy with swords"
::msgcat::mcset $loc POLEARM_UNSKILL "Clumsy with polearms"
::msgcat::mcset $loc HAFTED_UNSKILL "Clumsy with hafted weapons"
::msgcat::mcset $loc SLING_UNSKILL "Clumsy with slings"
::msgcat::mcset $loc BOW_UNSKILL "Clumsy with bows"
::msgcat::mcset $loc XBOW_UNSKILL "Clumsy with crossbows"
::msgcat::mcset $loc HARDY "Recover quicker from poison and cuts</li><li>Resist Black Breath"
::msgcat::mcset $loc HUNGRY "Needs food more often"
::msgcat::mcset $loc DIVINE "Recover quicker from hallucination, blindness, paralysis, confusion, fear, slowness, poison, stun and cuts</li><li>Score reduced by half"
::msgcat::mcset $loc SHADOW "Take 1/3 extra damage from light</li><li>Aggravate monsters disabled with -3 to stealth"
::msgcat::mcset $loc WOODEN "Feather falling disabled</li><li>Take 1/3 extra damage from fire</li><li>Powerful digging with hands</li><li>Strength and constitution increased a point at levels 26, 41 and 46</li><li>Dexterity decreased a point at levels 26, 41 and 46"
}

# Misc prompts in the character info window
::msgcat::mcset $loc prompt-name "Enter a name for your character:"
::msgcat::mcset $loc prompt-finish "Type 's' to restart, Escape to accept"
::msgcat::mcset $loc life-rating "Life Rating: %d/100"

