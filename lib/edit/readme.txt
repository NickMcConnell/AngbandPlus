Artifact changes:

# New artifacts (22) are as follows:
# 7: The Palantir of Westernesse (original creation by JLE)
# 18: The Balance Dragon Scale Mail 'Mediator' (original JLE creation)
# 26: The Hard Leather Armor of Himring (adapted from OAngband)
# 29: The Shield of Deflection of Gil-galad (adapted from OAngband)
# 33: The Metal Cap of Celebrimbor (suggested by Matthias Kurzke)
# 43: The Jewel-Encrusted Crown of Numenor (adapted from OAngband)
# 51: The Gauntlets of Eol (adapted from OAngband)
# 63: The Soft Leather Boots of Wormtongue (original JLE creation)
# 92: The Spear of Melkor (adapted from KAngband)
# 110: The Beaked Axe of Hurin (adapted from KAngband)
# 123: The Whip of Gothmog (original JLE creation)
# 127: The Heavy Crossbow of Umbar (adapted from OAngband)
# 128, 129: The Short Bows of Amrod and Amras (original JLE creations)

# 130: The Mattock of Nain (original JLE creation)
### (Note that this one REQUIRES the object.txt file from this same patch ###
### and will not work with the object.txt file from unpatched vanilla.     ###

# 131: The Dwarven Pick of Erebor (adapted from SAngband)
# 132: The Ball-and-Chain of Fundin Bluecloak (suggested by Skylar Thompson)
# 133: The Main Gauche of Azaghal (adapted from KAngband)
# 134: The Large Leather Shield of the Haradrim (suggested by Skylar Thompson)
# 135: The Broken Sword 'Narsil' (original JLE creation)

# Two new artifacts have been added but are commented out, and cannot be 
# added properly until there is an increase in the number of amulet flavours 
# in the source file "object1.c". They are:
# 14: The Elfstone 'Elessar'
# 15: The Jewel 'Evenstar'
# If you want to see these artifacts in gameplay, play a variant containing 
# them, such as Psiband or PernAngband.

# - Some existing artifacts have had their powers changed, usually increased:
#   but in four cases the artifact has lost a power. In particular:
# - Thorin, Doomcaller and Zarcuthra no longer provide confusion resistance
#   in addition to chaos resistance. These are the only artifacts to have 
#   actually *lost* any powers...
# - apart from Dor-Lomin, which is no longer *quite* such a no-brainer 
#   without resist blindness.
# - Gurthang has Fire and Poison brands, and provides resistance to both 
#   these elements.
# - Sting slays animals, in memory of its deeds against spiders.
# - The Rings of Power are a great deal nicer.
# - Some items provide resistance to fear: most notably those which are 
#   connected with exceptionally great warriors, or dwarves.
# - Some weapons provide Poison Brand: most notably, Rilia. Also included
#   are the two weapons (Anguirel and Gurthang) that were made by Eol
#   the Dark Elf, who is known to use poisoned weapons.
# - Anduril resists disenchantment. "The blade that is drawn from this 
#   sheath shall never be stained or broken even in defeat." -Galadriel.
# - Various other artifacts may have extra resistances - most notably, 
#   the Golden Crown of Gondor. Some artifacts have their pvals apply to 
#   speed. (In some cases, both of the above are true: hence Colannon, 
#   which now resists nexus (in-theme with its teleportation activation)
#   and has +3 to speed as well as stealth.)
# - The Arkenstone NO LONGER activates for clairvoyance: that activation
#   belongs to the Palantir of Westernesse (read LotR to see why.) Instead,
#   the Arkenstone activates for *detection* and has resist light and dark.

Ego-item changes:

# New ego-items are:
# 10: Armor of Vulnerability (a cursed ego-type for armor)
# 12: Dwarven Armor (heavy metal armor only)
# 21: Shield of Elvenkind (returning from Angband 2.7.8 and previous versions)

# 22: Shield of Preservation
### Note: In one previous version of this patch I inadvertently gave this ###
### item the power of Resist Fire, Cold, Acid and Elec. This should have  ###
### been Ignore Fire, Cold, Acid and Elec - the item should not be hurt,  ###
### but the player should not get the resists. Two high resists, three    ###
### sustains and hold life should be enough.                              ###

# 23: Shield of Vulnerability (a cursed ego-type for shields)
# 36: Crown of Serenity (adapted from OAngband: replaces Helm of Stupidity)
# 37: Crown of Night & Day (partially adapted from an OAngband ego-item)
# 43: Cloak of the Magi
# 52: Gloves of Thievery (leather gloves only)

# 53: Gauntlets of Combat (gauntlets and cesti only)
### Note that this one used to provide extra blows in previous versions ###
### but this was claimed to be unbalancing, so the blows are now gone.  ###

# 60: Boots of Stability (from OAngband)
# 61: Boots of Elvenkind (replaces Boots of Noise - too many cursed types of boot)
# 67: Weapon of Gondolin (rarer and more powerful than Westernesse)
# 70: Weapon of Fury (nastier than a Weapon of Extra Attacks, but also aggravates)
# 76: Weapon of Venom (Poison Brand: does NOT, however, provide poison resist)
# 101: Shovel/Pick of Impact (rare, causes earthquakes)
# 106: Bow of Lothlorien
# 107: Crossbow of the Haradrim
# 110: Sling of Buckland
# 111: Bow/Crossbow of the Nazgul (a cursed ego-type for bows)
# 120: Ammo of Holy Might (seeker, silver and mithril ammo only)
# 121: Ammo of Venom

#  - Also, crowns of Might and Lordliness each provide an extra ability, just 
# like Blessed weapons and Crowns of the Magi.

Object changes:

### New items are:
# 90: Mattock
# 170: Amulet of Sustenance
# 437: Mithril Shot
# 438: Silver Arrow
# 439: Silver Bolt

# In addition, items 423-429 are reserved for additional amulets, as and when
# new flavours are added to object1.c, and depending on when it becomes possible
# to code variable pvals, to-ac, to-hit and to-dam in these text files without 
# code-digging. Likewise, items 513-4 are reserved for two new artifact amulets.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Monster changes:

### New monsters by category:

# New "U" Greater Demons after 2500' (7)
# 567: Barbazu (AD&D 2nd edition: a "barbed devil" in AD&D 1st edition)
# 568: Bile Demon (from 'Dungeon Keeper' - flatulent, poisonous and acidic:
#      if you don't like him, change his hame to the AD&D name of "Amnizu".)
# 569: Osyluth (AD&D 2nd edition: a "bone devil" in AD&D 1st edition)
# 570: Gelugon (AD&D 2nd edition: an "ice devil" in AD&D 1st edition)
# 571: Horned Reaper (from 'Dungeon Keeper' - very big hitter, totally psychotic:
#      if you don't like him, change his name to the AD&D name of "Cornugon".)
# 572: Pit Fiend (from AD&D, a Big Bad Devil in 1st and 2nd editions.)
# 573: Greater Balrog (big, bad, Tolkienish.)

# Filling up the "d"ragon gaps (7) 
# 574: Baby bronze dragon
# 575: Baby gold dragon
# 576: Great Swamp Wyrm (green)
# 577: Great Wyrm of Perplexity (bronze)
# 578: Great Bile Wyrm (black)
# 579: Great Wyrm of Thunder (gold)
# 523: Great Wyrm of Many Colours (ported from ZAngband, replaces Tiamat)

# New "P" Giants (4)
# 584: Cyclops (ported from ZAngband)
# 585: Polyphemus, the Blind Cyclops
# 586: Atlas, the Titan
# 587: Kronos, Lord of the Titans

# Returning from Moria (7)
# 548: Silver Mouse
# 549: Rot Jelly
# 550: Giant Tan Bat
# 551: Giant Silver Ant
# 552: Giant Brown Tick
# 553: Disenchanter Bat
# 554: Shimmering Mold

# Character class types (5)
# 580: Ranger
# 581: Paladin
# 582: Ranger Chieftain
# 436: Knight Templar (replaces Archpriest)
# 583: Berserker

# New "B"irds (4)
# 559: Crow (ported from ZAngband)
# 560: Raven (ported from ZAngband)
# 561: Crebain
# 562: Winged Horror

# New "q"uadrupeds (4)
# 563: Cave bear (ported from ZAngband)
# 564: Grizzly bear (ported from ZAngband)
# 565: Werebear
# 566: Beorn the Shape-Changer

# New "e"yes (4)
# 588: Evil eye
# 589: Spectator (ported from ZAngband)
# 590: Gauth (from AD&D 2nd edition Monster Manual: "beholder-kin" page)
# 591: Beholder Hive-mother (ported from PernAngband's "Ultimate Beholder")

# Miscellaneous others, ported from variants (20)
# 555: Wyvern (from ZAngband)
# 556: Chest mimic (from ZAngband)
# 557: Demilich (from ZAngband)
# 558: Archlich (from ZAngband)
# 597: Silent watcher (from ZAngband)
# 598: Nighthawk (ported from ZAngband - formerly "Hunting Hawk of Julian")
# 599: Ghoul (ported from ZAngband)
# 601: Greater Basilisk (from ZAngband)
# 602: Aranea (from PernAngband)
# 603: Elder aranea (from PernAngband)
# 605: Bat of Gorgoroth (from OAngband)
# 606: Doombat (from PernAngband)
# 607: Wolf chieftain (from OAngband)
# 608: Bone golem (from PernAngband)
# 609: Bronze golem (from PernAngband)
# 611: Ar-Pharazon the Golden (from OAngband)
# 613: Shardstorm (from OAngband)
# 614: Storm of Unmagic (from OAngband)
# 615: Greater mummy (from ZAngband)
# 616: Multi-hued hound (from ZAngband)

# Miscellaneous new monsters, original creations of JLE (11)
# 525: Greater Demonic Quylthulg (replaces The Emperor Quylthulg)
# 543: Huan, Wolfhound of the Valar (replaces Cerberus)
# 592: Neekerbreeker
# 593: Giant firefly
# 594: Eol, the Dark Elf (originally created for KAngband by JLE)
# 595: Maeglin, the Traitor of Gondolin (likewise created for KAngband by JLE)
# 596: Elder vampire
# 600: Ghast
# 604: Kobold shaman
# 610: Ogre chieftain
# 612: Troll chieftain

### Monsters removed:

# 523: Tiamat (Replaced by Great Wyrm of Many Colours. Ancalagon should be 
       the Biggest Toughest Dragon in Angband. He is a lot tougher now.)
# 436: Archpriest (Replaced in monster list by Knight Templar. Originally
#      just about duplicated by Patriarch - two near-identical high Priests.)
# 525: Emperor Quylthulg (Replaced by Greater Demonic Quylthulg. Originally
#      just about duplicated by Qlzqqlzuup - two near-identical unique Qs.)
# 543: Cerberus (Replaced by Huan, Carcharoth's nemesis: a hound who is
#      justified in being of similar power level to Carcharoth. Carcharoth,
#      not Cerberus, is the hound guarding *this* hell's last gate.)

### Monster name and letter changes:
# 94: Novice archer becomes Kobold archer.
# 102: Pink naga returns to being Red naga.
# 103: Pink jelly returns to being Red jelly.
# 104: Giant pink frog returns to being Giant red frog.
# 132: Giant pink ant returns to being Giant red ant.
# 218: Giant pink scorpion returns to being Giant red scorpion.
# 282: Killer pink beetle returns to being Killer red beetle.
# 286: Giant red tick returns to being Giant fire tick.
# 289: Monadic Deva becomes Angel.
# 291: Killer red beetle returns to being Killer fire beetle.
# 311: Movanic Deva becomes Archangel.
# 350: Astral Deva becomes Cherub.
# 374: Giant red ant (formerly red ant lion) becomes Giant fire ant.
# 399: Killer Blue Beetle returns to being Killer Iridescent Beetle.
# 415: Planetar becomes Seraph.
# 455: Solar becomes Archon.
# 483: "Muar" is now simply "The Balrog of Moria".
# 517: Chaos beetle returns to being Jabberwock, and uses letter H.
# 535: "Murazor" is now simply "The Witch-King of Angmar".

### Other monster changes:

#   Dracolisks, dracoliches, Lesser and Greater titans are placed deeper, on 
# the grounds that they were tougher than any of the uniques at their original 
# level and for some levels deeper even than that... compare them with the 
# original wimpy Balrog of Moria, or Smaug, for instance.

#   Big rationalisation of the Dragons: now there is a "baby", "young", 
# "mature", "ancient" and "wyrm" form for all of red, white, blue, green, 
# black, gold, bronze and multi-hued. Other dragons come usually in two types:
# the "drake" and a greater form, sometimes a Great Wyrm (as in drakes and 
# great wyrms of Law, Chaos and Balance.) The Great Crystal Drake, Death 
# Drake and Ethereal Dragon all have names that don't quite match this scheme
# for historical reasons.

#   Changes to monster depths in the late stages: to make sure that at least 
# *one* monster has a native depth at every depth from 2500' to 4000', and 
# no more than one unique per level. No new non-uniques appear after 4000'. 

#   I understand that dracolisks used to breathe nexus, not nether, and that
# this was changed, possibly due to constant confusion with dracoliches. I 
# prefer the original version, and have changed them back. (They still breathe
# fire for a big amount of damage, and who is going to bother resisting nexus 
# at this level? At which point, you suddenly find yourself sinking through 
# the floor of that level with the Greater Vault on it...) It makes sense: 
# they're not undead, and only undead things ought to BREATHE nether - with 
# the possible exception of hounds and vortices.

#   Take a look at the description of the "Angel" and "paladin" monsters. You 
# might realise why you're fighting "good" creatures... 

#   Smeagol is far harder to nail down. But he may - just *might* - drop 
# something nice. Or rob you blind when you can't see him.

#   A lot of inconsistencies have been cleared up, e.g. cave ogres being worth 
# fewer xp than black ogres, or acidic cytoplasms compared to gelatinous cubes. 
# And there are no "left-handed" dragons or hounds with one claw attack bigger 
# than the other: nor do such monsters have one or three claw attacks, but two. 
# All Qs are non-evil (with no intelligence, how can they be evil?) and 
# natural, and have no minds - not even the unique. Pazuzu, described as a 
# demon, actually is one rather than a "B"ird, which he should never have been. 

#   Some non-uniques have escorts - mostly "captain" or "chieftain" monsters, 
# of which there are now some for orcs, trolls and ogres.

#   Monster depths have been reworked so that the last unique in a race usually 
# appears *after* the last non-unique. 

#   Exactly five more monsters have DROP_GREAT - all of them deep except for 
# Mim. (The others are Saruman, Vecna, the Witch-King and Glaurung.)

#   Dragons kick ass in melee combat.

#   Trolls regenerate, as in AD&D.

#   Description and spelling of "Chimaera" corrected to proper Greek myth
# specs, and its three attacks are correct for the creatures: the goat-head 
# butts, the lion bites, the dragon burns. Also it is significantly tougher - 
# originally far too wimpy. The monster was supposed to be terrifying.
#   Gorgimaera (corrected spelling) also changed in similar fashion - the 
# goat head butts, the dragon head burns, the gorgon head gazes to paralyze.

#   "Hippogriff" hybrid is purely eagle and horse, no lion. This was a simple
# error on the part of someone who doesn't know their myths or their Greek 
# well enough.

#   The Lernaean Hydra regenerates: it used to grow a new head every time one
# was cut off. In fact, that is one of the most famous things about it, from 
# the original Hercules story: there really is no excuse for the last set of 
# game designers to have forgotten that.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Changes to vaults:

# - 20 new vaults, mostly designed by Greg Wooledge or Chris Weisiger. 
# These vaults were part of an Angband beta once, removed from the final
# version, now replaced in amended form (e.g. not so many "8"s, and none 
# in Lesser Vaults.)

# - All Greater vaults have at least two "8"s in. All Lesser vaults have 
# at least two "9"s in. This means, two guaranteed great items in a 
# greater vault (each such item might, of course, be only a dagger of 
# slay demon), and two guaranteed good items in a lesser vault. Both are 
# guarded by potentially out-of-depth monsters - up to 11 for lesser 
# vaults, up to 40 for greater vaults.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Player race changes:

# - New player race: the Kobold. Low hit points, resists poison. Good for Con
# and Dex, slightly low Str and Int, VERY low Chr, great infravision.

Player history changes:

# - Kobold histories added.
# - Half-orcs' parentage can be acknowledged (lower social class.)
# - Lots of changes to the Elves. In particular, only High elves are 
# Vanyar, Noldor and Teleri: those elves which are not High are by 
# definition not of these three kindreds, which are the official Three 
# Kindreds of the High-Elves. Instead, normal Elves can be Sindar (the 
# highest social class, those who started the Great Journey to the West
# and got as far as Beleriand), Nandor (those who stopped earlier on the
# Journey, such as the Green-elves of Ossiriand - or arguably the Wood-
# elves of Lorien and Mirkwood), and Avari (those who refused the call
# altogether.)

# - Half-elves can be of any of the three non-high Elven kindreds
# (Sindar, Nandor, Avari), or of half-Noldor parentage: they cannot be 
# of half-Vanyar or half-Teleri, since the Noldor were alone among the
# High-Elves in ever returning to Middle-Earth and encountering Men.

Changes to shop ownership and cost adjustment files:
# - all files changed to take notice of the new Kobold race.

Changes to player class:

# - Paladins no longer get the most powerful priest spells. They're not 
# full priests any more than rangers are full mages. (They still have 
# Orb of Draining, Glyph of Warding and a 300-point heal, so why whine?)

# - Priests only get four attacks per round, like mages. With a 2000-point
# Heal and Orb of Draining both at 0% fail, who needs weapons? Priests 
# should be considered "full" spellcasters just like mages are.

# - Mage spells: Invulnerability fail rate increased to the maximum.
# - Mana cost and fail rates of the middle four Raals spells reduced. 
#   You should have *some* reward for finding Raals early...
# - You now need to be somewhat higher level for the best spells in 
#   "Resistance of Scarabtarices". "Resistance" ought not to be available 
#   as early as level 15: it's now level 30.

# - Rangers get faster pseudo-ID, but only weak pseudo-ID. Comparable to priests.

# - A few new cooler-sounding player titles. "Low Thief" and "High Thief" (and
# similar titles for other classes) lack imagination... 

The "limits.txt" file has been amended to account for all changes to the other 
files.

Have fun. -JLE
