

Thanks to:
Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch (although I'm using a very old version of it), Pav for maintaining a great *band website, those on oook who gave coding/variant making advice, reported bugs, and/or other helpful feedback, Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, and anyone else who gives comments or suggestions or just plays DaJAngband.
------------------------------------------------------------------------------------------

Readme for DaJAngband 1.1.2
		bugfixes since 1.1.1
  declining to climb rubble no longer takes a turn.
  fixed bug: sometimes an ego with a random power with get throwing might (which uses a pval) as a random power, but not get a pval. (I think it's fixed)
  fixed bug: if a monster is immune to fire/cold, you learn that it is hurt by fire/cold
  fixed bug: the game lets you chose the blank classes at birth
  fixed bug: !temporary boost doesn't wear off when it boosts strength. It should work correctly now. It also appears in self knowledge now.
  fixed bug: garnets are sometimes invisible in tiles mode (assigned to wrong tile -a blank one)
  fixed bug: cannot do anything with squelched items in inventory with hide squelchable turned on

		other changes since 1.1.1
  quality squelch of jewelry. Also, squelch (non-splendid) ego staffs if blunt is set to squelch all but splendid.
  added character history / notetaking from NPP
  can now see any traps in the space when (l)ooking at a space with an object
  PASS_WALL & KILL_WALL monsters are now smart enough to go around permanent walls.
  couple more improvements to necromancy magic realm
  !super spellcasting now partially restores mana (up to 1/10 of max mana +1) as well as their usual effect
  elven cloaks and elven leather caps only pseudo as splendid if they have something other than stealth (because they always have a stealth bonus).
  significant tweaks on empty vault generation & puddle generation
  always recognise egos on aware jewelry without ID (I thought this was already the case, but it seems I was wrong)
  some other tweaks as usual..

		Changes since 1.1.0
  telekinesis bugs fixed. It works the way it should now: must be either in line of sight or within max distance (usually about 8).
  removing curses with enchant scrolls is no longer dependant on the success of the enchantment
  monsters can no longer pick up (or destroy) the special vault chests.
  druids have a few more weapons they can use without pointy penalty
  ?rune of protection is not used up if it fails to make a glyph (like if you're testing out the scroll while standing on stairs..)
  the object list now updates when it should
  home inventory is now in knowledge menu
  more changes to black magic realm: witch's disinfectant strengthened, demon summoning lessened (never in groups and only appear next to the PC about half the time, otherwise appear about 20 spaces away. Also less likely that the evil spells trigger the demon summoning effect), assassin spells modified.

		Changes since 1.0.99

	Fixed bugs since 1.0.99:
  fixed: hidden attributes on randarts (I consider that a bug anyway)
  fixed: power sprite character history is sometimes messed up
  fixed: see invisible doesn't let you see monsters which are temporarily invisible.
  fixed: when arrows get brand/slays from a bow, it sometimes reads flags from the bow which aren't there (this only had an effect on the artifact short bows Amrod and Amras).
  fixed: flags from weapon in shield slot (usually main gauche) can add extra blows to melee.
  fixed: sometimes disturbs when it's not supposed to
  fixed: slime and silver poison display doesn't always update when it should
  fixed: most recommended classes for each race aren't highlighted in the birth menu.
  fixed: doesn't display correct damage for acid-coated ammo
  fixed: wizard lock doesn't work when targetting a space with an object
  fixed: potion of see invisible often only lasts a couple turns
  fixed: Elven cloak of enveloping came up as {splendid} instead of {cursed}. 
  fixed: Monsters try to summon even if all spaces are filled and no new monsters can be brought in: this still may happen with monsters who don't have the SMART flag

	Information Stuff:
 - can now recognise trap types on the map (usually..)
 - if you know that a monster never moves, then you remember where it is.
 - monster list is now displayed in backwords order, so it will usually be toughest to weakest (but it's still by index number).
 - object list added, and object list command ']'
 - Cursed egos (as well as cursed artifacts) now psuedo as 'terrible'. Stuff which previously psuedoed as 'broken' now psuedos (more accurately) as 'worthless'.

	Cool New Features:
 - golem race added: powerful but has NOREGEN flag
 - monsters in groups sometimes wake up their friends
 - themed levels added along with appropriate terrain (water for swamps, ponds for the forests, extra rubble for dwarf mine & earth cave). Ordinary trees are in as monsters but have several hacks which make them more like terrain features (They are detected by mapping, but not by detect monster.)
 - autosaves whenever a new level is generated

	Minor Tweaks:
 - slowing is more likely to work against hasted monsters
 - torch/lantern of darkvision no longer gives darkvision when it's out of fuel
 - amnesia sometimes causes the map to be forgotten like it used to
 - added a couple new potions (replacing !boldness and !lose memories).
 - humans get one extra stat point to spend
 - Mist of amnesia affects monsters within 14 spaces whether in LOS or not
 - monsters that resist nexus have a small chance to resist teleother
 - breath shield effect is a little more effective than it was
 - stat potion allocation modified again to even out stat gain throughout the dungeon

	Other Improvements & new stuff:
 - max fail rate 75% (before all modifiers. That's a soft cap, it can go slightly higher.)
 - IMPACT blows: the earthquake is triggered before the monster actually dies so the monster's drop isn't affected. Quakes are triggered with critical hits and rarely otherwise.
 - re-arranged prayers so that priests only use eight books (previously they were the only class
which still used all nine books in its realm).
 - added some new alchemy realm spells.  The chemical combat spellbook is almost completely redone.
 - player now gets XP for earthquake damage when purposely cast by a spell
 - heard but unseen monsters now appear on the monster list and can be targetted, both without showing the specific race name (just the type).
 - tree monsters and wall monsters block line of sight
 - damage through immunity fixed: can only happen in melee: 3/4 element damage, 1/4 hurt damage
 - *enchant* can re-enchant disenchanted artifacts
 - chance for double ego 'of lightness' on heavy armor (meaning a heavy armor can get "of resist acid of lightness").
 - nature realm re-organized a little (added fire spray and resist silver, removed detect evil)
 - spread effect added for fire spray, bug spray, and thunderclap spells.
 - artifacts now have a maximum depth they can appear.
 - HURT_COLD and HURT_FIRE flags are implemented (but not a lot of monsters have them)
 - White knight and Black knight classes added.

	Known bugs in 1.1.0: 
 - magic staffs don't always reveal (+0 +0) when they pseudo as average.
 - "tried" inscription dissapears from magic staffs when you get a pseudo-ID. (supposed to do this for weapons, but for magic staffs, you want to remember whether you tried (Z)apping it yet.)

		Fixed bugs between 1.0.98 and 1.0.99:
 - fixed: sometimes crashes when an earthquake happens.
 - fixed: earthquake works in vaults.
 - fixed: Darkvision STILL lets you see invisible. (thought it was fixed before, but now it's really fixed..)
 - fixed: weapons with the DANGER flag never hit yourself (but still have "sometimes hits yourself" in the weapon description).
 - fixed: randarts get sentient alignments way too often and can have both good and evil alignments at the same time.
 - fixed: thrown weapon randarts get flags which are only any good if you can wield them.
 - fixed: The spells of some monsters summoned by the call help spell have wrong behavior.
 - fixed: force stack command for staffs doesn't remove random extra ego stuff
 - fixed: some monsters' hit dice don't work (the ones with 200d3 kinda stuff). They end up with much less than they should.
 - fixed: multiple attack throwing weapons work now
 - fixed: burst of light spell now works as it should

		Notable changes between 1.0.98 and 1.0.99:
 - graphics tiles done for old tileset.
 - detection radius is now fixed (like in new V, but not as big an area as new V).
 - probing now reveals everything that cheak_peak reveals except doesn't always reveal spells.
 - slays/brands have partial stacking: (+25% added to x3 multiplier)
 - ability to target monsters in walls
 - new room types: empty vaults (including some room designs in vault.txt which aren't used for real vaults) ..for dungeon veriety.
 - nexus now has approprate effects on monsters
 - polymorph should be much less dangerous now (usually doesn't heal). Polymorph spells/wands are also more likely to work now.
 - chance based on (clevel + luck + device skill) for wand damage to get a boost (not too big a boost)
 - static resistance added (for drain charges) (included in random resistances). IMPORTANT: This is not complete protection like disenchantment resistance is.
 - objects in rubble can be found by searching without digging up the rubble. This is done so people like me don't feel like we have to dig up every pile of rubble.

------------------------------------------------------------------------------------------
	see
http://www.geocities.com/will_asher/MonsterDictionary/DaJAngbandmain.htm
	for complete changelists for each version and future plans.
------------------------------------------------------------------------------------------

	The in-game help file "DJA110", has more details about differences from vanilla. Please read it
if you are not familiar with DaJAngband or if you want more details about stuff.

	More in-depth explaination of a couple DaJAngband effects:

	Silver & Slime: Infections which have no effect in small amounts, but can have nasty effects if you let them accumulate to high levels. Cure wounds potions have no effect on them.  The two most effective things to fight these effects are elven waybread and the potion or spell of purity. Mushrooms of cure serious wounds also has a small anti-silver and slime effect. Mushrooms of health cure a little sliming, and mushrooms of clear mind cure a little silver poison.
	Sliming slowly turns a person's body into a mindless blob of slime, starting by lowering thier constitution. Danger levels for slime: at 20 begins to have occational minor effects, at 40 slime starts to have serious effects, and at 55 slime will be fatal if not cured very quickly. There are more monsters which cause sliming than there are that cause silver poison.
	Silver poison makes a person forget who he is and corrupts the mind so that he is no longer able to think about anything that matters. Its first effects are reduction of wisdom and randomly activated amnesia or hallucenation. Danger levels for silver: at 5-14 has rare minor effects, at 15-24 can have very serious effects, and at 25 silver poison will be fatal if not cured within a few turns of getting to this level.
	Charm prevents melee, shooting, and casting, but only reduces to-hit for throwing.
	Luck affects many different things in mostly minor ways. Beware the black cats. Luck affects chance-realm casters, especially the tourist, more than other classes.

	A note about magicbooks: Not all classes which are able to cast spells start out with a spellbook, but the class' spell realm is now noted on the birth screen when you're chosing you're character. Also, you probably won't be able to read every book in your magic realm. When in doubt, try to browse a book before buying it: If it shows you the list of spells in the book, then you can use that book, otherwise that book is worthless to you. With the chance realm, the tourist's books are in the general store. The one chance realm book which is sold in the magic store is not useable by tourists, but is of more interest to rogues.
	A note about the Nature and Necromancy realms: These two realms are largely borrowed from OAngband, but if you've played OAngband, it's important to remember that some of the spells that may have the same name as one in OAngband doesn't nessesarily have exactly the save effects. Browse a book and chose a spell to see that's spell's description.
	A further note about the Necromancy realm: The necromancy realm has some very powerful spells, but that realm also has more drawbacks than other realms. (Except maybe the chance realm, but the drawbacks are more obvious in the chance realm.) Several necromancy spells have a chance to leave an aura around you which aggravates demons.