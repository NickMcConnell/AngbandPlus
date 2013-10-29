

Thanks to:
Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch, Pav for maintaining a great *band website, those on oook who gave coding/variant making advice, reported bugs, and/or other helpful feedback, Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, and anyone else who gives comments or suggestions or just plays DaJAngband.
------------------------------------------------------------------------------------------

version 1.2.2 fixes the following bugs:
  (everburning) torches & lanterns don't give off light
  fixed bug: some monsters described as bugs which shouldn't be
  fixed extra message when using stone to mud on a vein with treasure
  a couple extremely minor bugs related to the HEAL_OTHR monster spell fixed
  fixed bug: Excorsise demons necromancer spell never worked on more than one demon
  (hopefully) fixed crash bug mentioned here: http://angband.oook.cz/forum/showpost.php?p=37451&postcount=1  (caused by the 'load a saved game' function ignoring the fact that some monsters can be temporarily dead.)

Other changes since 1.2.1:
  objects hidden in rubble are no longer restricted to non-gold
  golems no longer start with food (because they can't eat)
  teleport control is slightly more reliable
  'of wounding' with low bonuses pseudos as good instead of excellent
  object detection range is reduced and extra dice is hidden when item is not IDed or pseudoed
  certain ego types (including psuedo-randarts) have greater chance for extra dice.
  psuedo-randart weapons generally improved as weapons (chance for KILL slays among other things)
  athame and fur cloak weigh slightly less than they used to
  some more useful scrolls and potions have a small chance to appear in (usually small) stacks
  call darkness spell can now do damage without alerting the monster to your presense 
(if d8 < your character level)
  room spaces in the area of an earthquake which don't get damage no longer lose the CAVE_ROOM flag (this makes it much easier to re-light areas which have been darkened by earthquakes),
  tweaked rubble-climbing difficulty again (made slightly easier)
  putting something into the quiver (^q) is now a completely separate command from wielding an item (w). (This allows you to wield something directly from your quiver into your weapon slot and vice versa).
  not wearing a shield now slightly raises your strength bonus to damage for weapons at least 10lb
  mind flayers notice you faster if you have telepathy, also telepathy worsens your saving throw against mind blasting. Confusion from mind blasting may bypass Rconf if you have telepathy and bad luck.
  recharging prayer is a little stronger (still weaker than greater recharging wizard spell)
  if monster doesn't prefer ranged attacks/spells then range attacks are less common when in melee range

version 1.2.1 fixes the following bugs:
  monster generation messiness (including very few >L50 monsters, inappropriate escorts & pit monsters, etc)
  crash when recalling non-specific statue (like from knowledge menu)
  sometimes when monsters come back from the dead, the PC goes into limbo.
  pseudo and stacking for grenades doesn't work correctly
  possible to randomly teleport onto a trap door
  sleep monster wakes up sleeping monsters that resist
  also, that bug that a bunch of effects put monsters to sleep should be fixed now.

Changelist for DaJAngband 1.2.0

	Information / Interface Stuff:
  each artifact has its own rating (like ego types already did)
  separate level feelings for danger & treasure
  redo scoring system (first draft version done, needs testing to scale)
  egos in the black marget are fully IDed now (random ability/resist/sustain isn't hidden)
  The zap and 'aim' commands are combined into one (currently either 'a' or 'z' will work for both, but later I'll probably make it only 'z').  This eliminates the silliness that swaps the two commands between the roguelike and numpad keysets.
  Time display in minutes instead of game turns. Game turns are now completely hidden from the player (because they have no meaning in-game).
  Resting while poisoned or cut is now allowed as long as you are above your HP warning mark.  Also, there is now a minimum HP warning mark of  2 + (maxHP / 50)  (which obviously is still way too low for normal use).
  Anything you've seen in shops appears with the known objects in the knowledge menu (which means you can autoinscribe them), even if you the item is unaware (never IDed an item of that type).
  Your character may now take a nap at home to restock stores (to replace going to L1 to wait 1000 turns). This costs two hours of in-game time.
  There is now an option to squelch when destroying an item.
  A new pseudo tag 'decent' means it has bonuses, but not enough to be called "good".  It starts non-existant and broadens with your character level.  You will never see an object marked 'decent' because decent-pseudoed items auto-ID like average ones.
  The object list now sorts by value (if the object is aware).
  Permanent walls are now a different color than granite.
  If you save your macros to a pref file and name is "mymacs.prf" then it will load automatically when you open the game.

	Option Stuff:
  Added an option to turn off level feelings (turns on a much milder version of auto-scum)
  cheat_noid option added: everything is already identified
  Added an option to replace the 'equippy characters' on the main screen with the time display (defaults to on).
  Added an option to prevent autoinscriptions from overwriting an inscription already on the item.
  Changed the default of the autoscum option to off.
  "know complete monster race info" (cheat_know) no longer prevents scoring (but in other ways still acts as a cheat option (ie: it is marked on the character dump even if you turn it back off)). Functions of cheat_know which I consider cheating (any cheat-knowledge specific to an individual monster) now use cheat_hear.

	Monster Stuff:
  groups size for FRIEND1 monsters is reduced
  removed NO_SLEEP from most hounds
  EXPLODE monster spell added
  Mimmics now actually mimmic stuff! (Object mimmics are indistinguishable from objects unless/until you recognise them with high alertness or the detect hidden spell. Lurkers and trappers are indistinguishable from floor spaces until you recognise them...)
  Certain dungeon monsters now appear in town as weaker-than-average versions with removed (or reduced) experience reward. Also, distance range of monster spells is reduced in town.
  certain monsters give off their own light (mostly sprites, town monsters, and fire-based monsters)
  Monsters now have several spells they are allowed to cast before they have noticed you including heal, darkness, blink, teleport, and temporary invisibility.
  Certain town monsters and one or two dungeon monsters are now non-agressive (they never attack unless attacked or aggravated). (*I'm not completely sure if this works correctly or not though, so report bugs..)
  Certain monsters now have the ability to come back from the dead.
  Monsters which are hurt by fire will not summon fire monsters.
  Some significant but not-too-major changes to the monster list and some other tweaks as usual...

	Dungeon Feature Changes:
  Statues added, like trees, primarily for themed levels but sometimes found on normal dungeon levels.
  Trees (and statues) will never be placed in doorways (although they may still rarely block a doorway if there is a group of trees together).
  Diggers (or anything with a tunneling bonus) will do extra damage to trees and statues.
  Monster nests now come in different sizes.
  The dungeon level is occaionally a little smaller than full size.  This is not common.

	Magic Items & object stuff:
  exploding ammo ego and grenades are added (also everburning torch/lantern)
  objects can be dropped (and will stay) in a doorway now
  added potion of resist poison
  artifact DSMs weigh only slightly more than normal DSMs (they used to be over 40lb)
  launchers "of power" or "of accuracy" always have appropriate bonus higher than the other
  Pseudo-randart egos with random magic attributes added (replacing the old 'of randomness' egos), and a couple existing egos have added random stuff.  When I added random slays, I replaced the individual "slay xxx" egos with two 'of slaying' egos, one with one random slay, and one with two random slays.  If it only has one slay it will be listed as a weapon "of slay xxx" instead of "of slaying".
  Two more pointless types of heavy armor have been removed (in addition to the one I removed a while back).
  Added race-specific ESP on some objects (like Sting and occationally on slaying egos).

	Changes to effects & Other stuff:
  spikes are more useful: automatically uses up to 7 in one turn, bashing doors is slightly easier
  monsters usually don't bash down doors as easily as before (needs testing)
  frenzy effect is slightly more potent.  Also the charm effect makes you more vulnerable to amnesia and vice versa (hurts your saving throw against it).
  resist silver & resist slime added to random resistances (and a couple artifacts)
  create traps scroll / monster spell now may create traps on spaces with objects or monsters (but doesn't always).
  Timed branding spells added. (but some of the ammo branding spells still use the old method).
  Most high resists (namely Rshard, Rnether, Rconf, Rdisen, Rnexus, and Rchaos) reduce damage by a less random amount (higher minimum, lower maximum).
  various partial cures for slime and silver poison are generally (slightly) more effective.
  Even open doors stop magic illumination now. If you cast a light spell while standing in a doorway, it will illuminate in both directions.
  Lightning and cold do more damage in water, fire and acid do less damage in water (from UnAngband).

	bugfixes since last version:
  (fixed) some weirdness caused by trees being in the code as monsters.
  (fixed) priest's bless weapon spell only wears off while the weapon is being wielded.
  (fixed) slowing a monster which is already slowed may cause a crash.
  (fixed) unnessesary extra message when moving into a space with rubble.
  (fixed) Artifacts often don't appear in character history.
  (fixed) When you kill a unique which is unseen, it adds a "Killed it" message to the character history.
  (fixed) Activations on armor or (randart) throwing weapons do not recharge if you are wielding a light source that needs fuel.
  (I think it's fixed) magic staffs don't always reveal (+0 +0) when pseudo as average.

	bugfixes between 1.1.1 and 1.1.2
  declining to climb rubble no longer takes a turn.
  fixed bug: sometimes an ego with a random power with get throwing might (which uses a pval) as a random power, but not get a pval. (I think it's fixed)
  fixed bug: if a monster is immune to fire/cold, you learn that it is hurt by fire/cold
  fixed bug: the game lets you chose the blank classes at birth
  fixed bug: !temporary boost doesn't wear off when it boosts strength. It should work correctly now. It also appears in self knowledge now.
  fixed bug: garnets are sometimes invisible in tiles mode (assigned to wrong tile -a blank one)
  fixed bug: cannot do anything with squelched items in inventory with hide squelchable turned on

	Features added in recent versions:
 - added character history / notetaking from NPP
 - elven cloaks and elven leather caps only pseudo as splendid if they have something other than stealth (because they always have a stealth bonus).
 - always recognise egos on aware jewelry without ID (I thought this was already the case, but it seems I was wrong)
 - removing curses with enchant scrolls is no longer dependant on the success of the enchantment
 - can now recognise trap types on the map (usually..) (this has been improved since it was first implemented, now kobolds, alchemists, and rogues can actually see what stat would be drained by a dart trap by looking at it).
 - object list added, and object list command ']'
 - Cursed egos (as well as cursed artifacts) now psuedo as 'terrible'. Stuff which previously psuedoed as 'broken' now psuedos (more accurately) as 'worthless'.
 - monsters in groups sometimes wake up their friends
 - themed levels added along with appropriate terrain (water for swamps, ponds for the forests, extra rubble for dwarf mine & earth cave). Ordinary trees are in as monsters but have several hacks which make them more like terrain features (They are detected by mapping, but not by detect monster.)
 - autosaves whenever a new level is generated

------------------------------------------------------------------------------------------
	see
http://sites.google.com/site/dajangbandwebsite/
	for known bugs, complete changelists for each version, and future plans.

	The in-game help file "DJA120", has more details about differences from vanilla. Please read it
if you are not familiar with DaJAngband or if you want more details about stuff.

------------------------------------------------------------------------------------------
