

Thanks to:
Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch among other things, Pav for maintaining a great *band website, those on the oook forums who gave coding/variant making advice, reported bugs, and/or other helpful feedback, Buzzkill for the 32x32 tileset for DaJAngband and other feedback, Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, and anyone else who gives comments or suggestions or just plays DaJAngband.
------------------------------------------------------------------------------------------

--- Changelist for DaJAngband v1.3.0 ---

Version 1.3.0 fixes the following bugs:
  fixed bug: mist of amnesia spell can put monsters to sleep instead of just making them unaware of the PC.
  fixed bug: disguised multi-hued poison potion gives itself away on (i)nspection.
  fixed BUG: graphic for PC doesn't work (reverts to default) for certain races/classes (any race or class that has a space in its name)
  fixed BUG: squelch "all cursed except egos" squelches fumbling freely which isn't cursed (but has negative pval)
  fixed BUG: some things about thrown weapons not working correctly, esp. weapons that are supposed to return don't return
  Fixed a minor exploit: Before you could just throw cursed ammo and throwing weapons from the quiver to get rid of them. Now they aren't gotten rid of as easily: Cursed items thrown from the quiver now return to your quiver unless you actually use them against a monster.
  fixed bug having to do with (un)stacking items enhanced by the alchemist's enhance wand spell. 
  fixed a bug which often placed demons on innapropriate themed levels
  some other minor bugs which I found & fixed and forgot to make a note of.

	Interface changes:
  bone weapons are now in the knowledge menu, also there's now a chance for bone weapon randarts.
  ai_packs now defaults to ON. And certain of its effects only happen before the monster has met the PC.
  view_perma_grids now has no effect when using tiles (because it prevents seeing where your light radius ends).
  Tracking origin of items is implemented.

	Dungeon Feature changes:
  Medium vaults added.
  Cavern levels added & tweaked to fit DaJAngband.  Certain themed level types are more likely to be cavern levels.
  Some new pit/nest types added

	Spell / effects changes:
  PCs have a small chance to resist TELE_AWAY monster spell with Rnexus
  Spell range: Most monsters have a shorter (only slighter shorter in most cases) range in which their spells can affect you.  The same is true for PC spells, range for most spells is dependant on character level and has maximum range (20 spaces) if you're level 50, but a few spells max out at a shorter range (will say so in the spell description).
  Hallucenation has been revamped
  You can no longer shoot or throw at monsters which are holding you, and being held hurts ranged hit chance against other monsters as well.
  Disenchantment breath destroys glyphs.

	Object & Trap changes:
  Artifacts more likely to appear in vaults, special chests, or dropped by uniques than otherwise
  A couple new traps have been added.
  Staff/rod of curing now additionally gives very short lasting resistance to blindness and confusion (staff always does, rod has about a 1/3 chance)
  Torches stack if they have same time to the nearest minute (because it doesn't show seconds)
  You can now fall into a pit when you detect a pit in your current space. Also when you try to disarm a pit and trigger it you actually move into the space with the pit (how do you fall into a pit without being in the same space?)
  Ego grenades and ego DSMs have been added (EGO DSMs are rarer and less powerful than the ones in V3.2 -no DSMs 'of speed' or 'resistance').  A few ego types now have a (soft cap) max depth. Plus a couple other minor improvements to egos and pseudo-randarts.

	Player class, race & combat changes:
  Thieves' innate speed bonus now only applies to movement and some other actions (but not combat or spellcasting).  In the future I plan for some objects to have a 'movement speed' bonus which works like this.
  Significant tweaks to tourist and rogue spell lists (chance realm: boomerang throw, teleport control)
  There is now a penalty for firing range weapons at melee range or near maximum range. Also some launchers have shortened range.
  Armor doesn't protect (much) against GAZE, WAIL, or INSULT attacks.
  More changes to the alchemy magic realm including a craft grenade spell.
  Green Knight class added (a knight class for the alchemy realm -I mentioned earlier I'm planning on eventually having a knight class for each magic realm).

	Monster Stuff:
  SMART monsters only have a ~50% chance of choosing a spell as if AI_SMART was on (used to be always).
  NONCOMBAT spells normally are spells a monster can cast while not yet aware of the PC, but monsters are also limited to NONCOMBAT spells whenever the PC is outside of its racial spell range (but within MAX_RANGE), or when the PC is out of its LOS. TRAPS is now also a NONCOMBAT spell with an alternate effect of setting one trap in its own space (often without the PC noticing).
  Effect of DARKNESS monster spell altered to be more effective (still no damage unless the PC somehow becomes vulnerable to weak darkness -which possibly may happen later as an item drawback or something). Also has an alternate effect when cast as a NON_COMBAT spell (darkens the room or area around the monster instead of the PC).
  Monster size implemented, one effect being that monsters size 6 or bigger (hydras and most giants are size 6) effectively have no stealth when they are in your line of sight (their stealth value only affects how often you hear them and how easily they are detected by magic).
  Monster resistance levels: monsters can now be a little resistant, a lot resistant, immune, or especially vulnerable to most elements.  Some monsters (skeletons, creeping coins, etc) have resistance to missile weapon damage (but nothing is completely immune to missile weapon damage).
  Some monsters (not many) have a maximum populaion now, which means if you kill enough of them they are extinct and no more will be generated that game.  A few uniques have a maximum population of 1, but no longer have the UNIQUE flag (the UNIQUE flag has several other effects including making the monster less likely to be affected by spell effects).  I call these 'minor uniques'.
  Small flying monsters can fly past other monsters as if they had MOVE_BODY.
  Townspeople never disturb when they are unaware of the PC. Also, town monsters now never disturb with movement if you are at least clevel 20.
  There are now two different types of 'hit to blind' attacks. Some monsters which hit to blind using their claws (black stalker cat, hobs) or beak (raven, vrock) are not resisted as well by Rblind.  (Rblind gives 100% resistance to normal hit to blind, Rblind adds 35+(level/2) to save vs 'poke your eyes to blind' and the rest of the save is based on DEX.)
  Added SCALE monster flag to make some monsters scale with depth (very few monsters have this -it's mainly for certain theme-only monsters).
  A couple new monsters.

	Minor Tweaks:
  open pits are slightly easier to climb out of
  rods of disarming are (usually) more effective now (different area of effect), also wands/rods of disarming damage mimmics and launchers (because they are traps as well as monsters)
  chest mimmics are (usually) not placed as vault features before dL20
  random annoyances drawback tweaked (doesn't activate as often as before)
  potions of healing no longer give more nourishment than other types of potions (very little)
  uniques cannot be trampled by KILL_BODY monsters
  monster arrows, breath weapons, and (most) bolt spells now make the monster easier to notice
  pit traps are just as hard to get out of as open pits
  DRAIN_MANA monster spell is a little stronger
  PC class affects social class
  gold drops have been toned down (very slightly)
  monsters never heal themselves when they're already fully healthy
  staff of zapping is more effective now.
  regulized timeouts on torches and lanterns so that they are generated with multiples of 10 minutes of fuel.  Maximum torch fuel is now 16 hours (previously 16.667...hours), and maximum lamp fuel is 40 hours (previously 41.667...hours).
  effectiveness of jamming doors has been tweaked (improved) again.

------------------------------------------------------------------------------------------
	see
http://sites.google.com/site/dajangbandwebsite/
	for known bugs, complete changelists for each version, and future plans.

	The in-game help file "DJA120", has more details about differences from vanilla.  Please read it if you are not familiar with DaJAngband or if you want more details about stuff.  Among other things, this file has more details about some of the changes listed here.

	Get Buzzkill's 32x32 tileset here: http://www.mediafire.com/buzzkill#0,1

-------------------------------------------------------------------------
notable changes between 1.2.3 and 1.2.2:
  BR_WALL now has a chance to create walls (more likely if breather can pass/break walls)
  chest drops improved.  Also, sometimes nests have a silver or gold chest placed inside.
  traps gets harder to disarm with dungeon level, also there's a new trap type for >dl40
  tourists get some XP for picking up gold, they also get a charisma-based stealth bonus (which is negative with low CHA)
  weak pseudo is removed
  scroll/staff of darkness has better effect for black magic users
  shortened some rod & DSM recharge times
  resists can protect against inventory damage vs low level monsters (single resist protects against monsters < dL11, double resist protects against monsters < dL18)
  water slows you down a little (same for most monsters)
  teleport/phase can land you inside a vault if you are already inside a vault.
  cause critical wounds monster spell now can cause cuts (as well as cause mortal wounds)
  most summoning spells summon a reduced number of monsters, summon ape spell may summon more than one now, and the single S_MONSTER spell has a small chance to summon up to three if you have bad luck.
  SMART monsters always learn & react to your resists as if AI_LEARN was on. (reduced to 50% chance instead of always since 1.2.3)


notable changes between 1.2.2 and 1.2.1:
  some more useful scrolls and potions have a small chance to appear in (usually small) stacks
  call darkness spell can now do damage without alerting the monster to your presense 
(if d8 < your character level)
  putting something into the quiver (^q) is now a completely separate command from wielding an item (w). (This allows you to wield something directly from your quiver into your weapon slot and vice versa).
  not wearing a shield now slightly raises your strength bonus to damage for weapons at least 10lb
  mind flayers notice you faster if you have telepathy, also telepathy worsens your saving throw against mind blasting. Confusion from mind blasting may bypass Rconf if you have telepathy and bad luck.
  if monster doesn't prefer ranged attacks/spells then range attacks are less common when in melee range

notable changes between 1.2.0 / 1.2.1 and previous versions:
  separate level feelings for danger & treasure
  The zap and 'aim' commands are combined into one (currently either 'a' or 'z' will work for both, but later I'll probably make it only 'z').  This eliminates the silliness that swaps the two commands between the roguelike and numpad keysets.
  Time display in minutes instead of game turns. Game turns are now completely hidden from the player (because they have no meaning in-game).
  Resting while poisoned or cut is now allowed as long as you are above your HP warning mark.  Also, there is now a minimum HP warning mark of  2 + (maxHP / 50)  (which obviously is still way too low for normal use).
  Your character may now take a nap at home to restock stores (to replace going to L1 to wait 1000 turns). This costs two hours of in-game time.
  "know complete monster race info" (cheat_know) no longer prevents scoring (but in other ways still acts as a cheat option (ie: it is marked on the character dump even if you turn it back off)). Functions of cheat_know which I consider cheating (any cheat-knowledge specific to an individual monster) now use cheat_hear.
  removed NO_SLEEP from most hounds
  EXPLODE monster spell added
  Mimmics now actually mimmic stuff! (Object mimmics are indistinguishable from objects unless/until you recognise them with high alertness or the detect hidden spell. Lurkers and trappers are indistinguishable from floor spaces until you recognise them...)
  Certain dungeon monsters now appear in town as weaker-than-average versions with removed (or reduced) experience reward. Also, distance range of monster spells is reduced in town.
  certain monsters give off their own light (mostly sprites, town monsters, and fire-based monsters)
  Monsters now have several spells they are allowed to cast before they have noticed you including heal, darkness, blink, teleport, and temporary invisibility.
  Diggers (or anything with a tunneling bonus) will do extra damage to trees and statues.
  exploding ammo ego and grenades are added (also everburning torch/lantern)
  launchers "of power" or "of accuracy" always have appropriate bonus higher than the other
  Added race-specific ESP on some objects (like Sting and occationally on slaying egos).
  Spikes are more useful: automatically uses up to 7 in one turn (now always uses 5 in 1.3.0), Bashing doors is slightly easier.
  monsters usually don't bash down doors as easily as before.
  create traps scroll / monster spell now may create traps on spaces with objects or monsters (but doesn't always).
  Timed branding spells added. (but some of the ammo branding spells still use the old method).
  Most high resists (namely Rshard, Rnether, Rconf, Rdisen, Rnexus, and Rchaos) reduce damage by a less random amount (higher minimum, lower maximum).
  Even open doors stop magic illumination now. If you cast a light spell while standing in a doorway, it will illuminate in both directions.
  Lightning and cold do more damage in water, fire and acid do less damage in water (from UnAngband).

Other notable features from before 1.2.0:
  added character history / notetaking from NPP
  elven cloaks and elven leather caps only pseudo as splendid if they have something other than stealth (because they always have a stealth bonus).
  always recognise egos on aware jewelry without ID (I thought this was already the case, but it seems I was wrong)
  removing curses with enchant scrolls is no longer dependant on the success of the enchantment
  can now recognise trap types on the map (usually..) (this has been improved since it was first implemented, now kobolds, alchemists, and rogues can actually see what stat would be drained by a dart trap by looking at it).
  object list added, and object list command ']'
  Cursed egos (as well as cursed artifacts) now psuedo as 'terrible'. Stuff which previously psuedoed as 'broken' now psuedos (more accurately) as 'worthless'.
  monsters in groups sometimes wake up their friends
  themed levels added along with appropriate terrain (water for swamps, ponds for the forests, extra rubble for dwarf mine & earth cave). Ordinary trees are in as monsters but have several hacks which make them more like terrain features (They are detected by mapping, but not by detect monster.)
  autosaves whenever a new level is generated
