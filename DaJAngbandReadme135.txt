

Thanks to:
Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch among other things, Pav for maintaining a great *band website, those on the oook forums who gave coding/variant making advice, reported bugs, and/or other helpful feedback, Buzzkill for the 32x32 tileset for DaJAngband and other feedback, Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, and anyone else who gives comments or suggestions or plays DaJAngband.
------------------------------------------------------------------------------------------
		Changes between 1.3.3 and 1.3.5:

Fixed bugs:
- fixed bug: "There's nothing to disarm there" when moving onto new terrain types with EASY_ALTER turned on.
- fixed bug: grenades can get SLAY_ANIMAL ego, but it has no effect because grenades already get a x2 shards multiplier. -grenades of SLAY_ANIMAL now get a x2.5 multiplier against animals which are vulerable to shards.
- fixed bug: entdraught (Ent from Call Help nature spell) can make slime level go up
- fixed bug: display of stat bonuses/sustains is shifted wrong on character screen
- fixed bug: double weapons don't keep double weapon dice when turned into a randart.  see the spear 'Nimar' here: http://angband.oook.cz/ladder-show.php?id=11364  (a non double weapon got turned into a double weapon -because it was based on a double weapon.)
- fixed bug: the tunneling code sometimes has trouble making doors into rooms which are mostly corners (like "circle in a diamond") because it doesn't allow making a corner into an entrance.
- fixed bug: damage display is wrong for call darkness spell
- fixed bug involving monster resistance to light and dark.  The most noticable effect of the bug was that dark hounds and a few other monsters were hurt by light (or dark) more than they were supposed to be.
- fixed bug: corruption increases even after you stop wearing/weilding the corrupting item
- fixed bug: doors are placed one space away from the doorway instead of actually in the doorway.

Dungeon terrain, design & traps:
- separated big rubble and small rubble into two separate terrain features.
- lesser vaults modified so that no lesser vault requires digging through granite to get into the main part of the vault. (May have had their granite entrance replaced by big rubble.  ...although a few still have certain inner parts of the vault which require digging through granite to get to).
- traps tweaked again to be slightly more dangerous.  There's a couple new trap types including a LoS polymorph trap (which is not thoroughly tested...).
- The teleporter box is more controllable and generally works better now.
- Some flavor changes to themed levels.
- room runes added, give a special effect to the whole room (tested only to make sure the mechanics of the room effects work.  Actual game play effects are still mostly untested. -It's hard to test gameplay in wizard mode...)
- secret doors are now usually locked, locked doors are more common in general.
- very shallow levels (<dL8) have bigger chance of being reduced size
- chance for vault/room designs to be flipped (taken from Sil)

Object changes:
- a couple new minor objects, and object tweaks
- rods & stuff more vulnerable to destruction when stacked.
- added staff/wand ego which prevents charges being drained (This is the only ego wands can get)
- tweaked some object/ego generation stuff (including: slightly reduced amount of floor items generated)
- pseudo of "worthless" fully IDs the item (like average/decent does) as long as it's not an ego
- missile launchers and ammo are less likely to have high +dam and +to hit bonuses, and it's harder to get high +dam and +to hit on launchers and ammo using enchant scrolls (higher failure chance).
- ego extra chance for extra dice is now in ego_item.txt
- implemented a weapon speed modifier in object.txt which makes weapons easier (or harder) to get multiple attacks with by modifiying their effective weight for the purpose of the blows calculation. Tweaked a few weapons accordingly.  (Removed the extra dice from the *thanc daggers because daggers are easier to multiple attacks with.)
- fixed: objects placed underneath a statue should be hidden (but they show up on the object list)
- ring brands are now x2 instead of x3. Also you can now get a brand from a weapon (like a main gauche) wielded in the shield slot, but it becomes a x2 brand.
- branded ammo is much more likely to break on hitting a monster (see brandedammo in breakage_chance() )  Also ammo is much more likely to disappear when the destination is a water grid.
- improved mushrooms of stoneskin (Rslime and Rshards among other things)

Monster changes:
- water-only monsters should hide better in water when the PC is not adjacent to them even if the PC has noticed them before.  Water monsters in water are harder to hit with range weapons.
- tweaks made so that shallow monsters are less common in vaults, and groups are less likely to be big in vaults.
- Different actions make different amounts of noise. (In the little time I've had to playtest, this seems to have less gameplay effect that I expected.)  Also, temporary monster effects (like confusion and stunning) are now handled in a separate process_monster funcion.
- monster champions: certain monsters (captains/cheiftains, etc), have a % chance to be named & become a pseudo-unique (kindof like a randart). Many other monster types have a much smaller chance to be named.
- scared monsters can pust past monsters of the same race to run away.
- summon illusion monster spell, also clone self: Make several illusory copies of the monster casting the 
spell. These illusions appear to have the same HP as the casting monster at all times. Upon casting, the caster immediately swaps places with one of the illusions.
- <CENCORED> -This is a certain monster change I don't feel like warning people about beforehand...
- monster trap-setting spell tweaked

Spell changes:
- more minor improvements to the chance magic realm (mainly for tourists).  new spells: awareness of thieves, celebrity watch (basically detect uniques and named monsters), travel journal. (replacing two useless spells and a redundant spell)  Also, tourists get reduced mana compared to other classes that get their 1st spell at clvl 1.

	There are likely some changes not listed here that I have no record of anymore since I lost my old DaJAngband ideas text file.

------------------------------------------------------------------------------------------

changes between 1.3.1 and 1.3.2:
  the HASTE monster spell is slightly weaker for shallow monsters (hastens the monster by a smaller amount)
  randart code updated a bit (forgot to do it when I first added the flags). Only certain (weaker) activations are allowed from the quiver.  Artifacts with powerful activations are prevented from becoming randart throwing weapons.
  When a monster with KILL_BODY would otherwise trample a unique, it instead attempts to push past it. (previously, attempts to trample uniques simply failed).
  fixed BUG: <omitted description of easily exploited bug -reported by Hudstone>
  fixed BUG: randarts with powerful activations can (in theory) become throwing weapons -and activations are allowed from the quiver.
  can no longer get free action or similar flags from weapons in the quiver.
  torches/lanterns with darkvision ego resist light draining
  illumination spells have bigger area of effect in caverns
  monster stunning is slightly more powerful (mainly for the tourist's camera flash spell)
  character dumps now always show depth by both dungeon level and feet. depth_in_feet option still applies for in-game uses.
  fixed BUG: objects sometimes drop in the same grid as a small statue
  fixed BUG: earthquakes sometimes cause hit points to go up (above max) instead of down, causing death by overflow.

changes between 1.3.0 and 1.3.1:
  meteor swarm spell no longer destroys objects, and has a chance for each meteor being a radius 2 ball (instead of radius 1) at very high character levels.
  no stores ironman option no longer closes the home
  fixed a minor bug which made the alchemist's enhance wand spell slightly weaker
  earlier cavern levels have a chance to be lit
  now up to 3 rooms may appear on cavern levels (3 is rare) (not all cavern levels get room(s))
  new wizard mode command: ^a then 'J' jump to any level with a mod: can force cavern level, seek vault, or themed level.
  bug fixed: doesn't display resist silver or resist slime in the description of an object. (Made it look like egos with a random resist didn't get any resist).


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
  Tracking origin of items is implemented. (The game only tracks the origin of artifacts, egos, chests, treasure maps, and maybe a couple other item types)

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
  Noncombat spells normally are spells a monster can cast while not yet aware of the PC, but monsters are also limited to noncombat spells whenever the PC is outside of its racial spell range (but within MAX_RANGE which is 20 spaces), or when the PC is out of its LOS. TRAPS is now also a noncombat spell with an alternate effect of setting one trap in its own space (often without the PC noticing).
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
  The zap and 'aim' commands are combined into one (currently either 'a' or 'z' will work for both, but later I'll probably make it only 'z').  This eliminates the silliness that swaps the two commands between the roguelike and numpad keysets.
  Time display in minutes instead of game turns. Game turns are now completely hidden from the player (because they have no meaning in-game).
  Resting while poisoned or cut is now allowed as long as you are above your HP warning mark.  Also, there is now a minimum HP warning mark of  2 + (maxHP / 50)  (which obviously is still way too low for normal use).
  Your character may now take a nap at home to restock stores (to replace going to L1 to wait 1000 turns). This costs two hours of in-game time.
  "know complete monster race info" (cheat_know) no longer prevents scoring (but in other ways still acts as a cheat option (ie: it is marked on the character dump even if you turn it back off)). Functions of cheat_know which I consider cheating (any cheat-knowledge specific to an individual monster) now use cheat_hear.
  removed NO_SLEEP from most hounds
  Mimmics now actually mimmic stuff! (Object mimmics are indistinguishable from objects unless/until you recognise them with high alertness or the detect hidden spell. Lurkers and trappers are indistinguishable from floor spaces until you recognise them...)
  exploding ammo ego and grenades are added (also everburning torch/lantern)
  launchers "of power" or "of accuracy" always have appropriate bonus higher than the other
  Added race-specific ESP on some objects (like Sting and occationally on slaying egos).
  Spikes are more useful: always uses 5 spikes, bashing doors is slightly easier for the PC, slightly harder for some monsters.
  Most high resists (namely Rshard, Rnether, Rconf, Rdisen, Rnexus, and Rchaos) reduce damage by a less random amount (higher minimum, lower maximum).
  Lightning and cold do more damage in water, fire and acid do less damage in water (from UnAngband).

Other notable features from before 1.2.0:
  added character history / notetaking from NPP
  always recognise egos on aware jewelry without ID
  removing curses with enchant scrolls is no longer dependant on the success of the enchantment
  object list added, and object list command ']'
  Cursed egos (as well as cursed artifacts) now psuedo as 'terrible'. Stuff which previously psuedoed as 'broken' now psuedos (more accurately) as 'worthless'.
  themed levels added along with appropriate terrain (water for swamps, ponds for the forests, extra rubble for dwarf mine & earth cave). Ordinary trees are in as monsters but have several hacks which make them more like terrain features (They are detected by mapping, but not by detect monster.)
  game autosaves whenever a new level is generated

------------------------------------------------------------------------------------------

notable future plans:	(If I ever get around to it...)
- drastically reduce availability of magical trap disarming.
- more terrains: deep water, lava, etc
- wall & door mimics, creeping coins which are mimics, gargoyles which mimic statues, etc
- possibly further reduce ranges for spells & range weapons
- add monster morale level: have monsters who are running away flee towards stairs and take them. see post by half: http://angband.oook.cz/forum/showthread.php?p=63330#post63330 (monster morale)
- Maybe get rid of ai_sound and ai_smell options (always on).

