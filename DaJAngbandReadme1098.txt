

Thanks to:
First to Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch (although I'm using a very old version of it), Pav for hosting the files and maintaining a great *band website, those on oook who gave coding/variant making advice (Pete), Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that very nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, CunningGabe for new names of a couple jellies, and anyone else who gives comments or suggestions or just plays DaJAngband.

------------------------------------------------------------------------------------------

	Fixed Bugs:
 - The three extra slots in the home no longer look like a bug and they're easier to get to now.
 - DaJAngband had inherited a bug from V 3.0.9, which made a scroll of rune of protection be re-useable if you read it from the ground. That is fixed.
 - fixed: In DJA 1.0.97, darkvision allowed you to see invisible.
 - fixed: crash when identifying the sling of snowballs.
 - fixed: orb of draining doesn't always destroy cursed objects.
 - fixed: random resist thing on some egos choses from the completely wrong set of flags.
 - fixed: crash when using a staff of zapping against monsters with PASS_WALL who are in a wall.
 - fixed: detect invisible scroll or spell doesn't detect monsters which are temporarily invisibl.

	Stuff taken from new version of vanilla Angband:
 - shops carry deeper stuff when player gets deeper (I don't think it's as much of a change as in V)
 - mushrooms of emergency and clear mind
 - "always show lists" option added.

	Information stuff:
 - Monster memory is improved (notes HELPER flag and rarity and depth is always known)
 - Birth screen shows a lot more information as you're putting your character together.
 - Object description shows average weapon damage, true bow multiplier (since it's not exactly x2 and x3 anymore), blessed status, and odds of getting a critical hit/shot.
 - Identify and *identify* scrolls have a chance to not be used up when you read them.
 - Searching frequency and alertness are combined, and the (c)haracter screen now shows throwing skill, darkvision, charm resistance, and partial poison resistance.
 - The monster list shows whether monsters are in line of sight or not and whether monsters are aware of you or not.
 - The monster list goes haywire when you're hallucenating.
 - "disturb_espmove" option added to notice when monster has entered your range of telepathy and when it has line of sight to the PC.

	Cool Features: 
 - I'm not going to give nearly as much detail here as I have in the past, but there's several new and interesting magic items and spells (primarily in the prayer and alchemy realms). Among other things, the "prayer" prayer has been replaced, and there is a spell which lets a priest put a temporary blessing on an weapon, letting him wield it without penalty.
 - Magic staffs can now get weapon bonueses and egos. Since this can cause problems with stacking, I also added a free command "&" which can remove unwanted egos and weapon bonuses from a staff to allow it to stack with others of its kind.
 - Throwing and shooting now use two separate skills. Throwing weapons are implemented which can't be used for melee. They also have a chance based on throwing skill (and luck of course) to get branding from elemental rings that you're wearing.
 - Ego rings "of Eregion" and "of Warfare" borrowed from another variant (I forget which now). I had tried to add them before, but now they work.

	Monster changes:
 - There was some dicussion about Z hounds being nerfed a little on the forums, so I thought I'd mention that they're slightly rarer and come in smaller groups than in V, but that change was already in 1.0.97.
 - Monsters are not always asleep before they notice you. They walk around the dungeon, going about their business. The disturb_near option only triggers for monsters who are aware of you unless the monster has just entered your line of sight.
 - You will now always notice a non-invisible monster which is adjacent to you, and sleeping monsters get a penalty to stealth.
 - Monster breath damage is slightly reduced at long range. Also, there are shields with the BREATH_SHIELD flag which give damage reduction vs all monster breath.
 - A couple rather crude AI improvements: Monsters which are not in line of sight are able to blink. Monsters which prefer ranged combat have a little more chance to stay where they are and shoot or cast instead of coming closer to the player.
 - Some types of monsters (thieves, mages, kobolds, gargoyles, etc) are sometimes evil and sometimes not evil. Certain types of young and baby dragons are not always evil, but all the adult and ancient dragons are always evil. Any detect evil spell will let you know whether an individual monster is evil or not. Also, high wisdom characters have a chance to be able to tell whether a monster is evil or not by discernment.
 - Monsters and the player both can now climb over rubble, but not easily. You can never do it if you're carrying enough to be slowed. Monsters which don't have BASH_DOOR and can't fly, can't climb over rubble.
 - Boulders thrown by giants occationally leave rubble.
 - added monster spells which 'heal other monsters' and 'heal kin'. Also groups of orcs, ogres, or gargoyles are sometimes created with a shaman.

	Effects of effects and what they affect:
 - slow, confuse, fear, and sleep are now more effective and their effectiveness is affected by charisma.
 - *destruction* no longer affects vaults.
 - fixed weapon & armor curses (no shattering or removing of egos, added chance to affect range weapons & other armor)
 - traps now have a minimum level to appear on, also added a couple new traps which appear on deep levels.
 - perma_cursed items can be uncursed, but they have a heavy curse and re-curse themselves automatically after awhile.
 - lit rooms are now dimly lit.
 - There are nasty effects of the corrupt flag which is on the One Ring and a couple other egos and artifacts, though not so nasty if you only wear it a short time.  Speaking of item drawbacks, experience drain on items now only kicks in when you attack, shoot, throw something at a monster, or use offensive magic.
 - confusion adds 25% to fail rate instead of automatically preventing you from casting spells. Likewise, confusion gives monsters a high fail rate instead of making them never cast spells. Monster stunning is also weakened. Now they have a fail rate for spells and are much more likely to miss when they're stunned. Before, monsters did nothing at all while they were stunned. Critcal hits have a chance to stun monsters now.
 - Aggravation can now be nullified by very high stealth: If your stealth is high enough, The AGGRAVATE flag will just lower your stealth by a lot instead of actually causing aggravation.
 - cursed ammo/thrown weapons misfire/slip a lot of the time (random direction)

	Little minor stuff:
 - In DJA 1.0.97, you'd get no strength bonus for weapons lighter than 5lb and full strength bonus if they weighed at least 5lb. Now, there's no longer a breakpoint for getting a strength bonus: a dagger gets a small fraction of strength bonus, weapons get bigger fractions until at 7lb, a weapon gets full STR bonus. Then starting at 11lb, you start to get a little more than 1x sTR bonus until at 24lb, you get double STR bonus, and very heavy weapons may get even more than double. (I will likely think twice about this, double or more is probably too much).
 - removed the piece of Eddie's code which added PLITE to all artifacts which weren't obviously excellent.
 - cutlass restored to the damage it had in V. (I had changed it from 1d7 to 1d6)
 - magic staffs of a squelched type won't squelch if it's obviously an ego. Also, ego rings never squelch. (Makes it easy to recognise ego rings of a squelched type, but that's okay with me).

	Known bugs in 1.0.98: 
 - magic staffs don't always reveal (+0 +0) when they pseudo as average.
 - "tried" inscription dissapears from magic staffs when you get a pseudo-ID. (supposed to do this for weapons, but for magic staffs, you want to remember whether you tried (Z)apping it yet.)
 - temporary light for cameral flash tourist spell doesn't work quite right.


	Changes between 1.0.96 and 1.0.97 which are still notable:
 - Some improvements from vanilla 3.1: treasure and object detection combined, characters start wielding their equipment, elemental rings add branding damage to melee blows.
 - My attempt at balancing range weapons: Slightly nerfed bow damage (x2 multiplier multiplies damage by 1.75, etc). It's called "multiplier level 2" now because it's not really x2. Rogues, druids, barbarians, hobbits, and living ghouls get strength bonus added to slings, and barbarians also get strength bonus to thrown weapons. (In 1.0.98, weapons which are meant for being thrown always get strength bonus).
 - Done away with infravision: it is now completely replaced by monster stealth and character alertness. Most monsters which used to be warm-blooded and invisible are now visible but stealthy, but there is a new monster spell which lets them temporarily turn invisibile. (The illusionist monster has spells which are more appropriate for an illusionist now: can temporarily turn invisible, but doesn't have the hold spell). Each class and race has an alertness rating like with other skills. Dwarves are the least alert race, but to make up for it, they have darkvision, meaning they don't need light to see monsters.
 - Monster breath damage had been slightly nerfed. But I partially undid the nerfing for 1.0.98 since I added the BREATH_SHIELD flag and the fact that breath damage is (slightly) reduced at long range. I say partially because only monsters which have the BR_STRONG flag do as much breath damage as breath does in V, but almost all big breathers have that flag.

------------------------------------------------------------------------------------------
	see
http://www.geocities.com/will_asher/MonsterDictionary/DaJAngbandmain.htm
	for complete changelists for each version and future plans.
------------------------------------------------------------------------------------------

	More in-depth explaination of a couple DaJAngband effects:

	Silver & Slime: Infections which have no effect in small amounts, but can have nasty effects if you let them accumulate to high levels. Cure wounds potions have no effect on them.  The two most effective things to fight these effects are elven waybread and the potion or spell of purity. Mushrooms of cure serious wounds also has a small anti-silver and slime effect. Mushrooms of health cure a little sliming, and mushrooms of clear mind cure a little silver poison.
	Sliming slowly turns a person's body into a mindless blob of slime, starting by lowering thier constitution. Danger levels for slime: at 20 begins to have occational minor effects, at 40 slime starts to have serious effects, and at 55 slime will be fatal if not cured very quickly. There are more monsters which cause sliming than there are that cause silver poison.
	Silver poison makes a person forget who he is and corrupts the mind so that he is no longer able to think about anything that matters. Its first effects are reduction of wisdom and randomly activated amnesia or hallucenation. Danger levels for silver: at 5-14 has rare minor effects, at 15-24 can have very serious effects, and at 25 silver poison will be fatal if not cured within a few turns of getting to this level.
	Charm prevents melee, shooting, and casting, but only reduces to-hit for throwing.
	Luck affects many different things in mostly minor ways. Beware the black cats. Luck affects chance-realm casters, especially the tourist, more than other classes.

	A note about magicbooks: Not all classes which are able to cast spells start out with a spellbook, but the class' spell realm is now noted on the birth screen when you're chosing you're character. Also, you probably won't be able to read every book in your magic realm. When in doubt, try to browse a book before buying it: If it shows you the list of spells in the book, then you can use that book, otherwise that book is worthless to you. With the chance realm, the tourist's books are in the general store. The one chance realm book which is sold in the magic store is not useable by tourists, but is of more interest to rogues.
	A note about the Nature and Necromancy realms: These two realms are largely borrowed from OAngband, but if you've played OAngband, it's important to remember that some of the spells that may have the same name as one in OAngband doesn't nessesarily have exactly the save effects. Browse a book and chose a spell to see that's spell's description.
	A further note about the Necromancy realm: The necromancy realm has some very powerful spells, but that realm also has more drawbacks than other realms. (Except maybe the chance realm, but the drawbacks are more obvious in the chance realm.) Several necromancy spells have a chance to leave an aura around you which aggravates demons.