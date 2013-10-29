

Thanks to:
First to Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch (although I'm using a very old version of it), Pav for hosting the files and maintaining a great *band website, those on oook who gave coding/variant making advice, Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that very nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, CunningGabe for new names of a couple jellies, and anyone else who gives comments or suggestions or just plays DaJAngband.

------------------------------------------------------------------------------------------
Readme for DaJAngband 1.0.99

		Since 1.0.98:

	Fixed Bugs:
 - fixed: Darkvision STILL lets you see invisible. (thought it was fixed before, but now it's really fixed..)
 - fixed: weapons with the DANGER flag never hit yourself (but still have "sometimes hits yourself" in the weapon description).
 - fixed: randarts get sentient alignments way too often and can have both good and evil alignments at the same time.
 - fixed: thrown weapon randarts get flags which are only any good if you can wield them.
 - fixed: The spells of some monsters summoned by the call help spell have wrong behavior.
 - fixed: force stack command for staffs doesn't remove random extra ego stuff
 - fixed: some monsters' hit dice don't work (the ones with 200d3 kinda stuff). They end up with much less than they should.
 - fixed: multiple attack throwing weapons work now
 - fixed: burst of light spell now works as it should

	Information / Interface stuff:
 - now asks which ring to replace when wielding a ring.
 - potion of self knowledge now reveals exact level of corruption and luck.
 - hide_squelchable option defaults to ON
 - detect traps now detects traps on adjacent chests
 - chance of magic item success is displayed in item description

	Cool Features: 
 - NPP-style quiver added!
 - slays/brands have partial stacking: (+25% added to x3 multiplier)
 - added grab attack for owlbear and a few other monsters
 - ability to target monsters in walls
 - new room types: empty vaults (including some room designs in vault.txt which aren't used for real vaults) ..for dungeon veriety.
 - gauntlets of throwing added which give a multiplier to thrown weapons (like they were being shot from a bow). This ability can also be added to egos which get a random power.

	Effects of effects and what they affect:
 - magic item difficulty separated from native level (except for activatable artifacts). Also, the lesser DSMs are easier to activate now.
 - trying to use an empty staff/wand that you already know is empty doesn't take a turn. Same goes for rods which are charging. Also, the lesser DSMs are easier to use now.
 - nexus now has approprate effects on monsters
 - TUNNEL diggers/weapons do extra damage to HURT_ROCK monsters
 - polymorph should be much less dangerous now (usually doesn't heal). Polymorph spells/wands are also more likely to work now.
 - chance based on (clevel + luck + device skill) for wand damage to get a boost (not too big a boost)

	Misc tweaks / improvements:
 - MOVE_BODY monsters usually wake up the monsters they push past
 - quarterstaffs are double weapons (easier to get multiple attacks), also a couple other double weapons added. Priests and druids get a bonus with quarterstaffs.
 - gave stat potions a second allocation line so they'll keep appearing as you go deeper
 - objects in rubble can be found by searching without digging up the rubble. This is done so people like me don't feel like we have to dig up every pile of rubble.
 - static resistance added (for drain charges) (included in random resistances). IMPORTANT: This is not complete protection like disenchantment resistance is.
 - In an earthquake, a durable object may become buried in granite instead of breaking.
 - ..several other minor tweaks which I won't go into detail on.

	Known bugs in 1.0.99: 
 - magic staffs don't always reveal (+0 +0) when they pseudo as average.
 - "tried" inscription dissapears from magic staffs when you get a pseudo-ID. (supposed to do this for weapons, but for magic staffs, you want to remember whether you tried (Z)apping it yet.)

	Fixed Bugs between 1.0.97 and 1.0.98:
 - The three extra slots in the home no longer look like a bug and they're easier to get to now.
 - DaJAngband had inherited a bug from V 3.0.9, which made a scroll of rune of protection be re-useable if you read it from the ground. That is fixed.
 - fixed: crash when identifying the sling of snowballs.
 - fixed: orb of draining doesn't always destroy cursed objects.
 - fixed: random resist thing on some egos choses from the completely wrong set of flags.
 - fixed: detect invisible scroll or spell doesn't detect monsters which are temporarily invisibl.

	Other changes between 1.0.97 and 1.0.98 which are still notable:
 - shops carry deeper stuff when player gets deeper (I don't think it's as much of a change as in V)
 - Birth screen shows a lot more information as you're putting your character together.
 - Object description shows average weapon damage, true bow multiplier (since it's not exactly x2 and x3 anymore), blessed status, and odds of getting a critical hit/shot.
 - Identify and *identify* scrolls have a chance to not be used up when you read them.
 - Searching frequency and alertness are combined, and the (c)haracter screen now shows throwing skill, darkvision, charm resistance, and partial poison resistance.
 - Magic staffs can now get weapon bonueses and egos. Since this can cause problems with stacking, I also added a free command "&" which can remove unwanted egos and weapon bonuses from a staff to allow it to stack with others of its kind.
 - Throwing and shooting now use two separate skills. Throwing weapons are implemented which can't be used for melee. They also have a chance based on throwing skill (and luck of course) to get branding from elemental rings that you're wearing.
 - Ego rings "of Eregion" and "of Warfare" borrowed from another variant (I forget which now). I had tried to add them before, but now they work.
 - Monsters are not always asleep before they notice you. They walk around the dungeon, going about their business. The disturb_near option only triggers for monsters who are aware of you unless the monster has just entered your line of sight.
 - You will now always notice a non-invisible monster which is adjacent to you, and sleeping monsters get a penalty to stealth.
 - A couple rather crude AI improvements: Monsters which are not in line of sight are able to blink towards you. Monsters which prefer ranged combat have a little more chance to stay where they are and shoot or cast instead of coming closer to the player.
 - Monsters and the player both can now climb over rubble, but not easily. You can never do it if you're carrying enough to be slowed. Monsters which don't have BASH_DOOR and can't fly, can't climb over rubble.
 - Boulders thrown by giants occationally leave rubble.
 - slow, confuse, fear, and sleep are now more effective and their effectiveness is affected by charisma.
 - *destruction* no longer affects vaults.
 - perma_cursed items can be uncursed, but they have a heavy curse and re-curse themselves automatically after awhile.
 - confusion adds 25% to fail rate instead of automatically preventing you from casting spells. Likewise, confusion gives monsters a high fail rate instead of making them never cast spells. Monster stunning is also weakened. Now they have a fail rate for spells and are much more likely to miss when they're stunned. Before, monsters did nothing at all while they were stunned. Critcal hits have a chance to stun monsters now.


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