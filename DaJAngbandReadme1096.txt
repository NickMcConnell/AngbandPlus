

Thanks to:
First to Andrew Sidwell and the previous mantainers for the great work on vanilla Angband, Eddie Grove for the patch (although I'm using a very old version of it), Pav for hosting the files and maintaining a great *band website, Bahman Rabii (and Pat Tracy) for bits taken from OAngband (a couple spells and descriptions for a lot of objects), Nick for a few spells from FAAngband and the Phantom of Eilenel, Shawn McHorse for that very nice list of Tolkien uniques, Andrew Doull for the link to Shawn McHorse's old post among other things, CunningGabe for new names of a couple jellies, those on oook who gave coding/variant making advice, and anyone else who gives comments or suggestions or just plays DaJAngband.

------------------------------------------------------------------------------------------
	DaJAngband version 1.0.96 (pre 1.1.0)

changes since 1.0.95:
	added FORCE_SLEEP flag to several monsters which needed it, also added POWERFUL flag to some monsters, and reduced the group size for some very tough monsters which come in groups
	POWERFUL flag now affects more things: reduces your saving throw against most melee and spell effects including cause wounds spells, also gives about a 6% chance for stat draining to bypass sustains, and POWERFUL monsters still do slight damage with an element you are immune to (because if you're immune to fire, the smoke from it can still do damage, if you're immune to cold, the ice can still do damage, etc..)
	A couple race/class restrictions have been removed (specifically, a dunadan can now be a druid,
and a half-orc can now be a priest or paladin.)
	weapons that give light now usually do slight extra damage to HURT_LITE monsters
	birth.txt help file updated to include new races and class (but it doesn't have the war mage yet)
	monster stealth implemented.  How well you notice steathy monsters is based primarily on distance between you and the monster and your perception/searching skills.  A couple other factors also make a small difference.  Stealth level ranges from 0 to 6 and take up the previously unused slot in the monster entries of monster.txt.  (stealth of 0 has no stealth effect, stealth of 6 is nearly invisible because of stealth).  Most stealthy monsters are: felines, higher level thief-types, higher-level dark elves, and the ranger and ranger chieftain (instead of invisibility). Most very small monsters (white rats, mushroom patches) have a stealth of 2 which means you might not notice it from a distance.  Most other monsters have a stealth level of 0 or 1.
	The assassin class now gets a couple more useful spells. (It previously lacked any detection spells in the town books.)
	More tweaks with artifacts, egos, and items including the addition of gloves of magic mastery and some other ideas borrowed from FA/O. Also descriptions for a lot of objects borrowed from OAngband.
	Necromancers' summon demonic aid spell now works. Added a couple spells to the wizardry realm.
	War Mage character class added. This is supposed to be a class where you can actually use magic as your primary offence and (in theory) not need to attack with weapons at all. At this stage, it is barely tested and not finished, suggestions for improvement are encouraged.
	Social class has an effect on starting gold even when using point-based stats.
	a few misc. minor bugs fixed.

changes since 1.0.93:
	fixed/updated some stuff about monster recall and monster symbols in the help file.
	a few minor monster tweaks as usual (one not so minor: The Static drake is no longer invisible -I didn't intend it to be invisible in the first place. Sorry to any one who meets a static drake in an earlier version..)
	added HURT_DARK flag to make the necromancer's darkness spells more interesting. (most light fairies, unicorns, tree monsters and zhelung lizards are hurt by dark).
	Four more classes are added: Necromancer, Assassin, Tourist, and Barbarian. Wizards now start out with the weapon I meant them to start out with. (didn't catch that until now because I rarely be that type of class).
	implemented the telekinesis spell (for tourists and thieves). Also, the 'Slip into the Shadows' spell works now. (if you cast it in an earlier version, the game will crash -at least it did on my computer).  ..I really should've noticed that and fixed it before releasing 1.0.93.
	staffs and rods of perception are now called staffs/rods of identify.
	metal shod boots can be dwarven, added a deep ego launcher "of Steady Aim" which gives confusion resistance, and other ego & artifact tweaks.
	added an invisible luck stat which has various (mostly minor) effects. Black cats can actually lower your luck now.
	berserk rage, charm, frenzy, and fear now raise your spellcasting fail rate (by 5% to 10%, not cumulative)
	fixed a bug which sometimes gave you extra slime or silver poison or makes you confuse monsters when you load a saved game.  (This bug might still happen in savefiles from 1.0.93. other than this, savefiles are not broken)
	added partial poison resistance for kobolds and hobglibs. They are now less resistant to poison than they used to be, and still have a use for permanent resist poison items.
	added three somewhat experimental new monsters: gnawing bug (got the idea from part of Psi's ent ranger story) and two silver monsters. All three new monsters are pretty deep and rare, so I probably won't be meeting them very often myself.
	implemented sentient items, good and bad.  good sentient items give slight bonuses to prayer and will give penalty to black magic. Bad sentient items do the opposite. Good sentient weapons also count as blessed for priests. There will probably be other minor effects of good and bad sentient items in the future.  There is also a corrupt flag on the One Ring now, but it doesn't do anything yet.

changes since 1.0.92:
	waybread isn't quite so expensive and cures 3 points of slime instead of 2.
	message for ELEc melee and another message fixed.
	expdrain is a little more drastic than before (may be made more so later).
	implemented STOPREGEN flag for the One Ring and a couple other cursed artifacts. (Finally! I still don't know why it didn't work the first two times I tried to implement this.)
	priest and thief classes are finished. a couple races have been tweaked.
	a few old mushrooms have been replaced by more interesting new ones.
	speed potions inflict hunger. some healing potions have more nutrition value to risk satiation.
	blades don't dig as well, certain classes and races prefer certain types of weapons.
	speed potions & staffs can make you hungry, some healing potions give more nutrition
	silver & slime are fixed (its effects were extremely rare before unless you got to a deadly level)

changes since 1.0.91:
	Call help nature spell now works, and wizard lock mage spell works correctly now. New timed effects which may not have worked before now work. 
	a few monster tweaks, typo fixes, and little stuff like that.
	added rare potion of auto-brail (lets you be able to read scrolls & cast spells while blind).
	Alchemy spell realm added with archer and alchemist classes.
	going up or down stairs only takes 1/10 of a turn.
	acid attacks removed from slime blobs

.txt file changes:
	class & races have been almost completely redone.
	weapons have been partially redone. Weapon weight is now a large factor of cost, and range weapons are more expensive. (A long dagger (same as main gauche in V) costs more than a short sword because it is much easier to get more attacks with it). The weapon store no longer carries long bows or light crossbows (for a similar reason that lanterns were removed from the general store).  There are mini crossbows added which have a x2 multiplier. Weapons like the mace and flail are no longer considered priestly weapons. Mostly the ones that are priesly weapons are the all wooden ones (walking staff, quarterstaff), and the whip and war hammer. The staff of striking is a new tough priestly weapon and the Mace of Disruption is blessed. Also, more than half the weapons now have description lines.
	some major monster.txt changes and a few artifact replacements

code changes:
	There is now a total of 7 spell realms (at least there will be in 1.1.0, right now there's 5 realms.)
	added: a monster attack type, a few monster attack effects, a few timed effects, slime and silver poison effects, a few monster range weapons including breathe fear (which replaces the scare spell in a lot of deeper monsters.
	The edged weapon penalty for priests (who aren't implemented yet, but druids also have the penalty) is considerably milder, and may get even more milder after some testing due to there being much less priestly weapons.
	weapons and armor with a total tohit/dam/ac bonus of less than 3 will pseudo as average (and be automaticaly identified like other average things). This change is mainly because of a couple weapons that have an innate to-hit bonus.
	..other minor tweaks

some more specific notes:
	suggested artifacts like The Axe of Gimli and the Dagger of Westeresse of Merry. (Axe of Gimli is just 'Lotharang' renamed and tweaked).
	Sting, Orcrist, and Glamdring have detect orc activation (not very useful, but appropriate. ..maybe they should have detect evil instead.) (not implemented yet for 1.0.91)
	there are a couple new monsters that have melee effects that are helpful instead of harmful (all helpful monsters either appear only when summoned by the appropriate spell or are extremely rare, some also have an experience penalty if you kill them).
	much less monsters are considered evil, for example: kobolds are not evil, mindless undead monsters are not evil (just being controlled by evil), not all thieves are evil, etc. (I don't remember whether I implemented this yet or not.)
	Probing now has a 1/3 chance to give you knowledge of monster spells (it never does in V).  The explosion spell now has a 10% chance of destroying walls.
	Silver & Slime: Infections which have no effect in small amounts, but can have nasty effects if you let them accumulate to high levels. Cure wounds potions have no effect on them.  The two most common things to fight these effects are elven weybread, and the potion of purity. The mushroom of cure serious wounds also has a small anti-silver and slime effect. Danger levels for silver: at 5-14 has rare minor effects, at 15-24 can have very serious effects, and at 25 silver poison will be fatal if not cured within a couple turns of getting to this level.  Danger levels for slime: at 20 begins to have occational minor effects, at 40 slime starts to have serious effects, and at 55 slime will be fatal if not cured very quickly. There are more monsters which cause sliming than there are that cause silver poison.
	The Nature and Necromancy spell realms are largely borrowed from OAngband, but IMPORTANT: most of the borrowed spells don't have the exact same effect as they do in OAngband. check spell descriptions in the spellbook in game or in spell.txt. For one thing, there is no "sphere" effect like Oangband has.

new description of classes & races:
		primary classes:
	warrior: The good old warrior with a few tweaks in skills. (Slightly better skills, one point less strength, no CHA penatly).
	wizard: Mostly same as V mage but doesn't get all the attack spells and isn't quite so weak phisically. In exchange for having a higher hit die and being a slightly better fighter, he does not get the elemental brand spell or the BEAM flag (he doesn't get the beam from attack spells any more often than other classes do.) The War Mage (which isn't implemented yet) gets it instead.
	rogue: Has less stealth and is a slightly better fighter than V rogue. (There will be a thief class later which will have the high stealth of the V rogue). Has spells in the Chance/Escape spell realm which was also made for the Tourist (which isn't implemented yet).
	ranger: More like O Ranger than V Ranger because of Nature realm. He does not get extra shots, but does get elemental brand ammo. He is not as good with bows as V Ranger, but is still (slightly) better than the warrior.
	archer: Gets extra shots with bows like rangers do in V (DJA rangers don't get extra shots BTW). Is about as good or slightly worse at magic as the V rogue. Their enchant weapon spells are limited to enchanting only bows or arrows, but they will eventually learn to not only acid coat their arrows, but also apply poison brand or elemental brand to their ammo.
	paladin: Almost just like the Paladin Marshal from DaJAngband 1.0.03 which is not much different from the V Paladin. The prayers in DaJAngband are planned with the paladin in mind rather than the priest, so that will probably make it easier for him.  He also get two new paladin spells borrowed from OAngband: Sanctify for Battle and Holy Lance. In most ways, he is easier than V paladin, but he gets less hit points.
	necromancer: Specializes in the Black Magic realm which tends to have more drawbacks than other realms but has just as powerful combat spells as the wizardry realm.  Has very slow STR (like the V mage). Has similar spells to the OAngband Necromancer, but no shapechanging spells (I might add them later if I can figure out how). (see description of black magic realm)
		secondary classes:
	alchemist: The alchemist is more familiar with the elements of acid and poison because of dealing with chemicals and potions. Their first spell is stinking cloud which tends to make the first couple dungeon levels a bit easier, but they only get two attack spells (stinking cloud and acid arrow) before dungeon spellbooks and they are pretty bad fighters. (acid arrow is only slightly tougher than magic missile, and not nearly as mana effeciant). Eventually they will learn how to acid coat their ammo and poison brand their weapon. (acid coating is not as strong as acid brand- it has x2 instead of x3).  In the Core of Alchemy dungeon spellbook, there is the recipe for the Elixer of Life. Made perfectly, it is even more powerful than a potion of life, but you have a very small chance of making it perfectly and can have a veriety of inbetween effects, which are mostly (but not all) good but rarely come close to perfection.
	priest: harder to play than V priest for 3 reasons: 1) slightly worse strength and fighting skills, 2) prayers in prayerbooks are arranged with the paladin in mind rather than the priest, 3) There are less 'priestly' weapons that you don't get a penalty for. (but when you do get a penalty, it is not as severe as in V.)  The old clairvoyance prayer has been replaced by a prayer which gives temporary telepathy, priests also now get a prayer of resistance (like the mage spell).
	healer: As the name implies, they specialize in healing prayers and starts out with the "Purifications & Healing" dungeon book. He has low strength, and his fighting is only barely better than the mage.  He can learn some prayers other than healing ones, but he's pretty bad at teleporation and has almost no combat prayers and is very bad at the couple he does get.  Two unique things about him is that (1)his gods let him choose which prayer to learn next, and (2)he uses his intelligence stat for prayers rather than using wisdom, which makes him more fitting for the elf and magic gnome races which are more likely to be healers than dwarves are.
	druid: Being powerful in nature magic, a halfway-decent fighter, and having good skills, he needs some drawbacks, so he is both encumbered by gloves and penalized for edged weapons, and he advances at the same somewhat slow rate of the mage. Another unique thing about him is that he uses wisdom to cast spells in the nature realm.
	white knight: <not implemented yet>
	black knight: <not implemented yet>
	assassin: A black magic version of the rogue class.  Instead of starting with high stealth, he starts with decent stealth and gets +1 stealth every 20 character levels.  He also gets a damage bonus against sleeping monsters. He prefers the crossbow over other range weapons, but is good with any of them.
	thief: Pretty bad at fighting, the thief is a master of stealth and speed. He gets intrinsic extra speed and monster aggravation has less effect for them (weapons which normally give aggravation give -5 stealth instead).  He is not great with magic, but better at it than the rogue. He uses the alchemy spell realm, but has one book of spells unique to the thief.
	tourist: A unique class.  The idea is from Nethack, but don't think of the Nethack tourist because this isn't much like it.  His skills are all pretty bad except for searching.  He specializes in two types of spells: detection/perception type spells, and chance/random outcome type spells (He has one spellbook called 'The Lottery').  He is one of only a couple classes in DaJAngband who have weak pseudo-ID, but he gets the identify spell earlier even than the mage.
	war mage: <not implemented yet>
	barbarian: Similar to the warrior except that it is much harder for him to get multiple attacks. Instead, he gets a significant bonus when wielding heavy weapons. He also gets a few nature spells (including an 'extra attacks' spell in the last spellbook).  His skills are generally worse than the warrior, but he has higher strength and hit die than any other class.
	fighter wizard: <not implemented yet and might not bother. If I do, it'll have a better name>
	sage: <not implemented yet>
	witch/warlock: <not implemented yet>
	holy rogue: <not implemented yet>
		novelty classes:
	mystic: <not implemented yet>
	loser: <not implemented yet>
	stone slinger:  <not implemented yet> cross between archer and new ranger except gets extra shots with a sling and penatly with a bow or crossbow.
	chaos warrior: <not implemented yet>
	meditation ninja: <not implemented yet>

spell realms:
	#1 Wizardry: mostly the same as mage spells in V with the two rogue spells removed, and wizard lock added. (will tweak it more later)
	#2 Prayers: similar to prayers in V, but changed a lot to focus on the paladin rather than the priest.  Has "sanctify for battle" and "holy lance" borrowed from OAngband.  Also has *Protection From Evil* which doesn't last as long but has decent chance of protecting from monsters of higher level than you.
	#3 Nature: mostly borrowed from OAngband, but no shapechanges. (has one less spellbook than other realms)
	#4 Chance/Escape: kindof a hodgepodge including several spells with random effects (like wonder in V except different types). Included is a Tourism & Travel book for the Tourist, an escape book for the escape artist and rogue and a chaos book for the chaos warrior. There is also a Lottery book with random effect spells including an aquirement spell with nasty backfire effects even if it does succeed.
	#5 Alchemy: Another hodgepodge realm. Of the three main classes that use it (alchemist, archer, and thief), each of them get roughly 5 1/2 spellbooks worth of spells, and all of them use the same first three spellbooks. The light area alchemy spell doesn't nessesarily light up the whole room. The alchemy realm has more varied spells relating to poison, acid and weapon/ammo branding than any other realm.
	#6 Black Magic: Largely borrowed from OAngband necromancy spells, except again no shapechanges.  There is a lot of backfiring effects with black magic spells because as they gather their magic power they damage their own soul. Black magic users get very few healing spells, most of which have a cost other than mana, but some get attack spells that are just as strong as the mage's attack spells. In the last spellbook there is a 'become lich' spell which is a timed effect spell with several effects including immunity to cold, but also has drawback effects (like it slows regeneration and aggravates animals and creatures of light). There is also a 'wall of hellfire' spell which raises armor class and gives immunity to fire for a short time. (Casting both of these powerful spells at the same time is a bad idea..) Several black magic spells have a chance to give you a timed effect which aggravates demons because that is the nature of black magic.
	(last realm not implemented yet: )
	#7 Mind Powers: Spells having to do with mind power, divination, & mind control.  Has some overlap with Prayers and Nature spells.

------------------------------------------------------------------------------------------
	DaJAngband version 1.0.04

	added the poison bolt monster spell (the spell flag was already there, but it had no effect before)
	made the POWERFUL monster flag do more: if a monster breath does less than 50 damage and the monster has the POWERFUL flag then it will increase the breath damage (50% chance of at least doubling it).  Also spells cast by monsters with the POWERFUL flag are harder to resist now.
	Heavy weapon penalty that was already in the game does not apply to the barbarian class. (haven't figured out yet how to actually give the barbarian bonuses for heavy weapons).
	Restricted some class/race combos.  Did not restrict any of the original combos, only ones which included a new race and/or class.
	..other minor tweaks

------------------------------------------------------------------------------------------
	DaJAngband version 1.0.03
.txt file changes:
	A bunch of mostly-very-minor monster tweaks.
	Monster descriptions are finally finished- there is now at least some description for every monster in the game.
	There is a small (though significant in the very beginning) exp penatly for killing most of the town monsters who don't actually do anything to you.
	Added Maia race (mostly the same as OAngband's) for the players of very low skill level (like me).  Their (quite useful) special racial ability is resist disenchantment.
	There is now alternate character classes for most of the original classes: (excluding priest and mage) Barbarian-warrior class with even higher strength and hit die than warrior, slightly worse skills, and very hard to get more than one attack. Holy Rogue- Rogue with prayers and penalty for sharp weapons, can get multiple attacks as easy as the warrior. Stealth Ranger was already there (described below). Paladin Marshal- My view of a paladin (largely influenced by The Deed of Paksenarrion book), not a whole lot different from original.  The original versions of the classes are still available of course.  Note: the new races and classes have not been very well tested, but I tried to keep them roughly balanced.  I think I've only played with the Barbarian class once, and it probably needs more testing than the others just to see if the concept works.

code tweaks:
	The ability to sell to shops is now a birth option (defaults to off). The same option also affects the amount of gold found.  If you can sell to shops, you find the normall vanilla amount of gold, if you chose not to be able to sell to shops, then gold drops are significantly increased (as in Eddie's patch -actually the old and only released version of Eddie's patch (so far) has a multiplier of 4 and I changed it to 3 cause I seemed like I was finding quite a lot of gold).
	added spear of light and starlight as a priest-type spell (also in spell.txt), I always thought spear of light fit better as a priest-type spell. Currently only the Paladin Marshall can use them though.
	added a couple new types of monster nests which I won't give away.
	fixed town messiness caused by me trying to add a new shop and failing.

------------------------------------------------------------------------------------------
	DaJAngband version 1.0.02 (includes Eddie Grove's patch)

changes since original release:

.txt file tweaks:
	knights now can get the call light and detect traps prayers but not until clvl 18 and 22 (respectively), detect evil now costs 4 mana for them instead of 5.  I did this to make the 1st spellbook a little more worth carrying around for the knight. (even though it's already something of an uber-class, hopefully not too much so).
	singing happy drunks and squint-eyed rogues no longer drop anything (unless the rogue stole something from you, of course).  battle-scarred veterans and village witches now drop only items.  There is a new and very rare town monster that drops only gold, but unlike the drunk, the new town monster can attack for damage. (Actually, I think I made the drunk do 1d1 damage, but the new one does more than that).
	couple other very minor monster tweaks.

code tweaks:
	One new weapon appears in the weapon store, and the three new blunt weapons now appear in the temple. Randarts can now be made from new armor and weapons.  (>>As of v1.0.03, I'm not sure if they appear in stores because I royally messed up the code trying to do something I didn't know how to do and had to revert to an old copy of all the src files and redo all my changes since then, and for some reason I couldn't get the new objects to appear in stores the second time -I'll keep trying to figure out why.  Also, I took back out two of the new weapon types.)
	Torches that you start with never have less then 2000 turns of light (I didn't like starting with torches with only 1500 turns of light).

major changes that come with Eddie Grove's patch:
(These are only the more major changes copied from his announcement, see the patch-changes.txt file for a list of everything his patch does)
	* Change !selfKnowledge to *IDENTIFY all items wielded
	* Point-based character generation equivalent to best available from autoroller (and always use point-based)
	* New command to restock a store for the cost of all items for sale
	* Show charges on aware wands and staves without identify
	* Give free identify on non-jewelry that pseudos as average
	* Show raw numbers rather than ratings on 'C' page
	* Pseudo of artifact identifies it
	* Stolen gold is carried by thief
	** Disallow selling items to stores	(probably the biggest change -but don't worry, there is no lack of gold because he also increased gold drops in the dungeon)
	* Allow squelching of unaware flavors
	* Notice obvious effects when wielding unidentified objects
	* New pseudo level "Splendid" meaning obvious bonuses when wielded  (replaces special pseudo and partially replaces excellent)
	* Allow pseudo on jewelry
------------------------------------------------------------------------------------------

	Notes about changes you might want to know about beforehand:

object.txt:
	added three objects: 2 armors, and 1 weapon, nothing major. And the druid class starts out with the new weapon and one of the new armors.  Combined resistance rings into 3 instead of 6.  sustain brawn (STR&CON), sustain intellect (INT&WIS), and sustain sneakiness (DEX&CHA).
	Made late see invisible changes: temporary sources of see invisible are more common, see invisible rings are now native to Level 43, and other permanent forms of see invisible are rarer.

ego_item.txt:
	Again, changes for my late see invisible idea:   Holy Avengers give hold life instead of see invisible, and defenders give infravision in place of it. I let *slay undead* keep see invisible because it is less useful in general, and morgul weapons and "of the nazgul" bows still have it also because they have a downside.  "Of Seeing" appears later, "of Night and Day" appears earlier for resist-blindness but does not have see invisible.  There are also two new ego types that give infravision.

artifact.txt:
	There are changes for the late see invisible idea as well as a couple new artifacts, and a couple old ones removed.  (I haven't found any of the new artifacts in my testing as of yet.)

p_class.txt:
Original classes are unchanged.  All new classes have heavy pseudo-ID, I really don't like not having heavy pseudoID.

		Descriptions of new classes:

	An Alchemist is like a mage who learns spells with a lot of trial and error.  Because of this he cannot chose which spells he learns, has higher fail rates, and generally learns spells slower than the mage.  To make up for this, his specialty spells include some powerful early spells including stinking cloud (at level 1!), and his spellcasting is not hindered by gloves. (An alchemist needs his gloves!)  His skills tend to be worse than the mage, but he has noticably more strength than the mage and is not as bad a fighter.

	The Archer is a hard class to play because archery is his only strength.  He's a decent melee fighter but not as good as the paladin or ranger.  Starting at level 32, he learns a few spells (8 in total) which include shield and, of course, elemental brand for his ammo. At very high levels he does have two powerful attack spells.  One other strength is that he advances in level faster than any except the warrior.

	A Knight is a warrior with a few prayers and high charisma.  He learns prayers very slowly, but has a few specialty prayers which include turn undead, remove curse, and elemental brand for his weapon which he learns earlier than most (if not all) other classes who pray. He starts with excellent armor and weapon, and can handle more armor weight without hindering prayer (or spells) than any other class. He tends to have lower skills than a warrior, so I think he'll be a rather hard class to play. His only healing prayer is cure mortal wounds which is a play on the "I got better" when people recovered from mortal wounds in Monty Python's holy grail.  Of course, a mortal wound, by definition, is one you die from, so I think "cure mortal wounds" sounds kindof silly.

	Another hard class to play, the Healer specializes in healing prayers and starts out with the deep dungeon spellbook which specializes in healing. He has low strength, and his fighting is only barely better than the mage.  He can learn prayers other than healing ones on later levels, but he's very bad at combat spells and teleporting spells.  Two unique things about him is that (1)his gods let him choose which prayer to learn next, and (2)he uses his intelligence stat for prayers rather than using wisdom, which makes him more fitting for the elf and (old) gnome races which are more likely to be healers than dwarves are.

	The Druid, as the monster description says, is a priest of nature. Though you can call him a priest, it would seem silly to pray to nature, so he casts spells instead and is almost as good at it as the mage.  He is the only one besides the mage who can cast spells at 0% fail. Being powerful in magic, a halfway-decent fighter, and having good skills, he needs some drawbacks, so he is both encumbered by gloves and penalized for edged weapons, and he advances at the same somewhat slow rate of the mage.  He starts with unique new armor (good) and a unique new weapon (mediocure), and a mushroom (because he knows his mushrooms).  Another unique thing about him is that he uses wisdom to cast spells.

There is also an p_classalt file which is going to have alternate versions of the original classes, but currently only has alternate versions of the ranger and paladin (and I'm not sure I finished doing the alternate paladin).
	The Stealth Ranger is my idea of what a ranger should be.  He no longer has extra shots, because a ranger is not the same as an archer.  His stats are almost the same as the old ranger.  He has a little less hps, and higher stealth (of course).  The notable differance in skills is that the Stealth Ranger has noticably better disarming and noticably worse magic devices.  He is not much better or worse at spellcasting than the original ranger, just different.  He is noticably worse with combat spells as the original ranger, but some other spells he is better at.  The rapier is his starting weapon.

p_race.txt:
Only changes to the original races are that I made infravision less common (except that the dunadan now has 1 infra), high elves no longer have SI (I gave them sustain INT instead).  I didn't want one of the easiest classes to also have SI which is supposed to be very rare in my semi-variant. Also, I renamed the old gnome to Gnome 1 (because I added an alternate gnome).  
		Here are descriptions of the new races:
	The new Gnome (Gnome 2: Wisdom fairy) is the way I think I gnome should be.  The word "Gnome" comes from a word meaning "wisdom" and yet the old gnome has +0 wisdom.  The new gnome has slightly lower strength, intelligence, and constitution than the old gnome, slightly higher charisma, and extremely high wisdom. He has the low hit die of a hobbit, a very high saving throw which is second only to the high-elf.  His skills are very good, except he is not as good at magic devices than the old gnome, and his fighting is not so good (but melee is slightly better than the old gnome).  One advantage is that the gnome starts with chaos resistance.  He is perfect for the druid class, though he has some drawbacks which include advancing only slightly faster than the dunadan.

	The Hobglib is a race I made up.  They have strange personalities and like to experiment with magic.  They are extremely neutral when it comes to good or bad, they will refuse to take sides in almost any conflict, but apparently Morgoth has made enemies of even them (and allies of some of them..).  Their personality, stats, and poison resistance make them a perfect fit for the alchemist class.  Their stats are not based off of any other race, so I'll just tell them to you: Str +1, Int +2, Wis -2, Dex +1, Con +2, Cha -3.  Their fighting ability is nothing special, and their skills tend to be close to average.

	Dark Elves are almost all evil, but occationally there is a good one who is an outcast everywhere and tries to overthrow the evil which the rest of his race indulges in.  The major stat differences between he and surface elves are his high stealth, low charisma, and very good infravision.  His melee is equally as good as his range weapon skill, which is not as good as his surface cousins, but still good.  He starts out with sustain dexterity and darkness resistance.  His skills are generally quite good, his magic devices are noticably better than surface elves and his searching skill is also especially good.

And a couple novelty races:

	The Grave Ghoul is often mistaken for a ghoul, but it is a living creature which was once human, but through 
generations of hating light, cannibalism, eating raw corpses, and hanging out in graveyards, it has formed a kinship with the undead without being undead itself. It starts with terrible stats and average skills, but it can see invisible and resists nether. 

	A Power Sprite is tiny but extremely strong.  Its skills are mediocure and its hit die is the lower than any other race.  As well as being able to fly (which translates to feather falling) and being resistant to light like any other sprite, it is also fearless.  A power sprite mage would probably be the hardest class/race combo ever, but it would at least be able to carry more stuff than most mages..


monster.txt:

	Here are the new letter symbols followed by some notes about changes I made to monsters & stuff
  a - ant or beetle
  b - bat
  c - centipede or similar bug
  d - dragon (see notes about colors below)
  e - eye/beholder
  f - feline
  g - golem
  h - humanoid
  i - imp-related minor demon or dark fairy
  j - jellies & slimes
  k - kobold
  l - lizards & reptiles
  m - mold or vossar
  n - naga
  o - orc
  p - human (person)   (note: there are now some dungeon humans which are not represented by this symbol -just because there got to be too many monsters using the same symbol)
  q - quadruped
  r - rodent
  s - skeleton
  t - townspeople	(and some dungeon people, notably, but not limited to, witches)
  u - minor demon	(other than ones I consider to be imp-related)
  v - vortex
  w - worm or worm mass
  x - gargoyles
  y - fairies (mainly gnomes and sprites)
  z - zombies, mummies, and wights
  A - apes
  B - bird
  C - canine
  D - ancient dragon, drakes or great wyrms (see notes about colors below)
  E - Ents and other tree-monsters
  F - dragonflies or other non-breeding flying insect
  G - ghost (or ghost-like, there are one or two which are not undead)
  H - hybrid
  I - flying insect	(mostly breeders, but not all breeders)
  J - snake
  K - knights and select mystic warriors
  L - lich
  M - hydra (with one late exception)
  N - null		(see the in-game description)
  O - ogre
  P - giants and titans
  Q - quylthulg 	(no longer invisible and no longer has ANIMAL flag)
  R - frog or behemoth
  S - spider or related bug
  T - troll
  U - devil
  V - vampire
  W - wraith
  X - xorn or other minor elemental
  Y - unicorn or centaur
  Z - zephyr hound
  $ - creeping coins
  , - mushroom patch
  & - major demon
  % - elemental
  . - lurkers, trappers, and dust bunnies

	notes on changed monster colors:
   Nagas: added naga hatchling and made black naga a little tougher, and the red naga is now a fire naga instead of strength draining. (red still means strength draining in a few other monsters)
   Dragons: orange=sound, blueish-white=frost.  Green, red, bronze, and (bright) blue are the same as before.  The main reason for these changes is that I have added a new yellow dragon. There are also new grey and silver dragons, and the pseudo-dragon is removed. (Dragonflies/bats correspond to the new colors)  The weaker drakes now use the capital "D", and they have also changed colors. (I wish I had more symbols availble, then I'd make the drakes/wyrms a separate symbol)
   Humans: priests tend to be orange and rangers dark green (for the forest), other class-races are the same color as vanilla. Archers tend to be silver, alchemists tend to be purple, and weird personality type humans (including the illusionist) tend to be yellow.
   Werebeasts are always yellow (and they're always able to summon kin and lower experience).  I thought werebeasts should all have something in common.  Water hounds are renamed to acid hounds and are orange (I always thought it didn't make sense for water hounds to breathe acid or be grey in color).  There are separate acid vortex and water vortex, and the water vortex casts water bolts.
   There is now only two early-level centipedes. I got annoyed with those bunch of colored centipedes that were almost exactly the same.  The snakes are no longer known by their colors either, but by real snake types.
   There are a couple other things which I changed the colors of.  The main reason for the color changes is that I don't like having more than one monster that has the same color and symbol as another monster.  So I minimized that, and usually when there is more than one monster with the same color and symbol their native depth is about 20 or more levels apart so you can usually safely assume which monster it is. I made an exception to this rule only for uniques and monsters that are almost exactly alike anyway.


shop_own.txt
	Added a couple shopkeepers to include the added races.

Other .txt files are unchanged except for nessesary changes in limits.txt, p_hist.txt, and cost_adj.txt.

  At the bottom of this text file is more spoilery monster changes, so you might want to refrain from reading them.

------------------------------------------------------------------------------------------
   Future plans and things I hope I can figure out how to do:


change this: (there shouldn't be such a thing as 'perfect save' without having the relevant resistance)
[Quote:
Originally Posted by Djabanete  
It means you'll always be safe from certain attacks. Not exactly sure which attacks allow a saving throw (I know that cursing does, and maybe blindness/confusion/fear spells?), but I know you don't need FA if you have perfect save.  

You get a saving throw against cursing spells, blindness, confusion, fear, slowing and paralysis spells, and against melee paralysis attacks. (I'm not sure if saving throw protects against blindness/confusion/fear melee attacks - I'm pretty certain it doesn't protect against blindness/confusion from breaths). So a perfect saving throw does replace Free Action, though it requires quite a bit of courage to rely on that!]
------------------------------------------------------------------------------------------

		Spoiler monster changes:	(might not want to read further)







	Uniques: Gollum is redone (and known as Gollum), and he now appears a bit later. The level 3 unique is now Bill Ferny, and Farmer Maggot has been replaced by Harry Goatleaf. There are about the same amount of uniques as before. I added some and got rid of several of the old ones which, in my opinion, just didn't fit in the game).  Lotho Sackville-Baggins, Old Man Willow, and the Watcher in the Water are other commonly known added uniques from Tolkien.  Wormtongue (like most monsters and uniques) is tweaked a little, but is still on the same level. Also, I added two uniques from Wagner's Ring cycle which Tolkien got some of his ideas from.
	The Boar of Everholt is a semi-early "q" unique who does a lot of damage, it is quite tough for its native level.  Wulf, Renegade of Rohan, is another lesser-known added unique who is from Tolkien's works.  The silent watchers have turned into uniques (there are two of them).  The old silent watchers have been replaced by animated staffs of summoning.
	The orc uniques have been rearranged to fit which ones I think should be tougher than others. Goblins replace snagas and the Great Goblin from the Hobbit is the first orc unique (replacing Lagduf).  There are a few more orc uniques than there used to be (They are still all from LOTR). To make up for there being more of them, more than half of them are 'minor uniques' which are only slightly tougher than the regular orc captain.  Not all of them have good drops, so that it is more like fighting any other group of orcs.  One of them doesn't even come with escorts.
	There are a few uniques and monsters taken from The Deed of Pasksenarrion by Elizabeth Moon. The first one is Siniava the 'Honeycat' who is a cruel outlaw leader. Added monsters from that book also include the gods Achrya and Liart, along with their priests and a 'drider of Achrya', which appear late in the game.
	The more recent unique changes I didn't bother to describe here, but most of them are from people's suggestions and Shawn McHorse's Tolkien unique list.  A lot of original uniques had been removed, but there were at least eight which I took out at first and re-added recently (Ulfang & sons, Uldor, Mim's sons, Lorgan, Huan).

	New monster sets:
   The zhelung (under l for lizard) are a family of lizards, some of them with no noticable magic, others are very magical.  These are monsters I made up which include the rasti, well lizard, zhang, zhelung, and ancient zhelung (see their description for more description).  The zhelung usually live in underground bodies of water.  They are creatures of light, known for making light in the dark caves, but they're also wild and dangerous.  I also consider salamanders to be related to them.  BTW, my salamanders are tougher than they were and appear later.
   Another set of new monsters is the gnomes and sprites ("y" symbol). Sprites are very fast but are generally nothing to worry about (except for the power sprite). The gnome thief steals objects and the later gnomes have some powerful magic. Also sharing their symbol is the annoying leprechaun, which is much like the leprechaun in Nethack except that it's pretty rare. (all yeeks have been removed)
   Dark fairies share the "i" for imp symbol. Among them are the impsprite (basically an evil sprite), the brownie, the pooka, and a couple others. All except the impsprite have some dangerous spells. BTW, the original minor demons which I consider imp-related are: the homunculus, imp (obviously), quasit, and tengu.  (Icky things have been removed. I always hated icky things.)
   Gargoyles are living and flying grotesque statues which come in several different sizes.  Like trolls, they are of the element of stone, so I made them fall under the "troll" slay.
   Apes replaced angels with the "A" symbol.  I really don't think angels fit in this type of game (There are still 'Fallen' Archons which are now included among the major demons).  Hmmm, now there's a "summon an ape" monster spell..
   Nulls, as the description says, are tentacled swamp monsters with acid under their skin.  All of them are immune to acid, but only the tougher ones have acid attacks.  They normally live in water, but they can survive ok on land, they just don't move very fast unless they're in the water.  (and of course there is no water in vanilla)
   There are three types of vyrms ("w") which are magical evil worms. The mine vyrm is larger and based on the thing from the Discworld book "Thud."  The other two types of vyrms appear as smaller flying worms and they can be very nasty, though they don't have a lot of HP.
   Tree-monsters tend to have a ton of hps and do considerable damage.  All except one type can move, but they are extremely slow and as a consequence, they don't get a chance to attack often.  The exception is the poison vines, which have a good bit less hps and are faster than other tree monsters (but still slow compared to normal monster types).
   Centaurs and unicorns ("Y", for lack of another unused symbol) shouldn't need much explaination. The earliest unicorn, the wild unicorn, is non-magical, the others are magical.  All the unicorns do a lot of damage with their horn.
   The "K" knight symbol includes the black knight, death knight, knight templar, berserker, ranger chieftain, and the mystics.  It also includes some other select added warriors.  There was getting to be too many humans to put them all under one symbol.
   Devils overlap the major demons somewhat, and are mainly taken from Nethack.

	Other monster notes:
   Town monsters: The mean-looking mercenary and battlescarred veteran are a little tougher than before. There are a couple new town monsters, including the village witch (about as tough as the mean-looking mercenary), the fat weathy pompus jerk (about as tough as the squint-eyed rogue, except he doesn't steal), the locust (a rare and slow breeder), the small rat (pretty much the same as the scrawny cat and scruffy little dog), and two other rare and strange ones which I won't go into detail about..
   There are several monsters which I took from Nethack.  This is because Nethack's monsters generally have more flavour.  A few of the added Nethack monsters are: the acid blob (replacing the green ooze), apes, the chickatrice and cockatrice, rock mole, rabid rat, etc.  There is also a rabid wolf.
   I also added some more interesting cats (Beware the Black Cat).  There are a couple new eyes (mostly rare), and the Gauth has been replaced.  The harpy has more character and is more like I think a harpy should be.  Clear hounds have been removed.  Added a few new golems including a couple weak low-level ones, also the colbran is tougher and appears later.  The hooded shade, small dark reaper, and the furies are some new minor demons which can be troublesome. The erinyes is tougher than it used to be.  A couple new mimmics have also been added, and mimmics in general have been made weaker.
   There are some different minor elementals, and the straight elementals (normal fire, cold, water, and air) are tougher than before.  The xorn and xaren are now considered minor earth elementals.  The fire spirit, air spirit, etc, have been removed.
   A couple new nasty monsters to watch out for: rasti can be nasty when you first find them (just cause of speed and damage in groups). The slimes are kindof slow, but they multiply and most (except for the small slime) are acidic. To very slightly make up for this, green worm masses have been removed. The wolf spider is an early paralyzing monster (don't worry- it doesn't come in groups like other spiders), you probably want to avoid melee with it.  The greater poltergeist is rather hard to watch out for, being invisible, but I thought I should warn you that it exists anyway (It replaces the moaning spirit, but the greater poltergeist is a little nastier). Merrets are very fast small rodents who have low hps, but do a lot of damage. The eirrinel is a bat-shaped fairy, you'll want to watch out for it because it's fast and it disenchants.  The pooka can also be annoying.

