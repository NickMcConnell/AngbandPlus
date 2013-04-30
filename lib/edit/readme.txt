	To a_info.txt: 
 - A few more high resists sprinkled around. Exactly one of these is poison 
(to Gurthang, the bane of the dragon Glaurung whose poisonous fumes nearly 
killed Turin but didn't.) The most common new resist is fear (which nothing 
had before), then confusion (thanks to the split of chaos and confusion - see 
below.)
 - The Phial of Galadriel is now level 5 rarity 5 instead of 1/1. Given that 
level 5 is just where one starts running into Bullroarer, Mughash and the 
lower orc-uniques, the chances are that nobody will ever notice apart from 
those people who expect to find the Phial on dungeon level 1 anyway.
 - Resist chaos split properly from resist confusion (i.e. not all standard 
chaos-resistant artifacts resist confusion any more: Thorin and Zarcuthra 
being the most notable of these.) 
 - Rings of Power *far* more munchkinish, if only to compete with big rings 
of speed. 
 - Gurthang and Mormegil should both look alike (thanks to the Turin Turambar 
connection that gave both names to the same sword - well, one name (Mormegil) 
to the wielder, but as that was precisely because of the sword, then to say 
the two names don't apply to the sword is just splitting hairs.) So Mormegil 
is now 3d6 not 4d6.
 - More cursed artifacts have useful powers, not just Calris any longer. 
 - Speed bonuses given to some artifacts - small, usually +1 to +4. 
 - Crown of Gondor is *much* nicer. But not telepathic...
 - Acid brand given to two artifacts: Angrist (which could cut through iron 
as if it were butter, even Morgoth's crown) and Aule, which also gains a 
tunnelling bonus as befits an artifact belonging to the god who created the 
dwarves and the craft of mining.
 - Seven new artifacts: the Broken Sword 'Narsil', the Mattock of Nain, the
Metal Cap of Celebrimbor, the Short Bows of Amrod and Amras, the Whip of 
Gothmog and the Palantir of Westernesse. 
 - Five artifacts ported from variants: the Hard Leather Armor of Himring,
Shield of Deflection of Gil-galad and Heavy Crossbow of Umbar (formerly Harad) 
come from OAngband, and the Heavy Spear of Melkor and Beaked Axe of Hurin 
from JAngband/64 via KAngband. 
 - Most artifact activation recharge times made drastically shorter, so that
they are more than just one-shot items. The exceptions being really powerful
activations such as genocide. Vilya, for instance, now shoots lightning balls
every 20 turns if you want to.
 - The commented-out flags of KILL_DEMON and KILL_UNDEAD have been added to 
two or three artifacts each. The game works well without them, but if anyone 
is thinking of coding in Kill Undead or Kill Demon flags, please do, and 
uncomment these flags.
 - The same is true of three new artifact activations. To wit:
     STARLIGHT for the Shield of Gil-galad. The light should be "hard" (i.e.
        of the type breathed by Light Hounds and Ethereal Dragons, hurting 
        all creatures and not just light-sensitive), and in all directions.
        Say, 100 points of damage? Not excessive for a late-game artifact
        that can't be aimed - Shields of Deflection are rare.
     BERSERKER for the Beaked Axe of Hurin. Obvious enough, this one.
     AWARENESS for the Metal Cap of Celebrimbor. Awareness is temporary 
        telepathy (it goes by this name in Psiband), lasting for 20+d20 
        turns, average 30: so (with a 60-turn recharge) on average, the 
        player can be telepathic half the time.

	To e_info.txt: 
 - New ego-items added, rarities of the existing ones tweaked (e.g. all 
single-resist shields now have the same rarity, as was the case with armor.)
  - Also, every kind of item except digging implements now has at least one 
"bad" ego-type - this is in hope that somebody will bring in changes to 
pseudo-ID so that one can tell the power level but not the good/bad status 
of the item ("magic" rather than good or cursed, "extraordinary" rather 
than excellent or worthless, "unique" rather than special or terrible.) 
 - Weapons of Gondolin, bows of Lothlorien, crossbows of the Haradrim, armor
of Vulnerability, boots of Stability, helmets of Serenity, crowns of Night &
Day, gloves of Thievery, gauntlets of Combat, shields of Preservation, and 
more...
 - Weapons (melee and missile) of Poison Brand have been added in.
 - The weapons of *Slay Undead* and *Slay Demon* have the commented-out flags
of KILL_UNDEAD and KILL_DEMON.
 - Weapons of Morgul have some powers over the undead, and can also poison 
the living. However, they are still *heavily cursed*...

	To g_info.txt: Tables updated to tell shopkeepers how to treat Kobold 
player characters.

	To h_info.txt: several changes. 
 - Half-elves cannot be Vanyar (none of the Vanyar ever returned to the 
human lands except once, and that was for the War of Wrath, and then they 
went back home again. Indeed, only one Vanya - Indis, the second wife of 
Finwe the Noldorin king - is recorded as having married outside the race: 
they seem to have kept themselves to themselves.) So half-elves can be 
descended only from Noldor (the only High-Elves to return to Middle-Earth),
Sindar or Wood-Elves (the latter two being those types who never made it 
there at all.) 
 - Similarly, the Vanyar/Noldor/Teleri classifications refer to High Elves 
alone, i.e. those who *did* reach the Blessed Realm), and normal Elves are 
either Sindar (a subset of the Teleri, who never finished the journey) or 
Wood-Elves (those who never started the Great Journey at all or didn't 
even reach Beleriand - technically "Umanyar" or "Avari", or any other of 
umpteen differen classifications, but I've decided to call them "Wood-
elves", it's simpler.) 
 - Some Hobbit titles changed around, to allow in sons of a Shirriff or 
the Mayor.
 - Kobold character descriptions supplied by Prfnoff, from a post to RGRA.

	To k_info.txt: 
 - Rings of speed now level 75 instead of 80. 
 - Rings of [stat] also sustain that stat.
 - sustain rings are shallower so as to actually be *useful* for once.
 - PDSM restored to former glory. 
 - Other DSMs made a little more rational - Law and Chaos should be same 
depths and prices, likewise the five single hues of the Multi-Hued set. 
 - New template added (Palantir) for the INSTA_ART of the Palantir of Westernesse. 
Needs a graphic to be designed for it.
 - *ID* scrolls a little more common at deeper depths, and likewise potions 
of Healing, *Healing* and Life. 
 - Typo corrected in Mushrooms of Cure Serious Wounds, they should be 0.1 lb.
 - New digging tool - Mattock (ported from ZAngband) - a digger which can 
also make a useful weapon. It needs a graphic to be designed for it.
 - The late-game "good" weapons and armor are less rare now: Adamantite and 
Mithril Armor, Shadow Cloaks, Blades of Chaos, Maces of Disruption, Shields 
of Deflection... all were so rare that it was unusual to find any of them, 
apart from Mithril Chain Mail. Most are rarity 4, rather than the much rarer 
8 or 10.

 - Decided to make a few new ammo types: Silver Arrows and Bolts invented 
(intrinsic slay evil, but rarer than seeker stuff and not quite so big damage
against non-evil) and Mithril Shots imported from ZAngband. All of them 
ignore acid: note also that Silver Arrows and Bolts cannot have Poison 
Brand (this is in accordance with the old superstition that silver was proof 
against poison, a well-known - and false - belief of many kings in the 
ancient world.) Silver stuff is also configured so as not to get the 
(redundant) Slay Evil as an ego-type either, since they have it 
intrinsically.
 - Similarly, Maces of Disruption cannot get Slay Undead intrinsically.
 - Amulets of the Magi now resist blindness and confusion, but are deeper 
and twice as rare as they used to be. This change taken from OAngband.
 - Amulet of Sustenance: sustain all stats, hold life, slow digest. Rarer 
than Amulets of the Magi.
 - Amulet of Telepathy: provides exactly that.
 - Amulet of Resistance: provides basic four. Not intended for being the only
source at 2000', but as a swap item around 3000' so that a player can finally
get the chance to wear the combination of armor and shield, neither of which
themselves provide the basics. But having them available at 2000' would make
that choice too easy too early.

	To p_info.txt: 
 - Hobbits can now be rangers, but not mages. (Tolkien says: there is little
or nothing magical about Hobbits, but they are excellent with missiles - both
in terms of throwing stones at birds, and in having once sent a contingent of 
archers to the last battle of Arnor, although they achieved nothing there.)
 - Elves cannot be priests. Those elves that are not High are those that 
did not reach Valinor, and therefore turned their backs on the Valar, and so
are not religious enough to be priests. High-elves can still be priests, and 
of course Half-Elves can take after their human ancestors and follow human 
religions and superstitions.
 - Dunedain are too noble to be rogues: some may incline to evil, but none 
to petty crime like picking pockets. When a Dunadan goes bad, he does it in 
a big way, not as a petty criminal.
 - Dwarves can now be paladins. I can't imagine any dwarf religion that 
forbids the shedding of blood or the use of axes, so paladins should be 
allowed.
 - Kobold player race added: can be rogues, rangers, warriors or priests. 
Weak, but resists poison. 

	To r_info.txt: 52 new monsters, 4 old ones are gone (but replaced by 
others that can use the same graphic, and have similar enough powers to keep 
monster memory disruption to a minimum.) Others have been reorganised to keep 
a fairly even progression of non-uniques all the way to 4000'. Some typos and 
illogical stuff (such as cave ogres being worth fewer XP than black ogres) 
corrected. Some of the old monsters - e.g. Glaurung and Ancalagon - are 
tougher than they were. Non-uniques can have escorts. An explanation is 
attempted for why the player is fighting "good" creatures, to be found in the
description lines of the old "Angel" (formerly a "Monadic Deva") and the new 
"Paladin" monsters. Full details can be found in r_info.txt itself.

	To v_info.txt: All lesser vaults have at least two good items, guarded 
by monsters up to 11 levels out of depth. All greater vaults have at least 
four great items, guarded by monsters up to 40 levels OOD. Vaults should be 
guaranteed to have *something* special - and something dangerous, too.

	To z_info.txt: all numbers updated to allow for the new items and 
monsters.
