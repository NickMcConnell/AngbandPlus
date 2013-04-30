	To a_info.txt: 
 - A few more high resists sprinkled around. Exactly one of these is poison (to Gurthang, the bane of the dragon Glaurung whose poisonous fumes nearly killed Turin but didn't.) The most common new resist is fear (which nothing had before), then confusion (thanks to the split of chaos and confusion - see below.)
 - Resist chaos split properly from resist confusion (i.e. not all standard chaos-resistant artifacts resist confusion any more: Thorin and Zarcuthra being the most notable of these.) 
 - Rings of Power *far* more munchkinish, if only to compete with big rings of speed. 
 - Gurthang and Mormegil should both look alike (thanks to the Turin Turambar connection that gave both names to the same sword - well, one name (Mormegil) to the wielder, but as that was precisely because of the sword then to say the two names don't apply to the sword is just splitting hairs.) So Mormegil is now 3d6 not 4d6.
 - More cursed artifacts have useful powers, not just Calris any longer. 
 - Speed bonuses given to some artifacts - small, usually +1 to +4. 
 - Crown of Gondor is *much* nicer. 
 - Acid brand given to two artifacts: Angrist (which could cut through iron as if it were butter, even Morgoth's crown) and Aule, which also gains a tunnelling bonus as befits an artifact belonging to the god who created the dwarves and the craft of mining.
 - Three new artifacts: the Broken Sword 'Narsil', the Mattock of Nain and the Palantir of Westernesse. 
 - Five artifacts ported from variants: the Hard Leather Armor of Himring, Shield of Deflection of Gil-galad and Heavy Crossbow of Umbar (formerly Harad) come from OAngband, and the Heavy Spear of Melkor and Beaked Axe of Hurin from JAngband/64 via KAngband.
 - Some big-damage weapons with nothing else but damaging power (which may include slays) to show for them now have one or two extra attacks - exactly three weapons have this new, and one of them is Grond.
 - The commented-out flags of BRAND_POIS, KILL_DEMON and KILL_UNDEAD have been added to two or three artifacts each. The game works well without them, but if anyone is thinking of coding in a Poison Brand, or Kill Undead or Kill Demon flags, go ahead.

	To e_info.txt: 
 - New ego-items added, rarities of the existing ones tweaked (e.g. all single-resist shields now have the same rarity, as was the case with armor.) 
 - Also, every kind of item except digging implements now has at least one "bad" ego-type - this is in hope that somebody will bring in changes to pseudo-ID so that one can tell the power level but not the good/bad status of the item ("magic" rather than good or cursed, "extraordinary" rather than excellent or worthless, "unique" rather than special or terrible.) 
 - Weapons of Gondolin, bows of Lothlorien, crossbows of the Haradrim, armor of Vulnerability, boots of Stability, helmets of Serenity, crowns of Night & Day, gloves of Thievery, shields of Preservation, and more...
 - Weapons (melee and missile) of Poison Brand have been added in, but are currently commented out. This will change if Poison Brand is coded. 
 - The same is true of the flags of KILL_UNDEAD and KILL_DEMON for the weapons of *Slay Undead* and *Slay Demon*: they can be uncommented if the power is coded into the game.

	To g_info.txt: Tables updated to tell shopkeepers how to treat Kobold player characters.

	To h_info.txt: several changes. 
 - Half-elves cannot be Vanyar (none of the Vanyar ever returned to the human lands - except once, and that was for the War of Wrath, and then they went back home again. Indeed, only one Vanyar - Indis, the second wife of Finwe the Noldorin king - is recorded as having married outside the race: they seem to have kept themselves to themselves.) So half-elves can be descended only from Noldor (the only High-Elves to return from the Blessed Realm), Sindar or Wood-Elves (the latter two being those types who never made it there at all.) 
 - Similarly, the Vanyar/Noldor/Teleri classifications refer to High Elves alone, i.e. those who *did* reach the Blessed Realm) and normal Elves are either Sindar (a subset of the Teleri, who never finished the journey) or Wood-Elves (those who never started the Great Journey at all or didn't even reach Beleriand - technically "Umanyar" or "Avari", or any other of umpteen differen classifications, but I've decided to call them "Wood-elves", it's simpler.) 
 - Some Hobbit titles changed around, to allow in sons of a Shirriff or the Mayor.
 - Kobold descriptions supplied by Prfnoff from a post to rec.games.roguelike.angband.

	To k_info.txt: Rings of speed now level 75 instead of 80. Rings of [stat] also sustain that stat, sustain rings are shallower so as to actually be *useful* for once. PDSM restored to former glory. Other DSMs made a little more rational - Law and Chaos should be same depths and prices, likewise the five single hues of the Multi-Hued set. New template added (Palantir) for the INSTA_ART of the Palantir of Westernesse. *ID* scrolls a little more common at deeper depths, and likewise potions of Healing, *Healing* and Life. Typo corrected in Mushrooms of Cure Serious Wounds, they should be 0.1 lb not 0.2. New digging tool - Mattock (ported from ZAngband) - a digger which can also make a useful weapon.

	To p_info.txt: 
 - Hobbits can now be rangers, but not mages, and dwarves can now be paladins. Elves cannot be priests. (Tolkien says there is little magical about Hobbits, but they are excellent with missile weapons: hence allowing them to be rangers but not mages. I can't imagine any dwarf religion that forbids the shedding of blood or the use of axes, so paladins should be allowed. Elves are more magical than religious, if they are not High. High-elves can still be priests, but not paladins.) 
 - Kobold player race added: can be rogues, rangers, warriors or priests. Weak, but resists poison. 

	To r_info.txt: 49 new monsters, 5 old ones are gone. Others have been reorganised to keep a fairly even progression of non-uniques all the way to 4000'. Some typos and illogicalities (such as cave ogres being worth fewer than black ogres) corrected. Some of the old monsters - e.g. Glaurung and Ancalagon - are tougher than they were. Non-uniques can have escorts. An explanation is attempted for why the player is fighting "good" creatures, to be found in the description lines of "Angel" and "Paladin" monsters. Full details can be found in r_info.txt.

	To v_info.txt: A typo corrected, to change the "crown" vault from a Lesser to a Greater Vault as it should always have been. Another typo corrected to allow access to the two "bubbles" in the bottom right corner of vault 30.