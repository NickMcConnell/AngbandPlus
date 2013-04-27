XBAND 0.4.0

Author: Chris Watkins (xis@prodigy.net)

XBAND is an Angband variant based off of ZAngband 2.7.2. I made this game
to bring new gameplay elements into the world of Angband variants. XBAND
features one major new element; that of capturing and raising souls. 

Sometimes, when you kill a monster, it will drop a soul gem. These soul
gems can be imbued into rings and amulets at the soul dealer. 

Souls level up as you fight, their maximum level is level 6. Their pval is
equal to their level. As they level up, they gain more resists and abilities.

To gain level 2, you need to kill  200 monsters worth   1 xp or more
To gain level 3, you need to kill  400 monsters worth   8 xp or more
To gain level 4, you need to kill  600 monsters worth  27 xp or more
To gain level 5, you need to kill  800 monsters worth  64 xp or more
To gain level 6, you need to kill 1000 monsters worth 125 xp or more

As you can see from the chart, there are thresholds associated with each
level. Kills that give less XP than the threshold don't count toward
leveling up the items. When I say kill monsters, I really mean anything that
gains XP. Disarming traps also works, for example. 

There are five grades of rings and amulets. Copper, Silver, Gold, Platinum,
and Adamantium. The more valuable the material, the higher level an imbued 
soul can reach. 

Level 2 - Copper
Level 3 - Silver
Level 4 - Gold
Level 5 - Platinum
Level 6 - Adamantium

In addition, Adamantium is immmune to being destroyed by
the elements. 

Currently, each monster letter has a soul type associated with it. This
means that the soul of a snotling gives the same benefits as that of a black
orc. Monster types that you encounter in the first few levels of the game
(icky things, kobolds, etc.) have fairly weak souls. When you dive a little
deeper, you hit the mid-range of souls (lesser demons, ogres, golems, etc.).
The mid-range souls are about twice as strong. In the depths of the dungeon,
you find the most dangerous monster types (greater demons, ancient dragons,
liches, etc.), who have the most powerful souls.

In addition, many monsters have a secondary soul type. If a monster has an
elemental attack, its soul will have bonuses related to that element. If
a monster has multiple elemental attacks, it's soul gem will be created
with an elemental sub-type chosen randomly from its attacks.


Changes from version 0.3.1 include:

Vanilla town has had one more store added (Soul Dealer).
  As a consequence, Vanilla mode is now much more playable.
The Vanilla town is now the default town.
Removed the 'Imbue' option from the soul dealer; imbuing items is now a simple
  command. Just hit 'B' to (imbue/bind) a soul to an item.
  As a consequence, Ironman mode should now be much more playable.
Poison is now one of the basic resists, many items have been updated to reflect this.
Added poison immunity.
  Kobolds now gain poison immunity at L30.
  Poison soul gems now grant poison immunity at L6.
  Green Dragon Scale Mail now provides poison immunity.
Weapons and armor are no longer generated in stacks at the stores.
Rings and amulets no longer stack.
Made all staves/rods/potions/mushrooms/spells of "Cure X" act the same way.
Chaos magic has been re-worked a bit; it should have more flexibility now.
  Removed Wonder, replaced with Sense Power, which pseudo-ID's equipment.
  Removed Chain Lightning, replaced with Battle Frenzy, which berserks + hastes.
  Removed Touch of Confusion, replaced with Ray of Confusion, which confuses at range.
  Removed Chaos Branding, replaced with Mass Confusion, which confuses all monsters in LOS.
Psychometry (and by extension, the Chaos spell Sense Power) will now filter out already-sensed items.
Quest monsters now appear in red in visible monsters term.
The message about the soul dealer in monster recall is now displayed in green.
Character dumps are now saved in the SAVE directory, while note files are still
  saved in the USER directory. This prevents a problem where it was easy to over-
  write note files with character dumps.
Monsters that have elemental auras will now drop soul gems with elemental sub-types.
Weakened Ogre souls.
Changed the color of several monsters:
  Changed color of Lizardmen / Lizard King to prevent confusion with Gnome Mages
  Changed color of Alberich, Mime, and Hagen to all match.
  Changed color of most leprechauns
  Changed Forest Gnome to Gnome Archer, changed color.
  Changed color of snotlings to prevent confusion with goblins.
Dialed back damage bonuses. Each +1 now gives +5% damage.
Dialed back brand damage. Fire, Cold, Elec, Acid, and Poison brands now do 2X damage.
Made some changes to artifacts.
Messed around with hidden attribute detection on items that have not been *ID'ed*.
Randarts can now have Vampiric or Vorpal as random slays.
Randart lites now always have permanent light. 
  (The timeouts on fuel and the timeouts on activated abilities are linked; I had to either cut activated abilities on randart lites,
  or give all lites permanent lite).
Fixed ego-type 'of digging'.
Adjust costs of some ego items.
Warpstone weapons now cause random teleportation.
Ported over some bugfixes from Zangband 2.7.4b:
  Fixed some savefile loading bugs
  Fixed monster lights
  Fixed some crashes related to monster movement
  Fixed some door closing / opening stuff
  Fixed permenant inscription you used to get if you tried to destroy an artifact.
Changed algorithm for decreasing stats, algorithm taken from Z 2.7.4b
Fixed scrolls of rumor.
Replaced ZAngband with XBAND in a lot of help files.
Added the Soul Gem helpfile to the helpfile system.

Changes from version 0.3.0 include:

Randarts were being generated too often. This has been fixed.

Randart code has been further re-written:
  Randarts are generated with exactly two ego types (if possible), plus one
  of the following: a random sustain, slay, high resist, or other ability.
  Randarts sometimes have an activated ability.
Red/Blue/Black/White Dragon Scale Mail now provides immunity to their respective element
Made some of the mid-level soul types weaker
+Deadliness% now goes much higher. Maximum cap on a weapon was +75%, now +150%.
  Each + to damage was 3%, now +10%.
Increased damage from slays. All do X3 damage, except for Slay Animal (X2), 
  Slay Evil (X2), and Kill Dragon (X5)
Re-named some items to make naming more consistent / less confusing
  Armor of Bravery -> Boldness
  Rods/Staves of Perception -> Identify
  Helms of Holiness -> of Kings
Altered item generation algorithm; deeper items will now appear less often. Conversely,
  this change will make less deep items show up more often, which means you should see
  broken swords / potions of apple juice / assorted level-0 items in the dungeon again.
Fixed a jewelry pluralization bug

Changes from version 0.2.0 include:

Many new ego-item types. Some are lifted from Vanilla, others are original.
Added some "semi-cursed" items, items with both good and bad properties.
Ring / Amulet material is now *very* important - cheaper materials won't allow souls to fully develop
Made higher material rings / amulets rarer, but much more valuable
Rings / amulets once again stack, if unimbued.
Chaotic weapons (now called Warpstone weapons), are now created with random abilities.
The artifact system is completely re-worked. Randarts should be much weaker now.
Added a whole new monster race - the Modrons. They appear as 'N'.
Added a new monster - the Mutant Mushroom Patch. They should prove to be very annoying.
Added a new monster - the Yeek Necromancer.
Weakened the most powerful soul gems.
Fixed the bug with imbuing a stack of rings / amulets.
Changed the color of permanent walls, they now appear light brown.
Added many new room types from Z 2.7.3
Made unusual rooms about twice as common
Tweaked the character dump a bit
Fixed the bug that allowed the slay animal flag on armor.
Renamed ewoks -> forest gnomes, removed SILLY flag.
Changed color of kamikaze yeeks and serpent men to remove conflicts.
Tweaked rewards for depositing soul gems at the soul dealer
Added two more elemental soul types; law and balance. 
Added a new automatic-ID system for high-level characters.
  High-level characters will automatically pseudo-id weapons/armor when they are prompted to pick them up.
  Mages, Priests, High-Mages, Mindcrafters, and monks gain this ability at level 50, 
  Paladins, Rangers, Warrior-Mages, and Chaos-Warriors gain it at level 40,
  Warriors gain it at level 30,
  and Rogues gain it at level 20.
At very high levels, some classes will now automatically identify weapons/armor when they are prompted to pick them up.
  Paladins, Rangers, Warrior-Mages, and Chaos-Warriors gain it at level 50,
  Warriors gain it at level 40,
  and Rogues gain it at level 30.
At level 40, Rogues automatically identify all items when they are prompted to pick them up.
At level 50, Rogues automatically *Identify* all items when they are prompted to pick them up.
Rogues now gain Identify and *Identify* spells earlier.

Changes from version 0.1.0 include:

New features:

When evaluated, souls and imbued items now show any bonuses they give to
  accuracy, damage, or armor class.
Two new item flags have been added, SH_COLD and SH_ACID. They produce a
  freezing aura, and an acidic aura, respectively. Two new ego cloaks have
  been added that use these flags.
Cloaks that produce elemental auras have been made less deep.
The single versions of some of the low-level people (novice mages, rangers,
  etc.) have been removed and replaced with similar kobolds and yeeks. The 
  multiple versions of them remain.
Souls now have a secondary soul type. A soul's primary type is determined
  by it's race (kobold, mold, dragon, etc.), while it's secondary soul type
  is determined by any elemental attack it may have. Now a fire dragon will
  give different bonuses than an ice dragon. If a monster has multiple
  elemental attacks, a soul gem has a secondary type chosen at random. For
  example, some Storm Trolls will produce gems with an electric sub-type,
  while others produce gems with a cold sub-type.
Because of the above changes, racial soul bonuses have been reduced in
  power.
The Soul Dealer will now give out much better rewards when you deposit
  souls. He will now also accept souls of monsters he already has copies of,
  but only pays cash for those.
The format of the character dump has been changed a good bit, it now
  shows more information. Also, character dumps made by deceased characters
  are the same as those made by living characters. Lastly, your character's
  title will now show in the character dump.
Two new artifact weapons have been added.
Four new deep ego-weapons have been added.
Removed the Greater Hell Beast!!!
Removed the randomized error messages.
Soul gems now drop only half as often, and level only half as fast.
Holy Avengers no longer give extra attacks.

Bugfixes: 

When evaluated, souls and imbue items now show their proper maximum level
  and pval
Imbued items that are currently wielded can now be displayed
The soul dealer will no longer charge you if cancel viewing a soul

XBAND 0.1.0

Changes from ZAngband 2.7.2 include:

New soul system:

When monsters are killed, they sometimes drop soul gems. Soul gems can be
imbued into jewelry by the soul dealer. Imbued jewelry grants magical
abilities to the wielder. Imbued items grow in strength as the game
progresses. The exact abilities granted vary depending on the type of soul
gem imbued, and the level of the imbued items. The soul dealer can
appraise soul gems and imbued items (for a price).

You may also trade in soul gems at the soul dealer. He will reward you
based on the number of soul types you have previously turned in. Note that
he only wants one gem of each type.

While at the soul dealer, you can view which souls you have turned in, and
which you have not.

To encourage use of imbued items, all other rings and amulets (including
rings of speed, and the one!) have been stripped out of the game.

I've made changes to how elemental attacks damage items. Wands are now
destroyed by fire, and not electricity, while amulets and soul gems join
rings in their vulnerability to electricity.

I've included a new font I made some time ago, 8X8.FON. It's a bit ugly for
text, but provides truly square tiles for dungeon adventuring.

Quest levels now give meaningful level feelings again.

I stripped out a lot of ZAngband code for things that were incomplete, and 
I don't ever plan to complete. This includes Lua support, the virtue
system, the borg, and TK support. This might have been a bad idea.


NOTES:

I compile under Windows XP, using Visual Studio. I tried to fix the
makefiles for other platforms, but probably broke them horribly.

I am currently focusing on gameplay changes, but at some point in the future, I will
probably make massive changes to the setting / background.

I will probably break the savefile format repeatedly over the next few releases.

The vanilla town has no Soul Dealer.

Ironman characters have no method of imbuing souls into jewelry.




I hereby release XBAND under the terms of the GNU General Public License
(version 2), as well as under the traditional Angband license.  It may be
redistributed under the terms of the GPL (version 2 or any later version),
or under the terms of the traditional Angband license.
