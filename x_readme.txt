XBAND 0.5.0

Author: Chris Watkins (xis@prodigy.net)

XBAND is an Angband variant based off of ZAngband 2.7.2. I made this game
to bring new gameplay elements into the world of Angband variants. 

What makes XBAND unique?

1. The soul-gem system. In XBAND, you build your own magical rings and
     amulets by binding the souls of defeated monsters to jewelry.
2. The races. In XBAND, you play the part of an nasty monster. You might
     be an Orc, or a Troll. You aren't fighting to save the world. You
     are fighting because it's fun.
3. The auto-ID system. In XBAND, as your characters advance, they gain
     the ability to evaluate items at a glance. Some classes, like rogues,
     are very good at this. This helps speed things up in the later parts
     of the game.
4. The randart system. In XBAND, every artifact is randomly generated. As
     you descend deeper into the dungeon, you will find artifacts of
     greater and greater power.

Changes from version 0.4.0 include:

Massive setting changes:
  Stripped out ALL of the old races, replaced them with the green menace:
    Black Orcs, Savage Orcs, Goblins, Night Goblins, Stone Trolls,
    Ogres, Ettins, Hobgoblins, Gremlins, Snotlings, and Humans.
  Edited help files to match new races.
  Consequently, the townsfolk now look a little different...
  New functions for randart names.
  Many items have been renamed.
  Stripped out all references to Amberites, Trump and the Pattern.

Big changes to the item code:
  Normal artifacts have been removed, only randarts are generated now.
  Added ILL_STAT flags. Items can now be generated that boost one stat while
    penalizing another.
  Removed IGNORE_XXX, replaced with IGNORE_ELEM, which ignores all elements.
  Many items have had IGNORE_ELEM added.
  Items without IGNORE_ELEM will still ignore some elements, if the item
    grants resistance against that element (armor of resist acid, for example).
  Removed QUEST_ITEM, along with grond + morgoth's crown
  Added AURA_POIS, a poison sheath.
  Re-worked the character info screen a little bit to show the new flags:
    AURA_POIS is shown on the screen
    ILL_XXX flags are shown as penalties
    If an item has a penalty to a stat, but also sustains it, that value will
    be shown in yellow.
  Some ego-types have been made rarer, others more common.

Summoned monsters now appear next to the summoner, rather than the player.

Monsters generated in town no longer drop soul gems.

Added some more info when you 'i'nspect a bound item.

Greatly weakened staff + wand draining

Tweaked the Dark soul sub-type; added STEALTH.

Bug Fixes:
  Fixed potions of life; they weren't healing.
  Fixed a bug where randart lites were not being generated with some flags
  Fixed a bug where randarts with the 'Sharpness' ego-type could give way too
    many extra attacks
  Fixed a bug that allowed *ID'ed* artifacts to be destroyed
  Non-sword randarts can no longer have the 'Vorpal' flag, they get the
    'Earthquake' flag instead.
  Fixed a bug in the birth screens; hitting escape could cause a 
    program crash.
  Fixed a bug where acid was damaging acid-resistant armor+weapons
  Fixed a typo in the chaos spell descriptions.
  Reverted psychometry to previous behavior; new behavior was bugged.
  Updated spell info screens with new healing amounts.
  Fixed a bug where some worthless items were being pseudo-id'ed as "excellent"

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
