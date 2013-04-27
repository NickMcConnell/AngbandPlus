XBAND 0.2.0

Author: Chris Watkins (xis@prodigy.net)

XBAND is an Angband variant based off of ZAngband 2.7.2. I made this game
to bring new gameplay elements into the world of Angband variants. XBAND
features one major new element; that of capturing and raising souls. 

There are five grades of rings and amulets. Copper, Silver, Gold, Platinum,
and Adamantium. Adamantium is immmune to being destroyed by the elements,
but other than that, they all are basically the same, they just sell for
different prices. The more valuable ones may be a source of cash later in
the game.

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

Currently, each monster letter has a soul type associated with it. This
means that the soul of a snotling gives the same benefits as that of a black
orc. Monster types that you encounter in the first few levels of the game
(icky things, kobolds, etc.) have fairly weak souls. When you dive a little
deeper, you hit the mid-range of souls (lesser demons, ogres, golems, etc.).
The mid-range souls are about twice as strong. In the depths of the dungeon,
you find the most dangerous monster types (greater demons, ancient dragons,
liches, etc.), who have the most powerful souls.

In addition, many monsters have a secondary soul type. If a monster has an
elemental attack, it's soul will have bonuses related to that element. If
a monster has multiple elemental attacks, it's soul gem will be created
with an elemental sub-type chosen randomly from it's attacks.





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
imbued into jewelery by the soul dealer. Imbued jewelery grants magical
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

It's very likely that imbued items are completely unbalanced. I have a lot
more work to do in that area.

I compile under Windows XP, using Visual Studio. I tried to fix the
makefiles for other platforms, but probably broke them horribly.

I am currently focusing on gameplay changes, but at some point in the future, I will
probably make massive changes to the setting / background.

I will probably break the savefile format repeatedly over the next few releases.

The vanilla town has no Soul Dealer.

Ironman characters have no method of imbuing souls into jewelery.

If you purchase a stack of rings at the jewelers, you can imbue the entire stack. This is a bug.




I hereby release XBAND under the terms of the GNU General Public License
(version 2), as well as under the traditional Angband license.  It may be
redistributed under the terms of the GPL (version 2 or any later version),
or under the terms of the traditional Angband license.
