                         Cat-and-the-Hack Angband


Cat-and-the-Hack Angband is Copyright (c) 2003 by neko and Evan Jenkins,
with prior work by the thousands who have maintained the Angband source.
Distribution of this variant is the same as that of Vanilla Angband, of
course.

If you would like to use any of the changes in CatH in your variant, by
all means, do so. One purpose of CatH is a sort of 'proof of concept' of
ideas that have floated through my mind. I wouldn't mind a line of
credit if you do so, however.

The current verson of CatH-Angband should be available at
http://cath-angband.sourceforge.net/ . If you have any suggestions, bugs
or whatever, send them to neko@inet.att.co.kr or evan@drwbwl.com. Thank
you.

===============================================

CatH-Angband was started in 1997 as an attempt to balance out the
classes a bit, make certain classes fit their idioms a bit better, and
tweak the user interface to make life a little bit easier.

CatH was updated to the Angband 2.9.1 codebase in 2000. Three new races
were added, a few tweaks were made, and a few items were invented. But
after a few updates, CatH once again fell dormant.

CatH soon fell to the wayside as Angband turned 3.0 and gained many
improvements, including a few CatH already had. The original maintainer,
neko, didn't feel like updating CatH to version 3. Then one day, I
decided I'd step in and do all the grunt work. Without further ado,
here's a list of the differences between Vanilla Angband and CatH.

--Evan Jenkins

The critical hits system has been redone for most classes. While mages
have the same ol' system as before, other classes can now get crits more
frequently and with more power, depending on the weight of their weapons
(the heavier the better). For warriors and paladins, this factor could
now make large axes and such a more attractive proposition than the
extra-blow generating whips and such that get used in Vanilla.

Elemental rings, such as Rings of Flames or Vilia, now 'confer' powers
to the weapon you're wielding. For example, if you have a normal short
sword (+0, +0) and a Ring of Flames, you are effectively attacking with
a Short Sword of Burning (+0, +0). Some other artifacts confer powers to
weapons as well.

Range weapons of ammunition have been added. They magically create a
shot, arrow, or bolt when needed. Ammo created in this manner is
enchanted to around (+5,+5), give or take, and disappears after use.

Wood Elves have been added as a player race. They are highly specialized
with the bow and have intrinsic extra might.

A new race, the Lagdalen, has been added, with inherent speed and
eventual free action at the cost of the ability to wear rings or gloves.

Dark Avatars have been added as a player race, with incredible stats and
resist nether, but with intrinsic drain experience.

Players now have a chance to save their armor from being damaged from
acid attacks, depending on their dexterity. However, if this succeeds,
the player will take full damage (the player takes half damage if it
hits the armor). Acid and fire attacks also have a small chance of
disfiguring the player, reducing charisma.

Wood Elves (and any new race with the INH_MIGHT flag) get an extra bonus
point to extra might when they reach level 25 if they use a bow.

Rogues who attack a sleeping creature deal out extra damage, depending
on the kind of weapon they use (light blades do most damage).

Attacking a sleeping monster will always hit, regardless of class. How
could you miss?

Autoscumming is now available on startup, and _not_ while the character
is alive. In addition, there is now a "Nightmare mode" which quadruples
the chance of a special room being generated.

The monster health bar has been altered, ala GW-Angband. It now displays
the status of the monster, whether asleep, afraid, or whatever.

You are no longer limited to diving to level 127. If you have the
patience, you can dive to level 9999, about 94 miles deep. The odds of
finding good objects don't increase as you dive beyond 127 however
unless you're in nightmare mode. The monsters do get tougher, however.
At ten minutes a level, you'll spend over two months straight to reach
the bottom.

The Temple now stocks stat restore potions so you don't go to tears
waiting for the stupid Alchemy to restock that Restore Strength you need
so badly.

Magic mushroom patches have been removed, because they are flat out the
most irritating little (*#$#@s on the planet :) They have been replaced
with touger archers. The graphics tiles have not been updated however,
since I don't use them and am too lazy to redraw them. :)

Colorized the monster recall window, so that certain numbers stand out a
little better. Note in particular that a monster's depth is in red if
it's out of your maximum depth, umber if it's at or before depth.

Hitting "m" as a prayercaster will preform a prayer, and hitting "p" as
a spellcaster will perform a spell.

There is a "Quick mode" option for the autoroller, which gets rid of a
delay in the autoroller loop, generating stats very quickly. You're
still currently limited to one million rolls, however.

The status info at the side of the dungeon view now displays how much
experience is needed to get to the next character level.

Various other undocumented surprises.
