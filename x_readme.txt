XBAND 0.1.0

Author: Chris Watkins (xis@prodigy.net)

XBAND is an Angband variant based off of ZAngband 2.7.2. I made this game
to bring new gameplay elements into the world of Angband variants. This
first release features one major new element; that of capturing and
raising souls. 

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

You can turn in soul gems at the soul dealer, but he currently gives really
crappy rewards.

I compile under Windows XP, using Visual Studio. I tried to fix the
makefiles for other platforms, but probably broke them horribly.

I am currently focusing on gameplay changes, but at some point in the future, I will
probably make massive changes to the setting / background.

I will probably break the savefile format repeatedly over the next few releases.




I hereby release XBAND under the terms of the GNU General Public License
(version 2), as well as under the traditional Angband license.  It may be
redistributed under the terms of the GPL (version 2 or any later version),
or under the terms of the traditional Angband license.
