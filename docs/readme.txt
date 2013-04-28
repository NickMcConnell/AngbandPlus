

  ******  *          High adventure in a   *  world of magic and cold steel   *
 ***   ****                              ***                                ***
***      **                               **                                 **
****      *   *****     *  **     ****    ** ****    *****     *  **    **** **
 *******     *** ***  ********  ***  ***  ***  ***  *** ***  ********  ***  ***
   *******       ***   **   **  **    **  **    **      ***   **   **  **    **
*      ****   *** **   **   **  **    **  **    **   *** **   **   **  **    **
**      ***  **   **   **   **  **    **  **    **  **   **   **   **  **    **
****   ***   *** ***   **   **  ***  ***  ***  ***  *** ***   **   **  ***  ***
*  ******     ***  ** **** ****  ******* *** ****    ***  ** ** * ****  ****  **
                                      **
   Version 1.0.0                *     **
                                 ******



     This file describes how to use Sangband ports that do not use the new 
graphical interface.  It does not cover actual gameplay; for that, consult the 
Manual.


Table of Contents

1.  The Basics
 1.1  System requirements
 1.2  Playing
 1.3  Support
 1.4  Upgrading
 1.5  Where to find files
 1.6  Troubleshooting
 1.7  Window options
 1.8  Screenshots

2.  Development
 2.1  Emails, Websites, and Newsgroups
 2.2  Ways to help
 2.3  Credits

3.  Appendices
 Appendix A:  Copyright
 Appendix B:  Exclusion of warranty




====== The Basics ======

System Requirements:
     At least a x386 processor and a monitor capable of displaying 16 colors and 
about 640x480 (VGA) resolution.  Some ports, especially those that use graphics, 
need more than this.


Playing:
     Double-click the executable, choose to start a new character, roll him
up, and begin play.  The game will display useful help text if you type '?'
on most screens.


Support:
     The Sangband Official website is at:
http://www.runegold.org/sangband
     The dedicated Sangband support thread is at:
http://angband.oook.cz/forum/showthread.php?t=24
     You can also use the email at the top of this file.


Upgrading:
     When you see a new version announced, or one appears on the web site, you
may usually transfer your existing character to the new game.
     You make a backup copy of your savefile (located either in
"lib/save/yourname", or in your user directory, sangband section), then copy it
over to the new version.  If you want to, you can do the same thing with
your user preference files ("lib/user/yourname.prf" and/or "lib/user/user.prf")
You can also copy your old score file ("lib/apex/scores.raw").


Where to find files:
     On a single-user installation, all files will be in the game directory.  On
a multi-user installation, your personal files (save games, high scores,
preferences and config, screenshots, etc.) live in your user directory, usually
in the folder "sangband".  It is also possible to set up Sangband in other ways.


Troubleshooting:
     Most players just fire up the game and start having fun.  But you might
get unlucky.  This section lists problems that players have reported:


Q.  "I'm having trouble in the very early game."

A.  Be sure to spend your startup experience (type '$') on skills like throwing
or magic or melee - something that directly defeats your enemies - before
entering the dungeon.


Q.  "The game crashes on me, displays glitches, or has other problems."

     Sangband in general, and the graphical interface in specific, is in active
development.  Lacking a full-bore Quality Assurance Department, the developers
rely on you, the player, to report problems.

     Try isolating the problem.  Does turning off sound/music, graphics, or
mouse support help?  Does the problem happen consistantly when you or the game
do something specific?

     Report it.  Please include all information you think will help someone 1)
get the game to exhibit the problem, and 2) find and fix it.  If you are a coder
yourself, replacement code and/or diffs are always welcome.


Window options:
     On monitors 800x600 or larger, the game automatically displays several sub-
windows in addition to the main window.  Each sub-window can display information
of your choosing; the game ships with several sub-windows already set.  Go into
options (type '=') and choose "Window Options".  There are three sections; type
'>' and '<' to navigate between them.  To get help for the section you are in,
type '?'.


Screenshots:
     There are three different ways to take screenshots from within the game.

Pure ASCII screenshots (monochrome):
     Type ')' and choose text.  Suitable for newsgroup and other pure-text
environments.

Forum screenshots (color):
     Type ')' and choose forum.  Suitable for posts to the Angband Forum.

HTML screenshots (color):
     Type ')' and choose html.  Suitable for websites and browsing.




====== Development ======

Emails, Websites, and Forums:
      Suggestions, bugreports, and all other communication may be sent to the
email shown at the top of the Manual.  If it should bounce, then navigate to
the websites listed below and use their contact info.

The official Sangband webpage is at http://www.runegold.org/sangband/.
The Sangband Sourceforge repository is at 
http://sourceforge.net/projects/sangband/.

There are also several unofficial websites.  Let me know if you want yours
listed in this readme.

Sangband uses the Angband Forum, located at http://angband.oook.cz/forum/.


Ways to Help:
     Sangband is a well-established code project that offers firm support for
any number of Neat New Features.  Development is opening up; you don't have to
be a C coder to play a major role.  If you have ideas, and the skills to help
make them a reality, drop me a line.  What gets done right, right now - and what
keeps getting delayed - depends on your assistance.

      An example:  The future of Rogulike game interface should exploit the
capacities of modern graphical machines.  But how to do this best?  Further
improve the current 2D graphics?  Create a true graphical user interface?
Explore the possibilities of multi-color, extended ASCII artwork?  Go with an
isometric view?  Add sprites?  The answer will depend on the artists; my (LM's)
role will be that of making sure the game presents your work in the most
effective possible way.


--- Credits ---

     There is a reason why Sangband is as well-established as it is; a lot of
people have helped it prosper and grow.

Code and Text Sources:
     Moria, UMoria, Angband (various versions), Angband--, Angband/64, Antiband
Cthangband, sCthangband, Eric Bock's Angband, Rangband, Zceband, EyAngband,
FAngband, Hengband, Kangband, Kamband, Oangband, NPPAngband, PernAngband and
ToME, PsiAngband, UnAngband, Zangband.  For more details, see "/src/credits.txt
and "src/changes.txt" in the source release.

Developers:
     Joshua Middendorf, Christer Nyfält, Scott Yost

Contributors:
     Werner Baer, Charlie Ball, Eric Bock, Clefs, DarkGod, Andrew Doull, Steven
Fuerst, Diego Gonzalez, Jeff Greene, Eddie Grove, Adam Horowitz, Ben Harrison, 
Aram Harrow, Improv, Mikolaj Konarski, Matthias Kurzke, Julian Lighton, Joshua
Middendorf, Takeshi Mogami, Ross Morgan-Linial, Matthew Neumann, Christer
Nyfält, Bahman Rabii, Robert Ruehlmann, Andrew Schoonmaker, Paul Sexton, Ethan
Sicotte, William Tanksley, Topi Ylinen, Scott Yost, Eytan Zwieg, and many others

Artists and Writers:
     Adam Bolt, David Gervais, Reenen Laurie, Cosmic Gerbil.  Sounds and music
also from Angband and Falcon's Eye.

Porters:
     Christer Nyfält, Ken Dubuc, Dave




--- Copyright ---

     This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License.  Parts may also be available
under the terms of the Moria license.  For more details, see "/docs/copying.txt
in the source release.

     Exception:  Artistic works are sometime made available only under specific
terms; in such cases, the terms will either be detailed in the main directory in
which these works are located, or in the work itself.



--- Exclusion of Warranty ---

  11. Because the program is licensed free of charge, there is no warranty
for the program, to the extent permitted by applicable law.  Except when
otherwise stated in writing the copyright holders and/or other parties
provide the program "as is" without warranty of any kind, either expressed
or implied, including, but not limited to, the implied warranties of
merchantability and fitness for a particular purpose.  The entire risk as
to the quality and performance of the program is with you.  Should the
program prove defective, you assume the cost of all necessary servicing,
repair or correction.
