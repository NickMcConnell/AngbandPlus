Win32 Readme
============

Requirements: Lispworks Personal or other Lispworks.

If your Lispworks is not installed in:
c:\program files\xanalys\lispworks personal\lispworks-personal-4200.exe

you need to edit vanilla.bat and update the path.

If you don't have Lispworks, you can get a free personal copy at:
http://www.lispworks.com/downloads/lww-per-download.html

(You only need the executable, not the docs)

Lispworks Personal have a time limit of five hours, but that should be
enough for testing.  and if you want to play more, just save the game
with Ctrl-X and restart. 

--

To start the game, click the Langband icon.  You should be dropped
into Lispworks Personal with aprompt like:

CL-USER 1>

On this prompt type:  (load "vanilla")    and hit Enter

It will produce a lot of text first, then it will work hard for awhile
after it has "registered" Langband-Vanilla.  This is normal.  When it
finishes it will display a new prompt:

CL-USER 2>

On this prompt type: (langband)   and hit Enter

You will now enter the game.  Within the game, hit '?' for help.

When you quit the game you will get a new prompt like:

CL-USER 3>

To exit lispworks type (quit) to quit, or try (langband) again.


Have fun. :-)
