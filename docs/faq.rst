Frequently Asked Questions
==========================

The best way to get answers to your questions is to post them on the
`Angband forum`_ or `MAngband forum`_.

Issues and problems
-------------------

How do I report a bug?
**********************

Post on the `Angband forum`_ or `MAngband forum`_.

Be sure to include everything you can figure out to reproduce the bug.

Savefiles that show the problem might be requested, because they help tracking
bugs down.

Dark monsters are hard to see
*****************************

Fix (reduce) the alpha on your screen, or use the "Interact with colors" screen
under the options (``=``) menu. Navigate to the ``8`` using ``n`` and increase
the color intensity with r(ed)/g(reen)/b(lue).

I'm playing in a terminal and the Escape key isn't working as it should
***********************************************************************

This is a feature of terminals to allow you to enter keypresses that can't be
sent directly. Use the backtick ``\`` key instead - it's equivalent in every way
to Escape. (This is unverified for PWMAngband.)

Is there a way to disable that thing that pops up when you hit the enter key?
*****************************************************************************

Go into the options menu, choose "Edit keymaps", then "Create a keymap". Press
Enter at the "Keypress" prompt and a single space as the "Action".
In short: ``=m8 <enter> <enter>``.

And then you'll probably want to choose to "Append keymaps to a file" so that it
persists for next time you load the game.

This just replaces the default action of Enter with a "do nothing but don't tell
me about help" action. If you want to keep the menu available, say on the 'Tab'
key, you can also remap the Tab keypress to the ``\n`` action.

Development
-----------

What are the current plans for the game?
****************************************

See forums.

How do I suggest an idea/feature?
*********************************

Post it on the forums. If people think it's a good idea, it will generally get
some discussion; if they don't, it won't. The developers keep an eye on the
forums, and ideas deemed OK will get filed for future implementation.

Sometimes a suggestion may not be right for the game, though. Some suggestions
would change aspects of PWMAngband that are essential to its nature; PWMAngband
has a long history, and so has developed a certain character over the years.
Some suggestions might make a good game, perhaps even a better game than
PWMAngband, but would make a game that is not PWMAngband. To some extent,
variants exist to address this (for example: Tangaria has a more colorful
graphical environment), but even so they tend to adhere to the core PWMAngband
principles.

How do I get a copy of the source code?
***************************************

For now, go to the `developer's webpage`_ where you can download the
latest source code as well as a few previous versions. In the near future, a
GitHub page will be available.

How do I compile the game?
**************************

Please see the compiling section of the manual.

How do I contribute to the game?
********************************

Post about it on the forum.

.. _developer's webpage: https://powerwyrm.monsite-orange.fr
.. _Angband forum: http://angband.oook.cz/forum
.. _MAngband forum: https://mangband.org/forum/
