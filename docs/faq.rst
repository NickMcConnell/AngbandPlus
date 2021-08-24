==========================
Frequently Asked Questions
==========================

The best way to get answers to your questions is to post them on the `Angband forum`_, in the Variants section.

.. contents:: Contents
   :local:

Issues and problems
-------------------

How do I report a bug?
~~~~~~~~~~~~~~~~~~~~~~

Post on the `Angband forum`_.

Bug reports should include:

* that you are playing Xygos, not standard Angband or some other variant
* your current operating system (e.g. Windows 10)
* what version the problem appeared in
* the best steps you can figure out to reproduce the bug.

Savefiles that show the problem might be requested, because they help tracking bugs down.

Dark monsters are hard to see
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fix (reduce) the alpha on your screen, or use the "Interact with colors" screen under the options (``=``) menu.  Navigate to the ``8`` using ``n`` and increase the color intensity with r(ed)/g(reen)/b(lue).

I play in a terminal on Linux, and the colours are screwed up
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's very likely that your console is not set to display 256 colours. Try setting the TERM environment variable to "xterm-256color" in your shell before starting angband::

	$ export TERM=xterm-256color
	$ angband

I'm playing in a terminal and the Escape key isn't working as it should
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a feature of terminals to allow you to enter keypresses that can't be sent directly.  Use the backtick ``\`` key instead - it's equivalent in every way to Escape.  (This doesn't apply in Sangband, or NPPAngband.)

Is there a way to disable that thing that pops up when you hit the enter key?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Go into the options menu, choose "Edit keymaps", then "Create a keymap".  Press Enter at the "Keypress" prompt and a single space as the "Action".  In short: ``=m8 <enter> <enter>``.

And then you'll probably want to choose to "Append keymaps to a file" so that it persists for next time you load the game.

This just replaces the default action of Enter with a "do nothing but don't tell me about help" action. If you want to keep the menu available, say on the 'Tab' key, you can also remap the Tab keypress to the ``\n`` action.


Development
-----------

What are the current plans for the game?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Nothing is set in stone, but expect much of future Angband releases to be pulled in when they arrive and the
general movement from a Tolkien-inspired fantasy theme to eclectic sci-fi to continue.

How do I suggest an idea/feature?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Post it on the forums (variant section, marked as Xygos).  If people think it's a good idea, it will generally get some discussion; if they don't, it won't.  I keep an eye on the forums, and these ideas may be filed for future implementation.

How do I get a copy of the source code?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Go to the GitHub_ page, where you can find the bleeding edge as well as all previous versions.

How do I compile the game?
~~~~~~~~~~~~~~~~~~~~~~~~~~

Please see the compiling section of the manual.

How do I contribute to the game?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You have two options:

* Write your patch and submit it as a pull request on GitHub.
* Post about it on the forum.

All contributions are accepted as dual-licenced with both the Angband and GPLv2 licences.

If the patch is a bugfix, then you can reasonably expect it to be integrated into the development tree. If it's more involved, and the feature is not one that the next version is planned to have, the patch may go through several reviews before being incorporated. It may also just be unsuitable for Xygos - in which case, please don't take rejection badly; you may just be better off writing your own variant (or submitting it to another variant with
different goals).

Non-code activities are different. Documentation can be written on the wiki, or if you're a graphics designer (and they're always welcome) then please talk on the mailing list about your work.


.. _GitHub: https://github.com/msearle5/xygos/
.. _Angband forum: http://angband.oook.cz/forum
