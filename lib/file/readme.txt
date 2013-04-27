Most files in this directory are use by the get_rnd_line function for various
purposes in the game. They can be edited and customized by the user with any
ascii text editor. If you add / remove lines you should modify the index (the
first line) accordingly. If you set an invalid index weird messages and other
things can appear, and you can crash the game. Please see sample.txt before
you try modifying the files.  The a_*.txt and w_*.txt are used when the game
generates a random artifact.  However, instead of picking a name from the
appropriate file, the game may form a new name from syllables[].

The files in this directory:

adject.txt   Used with 'noun.txt' to generate names for nonweapon artifacts
chainswd.txt Possible noise for the Chainsword
crime.txt    Possible crimes that speaking uniques may have committed
dead.txt     The tombstone picture (the death screen)
death.txt    Possible 'last words' when the player dies
elvish.txt   Syllables for the names of random artifacts
mondeath.txt Possible 'last words' for speaking uniques
monfear.txt  Possible lines for scared speaking uniques
monfrien.txt Possible lines for friendly speaking uniques
monspeak.txt Possible lines for speaking uniques
news.txt     The game intro screen
noun.txt     Used with 'adject.txt' to generate names for nonweapon artifacts
prefix.txt   Used with 'suffix.txt' to generate names for weapon artifacts
readme.txt   You are reading this file right now
rumors.txt   Possible rumours (for scrolls or rumour and shopkeepers)
sample.txt   A sample file for the random line selecting function
silly.txt    Silly monster names for hallucination
suffix.txt   Used with 'prefix.txt' to generate names for weapon artifacts
