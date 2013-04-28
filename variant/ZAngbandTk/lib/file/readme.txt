Most files in this directory are use by the get_rnd_line function for various
purposes in the game. They can be edited and customized by the user with any
ascii text editor. If you add / remove lines you should modify the index (the
first line) accordingly. If you set an invalid index weird messages and other
things can appear, and you can crash the game. Please see sample.txt before
you try modifying the files.  The a_*.txt and w_*.txt are used when the game
generates a random artifact.  However, instead of picking a name from the
appropriate file, the game may form a new name from syllables[].

The files in this directory:

a_cursed.txt Possible names for randomly generated cursed armour artifacts
a_high.txt   Possible names for randomly generated 'powerful' armour artifacts
a_low.txt    Possible names for randomly generated 'weak' armour artifacts
a_med.txt    Possible names for randomly generated 'medium' armour artifacts
chainswd.txt Possible noise for the Chainsword
crime.txt    Possible crimes that speaking uniques may have committed
dead.txt     The tombstone picture (the death screen)
death.txt    Possible 'last words' when the player dies
elvish.txt   Syllables for the names of random artifacts
error.txt    Possible random error messages (instead of "Type ? for help")
mondeath.txt Possible 'last words' for speaking uniques
monfear.txt  Possible lines for scared speaking uniques
monfrien.txt Possible lines for friendly speaking uniques
monspeak.txt Possible lines for speaking uniques
news.txt     The game intro screen
readme.txt   You are reading this file right now
rumors.txt   Possible rumours (for scrolls or rumour and shopkeepers)
sample.txt   A sample file for the random line selecting function
silly.txt    Silly monster names for hallucination
w_cursed.txt Possible names for randomly generated cursed weapon artifacts
w_high.txt   Possible names for randomly generated 'powerful' weapon artifacts
w_low.txt    Possible names for randomly generated 'weak' weapon artifacts
w_med.txt    Possible names for randomly generated 'medium' weapon artifacts
