set loc en

::msgcat::mcset $loc copyright "%s: Copyright (c) 2009 Tim Baker"

::msgcat::mcset $loc original-a "Based on Angband: Copyright (c) 2000 R. Ruehlmann"
::msgcat::mcset $loc original-k "Based on KAngband: Copyright (c) 2000 John I'anson-Holton"
::msgcat::mcset $loc original-o "Based on OAngband: Copyright (c) 2000 Bahman Rabii"
::msgcat::mcset $loc original-z "Based on ZAngband (c) the ZAngband Dev Team"

::msgcat::mcset $loc newsgroup "Newsgroup: rec.games.roguelike.angband"
::msgcat::mcset $loc email "Send comments to omnibandtk@shaw.ca"

# Fonts
if {[Platform unix]} {
::msgcat::mcset $loc font,sys "Helvetica 12"
::msgcat::mcset $loc font,fixed "Courier 12"
}
if {[Platform windows]} {
if {"Tahoma" in [font families]} {
	::msgcat::mcset $loc font,sys "Tahoma 10"
	::msgcat::mcset $loc font,fixed "Courier 10"
} else {
	::msgcat::mcset $loc font,sys "{MS Sans Serif} 8"
	::msgcat::mcset $loc font,fixed "Courier 8"
}
}

# Startup Window: buttons
::msgcat::mcset $loc New
::msgcat::mcset $loc Open
::msgcat::mcset $loc Setup
::msgcat::mcset $loc Quit

# Choose Game Window: window title
::msgcat::mcset $loc choose-title "Choose Game"

# Choose Game Window: prompt
::msgcat::mcset $loc choose-prompt \
	"Choose a game from this list of savefiles\
	in the lib\\save directory. Click \"Browse...\" to choose a savefile\
	from a different directory."

# Choose Game Window: column titles
::msgcat::mcset $loc choose-Name Name
::msgcat::mcset $loc choose-Version Version
::msgcat::mcset $loc choose-Description Description

# Choose Game Window: savefile info
::msgcat::mcset $loc choose-savefile "Level %d %s %s, Depth %d/%d"

# Choose Game Window: buttons
::msgcat::mcset $loc choose-Open Open

# Choose Game Window: misc
::msgcat::mcset $loc choose-file "Selected file:"
::msgcat::mcset $loc scan-title "Scanning Saved Games..."
::msgcat::mcset $loc scan-file "Scanning \"%s\""
::msgcat::mcset $loc scan-display Displaying...

# Choose Game Window: tabs
::msgcat::mcset $loc choose-tab-living "Living"
::msgcat::mcset $loc choose-tab-dead "Dead"
::msgcat::mcset $loc choose-tab-backup "Backup"

# Choose Game Window: backup dialog
::msgcat::mcset $loc title-backup "Restore backup file"
::msgcat::mcset $loc msg-backup "The savefile \"%s\" appears to be a backup."
::msgcat::mcset $loc backup-choice1 "Rename the file to \"%s\"."
::msgcat::mcset $loc backup-choice2 "Choose a new name for the file."
::msgcat::mcset $loc backup-choice3 "Use the file without renaming it."

# Choose Game Window: delete dialog
::msgcat::mcset $loc choose-delete-title "Delete savefile"
::msgcat::mcset $loc choose-delete-msg "Really delete this savefile?\n\n%s"

# Setup Window: window title
::msgcat::mcset $loc setup-title Setup

# Setup Window: tabs
::msgcat::mcset $loc setup-Icon Icons
::msgcat::mcset $loc setup-Music Music
::msgcat::mcset $loc setup-Sound Sound
::msgcat::mcset $loc setup-Variant Variant

# Setup Window: Icons page
::msgcat::mcset $loc icons-prompt "Select an icon set:"
::msgcat::mcset $loc adam16 "Adam Bolt 16x16"
::msgcat::mcset $loc adam24 "Adam Bolt 24x24"
::msgcat::mcset $loc adam32 "Adam Bolt 32x32"
::msgcat::mcset $loc adam16-shape "Adam Bolt (Shape) 16x16"
::msgcat::mcset $loc adam24-shape "Adam Bolt (Shape) 24x24"
::msgcat::mcset $loc adam32-shape "Adam Bolt (Shape) 32x32"
::msgcat::mcset $loc adam16+orig "Adam Bolt + Original Features 16x16"
::msgcat::mcset $loc adam32+orig "Adam Bolt + Original Features 32x32"
::msgcat::mcset $loc ascii "Ascii"
::msgcat::mcset $loc ascii16+adam "Ascii + Adam Bolt Features 16x16"
::msgcat::mcset $loc ascii24+adam "Ascii + Adam Bolt Features 24x24"
::msgcat::mcset $loc ascii32+adam "Ascii + Adam Bolt Features 32x32"
::msgcat::mcset $loc ascii16+adam-shape "Ascii + Adam Bolt Features (Shape) 16x16"
::msgcat::mcset $loc ascii24+adam-shape "Ascii + Adam Bolt Features (Shape) 24x24"
::msgcat::mcset $loc ascii32+adam-shape "Ascii + Adam Bolt Features (Shape) 32x32"
::msgcat::mcset $loc ascii+dg32 "Ascii + David E. Gervais 32x32"
::msgcat::mcset $loc ascii16+orig "Ascii + Original Features 16x16"
::msgcat::mcset $loc ascii32+orig "Ascii + Original Features 32x32"
::msgcat::mcset $loc dg32 "David E. Gervais 32x32"
::msgcat::mcset $loc dg32+iso "Isometric David E. Gervais 32x32"
::msgcat::mcset $loc original32 "Original 32x32"

# Setup Window: Music page
::msgcat::mcset $loc music-prompt \
	"If your computer does not have a sound card, or the game\
	crashes a few seconds after starting, you should disable music."
::msgcat::mcset $loc music-use "Use music"
::msgcat::mcset $loc music-lib Library:
::msgcat::mcset $loc music-desc Description:
::msgcat::mcset $loc music-no-libs "No music libraries are installed."

# Platform: windows
::msgcat::mcset $loc music-use-bass "BASS"
::msgcat::mcset $loc music-prompt-bass \
"Requires DirectX 3 or above."
::msgcat::mcset $loc music-use-fmod "FMOD"
::msgcat::mcset $loc music-prompt-fmod \
"Play music using FMOD."
::msgcat::mcset $loc music-use-midas "MIDAS"
::msgcat::mcset $loc music-prompt-midas \
"The MIDAS Digital Audio System."

# Setup Window: Sound page
::msgcat::mcset $loc sound-prompt \
	"If your computer does not have a sound card, you should\
	choose not to use sound."
::msgcat::mcset $loc sound-use "Use sound"
::msgcat::mcset $loc sound-lib Library:
::msgcat::mcset $loc sound-desc Description:
::msgcat::mcset $loc sound-no-libs "No sound libraries are installed."

# Platform: windows
::msgcat::mcset $loc sound-use-directsound "DirectSound"
::msgcat::mcset $loc sound-prompt-directsound \
	"DirectSound starts playing sounds quickly, and plays\
	sounds at any recorded frequency."
::msgcat::mcset $loc sound-use-bass "BASS (samples)"
::msgcat::mcset $loc sound-prompt-bass \
	"Requires DirectX 3 or above."
::msgcat::mcset $loc sound-use-bass-stream "BASS (streams)"
::msgcat::mcset $loc sound-prompt-bass-stream \
	"Requires DirectX 3 or above."
::msgcat::mcset $loc sound-use-fmod "FMOD"
::msgcat::mcset $loc sound-prompt-fmod \
	"Play sounds using the FMOD library."
::msgcat::mcset $loc sound-use-wavemix "WaveMix"
::msgcat::mcset $loc sound-prompt-wavemix \
	"This is a slow sound mixer which plays sounds recorded\
	at 11KHz only. Use this only if your system does not have\
	DirectX installed."

# Platform: unix
::msgcat::mcset $loc sound-use-openal "OpenAL"
::msgcat::mcset $loc sound-prompt-openal \
	"Play sounds using the OpenAL library."
::msgcat::mcset $loc sound-use-sdl "SDL Mixer"
::msgcat::mcset $loc sound-prompt-sdl \
	"Play sounds using the SDL Mixer library."

# Setup Window: Variant page
::msgcat::mcset $loc variant-always "Always choose this variant"

# UpgradeTool
::msgcat::mcset $loc upgrade-tool-title "Import Old Files"
::msgcat::mcset $loc upgrade-tool-prompt \
	"UpgradeTool can import your settings and savefiles from\
	an older version of %s.\nIf you do not have an older version of\
	the game then you do not need to use UpgradeTool.\n\nDo you want to run\
	UpgradeTool now?"

