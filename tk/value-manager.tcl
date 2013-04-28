# File: value-manager.tcl

# Purpose: the Value Manager

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# The Value Manager records various values, and informs clients when those
# values are modified.

namespace eval NSValueManager {

	variable Value
	variable Priv

# namespace eval NSValueManager
}

# NSValueManager::InitModule --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::InitModule {} {

	global Angband
	variable Value
	variable Priv

	# Each client gets a unique ID
	set Priv(clientId) 0

	set Priv(init) 1
	qeinstall <Value> NSValueManager::ExpandValue

	# These are the default values. The tk/config/value file is sourced
	# below to set user-defined values.

	Manage healthBarBD [palette set 220]
	Manage healthBarBD2 145
	Manage healthBarBL [palette set 35]
	Manage healthBarBL2 170
	Manage healthBarDone [palette set 71]
	Manage healthBarDone2 145
	Manage healthBarToDo [palette set 129]
	Manage healthBarToDo2 145

	Manage healthNameBD [palette set 252]
	Manage healthNameBD2 145
	Manage healthNameBG [palette set 252]
	Manage healthNameBG2 145
	Manage healthNameBL [palette set 252]
	Manage healthNameBL2 145
	Manage healthNameText [palette set 0]
	Manage healthNameText2 180

	if {[variant KANGBANDTK ZANGBANDTK]} {
		Manage friendBarBD [palette set 212]
		Manage friendBarBD2 145
		Manage friendBarBL [palette set 192]
		Manage friendBarBL2 145
		Manage friendBarDone [palette set 198]
		Manage friendBarDone2 160
		Manage friendBarToDo [palette set 129]
		Manage friendBarToDo2 145
	}
	Manage listBG [palette set 255]
	Manage listHilite [palette set 158]
	Manage listInactive [palette set 249]
if 0 {
	Manage statusBD [palette set 253]
	Manage statusBD2 145
	Manage statusBG [palette set 129]
	Manage statusBG2 145
	Manage statusBL [palette set 0]
	Manage statusBL2 145
	Manage statusText [palette set 0]
	Manage statusText2 180
}
	Manage statusGoodBD [palette set 230]
	Manage statusGoodBD2 145
	Manage statusGoodBG [palette set 197]
	Manage statusGoodBG2 145
	Manage statusGoodBL [palette set 191]
	Manage statusGoodBL2 145
	Manage statusGoodText [palette set 0]
	Manage statusGoodText2 180

	Manage statusInfoBD [palette set 240]
	Manage statusInfoBD2 145
	Manage statusInfoBG [palette set 212]
	Manage statusInfoBG2 145
	Manage statusInfoBL [palette set 210]
	Manage statusInfoBL2 170
	Manage statusInfoText [palette set 0]
	Manage statusInfoText2 180

	Manage statusBadBD [palette set 220]
	Manage statusBadBD2 145
	Manage statusBadBG [palette set 107]
	Manage statusBadBG2 145
	Manage statusBadBL [palette set 71]
	Manage statusBadBL2 170
	Manage statusBadText [palette set 0]
	Manage statusBadText2 180

	Manage targetBD [palette set 220]
	Manage targetBD2 145
	Manage targetBG [palette set 71]
	Manage targetBG2 145
	Manage targetBL [palette set 35]
	Manage targetBL2 170
	Manage targetText [palette set 0]
	Manage targetText2 180

	Manage target2BD [palette set 253]
	Manage target2BD2 145
	Manage target2BG [palette set 129]
	Manage target2BG2 145
	Manage target2BL [palette set 0]
	Manage target2BL2 145
	Manage target2Text [palette set 0]
	Manage target2Text2 180

	# These are the standard Angband terminal colors mapped to
	# entries in our 256-color palette. They can be accessed via
	# "Value TERM_XXX".

	Manage TERM_DARK [palette set 255]
	Manage TERM_WHITE [palette set 0]
	Manage TERM_SLATE [palette set 250]
	Manage TERM_ORANGE [palette set 17]
	Manage TERM_RED [palette set 217]
if 1 {
	Manage TERM_GREEN [palette set 227]
} else {
	Manage TERM_GREEN [palette set 196]
}
	Manage TERM_BLUE [palette set 204]
	Manage TERM_UMBER [palette set 101]
	Manage TERM_L_DARK [palette set 129]
	Manage TERM_L_WHITE [palette set 247]
	Manage TERM_VIOLET [palette set 30]
	Manage TERM_YELLOW [palette set 5]
	Manage TERM_L_RED [palette set 35]
	Manage TERM_L_GREEN [palette set 185] ; # ~LawnGreen
	Manage TERM_L_BLUE [palette set 180] ; # ~turquoise2
	Manage TERM_L_UMBER [palette set 52]

	# This is a bit silly. The Color Window should allow the
	# user to edit the standard 16 colors, which are then used
	# for displaying items etc. Currently, every item type can
	# have its own color.

	Manage TV_AMULET [Get TERM_ORANGE]
	Manage TV_ARROW [Get TERM_L_UMBER]
	Manage TV_BOLT [Get TERM_L_UMBER]
	Manage TV_BOOTS [Get TERM_L_UMBER]
	Manage TV_BOTTLE [Get TERM_WHITE]
	Manage TV_BOW [Get TERM_UMBER]
	Manage TV_CHEST [Get TERM_SLATE]
	Manage TV_CLOAK [Get TERM_L_UMBER]
	Manage TV_CROWN [Get TERM_L_UMBER]
	Manage TV_DIGGING [Get TERM_SLATE]
	Manage TV_DRAG_ARMOR [Get TERM_SLATE]
	Manage TV_FLASK [Get TERM_YELLOW]
	Manage TV_FOOD [Get TERM_L_UMBER]
	Manage TV_GLOVES [Get TERM_L_UMBER]
	Manage TV_GOLD [Get TERM_ORANGE]
	Manage TV_HAFTED [Get TERM_WHITE]
	Manage TV_HARD_ARMOR [Get TERM_SLATE]
	Manage TV_HELM [Get TERM_L_UMBER]
	Manage TV_JUNK [Get TERM_WHITE]
	Manage TV_LITE [Get TERM_YELLOW]
	Manage TV_NONE [Get TERM_WHITE]
	Manage TV_POLEARM [Get TERM_WHITE]
	Manage TV_POTION [Get TERM_L_BLUE]
	Manage TV_RING [Get TERM_RED]
	Manage TV_ROD [Get TERM_VIOLET]
	Manage TV_SCROLL [Get TERM_WHITE]
	Manage TV_SHIELD [Get TERM_L_UMBER]
	Manage TV_SHOT [Get TERM_L_UMBER]
	Manage TV_SKELETON [Get TERM_WHITE]
	Manage TV_SOFT_ARMOR [Get TERM_SLATE]
	Manage TV_SPIKE [Get TERM_SLATE]
	Manage TV_STAFF [Get TERM_L_UMBER]
	Manage TV_SWORD [Get TERM_WHITE]
	Manage TV_WAND [Get TERM_GREEN]

	if {[variant ANGBANDTK]} {
		Manage TV_MAGIC_BOOK [Get TERM_L_RED]
		Manage TV_PRAYER_BOOK [Get TERM_L_GREEN]
	}
	if {[variant KANGBANDTK]} {
		Manage TV_ILLUSION_BOOK [Get TERM_L_BLUE]
		Manage TV_MAGIC_BOOK [Get TERM_L_RED]
		Manage TV_NATURE_BOOK [Get TERM_L_UMBER]
		Manage TV_PRAYER_BOOK [Get TERM_L_GREEN]
	}
	if {[variant OANGBANDTK]} {
		Manage TV_DRUID_BOOK [Get TERM_L_GREEN]
		Manage TV_MAGIC_BOOK [Get TERM_L_RED]
		Manage TV_NECRO_BOOK [Get TERM_VIOLET]
		Manage TV_PRAYER_BOOK [Get TERM_L_BLUE]
	}
	if {[variant ZANGBANDTK]} {
		Manage TV_FIGURINE [Get TERM_L_WHITE]
		Manage TV_STATUE [Get TERM_L_WHITE]
		Manage TV_CORPSE [Get TERM_L_WHITE]
		Manage TV_ARCANE_BOOK [Get TERM_L_WHITE]
		Manage TV_CHAOS_BOOK [Get TERM_L_RED]
		Manage TV_DEATH_BOOK [Get TERM_L_DARK]
		Manage TV_LIFE_BOOK [Get TERM_WHITE]
		Manage TV_NATURE_BOOK [Get TERM_L_GREEN]
		Manage TV_SORCERY_BOOK [Get TERM_L_BLUE]
		Manage TV_TRUMP_BOOK [Get TERM_ORANGE]
	}

	Manage show_icons 1
	Manage use_sound 1
	Manage ambient_delay 120
	Manage monster_delay 100
	Manage scroll_follow 1
	Manage allow_animation 1
	Manage mouse_repeat_delay 200
	Manage mouse_repeat_interval 100
	Manage mouse_repeat_town 100
	Manage show_cave_balloon 1

	if {[ScreenHeight] >= 1024} { # 1280x1024
		Manage autobar_button_size 28
	} elseif {[ScreenHeight] >= 768} { # 1024x768
		Manage autobar_button_size 24
	} elseif {[ScreenHeight] >= 600} { # 800x600
		Manage autobar_button_size 18
	} else { # 640x480
		Manage autobar_button_size 18
	}

	Manage tip,current 1000
	Manage tip,show 1

	Manage progress,float 0
	Manage progress,layout wide
	Manage progress,showBars 1
	Manage progress,showNumbers 1
	Manage progress,showMax 0
	Manage progress,showHP 1
	Manage progress,showSP 1
	Manage progress,showFD 1
	Manage progress,showWT 1

	Manage recall,show 1
	Manage recall,autoexpand 1
	Manage recall,showicon 1
	Manage recall,newstyle 0

	Manage record,dump 1
	Manage record,message 1
	Manage record,photo 1

	if {[ScreenHeight] >= 1024} { # 1280x1024
		set scale 6
	} elseif {[ScreenHeight] >= 768} { # 1024x768
		set scale 6
	} elseif {[ScreenHeight] >= 600} { # 800x600
		set scale 4
	} else { # 640x480
		set scale 4
	}
	Manage bigmap,scale $scale
	Manage micromap,scale $scale
	Manage micromap,float 1
	Manage map,detail high

	Manage main,statusbar,color White ; # #80FFFF
	Manage message,float 0

	Manage misc,mode,exp 0 ; # 0:ADV 1:EXP
	Manage misc,mode,armor_class 1
#	Manage misc,textLabels [expr {[ScreenWidth .] < 800}]
	Manage misc,textLabels 1
	Manage misc,layout wide
	Manage misc,float 0

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		Manage messages,combine 1
	}
if 0 {
	Manage messages,max 256
}
	Manage settings,showUnused 0

	Manage sound,wants 0
	Manage sound,dll nocard
	Manage sound,volume 50

	# Path elements of most-recently-used savefile
	Manage savefile {}

	# Icon configuration
	Manage config,prefix dg32

	Manage choicewindow,show 1
	Manage choicewindow,autoexpand 1
	Manage choicewindow,showicon 1

	Manage inventory,alwaysOnTop 1
	Manage inventory,style new

	Manage store,style new

	Manage message2window,show 1

	Manage music,wants 0
	Manage music,dll ""
	Manage music,play 0
	Manage music,volume 100
	Manage music,mute 0

	if {[Platform windows] && ("Arial" in [font families])} {
		set asciiFont Arial
		set size 24 ; # font actual "Arial -24" give point size 18
	} else {
		set asciiFont Courier
		set size 24
	}
	set iconHeight 32
	while {[font metrics "$asciiFont -$size" -linespace] > $iconHeight} {
		incr size -1
	}
	Manage font,ascii "$asciiFont -$size"

	Manage ascii,width 0
	Manage ascii,height 0
	Manage ascii,square 0
	Manage ascii,character,bg 252
	Manage ascii,feature,bg 252
	Manage ascii,feature1,bg 253
	Manage ascii,feature2,bg 255
	Manage ascii,monster,bg 222
	Manage ascii,uniques,bg 221
	Manage ascii,friend,bg 212
	Manage ascii,object,bg 232

	Manage font,system [Global font,sys]
	Manage font,fixed [Global font,fixed]

	Manage font,autobar [Global font,sys]
	Manage font,choice [Global font,sys]
	Manage font,equippy [Global font,fixed]
	Manage font,inventory [Global font,sys]
	Manage font,knowledge [Global font,sys]
	Manage font,macros [Global font,fixed]
	Manage font,magic [Global font,sys]
	Manage font,message [Global font,sys]
	Manage font,messages [Global font,sys]
	Manage font,misc [Global font,sys]
	Manage font,miscPopup [Global font,sys]
	Manage font,monster [Global font,sys]
	Manage font,recall [Global font,sys]
	Manage font,options [Global font,sys]
	Manage font,status [Global font,sys]
	Manage font,statusBar [Global font,sys]
	Manage font,store [Global font,sys]

	# If this is 1, then artifacts in the dungeon are listed in
	# the Knowledge Window.
	Manage know_unseen_artifacts 0

	if {[variant ANGBANDTK]} {
		Manage borg,prefix ""
	}

	### One-time warnings to the user.

	# Show Setup Window first time
	Manage warning,setup 0

	# Music is not available
	Manage warning,music 0

	# Artifacts are not listed in Knowledge anymore
#	Manage warning,artifacts 0

	# New Inventory Window
#	Manage warning,inventory,window 0
#	Manage warning,store,window 0

#	Manage options,autosave 1
	Manage window,autosave 1

	variable Write
	set Write(music,directory) "NSValueManager::Write"
	set Write(savefile) "NSValueManager::Write"
	variable Read
	set Read(music,directory) "NSValueManager::Read"
	set Read(savefile) "NSValueManager::Read"

	if {$::DEBUG} {

		# Verify we have all the tval's
		foreach tval [angband info tval] {
			if {![info exists Value($tval)]} {
				error "forgot to Manage $tval"
			}
		}
	}

	set Priv(init) 0

	# Note: default lib should come *last*
	if {[Platform windows]} {
		set libs [list wavemix directsound bass-stream bass]
	}
	if {[Platform unix]} {
		set libs [list sdl openal]
	}
	foreach lib $libs {
		set shlib sound-$lib[info sharedlibextension]
		if {[file exists [CPath lib $shlib]]} {
			Manage sound,wants 1
			Manage sound,dll $lib
		}
	}

	# Note: default lib should come *last*
	if {[Platform windows]} {
		set libs [list midas bass fmod]
	}
	if {[Platform unix]} {
		set libs {}
	}
	foreach lib $libs {
		set shlib music-$lib[info sharedlibextension]
		if {[file exists [CPath lib $shlib]]} {
			Manage music,wants 1
			Manage music,dll $lib
		}
	}

	LoadValueFile

	return
}

# NSValueManager::CloseModule --
#
#	Called before the game exits. Dumps all values to the tk/config/value
#	file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::CloseModule {} {

	global Angband
	variable Value
	variable Write

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"value\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	foreach name [lsort [array names Value]] {
		if {[info exists Write($name)]} {
			set value [eval $Write($name) $name]
		} else {
			set value [Get $name]
		}
		puts $fileId [list Manage $name $value]
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config value]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# NSValueManager::LoadValueFile --
#
#	Read tk\config\value.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::LoadValueFile {} {

	variable Value
	variable Read

	set fileName [PathTk config value]
	if {[file exists $fileName]} {
		source $fileName
	}

	foreach name [array names Read] {
		if {[info exists Value($name)]} {
			eval $Read($name) $name
		}
	}

	return
}

proc NSValueManager::ExpandValue {char object event detail charMap} {

	if {$detail ne ""} {
		set pattern <$event-$detail>
	} else {
		set pattern <$event>
	}
	switch -- $char {
		c {
			return [Get $detail]
		}
		d { return $detail }
		e { return $event }
		P { return $pattern }
		W { return $object }
		? {
			array set map $charMap
			array set map [list W $object P $pattern e $event d $detail c [Get $detail]]
			return [array get map]
		}
		default {
			array set map [list $char $char]
			array set map $charMap
			return $map($char)
		}
	}
}

# NSValueManager::Manage --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Manage {name value} {

	variable Priv
	variable Value

	if {$Priv(init)} {
		qeinstall <Value-$name>
	}

	set Value($name) $value

	return
}

# NSValueManager::Set --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Set {name value} {

	variable Value

	set Value($name) $value
	Changed $name

	return
}

# NSValueManager::Get --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Get {name} {

	variable Value

	return $Value($name)
}

# NSValueManager::Changed --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Changed {name} {

	variable Priv

	qegenerate <Value-$name>

	return
}

# NSValueManager::Write --
#
#	Special handling of some values.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Write {name} {

	# Instead of writing "Manage savefile {C:/ AngbandTk lib save SaveFile}"
	# we will write "Manage savefile {Path lib save SaveFile}"
	# so the user can move the game directory without trouble, and
	# make UpgradeTool's job easier.

	set value [Get $name]
	if {![string length $value]} {
		return $value
	}

	switch -- $name {
		music,directory -
		savefile {
			set path [file join {*}$value]
			if {[IsFileInPath $path]} {
				set list [StripCommon $path [Path]]
				return [concat Path $list]
			} elseif {[IsFileInCPath $path]} {
				set list [StripCommon $path [CPath]]
				return [concat CPath $list]
			}
		}
	}

#	return [list $value]
	return $value
}

# NSValueManager::Read --
#
#	Special handling of some values.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Read {name} {

	set value [Get $name]
	if {![string length $value]} {
		return
	}

	switch -- $name {
		music,directory -
		savefile {
			if {[lindex $value 0] eq "Path"} {
				Manage $name [file split [eval $value]]
			} elseif {[lindex $value 0] eq "CPath"} {
				Manage $name [file split [eval $value]]
			}
		}
	}

	return
}

# ColorFromValue --
#
#	Return the palette RGB color for the value with the given name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ColorFromValue {valueName} {

	set index [NSValueManager::Get $valueName]
	return [palette set $index]
}

# Value --
#
#	Convenience interface to NSValueManager::Get/Set
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Value {name args} {

	# Set info
	if {[llength $args]} {
		NSValueManager::Set $name [lindex $args 0]

	# Get info
	} else {
		return [NSValueManager::Get $name]
	}

	return
}

# ListBackgroundChanged --
#
#	Configures the background color of the given canvas to the
#	color of the "listBG" value.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ListBackgroundChanged {canvas} {

	$canvas configure -background [Value listBG]

	return
}

# default_tval_to_attr --
#
#	Returns the color associated with the given object tval.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc default_tval_to_attr {tval} {

	return [Value $tval]
}

# setg --
#
#	A namespace with commands called when the tk/config/setting file
#	is sourced.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

namespace eval setg {

proc one {keyword value} {

	# Never set cheating options
	if {[string match cheat* $keyword]} return

	# Ignore errors, such as outdated settings
	catch {
		Setting $keyword $value
	}

	return
}

# namespace eval setg
}

# Setting --
#
#	A namespace with commands called when the tk/config/setting file
#	is sourced.
#

namespace eval Config::Setting {

# namespace eval Config::Setting
}

proc Config::Setting::Source {path} {

	source $path

	return
}

proc Config::Setting::One {keyword value} {

	# Never set cheating options
	if {[string match cheat* $keyword]} return

	# Ignore errors, such as outdated settings
	catch {
		Setting $keyword $value
	}

	return
}

# ExpandSetting --
#
#	When a setting is changed in Angband (possibly by the "setting" command,
#	or by reading a preference file) a <Setting> quasi-event is generated.
#	Since some of our settings are unknown to Angband (they exist in Tcl code
#	only, such as show_icons) we use qeinstall for those settings so that
#	scripts can use qebind in a consistent manner to know when any setting
#	changes.
#
#	ExpandSetting is called to perform %-substitution in a qebind script.

proc ExpandSetting {char object event detail charMap} {

	if {$detail ne ""} {
		set pattern <$event-$detail>
	} else {
		set pattern <$event>
	}
	switch -- $char {
		c {
			return [Setting $detail]
		}
		d { return $detail }
		e { return $event }
		P { return $pattern }
		W { return $object }
		? {
			array set map $charMap
			array set map [list W $object P $pattern e $event d $detail c [Setting $detail]]
			return [array get map]
		}
		default {
			array set map [list $char $char]
			array set map $charMap
			return $map($char)
		}
	}
}

# Allow scripts to bind to these "fake" settings.
qeinstall <Setting-ambient_delay> ExpandSetting
qeinstall <Setting-monster_delay> ExpandSetting
qeinstall <Setting-scroll_follow> ExpandSetting
qeinstall <Setting-show_icons> ExpandSetting
qeinstall <Setting-mouse_repeat_delay> ExpandSetting
qeinstall <Setting-mouse_repeat_interval> ExpandSetting
qeinstall <Setting-mouse_repeat_town> ExpandSetting
qeinstall <Setting-autobar_button_size> ExpandSetting
qeinstall <Setting-show_cave_balloon> ExpandSetting

# Hack -- ZAngband uses plain_descriptions
if {[variant ZANGBANDTK]} {
	qebind ZAngband <Setting-plain_descriptions> "qegenerate <Setting-show_flavors>"
	qeinstall <Setting-show_flavors> ExpandSetting
}

# Setting --
#
#	Get or set the value of a setting. This one routine consolidates
#	all the different setting-related commands.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Setting {keyword args} {

	# Set
	if {[llength $args]} {
		set value [lindex $args 0]
		if {[variant ZANGBANDTK]} {
			# Hack -- Map show_flavors to plain_descriptions (reverse logic)
			switch -- $keyword {
				show_flavors {
					set keyword plain_descriptions
					set value [expr {$value ? 0 : 1}]
					angband setting set $keyword $value
#					qegenerate <Setting-show_flavors>
					return
				}
			}
		}
		switch -- $keyword {
			ambient_delay -
			monster_delay {
				angband sound setting $keyword $value
				Value $keyword $value
				qegenerate <Setting-$keyword>
			}
			scroll_follow -
			show_cave_balloon -
			show_icons {
				Value $keyword [NSUtils::GetBoolean $value]
				qegenerate <Setting-$keyword>
			}
			mouse_repeat_delay -
			mouse_repeat_interval -
			mouse_repeat_town {
				Value $keyword [NSUtils::GetInteger $value]
				qegenerate <Setting-$keyword>
			}
			autobar_button_size {
				Value $keyword [NSUtils::GetInteger $value]
				qegenerate <Setting-$keyword>
			}
			default {
				angband setting set $keyword $value
			}
		}

	# Get
	} else {
		if {[variant ZANGBANDTK]} {
			# Hack -- Map show_flavors to plain_descriptions (reverse logic)
			switch -- $keyword {
				show_flavors {
					set value [angband setting set plain_descriptions]
					return [expr {$value ? 0 : 1}]
				}
			}
		}
		switch -- $keyword {
			ambient_delay -
			monster_delay {
				return [angband sound setting $keyword]
			}
			scroll_follow -
			show_cave_balloon -
			show_icons -
			mouse_repeat_delay -
			mouse_repeat_interval -
			mouse_repeat_town -
			autobar_button_size {
				return [Value $keyword]
			}
			default {
				return [angband setting set $keyword]
			}
		}
	}

	return
}

# SettingDesc --
#
#	Return the human-readable description for a setting.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc SettingDesc {keyword} {

	if {[variant ZANGBANDTK]} {
		# Hack -- Map show_flavors to plain_descriptions
		switch -- $keyword {
			show_flavors {
				return "Show flavors in object descriptions"
			}
		}
	}

	switch -- $keyword {
		ambient_delay {
			return "Ambient sound delay"
		}
		monster_delay {
			return "Monster sound delay"
		}
		scroll_follow {
			return "Keep character centered in the display"
		}
		show_cave_balloon {
			return "Show tooltip describing what is under the mouse"
		}
		show_icons {
			return "Show icons in inventory/store list"
		}
		mouse_repeat_delay {
			return "Mouse repeat delay"
		}
		mouse_repeat_interval {
			return "Mouse repeat interval"
		}
		mouse_repeat_town {
			return "Mouse repeat interval on level 0"
		}
		autobar_button_size {
			return "Size of the autobar buttons"
		}
		default {
			return [angband setting desc $keyword]
		}
	}
}


# First element is the title of this group of artifacts/monsters/objects.
# Second element is list of arguments to pass to the "a_info/r_info/k_info find"
# command to display artifacts/monsters/objects in the group.

set data [list \
	"Sword" "-tval TV_SWORD" \
	"Polearm" "-tval TV_POLEARM" \
	"Hafted" "-tval TV_HAFTED" \
	"Bow" "-tval TV_BOW" \
	"Ammunition" "-tval TV_ARROW -tval TV_BOLT -tval TV_SHOT" \
	"Shield" "-tval TV_SHIELD" \
	"Crown" "-tval TV_CROWN" \
	"Helm" "-tval TV_HELM" \
	"Gloves" "-tval TV_GLOVES" \
	"Boots" "-tval TV_BOOTS" \
	"Cloak" "-tval TV_CLOAK" \
	"Dragon Scale Mail" "-tval TV_DRAG_ARMOR" \
	"Hard Armor" "-tval TV_HARD_ARMOR" \
	"Soft Armor" "-tval TV_SOFT_ARMOR" \
	"Ring" "-tval TV_RING" \
	"Amulet" "-tval TV_AMULET" \
	"Light Source" "-tval TV_FLASK -tval TV_LITE" \
	"Potion" "-tval TV_POTION" \
	"Scroll" "-tval TV_SCROLL" \
	"Wand" "-tval TV_WAND" \
	"Staff" "-tval TV_STAFF" \
	"Rod" "-tval TV_ROD" \
]
if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
	lappend data \
		"Prayer Book" "-tval TV_PRAYER_BOOK" \
		"Magic Book" "-tval TV_MAGIC_BOOK" 
}
if {[variant KANGBANDTK]} {
	lappend data \
		"Illusion Book" "-tval TV_ILLUSION_BOOK" \
		"Nature Book" "-tval TV_NATURE_BOOK"
}
if {[variant OANGBANDTK]} {
	lappend data \
		"Druid Book" "-tval TV_DRUID_BOOK" \
		"Necro Book" "-tval TV_NECRO_BOOK"
}
if {[variant ZANGBANDTK]} {
	lappend data \
		"Life Book" "-tval TV_LIFE_BOOK" \
		"Sorcery Book" "-tval TV_SORCERY_BOOK" \
		"Nature Book" "-tval TV_NATURE_BOOK" \
		"Chaos Book" "-tval TV_CHAOS_BOOK" \
		"Death Book" "-tval TV_DEATH_BOOK" \
		"Trump Book" "-tval TV_TRUMP_BOOK" \
		"Arcane Book" "-tval TV_ARCANE_BOOK" \
		"Figurine" "-tval TV_FIGURINE" \
		"Statue" "-tval TV_STATUE" \
		"Corpse" "-tval TV_CORPSE" \
}
lappend data \
	"Spikes" "-tval TV_SPIKE" \
	"Digging Tool" "-tval TV_DIGGING" \
	"Chest" "-tval TV_CHEST" \
	"Food" "-tval TV_FOOD" \
	"Gold" "-tval TV_GOLD" \
	"Skeleton" "-tval TV_SKELETON" \
	"Junk" "-tval TV_BOTTLE -tval TV_JUNK"
Global groups,k_info $data

# Artifact groups are the same as object groups, because of
# random artifacts.
Global groups,a_info $data

set data {}
lappend data \
	"Ancient Dragons" "-d_char D"
if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
	lappend data \
		"Angelic Beings" "-d_char A" \
}
lappend data \
	"Birds" "-d_char B" \
	"Canines" "-d_char C" \
	"Creeping Coins" "-d_char $" \
	"Demihumans" "-d_char h" \
	"Dragons" "-d_char d" \
	"Elementals" "-d_char E"
if {[variant OANGBANDTK ZANGBANDTK]} {
	lappend data \
		"Energy Ball" "-d_char *"
}
lappend data \
	"Eyes/Beholders" "-d_char e" \
	"Felines" "-d_char f" \
	"Ghosts" "-d_char G" \
	"Giant Ants" "-d_char a" \
	"Giant Bats" "-d_char b" \
	"Giant Centipedes" "-d_char c" \
	"Giant Dragon Flies" "-d_char F"
if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
	lappend data \
		"Giant Lice" "-d_char l"
}
lappend data \
	"Giants" "-d_char P" \
	"Golems" "-d_char g" \
	"Humans" "-d_char p" \
	"Hybrids" "-d_char H"
if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
	lappend data \
		"Hydras" "-d_char M"
}
lappend data \
	"Icky Things" "-d_char i" \
	"Jellies" "-d_char j" \
	"Killer Beetles" "-d_char K" \
	"Kobolds" "-d_char k" \
	"Lichs" "-d_char L" \
	"Major Demons" "-d_char U" \
	"Minor Demons" "-d_char u" \
	"Molds" "-d_char m" \
	"Multiplying Insects" "-d_char I"
if {[variant OANGBANDTK]} {
	lappend data \
		"Mummies" "-d_char M"
}
lappend data \
	"Mushroom Patches" "-d_char ," \
	"Nagas" "-d_char n" \
	"Ogres" "-d_char O" \
	"Orcs" "-d_char o" \
	"Quadropeds" "-d_char q" \
	"Quylthulgs" "-d_char Q" \
	"Reptiles/Amphibians" "-d_char R" \
	"Rodents" "-d_char r" \
	"Scorpions/Spiders" "-d_char S" \
	"Skeletons" "-d_char s" \
	"Snakes" "-d_char J" \
	"Townspeople" "-d_char t"
if {[variant ANGBANDTK KANGBANDTK]} {
	lappend data \
		"Tricksters" "-d_char !?=."
}
if {[variant OANGBANDTK]} {
	lappend data \
		"Tricksters" "-d_char !?=.#|~"
}
if {[variant ZANGBANDTK]} {
	lappend data \
		"Tricksters" "-d_char !?=.#|>(+&"
}
lappend data \
	"Trolls" "-d_char T" \
	"Vampires" "-d_char V" \
	"Vortices" "-d_char v"
if {[variant ZANGBANDTK]} {
	lappend data \
		"Water Dwellers" "-d_char ~"
}
lappend data \
	"Wights/Wraiths" "-d_char W" \
	"Worms/Worm Masses" "-d_char w" \
	"Xorns/Xarens" "-d_char X" \
	"Yeeks" "-d_char y" \
	"Yeti" "-d_char Y" \
	"Zephyr Hounds" "-d_char Z" \
	"Zombies/Mummies" "-d_char z" \
	"Uniques" "-unique yes"
Global groups,r_info $data
unset data

# NSValueManager::Verify --
#
#	Check that all the objects, monsters, and artifacts are accounted
#	for in the groups,x_info lists.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Verify {} {

	foreach info {a_info k_info r_info} {
		foreach idx [angband $info find] {
			set match($idx) 0
		}
		foreach {label findSpec} [Global groups,$info] {
			if {$info eq "r_info"} {
				set findSpec "-unique no $findSpec"
			}
			foreach idx [angband $info find {*}$findSpec] {
				if {$match($idx)} {
					dbwin "$info #$idx in more that one group\n"
				}
				set match($idx) 1
			}
		}
		foreach idx [angband $info find] {
			if {$idx && !$match($idx)} {
				dbwin "$info #$idx not in any group\n"
			}
		}
	}

	return
}

proc DumpValueManager {} {

	set win .dumpvalue
	toplevel $win
	text $win.text
	pack $win.text

	foreach name [lsort [array names NSValueManager::Value]] {
		set value [NSValueManager::Get $name]
		$win.text insert end "Manage $name $value\n"
	}

	return
}

