# File: birth.tcl

# Purpose: the Birth Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBirth {

variable Priv
variable Detail

MsgCatInit player charflags birth

#
# Descriptions of races
#

foreach race [angband info race_name] {
	regsub -all " " $race * race
	set Detail($race) [mc D_$race]
}

#
# Descriptions of classes
#

foreach class [angband info class_name] {
	regsub -all " " $class * class
	set Detail($class) [mc D_$class]
}

if {[variant ZANGBANDTK]} {

#
# Description of magic realms
#

set Detail(Life) [mc D_Life]
set Detail(Sorcery) [mc D_Sorcery]
set Detail(Arcane) [mc D_Arcane]
set Detail(Trump) [mc D_Trump]
set Detail(Nature) [mc D_Nature]
set Detail(Chaos) [mc D_Chaos]
set Detail(Death) [mc D_Death]

# ZANGBANDTK
}

# namespace eval NSBirth
}

# NSBirth::InitModule --
#
#	Called by angband_birth() to kick off character creation.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitModule {} {

	global Angband
	variable Priv

	package require Tkhtml [TKHTMLVERSION]

	NSModule::AddModuleIfNeeded NSList [CPathTk stdlist.tcl]
	NSModule::LoadIfNeeded NSList

	if {[variant KANGBANDTK]} {
		# Scan the actual plotNN.txt files for a list of quests
		ReadPlots
	}

	set oop [NSObject::New NSBirth]

	# Create a player-info window...
	NSModule::AddModule NSPlayer [CPathTk player.tcl]
	NSModule::LoadIfNeeded NSPlayer
	set playerId [Global player,oop]

	# Hack -- Get player-info window
	set win [NSPlayer::Info $playerId win]
	set Priv(playerId) $playerId
	set Priv(canvas) [NSPlayer::Info $playerId canvas]

	# Life Rating
	NSPlayer::AddTextItem $playerId 26 1 -1 Red "" left life

	# ...but don't feed the Term when keys are pressed
	bind $win <KeyPress> {}

	# Never allow change of name during creation
	bind $win <c> {}

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSBirth::Close $oop"

	# Window to show progress during autoroller
	InitAutoProgressWindow

	# Put the startup window on front again, and then position the
	# Birth Window and make it the front window.
	raise . ; focus .
	set win [NSBirth::Info $oop win]
	WindowPosition $win 2 3
	WindowBringToFront $win

	# Read a dead-character's save file
	if {[info exists ::AngbandPriv(load,win)]} {
		wm withdraw $::AngbandPriv(load,win)

	# Creating a new character
	} else {
		wm withdraw .
	}

	# The Birth Options Window will be loaded if needed
	NSModule::AddModule NSBirthOptions [CPathTk birth-options.tcl]

	return
}

# NSBirth::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::CloseModule {} {

	variable Priv

	catch {
		destroy [Info $Priv(oop) win]
	}

	return
}

# NSBirth::NSBirth --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::NSBirth {oop} {

	variable Priv

	set Priv(oop) $oop

	Info $oop gender [angband info gender_name 0]
	Info $oop race [angband info race_name 0]
	Info $oop class [angband info class_name 0]
	Info $oop autoroll 0
	Info $oop pointbased 0
	Info $oop maximize 0
	Info $oop preserve 0
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		Info $oop autoroll [Setting birth_auto_roller]
		Info $oop pointbased [Setting birth_point_based]
		Info $oop maximize [Setting birth_maximize]
		Info $oop preserve [Setting birth_preserve]
		foreach stat [angband info stat_name] {
			Info $oop points,$stat 10
		}
	}
	if {[variant KANGBANDTK]} {
		Info $oop plot 1
	}
	if {[variant ZANGBANDTK]} {
		Info $oop realm1 None
		Info $oop realm2 None
		Info $oop max_quest 20
		Info $oop hard_quests 0
		Info $oop request,realm1 ""
		Info $oop request,realm2 ""
		Info $oop autoroll [Setting autoroller]
		Info $oop pointbased [Setting point_based]
		Info $oop maximize [Setting maximize_mode]
		Info $oop preserve [Setting preserve_mode]
		foreach stat [angband info stat_name] {
			Info $oop points,$stat 10
		}
	}
	foreach stat [angband info stat_name] {
		Info $oop stat_limit,$stat ""
	}

	# No character has been created yet
	Info $oop hasRoll 0

	# Now read the user's desired default settings
	ReadSettings $oop

	InitWindow $oop

	set win [Info $oop win]

	if {[Platform unix]} {
		wm geometry $win +0+0
		update
	}
	wm geometry $win +[OffscreenOffsetX]+0

	# Destroy the object along with the window (later)
	NSUtils::DestroyObjectWithWidget NSBirth $oop $win

	set minx [set miny 0]
	if {[variant ANGBANDTK]} {
		set screenList [list Gender Race Class AutoRoll Points]
	}
	if {[variant OANGBANDTK]} {
		set screenList [list Gender Race Class AutoRoll Points]
	}
	if {[variant KANGBANDTK]} {
		set screenList [list Gender Race Class Plot AutoRoll Points]
	}
	if {[variant ZANGBANDTK]} {
		set screenList [list Gender Race Class Realm1 Realm2 AutoRoll Points]
	}

	# Get busy
	Info $oop busy 0
	Busy $oop 1

	# Create the list and text field
	InitList $oop

	# All I want to do is create the subwindows, not set lists
	# or anything.
	Info $oop createOnly 1

	foreach screen $screenList {
		Info $oop screen $screen
		BirthHack $oop
		InitScreen_$screen $oop
		update
		set minx [expr {($minx < [winfo width $win]) ? [winfo width $win] : $minx}]
		set miny [expr {($miny < [winfo height $win]) ? [winfo height $win] : $miny}]
		pack forget {*}[winfo children $win.content]
	}
	$win configure -width $minx -height $miny
	grid propagate $win no

	Info $oop createOnly 0

	Info $oop screen [lindex $screenList 0]
	InitScreen_Gender $oop

	# On the first screen, so user can't go back
	$win.buttons.back configure -state disabled

	# Not busy
	Busy $oop 0

	return
}

# NSBirth::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Info {oop info args} {

	global NSBirth

	# Verify the object
	NSObject::CheckObject NSBirth $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSBirth($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSBirth($oop,$info)
			}
		}
	}

	return
}

# NSBirth::InitWindow --
#
#	Initialize the Wizard-like window for character creation. The
#	window has a prompt, an info label, a varying content area, and
#	some buttons at the bottom.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitWindow {oop} {

	set win .birth$oop
	toplevel $win
	wm title $win [mc "Character Creation"]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSBirth::Close $oop"

	Info $oop win $win

	if {[Platform unix]} {
		set font "Helvetica 14 bold underline"
	}
	if {[Platform windows]} {
		set font [mc font,prompt]
	}
	set label $win.prompt
	label $label \
		-font $font

	set label $win.info
	message $label \
		-width 350

	set frame $win.content
	frame $frame \
		-borderwidth 0

	MakeDivider $win.divider1 x

	set frame $win.buttons
	frame $frame \
		-borderwidth 0
	button $frame.option \
		-command "NSBirth::Options $oop" -text [mc "Options"] -underline 0 \
		-width 9
	button $frame.back \
		-command "NSBirth::Back $oop" -text [mc "< Back"] -underline 2 \
		-width 9
	button $frame.next \
		-command "NSBirth::Next $oop" -text [mc "Next >"] -underline 0 \
		-width 9
	button $frame.cancel \
		-command "NSBirth::Close $oop" -text [mc "Quit"] -underline 0 \
		-width 9

	foreach button [winfo children $frame] {
		NSUtils::CheckButtonWidth $button
	}

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 0
	grid rowconfigure $win 2 -weight 1
	grid rowconfigure $win 3 -weight 0
	grid rowconfigure $win 4 -weight 0
	grid columnconfigure $win 0 -weight 1

	grid $win.prompt \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky w -padx 5 -pady 5
	grid $win.info \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky w -padx 10 -pady 5
	grid $win.content \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news -padx 10 -pady 5
	grid $win.divider1 \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew -padx 10
	grid $win.buttons \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky ew -padx 5 -pady 5

	set frame $win.buttons
	pack $frame.cancel \
		-side right -pady 5 -padx 5
	pack $frame.next \
		-side right -pady 5 -padx 5
	pack $frame.back \
		-side right -pady 5 -padx 5
	pack $frame.option \
		-side left -pady 5 -padx 5

	NSUtils::SetDefaultButton $win $win.buttons.next

	# The "break" command is added to the KeyPress bindings below
	# so the Entry widgets in the auto-roller screen do not see
	# any of these keypresses. See the bindtags command in
	# InitScreen_AutoRoll.
	bind $win <KeyPress-o> "NSUtils::InvokeButton $frame.option ; break"
	bind $win <KeyPress-equal> "NSUtils::InvokeButton $frame.option ; break"

	bind $win <KeyPress-greater> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeButton $frame.next
		}
		break
	"
	bind $win <KeyPress-Right> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeButton $frame.next
		}
		break
	"
	bind $win <KeyPress-n> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeButton $frame.next
		}
		break
	"

	bind $win <KeyPress-less> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeButton $frame.back
		}
		break
	"
	bind $win <KeyPress-Left> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeButton $frame.back
		}
		break
	"
	bind $win <KeyPress-b> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeButton $frame.back
		}
		break
	"

	bind $win <KeyPress-q> "NSUtils::InvokeButton $frame.cancel ; break"
	bind $win <KeyPress-Escape> "NSUtils::InvokeButton $frame.cancel ; break"
	bind $win <KeyPress-Return> "
		if {!\[NSBirth::Info $oop busy]} {
			NSUtils::InvokeDefaultButton $win
		}
		break
	"

	return
}

# NSBirth::Close --
#
#	Quit the game when one of the windows is closed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Close {oop} {

	# Save the current options
	WriteSettings $oop

	# Buh-bye
	angband game abort

	return
}

# NSBirth::InitAutoProgressWindow --
#
#	Initializes the window used to display feedback during the autoroller.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitAutoProgressWindow {} {

	variable Priv

	set playerId $Priv(playerId)

	set win .autoprogress
	toplevel $win
	wm title $win [mc "Autoroller"]
	wm withdraw $win
	TransientToWin $win [NSPlayer::Info $playerId win]

	label $win.prompt \
		-text [mc "> Hit any key to abort <"] -font {Times 12 bold}
	pack $win.prompt \
		-side top -padx 5 -pady 5

	MakeDivider $win.divider1 x
	pack $win.divider1 \
		-side top -fill x

	set data {
		"    Strength:" strength
		"Intelligence:" intelligence
		"      Wisdom:" wisdom
		"   Dexterity:" dexterity
		"Constitution:" constitution
		"    Charisma:" charisma
	}

	set frame $win.stats
	frame $frame \
		-borderwidth 0

	set font [Value font,fixed]

	foreach {title stat} $data {

		set frame $win.stats.$stat
		set name $frame.name
		set value $frame.value
		set match $frame.match

		frame $frame \
			-borderwidth 0
		label $name \
			-text [mc $title] -font $font -borderwidth 0
#		label $value \
#			-width 6 -font [BoldFont $font] -anchor e -borderwidth 0
		label $match \
			-width 6 -font [FontAddStyle $font italic] -anchor e -borderwidth 0

		pack $frame \
			-side top -expand yes -fill x -anchor w
		pack $name \
			-side left -expand no
#		pack $value \
#			-side left -expand no -padx 4
		pack $match \
			-side left -expand no
	}

	pack $win.stats \
		-side top -anchor c -padx 30 -pady 5

	MakeDivider $win.divider2 x
	pack $win.divider2 \
		-side top -fill x

	label $win.autoround \
		-font $font -text [mc "Round: %d"]
	pack $win.autoround \
		-side top -pady 5 -anchor c

	# Binding to interupt autoroller
	bind $win <KeyPress> {
		set NSBirth::Priv(userAbort) 1
	}

	return
}

# NSBirth::InitScreen_Gender --
#
#	Creates the display in the Birth Window to let the user choose
#	a gender.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Gender {oop} {

	set win [Info $oop win]

	$win.prompt configure -text [mc "Choose your character's gender"]
	$win.info configure -text [mc D_Gender]

	set frame $win.content.frameGender

	if {[Info $oop createOnly]} {
		frame $frame \
			-borderwidth 0
		pack $frame
		set radioList {}
		foreach gender [angband info gender_name] {
			set name [string tolower $gender]
			radiobutton $frame.$name \
				-text [mc $gender] -variable ::NSBirth($oop,gender) -value $gender
			pack $frame.$name \
				-side top -padx 55 -anchor w

			bind $frame.$name <ButtonPress-1> \
				{focus %W}
			bind $frame.$name <Double-Button-1> \
				"after idle NSUtils::InvokeButton $win.buttons.next"

			lappend radioList $frame.$name
		}

		return
	}

	pack $frame \
		-anchor nw

	set radioList {}
	foreach gender [angband info gender_name] {
		set name [string tolower $gender]
		lappend radioList $frame.$name
	}

	set index 0
	set count [llength $radioList]
	foreach radio $radioList {

		# Up-arrow selects previous radiobutton
		if {$index} {
			set prev [lindex $radioList [expr {$index - 1}]]
		} else {
			set prev [lindex $radioList end]
		}
		bind $radio <KeyPress-Up> \
			"focus $prev ; if {$::UseTile} { $prev invoke } else { $prev select }"

		# Down-arrow selects next radiobutton
		if {$index < $count - 1} {
			set next [lindex $radioList [expr {$index + 1}]]
		} else {
			set next [lindex $radioList 0]
		}
		bind $radio <KeyPress-Down> \
			"focus $next  ; if {$::UseTile} { $next invoke } else { $next select }"

		incr index
	}

	focus $frame.[string tolower [Info $oop gender]]

	return 0
}

# NSBirth::InitList --
#
#	Creates a list (an NSList) and scrolling text field (a Canvas text
#	item, because the scroll behaviour of the Text widget is broken).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitList {oop} {

	set win [Info $oop win]

	set font [Value font,knowledge]
	set cw [font measure $font "0"]
	set rowHgt [expr {[font metrics $font -linespace] + 8}]
	set width [expr {$cw * 20}]
	set height [expr {$rowHgt * 8}]

	#
	# List
	#

	set frame $win.content.frameList
	set tree [NSList::New $frame]
	$tree configure -width $width -height $height

	NSList::OnSelection $tree \
		"NSBirth::SelectionChanged $oop %T %c %S %D"

	NSList::OnInvoke $tree \
		"NSBirth::Invoke $oop %T %I %r"

	Info $oop tree $tree

	set frame $win.content.frameText
	frame $frame \
		-borderwidth 1 -relief sunken

	set rowHgt [font metrics $font -linespace]
	set width [expr {$cw * 40}]
	set height [expr {$rowHgt * 8}]

	array set fontdesc [fontdesc $font]

	set text $frame.text
if {[TKHTMLVERSION 1]} {
	html $frame.text \
		-background [Value listBG] -foreground White \
		-width $width -height $height \
		-borderwidth 0 -highlightthickness 0 -takefocus 1 \
		-fontfamily [list $fontdesc(-family)] -fontadjust 0 \
		-fontcommand "NSBirth::HtmlFontCmd $oop" \
		-yscrollcommand "$frame.yscroll set"
}
if {[TKHTMLVERSION 3]} {
	html $frame.text \
		-width $width -height $height \
		-yscrollcommand "$frame.yscroll set"

	bind $frame.text <MouseWheel> {
		%W yview scroll [expr {- (%D / 120) * 4}] units
	}
}
	scrollbar $frame.yscroll \
		-orient vertical -command "$frame.text yview"

	# Focus so we can use the mousewheel
	bind $frame.text <ButtonPress-1> "focus $frame.text"
	bind $frame.yscroll <ButtonPress-1> "focus $frame.text"

if {[TKHTMLVERSION 3]} {
	NSUtils::SynchScrollBar $frame.text $frame.yscroll
}
	set frame $win.content.frameText
	pack $text \
		-side left -expand yes -fill both
	pack $frame.yscroll \
		-side left -fill y

	return
}

# NSBirth::SetupList --
#
#	Prepare the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::SetupList {oop} {

	set win [Info $oop win]
	set tree [Info $oop tree]

	set frame $win.content.frameList
	pack $frame \
		-side left -padx {0 5} -pady 5 -fill y

	set frame $win.content.frameText
	pack $frame \
		-side left -pady 5 -expand yes -fill both

	# Clear the list info
	Info $oop desc {}
	Info $oop detail {}
	Info $oop value {}

	# Clear the list
	NSList::Clear $tree

	# Hack -- Focus on the list, so keyboard navigation works
	focus $tree

	return
}

# NSBirth::ZGetRaceAttributes --
#
#	Return a list of attributes for the given player race.
#	In ZAngband the flags are dynamically determined.
#	XXX Keep this in synch with player_flags() in files.c.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::ZGetRaceAttributes {race} {

	set flags {}
	switch -- $race {
		"Human" {
		}
		"Half-Elf" {
		}
		"Elf" {
			lappend flags {RES_LITE 0}
		}
		"Hobbit" {
			lappend flags {SUST_DEX 0}
		}
		"Gnome" {
			lappend flags {FREE_ACT 0}
		}
		"Dwarf" {
			lappend flags {RES_BLIND 0}
		}
		"Half-Orc" {
			lappend flags {RES_DARK 0}
		}
		"Half-Troll" {
			lappend flags {SUST_STR 0}
			lappend flags {REGEN 15}
			# FIXME: also slow digestion for Warrior class
		}
		"Amberite" {
			lappend flags {SUST_CON 0}
			lappend flags {REGEN 0}
		}
		"High-Elf" {
			lappend flags {RES_LITE 0}
			lappend flags {SEE_INVIS 0}
		}
		"Barbarian" {
			lappend flags {RES_FEAR 0}
		}
		"Half-Ogre" {
			lappend flags {SUST_STR 0}
			lappend flags {RES_DARK 0}
		}
		"Half-Giant" {
			lappend flags {RES_SHARDS 0}
			lappend flags {SUST_STR 0}
		}
		"Half-Titan" {
			lappend flags {RES_CHAOS 0}
		}
		"Cyclops" {
			lappend flags {RES_SOUND 0}
		}
		"Yeek" {
			lappend flags {RES_ACID 0}
			lappend flags {IM_ACID 20}
		}
		"Klackon" {
			lappend flags {RES_CONF 0}
			lappend flags {RES_ACID 0}
			lappend flags {SPEED 10}
		}
		"Kobold" {
			lappend flags {RES_POIS 0}
		}
		"Nibelung" {
			lappend flags {RES_DISEN 0}
			lappend flags {RES_DARK 0}
		}
		"Dark-Elf" {
			lappend flags {RES_DARK 0}
			lappend flags {SEE_INVIS 20}
		}
		"Draconian" {
			lappend flags {FEATHER 0}
			lappend flags {RES_FIRE 5}
			lappend flags {RES_COLD 10}
			lappend flags {RES_ACID 15}
			lappend flags {RES_ELEC 20}
			lappend flags {RES_POIS 35}
		}
		"Mindflayer" {
			lappend flags {SUST_INT 0}
			lappend flags {SUST_WIS 0}
			lappend flags {SEE_INVIS 15}
			lappend flags {TELEPATHY 30}
		}
		"Imp" {
			lappend flags {RES_FIRE 0}
			lappend flags {SEE_INVIS 10}
		}
		"Golem" {
			lappend flags {SEE_INVIS 0}
			lappend flags {FREE_ACT 0}
			lappend flags {RES_POIS 0}
			lappend flags {SLOW_DIGEST 0}
			lappend flags {HOLD_LIFE 35}
			lappend flags {RACE_IM_CUT 0}
			lappend flags {RACE_IM_STUN 0}
		}
		"Skeleton" {
			lappend flags {SEE_INVIS 0}
			lappend flags {RES_SHARDS 0}
			lappend flags {HOLD_LIFE 0}
			lappend flags {RES_POIS 0}
			lappend flags {RES_COLD 10}
			lappend flags {RACE_IM_CUT 0}
			lappend flags {RACE_RES_SANITY 0}
		}
		"Zombie" {
			lappend flags {SEE_INVIS 0}
			lappend flags {HOLD_LIFE 0}
			lappend flags {RES_NETHER 0}
			lappend flags {RES_POIS 0}
			lappend flags {SLOW_DIGEST 0}
			lappend flags {RES_COLD 5}
			lappend flags {RACE_IM_CUT 12}
			lappend flags {RACE_RES_SANITY 0}
		}
		"Vampire" {
			lappend flags {HOLD_LIFE 0}
			lappend flags {RES_DARK 0}
			lappend flags {RES_NETHER 0}
			lappend flags {LITE 0}
			lappend flags {RES_POIS 0}
			lappend flags {RES_COLD 0}
			lappend flags {RACE_IM_DARK 0}
			lappend flags {RACE_RES_SANITY 0}
		}
		"Spectre" {
			lappend flags {RES_COLD 0}
			lappend flags {SEE_INVIS 0}
			lappend flags {HOLD_LIFE 0}
			lappend flags {RES_NETHER 0}
			lappend flags {RES_POIS 0}
			lappend flags {SLOW_DIGEST 0}
			lappend flags {TELEPATHY 35}
			lappend flags {RACE_IM_CUT 0}
			lappend flags {RACE_RES_SANITY 0}
			lappend flags {RACE_EAT_NETHER 0}
		}
		"Sprite" {
			lappend flags {RES_LITE 0}
			lappend flags {FEATHER 0}
			lappend flags {SPEED 10}
		}
		"Beastman" {
			lappend flags {RES_SOUND 0}
			lappend flags {RES_CONF 0}
		}
	}
	return $flags
}

# NSBirth::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::SelectionChanged {oop tree count select deselect} {

	variable Detail

	set win [Info $oop win]

	# Do nothing if no new row was selected
	if {![llength $select]} return

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]

	set desc [lindex [Info $oop desc] $row]
	set detail [lindex [Info $oop detail] $row]
	set value [lindex [Info $oop value] $row]

	regsub -all " " $detail * detail
	set string $Detail($detail)
	# Translate < and > to &lt; and &gt;
	regsub -all < $string {\&lt;} string
	regsub -all > $string {\&gt;} string
	switch -- [Info $oop list,what] {
		race {
			append string <h3>[mc "Stat Modifiers"]</h3>
			set r [lsearch -exact [angband info race_name] $value]
			append string "<table border=0 cellpadding=0 cellspacing=0>"
			foreach stat [angband info stat_name] bonus [struct set player_race $r r_adj] {
				append string <tr>
				append string <td>[mc $stat]</td>
				append string "<td width=12></td>"
				append string "<td align=right>[format %+d $bonus]</td>"
				append string </tr>
			}
			append string </table>

			foreach skill {sav stl thn thb dis dev fos srh} {
				set min($skill) 100
				set max($skill) 0
				set r1 0
				foreach race [angband info race_name] {
					set v [struct set player_race $r1 r_$skill]
					if {$v < $min($skill)} {
						set min($skill) $v
					}
					if {$v > $max($skill)} {
						set max($skill) $v
					}
					incr r1
				}
			}
			append string <h3>[mc "Skills"]</h3>
			append string "<table border=0 cellpadding=0 cellspacing=0>"
			foreach skill {sav stl thn thb dis dev fos srh} \
				ability {saving_throw stealth fighting bows_throw disarming magic_device perception searching} \
				label {"Saving Throw" Stealth Fighting Shooting Disarming "Magic Device" Perception Searching} {
				append string <tr>
				append string <td>[mc $label]</td>
				append string "<td width=12></td>"
				set rating [struct set player_race $r r_$skill]
				set rating [expr {int(double($rating - $min($skill)) / ($max($skill) - $min($skill)) * 13)}]
				set s [NSCharInfoCanvas::likert $rating 1]
				set color $NSCharInfoCanvas::likert_color
				append string "<td align=right><font color=$color>$s</font></td>"
				append string </tr>
			}
			append string </table>

			append string <h3>[mc "Infravision"]</h3>
			set infra [struct set player_race $r infra]
			if {$infra == 1} {
				set units foot ; # Never this small
			} else {
				set units feet
			}
			# Hack -- Convert infravision to metric
			if {[variant OANGBANDTK]} {
				if {[Setting use_metric]} {
					set infra [expr {$infra * 3 / 10}]
					if {$infra == 1} {
						set units metre
					} else {
						set units metres
					}
				}
			}
			if {$infra == 0} {
				append string [format [mc infra-none] $infra [mc $units]]
			} else {
				append string [format [mc infra-fmt] $infra [mc $units]]
			}

			append string <h3>[mc "Attributes"]</h3>
			if {[variant ANGBANDTK KANGBANDTK]} {
				set flags {flags1 flags2 flags3}
			}
			if {[variant OANGBANDTK]} {
				set flags {flags1 flags2 flags3 flags_special}
			}
			if {[variant ZANGBANDTK]} {
				set flags {}
			}
			append string <ul>
			foreach f $flags {
				foreach flag [struct flags player_race $r $f] {
					append string <li>[mc $flag]</li>
				}
			}
			if {[variant ZANGBANDTK]} {
				foreach info [ZGetRaceAttributes $value] {
					lassign $info flag level
					if {$level == 0} {
						append string <li>[mc $flag]</li>
					} else {
						set fmt [format [mc attribute] [mc $flag] $level]
						append string <li>$fmt</li>
					}
				}
			}
			append string </ul>

			append string <h3>[mc "Recommended Classes"]</h3>
			foreach choice [struct set player_race $r choice] {
				set class [lindex [angband info class_name] $choice]
				regsub -all " " $class * d_class
				append string [mc $d_class]<br>
			}
		}

		class {
			append string <h3>[mc "Stat Modifiers"]</h3>
			set c [lsearch -exact [angband info class_name] $value]
			append string "<table border=0 cellpadding=0 cellspacing=0>"
			foreach stat [angband info stat_name] bonus [struct set player_class $c c_adj] {
				append string <tr>
				append string <td>[mc $stat]</td>
				append string "<td width=12></td>"
				append string "<td align=right>[format %+d $bonus]</td>"
				append string </tr>
			}
			append string </table>

			foreach skill {sav stl thn thb dis dev fos srh} {
				set min($skill) 100
				set max($skill) 0
				set c1 0
				foreach class [angband info class_name] {
					set v [struct set player_class $c1 c_$skill]
					if {$v < $min($skill)} {
						set min($skill) $v
					}
					if {$v > $max($skill)} {
						set max($skill) $v
					}
					incr c1
				}
			}
			append string <h3>[mc "Skills"]</h3>
			append string "<table border=0 cellpadding=0 cellspacing=0>"
			foreach skill {sav stl thn thb dis dev fos srh} \
				ability {saving_throw stealth fighting bows_throw disarming magic_device perception searching} \
				label {"Saving Throw" Stealth Fighting Shooting Disarming "Magic Device" Perception Searching} {
				append string <tr>
				append string <td>[mc $label]</td>
				append string "<td width=12></td>"
				set rating [struct set player_class $c c_$skill]
				set rating [expr {int(double($rating - $min($skill)) / ($max($skill) - $min($skill)) * 13)}]
				set s [NSCharInfoCanvas::likert $rating 1]
				set color $NSCharInfoCanvas::likert_color
				append string "<td align=right><font color=$color>$s</font></td>"
				append string </tr>
			}
			append string </table>
		}
	}

	set text $win.content.frameText.text

if {[TKHTMLVERSION 1]} {
	$text clear
	$text configure -marginwidth 0 -marginheight 0
}
if {[TKHTMLVERSION 3]} {
	array set fontdesc [fontdesc [Value font,knowledge]]
	$text reset
	$text style "
		html {
			color: White;
			background: [Value listBG];
			font-family: [list $fontdesc(-family)];
			font-size: $fontdesc(-size)pt;
		}
	"
}
	$text parse <html>
	$text parse <h1>$desc</h1>
	$text parse $string
	$text parse </html>
#	$text yview moveto 0.0

if {[TKHTMLVERSION 3]} {
	$text parse -final ""
}
	Info $oop [Info $oop list,what] $value

	return
}

# NSBirth::Invoke --
#
#	Called when a list item is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Invoke {oop tree item row} {

	set win [Info $oop win]
	NSUtils::InvokeButton $win.buttons.next

	return
}

# NSBirth::InfoListAppend --
#
#	Adds an item to the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InfoListAppend {oop desc detail value {fill ""}} {

	global NSBirth

	set win [Info $oop win]
	set tree [Info $oop tree]

	set item [NSList::NewItem $tree]
	NSList::SetText $tree $item $desc
	if {$fill ne ""} {
		NSList::SetTextFill $tree $item $fill
	}

	lappend NSBirth($oop,desc) $desc
	lappend NSBirth($oop,detail) $detail
	lappend NSBirth($oop,value) $value

	return
}

# NSBirth::HtmlFontCmd --
#
#	Font selector for TkHtml.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::HtmlFontCmd {oop size style} {

	set bold [lsearch -exact $style bold]
	set italic [lsearch -exact $style italic]
	set fixed [lsearch -exact $style fixed]
	set fontStyle $style
	if {$fixed >= 0} {
		set font [Value font,fixed]
		set fontStyle [lreplace $fontStyle $fixed $fixed]
	} else {
		set font [Value font,system]
	}
	array set fontdesc [fontdesc $font]
	set fontFamily $fontdesc(-family)
	set i $fontdesc(-size)
	set sizeList [list [expr {$i - 4}] [expr {$i - 2}] $i [expr {$i + 2}] [expr {$i + 4}] [expr {$i + 6}] [expr {$i + 8}]]
	incr size -1
	set fontSize [lindex $sizeList $size]
	return [list $fontFamily $fontSize $fontStyle]
}

# NSBirth::InitScreen_Race --
#
#	Creates the display in the Birth Window to let the user choose
#	a race.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Race {oop} {

	set win [Info $oop win]

	$win.prompt configure -text [mc "Choose your character's race"]
	$win.info configure -text [mc D_Race]

	SetupList $oop
	set tree [Info $oop tree]
	Info $oop list,what race

	if {$::DEBUG} {
		variable Detail
		foreach race [angband info race_name] {
			regsub -all " " $race * race
			if {![info exists Detail($race)]} {
				error "can't find race \"$race\""
			}
		}
	}

	if {[Info $oop createOnly]} return

	set raceList [lsort -dictionary [angband info race_name]]
	foreach race $raceList {
		regsub -all " " $race * d_race
		InfoListAppend $oop [mc $d_race] $race $race
	}

	set row [lsearch $raceList [Info $oop race]]
	NSList::Activate $tree "root child $row"

	return 0
}
 
# NSBirth::InitScreen_Class --
#
#	Creates the display in the Birth Window to let the user choose
#	a class.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Class {oop} {

	set win [Info $oop win]

	$win.prompt configure -text [mc "Choose your character's class"]
	$win.info configure -text [mc D_Class]

	SetupList $oop
	set tree [Info $oop tree]
	Info $oop list,what class

	if {$::DEBUG} {
		variable Detail
		foreach class [angband info class_name] {
			regsub -all " " $class * class
			if {![info exists Detail($class)]} {
				error "can't find class \"$class\""
			}
		}
	}

	if {[Info $oop createOnly]} return

# Get class choices for player race
set race [Info $oop race]
set r [lsearch -exact [angband info race_name] $race]
set choice [struct set player_race $r choice]

	set classList [lsort -dictionary [angband info class_name]]
	foreach class $classList {
set fill ""
set c [lsearch -exact [angband info class_name] $class]
if {[lsearch -integer $choice $c] == -1} {
	set fill gray70
}
		InfoListAppend $oop [mc $class] $class $class $fill
	}

	set row [lsearch $classList [Info $oop class]]
	NSList::Activate $tree "root child $row"

	return 0
}

if {[variant KANGBANDTK]} {

# NSBirth::InitScreen_Plot --
#
#	Creates the display in the Birth Window to let the user choose
#	a plot.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Plot {oop} {

	global NSBirth

	set win $NSBirth($oop,win)

	$win.prompt configure 	-text [mc "Choose a plot"]
	$win.info configure -text [mc D_Plot]
	SetupList $oop
	set tree [Info $oop tree]
	Info $oop list,what plot

	if {[Info $oop createOnly]} return

	set plotNum 1
	foreach plot [birth info plot_name] {
		InfoListAppend $oop $plot plot$plotNum $plotNum
		incr plotNum
	}

	set row [expr {$NSBirth($oop,plot) - 1}]
	NSList::Activate $tree "root child $row"

	return 0
}

# KANGBANDTK
}

if {[variant ZANGBANDTK]} {

# NSBirth::InitScreen_Realm1 --
#
#	Creates the display in the Birth Window to let the user choose
#	the character's first realm of magic.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Realm1 {oop} {

	if {![Info $oop createOnly]} {

		# The "birth info realm_name" command returns a list of magic realm names which the
		# user can choose. This list always includes "None". If it only includes
		# 1 or 2 choices, the user really doesn't have a choice, in which case this screen
		# is not presented.
		set realms [birth info realm_name]
		if {[llength $realms] <= 2} {

			# Choose the realm for the user. This will either be None or some other.
			Info $oop realm1 [lindex $realms end]

			# Skip this screen
			return 1
		}
	}

	set win [Info $oop win]

	$win.prompt configure -text [mc "Choose your character's first realm of magic"]
	$win.info configure -text [mc D_Realm]

	SetupList $oop
	set tree [Info $oop tree]
	Info $oop list,what realm1

	if {[Info $oop createOnly]} return

	set realmList [lrange [birth info realm_name] 1 end]
	foreach realm $realmList {
		InfoListAppend $oop [mc $realm] $realm $realm
	}

	# Synch with the user's preference
	if {[string length [Info $oop request,realm1]]} {
		Info $oop realm1 [Info $oop request,realm1]
		Info $oop request,realm1 ""
	}

	set row [lsearch $realmList [Info $oop realm1]]
	if {$row == -1} {set row 0}
	NSList::Activate $tree "root child $row"

	return 0
}

# NSBirth::InitScreen_Realm2 --
#
#	Creates the display in the Birth Window to let the user choose
#	the character's second realm of magic.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Realm2 {oop} {

	if {![Info $oop createOnly]} {

		# The "birth info realm_name" command returns a list of magic realm names which the
		# user can choose. This list always includes "None". If it only includes
		# 1 or 2 choices, the user really doesn't have a choice, in which case this screen
		# is not presented.
		set realms [birth info realm_name]
		if {[llength $realms] <= 2} {

			# Choose the realm for the user. This will either be None or some other.
			Info $oop realm2 [lindex $realms end]

			# Skip this screen
			return 1
		}
	}

	set win [Info $oop win]

	$win.prompt configure -text [mc "Choose your character's second realm of magic"]
	$win.info configure -text [mc D_Realm]

	SetupList $oop
	set tree [Info $oop tree]
	Info $oop list,what realm2

	if {[Info $oop createOnly]} return

	set realmList [lrange [birth info realm_name] 1 end]
	foreach realm $realmList {
		InfoListAppend $oop [mc $realm] $realm $realm
	}

	# Synch with the user's preference
	if {[string length [Info $oop request,realm2]]} {
		Info $oop realm2 [Info $oop request,realm2]
		Info $oop request,realm2 ""
	}

	set row [lsearch $realmList [Info $oop realm2]]
	if {$row == -1} {set row 0}
	NSList::Activate $tree "root child $row"

	return 0
}

# ZANGBANDTK
}

# NSBirth::InitScreen_AutoRoll --
#
#	Creates the display in the Birth Window to let the user enter
#	minimum stats for the autoroller
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_AutoRoll {oop} {

	set win [Info $oop win]

	if {![Info $oop createOnly]} {

		# Not using the auto-roller
		if {![Info $oop autoroll]} {

			# Skip this screen
			return 1
		}

		$win.buttons.option configure -state disabled
	}

	$win.prompt configure -text [mc "Enter minimum desired stats for the 'auto-roller'"]
	$win.info configure -text [mc D_auto-roller1]
	$win.info configure -text [mc D_auto-roller2]
	set frame $win.content.frameAutoRoll
	set frameStats $win.content.frameAutoRoll.stats

	set font [Value font,fixed]

	if {[Info $oop createOnly]} {

		set data {
			"    Strength:" strength
			"Intelligence:" intelligence
			"      Wisdom:" wisdom
			"   Dexterity:" dexterity
			"Constitution:" constitution
			"    Charisma:" charisma
		}

		frame $frame \
			-borderwidth 0
		pack $frame

		frame $frameStats \
			-borderwidth 0

		set entryList {}

		foreach {title stat} $data {

			set frame $frameStats.$stat
			set label $frame.label
			set entry $frame.entry

			frame $frame \
				-borderwidth 0
			label $label \
				-text [mc $title] -font $font
			entry $entry \
				-width 5 -font $font \
				-textvariable NSBirth($oop,stat_limit,$stat)
			label $frame.max \
				-text [format [mc "(Max of %s)"] [cnv_stat [birth stat max $stat]]] \
				-font $font

			# Change the bindtags for the entry so that the toplevel
			# keypresses are handled before the Entry bindings.
			bindtags $entry [list $entry $win Entry all]

			# Disallow alpha characters in the entry.
			bind $entry <KeyPress> {
				if {![string match \[bBnNoOqQ=\] %A]} {
					if {[string match \[a-zA-Z\] %A]} {
						break
					}
				}
			}

			pack $frame \
				-side top -expand yes -fill x -pady 2 -anchor w
			pack $label \
				-side left -expand no
			pack $entry \
				-side left -expand no -padx 4
			pack $frame.max \
				-side left -expand no

			trace vdelete ::NSBirth($oop,stat_limit,$stat) w \
				"NSBirth::CalcStatTotal $oop"
			trace variable ::NSBirth($oop,stat_limit,$stat) w \
				"NSBirth::CalcStatTotal $oop"

			lappend entryList $entry
		}

		# Return key selects the next entry. Return in the last entry
		# goes to the next screen.
		set index 0
		set count [llength $entryList]
		foreach entry $entryList {
			if {$index < $count - 1} {
				set next [lindex $entryList [expr {$index + 1}]]
				bind $entry <KeyPress-Return> "
					focus $next
					$next selection range 0 end
					$next icursor end
					break
				"
			}

			incr index
		}

		pack $frameStats \
			-side top -anchor c

		set label $win.content.frameAutoRoll.total
		label $label \
			-width 20 -anchor c
		pack $label \
			-side top -anchor n -pady 4

		return
	}

	pack $frame

	foreach stat [angband info stat_name] {
		set frame $frameStats.$stat
		$frame.max configure \
			-text [format [mc "(Max of %s)"] [cnv_stat [birth stat max $stat]]]
	}

	# Display the current total
	CalcStatTotal $oop

	focus $frameStats.strength.entry

	return 0
}

# NSBirth::ValidateAutoStats --
#
#	Extract the user's entered stats from the autroller display.
#	Validate that the stats are not greater than the maximum.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	If the stats are valid, the Priv(stat_limit,STAT) array
#	is set, and 1 is returned. If any stat is above the maximum then 0
#	is returned. Any or all of the stats can be left empty.

proc NSBirth::ValidateAutoStats {oop} {

	variable Priv

	# If the auto-roller isn't used, succeed
	if {![Info $oop autoroll]} {
		return 1
	}

	set win [Info $oop win]
	set frameStats $win.content.frameAutoRoll.stats
	foreach stat [angband info stat_name] {

		# Default to no minimum requested
		set Priv(stat_limit,$stat) 0

		set frame $frameStats.$stat
		set entry $frame.entry
		set string [string trim [$entry get]]

		# No value entered means "zero"
		if {![string length $string]} continue

		# Assume the value is valid
		set valid 1

		# Try 18/NN
		if {[regexp {^18/([0-9]+)$} $string ignore bonus]} {
			set value [expr {18 + $bonus}]

		# Try NN
		} elseif {[string is integer $string]} {
			set value $string
			if {$value > 18} {
				set valid 0
			}

		# Invalid format
		} else {
			set valid 0
		}

		# Validate value < limit
		if {$valid && ($value > [birth stat max $stat])} {
			set valid 0
		}

		# Invalid format
		if {!$valid} {
			bell ; focus $entry ; $entry selection range 0 end
			return 0
		}

		# Save the valid minimum
		set Priv(stat_limit,$stat) $value
	}

	# Success
	return 1
}

# NSBirth::CalcStatTotal --
#
#	Calculates and displays the total needed to create a character with
#	the minimum stats entered by the user. The get_stat() routine rolls
#	up each stat as "5 + 1d3 + 1d4 + 1d5" for a range of "8 to 17". This
#	value is then modified for race and class. The sum of the roll for each
#	stat (unmodified for race or class) must fall in the range of
#	"43 to 53". By subtracting off the race bonus, class bonus, and
#	automatic +5 bonus (see above) from the values entered by the user,
#	it is possible to calculate the sum of all the rolls that will result in
#	the stats the user wants. As long as this sum is less than 54, it
#	should be possible for the auto-roller to meet the requirements.
#
#	In ZAngbandTk, the range is 43-56.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::CalcStatTotal {oop args} {

	set win [Info $oop win]
	set frameStats $win.content.frameAutoRoll.stats

	# Sum up the roll needed for each stat
	set total 0

	# Check each stat
	foreach stat [angband info stat_name] {

		# Get the lower limit for this stat
		set stat_limit 0

		# Get the value entered by the user
		set frame $frameStats.$stat
		set entry $frame.entry
		set string [string trim [$entry get]]

		# An empty string means "zero"
		if {[string length $string]} {

			# Try 18/NN
			if {[regexp {^18/([0-9]+)$} $string ignore bonus]} {
				set stat_limit [expr {18 + $bonus}]

			# Try NN
			} elseif {[string is integer $string]} {
				set value $string
				if {$value < 19} {
					set stat_limit $value
				}
			}
		}

		# Get the class/race bonus
		set bonus [birth stat bonus $stat]

		# Add the automatic bonus
		incr bonus 5

		# A valid limit was specified
		if {$stat_limit} {

			# Subtract the bonus
			set value [SubtractStatBonus $oop $stat $stat_limit $bonus]

			# Sum up the roll needed for each stat
			incr total $value
		}
	}

	if {$total < 0} {set total 0}
	set limit [birth info stat_limit]
	$win.content.frameAutoRoll.total configure \
		-text [format [mc "Total: %2d/%d"] $total $limit]

	return
}

# NSBirth::SubtractStatBonus --
#
#	This must be adjust_stat() in reverse. See the comments for
#	CalcStatTotal().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::SubtractStatBonus {oop stat value bonus} {

	if {$value <= 0} {
		return 0
	}
	if {$value > [birth stat max $stat]} {
		set value [birth stat max $stat]
	}

	set maximize [Info $oop maximize]

	# Negative bonus
	if {$bonus < 0} {
		for {set i 0} {$i < -$bonus} {incr i} {
			if {$value < 18} {
				incr value
			} else {
				incr value 10
			}
		}

	# Positive bonus
	} elseif {$bonus > 0} {
		for {set i 0} {$i < $bonus} {incr i} {
			if {$maximize} {
				if {$value > 18 + 10} {
					incr value -10
				} elseif {$value > 18} {
					set value 18
				} elseif {$value > 3} {
					incr value -1
				}
			} else {
				if {$value > 18 + 90} {
					incr value -1
				} elseif {$value > 18 + 70} {
					incr value -8
				} elseif {$value > 18 + 20} {
					incr value -20
				} elseif {$value > 18} {
					set value 18
				} elseif {$value > 3} {
					incr value -1
				}
			}
		}
	}

	return $value
}

# NSBirth::InitScreen_Points --
#
#	Creates the display in the Birth Window to let the user choose
#	stats using points.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_Points {oop} {

	set win [Info $oop win]

	if {![Info $oop createOnly]} {

		# Not using the point-based
		if {![Info $oop pointbased]} {

			# Skip this screen
			return 1
		}

		$win.buttons.option configure -state disabled
	}

	$win.prompt configure -text [mc Prompt_Points]
	$win.info configure -text [mc Info_Points]
	set frame $win.content.framePoints
	set frameStats $frame.stats

	if {[Info $oop createOnly]} {

		set data {
			"    Strength:" strength
			"Intelligence:" intelligence
			"      Wisdom:" wisdom
			"   Dexterity:" dexterity
			"Constitution:" constitution
			"    Charisma:" charisma
		}

		frame $frame \
			-borderwidth 0
		pack $frame

		frame $frameStats \
			-borderwidth 0

		set entryList {}

		set font [Value font,fixed]

		foreach {title stat} $data {

			set frame $frameStats.$stat
			set label $frame.label
			set entry $frame.entry

			frame $frame \
				-borderwidth 0
			label $label \
				-text [mc $title] -font $font
			spinbox $entry \
				-width 5 -font $font \
				-command "NSBirth::PointDelta $oop $stat %d"
			label $frame.cost \
				-text "" -font $font -anchor w \
				-width [string length [format [mc "(Cost %d)"] 30]]

			# Change the bindtags for the spinbox so that the toplevel
			# keypresses are handled before the Spinbox bindings.
			bindtags $entry [list $entry $win Spinbox all]

			# Disallow editing the entry
			$entry configure -state readonly ; #-cursor {}

			pack $frame \
				-side top -expand yes -fill x -pady 2 -anchor w
			pack $label \
				-side left -expand no
			pack $entry \
				-side left -expand no -padx 4
			pack $frame.cost \
				-side left -expand no

			lappend entryList $entry
		}

		# Return key selects the next entry. Return in the last entry
		# goes to the next screen.
		set index 0
		set count [llength $entryList]
		foreach entry $entryList {
			if {$index < $count - 1} {
				set next [lindex $entryList [expr {$index + 1}]]
				bind $entry <KeyPress-Return> "
					focus $next
					$next selection range 0 end
					$next icursor end
					break
				"
			}

			incr index
		}

		pack $frameStats \
			-side top -anchor c

		set label $win.content.framePoints.total
		label $label \
			-width 20 -anchor c
		pack $label \
			-side top -anchor n -pady 4

		return
	}

	pack $frame

	foreach stat [angband info stat_name] {
		catch {
			birth points stat $stat [Info $oop points,$stat]
		}
	}
	PointDisplay $oop

	return 0
}

# NSBirth::PointDelta --
#
#	Called when a up-down control is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::PointDelta {oop stat delta} {

	if {$delta eq "up"} {
		set delta 1
	} else {
		set delta -1
	}
	if {[catch {
		set oldValue [birth points stat $stat]
		set newValue [expr {$oldValue + $delta}]
		birth points stat $stat $newValue
		Info $oop points,$stat $newValue
	}]} {
		bell
		return
	}

	PointDisplay $oop

	return
}

# NSBirth::PointDisplay --
#
#	Display the current stats and costs.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::PointDisplay {oop} {

	set win [Info $oop win]

	set total 0
	foreach stat [angband info stat_name] {
		set frame $win.content.framePoints.stats.$stat

		# Display the current stat
		$frame.entry configure -state normal
		$frame.entry delete 0 end
		$frame.entry insert 0 [cnv_stat [birth stat current $stat]]
		$frame.entry configure -state readonly

		# Display the cost for this stat
		set cost [birth points cost $stat]
		$frame.cost configure -text [format [mc "(Cost %d)"] $cost]

		# Total cost
		incr total $cost
	}

	# Display the total cost
	$win.content.framePoints.total configure \
		-text [format [mc "Total: %2d/%d"] $total 48]

	return
}

# NSBirth::InitScreen_RollOne --
#
#	Hides the Birth Window, shows the Player Window, and then
#	rolls up the first character.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitScreen_RollOne {oop} {

	variable Priv

	set playerId $Priv(playerId)
	set win [NSPlayer::Info $playerId win]
	WindowPosition $win 2 3
	WindowBringToFront $win

	wm withdraw [Info $oop win]

	# Use the autoroller?
	if {[Info $oop autoroll]} {

		# One-time autoroller setup
		set Priv(auto_round) 0
		set Priv(last_round) 0

		# Set the auto-roller round
		set win .autoprogress
		$win.autoround configure -text $Priv(auto_round)

		# Reset the "success" counter
		foreach stat [angband info stat_name] {
			set Priv(stat_match,$stat) 0
			$win.stats.$stat.match configure -text (NONE)
		}
	}

	Configure $oop Roll

	return 0
}

# NSBirth::RollOne --
#
#	Randomly generate a character. If using the auto-roller, then
#	loop until an acceptable character is generated, or the user
#	aborts.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::RollOne {oop} {

	variable Priv

	# This is not the first roll
	if {[Info $oop hasRoll]} {

		# Remember the current character
		birth save_prev
	}

	# Auto-roll
	if {[Info $oop autoroll]} {

		# Note when we started
		set Priv(last_round) $Priv(auto_round)

		# User can cancel
		set Priv(userAbort) 0

		# Clear the display
		NSPlayer::WipeInfo $Priv(playerId)

		# Display the progress window
		set win .autoprogress
WindowPosition $win 2 3
#		wm deiconify $win
		focus $win
		update

		# Loop forever
		while 1 {

			set accept 1

			# Get a new character
			birth get_stats

			# Advance the round
			incr Priv(auto_round)

			# Hack -- Prevent overflow
			if {$Priv(auto_round) >= 1000000} break

			# Check and count acceptable stats
			foreach stat [angband info stat_name] {

				# This stat is okay
				if {[birth stat current $stat] >= $Priv(stat_limit,$stat)} {
					incr Priv(stat_match,$stat)

				# This stat is not okay
				} else {
					set accept 0
				}
			}

			# Break if "happy"
			if {$accept} break

			# Display progress every 100 rounds
			if {$Priv(auto_round) % 100} continue

			# Display progress
			AutoProgress $oop

			# Draw, and check for events
			update

			# Check for a keypress
			if {$Priv(userAbort)} break
		}

		# Hide the progress window
		wm withdraw .autoprogress

	# Not using the auto-roller
	} else {

		# Generate a random set of stats
		birth get_stats
	}

	# Generate the other character info
	birth get_player

	# Set player-info display...
	NSPlayer::SetInfo $Priv(playerId)

	# ...but clear the name
	$Priv(canvas) itemconfigure name -text ""

	# A character was created
	Info $oop hasRoll 1

	return
}

# NSBirth::AutoProgress --
#
#	Display progress of the autoroller.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::AutoProgress {oop} {

	variable Priv

	set win .autoprogress

	foreach stat [angband info stat_name] {

		set frame $win.stats.$stat
		set value $frame.value
		set match $frame.match

		# Put the stat
#		$value configure -text [cnv_stat [birth stat current $stat]]

		# Put the percent
		if {$Priv(stat_match,$stat)} {

			set p [expr {1000 * $Priv(stat_match,$stat) / $Priv(auto_round)}]
			set buf [expr {$p / 10}].[expr {$p % 10}]%
			$match configure -text $buf

		# Never happened
		} else {
			$match configure -text (NONE)
		}
	}

	# Display round
	$win.autoround configure -text [format [mc "Round: %d"] $Priv(auto_round)]

	return
}

# NSBirth::BirthHack --
#
#	The "birth" command requires that each stage of character creation
#	be completed in order. Because this display allows the user to go
#	forwards and backwards, we must *reset* the "birth" stage and then
#	artificially move it forward to the correct stage, by supplying
#	values for the stages already gained.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::BirthHack {oop} {

	set win [Info $oop win]

	birth reset

	# If a dead-character savefile was loaded, we should preserve
	# the character name.
#	birth name ""

	if {[variant ANGBANDTK]} {
		set screenList [list Gender Race Class Options AutoRoll RollOne]
	}
	if {[variant OANGBANDTK]} {
		set screenList [list Gender Race Class Options AutoRoll RollOne]
	}
	if {[variant KANGBANDTK]} {
		set screenList [list Gender Race Class Plot Options AutoRoll RollOne]
	}
	if {[variant ZANGBANDTK]} {
		set screenList [list Gender Race Class Realm1 Realm2 Options AutoRoll RollOne]
	}

	foreach screen $screenList {
		if {[string equal $screen [Info $oop screen]]} break
		switch -- $screen {	
			Gender {birth gender [Info $oop gender]}
			Race {birth race [Info $oop race]}
			Class {birth class [Info $oop class]}
			Options {
				if {[variant ZANGBANDTK]} {
					birth option max_quest [Info $oop max_quest]
				}
				birth done_options
			}
			AutoRoll {}
			RollOne {}
		}
		if {[variant KANGBANDTK]} {
			switch -- $screen {
				Plot {birth plot [Info $oop plot]}
			}
		# KANGBANDTK
		}
		if {[variant ZANGBANDTK]} {
			switch -- $screen {
				Realm1 {birth realm1 [Info $oop realm1]}
				Realm2 {birth realm2 [Info $oop realm2]}
			}
		# ZANGBANDTK
		}
	}

	return
}

# NSBirth::GetName --
#
#	Let's the user enter a name for the character.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::GetName {oop} {

	variable Priv

	set playerId $Priv(playerId)
	set canvas $Priv(canvas)

	# Get a character name from the user. The default is
	# the current name, because the character might be from
	# a dead character's file.
if 1 {
	# Don't use the NSPlayer::GetName thing because it doesn't allow for
	# IME (?) input (for Japanese players etc)

	set win [NSPlayer::Info $playerId win]

	# Ask until a valid name is entered, or cancel
	while 1 {

		# Ask for a new name
		set name [NSUtils::StringBox -title [mc pyname-title] \
			-initial [angband player name] -prompt [mc pyname-prompt] \
			-message [mc pyname-msg] \
			-buttons [list [mc OK] [mc Cancel]] -parent $win \
			-entrywidth 20 -maxlen 15]

		# Validate the name
		if {[string length $name]} {
			set valid 1
			foreach ch [split $name ""] {
				if {[string is control $ch]} {
					set valid 0
				}
			}
			if {!$valid} {
				tk_messageBox -icon info -title [mc pyname-invalid-title] \
					-message [mc pyname-invalid-msg] -parent $win
				continue
			}
		} else {
			# TODO: Pick a random name
			bell
			continue
		}
		break
	}

} else {
	set name [NSPlayer::GetName $playerId [angband player name] 0]
}

	# Set character name
	birth name $name

	# Set name in player-info window
	$canvas itemconfigure name -text $name -fill [Value TERM_L_BLUE]

	return
}

# NSBirth::Busy --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Busy {oop busy} {

	if {$busy == [Info $oop busy]} return
	Info $oop busy $busy

	return
}

# NSBirth::Next --
#
#	Goes to the next screen in the Birth Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Next {oop} {

	set win [Info $oop win]

	# No going to the next screen before things are ready
	if {[Info $oop busy]} return

	# The "Options" button is eventually disabled
	$win.buttons.option configure -state normal

	set nextScreen ""

	if {[variant ANGBANDTK]} {
		switch [Info $oop screen] {

			Gender {set nextScreen Race}
			Race {set nextScreen Class}
			Class {
				# Note: pointbased > autoroll
				if {[Info $oop pointbased]} {
					set nextScreen Points
				} elseif {[Info $oop autoroll]} {
					set nextScreen AutoRoll
				} else {
					set nextScreen RollOne
				}
			}
			AutoRoll {
				if {[ValidateAutoStats $oop]} {
					set nextScreen RollOne
				}
			}
			Points {set nextScreen RollOne}
			RollOne {}
		}
	# ANGBANDTK
	}

	if {[variant KANGBANDTK]} {
		switch [Info $oop screen] {

			Gender {set nextScreen Race}
			Race {set nextScreen Class}
			Class {set nextScreen Plot}
			Plot {
				# Note: pointbased > autoroll
				if {[Info $oop pointbased]} {
					set nextScreen Points
				} elseif {[Info $oop autoroll]} {
					set nextScreen AutoRoll
				} else {
					set nextScreen RollOne
				}
			}
			AutoRoll {
				if {[ValidateAutoStats $oop]} {
					set nextScreen RollOne
				}
			}
			Points {set nextScreen RollOne}
			RollOne {}
		}
	# KANGBANDTK
	}

	if {[variant OANGBANDTK]} {
		switch [Info $oop screen] {

			Gender {set nextScreen Race}
			Race {set nextScreen Class}
			Class {
				# Note: pointbased > autoroll
				if {[Info $oop pointbased]} {
					set nextScreen Points
				} elseif {[Info $oop autoroll]} {
					set nextScreen AutoRoll
				} else {
					set nextScreen RollOne
				}
			}
			AutoRoll {
				if {[ValidateAutoStats $oop]} {
					set nextScreen RollOne
				}
			}
			Points {set nextScreen RollOne}
			RollOne {}
		}
	# OANGBANDTK
	}

	if {[variant ZANGBANDTK]} {
		switch [Info $oop screen] {

			Gender {set nextScreen Race}
			Race {set nextScreen Class}
			Class {set nextScreen Realm1}
			Realm1 {set nextScreen Realm2}
			Realm2 {
				# Note: pointbased > autoroll
				if {[Info $oop pointbased]} {
					set nextScreen Points
				} elseif {[Info $oop autoroll]} {
					set nextScreen AutoRoll
				} else {
					set nextScreen RollOne
				}
			}
			AutoRoll {
				if {[ValidateAutoStats $oop]} {
					set nextScreen RollOne
				}
			}
			Points {set nextScreen RollOne}
			RollOne {}
		}
	# ZANGBANDTK
	}

	if {[string length $nextScreen]} {
		Busy $oop 1
		pack forget {*}[winfo children $win.content]
		Info $oop screen $nextScreen
		BirthHack $oop
		if {[InitScreen_$nextScreen $oop]} {
			Busy $oop 0
			Next $oop
			return
		}

		# Not on the first screen, so user can go back
		$win.buttons.back configure -state normal

		update
		Busy $oop 0
	}

	return
}

# NSBirth::Back --
#
#	Goes to the previous screen in the Birth Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Back {oop} {

	set win [Info $oop win]

	# No going to the previous screen before things are ready
	if {[Info $oop busy]} return

	# The "Options" button is eventually disabled
	$win.buttons.option configure -state normal

	set prevScreen ""

	if {[variant ANGBANDTK]} {
		switch [Info $oop screen] {

			Race {set prevScreen Gender}
			Class {set prevScreen Race}
			AutoRoll {set prevScreen Class}
			Points {set prevScreen Class}
			RollOne {}
		}
	# ANGBANDTK
	}

	if {[variant KANGBANDTK]} {
		switch [Info $oop screen] {

			Race {set prevScreen Gender}
			Class {set prevScreen Race}
			Plot {set prevScreen Class}
			AutoRoll {set prevScreen Plot}
			Points {set prevScreen Plot}
			RollOne {prevScreen AutoRoll}
		}
	# KANGBANDTK
	}

	if {[variant OANGBANDTK]} {
		switch [Info $oop screen] {

			Race {set prevScreen Gender}
			Class {set prevScreen Race}
			AutoRoll {set prevScreen Class}
			Points {set prevScreen Class}
			RollOne {prevScreen AutoRoll}
		}
	# OANGBANDTK
	}

	# Try this: set an array Priv(back,$screen) for each screen.
	# For RollOne "set Priv(back,RollOne) AutoRoll" and in InitScreen_AutoRoll()
	# just return "1" to bypass the AutoRoll screen if autoroll is FALSE.

	if {[variant ZANGBANDTK]} {

		switch [Info $oop screen] {

			Race {set prevScreen Gender}
			Class {set prevScreen Race}
			Realm1 {set prevScreen Class}
			Realm2 {set prevScreen Realm1}
			AutoRoll {set prevScreen Realm2}
			Points {set prevScreen Realm2}
			RollOne {prevScreen AutoRoll}
		}
	# ZANGBANDTK
	}

	if {[string length $prevScreen]} {
		Busy $oop 1
		pack forget {*}[winfo children $win.content]
		Info $oop screen $prevScreen
		BirthHack $oop
		if {[InitScreen_$prevScreen $oop]} {
			Busy $oop 0
			Back $oop
			return
		}

		# On the first screen, so user can't go back
		if {[Info $oop screen] eq "Gender"} {
			$win.buttons.back configure -state disabled
		}

		update
		Busy $oop 0
	}

	return
}

# NSBirth::Configure --
#
#	Manages the flow of control after the Player Window is displayed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Configure {oop stage} {

	variable Priv

	set playerId $Priv(playerId)
	set win [NSPlayer::Info $playerId win]
	set canvas $Priv(canvas)

	switch -- $stage {
		StartOver {
			birth reset

			# No character has been created yet
			Info $oop hasRoll 0

			Info $oop screen Gender
			InitScreen_Gender $oop
			wm deiconify [Info $oop win]
			focus [Info $oop win]
			wm withdraw $win
		}
		Roll {
			$Priv(canvas) itemconfigure prompt -text "Rolling..."
			$canvas itemconfigure life \
				-text ""
			bind $win <s> {}
			bind $win <r> {}
			bind $win <Escape> {}

			RollOne $oop

			if {[Info $oop pointbased]} {
				set prompt "'s' to restart"
			} else {
				set prompt "'r' to reroll, 's' to restart"
				if {[birth info has_prev]} {
					append prompt ", 'p' for prev"
				}
			}
			append prompt ", or ESC to accept"
			$canvas itemconfigure prompt \
				-text $prompt

			# Life Rating
			$canvas itemconfigure life \
				-text [format [mc life-rating] [angband player life_rating]]

			bind $win <s> "NSBirth::Configure $oop StartOver"
			if {![Info $oop pointbased]} {
				if {[birth info has_prev]} {
					bind $win <p> "NSBirth::Configure $oop LoadPrev"
				}
				bind $win <r> "NSBirth::Configure $oop Roll"
			}
			bind $win <Escape> "NSBirth::Configure $oop Name"
		}
		LoadPrev {

			# Load the previous roll
			birth load_prev

			# Set player-info display...
			NSPlayer::SetInfo $playerId

			# ...but set the prompt again
			$canvas itemconfigure prompt \
				-text "'r' to reroll, 's' to restart, 'p' for prev, or ESC to accept"

			$canvas itemconfigure life \
				-text "Life Rating: [angband player life_rating]/100"
		}
		Name {
			$canvas itemconfigure prompt \
				-text [mc prompt-name]
			bind $win <p> {}
			bind $win <s> {}
			bind $win <r> {}
			bind $win <Escape> {}

			GetName $oop

			$canvas itemconfigure prompt \
				-text [mc prompt-finish]
			bind $win <s> "NSBirth::Configure $oop StartOver"
			bind $win <Escape> "NSBirth::Configure $oop Accept"
		}
		Accept {

			# Choose a place to save it
			set path [tk_getSaveFile -parent $win -initialdir [Path lib save] \
				-initialfile [angband player name]]
			update

			# No file chosen, so exit
			if {![string length $path]} {
				Close $oop
			}

			# Tell the game where the savefile should go
			angband game savefile $path

			# Save the current options
			WriteSettings $oop

			# Cleanup at the C level
			birth done

			# Read a dead-character's save file
			if {[info exists ::AngbandPriv(load,win)]} {
				WindowBringToFront $::AngbandPriv(load,win)

			# Hack -- Initialize the Load Window now, so when the
			# Player Window is destroyed, the application still has
			# an active front window (otherwise the application will
			# be swapped into the background).
			} else {
				angband_load init
			}

			# Destroy player-info display
			NSModule::CloseModule NSPlayer

			# Clean up
			destroy .autoprogress
			NSModule::CloseModule NSBirth

			# Hack -- A new character was created
			Global isNewGame 1

			# Hack -- Feed the Term to continue at C level
			angband keypress " "
		}
	}

	return
}

# NSBirth::Options --
#
#	Displays the Birth Options Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::Options {oop} {

	NSModule::LoadIfNeeded NSBirthOptions

	return
}

# NSBirth::WriteSettings --
#
#	Save the current settings to the tk/config/birth file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::WriteSettings {oop} {

	global Angband

	set win [Info $oop win]

	set fileName [PathTk config birth]
	if {[catch {openlf $fileName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"birth\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -parent $win -message $msg
		return
	}

	set data [list \
		char,gender [Info $oop gender] \
		char,race [Info $oop race] \
		char,class [Info $oop class] \
		stat_limit,strength [Info $oop stat_limit,strength] \
		stat_limit,intelligence [Info $oop stat_limit,intelligence] \
		stat_limit,wisdom [Info $oop stat_limit,wisdom] \
		stat_limit,dexterity [Info $oop stat_limit,dexterity] \
		stat_limit,constitution [Info $oop stat_limit,constitution] \
		stat_limit,charisma [Info $oop stat_limit,charisma] \
	]
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		lappend data \
			birth_point_based [Setting birth_point_based] \
			birth_auto_roller [Setting birth_auto_roller] \
			birth_maximize [Setting birth_maximize] \
			birth_preserve [Setting birth_preserve]
		if {[variant ANGBANDTK KANGBANDTK]} {
			lappend data \
				birth_ironman [Setting birth_ironman] \
				birth_no_stores [Setting birth_no_stores] \
				birth_no_artifacts [Setting birth_no_artifacts] \
				birth_rand_artifacts [Setting birth_rand_artifacts]
		}
		if {[variant KANGBANDTK]} {
			lappend data \
				birth_smart_cheat [Setting birth_smart_cheat]
		}
	}
	if {[variant KANGBANDTK]} {
		lappend data \
			plot [Info $oop plot]
	}
	if {[variant ZANGBANDTK]} {
		lappend data \
			point_based [Setting point_based] \
			autoroller [Setting autoroller] \
			maximize_mode [Setting maximize_mode] \
			preserve_mode [Setting preserve_mode] \
			ironman_shops [Setting ironman_shops] \
			ironman_small_levels [Setting ironman_small_levels] \
			ironman_downward [Setting ironman_downward] \
			ironman_autoscum [Setting ironman_autoscum] \
			ironman_hard_quests [Setting ironman_hard_quests] \
			ironman_empty_levels [Setting ironman_empty_levels] \
			ironman_rooms [Setting ironman_rooms] \
			ironman_nightmare [Setting ironman_nightmare] \
			lite_town [Setting lite_town] \
			max_quest [Info $oop max_quest] \
			vanilla_town [Setting vanilla_town] \
			char,realm1 [Info $oop realm1] \
			char,realm2 [Info $oop realm2] \
			terrain_streams [Setting terrain_streams] \
			munchkin_death [Setting munchkin_death]
	}

	lappend data \
		points,strength [Info $oop points,strength] \
		points,intelligence [Info $oop points,intelligence] \
		points,wisdom [Info $oop points,wisdom] \
		points,dexterity [Info $oop points,dexterity] \
		points,constitution [Info $oop points,constitution] \
		points,charisma [Info $oop points,charisma]

	puts $fileId "# Automatically generated. Do not edit.\n"

	foreach {keyword value} $data {
		puts $fileId "ReadSettingAux $keyword \"$value\""
	}

	close $fileId

	return
}

# NSBirth::ReadSettingAux --
#
#	This command is called for each option read from the tk/config/birth
#	file, which contains default settings for character creation.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::ReadSettingAux {keyword value} {

	variable Priv

	set oop $Priv(oop)

	switch -- $keyword {
		char,gender -
		char,race -
		char,class {
			scan $keyword "char,%s" info
			if {[lsearch -exact [angband info ${info}_name] $value] != -1} {
				Info $oop $info $value
			}
		}
		stat_limit,strength -
		stat_limit,intelligence -
		stat_limit,wisdom -
		stat_limit,dexterity -
		stat_limit,constitution -
		stat_limit,charisma {
			Info $oop $keyword $value
		}
	}
	if {[variant ANGBANDTK KANGBANDTK]} {
		switch -- $keyword {
			birth_auto_roller {
				Setting $keyword $value
				Info $oop autoroll [string is true $value]
			}
			birth_point_based {
				Setting $keyword $value
				Info $oop pointbased [string is true $value]
			}
			birth_maximize -
			birth_preserve -
			birth_ironman -
			birth_no_stores -
			birth_no_artifacts -
			birth_rand_artifacts {
				Setting $keyword $value
			}
		}
	}
	if {[variant KANGBANDTK]} {
		switch -- $keyword {
			birth_smart_cheat {
				Setting $keyword $value
			}
		}
	}
	if {[variant OANGBANDTK]} {
		switch -- $keyword {
			birth_auto_roller {
				Setting $keyword $value
				Info $oop autoroll [string is true $value]
			}
			birth_point_based {
				Setting $keyword $value
				Info $oop pointbased [string is true $value]
			}
			birth_maximize -
			birth_preserve {
				Setting $keyword $value
			}
		}
	}
	if {[variant KANGBANDTK]} {
		switch -- $keyword {
			plot {
				if {($value >= 1) && ($value <= 10)} {
					Info $oop $keyword $value
				}
			}
		}
	# KANGBANDTK
	}
	if {[variant ZANGBANDTK]} {
		switch -- $keyword {
			autoroller {
				Setting $keyword $value
				Info $oop autoroll [string is true $value]
			}
			point_based {
				Setting $keyword $value
				Info $oop pointbased [string is true $value]
			}
			max_quest {
				Info $oop $keyword $value
			}
			maximize_mode -
			preserve_mode -
			terrain_streams -
			munchkin_mindcraft -
			munchkin_death -
			ironman_autoscum -
			ironman_downward -
			ironman_hard_quests -
			ironman_shops -
			ironman_small_levels -
			ironman_empty_levels -
			ironman_rooms -
			lite_town -
			vanilla_town -
			Rand_unbiased {
				Setting $keyword $value
			}
			char,realm1 -
			char,realm2 {
				scan $keyword {char,%s} realm
				Info $oop request,$realm $value
			}
		}
	# ZANGBANDTK
	}

	switch -- $keyword {
		points,strength -
		points,intelligence -
		points,wisdom -
		points,dexterity -
		points,constitution -
		points,charisma {
			Info $oop $keyword $value
		}
	}

	return
}

# NSBirth::ReadSettings --
#
#	Read the tk/config/birth file, which contains default settings
#	for character creation.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::ReadSettings {oop} {

	global Angband

	set win .

	set fileName [PathTk config birth]
	if {![file exists $fileName]} {
		return
	}

	if {[catch {
		source $fileName
	} err]} {
		set msg "The following error occurred while attempting to "
		append msg "read\nthe tk/config/birth file:\n\n$err\n\n"
		append msg "This is not a fatal error but you may need to "
		append msg "set the\ncharacter-creation options again."
		tk_messageBox -title "File Error" -icon info -type ok \
			-parent $win -message $msg
	}

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {

		# point-based takes precedence over auto-roller
		if {[Setting birth_point_based]} {
			Setting birth_auto_roller 0
			Info $oop autoroll 0
		}
	}
	if {[variant ZANGBANDTK]} {

		# point-based takes precedence over auto-roller
		if {[Setting point_based]} {
			Setting autoroller 0
			Info $oop autoroll 0
		}
	}

	return
}

if {[variant KANGBANDTK]} {

# NSBirth::ReadPlots --
#
#	Reads each of the lib/edit/q_infoN.txt files and sets the
#	corresponding Detail(plotN) variable with a list of plot
#	names.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

# Old version
proc NSBirth::ReadPlots {} {

	global Angband
	variable Detail

	# Parse each lib/edit/q_info*.txt file
	foreach fileName [glob -directory [Path lib edit] q_info*.txt] {
		set fileId -1
		catch {

			# Get the plot number from the file name
			regexp {q_info(.*).txt} $fileName dummy plotNum

			# Build a list of quest names
			set questList {}

			# Open this q_info*.txt file
			set fileId [open $fileName]

			# Check each line
			foreach lineBuf [split [read $fileId] \n] {

				# Look for "N:number:name" quest introduction
				if {[regexp {^N:([0-9]+):(.+)} $lineBuf dummy index questName]} {
					lappend questList $questName
				}
			}

			# Now build a description of the quests in this plot
			set plotInfo "The quests in this plot include:\n"
			set count [llength $questList]
			set index 0
			foreach questName $questList {
				incr index
				if {$index == $count} {
					append plotInfo "and $questName"
				} else {
					append plotInfo "$questName, "
				}
			}
			set Detail(plot$plotNum) $plotInfo
		}

		# Close the file if it was opened
		if {$fileId != -1} {
			close $fileId
		}
	}

	return
}

proc NSBirth::ReadPlots {} {

	global Angband
	variable Detail

	# Parse each lib/edit/q0000*.txt file
	foreach fileName [glob -directory [Path lib edit] q0000*.txt] {
		set fileId -1
		catch {

			# Open the file
			set fileId [open $fileName]

			# Check each line
			foreach lineBuf [split [read $fileId] \n] {

				# Look for "Q:number:N:name" quest name
				if {[regexp {^Q:([0-9]+):N:(.+)} $lineBuf dummy number name]} {
					set questName($number) $name
				}
			}

		}

		# Close the file if it was opened
		if {$fileId != -1} {
			close $fileId
		}
	}

	# Parse each lib/edit/t0000*.txt file
	foreach fileName [glob -directory [Path lib edit] t0000*.txt] {
		set fileId -1
		catch {

			# Get the town number from the file name
			regexp {t0+(.*).txt} $fileName dummy townNum

			# Build a list of quest numbers
			set questList {}

			# Open the file
			set fileId [open $fileName]

			# Check each line
			foreach lineBuf [split [read $fileId] \n] {

				# Look for "?:[EQU $QUESTx 2]" line
				if {[scan $lineBuf {?:[EQU $QUEST%d %d]} number status] == 2} {
					if {$status == 2} {
						lappend questList $number
					}
				}
			}

			# Now build a description of the quests in this town
			set plotInfo "The quests in this plot include:\n"
			set count [llength $questList]
			set index 0
			foreach number $questList {
				incr index
				if {$index == $count} {
					append plotInfo "and $questName($number)"
				} else {
					append plotInfo "$questName($number), "
				}
			}
			set Detail(plot$townNum) $plotInfo
		}

		# Close the file if it was opened
		if {$fileId != -1} {
			close $fileId
		}
	}

	return
}

# KANGBANDTK
}

proc BirthObj {command args} {
	return [NSBirth::$command $::NSBirth::Priv(oop) {*}$args]
}
