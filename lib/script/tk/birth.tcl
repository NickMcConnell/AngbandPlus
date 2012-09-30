# File: birth.tcl

# Purpose: the Birth Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBirth {

variable Priv
variable Detail

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

	NSModule::LoadIfNeeded NSUpDownEntry

	set oop [NSObject::New NSBirth]

	# Create a player-info window...
	NSModule::AddModule NSPlayer [PathTk player.tcl]
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
	NSModule::AddModule NSBirthOptions [PathTk birth-options.tcl]

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
	wm geometry $win +[winfo screenwidth $win]+0

	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSBirth $oop $win

	set minx [set miny 0]
	set screenList [list Gender Race Class Realm1 Realm2 AutoRoll Points]

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
		eval pack forget [winfo children $win.content]
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

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 0
	grid rowconfigure $win 2 -weight 1
	grid rowconfigure $win 3 -weight 0
	grid rowconfigure $win 4 -weight 0
	grid columnconfigure $win 0 -weight 1

	grid $win.prompt \
		-row 0 -col 0 -rowspan 1 -columnspan 1 -sticky w -padx 5 -pady 5
	grid $win.info \
		-row 1 -col 0 -rowspan 1 -columnspan 1 -sticky w -padx 10 -pady 5
	grid $win.content \
		-row 2 -col 0 -rowspan 1 -columnspan 1 -sticky news -padx 10 -pady 5
	grid $win.divider1 \
		-row 3 -col 0 -rowspan 1 -columnspan 1 -sticky ew -padx 10
	grid $win.buttons \
		-row 4 -col 0 -rowspan 1 -columnspan 1 -sticky ew -padx 5 -pady 5

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
	bind $win <KeyPress-o> "tkButtonInvoke $frame.option ; break"
	bind $win <KeyPress-equal> "tkButtonInvoke $frame.option ; break"
	
	bind $win <KeyPress-greater> "
		if {!\[NSBirth::Info $oop busy]} {
			tkButtonInvoke $frame.next
		}
		break
	"
	bind $win <KeyPress-Right> "
		if {!\[NSBirth::Info $oop busy]} {
			tkButtonInvoke $frame.next
		}
		break
	"
	bind $win <KeyPress-n> "
		if {!\[NSBirth::Info $oop busy]} {
			tkButtonInvoke $frame.next
		}
		break
	"

	bind $win <KeyPress-less> "
		if {!\[NSBirth::Info $oop busy]} {
			tkButtonInvoke $frame.back
		}
		break
	"
	bind $win <KeyPress-Left> "
		if {!\[NSBirth::Info $oop busy]} {
			tkButtonInvoke $frame.back
		}
		break
	"
	bind $win <KeyPress-b> "
		if {!\[NSBirth::Info $oop busy]} {
			tkButtonInvoke $frame.back
		}
		break
	"

	bind $win <KeyPress-q> "tkButtonInvoke $frame.cancel ; break"
	bind $win <KeyPress-Escape> "tkButtonInvoke $frame.cancel ; break"
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
	wm transient $win [NSPlayer::Info $playerId win]
	wm withdraw $win

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

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font [Global font,fixed,normal]
	}

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
			-width 6 -font [AddStyle $font italic] -anchor e -borderwidth 0

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
				"after idle tkButtonInvoke $win.buttons.next"
	
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
			"focus $prev ; $prev select"

		# Down-arrow selects next radiobutton
		if {$index < $count - 1} {
			set next [lindex $radioList [expr {$index + 1}]]
		} else {
			set next [lindex $radioList 0]
		}
		bind $radio <KeyPress-Down> \
			"focus $next ; $next select"

		incr index
	}

	focus $frame.[string tolower [Info $oop gender]]

	return 0
}

# NSBirth::InitList --
#
#	Creates a list (an NSCanvist) and scrolling text field (a Canvas text
#	item, because the scroll behaviour of the Text widget is broken).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::InitList {oop} {

	set win [Info $oop win]

	set font [Global font,sys,normal]
	set cw [font measure $font "0"]
	set lineSpace [expr {[font metrics $font -linespace] + 2}]
	set width [expr {$cw * 20}]
	set height [expr {$lineSpace * 8}]
	
	set frame $win.content.frameList
	frame $frame \
		-borderwidth 2 -relief sunken
	set canvistId [NSObject::New NSCanvist $frame $lineSpace $width $height \
		"NSBirth::NewItemCmd $oop" "NSBirth::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -yscrollcommand "$frame.yscroll set"
	scrollbar $frame.yscroll \
		-command "$canvas yview" -orient vertical

	NSCanvist::Info $canvistId selectionCmd \
		"NSBirth::SelectionChanged $oop"

	NSCanvist::Info $canvistId invokeCmd \
		"NSBirth::Invoke $oop"

	frame $win.content.filler1 \
		-width 5 -borderwidth 0

	set frame $win.content.frameText
	frame $frame \
		-borderwidth 2 -relief sunken

	set lineSpace [font metrics $font -linespace]
	set width [expr {$cw * 40}]
	set height [expr {$lineSpace * 8}]

	set text $frame.text
	canvas $frame.text \
		-background White -width $width -height $height \
		-highlightthickness 0 -borderwidth 0 \
		-yscrollcommand "$frame.yscroll set"
	scrollbar $frame.yscroll \
		-orient vertical -command "$frame.text yview"

	incr width -4
	$frame.text create text 0 0 -font [AddStyle $font bold underline] \
		-anchor nw -width $width -tags header
	set lineSpace [font metrics [AddStyle $font bold underline] -linespace]
	$frame.text create text 0 $lineSpace -font $font -anchor nw \
		-width $width -tags text 

	Info $oop canvistId $canvistId

	set frame $win.content.frameList
	pack $canvas \
		-side left -fill y
	pack $frame.yscroll \
		-side left -fill y

	set frame $win.content.frameText
	pack $text \
		-side left -fill both
	pack $frame.yscroll \
		-side left -fill y

#	bind $win.content.frameList.yscroll <Map> \
#		"eval %W set \[$canvas yview\]"
	NSUtils::SynchScrollBar $canvas $win.content.frameList.yscroll

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
	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	set frame $win.content.frameList
	pack $frame \
		-side left -pady 5 -fill y

	pack $win.content.filler1 \
		-side left -fill y
	
	set frame $win.content.frameText
	pack $frame \
		-side left -pady 5 -fill both

	# Clear the list info
	Info $oop desc {}
	Info $oop detail {}
	Info $oop value {}

	# Clear the list
	NSCanvist::DeleteAll $canvistId

	# Hack -- Focus on the list, so keyboard navigation works
	focus $canvas
	
	return
}

# NSBirth::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirth::NewItemCmd {oop canvistId y text} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Global font,sys,normal]

	set canvasWidth [$canvas cget -width]

	# Selection rectangle
	lappend itemIdList [$canvas create rectangle 0 $y \
		$canvasWidth [expr {$y + $lineHeight}] -fill "" -outline "" \
		-tags selrect]

	# Text
	lappend itemIdList [$canvas create text 2 [expr {$y + 1}] \
		-text $text -anchor nw -font $font -fill Black \
		-tags hilite]

	return $itemIdList
}

# NSBirth::HighlightItemCmd --
#
#	Called by NSCanvist::Select() to highlight a row.
#
# Arguments:
#	oop						OOP ID. See above.
#	canvistId				OOP ID of NSCanvist object.
#	state					1 or 0 highlight state.
#	args					List of canvas item ids
#
# Results:
#	What happened.

proc NSBirth::HighlightItemCmd {oop canvistId state args} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set itemIdList $args

	set idRect [FindItemByTag $canvas $itemIdList selrect]
	set idText [FindItemByTag $canvas $itemIdList hilite]

	if {$state} {
		set fill [Global SystemHighlight]
		$canvas itemconfigure $idRect -fill $fill -outline $fill
		set fill [Global SystemHighlightText]
		$canvas itemconfigure $idText -fill $fill

	} else {
		$canvas itemconfigure $idRect -fill "" -outline ""
		$canvas itemconfigure $idText -fill Black
	}

	return
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

proc NSBirth::SelectionChanged {oop canvistId select deselect} {

	variable Detail
	
	set win [Info $oop win]
	set canvas [NSCanvist::Info $canvistId canvas]

	# Do nothing if no new row was selected
	if {![llength $select]} return

	# Get the (first) row
	set row [lindex $select 0]

	set desc [lindex [Info $oop desc] $row]
	set detail [lindex [Info $oop detail] $row]
	set value [lindex [Info $oop value] $row]
	
	set text $win.content.frameText.text

	$text itemconfigure header -text $desc
	regsub -all " " $detail * detail
	$text itemconfigure text -text $Detail($detail)
	$text configure -scrollregion [$text bbox all]
	$text yview moveto 0.0

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

proc NSBirth::Invoke {oop canvistId x y} {

	set row [NSCanvist::PointToRow $canvistId $x $y]
	if {$row == -1} return
	set win [Info $oop win]
	tkButtonInvoke $win.buttons.next

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

proc NSBirth::InfoListAppend {oop desc detail value} {

	global NSBirth

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]

	NSCanvist::Insert $canvistId end $desc

	lappend NSBirth($oop,desc) $desc
	lappend NSBirth($oop,detail) $detail
	lappend NSBirth($oop,value) $value

	return
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
	set canvistId [Info $oop canvistId]
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
	NSCanvist::UpdateSelection $canvistId $row all
	NSCanvist::See $canvistId $row

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
	set canvistId [Info $oop canvistId]
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

	set classList [lsort -dictionary [angband info class_name]]
	foreach class $classList {
		InfoListAppend $oop [mc $class] $class $class
	}

	set row [lsearch $classList [Info $oop class]]
	NSCanvist::UpdateSelection $canvistId $row all
	NSCanvist::See $canvistId $row

	return 0
}

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
	set canvistId [Info $oop canvistId]
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
	NSCanvist::UpdateSelection $canvistId $row all
	NSCanvist::See $canvistId $row

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
	set canvistId [Info $oop canvistId]
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
	NSCanvist::UpdateSelection $canvistId $row all
	NSCanvist::See $canvistId $row

	return 0
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

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font [Global font,fixed,normal]
	}

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
	
		if {[Platform unix]} {
			set font {Courier 12}
		}
		if {[Platform windows]} {
			set font [Global font,fixed,normal]
		}

		foreach {title stat} $data {
	
			set frame $frameStats.$stat
			set label $frame.label
			set entry $frame.entry
	
			frame $frame \
				-borderwidth 0
			label $label \
				-text [mc $title] -font $font
			updownentry $entry \
				-width 5 -font $font \
				-command "NSBirth::PointDelta $oop $stat"
			label $frame.cost \
				-text "" -font $font -anchor w \
				-width [string length [format [mc "(Cost %d)"] 30]]
	
			# Change the bindtags for the entry so that the toplevel
			# keypresses are handled before the Entry bindings.
			bindtags [$entry info entry] [list [$entry info entry] $win Entry all]
			
			# Disallow editing the entry
			[$entry info entry] configure -state disabled -cursor {}
	
			pack $frame \
				-side top -expand yes -fill x -pady 2 -anchor w
			pack $label \
				-side left -expand no
			pack $entry \
				-side left -expand no -padx 4
			pack $frame.cost \
				-side left -expand no
		
			lappend entryList [$entry info entry]
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
		$frame.entry configure -state disabled

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

	set screenList [list Gender Race Class Realm1 Realm2 Options AutoRoll RollOne]

	foreach screen $screenList {
		if {[string equal $screen [Info $oop screen]]} break
		switch -- $screen {	
			Gender {birth gender [Info $oop gender]}
			Race {birth race [Info $oop race]}
			Class {birth class [Info $oop class]}
			Options {
				birth option max_quest [Info $oop max_quest]
				birth done_options
			}
			AutoRoll {}
			RollOne {}
			Realm1 {birth realm1 [Info $oop realm1]}
			Realm2 {birth realm2 [Info $oop realm2]}
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

	# Prompt

	# Get a character name from the user. The default is
	# the current name, because the character might be from
	# an dead character's file.
	set name [NSPlayer::GetName $playerId [angband player name] 0]

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

	if {[string length $nextScreen]} {
		Busy $oop 1
		eval pack forget [winfo children $win.content]
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
	
	# Try this: set an array Priv(back,$screen) for each screen.
	# For RollOne "set Priv(back,RollOne) AutoRoll" and in InitScreen_AutoRoll()
	# just return "1" to bypass the AutoRoll screen if autoroll is FALSE.
	
	switch [Info $oop screen] {
	
		Race {set prevScreen Gender}
		Class {set prevScreen Race}
		Realm1 {set prevScreen Class}
		Realm2 {set prevScreen Realm1}
		AutoRoll {set prevScreen Realm2}
		Points {set prevScreen Realm2}
		RollOne {prevScreen AutoRoll}
	}
	
	if {[string length $prevScreen]} {
		Busy $oop 1
		eval pack forget [winfo children $win.content]
		Info $oop screen $prevScreen
		BirthHack $oop
		if {[InitScreen_$prevScreen $oop]} {
			Busy $oop 0
			Back $oop
			return
		}

		# On the first screen, so user can't go back
		if {[string equal [Info $oop screen] Gender]} {
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
				-text "Life Rating: [angband player life_rating]/100"

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
				-text "Enter a name (hit Enter when done):"
			bind $win <p> {}
			bind $win <s> {}
			bind $win <r> {}
			bind $win <Escape> {}

			GetName $oop

			$canvas itemconfigure prompt \
				-text "Type 's' to restart, Escape to accept"
			bind $win <s> "NSBirth::Configure $oop StartOver"
			bind $win <Escape> "NSBirth::Configure $oop Accept"
		}
		Accept {

			# Choose a place to save it
			set path [tk_getSaveFile -parent $win -initialdir [PathTk lib save] \
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
		munchkin_death [Setting munchkin_death] \
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

	# point-based takes precedence over auto-roller
	if {[Setting point_based]} {
		Setting autoroller 0
		Info $oop autoroll 0
	}

	return
}

proc BirthObj {command args} {
	return [eval NSBirth::$command $::NSBirth::Priv(oop) $args]
}
