# File: tips.tcl

# Purpose: the Tips Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTips {

	variable Priv

# namespace eval NSTips
}

# NSTips::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::InitModule {} {

	variable Priv

	set Priv(current) [Value tip,current]
	set Priv(showTips) [Value tip,show]
#	NSValueManager::AddClient showTips \
#		"set NSTips::Priv(showTips) [Value showTips]"

	ReadTipsFile

	NSObject::New NSTips

	return
}

# NSTips::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::CloseModule {} {
}

# NSTips::ReadTipsFile --
#
#	Read the tips.txt file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::ReadTipsFile {} {

	global Angband
	variable Priv

	set tips_file "tips.txt"
	if {[catch {open [PathTk $tips_file]} fileId]} {
		set prompt "The following error occurred while attempting to open "
		append prompt "the \"$tips_file\" file for reading"
		HandleError $fileId $prompt
		return
	}

	set Priv(count) 0
	while 1 {
		if {[gets $fileId lineBuf] <= 0} break
		lappend Priv(tips) $lineBuf
		incr Priv(count)
	}

	close $fileId

	return
}

# NSTips::NSTips --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::NSTips {oop} {

	InitWindow $oop

	DisplayNextTip $oop

	#
	# Global list of application windows
	#

	Global tip,oop $oop
	Window tip [Info $oop win]

	return
}

# NSAssign::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::Info {oop info args} {

	global NSTips

	# Verify the object
	NSObject::CheckObject NSTips $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSTips($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSTips($oop,$info)
			}
		}
	}

	return
}

# NSTips::InitWindow --
#
#	Initialize the Tips Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::InitWindow {oop} {

	global Angband
	variable Priv

	set win .tips$oop
	toplevel $win
	wm title $win "ZAngband Tips"

	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSTips::Close $oop"

	Info $oop win $win

	# Header
	if {[Platform unix]} {
		set font {Times 24 bold}
	}
	if {[Platform windows]} {
		set font {Times 18 bold}
	}
	label $win.header \
		-text "Welcome to ZAngband" -font $font

	# Tip display
	frame $win.tip \
		-borderwidth 1 -relief sunken
	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}
	text $win.tip.text \
		 -font $font -width 46 -height 10 -cursor {} \
		-background #FFFFE0 -borderwidth 0 -highlightthickness 0 \
		-wrap word -takefocus 0
	bindtags $win.tip.text [list $win.tip.text $win all]
	pack $win.tip.text \
		-fill both

	frame $win.misc \
		-borderwidth 0

	# Checkbutton
	checkbutton $win.misc.checkshow \
		-text "Show this window every time the game starts" \
		-variable NSTips::Priv(showTips) \
		-command "Value tip,show \$NSTips::Priv(showTips)"

	# Tip number
	label $win.misc.tipnumber \
		-anchor e

	pack $win.misc.checkshow \
		-side left -expand no
	pack $win.misc.tipnumber \
		-side right -expand no

	# Divider
	MakeDivider $win.divider1 x

	# Buttons
	frame $win.buttons \
		-borderwidth 0
	button $win.buttons.back \
		-text "< Back" -command "NSTips::DisplayPreviousTip $oop" -width 11 \
		-underline 2
	button $win.buttons.next \
		-text "Next >" -command "NSTips::DisplayNextTip $oop" -width 11 \
		-underline 0 -default active
	button $win.buttons.close \
		-text "Close" -command "NSTips::Close $oop" -width 11
	pack $win.buttons.close \
		-side right -pady 5 -padx 5
	pack $win.buttons.next \
		-side right -pady 5 -padx 5
	pack $win.buttons.back \
		-side right -pady 5 -padx 5

	# Geometry
	grid columnconfigure $win 0 -weight 0
	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 0
	grid rowconfigure $win 2 -weight 0
	grid rowconfigure $win 3 -weight 0
	grid rowconfigure $win 4 -weight 0

	grid $win.header \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky w
	grid $win.tip \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -padx 5 -pady 5
	grid $win.misc \
		-row 2 -column 0 -rowspan 1 -columnspan 2 -padx 5 -sticky ew
	grid $win.divider1 \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew -padx 5
	grid $win.buttons \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky e

	NSUtils::SetDefaultButton $win $win.buttons.next

	bind $win <KeyPress-Left> "tkButtonInvoke $win.buttons.back"
	bind $win <KeyPress-b> "tkButtonInvoke $win.buttons.back"
	bind $win <KeyPress-less> "tkButtonInvoke $win.buttons.back"

	bind $win <KeyPress-Right> "tkButtonInvoke $win.buttons.next"
	bind $win <KeyPress-n> "tkButtonInvoke $win.buttons.next"
	bind $win <KeyPress-greater> "tkButtonInvoke $win.buttons.next"

	bind $win <KeyPress-Return> "NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> "tkButtonInvoke $win.buttons.close"

	WindowPosition $win 2 3
	focus $win

	return
}

# NSTips::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::Close {oop} {

	variable Priv

	wm withdraw [Info $oop win]
	Value tip,current $Priv(current)

	return
}

# NSTips::DisplayTip --
#
#	Display a tip.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::DisplayTip {oop tip} {

	variable Priv

	set win [Info $oop win]
	set text $win.tip.text

	$text delete 1.0 end

	if {[Platform unix]} {
		set font {Helvetica 12}
	}
	if {[Platform windows]} {
		set font {Helvetica 10}
	}

	$text insert end "Did you know...\n" tag1
	$text tag configure tag1 -font [BoldFont $font] \
		-lmargin1 20 -spacing1 20 -spacing3 15

	$text insert end $tip tag2
	$text tag configure tag2 -font $font \
		-lmargin1 20 -lmargin2 20 -rmargin 20

	$win.misc.tipnumber configure -text "[expr {$Priv(current) + 1}]/$Priv(count)"

	return
}

# NSTips::DisplayNextTip --
#
#	Display the next tip, wrapping if required.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::DisplayNextTip {oop} {

	variable Priv

	set current $Priv(current)
	incr current
	if {$current >= $Priv(count)} {
		set current 0
	}
	set Priv(current) $current
	DisplayTip $oop [lindex $Priv(tips) $current]

	return
}

# NSTips::DisplayPreviousTip --
#
#	Display the previous tip, wrapping if required.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTips::DisplayPreviousTip {oop} {

	variable Priv

	set current $Priv(current)
	incr current -1
	if {$current < 0} {
		set current [expr {$Priv(count) - 1}]
	}
	set Priv(current) $current
	DisplayTip $oop [lindex $Priv(tips) $current]

	return
}

