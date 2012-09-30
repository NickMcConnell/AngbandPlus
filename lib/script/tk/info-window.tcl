# File: info-window.tcl

# Purpose: the Info Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSInfoWindow {

# namespace eval NSInfoWindow
}

# NSInfoWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSInfoWindow::InitModule {} {

	NSObject::New NSInfoWindow
}

# NSInfoWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::CloseModule {} {

	catch {
		destroy [Window info]
	}

	return
}

# NSInfoWindow::NSInfoWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID.
#
# Results:
#	What happened.

proc NSInfoWindow::NSInfoWindow {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow info $win \
		"GetDefaultGeometry $win main2 main" "" ""

	# Update ourself when the font,knowledge value changes
	Info $oop clientId,font,knowledge \
		[NSValueManager::AddClient font,knowledge \
			"NSInfoWindow::ValueChanged_font_knowledge $oop"]

	# Update ourself when the listBG value changes
	Info $oop clientId,listBG \
		[NSValueManager::AddClient listBG \
		"NSInfoWindow::ValueChanged_listBG $oop"]

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSInfoWindow $oop $win

	#
	# Global list of application windows
	#

	Global info,oop $oop
	Window info $win

	return
}

# NSInfoWindow::~NSInfoWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::~NSInfoWindow {oop} {

	NSValueManager::RemoveClient listBG [Info $oop clientId,listBG]
	NSValueManager::RemoveClient font,knowledge [Info $oop clientId,font,knowledge]

	return
}

# NSInfoWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::Info {oop info args} {

	global NSInfoWindow

	# Verify the object
	NSObject::CheckObject NSInfoWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSInfoWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSInfoWindow($oop,$info)
			}
		}
	}

	return
}

# NSInfoWindow::InitWindow --
#
#	Create the window associated with the object.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	.

proc NSInfoWindow::InitWindow {oop} {

	set win .info$oop
	toplevel $win
	wm title $win XXXXX

	wm transient $win [Window main]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSInfoWindow::Close $oop"

	# Set instance variables
	Info $oop win $win

	set frame $win.frameList
	frame $frame \
		-relief sunken -borderwidth 1

	set font [Value font,knowledge]
	set canvistId [NSObject::New NSTexist $frame $font 60 10]
	set canvas [NSTexist::Info $canvistId text]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$frame.yscroll set"
	$canvas configure -xscrollcommand "$frame.xscroll set"

	scrollbar $frame.yscroll \
		-orient vertical -command "$canvas yview"

	scrollbar $frame.xscroll \
		-orient horizontal -command "$canvas xview"

	# This call updates the list background color whenever the
	# global list background color changes
	Info $oop list,clientId \
		[NSValueManager::AddClient listBG "$canvas configure \
			-background \[Value listBG]"]

	Info $oop canvistId $canvistId

	pack $win.frameList \
		-expand yes -fill both

	#
	# Geometry
	#

	grid rowconfig $win.frameList 0 -weight 1
	grid rowconfig $win.frameList 1 -weight 0
	grid columnconfig $win.frameList 0 -weight 1
	grid columnconfig $win.frameList 1 -weight 0

	grid $canvas \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress> {
		angband keypress %A
	}

	# Synch the scrollbars when window is shown.
	NSUtils::SynchScrollBar $canvas $win.frameList.yscroll
	NSUtils::SynchScrollBar $canvas $win.frameList.xscroll

	return
}

# NSInfoWindow::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	.

proc NSInfoWindow::Close {oop} {

	angband keypress \033

	return
}

# NSInfoWindow::SetList --
#
#	Display a list of items. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	.

proc NSInfoWindow::SetList {oop title info} {

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]

	wm title $win $title

	NSTexist::Delete $canvistId 0 end

	foreach elem $info {
		NSTexist::Insert $canvistId end $elem White
	}

	return
}

# NSInfoWindow::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Info Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::ValueChanged_font_knowledge {oop} {

	set texistId [Info $oop canvistId]
	[NSTexist::Info $texistId text] configure -font [Value font,knowledge]

	return
}

# NSInfoWindow::ValueChanged_listBG --
#
#	Called when the listBG value changes.
#	Updates the Info Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::ValueChanged_listBG {oop} {

	set color [Value listBG]
	
	set texistId [Info $oop canvistId]
	[NSTexist::Info $texistId text] configure -background $color

	return
}

