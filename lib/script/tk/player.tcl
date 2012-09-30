# File: player.tcl

# Purpose: the Character Info Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPlayer {

	variable likert_color
	variable Priv

# namespace eval NSPlayer
}

# NSPlayer::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::InitModule {} {

	NSModule::AddModuleIfNeeded NSCharInfoCanvas [PathTk charinfo-canvas.tcl]
	NSModule::LoadIfNeeded NSCharInfoCanvas

	NSObject::New NSPlayer

	return
}

# NSPlayer::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::CloseModule {} {

	catch {
		destroy [Window player]
		NSWindowManager::UnregisterWindow player
		unset ::Windows(player)
	}

	return
}

# NSPlayer::NSPlayer --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::NSPlayer {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow player $win \
		"NSPlayer::GeometryCmd $oop" "" "NSPlayer::DisplayCmd $oop"

	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSPlayer $oop $win

	#
	# Global list of application windows
	#

	Global player,oop $oop
	Window player $win

	return
}

# NSPlayer::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::Info {oop info args} {

	global NSPlayer

	# Verify the object
	NSObject::CheckObject NSPlayer $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPlayer($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPlayer($oop,$info)
			}
		}
	}

	return
}

# NSPlayer::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::InitWindow {oop} {

	global NSPlayer
	global Windows
	variable Priv

	set win .player$oop
	toplevel $win
	wm title $win "Character Info"
	wm resizable $win no no

	# Character creation
	if {[info exists Windows(main)]} {
		wm transient $win [Window main]
	}

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "angband keypress \033"

	Info $oop win $win

	#
	# Menus
	#

	InitMenus $oop

	#
	# Main display
	#

	set canvasId [NSObject::New NSCharInfoCanvas $win]
	set canvas [NSCharInfoCanvas::Info $canvasId canvas]
	set font [NSCharInfoCanvas::Info $canvasId font,font]
	set fontHeight [NSCharInfoCanvas::Info $canvasId font,height]
	NSCharInfoCanvas::Info $canvasId topRow 2

	NSCharInfoCanvas::AddTextItem $canvasId 1 0 -1 White "" left prompt

	pack $canvas \
		-expand no

	set Priv(font,font) [NSCharInfoCanvas::Info $canvasId font,font]
	set Priv(font,height) $fontHeight
	set Priv(font,width) [NSCharInfoCanvas::Info $canvasId font,width]

	Info $oop canvasId $canvasId
	Info $oop canvas $canvas

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress> {
		angband keypress %A
	}

	bind $win <KeyPress-h> {
		angband_display playerflags show
	}

	return
}

# NSPlayer::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::InitMenus {oop} {
}

# NSPlayer::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			SetInfo $oop
		}
		postDisplay {
		}
	}

	return
}

# NSPlayer::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]
	set x [winfo x $winMain]
	set y [winfo y $winMain]
	set width [winfo width $win]
	set height [winfo height $win]
	return ${width}x$height+[winfo x $winMain]+[winfo y $winMain]
}

# NSPlayer::AddTextItem --
#
#	Create a new canvas text item with given option values.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::AddTextItem {oop col row width fill text justify tags} {

	NSCharInfoCanvas::AddTextItem [Info $oop canvasId] $col $row $width \
		$fill $text $justify $tags

	return
}

# NSPlayer::WipeInfo --
#
#	Set text of character-specific items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::WipeInfo {oop} {

	NSCharInfoCanvas::WipeInfo [Info $oop canvasId]
	NSCharInfoCanvas::PositionItems [Info $oop canvasId]

	return
}

# NSPlayer::SetInfo --
#
#	Set text of character-specific items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::SetInfo {oop} {

	NSCharInfoCanvas::SetInfo [Info $oop canvasId]

	return
}


# NSPlayer::GetName --
#
#	Allows the user to enter a new character name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlayer::GetName_PutName {} {

	variable Priv

	set nameFrame $Priv(frame)
	set name $Priv(name)
	set fill White
	if {$Priv(default)} {
		set fill [Value TERM_YELLOW]
	}

	for {set i 0} {$i < 15} {incr i} {
		set letter $nameFrame.letter$i.label
		set char [string index $name $i]
		if {$char != {}} {
			$letter configure -text $char -background $fill
		} else {
			$letter configure -text {} -background [Global SystemButtonFace]
		}
	}

	return
}

proc NSPlayer::GetName_KeyPress {ascii} {

	variable Priv

#	if {![string match {[a-zA-Z0-9 ]} $ascii]} return

	# Anything except a control character
	if {[string is control $ascii]} return

	if {$Priv(default)} {
		set Priv(name) ""
		set Priv(length) 0
		set Priv(default) 0
	}

	if {$Priv(length) == 15} return
	append Priv(name) $ascii
	incr Priv(length)
	GetName_PutName

	return
}

proc NSPlayer::GetName_Delete {} {

	variable Priv

	if {$Priv(default)} {
		set Priv(name) ""
		set Priv(length) 0
		set Priv(default) 0
		GetName_PutName
		return
	}

	if {!$Priv(length)} return
	incr Priv(length) -1
	set end [expr {$Priv(length) - 1}]
	set Priv(name) [string range $Priv(name) 0 $end]
	GetName_PutName

	return
}

proc NSPlayer::GetName {oop default allowAbort} {

	global NSPlayer
	variable Priv

	# Get the canvas
	set canvas [Info $oop canvas]

	# Selection rectangle around canvas item
	set heightBy2 [expr {$Priv(font,height) / 2}]
	set width [expr {$Priv(font,width) * 15 + 2}]
	set bbox [$canvas bbox name]
	set bbox [lreplace $bbox 2 2 [expr {[lindex $bbox 0] + $width}]]
	eval $canvas create rectangle $bbox -outline [Value TERM_L_RED] \
		-width 2.0 -tags selectBox

	$canvas create line [lindex $bbox 2] [expr {[lindex $bbox 1] + $heightBy2}] \
		[expr {[lindex $bbox 2] + 30}] [expr {[lindex $bbox 1] + $heightBy2}] \
		-width 2.0 -fill [Value TERM_L_RED] -tags selectBox

	# Create a frame with 15 one-character label widgets
	set frame $canvas.nameFrame
	frame $frame \
		-borderwidth 2 -relief ridge
	for {set i 0} {$i < 15} {incr i} {
		set letterBox $frame.letter$i
		frame $letterBox \
			-borderwidth 1 -relief sunken
		label $letterBox.label \
			-font $Priv(font,font) -width 1
		pack $letterBox \
			-side left -padx 1 -expand no
		pack $letterBox.label
	}

	place $frame \
		-x [expr {[lindex $bbox 2] + 30}] \
		-y [expr {[lindex $bbox 1] + $heightBy2}] \
		-anchor w

	bindtags $frame $frame
	bind $frame <KeyPress> "NSPlayer::GetName_KeyPress %A"
	bind $frame <BackSpace> NSPlayer::GetName_Delete
	bind $frame <Return> "set NSPlayer::Priv(result) 1"
	if {$allowAbort} {
		bind $frame <Escape> "set NSPlayer::Priv(result) 0"
	}

	set Priv(frame) $frame

	# Set a grab and claim the focus too.
	NSUtils::GrabSave $frame
	focus $frame

	set Priv(result) -1
	set Priv(name) $default
	set Priv(length) [string length $Priv(name)]
	set Priv(default) $Priv(length)
	GetName_PutName

	# Wait for the user to accept or cancel
	vwait NSPlayer::Priv(result)

	# Release grab and focus
	NSUtils::GrabRelease $frame

	$canvas delete selectBox
	destroy $frame

	if {$Priv(result)} {
		return $Priv(name)
	}
	return {}
}

