# File: message-window.tcl

# Purpose: the Messages Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMessageWindow {

	variable Priv

# namespace eval NSMessageWindow
}

# NSMessageWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::InitModule {} {

	NSObject::New NSMessageWindow

	return
}

# NSMessageWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::CloseModule {} {

	catch {
		destroy [Window message2]
	}

	return
}

# NSMessageWindow::NSMessageWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageWindow::NSMessageWindow {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow message2 $win \
		"NSMessageWindow::GeometryCmd $oop" "" \
		"NSMessageWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMessageWindow $oop $win

	# Update ourself when the font,messages value changes
	Info $oop clientId,font,messages \
		[NSValueManager::AddClient font,messages \
		"NSMessageWindow::ValueChanged_Font_Messages $oop"]

	Info $oop clientId,listBG \
		[NSValueManager::AddClient listBG \
		"[Info $oop text] configure -background \[Value listBG]"]

	#
	# Global list of application windows
	#

	Global message2,oop $oop
	Window message2 $win

	return
}

# NSMessageWindow::~NSMessageWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::~NSMessageWindow {oop} {

	NSValueManager::RemoveClient listBG [Info $oop clientId,listBG]
	NSValueManager::RemoveClient font,messages [Info $oop clientId,font,messages]
	
	return
}

# NSMessageWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::Info {oop info args} {

	global NSMessageWindow

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMessageWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMessageWindow($oop,$info)
			}
		}
	}

	return
}

# NSMessageWindow::InitWindow --
#
#	Create the window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageWindow::InitWindow {oop} {

	set win .message$oop
	toplevel $win
	wm title $win "Messages"

	wm transient $win [Window main]

	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMessageWindow::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Set instance variables
	Info $oop win $win

	set frame $win.frame
	frame $frame \
		-borderwidth 0

	set font [Value font,messages]
	set width 60
	set height 5
	set text $frame.text
	text $text \
		-borderwidth 0 -highlightthickness 0 -font $font -width 50 -height 5 \
		-background [Value listBG] -foreground White -wrap none \
		-cursor {}
	bindtags $text [list $text $win all]

	pack $text -expand yes -fill both
	pack $frame -expand yes -fill both

	Info $oop text $text

	bind $text <Configure> \
		"NSMessageWindow::Configure $oop %h %w"

	bind $text <Double-ButtonPress-1> {
		DoUnderlyingCommand ^p
	}	

	# Support for colored messages
	foreach attr [Global term_attr] {
		$text tag configure $attr -foreground [Value $attr]
	}

	return
}

# NSMessageWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
		}
		postDisplay {
			Value message2window,show 1
		}
		postWithdraw {
			Value message2window,show 0
		}
	}

	return
}

# NSMessageWindow::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::GeometryCmd {oop} {

	set win [Info $oop win]
	set left 0
	set top 0
	set right [winfo screenwidth .]
	set bottom [winfo screenheight .]
	
	set width [winfo width $win]
	set height [winfo height $win]
	set x [expr {$right - [NSToplevel::TotalWidth $win]}]
	if {[winfo x [Window main]] >= [winfo screenwidth .]} {
		incr x [winfo screenwidth .]
	}
	set y $top
	return ${width}x$height+$x+$y
}

# NSMessageWindow::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageWindow::Close {oop} {

	NSWindowManager::Undisplay message2

	return
}

# NSMessageWindow::TrackMessage --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageWindow::TrackMessage {oop} {

	set text [Info $oop text]
	$text delete 1.0 end

	# Get the number of messages
	set max [angband message count]

	# Limit to number of displayed messages
	if {$max > [Info $oop numRows]} {
		set max [Info $oop numRows]
	}
	incr max -1

	# Collect each message
	for {set i $max} {$i >= 0} {incr i -1} {
		lappend info [angband message get $i] [angband message color $i] \n {}
	}

	# Display
	eval $text insert end $info

	return
}

# NSMessageWindow::Configure --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageWindow::Configure {oop height width} {

	set text [Info $oop text]
#	set height [winfo height $text]
	incr height [expr {[$text cget -pady] * -2}]
	set rowHeight [font metrics [Value font,messages] -linespace]
	set numRows [expr {$height / $rowHeight}]
	Info $oop numRows $numRows

	TrackMessage $oop

	return
}

# NSMessageWindow::ValueChanged_Font_Messages --
#
#	Called when the font,messages value changes.
#	Updates the Message Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::ValueChanged_Font_Messages {oop} {

	set text [Info $oop text]
	$text configure -font [Value font,messages]
	Configure $oop [winfo height $text] [winfo width $text]

	return
}

