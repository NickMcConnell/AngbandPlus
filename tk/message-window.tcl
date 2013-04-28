# File: message-window.tcl

# Purpose: the Message Window

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMessageWindow {

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

	variable Priv

	MsgCatInit misc-win

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
		destroy [Window message]
	}

	return
}

# NSMessageWindow::NSMessageWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::NSMessageWindow {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow message $win \
		"NSMessageWindow::GeometryCmd $oop" "" \
		"NSMessageWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMessageWindow $oop $win

	#
	# Global list of application windows
	#

	Global message,oop $oop
	Window message $win

	return
}

# NSMessageWindow::~NSMessageWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::~NSMessageWindow {oop} {

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

	# Verify the object
	NSObject::CheckObject NSMessageWindow $oop

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
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::InitWindow {oop} {

	set win .message$oop
	toplevel $win
	wm title $win [mc "Message"]
	wm resizable $win yes no

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	wm protocol $win WM_DELETE_WINDOW {
		NSMainWindow::Info [Global main,oop] messageWindow 0
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MESSAGE
	}
	Term_KeyPress_Bind $win

	Info $oop win $win

	set child [InitDisplay [Window main].message]
	pack $child \
		-side top -expand yes -fill x

	set child [InitDisplay $win]
	pack $child \
		-side top -expand yes -fill x

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

proc NSMessageWindow::DisplayCmd {oop message first} {

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
	set winMain [Window main]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	set x $waLeft
	if {[Global HardcodeGeometry]} {
		incr x [OffscreenOffsetX]
	}

	set y $waTop
	set width [NSToplevel::ContentWidth $win $waWidth]
	set height [winfo reqheight $win]

	return ${width}x$height+$x+$y
}

# NSMessageWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::Close {oop} {

	NSWindowManager::Undisplay message

	return
}

# NSMessageWindow::InitDisplay --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::InitDisplay {parent} {

	set font [Value font,message]

	set text $parent.message
	text $text \
		-width 80 -height 1 -font $font -background Black \
		-foreground White -cursor {} -borderwidth 0 -highlightthickness 0
	bindtags $text [list $text $parent all]

	# When the Message Window is clicked:
	#	- see the previous message, or
	#	- toggle the display of inventory, spells, etc, or
	#	- bypass the -more- prompt
	bind $text <ButtonPress-1> {
		switch -- [angband inkey_flags] {
			INKEY_CMD {
				DoUnderlyingCommand ^o
			}
			INKEY_ITEM -
			INKEY_SPELL {
				angband keypress *
			}
			INKEY_MORE {
				angband keypress " "
			}
		}
		if {[variant ZANGBANDTK]} {
			switch -- [angband inkey_flags] {
				INKEY_CMD_PET -
				INKEY_MINDCRAFT -
				INKEY_POWER {
					angband keypress *
				}
			}
		}
	}
	bind $text <Double-ButtonPress-1> {
		DoUnderlyingCommand ^p
	}

	NSStatusText::StatusText $text [Global main,statusBar] [mc status-message]

	# Update ourself when the font for the Message Window changes
	qebind NSMessageWindow <Value-font,message> \
		NSMessageWindow::ValueChanged_font_message

	if {0 && [llength [info commands tk_chooseFont]]} {
		bind $text <ButtonPress-3> {
			SetMessageFont [tk_chooseFont -parent %W -style]
		}
	}

	pack $text \
		-side top -expand yes -fill x

	Global message,message $text

	# Support for colored messages
	foreach attr [angband info term_attr] {
		$text tag configure $attr -foreground [Value $attr]
	}

	# Pop up a menu when the window is clicked
	set menu $parent.popup
	menu $menu -tearoff 0
	bind $text <ButtonPress-3> \
		"NSMessageWindow::ContextMenu $menu %X %Y"

	return $text
}

# NSMessageWindow::ContextMenu_Message --
#
#	Pop up a context menu in the Message Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::ContextMenu {menu x y} {

	$menu delete 0 end

	$menu add command -label [mc "Set Font"] \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font message"
	if {[winfo ismapped [Window message]]} {
		set label [mc "Attach To Main"]
		set value 0
	} else {
		set label [mc Float]
		set value 1
	}
	$menu add command -label $label \
		-command "
		NSMainWindow::Info [Global main,oop] messageWindow $value
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MESSAGE
		"
	$menu add separator
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSMessageWindow::ValueChanged_font_message --
#
#	Called when the font,message value changes.
#	Updates the Message Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageWindow::ValueChanged_font_message {} {

	set win [Window message]
	set text [Global message,message]

	$text configure -font [Value font,message]
	update idletasks
	wm geometry $win [winfo reqwidth $win]x[winfo reqheight $win]

	return
}

