# File: mindcraft-menu.tcl

# Purpose: the Mindcraft Menu

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMindcraftMenu {

# namespace eval NSMindcraftMenu
}

# NSMindcraftMenu::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::InitModule {} {

	MsgCatInit mindcraft

	return
}

# NSMindcraftMenu::NSMindcraftMenu --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::NSMindcraftMenu {oop mbarId} {

	global NSMindcraftMenu

	Info $oop mbar $mbarId

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 \
		-postcommand "NSMindcraftMenu::SetupMenus $oop" \
		-identifier MENU_MINDCRAFT]

	# Context-sensitive help
	NSMenu::Info $menuId menuSelectCmd \
		"NSMindcraftMenu::MenuSelect $oop"

	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_MINDCRAFT -label [mc Mindcraft] -underline 0 \
		-identifier M_MINDCRAFT

	foreach power [angband mindcraft get] {
		angband mindcraft info $power attrib
		lappend entries [list -type command \
			-label "$attrib(char) $attrib(name)" \
			-command "NSMindcraftMenu::Invoke $oop $attrib(char)" \
			-underline 0 -identifier "E_POWER_$attrib(char)"]
	}

	NSMenu::MenuInsertEntries $mbarId -end MENU_MINDCRAFT $entries

	return
}

# NSMindcraftMenu::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::Info {oop info args} {

	global NSMindcraftMenu

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMindcraftMenu($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMindcraftMenu($oop,$info)
			}
		}
	}

	return
}

# NSMindcraftMenu::SetupMenus --
#
#	Enable entries representing usable Mindcraft powers.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::SetupMenus {oop} {

	set flag [lindex [angband inkey_flags] 0]

	# Enable entries during commands and when choosing mindcraft powers
	if {$flag ne "INKEY_MINDCRAFT"} {
		if {$flag ne "INKEY_CMD"} return
	}

	set mbarId [Info $oop mbar]

	set identList {}
	foreach power [angband mindcraft get] {
		angband mindcraft info $power attrib
		if {$attrib(okay)} {
			lappend identList E_POWER_$attrib(char)
		}
	}

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSMindcraftMenu::MenuSelect --
#
#	Displays power memory when an entry is highlighted.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::MenuSelect {oop menuId index ident} {

	if {$index ne "none"} {
		set powerIndex [lindex [angband mindcraft get] $index]
		NSRecall::RecallMindcraft $powerIndex
	}
	
	return
}

# NSMindcraftMenu::Invoke --
#
#	Call "angband keypress" to invoke the Mindcraft power.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::Invoke {oop charPower} {

	if {[angband inkey_flags] eq "INKEY_CMD"} {

		# Use "cast" for Mindcraft powers
		set charCmd m
	
		angband keypress $charCmd$charPower
	}

	if {[angband inkey_flags] eq "INKEY_MINDCRAFT"} {
		angband keypress $charPower
	}

	return
}

# NSMindcraftMenu::PopupSelect --
#
#	Show a pop-up menu of Mindcraft power choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftMenu::PopupSelect {menu x y} {

	global PopupResult

	set PopupResult 0

	$menu delete 0 end
	set num 0
	foreach power [angband mindcraft get] {

		# Get information about this power
		angband mindcraft info $power attrib

		# Can we invoke this power?
		if {!$attrib(okay)} continue

		# Append menu entry
		$menu add command -label "$attrib(char) $attrib(name)" \
			-command "angband keypress $attrib(char) ; set PopupResult 1" \
			-underline 0

		# Count the number of powers added to the menu
		incr num
	}

	if {$num} {
		$menu add separator
	}
	$menu add command -label [mc Cancel]

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	tk_popup $menu $x $y [expr {$num / 2}]

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	# If the user unposts the menu without choosing an entry, then
	# I want to feed Escape into the Term. I tried binding to the <Unmap>
	# event but it isn't called on Windows(TM).
	after idle {
		if {!$PopupResult} {
			angband keypress \033
		}
	}

	return
}

