# File: power.tcl

# Purpose: The Power Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPower {

	variable Priv

# namespace eval NSPower
}

# NSPower::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::InitModule {} {

	MsgCatInit power

	NSModule::LoadIfNeeded NSList

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSToolbar

	NSObject::New NSPower

	return
}

# NSPower::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::CloseModule {} {

	catch {
		destroy [Window power]
	}

	return
}

# NSPower::NSPower --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::NSPower {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow power $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSPower::DisplayCmd $oop"

	#
	# Global list of application windows
	#

	Global power,oop $oop
	Window power $win

	return
}

# NSPower::~NSPower --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::~NSPower {oop} {

	return
}

# NSPower::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::Info {oop info args} {

	global NSPower

	# Verify the object
	NSObject::CheckObject NSPower $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPower($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPower($oop,$info)
			}
		}
	}

	return
}

# NSPower::InitWindow --
#
#	Create the window associated with this object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::InitWindow {oop} {

	set win .power$oop
	toplevel $win
	wm title $win [mc Powers]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSPower::Close $oop"

	Info $oop win $win

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_ButtonOptions \
		-command "DoKeymapCmd \033 = {}" -showlabel no
	NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-command "DoKeymapCmd \033 ? {}" -showlabel no

	#
	# List
	#

	set frame $win.frame
	set tree [NSList::New $frame -columns 5 -font magic]
	$tree configure -showheader yes -width 400
	$tree column configure 0 -text [mc Name]
	$tree column configure 1 -text [mc Level] -justify right
	$tree column configure 2 -text [mc Cost] -justify right
	$tree column configure 3 -text [mc Fail] -justify right
	$tree column configure 4 -text [mc Stat]
	for {set C 1} {$C < 5} {incr C} {
		$tree style layout s$C eTxt -padx 6
	}

	# Add text element for char) in Name column
	$tree element create eTxt1 text -fill White
	$tree style elements s0 {eSel.e eTxt1 eTxt}
	$tree style layout s0 eSel.e -union {eTxt1 eTxt}
	$tree style layout s0 eTxt1 -padx {6 0}
	$tree style layout s0 eTxt -padx {0 6}

	NSList::OnSelection $tree \
		"NSPower::SelectionChanged $oop %T %c %S %D"

	NSList::OnInvoke $tree \
		"NSPower::Invoke $oop %T %r"

	Info $oop tree $tree

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

	#
	# Context Menu
	#

	set m $win.context
	menu $m -tearoff 0
	bind $tree <Button-3> "NSPower::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress> {
		angband keypress %A
	}

	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus $tree
		}
	"

	return
}

# NSPower::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			SetList $oop
		}
		postDisplay {
		}
	}

	return
}

# NSPower::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::Close {oop} {

	angband keypress \033

	return
}

# NSPower::SelectionChanged --
#
#	When a power is selected, display info in Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::SelectionChanged {oop tree count select deselect} {

	if {![llength $select]} return
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	NSRecall::RecallPower [lindex [angband power get] $row]

	return
}

# NSPower::Invoke --
#
#	When a power is double-clicked, "angband keypress" the char.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::Invoke {oop tree row} {

	if {[angband inkey_flags] ne "INKEY_POWER"} return

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"
	set char [string index $powerChars $row]

	angband keypress $char

	return
}

# NSPower::ContextMenu --
#
#	When a power is right-clicked, pop up a context menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::ContextMenu {oop menu x y} {

	variable Priv

	# Get the tree
	set tree [Info $oop tree]

	set font [$menu cget -font]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $tree]}]
	set y1 [expr {$y - [winfo rooty $tree]}]
	set row [NSList::Point2Row $tree $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	# No row is hit
	if {$row == -1} {

		$menu add command -label [mc Close] -command {angband keypress \033}
		$menu add separator
		$menu add command -label [mc Cancel]

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"
	set char [string index $powerChars $row]

	$menu add command -label [mc "Select This Power"] \
		-command "angband keypress $char" -font [BoldFont $font]
	$menu add separator
	$menu add command -label [mc Close] -command {angband keypress \033}
	$menu add separator
	$menu add command -label [mc Cancel]

	# Pop up the menu
	tk_popup $menu $x $y

	return
}

# NSPower::SetList --
#
#	Fill the list with powers.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPower::SetList {oop} {

	set tree [Info $oop tree]

	NSList::Clear $tree

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

	set row 0
	foreach power [angband power get] {

		# Get information about the power
		angband power info $power attrib

		# Get the power char (a-z0-9)
		set attrib(char) [string index $powerChars $row]

		# Append the power to the list
		set item [$tree item create]
		$tree item element configure $item 0 eTxt1 -text "$attrib(char)\) "
		NSList::SetText $tree $item $attrib(name)
		if {$attrib(chance) == 100 ||
			$attrib(level) > [angband player level]} {
			NSList::SetTextFill $tree $item gray70
		}
		NSList::SetTextEx $tree $item 1 $attrib(level)
		NSList::SetTextEx $tree $item 2 $attrib(cost)
		NSList::SetTextEx $tree $item 3 $attrib(chance)
		NSList::SetTextEx $tree $item 4 $attrib(stat)
		$tree item lastchild root $item

		incr row
	}

	return
}

