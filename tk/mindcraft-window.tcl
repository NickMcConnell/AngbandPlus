# File: mindcraft-window.tcl

# Purpose: the Mindcraft Window and Mindcraft Menu

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMindcraftWindow {

	variable Priv

# namespace eval NSMindcraftWindow
}

# NSMindcraftWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::InitModule {} {

	MsgCatInit mindcraft

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSList
	NSModule::LoadIfNeeded NSToolbar

	NSObject::New NSMindcraftWindow

	return
}

# NSMindcraftWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::CloseModule {} {

	catch {
		destroy [Window mindcraft]
	}

	return
}

# NSMindcraftWindow::NSMindcraftWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::NSMindcraftWindow {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow mindcraft $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSMindcraftWindow::DisplayCmd $oop"

	#
	# Global list of application windows
	#

	Global mindcraft,oop $oop
	Window mindcraft $win

	return
}

# NSMindcraftWindow::~NSMindcraftWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::~NSMindcraftWindow {oop} {

	return
}

# NSMindcraftWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::Info {oop info args} {

	global NSMindcraftWindow

	# Verify the object
	NSObject::CheckObject NSMindcraftWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMindcraftWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMindcraftWindow($oop,$info)
			}
		}
	}

	return
}

# NSMindcraftWindow::InitWindow --
#
#	Create the window associated with this object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::InitWindow {oop} {

	set win .mindcraft$oop
	toplevel $win
	wm title $win [mc Mindcraft]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMindcraftWindow::Close $oop"

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
	for {set C 1} {$C < 5} {incr C} {
		$tree style layout s$C eTxt -padx 6
	}
	$tree column configure 0 -text [mc Name]
	$tree column configure 1 -text [mc Level] -justify right
	$tree column configure 2 -text [mc Mana] -justify right
	$tree column configure 3 -text [mc Fail] -justify right
	$tree column configure 4 -text [mc Comment]

	$tree configure -showheader yes -width 400
	scan [$tree column bbox 0] "%d %d %d %d" left top right bottom
	$tree configure -height [expr {([font metrics [Value font,magic] -linespace] + 8) * 10 + ($bottom - $top)}]

	# Add text element for char) in Name column
	$tree element create eTxt1 text -fill White
	$tree style elements s0 {eSel.e eTxt1 eTxt}
	$tree style layout s0 eSel.e -union {eTxt1 eTxt}
	$tree style layout s0 eTxt1 -padx {6 0}
	$tree style layout s0 eTxt -padx {0 6}

	NSList::OnInvoke $tree \
		"NSMindcraftWindow::Invoke $oop %r"

	Info $oop tree $tree

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

	#
	# Context Menu
	#

	set m $win.context
	menu $m -tearoff 0
	bind $tree <Button-3> "NSMindcraftWindow::ContextMenu $oop $m %X %Y"

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

# NSMindcraftWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			SetList $oop
		}
		postDisplay {
		}
	}

	return
}

# NSMindcraftWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::Close {oop} {

	angband keypress \033
}

# NSMindcraftWindow::Invoke --
#
#	When a Mindcraft power is double-clicked, "angband keypress" the
#	char.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::Invoke {oop row} {

	if {[angband inkey_flags] ne "INKEY_MINDCRAFT"} return

	set power [lindex [angband mindcraft get] $row]
	angband mindcraft info $power attrib
	angband keypress $attrib(char)

	return
}

# NSMindcraftWindow::ContextMenu --
#
#	When a power is right-clicked, pop up a context menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::ContextMenu {oop menu x y} {

	variable Priv

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

	set power [lindex [angband mindcraft get] $row]
	angband mindcraft info $power attrib

	$menu add command -label [mc "Select This Power"] \
		-command "angband keypress $attrib(char)" -font [BoldFont $font]
	$menu add separator
	$menu add command -label [mc Close] -command {angband keypress \033}
	$menu add separator
	$menu add command -label [mc Cancel]

	# Pop up the menu
	tk_popup $menu $x $y

	return
}

# NSMindcraftWindow::SetList --
#
#	Fill the list with castable Mindcraft powers.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMindcraftWindow::SetList {oop} {

	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]

	NSList::Clear $tree

	set font [Value font,magic]

	# Get a list of Mindcraft indexes
	set powerList [angband mindcraft get]

	# Check each power
	foreach power $powerList {

		# Get power info
		angband mindcraft info $power attrib

		# Can't use this power
		if {!$attrib(okay)} continue

		# Append power to the list
		set item [NSList::NewItem $tree]
		$tree item element configure $item 0 eTxt1 -text "$attrib(char)\) "
		NSList::SetText $tree $item $attrib(name)
		NSList::SetTextEx $tree $item 1 $attrib(level)
		NSList::SetTextEx $tree $item 2 $attrib(mana)
		NSList::SetTextEx $tree $item 3 $attrib(fail)
		NSList::SetTextEx $tree $item 4 $attrib(comment)
	}

	return
}

