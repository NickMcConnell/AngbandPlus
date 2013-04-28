# File: choose-monster.tcl

# Purpose: the Choose-Monster Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSChooseMonster {

	variable Priv

# namespace eval NSChooseMonster
}

# NSChooseMonster::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::InitModule {} {

	variable Priv

	MsgCatInit know

	NSModule::LoadIfNeeded NSList

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	NSObject::New NSChooseMonster

	return
}

# NSChooseMonster::NSChooseMonster --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::NSChooseMonster {oop} {

	InitWindow $oop

	Info $oop group,current -1
	Info $oop member,current -1

	set win [Info $oop win]

	# The user can type ESCAPE to cancel
	# The user can type ENTER to use the selected monster (if any)
	# The user can type the d_char of any monster race (if valid)
	bind $win <KeyPress-Escape> \
		"NSChooseMonster::Close $oop"
	bind $win <KeyPress-Return> \
		"NSChooseMonster::Accept $oop"
	bind $win <KeyPress> \
		"NSChooseMonster::TrySymbol $oop %A"

	# Searching
	bind $win <Control-KeyPress-f> \
		"NSChooseMonster::Find $oop 0"
	bind $win <Control-KeyPress-g> \
		"NSChooseMonster::Find $oop 1"

	NSWindowManager::RegisterWindow choosemonster $win \
		"GetDefaultGeometry $win reqwidth main" "" \
		"NSChooseMonster::DisplayCmd $oop"		

	# Update ourself when the font,knowledge value changes
	qebind NSChooseMonster <Value-font,knowledge> \
		"NSChooseMonster::ValueChanged_font_knowledge $oop"

	#
	# Global list of application windows
	#

	Global choosemonster,oop $oop
	Window choosemonster $win

	return
}

# NSChooseMonster::~NSChooseMonster --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::~NSChooseMonster {oop} {

	return
}

# NSChooseMonster::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::Info {oop info args} {

	global NSChooseMonster

	# Verify the object
	NSObject::CheckObject NSChooseMonster $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSChooseMonster($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSChooseMonster($oop,$info)
			}
		}
	}

	return
}

# NSChooseMonster::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::InitWindow {oop} {

	set win .choosemonster$oop
	toplevel $win
	wm title $win "Choose Monster"

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSChooseMonster::Close $oop"

	Info $oop win $win

	InitMenus $oop

	MakeDivider $win.divider1 x

	#
	# Group List
	#

	set font [Value font,knowledge]
	set cw [font measure $font "W"]
	set width [CalcGroupListWidth $oop]

	set frame $win.frameGroup
	set tree [NSList::New $frame -icon yes]
	$tree configure -width $width -height [expr {40 * 5}]

	# Do something when selection changes
	NSList::OnSelection $tree \
		"NSChooseMonster::SelectionChanged_Group $oop %T %c %S %D"

	Info $oop group,tree $tree

	#
	# Member List
	#

	set width 350

	set frame $win.frameMember
	set tree [NSList::New $frame -icon yes]
	$tree configure -width $width -height 300

	# Do something when selection changes
	NSList::OnSelection $tree \
		"NSChooseMonster::SelectionChanged_Member $oop %T %c %S %D"

	# Do something when a selected member item double-clicked.
	NSList::OnInvoke $tree \
		"NSChooseMonster::Invoke_Member $oop %r"

	Info $oop member,tree $tree

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0
	grid rowconfig $win 1 -weight 1
	grid columnconfig $win 0 -weight 0
	grid columnconfig $win 1 -weight 1

	grid $win.divider1 \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frameGroup \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $win.frameMember \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky news

	return
}

# NSChooseMonster::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::InitMenus {oop} {

	global NSMenu

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSChooseMonster::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbar $mbar

	#
	# Choose
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_CHOOSE
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_CHOOSE -label "Choose" -underline 0 \
		-identifier M_CHOOSE

	set entries {}
	lappend entries [list -type command -label [mc Find...] \
		-command "NSChooseMonster::Find $oop 0" \
		-accelerator $mod+F -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-command "NSChooseMonster::Find $oop 1" \
		-accelerator $mod+G -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-command "NSChooseMonster::Close $oop" -underline 0 \
		-accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_CHOOSE $entries

	return
}

# NSChooseMonster::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::SetupMenus {oop mbarID} {

	lappend identList E_FIND E_FIND_AGAIN E_CLOSE

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSChooseMonster::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::DisplayCmd {oop message first args} {

	variable Priv

	set tree [Info $oop group,tree]

	switch -- $message {
		preDisplay {
			Info $oop allowUnique 0
			Info $oop returnIndex 0
			if {[llength $args]} {
				if {[lsearch -exact $args -unique] != -1} {
					Info $oop allowUnique 1
				}
				if {[lsearch -exact $args -r_idx] != -1} {
					Info $oop returnIndex 1
				}
			}

			# Hmmm. Do this because the canvas was resized above.
#			update idletasks

			SetHook $oop choose_hook_monster
		}
		postDisplay {
			focus $tree
		}
		postWithdraw {
			Unmap $oop
		}
	}

	return
}

# NSChooseMonster::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::Close {oop} {

	angband keypress \033

	return
}

# NSChooseMonster::Unmap --
#
#	Do something when unmapping the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::Unmap {oop} {

	NSList::Clear [Info $oop group,tree]
	NSList::Clear [Info $oop member,tree]

	return
}

# NSChooseMonster::SetHook --
#
#	Set the hook..
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::SetHook {oop hook} {

	Info $oop hook $hook
	SetList_Group $oop

	return
}

# NSChooseMonster::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::CallHook {oop message args} {

	return [uplevel #0 NSChooseMonster::[Info $oop hook] $oop $message $args]
}

# NSChooseMonster::SelectionChanged_Group --
#
#	When a "group" list item is selected, display members in the
#	"member" list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::SelectionChanged_Group {oop tree count select deselect} {

	# If nothing was selected, clear the member list
	if {![llength $select]} {
		NSList::Clear [Info $oop member,tree]
		Info $oop group,current -1
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop group,current $row

	# Display members in that group
	SetList_Member $oop $row

	return
}

# NSChooseMonster::SelectionChanged_Member --
#
#	Do something when a "member" list item is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::SelectionChanged_Member {oop tree count select deselect} {

	# Do nothing if no new row was selected
	if {![llength $select]} {
		Info $oop member,current -1
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop member,current $row

	# Call the hook to do stuff
	CallHook $oop select_member $row

	return
}

# NSChooseMonster::Invoke_Member --
#
#	Do something when a "member" list item is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::Invoke_Member {oop row} {

	# Sometimes return r_info[] index
	set r_idx [lindex [Info $oop member,match] $row]

	# Sometimes return r_info[] index
	if {[Info $oop returnIndex]} {
		angband keypress [format "%d\n" $r_idx]
		return
	}

	angband keypress [angband r_info set $r_idx d_char]

	return
}

# NSChooseMonster::Accept --
#
#	If the user types ENTER, and a monster is selected, then
#	"angband keypress" the monster's d_char.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::Accept {oop} {

	set row [Info $oop member,current]
	if {$row == -1} return

	# Get the r_info[] index
	set r_idx [lindex [Info $oop member,match] $row]

	# Sometimes return r_info[] index
	if {[Info $oop returnIndex]} {
		angband keypress [format "%d\n" $r_idx]
		return
	}

	angband keypress [angband r_info set $r_idx d_char]

	return
}

# NSChooseMonster::TrySymbol --
#
#	If the given symbol is a valid monster d_char, then
#	"angband keypress" it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::TrySymbol {oop symbol} {

	if {[Info $oop returnIndex]} return

	if {[string length $symbol] != 1} return

	# This is to overcome a limitation of the "quasi-boolean"
	# implementation, which must be one of "yes", "no" or "ignore"
	if {[Info $oop allowUnique]} {
		set allowUnique yes
	} else {
		set allowUnique no
	}

	set match [angband r_info find -d_char $symbol -limit 1 \
		-unique $allowUnique]
	if {![llength $match]} return
	angband keypress $symbol

	return
}

# NSChooseMonster::SetList_Group --
#
#	Set the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::SetList_Group {oop} {

	set win [Info $oop win]
	set treeG [Info $oop group,tree]

	# Clear the list
	NSList::Clear $treeG

	# Call hook to set the group list
	CallHook $oop set_list_group

	Info $oop group,current -1

	# Hack -- Clear the "member" list
#	NSList::Clear [Info $oop member,tree]

	return
}

# NSChooseMonster::SetList_Member --
#
#	Set the member list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::SetList_Member {oop group} {

	set tree [Info $oop member,tree]

	# Clear the list
	NSList::Clear $tree

	Info $oop member,current -1

	# Call hook to set the member list
	CallHook $oop set_list_member $group

	return
}

# NSChooseMonster::Find --
#
#	Simple search routine to look for a member by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::Find {oop again} {

	variable Priv

	set treeG [Info $oop group,tree]
	set treeM [Info $oop member,tree]

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a name
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) -prompt Name \
			-buttons [list [mc Find] [mc Cancel]] -parent [Info $oop win]]
		if {![string length $string]} return

		# Clean up after the dialog
		update

		# Reset search parameters
		set Priv(find,string) $string
	}

	# Default to searching from the beginning
	set group 0
	set member 0

	# Search in selected group, if any
	if {!$Priv(find,fromStart)} {	
		set groupCurr [Info $oop group,current]
		set memberCurr [Info $oop member,current]
		if {$groupCurr != -1} {
			set group $groupCurr
		}
		if {$memberCurr != -1} {
			set member [expr {$memberCurr + 1}]
		}
	}

	set max [expr {[$treeG numitems] - 1}]

	# Compare lowercase
	set string [string tolower $string]

	for {set i $group} {$i < $max} {incr i} {

		set match [lrange [CallHook $oop member_list $i] $member end]

		foreach index $match {

			# Get the member name
			set name2 [CallHook $oop member_name $index]

			# Compare lowercase
			set name2 [string tolower $name2]

			# Found a match
			if {[string first $string $name2] != -1} {

				# The new group is not displayed
				if {![$treeG selection includes "root child $i"]} {

					# Select the new group. As a side effect, the
					# SetList_Member() is called to display the monsters in
					# the group.
					NSList::Activate $treeG "root child $i"

				# The new group is already selected
				} else {
				}

				# Select the matching member, deselecting all others
				NSList::Activate $treeM "root child $member"

				# Don't search from start next time
				set Priv(find,fromStart) 0

				# Done
				return
			}
			incr member
		}
		set member 0
	}

	# If we didn't search from the start, then wrap around
	if {!$Priv(find,fromStart)} {
		set Priv(find,fromStart) 1
		Find $oop 1
		return
	}

	return
}

proc NSChooseMonster::choose_hook_monster {oop message args} {

	switch -- $message {

		set_list_group {

			set treeG [Info $oop group,tree]

			foreach {title findSpec} [Global groups,r_info] {

				# Skip unique monster group
				if {![Info $oop allowUnique]} {
					if {[string first "-unique yes" $findSpec] != -1} continue
				}

				# Find the last monster in the group
				set match [angband r_info find -limit 1 \
					-backwards -unique no {*}$findSpec]

				# Get the icon
				set icon [angband r_info info [lindex $match 0] icon]

				# Add this group to the list
				set item [$treeG item create]
				NSList::SetIcon $treeG $item $icon
				NSList::SetText $treeG $item [mc $title]
				$treeG item lastchild root $item
			}
		}

		set_list_member {

			set treeM [Info $oop member,tree]

			set group [lindex $args 0]
			set findSpec [lindex [Global groups,r_info] [expr {$group * 2 + 1}]]

			# Get a list of monsters in the group
			set match [angband r_info find -unique no {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the icon and name
				set d_char [angband r_info set $index d_char]
				set icon [angband r_info info $index icon]
				set name [angband r_info info $index name]
				if {[Info $oop returnIndex]} {
					append name " ($d_char)"
				}

				# Append match to the list
				set item [$treeM item create]
				NSList::SetIcon $treeM $item $icon
				NSList::SetText $treeM $item $name
				$treeM item lastchild root $item
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set r_idx [lindex [Info $oop member,match] $row]
			NSRecall::RecallMonster $r_idx
		}

		group_names {
			set names {}
			foreach {title findSpec} [Global groups,r_info] {
				lappend names $title
			}
			return $names
		}

		member_name {
			return [angband r_info info [lindex $args 0] name]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex [Global groups,r_info] [expr {$group * 2 + 1}]]
			return [angband r_info find -unique no {*}$findSpec]
		}
	}

	return
}

# NSChooseMonster::CalcGroupListWidth --
#
#	Returns the desired width of the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::CalcGroupListWidth {oop} {

	variable Priv

	# Get the desired font
	set font [Value font,knowledge]

	set maxWidth 100

	foreach hook choose_hook_monster {
		foreach name [$hook $oop group_names] {
			set width [font measure $font $name]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	return [expr {[icon width] + 8 + $maxWidth + 20 + 4}]
}

# NSChooseMonster::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Knowledge Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseMonster::ValueChanged_font_knowledge {oop} {

	# Get the desired font
	set font [Value font,knowledge]

	# Set the width of the group list
	set treeG [Info $oop group,tree]
	set oldWidth [winfo width $treeG]
	set newWidth [CalcGroupListWidth $oop]
	$treeG configure -width $newWidth

	# Hack -- Resize the toplevel so the member list is not resized
	set diff [expr {$newWidth - $oldWidth}]
	if {$diff} {
		set win [Info $oop win]
		set newWidth [expr {[winfo width $win] + $diff}]
		NSToplevel::SetTotalWidth $win $newWidth
	}

	return
}

