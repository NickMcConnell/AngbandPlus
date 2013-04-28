# File: pets.tcl

# Purpose: the Pets Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPets {

	variable Priv

# namespace eval NSPets
}

# NSPets::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::InitModule {} {

	MsgCatInit pet

	NSModule::LoadIfNeeded NSList

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSToolbar

	NSObject::New NSPets

	return
}

# NSPets::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::CloseModule {} {

	catch {
		destroy [Window pets]
	}

	return
}

# NSPets::NSPets --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::NSPets {oop} {

	variable Priv

	InitWindow $oop

	Info $oop current -1

	set win [Info $oop win]

	NSWindowManager::RegisterWindow pets [Info $oop win] \
		"GetDefaultGeometry $win reqwidth main2" "" \
		"NSPets::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSPets $oop $win

	#
	# Global list of application windows
	#

	Global pets,oop $oop
	Window pets [Info $oop win]

	return
}

# NSPets::~NSPets --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::~NSPets {oop} {

	return
}

# NSPets::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::Info {oop info args} {

	global NSPets

	# Verify the object
	NSObject::CheckObject NSPets $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPets($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPets($oop,$info)
			}
		}
	}

	return
}

# NSPets::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::InitWindow {oop} {

	variable Priv

	set win .pets$oop
	toplevel $win
	wm title $win [mc Pets]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSPets::Close $oop"

	Info $oop win $win

	#
	# Menus
	#

	InitMenus $oop

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
	set tree [NSList::New $frame -columns 4 -icon yes]
	$tree configure -showheader yes -width 400
	$tree column configure 0 -text [mc Name]
	$tree column configure 1 -text [mc Items] -justify right
	$tree column configure 2 -text [mc Upkeep] -justify right
	$tree column configure 3 -text [mc Status]
	for {set C 0} {$C < 4} {incr C} {
		$tree style layout s$C eTxt -padx 6
	}

	# When a pet is selected, recall it
	NSList::OnSelection $tree \
		"NSPets::SelectionChanged $oop %T %c %S %D"

	Info $oop tree $tree

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew -pady 0
	grid $win.frame \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# Context Menu
	#

	set m $win.context
	menu $m -tearoff 0
	bind $tree <ButtonPress-3> \
		"NSPets::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $tree
	bind $win <Control-KeyPress-w> "NSPets::Close $oop"

	return
}

# NSPets::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::InitMenus {oop} {

	global NSPets
	global NSMenu
	variable Priv

	set win [Info $oop win]

	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSPets::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSPets::MenuSelect $oop"

	#
	# Pets Menu
	#

	set menuId [NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_PETS]
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_PETS -label [mc Pets] -underline 0 -identifier M_PETS

	set entries {}
	lappend entries [list -type command -label [mc "Dismiss Selected"] \
		-command "NSPets::Dismiss $oop selected" -underline 0 \
		-identifier E_DISMISS_SELECTED]
	lappend entries [list -type command -label [mc "Dismiss All"] \
		-command "NSPets::Dismiss $oop all" -underline 8 \
		-identifier E_DISMISS_ALL]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-command "NSPets::Close $oop" -underline 0 \
		-accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_PETS $entries

	#
	# Command Menu
	#

	set menuId [NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_COMMAND]
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_COMMAND -label [mc Command] -underline 0 -identifier M_COMMAND

	set entries {}
	lappend entries [list -type radiobutton -label [mc "Stay Close"] \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_CLOSE_DIST] -identifier E_CLOSE_DIST]
	lappend entries [list -type radiobutton -label [mc "Follow Me"] \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_FOLLOW_DIST] -identifier E_FOLLOW]
	lappend entries [list -type radiobutton -label [mc "Seek And Destroy"] \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_DESTROY_DIST] -identifier E_DESTROY]
	lappend entries [list -type radiobutton -label [mc "Give Me Space"] \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_SPACE_DIST] -identifier E_SPACE]
	lappend entries [list -type radiobutton -label [mc "Stay Away"] \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_AWAY_DIST] -identifier E_AWAY]
	lappend entries [list -type separator]
	lappend entries [list -type checkbutton -label [mc "Open Doors"] \
		-underline 0 \
		-variable NSPets($oop,pet_open_doors) -identifier E_DOORS]
	lappend entries [list -type checkbutton -label [mc "Pick Up Items"] \
		-underline 0 \
		-variable NSPets($oop,pet_pickup_items) -identifier E_ITEMS]

	NSMenu::MenuInsertEntries $mbar -end MENU_COMMAND $entries

	return
}

# NSPets::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::SetupMenus {oop mbarID} {

	if {[Info $oop current] != -1} {
		lappend identList E_DISMISS_SELECTED
	}
	if {[llength [angband player pets]]} {
		lappend identList E_DISMISS_ALL
		set letters bcdefgh
	} else {
		set letters abcdefg
	}
	lappend identList E_CLOSE

	set index -1
	foreach ident [list E_CLOSE_DIST E_FOLLOW E_DESTROY E_SPACE \
		E_AWAY E_DOORS E_ITEMS] {
		lappend identList $ident
		set char [string index $letters [incr index]]
		NSMenu::EntryConfigure $mbarID $ident -accelerator $char \
			-command "angband keypress $char"
	}

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSPets::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::MenuSelect {oop menuId index ident} {

	switch -- $ident {
		{} {
			set desc {}
		}

		M_PETS {
			set desc "Contains commands for dismissing pets."
		}
		E_DISMISS_SELECTED {
			set desc "Dismisses selected pets."
		}
		E_DISMISS_ALL {
			set desc "Dismisses all pets."
		}
		E_CLOSE {
			set desc "Closes the window."
		}

		M_COMMAND {
			set desc "Contains commands for controlling pets."
		}
		E_CLOSE_DIST {
			set desc "Pets follow closely. (dist [const PET_CLOSE_DIST])"
		}
		E_FOLLOW {
			set desc "Pets follow nearby. (dist [const PET_FOLLOW_DIST])"
		}
		E_DESTROY {
			set desc "Pets hunt nearby monsters. (dist [const PET_DESTROY_DIST])"
		}
		E_SPACE {
			set desc "Pets keep a short distance away."
		}
		E_AWAY {
			set desc "Pets roam freely."
		}
		E_DOORS {
			if {[struct set player_type 0 pet_open_doors]} {
				set desc "Don't allow pets to open doors."
			} else {
				set desc "Allow pets to open doors."
			}
		}
		E_ITEMS {
			if {[struct set player_type 0 pet_pickup_items]} {
				set desc "Don't allow pets to pick up items."
			} else {
				set desc "Allow pets to pick up items."
			}
		}

		default {
			set menu [NSMenu::Info $menuId menu]
			set desc [$menu entrycget $index -label]
		}
	}

	set statusBar [Info $oop win].statusBar
	$statusBar cover set $desc
	if {($desc eq "") && ($menuId == [Info $oop mbarId])} {
		if {[$statusBar cover visible]} {
			$statusBar cover hide
		} else {
			$statusBar cover show
		}
	}

	return
}

# NSPets::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::DisplayCmd {oop message first} {

	variable Priv

	switch -- $message {
		preDisplay {

			# Synchronzize the radiobutton menu entries
			Info $oop pet_follow_distance \
				[struct set player_type 0 pet_follow_distance]

			# Synchronzize the checkbutton menu entries
			Info $oop pet_open_doors \
				[struct set player_type 0 pet_open_doors]
			Info $oop pet_pickup_items \
				[struct set player_type 0 pet_pickup_items]

			SetList $oop
		}
		postDisplay {
			focus [Info $oop tree]
		}
		postWithdraw {
			NSList::Clear [Info $oop tree]
		}
	}

	return
}

# NSPets::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::Close {oop} {

	angband keypress \033

	return
}

# NSPets::SelectionChanged --
#
#	When a list item is highlighted, display memory for the highlighted
#	pet in the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::SelectionChanged {oop tree count select deselect} {

	# Do nothing if no new row was selected
	if {![llength $select]} {
		Info $oop current -1
		StatusBar $oop "" 0
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop current $row

	set r_idx [lindex [Info $oop r_match] $row]

	# Display memory for the selected pet
	NSRecall::RecallMonster $r_idx

	return
}

# NSPets::CountPetObjects --
#
#	Return the number of objects carried by a pet.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::CountPetObjects {oop m_idx} {

	set numObjects 0

	# Get the o_list[] index of the first object carried
	set o_idx [angband m_list set $m_idx hold_o_idx]

	# Check the chain of objects being carried
	while {$o_idx} {

		# Get info about this object
		set o_idx [angband o_list set $o_idx next_o_idx]

		# One more object
		incr numObjects
	}

	return $numObjects
}

# NSPets::SetList --
#
#	Set the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::SetList {oop} {

	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]

	# Clear the list
	NSList::Clear $tree

	# Create a list of m_idx/r_idx pairs
	set match2 {}
	foreach m_idx [angband player pets] {
		set r_idx [angband m_list set $m_idx r_idx]
		lappend match2 [list $m_idx $r_idx]
	}

	# Sort based on increasing r_idx, then break up into
	# m_idx/r_idx lists

	set match [set r_match {}]
	if {[llength $match2]} {
		set match2 [lsort -integer -index 1 $match2]
		foreach elem $match2 {
			lappend match [lindex $elem 0]
			lappend r_match [lindex $elem 1]
		}
	}
	Info $oop match $match
	Info $oop r_match $r_match

	# Total upkeep
	set total_friend_levels 0

	# Add each pet to the list
	foreach m_idx [Info $oop match] r_idx [Info $oop r_match] {

		# Get icon and name
		set icon [angband r_info info $r_idx icon]
		set name [angband r_info info $r_idx name]

		# Count the objects carried by the pet
		set numObjects [CountPetObjects $oop $m_idx]

		set r_level [angband r_info set $r_idx level]

		# look_mon_desc(), but only if visible
		set status ""
		if {[angband m_list set $m_idx ml]} {
			set status [angband m_list info $m_idx look_mon_desc]
		}

		# Append the pet to the list
		set item [$tree item create]
		NSList::SetIcon $tree $item $icon
		NSList::SetTextEx $tree $item 0 $name
		NSList::SetTextEx $tree $item 1 $numObjects
		NSList::SetTextEx $tree $item 2 $r_level
		NSList::SetTextEx $tree $item 3 $status
		$tree item lastchild root $item

		incr total_friend_levels $r_level
	}

	if {[variant KANGBANDTK]} {
		set upkeep_factor 0
	}

	if {[variant ZANGBANDTK]} {

		# Calculate the upkeep
		set upkeep_factor 0
		set total_friends [llength [Info $oop match]]
		set pclass [struct set player_type 0 pclass]
		set pet_upkeep_div [struct set player_class $pclass pet_upkeep_div]
		if {$total_friends > 1 + ([angband player level] / $pet_upkeep_div)} {
			set upkeep_factor $total_friend_levels
			if {$upkeep_factor > 100} {
				set upkeep_factor 100
			} elseif {$upkeep_factor < 10} {
				set upkeep_factor 10
			}
		}
	}

	# Display the number of pets in the status bar
	set num [llength [Info $oop match]]
	if {$num == 1} {
		set s ""
	} else {
		set s s
	}
	$win.statusBar itemconfigure t2 \
		-text "$num pet$s  Upkeep: $upkeep_factor%"

	return
}

# NSPets::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSPets::Dismiss --
#
#	Dismisses pets. This command relies on some C code which accepts
#	a list of m_list[] indexes of pets which should be dismissed. This
#	allows us to dimiss a pet on a certain row of the list, all
#	selected pets, or all pets of a certain race.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::Dismiss {oop action args} {

	set tree [Info $oop tree]

	switch -- $action {
		all {
			# Dismiss all pets (confirm)
			angband keypress a
		}
		one {
			# Dismiss pet on given row (no confirm)
			set row [lindex $args 0]
			set m_idx [lindex [Info $oop match] $row]
			angband keypress ~$m_idx\n\033
		}
		race {
			# Dismiss pets with race of pet on given row (no confirm)
			set row [lindex $args 0]
			set r_idx [lindex [Info $oop r_match] $row]
			angband keypress ~
			foreach m_idx [Info $oop match] r_idx2 [Info $oop r_match] {
				if {$r_idx2 == $r_idx} {
					angband keypress $m_idx\n
				}
			}
			angband keypress \033
		}
		selected {
			# Dismiss selected pets (no confirm)
			angband keypress ~
			foreach item [$tree selection get] {
				set row [NSList::Item2Row $tree $item]
				set m_idx [lindex [Info $oop match] $row]
				angband keypress $m_idx\n
			}
			angband keypress \033
		}
	}

	return
}

# NSPets::ContextMenu --
#
#	When an list item is right-clicked, pop up a context menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::ContextMenu {oop menu x y} {

	set tree [Info $oop tree]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $tree]}]
	set y1 [expr {$y - [winfo rooty $tree]}]
	set row [NSList::Point2Row $tree $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	if {$row != -1} {
		$menu add command -label [mc "Dismiss This Pet"] \
			-command "NSPets::Dismiss $oop one $row"
		$menu add command -label [mc "Dismiss This Type"] \
			-command "NSPets::Dismiss $oop race $row"
	}
	if {[$tree selection count]} {
		$menu add command -label [mc "Dismiss Selected"] \
			-command "NSPets::Dismiss $oop selected"
	}
	if {[llength [angband player pets]]} {
		$menu add command -label [mc "Dismiss All"] \
			-command "NSPets::Dismiss $oop all"
	}
	if {[$menu index end] ne "none"} {
		$menu add separator
	}
	$menu add command -label [mc Close] \
		-command "NSPets::Close $oop"
	$menu add separator
	$menu add command -label [mc Cancel]

	# Pop up the menu
	tk_popup $menu $x $y

	return
}

