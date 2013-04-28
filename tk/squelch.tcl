# File: squelch.tcl

# Purpose: the Squelch Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSSquelch {

	variable MenuString
	variable Priv

# namespace eval NSSquelch
}

# NSSquelch::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::InitModule {} {

	variable Priv

	MsgCatInit know squelch

	InitImageIfNeeded Image_Sound sound.gif

	NSModule::LoadIfNeeded NSList

	set Priv(page) {}
	lappend Priv(page) page_armor [mc Armor]
	lappend Priv(page) page_weapon [mc Weapons]
	lappend Priv(page) page_other [mc Other]

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	# Split global k_info groups into our Armor/Weapon/Other groups
	foreach {title findSpec} [Global groups,k_info] {
		switch -glob -- $findSpec {
			*TV_DIGGING* -
			*TV_SWORD* -
			*TV_POLEARM* -
			*TV_HAFTED* -
			*TV_BOW* -
			*TV_ARROW* {
				lappend Priv(group,weapon) $title $findSpec
			}
			*TV_SHIELD* -
			*TV_CROWN* -
			*TV_HELM* -
			*TV_GLOVES* -
			*TV_BOOTS* -
			*TV_CLOAK* -
			*TV_DRAG_ARMOR* -
			*TV_HARD_ARMOR* -
			*TV_SOFT_ARMOR* {
				lappend Priv(group,armor) $title $findSpec
			}
			default {
				lappend Priv(group,other) $title $findSpec
			}
		}
	}

	# Create the Squelch Window
	NSObject::New NSSquelch

	return
}

# NSSquelch::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::CloseModule {} {

	catch {
		destroy [Window squelch]
	}

	return
}

# NSSquelch::NSSquelch --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::NSSquelch {oop} {

	variable Priv

	Info $oop page [lindex $Priv(page) 0]

	# Info about selected group & member for each page
	foreach {page label} $Priv(page) {
		Info $oop group,current,$page [list 0]
		Info $oop member,current,$page {}
	}

	InitWindow $oop

	# Currently-selected groups and members
	Info $oop group,current {}
	Info $oop member,current {}

	set win [Info $oop win]

	NSWindowManager::RegisterWindow squelch $win \
		"GetDefaultGeometry $win main main2" "NSSquelch::SetupCmd $oop" \
		"NSSquelch::DisplayCmd $oop"		

	# Update ourself when the font,knowledge value changes
	qebind NSSquelch <Value-font,knowledge> \
		"NSSquelch::ValueChanged_font_knowledge $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSSquelch $oop $win

	bind $win <KeyPress-Escape> "NSSquelch::Close $oop"
	bind $win <Control-KeyPress-w> "NSSquelch::Close $oop"
	bind $win <KeyPress-f> "NSSquelch::Find $oop 0"
	bind $win <KeyPress-g> "NSSquelch::Find $oop 1"

	#
	# Global access
	#

	Window squelch $win
	Global squelch,oop $oop

	return
}

# NSSquelch::~NSSquelch --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::~NSSquelch {oop} {

	return
}

# NSSquelch::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::Info {oop info args} {

	global NSSquelch

	# Verify the object
	NSObject::CheckObject NSSquelch $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSSquelch($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSSquelch($oop,$info)
			}
		}
	}

	return
}

# NSSquelch::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::InitWindow {oop} {

	variable Priv

	set win .squelch$oop
	toplevel $win
	wm title $win [mc Squelch]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSSquelch::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Tabs!
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach {page label} $Priv(page) {
		NSTabs::Add $tabsId $label
	}
	NSTabs::Info $tabsId invokeCmd "NSSquelch::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Group List
	#

	set width [CalcGroupListWidth $oop]

	set frame $win.frameGroup
	set tree [NSList::New $frame -icon yes]
	$tree configure -width $width -height [expr {40 * 5}] -selectmode extended

	# Do something when the selection changes
	NSList::OnSelection $tree \
		"NSSquelch::SelectionChanged_Group $oop %T %c %S %D"

	Info $oop group,tree $tree

	#
	# Member List
	#

	set width 300

	set frame $win.frameMember
	set tree [NSList::New $frame -icon yes -columns 2]
	$tree configure -width $width -height [expr {40 * 5}] -selectmode extended

	# Do something when the selection changes
	NSList::OnSelection $tree \
		"NSSquelch::SelectionChanged_Member $oop %T %c %S %D"

	Info $oop member,tree $tree

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 0 -minsize 0
	grid rowconfig $win 2 -weight 1 -minsize 0
	grid rowconfig $win 3 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 0 -minsize 0
	grid columnconfig $win 1 -weight 1 -minsize 0
 
	if {[Platform windows]} {
		grid [MakeDivider $win.divider2 x] \
			-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew -pady 2
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frameGroup \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $win.frameMember \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	return
}

# NSSquelch::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::InitMenus {oop} {

	variable MenuString
	variable Priv

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSSquelch::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSSquelch::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSSquelch::MenuInvoke $oop"

	#
	# Squelch
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SQUELCH
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_SQUELCH -label [mc Squelch] -underline 0 \
		-identifier M_SQUELCH

	set entries {}
	set i 1
	foreach {page label} $Priv(page) {
		lappend entries [list -type radiobutton -label $label \
			-variable ::NSSquelch($oop,radio,page) -value $page \
			-accelerator $i -identifier E_PAGE_$i]
		bind $win <KeyPress-$i> "NSSquelch::SetPage $oop $page"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Save As Default"] \
		-underline 0 -identifier E_SAVE]
	lappend entries [list -type command -label [mc "Load Default"] \
		-underline 0 -identifier E_LOAD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_SQUELCH $entries

	#
	# Mode
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_MODE
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_MODE -label [mc Mode] -underline 0 \
		-identifier M_MODE

	set entries {}
	if {[variant OANGBANDTK]} {
		lappend data \
			None NONE none n \
			Cursed CURSED cursed c \
			Average AVERAGE average v \
			Good GOOD good d \
			All ALL all a
	} else {
		lappend data \
			None NONE none n \
			Cursed CURSED cursed c \
			Average AVERAGE average v \
			Good GOOD good d \
			Excellent EXCELLENT excellent e \
			All ALL all a
	}
	foreach {label ident mode key} $data {
		lappend entries [list -type radiobutton -label [mc $label] \
			-variable ::NSSquelch($oop,radio,mode) -value $mode \
			-accelerator $key -underline 0 -identifier E_MODE_$ident]
		bind $win <KeyPress-$key> "NSMenu::TryInvoke $mbar E_MODE_$ident"
		set MenuString(E_MODE_$ident) "Sets squelching mode to $mode."
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Select All"] \
		-accelerator $mod+A -underline 0 -identifier E_SELECT_ALL]
	bind $win <Control-KeyPress-$key> "NSMenu::TryInvoke $mbar E_SELECT_ALL"

	NSMenu::MenuInsertEntries $mbar -end MENU_MODE $entries

	set MenuString(M_SQUELCH) \
		"Contains commands for ..."
	set MenuString(E_FIND) \
		"Searches for a member by name."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSSquelch::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SetupMenus {oop mbarID} {

	variable Priv

	lappend identList E_FIND E_FIND_AGAIN E_CLOSE

	set i 0
	foreach {page label} $Priv(page) {
		lappend identList E_PAGE_[incr i]
	}

	Info $oop radio,mode ""
	set numGroup [llength [Info $oop group,current]]
	set numMember [llength [Info $oop member,current]]
	if {($numGroup > 1) || ($numGroup == 1 && $numMember)} {
		lappend identList E_MODE_NONE E_MODE_ALL
		switch -- [Info $oop page] {
			page_armor -
			page_weapon {
				lappend identList E_MODE_CURSED E_MODE_AVERAGE E_MODE_GOOD
				if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
					lappend identList E_MODE_EXCELLENT
				}
			}
			page_other {
			}
		}

		# If a single member is selected, then set the "Mode" radio entry
		if {$numMember == 1} {
			set group [lindex [Info $oop group,current] 0]
			set member [lindex [Info $oop member,current] 0]
			set k_idx [lindex [CallHook $oop member_list $group] $member]
			Info $oop radio,mode [angband k_info set $k_idx squelch]
		}
	}
	lappend identList E_SELECT_ALL

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSSquelch::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		E_PAGE_* {
			set desc "Displays this page."
		}

		default {
			if {[info exists MenuString($ident)]} {
				set desc $MenuString($ident)
			} else {
				set menu [NSMenu::Info $menuId menu]
				set desc [$menu entrycget $index -label]
			}
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

# NSSquelch::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_PAGE_* {
			set page [Info $oop radio,page]
			SetPage $oop $page
		}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_SAVE {}
		E_LOAD {}
		E_CLOSE {Close $oop}

		E_MODE_* {
			set mode [Info $oop radio,mode]
			set selGroup [Info $oop group,current]
			set treeM [Info $oop member,tree]
			if {[llength $selGroup] == 1} {
				foreach row [Info $oop member,current] {
					set k_idx [lindex [Info $oop member,match] $row]
					angband k_info set $k_idx squelch $mode
					NSList::SetTextEx $treeM "root child $row" 1 $mode
				}
			} else {
				foreach row $selGroup {
					foreach k_idx [CallHook $oop member_list $row] {
						angband k_info set $k_idx squelch $mode
					}
				}
			}
		}

		E_SELECT_ALL {
			set selGroup [Info $oop group,current]
			if {[llength $selGroup] == 1} {
				set treeM [Info $oop member,tree]
				NSList::SelectAll $treeM
			} else {
				set treeG [Info $oop group,tree]
				NSList::SelectAll $treeG
			}
		}
	}

	return
}

# NSSquelch::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::DisplayCmd {oop message first args} {

	variable Priv

	switch -- $message {
		preDisplay {
			SetPage $oop [Info $oop page]
		}
		postDisplay {
		}
		postWithdraw {
			StateRemember $oop
			NSList::Clear [Info $oop group,tree]
			NSList::Clear [Info $oop member,tree]
		}
	}

	return
}

# NSSquelch::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SetupCmd {oop} {

	return
}

# NSSquelch::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::Close {oop} {

	NSWindowManager::Undisplay squelch

	return
}

# NSSquelch::StateRemember --
#
#	Remember the group/member selections.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::StateRemember {oop} {

	if {!$NSWindowManager::Priv(squelch,setup)} return

	set page [Info $oop page]

	# Because we are clearing the lists here, and don't want to
	# upset the user, I save the selected group/member so it can
	# be restored in StateRestore() below.
	Info $oop group,current,$page [Info $oop group,current]
	Info $oop member,current,$page [Info $oop member,current]

	return
}

# NSSquelch::StateRestore --
#
#	Restore the group/member selections.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::StateRestore {oop} {

	set page [Info $oop page]

	# Restore the selected group
	set current [Info $oop group,current,$page]
	set treeG [Info $oop group,tree]
	if {[llength $current]} {
		set items {}
		foreach row $current {
			lappend items "root child $row"
		}
		$treeG selection modify $items {}
		$treeG activate [lindex $items end]
		$treeG see active
	}

	# Restore the selected member
	set current [Info $oop member,current,$page]
	set treeM [Info $oop member,tree]
	if {[llength $current]} {
		set items {}
		foreach row $current {
			lappend items "root child $row"
		}
		$treeM selection modify $items {}
		$treeM activate [lindex $items end]
		$treeM see active
	}

	return
}

# NSSquelch::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::CallHook {oop message args} {

	return [[Info $oop page] $oop $message {*}$args]
}

# NSSquelch::SetPage --
#
#	Set the page.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SetPage {oop page} {

	variable Priv

	if {[string compare $page [Info $oop page]]} {
		set again 0
	} else {
		set again 1
	}

	if {!$again} {
		StateRemember $oop
	}

	# Remember the page
	Info $oop page $page

	# Set the group list
	SetList_Group $oop

	# Radiobutton menu entries
	Info $oop radio,page $page

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [expr {[lsearch -exact $Priv(page) $page] / 2}]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	StateRestore $oop

	return
}

# NSSquelch::SelectionChanged_Group --
#
#	When a "group" list item is selected, display members in the
#	"member" list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SelectionChanged_Group {oop tree count select deselect} {

	set selection {}
	foreach item [$tree selection get] {
		lappend selection [NSList::Item2Row $tree $item]
	}

	# Remember the selected groups
	Info $oop group,current $selection

	if {$count == 1} {

		# Get the row
		set row [lindex $selection 0]

		# Display members in that group
		SetList_Member $oop $row

	# Not 1 group selected, clear the member list
	} else {
		NSList::Clear [Info $oop member,tree]
	}

	return
}

# NSSquelch::SelectionChanged_Member --
#
#	Do something when a "member" list item is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SelectionChanged_Member {oop tree count select deselect} {

	set win [Info $oop win]

	set selection {}
	foreach item [$tree selection get] {
		lappend selection [NSList::Item2Row $tree $item]
	}

	Info $oop member,current $selection

	# One item is selected
	if {$count == 1} {

		# Get the row
		set row [lindex $selection 0]

		# Call the hook to do other stuff (recall, etc)
		CallHook $oop select_member $row

		# Done
		return

	} else {

		$win.statusBar itemconfigure t2 -text ""
	}

	return
}

# NSSquelch::SetList_Group --
#
#	Set the list of groups.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SetList_Group {oop} {

	set win [Info $oop win]
	set tree [Info $oop group,tree]

	# Feedback
	StatusBar $oop "Displaying..." 0
	update idletasks

	# Clear the list
	NSList::Clear $tree

	# Call hook to set the group list
	CallHook $oop set_list_group

	Info $oop group,current {}

	# Feedback
	StatusBar $oop "Done." 1

	return
}

# NSSquelch::SetList_Member --
#
#	Set the list of members.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::SetList_Member {oop group} {

	set tree [Info $oop member,tree]

	# Clear the list
	NSList::Clear $tree

	Info $oop member,current {}

	# Call hook to set the member list
	CallHook $oop set_list_member $group

	$tree yview moveto 0.0

	return
}

# NSSquelch::CalcGroupListWidth --
#
#	Returns the desired width of the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::CalcGroupListWidth {oop} {

	variable Priv

	# Get the desired font
	set font [Value font,knowledge]

	# Minimum desired width of the group list
	set maxWidth 100

	# Check each hook
	foreach {page title} $Priv(page) {

		# Check each name
		foreach name [$page $oop group_names] {

			# Calculate the width in pixels
			set width [font measure $font $name]

			# Remember the maximum width
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	return [expr {[icon width] + 8 + $maxWidth + 20 + 4}]
}

# NSSquelch::Find --
#
#	Simple search routine to look for a member by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::Find {oop again} {

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
			-initial $Priv(find,string) -prompt [mc find-prompt] \
			-buttons [list [mc Find] [mc Cancel]] -parent [Info $oop win]]

		# User cancelled
		if {![string length $string]} return

		# Clean up after the dialog, give message
		StatusBar $oop "Searching..." 1
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
		if {$groupCurr != {}} {
			set group [lindex $groupCurr 0]
		}
		if {$memberCurr != {}} {
			set member [expr {[lindex $memberCurr 0] + 1}]
		}
	}

	set max [expr {[$treeG numitems] - 1}]

	# Compare lowercase
	set string [string tolower $string]

	# Check each group
	for {set i $group} {$i < $max} {incr i} {

		# Get a list of member indexes
		set match [lrange [CallHook $oop member_list $i] $member end]

		# Check each member index
		foreach index $match {

			# Get the member name
			set name2 [CallHook $oop member_name $i $index]

			# Compare lowercase
			set name2 [string tolower $name2]

			# Found a match
			if {[string first $string $name2] != -1} {

				# The new group is not displayed
				if {[$treeG selection count] != 1 ||
					![$treeG selection includes "root child $i"]} {

					# Select the new group. As a side effect, the
					# SetList_Member() command is called to display
					# the monsters in the group.
					NSList::Activate $treeG "root child $i"

				# The new group is already selected
				} else {
				}

				# Select the matching member, deselecting all others
				NSList::Activate $treeM "root child $member"

				# Don't search from start next time
				set Priv(find,fromStart) 0

				# Clear "Searching..." message
				StatusBar $oop "" 0

				# Done
				return
			}

			# Next row
			incr member
		}

		# First row
		set member 0
	}

	# If we didn't search from the start, then wrap around
	if {!$Priv(find,fromStart)} {
		set Priv(find,fromStart) 1
		Find $oop 1
		return
	}

	StatusBar $oop "No match for \"$string\"." 1

	return
}

# NSSquelch::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSSquelch::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetPage $oop [lindex $Priv(page) [expr {$index * 2}]]

	return
}

# NSSquelch::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Assign Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSquelch::ValueChanged_font_knowledge {oop} {

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

proc NSSquelch::page_armor {oop message args} {

	variable Priv

	switch -- $message {

		set_list_group {

			set treeG [Info $oop group,tree]

			foreach {title findSpec} $Priv(group,armor) {

				# Find the last monster in the group
				set match [angband k_info find -limit 1 \
					-backwards {*}$findSpec]

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info [lindex $match 0] icon]

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
			set findSpec [lindex $Priv(group,armor) [expr {$group * 2 + 1}]]

			# Get a list of monsters in the group
			set match [angband k_info find {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info $index icon]

				# Hack -- object_desc
				set name [angband k_info info $index object_desc]

				set squelch [angband k_info set $index squelch]

				# Add this member to the list
				set item [$treeM item create]
				NSList::SetIcon $treeM $item $icon
				NSList::SetText $treeM $item $name
				NSList::SetTextEx $treeM $item 1 $squelch
				$treeM item lastchild root $item
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
[Info $oop win].statusBar itemconfigure t2 -text $index
			NSRecall::RecallObjectKind $index
		}

		group_names {
			set names {}
			foreach {title findSpec} $Priv(group,armor) {
				lappend names [mc $title]
			}
			return $names
		}

		group_list {
			set index -1
			set result {}
			foreach {title findSpec} $Priv(group,armor) {
				lappend result [incr index]
			}
			return $result
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			return [angband k_info info $index object_desc]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex $Priv(group,armor) [expr {$group * 2 + 1}]]
			return [angband k_info find {*}$findSpec]
		}
	}

	return
}

proc NSSquelch::page_weapon {oop message args} {

	variable Priv

	switch -- $message {

		set_list_group {

			set treeG [Info $oop group,tree]

			foreach {title findSpec} $Priv(group,weapon) {

				# Find the last monster in the group
				set match [angband k_info find -limit 1 \
					-backwards {*}$findSpec]

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info [lindex $match 0] icon]

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
			set findSpec [lindex $Priv(group,weapon) [expr {$group * 2 + 1}]]

			# Get a list of monsters in the group
			set match [angband k_info find {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info $index icon]

				# Hack -- object_desc
				set name [angband k_info info $index object_desc]

				set squelch [angband k_info set $index squelch]

				# Add this member to the list
				set item [$treeM item create]
				NSList::SetIcon $treeM $item $icon
				NSList::SetText $treeM $item $name
				NSList::SetTextEx $treeM $item 1 $squelch
				$treeM item lastchild root $item
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
			NSRecall::RecallObjectKind $index
		}

		group_names {
			set names {}
			foreach {title findSpec} $Priv(group,weapon) {
				lappend names [mc $title]
			}
			return $names
		}

		group_list {
			set index -1
			set result {}
			foreach {title findSpec} $Priv(group,weapon) {
				lappend result [incr index]
			}
			return $result
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			return [angband k_info info $index object_desc]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex $Priv(group,weapon) [expr {$group * 2 + 1}]]
			return [angband k_info find {*}$findSpec]
		}
	}

	return
}

proc NSSquelch::page_other {oop message args} {

	variable Priv

	switch -- $message {

		set_list_group {

			set treeG [Info $oop group,tree]

			foreach {title findSpec} $Priv(group,other) {

				# Find the last (non insta-art) object in the group
				set match [angband k_info find -backwards -insta_art no \
					-limit 1 {*}$findSpec]

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info $match icon]

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
			set findSpec [lindex $Priv(group,other) [expr {$group * 2 + 1}]]

			# Keep a list of matching indexes
			set match {}

			# Collect info for each row
			set itemList {}

			# Add each match to the list
			foreach index [angband k_info find -insta_art no {*}$findSpec] {

				if {![angband k_info set $index flavor] ||
					[angband k_info set $index aware]} {

					# Get the assignment. This resolves alternates.
					set icon [angband k_info info $index icon]
				} else {
					set last [angband k_info find -backwards -limit 1 \
						-insta_art no {*}$findSpec]
					set icon [angband k_info info $last icon]
				}

				# Hack -- object_desc
				set name [angband k_info info $index object_desc]

				set squelch [angband k_info set $index squelch]

				# Add this member to the list
				set item [$treeM item create]
				NSList::SetIcon $treeM $item $icon
				NSList::SetText $treeM $item $name
				NSList::SetTextEx $treeM $item 1 $squelch
				$treeM item lastchild root $item

				# Keep a list of matching indexes
				lappend match $index
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
			NSRecall::RecallObjectKind $index
		}

		group_names {
			set names {}
			foreach {title findSpec} $Priv(group,other) {
				lappend names [mc $title]
			}
			return $names
		}

		group_list {
			set index -1
			set result {}
			foreach {title findSpec} $Priv(group,other) {
				lappend result [incr index]
			}
			return $result
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			return [angband k_info info $index object_desc]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex $Priv(group,other) [expr {$group * 2 + 1}]]
			return [angband k_info find -insta_art no {*}$findSpec]
		}
	}

	return
}

