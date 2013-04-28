# File: debug-alloc.tcl

# Purpose: the Alloc Window for debugging

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSDebugAlloc {

	variable Priv

# namespace eval NSDebugAlloc
}

# NSDebugAlloc::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::InitModule {} {

	variable Priv

	MsgCatInit know

	NSModule::LoadIfNeeded NSList

	lappend Priv(hook) artifact
	lappend Priv(hook) monster
	lappend Priv(hook) object

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	# Create the DebugAlloc Window
	NSObject::New NSDebugAlloc

	return
}

# NSDebugAlloc::NSDebugAlloc --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::NSDebugAlloc {oop} {

	InitWindow $oop

	set win [Info $oop win]

	# Update ourself when the font,knowledge value changes
	qebind NSDebugAlloc <Value-font,knowledge> \
		"NSDebugAlloc::ValueChanged_font_knowledge $oop"

	qebind NSDebugAlloc <IconCfg> \
		"NSDebugAlloc::IconCfg $oop"

	Info $oop group,current -1
	Info $oop member,current -1

	bind $win <KeyPress-a> "NSDebugAlloc::MenuInvoke $oop 0 E_ALLOCATE"
	bind $win <KeyPress-Return> "NSDebugAlloc::MenuInvoke $oop 0 E_ALLOCATE"
	bind $win <KeyPress-f> "NSDebugAlloc::Find $oop 0"
	bind $win <KeyPress-g> "NSDebugAlloc::Find $oop 1"

	NSToplevel::NaturalSize $win ""

	SetHook $oop object

	set tree [Info $oop group,tree]
	NSList::Activate $tree "root firstchild"
	focus $tree

	if {[Platform windows]} {
		wm withdraw $win
	}
	wm geometry $win +20+20
	update idletasks
	wm iconify $win

	return
}

# NSDebugAlloc::~NSDebugAlloc --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::~NSDebugAlloc {oop} {

	return
}

# NSDebugAlloc::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::Info {oop info args} {

	global NSDebugAlloc

	# Verify the object
	NSObject::CheckObject NSDebugAlloc $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSDebugAlloc($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSDebugAlloc($oop,$info)
			}
		}
	}
}

# NSDebugAlloc::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::InitWindow {oop} {

	variable Priv

	set win .debugalloc$oop
	toplevel $win
	wm title $win "Alloc - Debug"

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSDebugAlloc::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Tabs
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach hook $Priv(hook) {
		NSTabs::Add $tabsId [mc $hook]
	}
	NSTabs::Info $tabsId invokeCmd "NSDebugAlloc::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Divider
	#

	MakeDivider $win.divider2 x

	#
	# Group List
	#

	set font [Value font,knowledge]
	set lineSpace [font metrics $font -linespace]
	set rowHgt $lineSpace
	if {[icon height] > $rowHgt} {
		set rowHgt [icon height]
	}
	incr rowHgt 8

	set frame $win.frameGroup
	set tree [NSList::New $frame -icon yes]
	$tree configure -width [CalcGroupListWidth $oop] -height [expr {40 * 5}]

	# Do something when a group item is selected
	NSList::OnSelection $tree \
		"NSDebugAlloc::SelectionChanged_Group $oop %T %c %S %D"

	Info $oop group,tree $tree

	#
	# Member List
	#

	set width 300

	set frame $win.frameMember
	set tree [NSList::New $frame -icon yes -columns 2]
	$tree configure -width $width -height [expr {40 * 5}]

	# Do something when a member item is invoked.
	NSList::OnInvoke $tree \
		"NSDebugAlloc::Invoke_Member $oop %T %I %r"

	# Do something when a member item is selected.
	NSList::OnSelection $tree \
		"NSDebugAlloc::SelectionChanged_Member $oop %T %c %S %D"

	Info $oop member,tree $tree

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 16

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
		grid $win.divider2 -in $win \
			-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frameGroup -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $win.frameMember -in $win \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	return
}

# NSDebugAlloc::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::InitMenus {oop} {

	variable Priv

	set win [Info $oop win]

	set mod Ctrl

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSDebugAlloc::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbar $mbar

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSDebugAlloc::MenuInvoke $oop"

	#
	# Debug
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_DEBUG
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_DEBUG -label Debug -underline 0 -identifier M_DEBUG

	set entries {}
	lappend entries [list -type command -label "New Window" \
		-accelerator $mod+N -underline 0 -identifier E_NEW]
	lappend entries [list -type command -label "Reboot NSDebugAlloc" \
		-identifier E_REBOOT]
	lappend entries [list -type separator]
	set i 1
	foreach hook $Priv(hook) {
		lappend entries [list -type radiobutton -label [mc $hook] \
			-variable NSDebugAlloc($oop,radio,hook) -value $hook \
			-command "NSDebugAlloc::SetHook $oop $hook" \
			-accelerator $i -identifier E_HOOK_$hook]
		bind $win <KeyPress-$i> "NSDebugAlloc::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Allocate" \
		-accelerator a -underline 0 -identifier E_ALLOCATE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-accelerator $mod+W -underline 0 -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_DEBUG $entries

	#
	# Hook Menu
	#

	set m [NSMenu::Info $mbar menu].hook
	menu $m -tearoff 0
	Info $oop hookMenu,menu $m
	Info $oop hookMenu,inserted 0

	return
}

# NSDebugAlloc::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::SetupMenus {oop mbarID} {

	variable Priv

	# Debug
	lappend identList E_NEW E_REBOOT E_FIND E_FIND_AGAIN E_CLOSE

	if {[Info $oop member,current] != -1} {
		lappend identList E_ALLOCATE
	}

	foreach hook $Priv(hook) {
		lappend identList E_HOOK_$hook
	}

	if {[Info $oop hookMenu,inserted]} {
		set menu [Info $oop hookMenu,menu]
		set last [$menu index end]
		for {set i 0} {$i <= $last} {incr i} {
			if {[$menu type $i] eq "separator"} continue
			$menu entryconfigure $i -state disabled
		}
		CallHook $oop menu_setup
	}

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSDebugAlloc::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_NEW {NSObject::New NSDebugAlloc}
		E_REBOOT {RebootDebug $oop NSDebugAlloc}
		E_HOOK_* {
			scan $ident "E_HOOK_%s" hoo
			SetHook $oop $hook
		}
		E_ALLOCATE {CallHook $oop allocate}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_CLOSE {Close $oop}
	}

	return
}

# NSDebugAlloc::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::Close {oop} {

	destroy [Info $oop win]
	NSObject::Delete NSDebugAlloc $oop

	return
}

# NSDebugAlloc::SetHook --
#
#	Set the hook. The hook controls what is displayed and what is
#	done when icons/sprites are selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::SetHook {oop hook} {

	variable Priv

	# Remember the hook
	Info $oop hook $hook

	# Clear the hook-specific menu
	set hookMenu [Info $oop hookMenu,menu]
	$hookMenu delete 0 end
	destroy {*}[winfo children $hookMenu]

	# Set the group list
	SetList_Group $oop

	# If the hook-menu is empty, remove it, otherwise insert it
	set mbarId [Info $oop mbar]
	set menu [NSMenu::Info $mbarId menu]
	if {[$hookMenu index end] eq "none"} {
		if {[Info $oop hookMenu,inserted]} {
			$menu delete end
			Info $oop hookMenu,inserted 0
		}
	} else {
		if {![Info $oop hookMenu,inserted]} {
			$menu add cascade -menu $hookMenu
			Info $oop hookMenu,inserted 1
		}
		$menu entryconfigure end -label [mc $hook]
	}

	# Radiobutton menu entries
	Info $oop radio,hook $hook

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $Priv(hook) $hook]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSDebugAlloc::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::CallHook {oop message args} {

	return [uplevel #0 NSDebugAlloc::hook_[Info $oop hook] $oop $message $args]
}

# NSDebugAlloc::SelectionChanged_Group --
#
#	When a "group" list item is selected, display members in the
#	"member" list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::SelectionChanged_Group {oop tree count select deselect} {

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

# NSDebugAlloc::SelectionChanged_Member --
#
#	Do something when a "member" list item is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::SelectionChanged_Member {oop tree count select deselect} {

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

# NSDebugAlloc::Invoke_Member --
#
#	Do something when a "member" list item is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::Invoke_Member {oop tree item row} {

	CallHook $oop allocate

	return
}

# NSDebugAlloc::SetList_Group --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::SetList_Group {oop} {

	set win [Info $oop win]
	set tree [Info $oop group,tree]

	# Feedback
	StatusBar $oop "Displaying..." 0
	update idletasks

	# Clear the list
	NSList::Clear $tree

	# Call hook to set the group list
	CallHook $oop set_list_group

	Info $oop group,current -1

	# Clear the "member" list
# should happen when selection deleted
#	NSList::Clear [Info $oop member,tree]

	# Feedback
	StatusBar $oop "Done." 1

	return
}

# NSDebugAlloc::SetList_Member --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::SetList_Member {oop group} {

	set tree [Info $oop member,tree]

	# Clear the list
	NSList::Clear $tree

	Info $oop member,current -1

	# Call hook to set the member list
	CallHook $oop set_list_member $group

	return
}

# NSDebug::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Priv(hook) $index]

	return
}

# NSDebugAlloc::Find --
#
#	Simple search routine to look for a member by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::Find {oop again} {

	variable Priv

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a name
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) -prompt Name \
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
		if {$groupCurr != -1} {
			set group $groupCurr
		}
		if {$memberCurr != -1} {
			set member [expr {$memberCurr + 1}]
		}
	}

	set max [expr {[[Info $oop group,tree] numitems] - 1}]

	# Compare lowercase
	set string [string tolower $string]

	# Check each group
	for {set i $group} {$i < $max} {incr i} {

		# Get a list of member indexes
		set match [lrange [CallHook $oop member_list $i] $member end]

		# Check each member index
		foreach index $match {

			# Get the member name
			set name2 [CallHook $oop member_name $index]

			# Compare lowercase
			set name2 [string tolower $name2]

			# Found a match
			if {[string first $string $name2] != -1} {

				set tree [Info $oop group,tree]

				# The new group is not displayed
				if {![$tree selection includes "root child $i"]} {

					# Select the new group. As a side effect, the
					# SetList_Member() command is called to display
					# the monsters in the group.
					NSList::Activate $tree "root child $i"
				}

				set tree [Info $oop member,tree]

				# Select the matching member, deselecting all others
				NSList::Activate $tree "root child $member"

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

# NSDebugAlloc::StatusBar --
#
#	Displays text in the status bar, and optionally clears it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

proc NSDebugAlloc::hook_artifact {oop message args} {

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]

			set groupMatch {}
			set groupIndex -1

			foreach {title findSpec} [Global groups,a_info] {

				incr groupIndex

				# Find the last artifact in the group
				set match [angband a_info find -limit 1 \
					-backwards {*}$findSpec]

				# Require some artifacts (must consider random artifacts)
				if {![llength $match]} continue

				# Get the icon
				set icon [angband a_info info [lindex $match 0] icon]

				set item [NSList::NewItem $tree]
				NSList::SetIcon $tree $item $icon
				NSList::SetText $tree $item [mc $title]

				lappend groupMatch $groupIndex
			}

			Info $oop group,match $groupMatch
		}

		set_list_member {

			set row [lindex $args 0]

			set tree [Info $oop member,tree]
			set group [lindex [Info $oop group,match] $row]

			set findSpec [lindex [Global groups,a_info] [expr {$group * 2 + 1}]]

			# Get a list of monsters in the group
			set match [angband a_info find {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the icon and name
				set icon [angband a_info info $index icon]
				set name [angband a_info info $index object_desc]

				set item [NSList::NewItem $tree]
				NSList::SetIcon $tree $item $icon
				NSList::SetText $tree $item $name
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set a_idx [lindex [Info $oop member,match] $row]
			[Info $oop win].statusBar itemconfigure t2 -text #$a_idx
			NSRecall::RecallArtifact $a_idx
		}

		allocate {
			set row [Info $oop member,current]
			set a_idx [lindex [Info $oop member,match] $row]
			StatusBar $oop "Allocate artifact #$a_idx" 1
			debug create_artifact $a_idx
			angband keypress " "
		}

		group_names {
			set result {}
			foreach {title findSpec} [Global groups,a_info] {
				if {[llength [angband a_info find -limit 1 {*}$findSpec]]} {
					lappend result $title
				}
			}
			return $result
		}

		member_name {
			return [angband a_info info [lindex $args 0] object_desc]
		}

		member_list {
			set row [lindex $args 0]
			set group [lindex [Info $oop group,match] $row]
			set findSpec [lindex [Global groups,a_info] [expr {$group * 2 + 1}]]
			return [angband a_info find {*}$findSpec]
		}
	}

	return
}

proc NSDebugAlloc::hook_monster {oop message args} {

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]

			foreach {title findSpec} [Global groups,r_info] {

				if {[variant OANGBANDTK]} {
					if {[string first "-unique yes" $findSpec] != -1} {
						append findSpec " -notflag PLAYER_GHOST"
					}
				}

				# Find the last monster in the group
				set match [angband r_info find -limit 1 \
					-backwards -unique no {*}$findSpec]

				# Get the icon
				set icon [angband r_info info [lindex $match 0] icon]

				set item [NSList::NewItem $tree]
				NSList::SetIcon $tree $item $icon
				NSList::SetText $tree $item [mc $title]
			}

			Info $oop hookMenu,radio,friend 0
			Info $oop hookMenu,radio,pet 0

			set ident {}
			set hookMenu [Info $oop hookMenu,menu]
			$hookMenu add checkbutton -label "Group" \
				-variable NSDebugAlloc($oop,hookMenu,radio,group)
			lappend ident group
			$hookMenu add checkbutton -label "Sleep" \
				-variable NSDebugAlloc($oop,hookMenu,radio,sleep)
			lappend ident sleep
			if {[variant KANGBANDTK]} {
				$hookMenu add separator
				$hookMenu add checkbutton -label "Pet" \
					-variable NSDebugAlloc($oop,hookMenu,radio,pet)
				lappend ident pet
			}
			if {[variant ZANGBANDTK]} {
				$hookMenu add separator
				$hookMenu add checkbutton -label "Friend" \
					-variable NSDebugAlloc($oop,hookMenu,radio,friend)
				lappend ident friend
				$hookMenu add checkbutton -label "Pet" \
					-variable NSDebugAlloc($oop,hookMenu,radio,pet)
				lappend ident pet
			}
			Info $oop hookMenu,ident,hook $ident
		}

		set_list_member {

			set tree [Info $oop member,tree]

			set group [lindex $args 0]
			set findSpec [lindex [Global groups,r_info] [expr {$group * 2 + 1}]]

			if {[variant OANGBANDTK]} {
				if {[string first "-unique yes" $findSpec] != -1} {
					append findSpec " -notflag PLAYER_GHOST"
				}
			}

			# Get a list of monsters in the group
			set match [angband r_info find -unique no {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the icon and name
				set icon [angband r_info info $index icon]
				set name [angband r_info info $index name]

				set value {}

				set flags1 [struct flags monster_race $index flags1]
				if {[lsearch -exact $flags1 ESCORT] != -1} {
					lappend value "E"
				}
				if {[variant OANGBANDTK]} {
					if {[lsearch -exact $flags1 ESCORTS] != -1} {
						lappend value "E+"
					}
					if {[lsearch -exact $flags1 FRIEND] != -1} {
						lappend value "F"
					}
				}
				if {[lsearch -exact $flags1 FRIENDS] != -1} {
					lappend value "F+"
				}

				set cur_num [angband r_info set $index cur_num]
				set max_num [angband r_info set $index max_num]
				if {[angband r_info info $index unique] && ($cur_num >= $max_num)} {
					lappend value "Dead"
				}

				set value [join $value ", "]

				set item [NSList::NewItem $tree]
				NSList::SetIcon $tree $item $icon
				NSList::SetText $tree $item $name
				NSList::SetTextEx $tree $item 1 $value
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set r_idx [lindex [Info $oop member,match] $row]
			[Info $oop win].statusBar itemconfigure t2 -text #$r_idx
			NSRecall::RecallMonster $r_idx
		}

		allocate {
			set row [Info $oop member,current]
			set r_idx [lindex [Info $oop member,match] $row]
			set group [Info $oop hookMenu,radio,group]
			set sleep [Info $oop hookMenu,radio,sleep]
			set friend [Info $oop hookMenu,radio,friend]
			set pet [Info $oop hookMenu,radio,pet]
			StatusBar $oop "Allocate monster #$r_idx" 1
			debug summon_specific $r_idx -group $group -sleep $sleep \
				-friend $friend -pet $pet
			angband keypress " "
		}

		group_names {
			set result {}
			foreach {title findSpec} [Global groups,r_info] {
				lappend result $title
			}
			return $result
		}

		member_name {
			return [angband r_info info [lindex $args 0] name]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex [Global groups,r_info] [expr {$group * 2 + 1}]]
			if {[variant OANGBANDTK]} {
				if {[string first "-unique yes" $findSpec] != -1} {
					append findSpec " -notflag PLAYER_GHOST"
				}
			}
			return [angband r_info find -unique no {*}$findSpec]
		}

		menu_setup {
			set hookMenu [Info $oop hookMenu,menu]
			$hookMenu entryconfigure 0 -state normal
			$hookMenu entryconfigure 1 -state normal
			if {[variant KANGBANDTK]} {
				$hookMenu entryconfigure 3 -state normal
			}
			if {[variant ZANGBANDTK]} {
				$hookMenu entryconfigure 3 -state normal
				$hookMenu entryconfigure 4 -state normal
			}
		}

		menu_cmd {
			switch -- [lindex $args 0] {
			}
		}
	}

	return
}

proc NSDebugAlloc::hook_object {oop message args} {

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]

			foreach {title findSpec} [Global groups,k_info] {

				# Find the last object in the group
				set match [angband k_info find -limit 1 \
					-backwards {*}$findSpec]

				# Get the icon
				set icon [angband k_info info [lindex $match 0] icon]

				set item [NSList::NewItem $tree]
				NSList::SetIcon $tree $item $icon
				NSList::SetText $tree $item [mc $title]
			}
		}

		set_list_member {

			set tree [Info $oop member,tree]

			set group [lindex $args 0]
			set findSpec [lindex [Global groups,k_info] [expr {$group * 2 + 1}]]

			# Get a list of objects in the group
			set match [angband k_info find {*}$findSpec]

			# Collect info for each row
			set itemList {}

			# Add each match to the list
			foreach index $match {

				# Get the icon
				set icon [angband k_info info $index icon]

				# Hack -- object_desc_flavor
				set name [angband k_info info $index object_desc_flavor]

				set item [NSList::NewItem $tree]
				NSList::SetIcon $tree $item $icon
				NSList::SetText $tree $item $name
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		select_member {
			set row [lindex $args 0]
			set k_idx [lindex [Info $oop member,match] $row]
			[Info $oop win].statusBar itemconfigure t2 -text #$k_idx
			NSRecall::RecallObjectKind $k_idx
		}

		allocate {
			set row [Info $oop member,current]
			set k_idx [lindex [Info $oop member,match] $row]
			StatusBar $oop "Allocate object #$k_idx" 1
			debug create_item $k_idx
			angband keypress " "
		}

		group_names {
			set result {}
			foreach {title findSpec} [Global groups,k_info] {
				lappend result $title
			}
			return $result
		}

		member_name {
			return [angband k_info info [lindex $args 0] object_desc]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex [Global groups,k_info] [expr {$group * 2 + 1}]]
			return [angband k_info find {*}$findSpec]
		}
	}

	return
}

proc NSDebugAlloc::hook_XXX {oop message args} {

	switch -- $message {

		set_list_group {
		}

		set_list_member {
		}

		select_member {
		}

		allocate {
		}
	}

	return
}

# NSDebugAlloc::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Assign Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::ValueChanged_font_knowledge {oop} {

	# Set the width of the group list
	set tree [Info $oop group,tree]
	$tree configure -width [CalcGroupListWidth $oop]

	return
}

# NSDebugAlloc::CalcGroupListWidth --
#
#	Returns the desired width of the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::CalcGroupListWidth {oop} {

	variable Priv

	# Get the desired font
	set font [Value font,knowledge]

	# Minimum desired width of the group list
	set maxWidth 100

	# Check each hook
	foreach hook $Priv(hook) {

		# Check each name
		foreach name [hook_$hook $oop group_names] {

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

# NSDebugAlloc::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::IconCfg {oop} {

	set currentG [Info $oop group,current]
	set currentM [Info $oop member,current]
	SetList_Group $oop
	if {$currentG != -1} {
		NSList::Activate [Info $oop group,tree] "root child $currentG"
		if {$currentM != -1} {
			NSList::Activate [Info $oop member,tree] "root child $currentM"
		}
	}

	return
}

proc RebootDebug {oop module} {

	catch {
		${module}::Close $oop
	}
	NSModule::RebootModule $module

	return
}
