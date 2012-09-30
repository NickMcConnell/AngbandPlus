# File: debug-alloc.tcl

# Purpose: the Alloc Window for debugging

#
# Copyright (c) 1997-2001 Tim Baker
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

	# Update ourself when the listBG value changes
	Info $oop listBG,clientId \
		[NSValueManager::AddClient listBG \
		"NSDebugAlloc::ValueChanged_listBG $oop"]

	# Update ourself when the font,knowledge value changes
	Info $oop font,clientId \
		[NSValueManager::AddClient font,knowledge \
		"NSDebugAlloc::ValueChanged_font_knowledge $oop"]

	Info $oop group,current -1
	Info $oop member,current -1

	bind $win <KeyPress-a> "NSDebugAlloc::MenuInvoke $oop 0 E_ALLOCATE"
	bind $win <KeyPress-Return> "NSDebugAlloc::MenuInvoke $oop 0 E_ALLOCATE"
	bind $win <KeyPress-f> "NSDebugAlloc::Find $oop 0"
	bind $win <KeyPress-g> "NSDebugAlloc::Find $oop 1"

	NSToplevel::NaturalSize $win ""

	SetHook $oop alloc_hook_object
	set canvistId [Info $oop group,canvistId]
	NSCanvist::UpdateSelection $canvistId 0 ""

	set canvas [Info $oop groupCanvas]
	focus $canvas

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

	NSValueManager::RemoveClient listBG [Info $oop listBG,clientId]
	NSValueManager::RemoveClient font,knowledge [Info $oop font,clientId]

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
			groupCanvas {
				set canvistId [Info $oop group,canvistId]
				return [NSCanvist::Info $canvistId canvas]
			}
			memberCanvas {
				set canvistId [Info $oop member,canvistId]
				return [NSCanvist::Info $canvistId canvas]
			}
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

	# Tabs!
	set tabsId [NSObject::New NSTabs $win]
	foreach {hook label} $Priv(hook) {
		NSTabs::Add $tabsId $label
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
	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}
	incr rowHgt 8
	set width [CalcGroupListWidth $oop]

	frame $win.frameGroup \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frameGroup $rowHgt $width 100 \
		"NSDebugAlloc::NewItemCmd $oop" "NSDebugAlloc::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -height [expr {40 * 5}]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frameGroup.scroll set"
	scrollbar $win.frameGroup.scroll \
		-command "$canvas yview" -orient vert

#	bind $win.frameGroup.scroll <Map> "eval %W set \[$canvas yview]"
	NSUtils::SynchScrollBar $canvas $win.frameGroup.scroll

	# Do something when a group item is selected
	NSCanvist::Info $canvistId selectionCmd \
		"NSDebugAlloc::SelectionChanged_Group $oop"

	pack $win.frameGroup.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both -anchor nw

	Info $oop group,canvistId $canvistId

	#
	# Member List
	#

	set width 300

	frame $win.frameMember \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frameMember $rowHgt $width 100 \
		"NSDebugAlloc::NewItemCmd $oop" "NSDebugAlloc::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -height [expr {40 * 5}]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frameMember.scroll set"
	scrollbar $win.frameMember.scroll \
		-command "$canvas yview" -orient vert

#	bind $win.frameMember.scroll <Map> "eval %W set \[$canvas yview]"
	NSUtils::SynchScrollBar $canvas $win.frameMember.scroll

	# Do something when a member item is invoked.
	NSCanvist::Info $canvistId invokeCmd \
		"NSDebugAlloc::Invoke_Member $oop"

	# Do something when a member item is selected.
	NSCanvist::Info $canvistId selectionCmd \
		"NSDebugAlloc::SelectionChanged_Member $oop"

	bind $canvas <Configure> \
		"NSDebugAlloc::Configure $oop $canvas"

	Info $oop member,canvistId $canvistId

	pack $win.frameMember.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both -anchor nw

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
	foreach {hook label} $Priv(hook) {
		lappend entries [list -type radiobutton -label "$label" \
			-variable NSDebugAlloc($oop,radio,hook) -value $hook \
			-command "NSDebugAlloc::SetHook $oop $hook" \
			-accelerator $i -identifier E_HOOK_$i]
		bind $win <KeyPress-$i> "NSDebugAlloc::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Allocate" \
		-accelerator a -underline 0 -identifier E_ALLOCATE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Find..." \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label "Find Again" \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" \
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
	
	set i 0
	foreach {hook label} $Priv(hook) {
		lappend identList E_HOOK_[incr i]
	}

	if {[Info $oop hookMenu,inserted]} {
		set menu [Info $oop hookMenu,menu]
		set last [$menu index end]
		for {set i 0} {$i <= $last} {incr i} {
			if {[string equal [$menu type $i] separator]} continue
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
			scan $ident "E_HOOK_%d" hookNum
			SetHook $oop [lindex $Priv(hook) [expr {($hookNum - 1) * 2}]]
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
	eval destroy [winfo children $hookMenu]

	# Set the group list
	SetList_Group $oop

	# If the hook-menu is empty, remove it, otherwise insert it
	set mbarId [Info $oop mbar]
	set menu [NSMenu::Info $mbarId menu]
	if {[string equal [$hookMenu index end] none]} {
		if {[Info $oop hookMenu,inserted]} {
			$menu delete end
			Info $oop hookMenu,inserted 0
		}
	} else {
		set index [lsearch -exact $Priv(hook) $hook]
		if {![Info $oop hookMenu,inserted]} {
			$menu add cascade -menu $hookMenu
			Info $oop hookMenu,inserted 1
		}
		$menu entryconfigure end \
			-label [lindex $Priv(hook) [incr index]]
	}

	# Radiobutton menu entries
	Info $oop radio,hook $hook

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [expr {[lsearch -exact $Priv(hook) $hook] / 2}]]
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

	return [uplevel #0 NSDebugAlloc::[Info $oop hook] $oop $message $args]
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

proc NSDebugAlloc::SelectionChanged_Group {oop canvistId select deselect} {

	# If nothing was selected, clear the member list
	if {![llength $select]} {
		set canvistId [Info $oop member,canvistId]
		NSCanvist::DeleteAll $canvistId
		Info $oop group,current -1
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
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

proc NSDebugAlloc::SelectionChanged_Member {oop canvistId select deselect} {

	# Do nothing if no new row was selected
	if {![llength $select]} {
		Info $oop member,current -1
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
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

proc NSDebugAlloc::Invoke_Member {oop canvistId x y} {

	set row [NSCanvist::PointToRow $canvistId $x $y]
	if {$row == -1} return

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
	set canvistId [Info $oop group,canvistId]

	# Feedback
	StatusBar $oop "Displaying..." 0
	update idletasks

	# Clear the list
	NSCanvist::DeleteAll $canvistId

	# Call hook to set the group list
	CallHook $oop set_list_group

	Info $oop group,current -1

	# Hack -- Clear the "member" list
	NSCanvist::DeleteAll [Info $oop member,canvistId]

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

	set canvistId [Info $oop member,canvistId]

	# Clear the list
	NSCanvist::DeleteAll $canvistId

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
	SetHook $oop [lindex $Priv(hook) [expr {$index * 2}]]

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
		set string [NSUtils::StringBox -title "Find" \
			-initial $Priv(find,string) -prompt Name \
			-buttons [list "Find" "Cancel"] -parent [Info $oop win]]

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

	set max [NSCanvist::Info [Info $oop group,canvistId] count]

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
	
				set canvistId [Info $oop group,canvistId]
	
				# The new group is not displayed
				if {![NSCanvist::IsRowSelected $canvistId $i]} {

					# Clear the current selection
					NSCanvist::RemoveSelection $canvistId
					
					# Select the new group. As a side effect, the
					# SetList_Member() command is called to display
					# the monsters in the group.
					NSCanvist::UpdateSelection $canvistId $i {}
					NSCanvist::See $canvistId $i
				}
	
				set canvistId [Info $oop member,canvistId]
	
				# Select the matching member, deselecting all others
				NSCanvist::UpdateSelection $canvistId $member all
				NSCanvist::See $canvistId $member
	
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

# NSDebugAlloc::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::NewItemCmd {oop canvistId y icon text {value ""}} {

	set c [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Value font,knowledge]

	set fh [font metrics $font -linespace]
	set diff [expr {int([expr {($lineHeight - $fh) / 2}])}]

	# Get the width of the canvas
	set canvasWidth [winfo width $c]

	# Selection rectangle around everything
	lappend itemIdList [$c create rectangle 2 [expr {$y + 2}] \
		[expr {([winfo width $c] - 1) - 2}] [expr {$y + $lineHeight - 2}] \
		-fill {} -outline {} -tags selrect -width 2.0]

	# Image
	set iw [icon size]
	set ih [icon size]
	set wid [expr {[icon size] + 8}]
	set xdiff [expr {int([expr {($wid - $iw) / 2}])}]
	set ydiff [expr {int([expr {($lineHeight - $ih) / 2}])}]
	lappend itemIdList [$c create widget $xdiff [expr {$y + $ydiff}] \
		-assign $icon -tags enabled]

	# Text
	set fill White
	lappend itemIdList [$c create text [expr {$wid + 1}] [expr {$y + $diff}] \
		-text $text -anchor nw -font $font -fill $fill -tags {text enabled}]

	#
	# Value
	#

	if {[string length $value]} {
		lappend itemIdList [$c create text [expr {($canvasWidth - 1) - 4}] \
			[expr {$y + $diff}] -text $value -anchor ne -font $font \
			-fill White -tags value]
	}

	return $itemIdList
}

# NSDebugAlloc::HighlightItemCmd --
#
#	Called by NSCanvist::Select() to highlight a row.
#
# Arguments:
#	oop					OOP ID. See above.
#	canvistId					OOP ID of NSCanvist object.
#	state					1 or 0 highlight state.
#	args					List of canvas item ids
#
# Results:
#	What happened.

proc NSDebugAlloc::HighlightItemCmd {oop canvistId state args} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set itemIdList $args

	set idRect [lindex $itemIdList 0]

	if {[NSUtils::HasFocus $canvas]} {
		set fill [Value listHilite]
	} else {
		set fill [Value listInactive]
	}

	if {$state} {
		$canvas itemconfigure $idRect -outline $fill

	} else {
		$canvas itemconfigure $idRect -outline {}
	}

	return
}

# NSDebugAlloc::Configure --
#
#	Called as a <Configure> event script. Resizes the selection rectangles
#	so they fit properly.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::Configure {oop canvas} {

	# Get the width of the canvas
	set canvasWidth [winfo width $canvas]

	foreach itemId [$canvas find withtag selrect] {
		set coords [$canvas coords $itemId]
		set right [expr {($canvasWidth - 1) - 2}]
		set coords [lreplace $coords 2 2 $right]
		eval $canvas coords $itemId $coords
	}

	foreach itemId [$canvas find withtag value] {
		set coords [$canvas coords $itemId]
		set right [expr {($canvasWidth - 1) - 4}]
		set coords [lreplace $coords 0 0 $right]
		eval $canvas coords $itemId $coords
	}

	return
}


proc NSDebugAlloc::alloc_hook_XXX {oop message args} {

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

	# Get the desired font
	set font [Value font,knowledge]

	# Calculate row height
	set rowHgt [font metrics $font -linespace]
	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}
	incr rowHgt 8

	# Set row height of group list
	set canvistId [Info $oop group,canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	# Set the width of the group list
	$canvas configure -width [CalcGroupListWidth $oop]

	# Set row height of member list
	set canvistId [Info $oop member,canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	return
}

# NSDebugAlloc::ValueChanged_listBG --
#
#	Called when the listBG value changes.
#	Updates the Assign Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebugAlloc::ValueChanged_listBG {oop} {

	set color [Value listBG]
	
	[Info $oop groupCanvas] configure -background $color
	[Info $oop memberCanvas] configure -background $color

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
	foreach {hook title} $Priv(hook) {

		# Check each name
		foreach name [$hook $oop group_names] {

			# Calculate the width in pixels
			set width [font measure $font $name]
			
			# Remember the maximum width
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	return [expr {[icon size] + 8 + $maxWidth + 20 + 4}]
}

proc RebootDebug {oop module} {

	catch {
		${module}::Close $oop
	}
	NSModule::RebootModule $module

	return
}
