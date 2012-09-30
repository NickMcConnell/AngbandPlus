# File: knowledge.tcl

# Purpose: the Knowledge Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSKnowledge {

	variable MenuString
	variable Priv

# namespace eval NSKnowledge
}

# NSKnowledge::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::InitModule {} {

	variable Priv

	set Priv(hook) {}

	# Create the Knowledge Window
	NSObject::New NSKnowledge

	return
}

# NSKnowledge::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::CloseModule {} {

	catch {
		destroy [Window knowledge]
	}

	return
}

# NSKnowledge::NSKnowledge --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::NSKnowledge {oop} {

	variable Priv

	InitWindow $oop

	set win [Info $oop win]
	
	Info $oop group,current -1
	Info $oop member,current -1

	# Info about selected group & member for each hook
	foreach {hook label} $Priv(hook) {
		Info $oop group,$hook 0
		Info $oop member,$hook -1
	}

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	NSWindowManager::RegisterWindow knowledge $win \
		"GetDefaultGeometry $win reqwidth main" "" \
		"NSKnowledge::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSKnowledge $oop $win

	#
	# Global list of application windows
	#

	Global knowledge,oop $oop
	Window knowledge $win

	return
}

# NSKnowledge::~NSKnowledge --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::~NSKnowledge {oop} {

	NSValueManager::RemoveClient listBG [Info $oop listBG,clientId]
	NSValueManager::RemoveClient font,knowledge [Info $oop font,clientId]

	return
}

# NSKnowledge::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::Info {oop info args} {

	global NSKnowledge

	# Verify the object
	NSObject::CheckObject NSKnowledge $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSKnowledge($oop,$info) [lindex $args 0]
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
				return $NSKnowledge($oop,$info)
			}
		}
	}

	return
}

# NSKnowledge::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::InitWindow {oop} {

	variable Priv

	set win .knowledge$oop
	toplevel $win
	wm title $win "Knowledge"

	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSKnowledge::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

	Info $oop win $win

	#
	# Menus
	#

	InitMenus $oop

	#
	# Divider
	#

	MakeDivider $win.divider2 x

	#
	# Tabs!
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach {hook label} $Priv(hook) {
		NSTabs::Add $tabsId $label
	}
	NSTabs::Info $tabsId invokeCmd "NSKnowledge::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Group List
	#

	set cw [font measure [Value font,knowledge] "W"]
#	set width [expr {$cw * 26}]
	set width [CalcGroupListWidth $oop]
	set iconSize [expr {[icon size] + 8}]

	frame $win.frameGroup \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frameGroup $iconSize $width 300 \
		"NSKnowledge::NewItemCmd $oop" "NSKnowledge::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frameGroup.scroll set"
	scrollbar $win.frameGroup.scroll \
		-command "$canvas yview" -orient vert

	# Do something when a group is selected
	NSCanvist::Info $canvistId selectionCmd \
		"NSKnowledge::SelectionChanged_Group $oop"

	# Update ourself when the listBG value changes
	Info $oop listBG,clientId \
		[NSValueManager::AddClient listBG \
		"NSKnowledge::ValueChanged_listBG $oop"]

	# Update ourself when the font,knowledge value changes
	Info $oop font,clientId \
		[NSValueManager::AddClient font,knowledge \
		"NSKnowledge::ValueChanged_font_knowledge $oop"]

	pack $win.frameGroup.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both -anchor nw

	Info $oop group,canvistId $canvistId

	#
	# Member List
	#

#	set width [expr {$cw * 50}]
	set width 350
	
	frame $win.frame \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frame $iconSize $width 300 \
		"NSKnowledge::NewItemCmd $oop" "NSKnowledge::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frame.scroll set"
	scrollbar $win.frame.scroll \
		-command "$canvas yview" -orient vert

	# Do something when a member is selected
	NSCanvist::Info $canvistId selectionCmd \
		"NSKnowledge::SelectionChanged_Member $oop"

	# Do something when a selected member is clicked.
	NSCanvist::Info $canvistId clickCmd \
		"NSKnowledge::Click_Member $oop"

	bind $canvas <Configure> "
		NSKnowledge::Configure $oop $canvas
	"

	Info $oop member,canvistId $canvistId

	pack $win.frame.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both -anchor nw

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
		grid $win.divider2 \
			-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frameGroup \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $win.frame \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress-Escape> "NSKnowledge::Close $oop"
	bind $win <Control-KeyPress-w> "NSKnowledge::Close $oop"

	bind $win <KeyPress-f> "NSKnowledge::Find $oop 0"
	bind $win <KeyPress-g> "NSKnowledge::Find $oop 1"

	#
	# Synch the scrollbars when window is shown.
	# Hmmm. This doesn't work if you bind to $win.frame.scroll...
	#

	NSUtils::SynchScrollBar [Info $oop groupCanvas] $win.frameGroup.scroll
	NSUtils::SynchScrollBar [Info $oop memberCanvas] $win.frame.scroll

	return
}

# NSKnowledge::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::InitMenus {oop} {

#	global NSKnowledge
#	global NSMenu
	variable MenuString
	variable Priv

	set win [Info $oop win]

	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSKnowledge::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSKnowledge::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSKnowledge::MenuInvoke $oop"

	#
	# Knowledge Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_KNOWLEDGE
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_KNOWLEDGE -label "Knowledge" -underline 0 \
		-identifier M_KNOWLEDGE

	set entries {}
	set i 1
	foreach {hook label} $Priv(hook) {
		lappend entries [list -type radiobutton -label $label \
			-variable NSKnowledge($oop,radio,hook) -value $hook \
			-accelerator $i -identifier E_HOOK_$i]
		bind $win <KeyPress-$i> "NSKnowledge::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Find..." \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label "Find Again" \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Set Font" \
		-underline 0 -identifier E_FONT]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_KNOWLEDGE $entries

	#
	# Hook Menu
	#

	set m [NSMenu::Info $mbar menu].hook
	menu $m -tearoff 0
	Info $oop hookMenu,menu $m
	Info $oop hookMenu,inserted 0

	set MenuString(M_KNOWLEDGE) \
		"Contains commands for displaying and searching groups."
	set MenuString(E_FIND) \
		"Searches for a member by name."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSKnowledge::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::SetupMenus {oop mbarID} {

	variable Priv

	set i 0
	foreach {hook label} $Priv(hook) {
		lappend identList E_HOOK_[incr i]
	}

	lappend identList E_FIND E_FIND_AGAIN E_FONT E_CLOSE

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

	[Info $oop win].statusBar cover show

	return
}

# NSKnowledge::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::MenuSelect {oop menuId index ident} {

	variable MenuString
	variable Priv

	switch -glob -- $ident {
		{} {
			set desc {}
		}
		E_HOOK_* {
			set desc "Displays this group."
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

	[Info $oop win].statusBar cover set $desc
	if {![string length $desc]} {
		if {$menuId == [Info $oop mbarId]} {
			[Info $oop win].statusBar cover hide
		}
	}

	return
}

# NSKnowledge::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_HOOK_* {
			scan $ident "E_HOOK_%d" hookNum
			SetHook $oop [lindex $Priv(hook) [expr {($hookNum - 1) * 2}]]
		}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_FONT {
			NSModule::LoadIfNeeded NSFont
			NSWindowManager::Display font knowledge
		}
		E_CLOSE {Close $oop}
	}

	return
}

# NSKnowledge::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::DisplayCmd {oop message first args} {

	variable Priv
	
	switch -- $message {
		preDisplay {

			# Assume angband.exe called us. See Close()
			Info $oop hideOnEscape 0

			# Display a specific group
			if {[llength $args]} {
				set hook [lindex $args 0]
				Info $oop hook hook_$hook
				if {[llength $args] > 1} {
					set index [lindex $args 1]
					DisplayMember $oop hook_$hook $index

					# Hack -- Escape hides window
					Info $oop hideOnEscape 1
				}
			}

			SetHook $oop [Info $oop hook]
		}
		postDisplay {
		}
		postWithdraw {
			StateRemember $oop
			NSCanvist::DeleteAll [Info $oop group,canvistId]
			NSCanvist::DeleteAll [Info $oop member,canvistId]
		}
	}

	return
}

# NSKnowledge::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::Close {oop} {

	if {[Info $oop hideOnEscape]} {
		NSWindowManager::Undisplay knowledge
	}

	angband keypress \033

	return
}

# NSKnowledge::StateRemember --
#
#	Remember the selected group and member, then clear the lists.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::StateRemember {oop} {

	global Angband

	### Mega-Hack -- Don't do this during geometry foolishness
#	if !$NSWindowManager::Priv(knowledge,setup) return

	set hook [Info $oop hook]

	# Because we are clearing the lists here, and don't want to
	# upset the user, I save the selected group/member so it can
	# be restored in StateRestore() below.

	# Because the contents of a group change as the character gains
	# knowledge, I save the proper index, not the row, of the
	# selected group/member.

	set row [Info $oop group,current]
	if {$row != -1} {
		set index [lindex [Info $oop group,match] $row]
	} else {
		set index -1
	}
	Info $oop group,$hook $index

	set row [Info $oop member,current]
	if {$row != -1} {
		set index [lindex [Info $oop member,match] $row]
	} else {
		set index -1
	}
	Info $oop member,$hook $index

	return
}

# NSKnowledge::StateRestore --
#
#	Restore the display.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::StateRestore {oop} {

	global Angband

	### Mega-Hack -- Don't do this during geometry foolishness
#	if !$NSWindowManager::Priv(knowledge,setup) return

	set hook [Info $oop hook]
	
	# Restore the selected group. It might not be there if the
	# "cheat_know" option was turned off.
	set current [Info $oop group,$hook]
	if {$current != -1} {
		set row [lsearch -exact [Info $oop group,match] $current]
		if {$row != -1} {
			set canvistId [Info $oop group,canvistId]
			NSCanvist::UpdateSelection $canvistId $row {}
			NSCanvist::See $canvistId $row
		} else {
			Info $oop member,$hook -1
		}
	}

	# Restore the selected member. It might not be there if the
	# "cheat_know" option was turned off.
	set current [Info $oop member,$hook]
	if {$current != -1} {
		set row [lsearch -exact [Info $oop member,match] $current]
		if {$row != -1} {
			set canvistId [Info $oop member,canvistId]
			NSCanvist::UpdateSelection $canvistId $row {}
			NSCanvist::See $canvistId $row
		}
	}

	return
}

# NSKnowledge::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::CallHook {oop message args} {

	return [uplevel #0 NSKnowledge::[Info $oop hook] $oop $message $args]
}

# NSKnowledge::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::SetHook {oop hook} {

	global NSMenu
	variable Priv

	if {$hook != [Info $oop hook]} {
		set again 0
	} else {
		set again 1
	}

	if {!$again} {
		StateRemember $oop
	}

	# Remember the hook
	Info $oop hook $hook

	# Clear the hook-specific menu
	set hookMenu [Info $oop hookMenu,menu]
	$hookMenu delete 0 end
	eval destroy [winfo children $hookMenu]

	# Show icons by default
	ConfigureList_Member $oop 1

	# Calculate the row height of the group list
	set rowHgt [font metrics [Value font,knowledge] -linespace]
	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}
	incr rowHgt 8
	set canvistId [Info $oop group,canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	# Set the group list
	SetList_Group $oop

	# If the hook-menu is empty, remove it, otherwise insert it
	set mbarId [Info $oop mbarId]
	if {[string equal [$hookMenu index end] none]} {
		if {[Info $oop hookMenu,inserted]} {
			$NSMenu($mbarId,menu) delete 2
			Info $oop hookMenu,inserted 0
		}
	} else {
		set index [lsearch -exact $Priv(hook) $hook]
		if {![Info $oop hookMenu,inserted]} {
			$NSMenu($mbarId,menu) add cascade -menu $hookMenu
			Info $oop hookMenu,inserted 1
		}
		$NSMenu($mbarId,menu) entryconfigure end \
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

	[Info $oop win].statusBar itemconfigure t2 -text ""

	StateRestore $oop

	return
}

# NSKnowledge::Click_Member --
#
#	Do something when a selected member is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::Click_Member {oop canvistId row} {

	CallHook $oop click_member $row
	return
}

# NSKnowledge::SelectionChanged_Group --
#
#	When a "group" list item is selected, display artifacts/monsters
#	in the "member" list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::SelectionChanged_Group {oop canvistId select deselect} {

	# If nothing was selected, clear the member list
	if {![llength $select]} {
		set canvistId [Info $oop member,canvistId]
		NSCanvist::DeleteAll $canvistId
		Info $oop group,current -1
		StatusBar $oop "" 0
		[Info $oop win].statusBar itemconfigure t2 -text ""
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
	Info $oop group,current $row

	# Add matching artifacts/monsters to the list
	SetList_Member $oop $row

	return
}

# NSKnowledge::SelectionChanged_Member --
#
#	When a "member" list item is selected, display memory for the
#	artifact/monster in the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::SelectionChanged_Member {oop canvistId select deselect} {

	# Do nothing if no new row was selected
	if {![llength $select]} {
		Info $oop member,current -1
		StatusBar $oop "" 0
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
	Info $oop member,current $row

	# Call the hook to do other stuff (recall, etc)
	CallHook $oop select_member $row

	return
}

# NSKnowledge::SetList_Group --
#
#	Set the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::SetList_Group {oop} {

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

# NSKnowledge::SetList_Member --
#
#	Set the member list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::SetList_Member {oop group} {

	set canvistId [Info $oop member,canvistId]

	# Clear the list
	NSCanvist::DeleteAll $canvistId

	Info $oop member,current -1

	# Call hook to set the member list
	CallHook $oop set_list_member $group

	return
}

# NSKnowledge::ConfigureList_Member --
#
#	Set the row height of the member list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::ConfigureList_Member {oop show_icons} {

	set canvistId [Info $oop member,canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	set rowHgt [font metrics [Value font,knowledge] -linespace]

	if {$show_icons} {
		if {[icon size] > $rowHgt} {
			set rowHgt [icon size]
		}
	}

	incr rowHgt 8
	
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	return
}

# NSKnowledge::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSKnowledge::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::NewItemCmd {oop canvistId y args} {

	variable Priv

	array set data [list -text "" -assign "" -color White -extra ""]
	array set data $args

	set c [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Value font,knowledge]

	set fh [font metrics $font -linespace]
	set diff [expr {int([expr {($lineHeight - $fh) / 2}])}]

	# Get the width of the canvas
	set canvasWidth [winfo width $c]

if 0 {

	# Image
	if {[string length $data(-assign)]} {
		set iw [icon size]
		set ih [icon size]
		set wid [expr {[icon size] + 8}]
		set xdiff [expr {int([expr {($wid - $iw) / 2}])}]
		set ydiff [expr {int([expr {($lineHeight - $ih) / 2}])}]
		lappend itemIdList [$c create widget $xdiff [expr {$y + $ydiff}] \
			-assign $data(-assign)]
	} else {
		set wid 3
	}
}

	# Text
	lappend itemIdList [$c create text [expr {$wid + 1}] [expr {$y + $diff}] \
		-text $data(-text) -anchor nw -font $font -fill $data(-color) \
		-tags text]

	# Selection rectangle around everything
	lappend itemIdList [$c create rectangle 2 [expr {$y + 2}] \
		[expr {([winfo width $c] - 1) - 2}] [expr {$y + $lineHeight - 2}] \
		-fill {} -outline {} -tags selrect -width 2.0]

	if {[string length $data(-extra)]} {
		lappend itemIdList [$c create text [expr {($canvasWidth - 1) - 4}] \
			[expr {$y + $diff}] -text $data(-extra) -anchor ne -font $font \
			-fill White -tags extra]
	}

	return $itemIdList
}

# NSKnowledge::HighlightItemCmd --
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

proc NSKnowledge::HighlightItemCmd {oop canvistId state args} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set itemIdList $args

	set idRect [FindItemByTag $canvas $itemIdList selrect]

	if {[NSUtils::HasFocus $canvas]} {
		set fill [Value listHilite]
	} else {
		set fill [Value listInactive]
	}

	if {$state} {
		$canvas itemconfigure $idRect -outline $fill

	} else {
		$canvas itemconfigure $idRect -fill {} -outline {}
	}

	return
}

# NSKnowledge::Configure --
#
#	Called as a <Configure> event script. Resizes the selection rectangles
#	so they fit properly.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::Configure {oop canvas} {

	# Get the width of the canvas
	set canvasWidth [winfo width $canvas]

	foreach itemId [$canvas find withtag selrect] {
		set coords [$canvas coords $itemId]
		set right [expr {($canvasWidth - 1) - 2}]
		set coords [lreplace $coords 2 2 $right]
		eval $canvas coords $itemId $coords
	}

	foreach itemId [$canvas find withtag extra] {
		set coords [$canvas coords $itemId]
		set right [expr {($canvasWidth - 1) - 4}]
		set coords [lreplace $coords 0 0 $right]
		eval $canvas coords $itemId $coords
	}

	# Set the scrollregion to prevent horizontal scrolling
	scan [$canvas cget -scrollregion] "%s %s %s %s" x1 y1 x2 y2
	$canvas configure -scrollregion "$x1 $y1 $canvasWidth $y2"

	return
}

# NSKnowledge::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::InvokeTab {oop tabsId tabId} {

	variable Priv
	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Priv(hook) [expr {$index * 2}]]

	return
}

# NSKnowledge::DisplayMember --
#
#	Find the group the given member is in, and display it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::DisplayMember {oop hook member} {

	foreach group [CallHook $oop group_list] {
		set memberList [CallHook $oop member_list $group]
		set index [lsearch -exact $memberList $member]
		if {$index != -1} {
			# Note: real index, not list row
			Info $oop group,$hook $group
			Info $oop member,$hook $member
			break
		}
	}

	return
}

# NSKnowledge::Find --
#
#	Simple search routine to look for a member by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::Find {oop again} {

	variable Priv

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a name
		set string [NSUtils::StringBox -title "Find" \
			-initial $Priv(find,string) -prompt "find-prompt" \
			-buttons [list "Find" "Cancel"] -parent [Info $oop win]]
		if {![string length $string]} return

		# Clean up after the dialog, give message
		StatusBar $oop "Searching..." 1
		update

		# Reset search parameters
		set Priv(find,string) $string
	}

	# Default to searching from the beginning
	set groupRow 0
	set memberRow 0

	# Search in selected group, if any
	if {!$Priv(find,fromStart)} {	
		set groupCurr [Info $oop group,current]
		set memberCurr [Info $oop member,current]
		if {$groupCurr != -1} {
			set groupRow $groupCurr
		}
		if {$memberCurr != -1} {
			set memberRow [expr {$memberCurr + 1}]
		}
	}

	set max [NSCanvist::Info [Info $oop group,canvistId] count]
	
	# Compare lowercase
	set string [string tolower $string]

	set groupList [CallHook $oop group_list]

	for {set i $groupRow} {$i < $max} {incr i} {

		set group [lindex $groupList $i]
		set memberList [lrange [CallHook $oop member_list $group] $memberRow end]

		foreach index $memberList {
	
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

				# The new group is already selected
				} else {
				}
	
				set canvistId [Info $oop member,canvistId]
	
				# Select the matching member, deselecting all others
				NSCanvist::UpdateSelection $canvistId $memberRow all
				NSCanvist::See $canvistId $memberRow
	
				# Don't search from start next time
				set Priv(find,fromStart) 0

				# Clear "Searching..." message
				StatusBar $oop "" 0

				# Done
				return
			}
			incr memberRow
		}
		set memberRow 0
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



# NSKnowledge::CalcGroupListWidth --
#
#	Returns the desired width of the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::CalcGroupListWidth {oop} {

	variable Priv

	# Get the desired font
	set font [Value font,knowledge]

	set maxWidth 100
	
	foreach {hook label} $Priv(hook) {
		foreach name [$hook $oop group_names] {
			set width [font measure $font $name]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	return [expr {[icon size] + 8 + $maxWidth + 20 + 4}]
}

# NSKnowledge::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Knowledge Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::ValueChanged_font_knowledge {oop} {

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
	set oldWidth [winfo width $canvas]
	set newWidth [CalcGroupListWidth $oop]
	$canvas configure -width $newWidth

	# Set the scrollregion to prevent horizontal scrolling
	scan [$canvas cget -scrollregion] "%s %s %s %s" x1 y1 x2 y2
	$canvas configure -scrollregion "$x1 $y1 $newWidth $y2"

	# Hack -- Resize the toplevel so the member list is not resized
	set diff [expr {$newWidth - $oldWidth}]
	if {$diff} {
		set win [Info $oop win]
		set newWidth [expr {[winfo width $win] + $diff}]
		NSToplevel::SetTotalWidth $win $newWidth
	}

	if {[winfo ismapped [Info $oop win]]} {
		[Info $oop groupCanvas] itemconfigure text -font $font
		[Info $oop memberCanvas] itemconfigure text -font $font
	}

	return
}

# NSKnowledge::ValueChanged_listBG --
#
#	Called when the listBG value changes.
#	Updates the Knowledge Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKnowledge::ValueChanged_listBG {oop} {

	set color [Value listBG]
	
	[Info $oop groupCanvas] configure -background $color
	[Info $oop memberCanvas] configure -background $color

	return
}

