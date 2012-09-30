# File: debug.tcl

# Purpose: the Debug Window for debugging

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSDebug {

	variable Priv
	set Priv(font) [Global font,fixed,normal]
	
	variable auto_lite 0
	variable free_moves 0
	variable see_monsters 0

# namespace eval NSDebug
}

# NSDebug::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::InitModule {} {

	variable Priv

	# Create the Debug Window
	NSObject::New NSDebug

	return
}

# NSDebug::NSDebug --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::NSDebug {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSToplevel::NaturalSize $win ""

	Info $oop display,current -1
	Info $oop detail,current -1

	if {[Platform windows]} {
		wm withdraw $win
	}
	wm geometry $win +20+20
	update idletasks
	wm iconify $win

	return
}

# NSDebug::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::Info {oop info args} {

	global NSDebug

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSDebug($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSDebug($oop,$info)
			}
		}
	}

	return
}

# NSDebug::InitWindow --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::InitWindow {oop} {

	global NSDebug
	variable Priv

	set win .debug$oop
	toplevel $win
	wm title $win Debug

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSDebug::Close $oop"

	Info $oop win $win

	InitMenus $oop

	# Divider
	MakeDivider $win.divider x

	# Tabs!
	set tabsId [NSObject::New NSTabs $win]
	foreach {hook label} $Priv(hook) {
		NSTabs::Add $tabsId $label
	}
	NSTabs::Info $tabsId invokeCmd "NSDebug::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	# List of monsters, objects, etc
	set frame [frame $win.frameList -relief sunken -borderwidth 1]
	set canvistId [NSObject::New NSTexist $frame $Priv(font) 60 10]
	set canvas [NSTexist::Info $canvistId text]
	$canvas configure -background [Value listBG] \
		-yscrollcommand "$frame.yscroll set" \
		-exportselection no
	scrollbar $frame.yscroll \
		-orient vertical -command "$canvas yview"

	# Selection command
	NSTexist::Info $canvistId selectionCmd \
		"NSDebug::SelectionChanged_Display $oop"

	Info $oop display,canvistId $canvistId

	# This call updates the list background color whenever the
	# global list background color changes
	Info $oop display,clientId \
		[NSValueManager::AddClient listBG "ListBackgroundChanged $canvas"]

	pack $frame.yscroll -side right -fill y
	pack $canvas -side left -expand yes -fill both -anchor nw

	# Type list
	set frame [frame $win.frameList2 -relief sunken -borderwidth 1]
	set canvistId [NSObject::New NSTexist $frame $Priv(font) 60 9]
	set canvas [NSTexist::Info $canvistId text]
	$canvas configure -background [Value listBG] \
		-yscrollcommand "$frame.yscroll set" \
		-exportselection no
	scrollbar $frame.yscroll \
		-orient vertical -command "$canvas yview"

	# Selection command
	NSTexist::Info $canvistId selectionCmd \
		"NSDebug::SelectionChanged_Detail $oop"

	# Invoke command
	NSTexist::Info $canvistId invokeCmd \
		"NSDebug::Invoke_Detail $oop"

	Info $oop detail,canvistId $canvistId

	# This call updates the list background color whenever the
	# global list background color changes
	Info $oop detail,clientId \
		[NSValueManager::AddClient listBG "ListBackgroundChanged $canvas"]

	pack $frame.yscroll -side right -fill y
	pack $canvas -side left -expand yes -fill both -anchor nw

	#
	# Edit-value entry
	#

	set frame $win.frameValue
	frame $frame \
		-borderwidth 0
	label $frame.label \
		-text "Value:"
	entry $frame.entry \
		-width 20 -font [Global font,fixed,normal]

	bind $frame.entry <KeyPress-Return> \
		"NSDebug::AcceptValue $oop"

	pack $frame.label -side left
	pack $frame.entry -side left

	Info $oop valueEntry $frame.entry

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
	grid rowconfig $win 4 -weight 0 -minsize 0
	grid rowconfig $win 5 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	if {[Platform windows]} {
		grid $win.divider \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frameList \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameList2 \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frameValue \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky ew -pady 2
	grid $win.statusBar \
		-row 5 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	return
}

# NSDebug::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::InitMenus {oop} {

	set win [Info $oop win]

	# Modifier key
	set mod Ctrl

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSDebug::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbar $mbar

	#
	# Debug Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_DEBUG
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_DEBUG -label Debug -underline 0 -identifier M_DEBUG

	set entries {}
	lappend entries [list -type command -label "New Window" -command "NSObject::New NSDebug" -accelerator $mod+N -underline 0 -identifier E_NEW]
	lappend entries [list -type command -label "Debug Mode" -command "DoUnderlyingCommand ^A" -identifier E_DEBUG_MODE]
	lappend entries [list -type command -label "Reload" -command "InitDebug $oop" -identifier E_INIT_DEBUG]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" -command "NSDebug::Close $oop" -accelerator $mod+W -underline 0 -identifier E_CLOSE]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_DEBUG $entries

	#
	# Character Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_CHARACTER
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_CHARACTER -label Character -underline 0 \
		-identifier M_CHARACTER

	set entries {}
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Cure All Maladies" \
		-command "DoCommandIfAllowed ^Aa" -identifier E_CURE_ALL \
		-accelerator a]
	lappend entries [list -type checkbutton -label "Free Moves *" \
		-command "debug command free_moves" -onvalue 1 -offvalue 0 \
		-variable NSDebug::free_moves -identifier E_FREE_MOVES]
	lappend entries [list -type command -label "Gain Experience" \
		-command "DoCommandIfAllowed ^Ax" -identifier E_GAIN_EXPERIENCE \
		-accelerator x]
	lappend entries [list -type command -label "Phase Door" \
		-command "DoCommandIfAllowed ^Ap" -identifier E_PHASE_DOOR \
		-accelerator p]
	lappend entries [list -type command -label "Rerate Hitpoints" \
		-command "DoCommandIfAllowed ^Ah" -identifier E_RERATE_HITPOINTS \
		-accelerator h]
	lappend entries [list -type command -label "Self Knowledge" \
		-command "DoCommandIfAllowed ^Ak" -identifier E_SELF_KNOWLEDGE \
		-accelerator k]
	lappend entries [list -type command -label "Teleport" \
		-command "DoCommandIfAllowed ^At" -identifier E_TELEPORT \
		-accelerator t]
	lappend entries [list -type command -label "Teleport To Target" \
		-command "DoCommandIfAllowed ^Ab" -identifier E_TELEPORT_TO_TARGET \
		-accelerator b]
	lappend entries [list -type command -label "Teleport To Location" \
		-command "debug command teleport_to_location" \
		-identifier E_TELEPORT_TO_LOCATION]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_CHARACTER $entries

	#
	# Dungeon Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_DUNGEON
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_DUNGEON -label Dungeon -underline 0 -identifier M_DUNGEON

	set entries {}
	lappend entries [list -type checkbutton -label "Auto-Lite *" \
		-command "debug command auto_lite" -onvalue 1 -offvalue 0 \
		-variable NSDebug::auto_lite -identifier E_AUTO_LITE]
	lappend entries [list -type command -label "Detection" \
		-command "DoCommandIfAllowed ^Ad" -identifier E_DETECTION \
		-accelerator d]
	lappend entries [list -type command -label "Jump To Level" \
		-command "DoCommandIfAllowed ^Aj" -identifier E_GOTO_LEVEL \
		-accelerator j]
	lappend entries [list -type command -label "Magic Mapping" \
		-command "DoCommandIfAllowed ^Am" -identifier E_MAGIC_MAPPING \
		-accelerator m]
	lappend entries [list -type command -label "Wizard Dark" -command "" \
		-identifier E_WIZARD_DARK]
	lappend entries [list -type command -label "Wizard Lite" \
		-command "DoCommandIfAllowed ^Aw" -identifier E_WIZARD_LITE \
		-accelerator w]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_DUNGEON $entries

	#
	# Monster Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_MONSTER
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_MONSTER -label Monster -underline 0 -identifier M_MONSTER

	set entries {}
	lappend entries [list -type command -label "Mass Genocide" -command "DoCommandIfAllowed ^Az" -identifier E_MASS_GENOCIDE -accelerator z]
	lappend entries [list -type checkbutton -label "See Monsters *" -command "debug command see_monsters" -onvalue 1 -offvalue 0 -variable NSDebug::see_monsters -identifier E_SEE_MONSTERS]
	lappend entries [list -type command -label "Summon Random" -command "DoCommandIfAllowed ^As" -identifier E_SUMMON_ANY -accelerator s]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_MONSTER $entries

	#
	# Object Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_OBJECT
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_OBJECT -label Object -underline 0 -identifier M_OBJECT

	set entries {}
	lappend entries [list -type command -label "Acquire Good" -command "DoCommandIfAllowed ^Ag" -identifier E_ACQUIRE_GOOD -accelerator g]
	lappend entries [list -type command -label "Acquire Very Good" -command "DoCommandIfAllowed ^Av" -identifier E_ACQUIRE_VERY_GOOD -accelerator v]
	lappend entries [list -type command -label "Allocate Artifact" -command "" -identifier E_CREATE_ARTIFACT]
	lappend entries [list -type command -label "Create Any" -command "" -identifier E_CREATE_OBJECT]
	lappend entries [list -type command -label "Identify" -command "DoCommandIfAllowed ^Ai" -identifier E_IDENTIFY -accelerator i]
	lappend entries [list -type command -label "Identify Fully" -command "^AI" -identifier E_IDENTIFY_FULLY -accelerator I]
	lappend entries [list -type command -label "Identfy Many" -command "" -identifier E_IDENTIFY_MANY]
	lappend entries [list -type command -label "Identify Pack" -command "" -identifier E_IDENTIFY_PACK]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Object List" -command "NSDebug::SetHook $oop debug_hook_object" -identifier E_OBJECT_LIST]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_OBJECT $entries

	return
}

# NSDebug::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SetupMenus {oop mbarID} {

	set isCmd [expr {[string compare [angband inkey_flags] INKEY_CMD] == 0}]

	# See if "debug" mode was entered
	angband player cheat cheat

	# Debug Menu
	lappend identList E_NEW E_INIT_DEBUG E_CLOSE
	if {!$cheat(debug)} {
		lappend identList E_DEBUG_MODE
	}
	
	# Character Menu
	if {$isCmd && $cheat(debug)} {
		lappend identList E_CURE_ALL E_GAIN_EXPERIENCE E_RERATE_HITPOINTS \
			E_PHASE_DOOR E_SELF_KNOWLEDGE E_TELEPORT E_TELEPORT_TO_TARGET \
			E_TELEPORT_TO_LOCATION
	}
	lappend identList E_EQUIPMENT E_INVENTORY E_FREE_MOVES E_EDIT_CHARACTER

	# Dungeon Menu
	if {$isCmd && $cheat(debug)} {
		lappend identList E_DETECTION E_GOTO_LEVEL E_MAGIC_MAPPING \
			E_WIZARD_DARK E_WIZARD_LITE
	}
	lappend identList E_AUTO_LITE

	# Monster Menu
	if {$isCmd && $cheat(debug)} {
		lappend identList E_MASS_GENOCIDE E_SUMMON_ANY
	}
	lappend identList E_SEE_MONSTERS E_MONSTER_LIST

	# Object Menu
	if {$isCmd && $cheat(debug)} {
		lappend identList E_ACQUIRE_GOOD E_ACQUIRE_VERY_GOOD \
			E_CREATE_ARTIFACT E_CREATE_OBJECT E_IDENTIFY E_IDENTIFY_FULLY \
			E_IDENTIFY_MANY E_IDENTIFY_PACK
	}
	lappend identList E_OBJECT_LIST

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSDebug::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::Close {oop} {

	NSValueManager::RemoveClient listBG [Info $oop display,clientId]
	NSValueManager::RemoveClient listBG [Info $oop detail,clientId]

	destroy [Info $oop win]
	NSObject::Delete NSDebug $oop

	return
}

# NSDebug::SelectionChanged_Display --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SelectionChanged_Display {oop canvistId select deselect} {

	# Nothing was selected
	if {![llength $select]} {

		# Clear detail list
		set canvistId [Info $oop detail,canvistId]
		NSTexist::Delete $canvistId 0 end

		# Clear statusbar
		[Info $oop win].statusBar itemconfigure t2 -text ""

		# No row is selected
		Info $oop display,current -1

		# Done
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
	Info $oop display,current $row

	CallHook $oop select_display $row

	return
}

# NSDebug::SelectionChanged_Detail --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SelectionChanged_Detail {oop canvistId select deselect} {

	# If nothing was selected, clear the detail list
	if {![llength $select]} {
		[Info $oop valueEntry] delete 0 end
		Info $oop detail,current -1
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
	Info $oop detail,current $row

	CallHook $oop select_detail $row

	return
}

# NSDebug::Invoke_Detail --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::Invoke_Detail {oop canvistId x y} {

	set row [NSTexist::PointToRow $canvistId $x $y]
	if {$row == -1} return
	CallHook $oop edit $row

	return
}

# NSDebug::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::CallHook {oop message args} {

	return [uplevel #0 NSDebug::[Info $oop hook] $oop $message $args]
}

# NSDebug::SetHook --
#
#	Set the hook. The hook controls what is displayed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SetHook {oop hook} {

	variable Priv

	Info $oop hook $hook

	CallHook $oop open

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

# NSDebug::CalcLineLength --
#
#	Return the number of characters that will fit on a line.#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::CalcLineLength {oop} {

	variable Priv

	set canvistId [Info $oop display,canvistId]
	set text [NSTexist::Info $canvistId text]

	set cw [font measure $Priv(font) "W"]
	set width [winfo width $text]
	return [expr {int([expr {($width - 4) / $cw}])}]
}

# NSDebug::SetList_Display --
#
#	Clears the display list, then calls the hook to append rows to it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SetList_Display {oop} {

	set canvistId [Info $oop display,canvistId]

	# Feedback
	StatusBar $oop "Displaying..." 0
	update idletasks

	# Clear the list
	NSTexist::Delete $canvistId 0 end

	# Call hook to set the display list
	CallHook $oop set_list_display

	# Number of matches
	StatusBar $oop "Done." 1

	# Hack -- Clear the detail list
	set canvistId [Info $oop detail,canvistId]
	NSTexist::Delete $canvistId 0 end

	return
}

# NSDebug::SetList_Detail --
#
#	Clears the detail list, then calls the hook to append rows to it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SetList_Detail {oop} {

	set canvistId [Info $oop detail,canvistId]

	# Clear the list
	NSTexist::Delete $canvistId 0 end

	# Call hook to set the detail list
	CallHook $oop set_list_detail

	return
}

# NSDebug::UpdateList_Detail --
#
#	Set a row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::UpdateList_Detail {oop row text color} {

	set canvistId [Info $oop detail,canvistId]

	# Delete old row
	NSTexist::Delete $canvistId $row $row

	# Insert row again
	NSTexist::Insert $canvistId $row $text $color

	# Select it
	NSTexist::UpdateSelection $canvistId $row {}

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

proc NSDebug::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Priv(hook) [expr {$index * 2}]]

	return
}

# NSDebug::StatusBar --
#
#	Displays text in the status bar, and optionally clears it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSDebug::List_Inventory --
#
#	Display the character's inventory in the first list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::ListInvOrEquip {oop invOrEquip} {

	global NSDebug

	set canvistId $NSDebug($oop,list)

	# Clear the list
	NSTexist::Delete $canvistId 0 end

	# Get a list of items
	set items [angband $invOrEquip find]

	# Append each item
	foreach index $items {

		angband $invOrEquip info $index attrib
		set desc $attrib(name)
		set color [default_tval_to_attr $attrib(tval)]
		set icon $attrib(icon)
		
		NSTexist::Insert $canvistId end $desc $color
	}

	return
}


proc NSDebug::AcceptValue {oop} {

	if {[Info $oop detail,current] == -1} return

	set entry [Info $oop valueEntry]
	set value [$entry get]
	CallHook $oop accept_value $value

	return
}

proc NSDebug::debug_hook_XXX {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		select_display {
		}

		select_detail {
		}

		edit {
		}
	}

	return
}

proc InitDebug {oop} {

	catch {NSDebug::Close $oop}
	NSModule::RebootModule NSDebug

	return
}
