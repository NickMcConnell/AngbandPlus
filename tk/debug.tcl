# File: debug.tcl

# Purpose: the Debug Window for debugging

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSDebug {

	variable Priv

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

	MsgCatInit

	NSModule::LoadIfNeeded NSList

	lappend Priv(hook) hook_character Character
	lappend Priv(hook) hook_monster Monsters
	lappend Priv(hook) hook_object Objects
	lappend Priv(hook) hook_equipment Equipment
	lappend Priv(hook) hook_inventory Inventory
	if {[variant KANGBANDTK ZANGBANDTK]} {
		lappend Priv(hook) hook_quest Quests
	}

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
	Info $oop display,currentItem ""
	Info $oop detail,current -1

#	SetHook $oop hook_monster
Info $oop hook ""

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

	# Verify the object
	NSObject::CheckObject NSDebug $oop

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

	if {$::UseTile} {
		ttk::panedwindow $win.splitterV -orient vertical
	} else {
		panedwindow $win.splitterV -orient vertical -opaqueresize true -sashrelief sunken
	}

	# List of monsters, objects, etc
	set frame $win.frameList
	set tree [NSList::New $frame]
	$tree configure -width 500 -showheader yes

	Info $oop sort,column ""
	$tree notify install <Header-invoke>
	$tree notify bind $tree <Header-invoke> \
		"NSDebug::Sort $oop %C 1"

	# Selection command
	NSList::OnSelection $tree \
		"NSDebug::SelectionChanged_Display $oop %T %c %S %D"

	Info $oop display,tree $tree

	# Type list
	set frame $win.frameList2
	set tree [NSList::New $frame]

	# Selection command
	NSList::OnSelection $tree \
		"NSDebug::SelectionChanged_Detail $oop %T %c %S %D"

	# Invoke command
	NSList::OnInvoke $tree \
		"NSDebug::Invoke_Detail $oop %T %I %r"

	Info $oop detail,tree $tree

	#
	# Controls for editing fields
	#

	set frameEdit $win.frameEdit
	frame $frameEdit \
		-borderwidth 0 -height 40
	pack propagate $frameEdit no

	set frame [frame $frameEdit.frameString -borderwidth 0]
	entry $frame.entry \
		-width 20 -font [Value font,fixed]
	bind $frame.entry <KeyPress-Return> \
		"NSDebug::AcceptValue $oop String"
	pack $frame.entry -side left

	set frame [frame $frameEdit.frameBoolean -borderwidth 0]
	radiobutton $frame.true -text TRUE -variable ::NSDebug($oop,edit,bool) \
		-value TRUE -command "NSDebug::AcceptValue $oop Boolean"
	radiobutton $frame.false -text FALSE -variable ::NSDebug($oop,edit,bool) \
		-value FALSE -command "NSDebug::AcceptValue $oop Boolean"
	pack $frame.true -side left
	pack $frame.false -side left

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	$win.splitterV add $win.frameList
	$win.splitterV add $win.frameList2

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 0 -minsize 0
	grid rowconfig $win 2 -weight 1 -minsize 0
	grid rowconfig $win 3 -weight 1 -minsize 0
	grid rowconfig $win 4 -weight 0 -minsize 0
	grid rowconfig $win 5 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	if {[Platform windows]} {
		grid $win.divider \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
if 1 {
	grid $win.splitterV \
		-row 2 -column 0 -rowspan 2 -columnspan 1 -sticky news
} else {
	grid $win.frameList \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameList2 \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky news
}
	grid $win.frameEdit \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky ew -pady 2
	grid $win.statusBar \
		-row 5 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	bind $win <F5> "NSDebug::SetList_Display $oop"
	qebind $win <Dungeon-enter> "
		if {\[NSDebug::Info $oop hook] ne {}} {
			NSDebug::SetList_Display $oop
		}
	"

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
	lappend entries [list -type command -label [mc Close] -command "NSDebug::Close $oop" -accelerator $mod+W -underline 0 -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_DEBUG $entries

	#
	# Character Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_CHARACTER
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_CHARACTER -label Character -underline 0 \
		-identifier M_CHARACTER

	set entries {}
	lappend entries [list -type command -label "Equipment" \
		-command "NSDebug::SetHook $oop hook_equipment" \
		-identifier E_EQUIPMENT]
	lappend entries [list -type command -label "Inventory" \
		-command "NSDebug::SetHook $oop hook_inventory" \
		-identifier E_INVENTORY]
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
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Edit" \
		-command "NSDebug::SetHook $oop hook_character" \
		-identifier E_EDIT_CHARACTER]

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
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Monster List" -command "NSDebug::SetHook $oop hook_monster" -identifier E_MONSTER_LIST]

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
if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
	lappend entries [list -type command -label "Identify Fully" -command "DoCommandIfAllowed ^AI" -identifier E_IDENTIFY_FULLY -accelerator I]
} else {
	lappend entries [list -type command -label "Identify Fully" -command "DoCommandIfAllowed ^Af" -identifier E_IDENTIFY_FULLY -accelerator f]
}
	lappend entries [list -type command -label "Identfy Many" -command "" -identifier E_IDENTIFY_MANY]
	lappend entries [list -type command -label "Identify Pack" -command "" -identifier E_IDENTIFY_PACK]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Object List" -command "NSDebug::SetHook $oop hook_object" -identifier E_OBJECT_LIST]

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

proc NSDebug::SelectionChanged_Display {oop tree count select deselect} {

	# Nothing was selected
	if {![llength $select]} {

		# Clear detail list
		NSList::Clear [Info $oop detail,tree]

		# Clear statusbar
		[Info $oop win].statusBar itemconfigure t2 -text ""

		# No row is selected
		Info $oop display,current -1
		Info $oop display,currentItem ""

		# Done
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop display,current $row
	Info $oop display,currentItem $item

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

proc NSDebug::SelectionChanged_Detail {oop tree count select deselect} {

	# Nothing was selected
	if {![llength $select]} {
		pack forget {*}[winfo children [Info $oop win].frameEdit]
		Info $oop detail,current -1
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop detail,current $row

	CallHook $oop select_detail $row

	return
}

# NSDebug::Sort --
#
#	Called when a column header is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::Sort {oop column toggle} {

	set tree [Info $oop display,tree]

	if {($column eq [Info $oop sort,column]) && $toggle} {
		if {[Info $oop sort,order,$column] eq "-decreasing"} {
			Info $oop sort,order,$column -increasing
			set arrow up
		} else {
			Info $oop sort,order,$column -decreasing
			set arrow down
		}
	} else {
		if {[Info $oop sort,order,$column] eq "-decreasing"} {
			set arrow down
		} else {
			set arrow up
		}
		Info $oop sort,column $column
	}
	$tree column configure all -arrow none
	$tree column configure $column -arrow $arrow

	set opts [Info $oop sort,opts,$column]
	lappend opts [Info $oop sort,order,$column]
if 0 {
	foreach tag [Info $oop sort,order] {
		if {$tag ne $column} {
			lappend opts -column $tag {*}[Info $oop sort,opts,$tag] {*}[Info $oop sort,order,$tag]
		}
	}
}
	$tree item sort root -column $column {*}$opts

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

proc NSDebug::Invoke_Detail {oop tree item row} {

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

	set tree [Info $oop display,tree]
	foreach column [$tree column id all] {
		$tree column configure $column -button no
	}

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

# NSDebug::SetMatch --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::SetMatch {oop which item data} {

	global NSDebug

	if {$item eq "current"} {
		set item [Info $oop display,currentItem]
	}

	set NSDebug($oop,match,$which,$item) $data

	return
}

# NSDebug::GetMatch --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSDebug::GetMatch {oop which item args} {

	global NSDebug

	if {$item eq "current"} {
		set item [Info $oop display,currentItem]
	}

	return $NSDebug($oop,match,$which,$item)
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

	set tree [Info $oop display,tree]

	# Feedback
	StatusBar $oop "Displaying..." 0
	update idletasks

	# Clear the list
	NSList::Clear $tree

	array unset ::NSDebug $oop,match,display,*

	# Call hook to set the display list
	CallHook $oop set_list_display

	# Number of matches
	StatusBar $oop "Done." 1

	# Hack -- Clear the detail list
	set tree [Info $oop detail,tree]
	NSList::Clear $tree

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

	set tree [Info $oop detail,tree]

	# Clear the list
	NSList::Clear $tree

	array unset ::NSDebug $oop,match,detail,*

	# Call hook to set the detail list
	CallHook $oop set_list_detail

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

proc NSDebug::EditField {oop type index field} {

	set value [struct set $type $index $field]
	set value [NSUtils::StringBox -title "Edit Field" -initial $value \
		-prompt Value -buttons [list [mc OK] [mc Cancel]] \
		-parent [Info $oop win]]
	if {[string length $value]} {
		struct set $type $index $field $value
		SetList_Detail $oop
	}

	return
}

proc NSDebug::AcceptValue {oop type} {

	if {[Info $oop detail,current] == -1} return

	switch -- $type {
		Boolean {
			set value [Info $oop edit,bool]
		}
		String {
			set entry [Info $oop win].frameEdit.frameString.entry
			set value [$entry get]
		}
	}
	CallHook $oop accept_value $value

	return
}

proc NSDebug::SelectDetail {oop struct elem field} {

	array set info [struct fieldinfo $struct $elem $field]

	if {[pack slaves [Info $oop win].frameEdit] ne ""} {
		pack forget [pack slaves [Info $oop win].frameEdit]
	}

	if {!$info(edit)} return

	switch -- $info(type) {
		FLD_BOOL {
			pack [Info $oop win].frameEdit.frameBoolean -side left -padx 6
			Info $oop edit,bool [struct set $struct $elem $field]
		}
		default {
			pack [Info $oop win].frameEdit.frameString -side left -padx 6
			set entry [Info $oop win].frameEdit.frameString.entry
			$entry delete 0 end
			$entry insert end [struct set $struct $elem $field]
		}
	}

	return
}

proc NSDebug::hook_character {oop message args} {

	switch -- $message {

		open {
			set tree [Info $oop display,tree]
			NSList::Config $tree -columns 2
			$tree column configure 0 -expand no -justify right -text "p_idx"
			$tree column configure 1 -expand yes -text "Name"

			set tree [Info $oop detail,tree]
			NSList::Config $tree -columns 2
			$tree column configure 1 -expand yes

			SetList_Display $oop 
		}

		close {
		}

		set_list_display {

			set tree [Info $oop display,tree]

			set item [$tree item create]
			NSList::SetText $tree $item "0: "
			NSList::SetTextEx $tree $item 1 [angband player name]
			$tree item lastchild root $item
		}

		set_list_detail {
			set row [Info $oop display,current]
			set tree [Info $oop detail,tree]
			foreach {title value} [struct set player_type $row] {
				set item [$tree item create]
				NSList::SetText $tree $item $title
				NSList::SetTextEx $tree $item 1 $value
				$tree item lastchild root $item
			}
		}

		select_display {
			SetList_Detail $oop
		}

		select_detail {
			set row [lindex $args 0]
			SelectDetail $oop player_type 0 $row
		}

		accept_value {
			set value [lindex $args 0]
			set row [Info $oop detail,current]
			struct set player_type 0 $row $value

			# Update the detail-list
			NSList::SetTextEx [Info $oop detail,tree] "root child $row" 1 $value
		}
	}

	return
}

proc NSDebug::hook_equipment {oop message args} {

	switch -- $message {

		open {
			set tree [Info $oop display,tree]
			NSList::Config $tree -columns 2
			$tree column configure 0 -expand no -justify left -text "Char"
			$tree column configure 1 -expand yes -text "Description"

			set tree [Info $oop detail,tree]
			NSList::Config $tree -columns 2
			$tree column configure 1 -expand yes

			SetList_Display $oop 
		}

		close {
		}

		set_list_display {

			set tree [Info $oop display,tree]

			# Get a list of items
			set match [angband equipment find]

			# Append each item
			foreach index $match {

				angband equipment info $index attrib
				set name $attrib(name)
				set color [default_tval_to_attr $attrib(tval)]
				set icon $attrib(icon)

				set item [$tree item create]
				NSList::SetText $tree $item "$attrib(char)\) "
				NSList::SetTextEx $tree $item 1 $name
				NSList::SetTextFillEx $tree $item 1 $color
				$tree item lastchild root $item

				SetMatch $oop display $item $index
			}
		}

		set_list_detail {
		}

		select_display {
			set row [lindex $args 0]
			set item [GetMatch $oop display current]
			NSRecall::RecallObject equipment $item
		}

		select_detail {
		}
	}

	return
}

proc NSDebug::hook_inventory {oop message args} {

	switch -- $message {

		open {
			set tree [Info $oop display,tree]
			NSList::Config $tree -columns 2
			$tree column configure 0 -expand no -justify left -text "Char"
			$tree column configure 1 -expand yes -text "Description"

			set tree [Info $oop detail,tree]
			NSList::Config $tree -columns 2
			$tree column configure 1 -expand yes

			SetList_Display $oop 
		}

		close {
		}

		set_list_display {

			set tree [Info $oop display,tree]

			# Get a list of items
			set items [angband inventory find]

			# Append each item
			foreach index $items {
				set item [$tree item create]
				CallHook $oop _set_item_display $item $index
				$tree item lastchild root $item
				SetMatch $oop display $item $index
			}
		}

		set_list_detail {
			set tree [Info $oop detail,tree]
			set i_idx [GetMatch $oop display current]
			foreach {title value} [struct set inventory $i_idx] {
				set item [$tree item create]
				NSList::SetText $tree $item $title
				NSList::SetTextEx $tree $item 1 $value
				$tree item lastchild root $item
			}
		}

		select_display {
			set row [lindex $args 0]
			SetList_Detail $oop
			NSRecall::RecallObject inventory $row
		}

		select_detail {
			set row [lindex $args 0]
			set i_idx [GetMatch $oop display current]
			SelectDetail $oop inventory $i_idx $row
		}

		accept_value {
			set value [lindex $args 0]
			set i_idx [GetMatch $oop display current]
			set row [Info $oop detail,current]
			struct set inventory $i_idx $row $value

			CallHook $oop _set_item_display "root child [Info $oop display,current]" $i_idx
			NSList::SetTextEx [Info $oop detail,tree] "root child $row" 1 $value
		}

		_set_item_display {
			set tree [Info $oop display,tree]
			set item [lindex $args 0]
			set index [lindex $args 1]

			angband inventory info $index attrib
			set desc $attrib(name)
			set color [default_tval_to_attr $attrib(tval)]
			set icon $attrib(icon)
			NSList::SetText $tree $item "$attrib(char)) "
			NSList::SetTextEx $tree $item 1 $desc
			NSList::SetTextFillEx $tree $item 1 $color
		}
	}

	return
}


proc NSDebug::hook_monster {oop message args} {

	switch -- $message {

		open {
			set tree [Info $oop display,tree]
			NSList::Config $tree -columns 3
			$tree column configure "order 0" -expand no -justify right -text "m_idx"
			$tree column configure "order 1" -expand yes -text "Name"
			$tree column configure "order 2" -text "Flags"

			foreach column [$tree column id all] {
				$tree column configure $column -button yes
				Info $oop sort,order,$column -increasing
				Info $oop sort,opts,$column -dictionary
			}
			Info $oop sort,column [$tree column id "order 0"]

			set tree [Info $oop detail,tree]
			NSList::Config $tree -columns 2
			$tree column configure 1 -expand yes

			SetList_Display $oop 
		}

		close {
		}

		set_list_display {

			set tree [Info $oop display,tree]

			set match [angband m_list find -field r_idx != 0]
			foreach m_idx $match {
				set item [$tree item create]
				CallHook $oop _set_item_display $item $m_idx
				$tree item lastchild root $item
				SetMatch $oop display $item $m_idx
			}

			Sort $oop [Info $oop sort,column] 0
		}

		set_list_detail {

			set tree [Info $oop detail,tree]
			set m_idx [GetMatch $oop display current]
			foreach {title value} [angband m_list set $m_idx] {
				set item [$tree item create]
				NSList::SetText $tree $item $title
				NSList::SetTextEx $tree $item 1 $value
				$tree item lastchild root $item
			}
		}

		select_display {

			set row [lindex $args 0]

			SetList_Detail $oop

			set m_idx [GetMatch $oop display current]
			set r_idx [angband m_list set $m_idx r_idx]
			NSRecall::RecallMonster $r_idx

			# Display total number of these monsters.
			set match [angband m_list find -field r_idx == $r_idx]
			set count [llength $match]
			[Info $oop win].statusBar itemconfigure t2 -text "$count in cave"
		}

		select_detail {
			set row [lindex $args 0]
			set m_idx [GetMatch $oop display current]
			SelectDetail $oop monster_type $m_idx $row
		}

		accept_value {
			set value [lindex $args 0]
			set m_idx [GetMatch $oop display current]
			set row [Info $oop detail,current]
			struct set monster_type $m_idx $row $value

			CallHook $oop _set_item_display "root child [Info $oop display,current]" $m_idx
			NSList::SetTextEx [Info $oop detail,tree] "root child $row" 1 $value
		}

		edit {
			set m_idx [GetMatch $oop display current]
			EditField $oop monster_type $m_idx [lindex $args 0]
		}

		_set_item_display {

			set tree [Info $oop display,tree]
			set item [lindex $args 0]
			set m_idx [lindex $args 1]

			# Set array with field/value pairs
			array set m_attrib [angband m_list set $m_idx]

			# Get the icon and name
			set icon [angband r_info info $m_attrib(r_idx) icon]
			set name [angband r_info info $m_attrib(r_idx) name]

			# Flags
			set flag_V [set flag_M [set flag_P [set flag_U [set flag_C " "]]]]

			# Flag Visible 'V'
			if {$m_attrib(ml)} {
				set flag_V V
			}

			# Flag Carrying 'C'
			if {$m_attrib(hold_o_idx)} {
				set flag_C C
			}

			# Flag Unique 'U'
			if {[angband r_info info $m_attrib(r_idx) unique]} {
				set flag_U U
			}

			# Flag Multiply 'M'
			set flags [struct flags monster_race $m_attrib(r_idx) flags2]
			if {[lsearch -exact $flags MULTIPLY] != -1} {
				set flag_M M
			}

			if {[variant KANGBANDTK]} {
				# Flag Pet 'P'
				if {$m_attrib(is_pet)} {
					set flag_p P
				}
			} 
			if {[variant ZANGBANDTK]} {
				# Flag Pet 'P'
				if {$m_attrib(smart) & 0x00800000} {
					set flag_P P
				}
			}
			set flags "$flag_V$flag_U$flag_C$flag_P$flag_M"

			NSList::SetText $tree $item "$m_idx: "
			NSList::SetTextEx $tree $item 1 $name
			NSList::SetTextEx $tree $item 2 $flags
		}
	}

	return
}

proc NSDebug::hook_object {oop message args} {

	switch -- $message {

		open {
			set tree [Info $oop display,tree]
			NSList::Config $tree -columns 4
			$tree column configure "order 0" -expand no -justify right \
				-text "o_idx"
			$tree column configure "order 1" -expand yes -text "Description"
			$tree column configure "order 2" -text "Type"
			$tree column configure "order 3" -text "Flags"

			foreach column [$tree column id all] {
				$tree column configure $column -button yes
				Info $oop sort,order,$column -increasing
				Info $oop sort,opts,$column -dictionary
			}
			Info $oop sort,column [$tree column id "order 0"]

			set tree [Info $oop detail,tree]
			NSList::Config $tree -columns 2
			$tree column configure 1 -expand yes

			SetList_Display $oop 
		}

		close {
		}

		set_list_display {

			set tree [Info $oop display,tree]

			set match [angband o_list find -field k_idx != 0]
			foreach o_idx $match {
				set item [$tree item create]
				CallHook $oop _set_item_display $item $o_idx
				$tree item lastchild root $item
				SetMatch $oop display $item $o_idx
			}

			Sort $oop [Info $oop sort,column] 0
		}

		set_list_detail {

			set tree [Info $oop detail,tree]
			set o_idx [GetMatch $oop display current]
			foreach {title value} [angband o_list set $o_idx] {
				set item [$tree item create]
				NSList::SetText $tree $item $title
				NSList::SetTextEx $tree $item 1 $value
				$tree item lastchild root $item
			}
		}

		select_display {
			set row [lindex $args 0]
			set o_idx [GetMatch $oop display current]
			SetList_Detail $oop
			NSRecall::RecallObject cave $o_idx
		}

		select_detail {
			set row [lindex $args 0]
			set o_idx [GetMatch $oop display current]
			SelectDetail $oop object_type $o_idx $row
		}

		accept_value {
			set value [lindex $args 0]
			set item [Info $oop display,currentItem]
			set o_idx [GetMatch $oop display current]

			set row [Info $oop detail,current]
			struct set object_type $o_idx $row $value

			CallHook $oop _set_item_display $item $o_idx
			NSList::SetTextEx [Info $oop detail,tree] "root child $row" 1 $value
		}

		edit {
			set o_idx [GetMatch $oop display current]
			EditField $oop object_type $o_idx [lindex $args 0]
		}

		_set_item_display {
			set tree [Info $oop display,tree]
			set item [lindex $args 0]
			set o_idx [lindex $args 1]

			# Set array with field/value pairs
			array set o_attrib [angband o_list set $o_idx]

			# Test for non-zero field
#				if {!$o_attrib(k_idx)} continue

			# Get the name and image
			set icon [angband o_list info $o_idx icon]
			set name [angband o_list info $o_idx name]
			set color [default_tval_to_attr $o_attrib(tval)]

			set flag_A [set flag_C " "]

			# Flag Artifact 'A'
			if {$o_attrib(name1)} {
				set flag_A A
			}

			# Flag Carried 'C'
			if {$o_attrib(held_m_idx)} {
				set flag_C C
			}

			# Flags
			set flags $flag_A$flag_C

			NSList::SetText $tree $item "$o_idx: "
			NSList::SetTextEx $tree $item 1 $name
			NSList::SetTextEx $tree $item 2 [string range $o_attrib(tval) 3 end]
			NSList::SetTextEx $tree $item 3 $flags
			NSList::SetTextFillEx $tree $item 1 $color
		}

		sort {
			lassign $args column toggle
		}
	}

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

proc NSDebug::hook_quest {oop message args} {

	switch -- $message {

		open {
			set tree [Info $oop display,tree]
			NSList::Config $tree -columns 3
			$tree column configure 0 -expand no -justify right
			$tree column configure 1 -expand yes

			set tree [Info $oop detail,tree]
			NSList::Config $tree -columns 2
			$tree column configure 1 -expand yes

			SetList_Display $oop 
		}

		close {
		}

		set_list_display {

			set tree [Info $oop display,tree]

			foreach q_idx [struct find quest_type -field type != QUEST_TYPE_NONE] {
				set item [$tree item create]
				CallHook $oop _set_item_display $item $q_idx
				$tree item lastchild root $item
				SetMatch $oop display $item $q_idx
			}
		}

		set_list_detail {

			set tree [Info $oop detail,tree]
			set q_idx [GetMatch $oop display current]
			foreach {title value} [struct set quest_type $q_idx] {
				set item [$tree item create]
				NSList::SetText $tree $item $title
				NSList::SetTextEx $tree $item 1 $value
				$tree item lastchild root $item
			}
		}

		select_display {
			set row [lindex $args 0]
			SetList_Detail $oop
			NSRecall::RecallQuest [GetMatch $oop display current]
		}

		select_detail {
		}

		edit {
			set row [Info $oop display,current]
			set q_idx [lindex [Info $oop display,match] $row]
			EditField $oop quest_type $q_idx [lindex $args 0]
		}

		_set_item_display {
			set tree [Info $oop display,tree]
			set item [lindex $args 0]
			set q_idx [lindex $args 1]

			# Set array with field/value pairs
			array set attrib [struct set quest_type $q_idx]

			switch -- $attrib(type) {
				QUEST_TYPE_KILL_LEVEL -
				QUEST_TYPE_KILL_ANY_LEVEL {
					if {$attrib(r_idx) == 0} {
						if {$attrib(max_num) == 0} {
							set string "$attrib(name): Kill n of monster r"
						} else {
							set string "$attrib(name): Kill $attrib(max_num) of monster r"
						}
					} else {
						set r_name [angband r_info info $attrib(r_idx) name]
						if {[angband r_info info $attrib(r_idx) unique]} {
							set string "$attrib(name): Kill $r_name"
							if {$attrib(cur_num)} {
								append string ", have killed"
							}
						} else {
							set string "$attrib(name): Kill $attrib(max_num) $r_name, have killed $attrib(cur_num)"
						}
					}
				}
				QUEST_TYPE_FIND_ARTIFACT {
					set string "$attrib(name): Find artifact"
				}
				QUEST_TYPE_FIND_EXIT {
					set string "$attrib(name): Find exit"
				}
				QUEST_TYPE_KILL_NUMBER {
					set string "$attrib(name): Kill $attrib(num_mon) creatures, have killed $attrib(cur_num)"
				}
				QUEST_TYPE_KILL_ALL {
					set string "$attrib(name): Kill all monsters"
				}
				QUEST_TYPE_RANDOM {
					set r_name [angband r_info info $attrib(r_idx) name]
					if {[angband r_info info $attrib(r_idx) unique]} {
						set string "Kill $r_name"
					} else {
						if {$attrib(max_num) > 1} {
							set r_name [angband r_info info $attrib(r_idx) plural]
						}
						set string "Kill $attrib(max_num) $r_name"
						if {$attrib(cur_num) < $attrib(max_num)} {
							append string ", have killed $attrib(cur_num)"
						}
					}
				}
			}

			switch -- $attrib(status) {
				QUEST_STATUS_UNTAKEN {
					set status (unassigned)
				}
				QUEST_STATUS_TAKEN {
					set status (incomplete)
				}
				QUEST_STATUS_COMPLETED {
					set status (unrewarded)
				}
				QUEST_STATUS_REWARDED {
					set status (rewarded)
				}
				QUEST_STATUS_FINISHED {
					set status (completed)
				}
			}

			NSList::SetText $tree $item "$q_idx: "
			NSList::SetTextEx $tree $item 1 $string
			NSList::SetTextEx $tree $item 2 $status
		}
	}

	return
}

# KANGBANDTK ZANGBANDTK
}

proc NSDebug::hook_XXX {oop message args} {

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
