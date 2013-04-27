# File: pets.tcl

# Purpose: the Pets Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
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
		"GetDefaultGeometry $win reqwidth reqheight" "" \
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

	NSValueManager::RemoveClient font,knowledge [Info $oop clientId,font]

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
	wm title $win "Pets"

	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSPets::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

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

	frame $win.frame \
		-borderwidth 1 -relief sunken

	# Get the desired font
	set font [Value font,knowledge]

	# Column titles
	set canvas2 $win.frame.header
	canvas $canvas2 \
		-borderwidth 0 -highlightthickness 0
	MakeStatus $oop $canvas2 0 0 400 foo
	set fontHeight [font metrics $font -linespace]
	set height [expr {3 + $fontHeight + 3}]
	$canvas2 configure -height $height

	foreach title {Name Items Status} anchor {nw ne ne nw} {
		$canvas2 create text 0 3 -text $title -fill gray -anchor $anchor \
			-font $font -tags header,$title
	}

	Info $oop header,canvas $canvas2

	set rowHgt [font metrics $font -linespace]
	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}
	incr rowHgt 8

	set color [format #%02x%02x%02x 0 0 153]

	set canvistId [NSObject::New NSCanvist $win.frame $rowHgt 400 250 \
		"NSPets::NewItemCmd $oop" "NSPets::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -background $color -yscrollcommand "$win.frame.scroll set"
	scrollbar $win.frame.scroll \
		-command "$canvas yview" -orient vertical

	# Update ourself when the font,knowledge value changes
	Info $oop clientId,font \
		[NSValueManager::AddClient font,knowledge \
		"NSPets::ValueChanged_font_knowledge $oop"]

	# When a pet is selected, recall it
	NSCanvist::Info $canvistId selectionCmd \
		"NSPets::SelectionChanged $oop"

	bind $canvas <Configure> \
		"NSPets::Configure $oop"

	Info $oop canvistId $canvistId
	Info $oop canvas $canvas

	pack $win.frame.scroll -side right -fill y
	pack $win.frame.header -side top -expand no -fill x
	pack $canvas -side left -expand yes -fill both -anchor nw

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
	bind $canvas <ButtonPress-3> \
		"NSPets::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	focus $canvas
	Term_KeyPress_Bind $canvas
	bind $win <Control-KeyPress-w> "NSPets::Close $oop"

	#
	# Synch the scrollbars when window is shown.
	#

	NSUtils::SynchScrollBar $canvas $win.frame.scroll

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
		-menu MENU_PETS -label "Pets" -underline 0 -identifier M_PETS

	set entries {}
	lappend entries [list -type command -label "Dismiss Selected" \
		-command "NSPets::Dismiss $oop selected" -underline 0 \
		-identifier E_DISMISS_SELECTED]
	lappend entries [list -type command -label "Dismiss All" \
		-command "NSPets::Dismiss $oop all" -underline 8 \
		-identifier E_DISMISS_ALL]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" \
		-command "NSPets::Close $oop" -underline 0 \
		-accelerator $mod+W -identifier E_CLOSE]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_PETS $entries

	#
	# Command Menu
	#

	set menuId [NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_COMMAND]
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_COMMAND -label "Command" -underline 0 -identifier M_COMMAND
	
	set entries {}
	lappend entries [list -type radiobutton -label "Stay Close" \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_CLOSE_DIST] -identifier E_CLOSE_DIST]
	lappend entries [list -type radiobutton -label "Follow Me" \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_FOLLOW_DIST] -identifier E_FOLLOW]
	lappend entries [list -type radiobutton -label "Seek And Destroy" \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_DESTROY_DIST] -identifier E_DESTROY]
	lappend entries [list -type radiobutton -label "Give Me Space" \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_SPACE_DIST] -identifier E_SPACE]
	lappend entries [list -type radiobutton -label "Stay Away" \
		-variable NSPets($oop,pet_follow_distance) \
		-value [const PET_AWAY_DIST] -identifier E_AWAY]
	lappend entries [list -type separator]
	lappend entries [list -type checkbutton -label "Open Doors" \
		-underline 0 \
		-variable NSPets($oop,pet_open_doors) -identifier E_DOORS]
	lappend entries [list -type checkbutton -label "Pick Up Items" \
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

	[Info $oop win].statusBar cover show

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
			set desc "Allow pets to open doors."
		}
		E_ITEMS {
			set desc "Allow pets to pick up items."
		}
		
		default {
			set menu [NSMenu::Info $menuId menu]
			set desc [$menu entrycget $index -label]
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
if 0 {
			# XXX Hack -- Synchronzize the radiobutton menu entries
			Info $oop pet_follow_distance \
				[struct set player_type 0 pet_follow_distance]

			# XXX Hack -- Synchronzize the checkbutton menu entries
			Info $oop pet_open_doors \
				[struct set player_type 0 pet_open_doors]
			Info $oop pet_pickup_items \
				[struct set player_type 0 pet_pickup_items]
			
			SetList $oop
}		
		}
		postDisplay {
		}
		postWithdraw {
			NSCanvist::DeleteAll [Info $oop canvistId]
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

proc NSPets::SelectionChanged {oop canvistId select deselect} {

	# Do nothing if no new row was selected
	if {![llength $select]} {
		Info $oop current -1
		StatusBar $oop "" 0
		return
	}

	# Get the (first) row
	set row [lindex $select 0]
	Info $oop current $row

	set r_idx [lindex [Info $oop r_match] $row]
	
	# Display memory for the selected pet
	#NSRecall::RecallMonster $r_idx

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

# NSPets::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::NewItemCmd {oop canvistId y iconSpec name numObjects status {color White}} {

	variable Priv

	set c [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Value font,knowledge]

	set fh [font metrics $font -linespace]
	set diff [expr {int([expr {($lineHeight - $fh) / 2}])}]

if 0 {
	# Image
	if {[string length $iconSpec]} {
		set iw [icon size]
		set ih [icon size]
		set wid [expr {[icon size] + 8}]
		set xdiff [expr {int([expr {($wid - $iw) / 2}])}]
		set ydiff [expr {int([expr {($lineHeight - $ih) / 2}])}]
		lappend itemIdList [$c create widget $xdiff [expr {$y + $ydiff}] \
			-assign $iconSpec]
	}
}
	# Name
	lappend itemIdList [$c create text 0 [expr {$y + $diff}] \
		-text $name -anchor nw -font $font -fill $color -tags name]

	# Option: Pets can pick up items
	if {[Info $oop pet_pickup_items]} {

		# Number of objects
		lappend itemIdList [$c create text 0 [expr {$y + $diff}] \
			-text $numObjects -anchor ne -font $font -fill $color \
			-tags objects]
	}
	
	# Status
	lappend itemIdList [$c create text 0 [expr {$y + $diff}] \
		-text $status -anchor nw -font $font -fill $color \
		-tags status]

	# Selection rectangle around everything
	lappend itemIdList [$c create rectangle 2 [expr {$y + 2}] \
		2 [expr {$y + $lineHeight - 2}] -fill {} -outline {} \
		-tags selrect -width 2.0]

	# Maximum width of name
	set width [font measure $font $name]
	if {$width > $Priv(width,name)} {
		set Priv(width,name) $width
	}

	# Maximum width of objects
	set width [font measure $font $numObjects]
	if {$width > $Priv(width,objects)} {
		set Priv(width,objects) $width
	}

	# Maximum width of status
	set width [font measure $font $status]
	if {$width > $Priv(width,status)} {
		set Priv(width,status) $width
	}

	return $itemIdList
}

# NSPets::PositionItems --
#
#	Arranges all the canvas items in the list. This is called after all
#	the items are added, and when the <Configure> event indicates the
#	window has been resized. This is the routine that lets variable-width
#	fonts work.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::PositionItems {oop} {

	variable Priv

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	# Get the header canvas
	set header [Info $oop header,canvas]

	# Get the font
	set font [Value font,knowledge]

	set offset [expr {[icon size] + 8}]

	# The list is empty
	if {![NSCanvist::Info $canvistId count]} {

		if {[Info $oop pet_pickup_items]} {
			set titleList [list Name Items Status]
			set alignList [list nw ne ne nw]
		} else {
			$header coords header,Items 0 3
			set titleList [list Name Status]
			set alignList [list nw ne nw]
		}
		foreach title $titleList align $alignList {
			set label $title
			if {[string equal $align ne]} {
				incr offset [font measure $font $label]
			}
			$header coords header,$title $offset 3
			if {[string equal $align nw]} {
				incr offset [font measure $font $label]
			}
			incr offset 16
		}

		# Done
		return
	}

	# Get the width of the canvas
	set canvasWidth [winfo width $canvas]

	# Configure the name header
	$header coords header,Name $offset 3
	
	# Position each name
	set coords [$canvas coords name]
	$canvas move name [expr {$offset - [lindex $coords 0]}] 0
	incr offset $Priv(width,name)

	# Option: Pets can pick up items
	if {[Info $oop pet_pickup_items]} {
	
		# Position each objects
		incr offset $Priv(width,objects)
		set coords [$canvas coords objects]
		$canvas move objects [expr {$offset - [lindex $coords 0]}] 0
	
		# Configure the objects header
		$header coords header,Items $offset 3

	} else {

		# Configure the objects header
		$header coords header,Items 0 3
	}
	

	# Position each status
	incr offset [font measure $font "AB"]
	set coords [$canvas coords status]
	$canvas move status [expr {$offset - [lindex $coords 0]}] 0

	# Configure the status header
	$header coords header,Status $offset 3

	# Position each selection rectangle
	set x1 [expr {($canvasWidth - 1) - 2}]
	foreach itemId [$canvas find withtag selrect] {
		scan [$canvas coords $itemId] "%s %s %s %s" c0 c1 c2 c3
		$canvas coords $itemId $c0 $c1 $x1 $c3
	}

	# Set the scrollregion to prevent horizontal scrolling
	scan [$canvas cget -scrollregion] "%s %s %s %s" x1 y1 x2 y2
	$canvas configure -scrollregion "$x1 $y1 $canvasWidth $y2"

	return
}

# NSPets::HighlightItemCmd --
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

proc NSPets::HighlightItemCmd {oop canvistId state args} {

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

# NSPets::Configure --
#
#	Respond to the <Configure> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::Configure {oop} {

	# Arrange list items
	PositionItems $oop

	# Resize the list header
	set canvas [Info $oop header,canvas]
	ConfigureStatus $oop $canvas 0 0 [winfo width $canvas] foo

	return
}

# NSPets::MakeStatus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::MakeStatus {oop canvas x y width where} {

	set font [Value font,magic]
	set fontHeight [font metrics $font -linespace]
	set height [expr {3 + $fontHeight + 3}]

	# 3-pixel border + background
	$canvas create rectangle [expr {$x + 1}] [expr {$y + 1}] \
		[expr {$x + $width - 2}] [expr {$y + $height - 2}] \
		-outline #282828 -fill #0000D4 -width 3.0 \
		-tags $where,bg

	# 1-pixel border
	$canvas create rectangle [expr {$x + 1}] [expr {$y + 1}] \
		[expr {$x + $width - 2}] [expr {$y + $height - 2}] \
		-outline #0070FF -fill {} \
		-tags $where,bd

	return
}

proc NSPets::ConfigureStatus {oop canvas x y width where} {

	set font [Value font,knowledge]
	set fontHeight [font metrics $font -linespace]
	set height [expr {3 + $fontHeight + 3}]

	# 3-pixel border + background
	$canvas coords $where,bg [expr {$x + 1}] [expr {$y + 1}] \
		[expr {$x + $width - 2}] [expr {$y + $height - 2}]

	# 1-pixel border
	$canvas coords $where,bd [expr {$x + 1}] [expr {$y + 1}] \
		[expr {$x + $width - 2}] [expr {$y + $height - 2}]

	return
}

# NSPets::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Pets Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPets::ValueChanged_font_knowledge {oop} {

	set win [Info $oop win]

	# Get the desired font
	set font [Value font,knowledge]

	# Get the font height
	set fontHgt [font metrics $font -linespace]

	# Calculate row height
	set rowHgt $fontHgt
	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}
	incr rowHgt 8

	# Set the row height of the list
	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	# Set font of list header
	set canvas [Info $oop header,canvas]
	foreach title [list Name Items Status] {
		$canvas itemconfigure header,$title -font $font
	}

	# Resize the list header
	$canvas configure -height [expr {3 + $fontHgt + 3}]
	ConfigureStatus $oop $canvas 0 0 [winfo width $canvas] foo

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

	set canvistId [Info $oop canvistId]

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
			foreach row [NSCanvist::Selection $canvistId] {
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

	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $canvas]}]
	set y1 [expr {$y - [winfo rooty $canvas]}]
	set row [NSCanvist::PointToRow $canvistId $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	if {$row != -1} {
		$menu add command -label "Dismiss This Pet" \
			-command "NSPets::Dismiss $oop one $row"
		$menu add command -label "Dismiss This Type" \
			-command "NSPets::Dismiss $oop race $row"
	}
	if {[llength [NSCanvist::Selection $canvistId]]} {
		$menu add command -label "Dismiss Selected" \
			-command "NSPets::Dismiss $oop selected"
	}
	if {[llength [angband player pets]]} {
		$menu add command -label "Dismiss All" \
			-command "NSPets::Dismiss $oop all"
	}
	if {[string compare [$menu index end] none]} {
		$menu add separator
	}
	$menu add command -label "Close" \
		-command "NSPets::Close $oop"
	$menu add separator
	$menu add command -label "Cancel"
	
	# Pop up the menu
	tk_popup $menu $x $y

	return
}

