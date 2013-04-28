# File: vault-palette.tcl

# Purpose: the Vault Palette Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSVaultPalette {

	variable Priv

# namespace eval NSVaultPalette
}

# NSVaultPalette::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::InitModule {} {

	variable Priv

	# Create the Vault Editor Window
	set Priv(oop) [NSObject::New NSVaultPalette]

	return
}

# NSVaultPalette::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::CloseModule {} {

	variable Priv
	
	catch {
		destroy [NSVaultPalette::Info $Priv(oop) win]
	}

	return
}

# NSVaultPalette::NSVaultPalette --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::NSVaultPalette {oop} {

	Info $oop vaultEditorId $::NSVaultEditor::Priv(editorId)
	Info $oop after ""
	Info $oop dirty 0
	Info $oop drag 0
	Info $oop ignoreClick 0
	Info $oop select 0

	InitWindow $oop

	set win [Info $oop win]

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSVaultPalette $oop $win

	Read $oop

	return
}

# NSVaultPalette::~NSVaultPalette --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::~NSVaultPalette {oop} {

	if {[Info $oop dirty]} {
		set answer [tk_messageBox -message "Save vault palette?" -type yesno]
		if {$answer eq "yes"} {
			Write $oop
		}
	}

	return
}

# NSVaultPalette::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Info {oop info args} {

	global NSVaultPalette

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSVaultPalette($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSVaultPalette($oop,$info)
			}
		}
	}
}

# NSVaultPalette::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::InitWindow {oop} {

	variable Priv

	set win .vaultPalette$oop
	toplevel $win
	wm title $win "Palette"

	# Start out withdrawn (hidden)
	wm withdraw $win

	TransientToWin $win [NSVaultEditor::Info [Info $oop vaultEditorId] win]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSVaultPalette::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	frame $win.divider2 \
		-borderwidth 1 -height 2 -relief groove

	#
	# Canvas + scrollbars
	#

	set frame [frame $win.frameCanvas -borderwidth 0]
	set canvas $frame.canvas
	canvas $canvas -borderwidth 0 -highlightthickness 0 -background White \
		-yscrollcommand "$frame.yscroll set" \
		-xscrollcommand "$frame.xscroll set" \
		-yscrollincrement 24
	scrollbar $frame.yscroll -orient vertical -command "$canvas yview"
	scrollbar $frame.xscroll -orient horizontal -command "$canvas xview"

	bind $canvas <ButtonPress-1> \
		"NSVaultPalette::ButtonPress1 $oop %x %y"
	bind $canvas <Control-ButtonPress-1> \
		"NSVaultPalette::ButtonPress1 $oop %x %y ctrl"
	bind $canvas <Shift-ButtonPress-1> \
		"NSVaultPalette::ButtonPress1 $oop %x %y shift"
	bind $canvas <Button1-Motion> \
		"NSVaultPalette::Button1Motion $oop %x %y"
	bind $canvas <ButtonRelease-1> \
		"NSVaultPalette::ButtonRelease1 $oop %x %y"
	bind $canvas <Button1-Enter> \
		"NSVaultPalette::Button1Enter $oop %x %y"
	bind $canvas <Button1-Leave> \
		"NSVaultPalette::Button1Leave $oop %x %y"

	$canvas bind tile <ButtonPress-1> \
		"NSVaultPalette::TileButtonPress1 $oop %x %y"
	$canvas bind tile <Control-ButtonPress-1> \
		"NSVaultPalette::TileButtonPress1 $oop %x %y ctrl"
	$canvas bind tile <Shift-ButtonPress-1> \
		"NSVaultPalette::TileButtonPress1 $oop %x %y shift"
	$canvas bind tile <Button1-Motion> \
		"NSVaultPalette::TileButton1Motion $oop %x %y"
	$canvas bind tile <ButtonRelease-1> \
		"NSVaultPalette::TileButtonRelease1 $oop %x %y"

	# Selection feedback
	$canvas create rectangle 0 0 1 1 -outline gray -fill "" \
		-state hidden -tags select

	# Context Menu
	set m $canvas.context
	menu $m -tearoff 0
	bind $canvas <ButtonPress-3> \
		"NSVaultPalette::ContextMenu $oop $m %X %Y 0"
	$canvas bind tile <ButtonPress-3> \
		"NSVaultPalette::ContextMenu $oop $m %X %Y 1"

	Info $oop canvas $canvas

	#
	# Geometry
	#

	grid rowconfigure $win.frameCanvas 0 -weight 1
	grid rowconfigure $win.frameCanvas 1 -weight 0
	grid columnconfigure $win.frameCanvas 0 -weight 1
	grid columnconfigure $win.frameCanvas 1 -weight 0

	grid $win.frameCanvas.canvas \
		-row 0 -column 0 -sticky news
	grid $win.frameCanvas.yscroll \
		-row 0 -column 1 -sticky ns
	grid $win.frameCanvas.xscroll \
		-row 1 -column 0 -sticky we

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid columnconfigure $win 0 -weight 1
 
	grid $win.divider2 \
		-row 0 -column 0 -sticky ew
	grid $win.frameCanvas \
		-row 1 -column 0 -sticky news

	bind $win <KeyPress-Delete> \
		"NSVaultPalette::Delete $oop"

	return
}

# NSVaultPalette::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::InitMenus oop {

	return
}

# NSVaultPalette::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::SetupMenus {oop mbarID} {

	return
}

# NSVaultPalette::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::MenuSelect {oop menuId index ident} {

	return
}

# NSVaultPalette::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::MenuInvoke {oop menuId ident args} {

	return
}

# NSVaultPalette::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Close {oop} {

	set win [Info $oop win]
	wm withdraw $win

	return
}

# NSVaultPalette::Synch --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Synch {oop} {

	set canvas [Info $oop canvas]

	if {[scan [$canvas bbox tile] "%s %s %s %s" x1 y1 x2 y2] == 4} {
		incr x2 9
		incr y2 9
		$canvas configure -scrollregion "0 0 $x2 $y2"
	}

	return
}

# NSVaultPalette::ContextMenu --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::ContextMenu {oop menu x y tile} {

	if {[Info $oop ignoreClick]} {
		Info $oop ignoreClick 0
		return
	}

	set canvas [Info $oop canvas]

	# Clear the menu
	$menu delete 0 end

	if {$tile} {
		set itemId [$canvas find withtag current]
		if {[$canvas itemcget $itemId -borderwidth]} {
			set itemId selected
		}
		$menu add command -label "Get Tool Icon" \
			-command "NSVaultPalette::GetToolIcon $oop $itemId"
		$menu add command -label Delete \
			-command "NSVaultPalette::Delete $oop $itemId"

		Info $oop ignoreClick 1
	}

	$menu add separator
	$menu add command -label Cancel

	# Pop up the menu
	tk_popup $menu $x $y

	return
}

# NSVaultPalette::ButtonPress1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::ButtonPress1 {oop x y {mods {}}} {

	if {[Info $oop ignoreClick]} {
		Info $oop ignoreClick 0
		return
	}

	set canvas [Info $oop canvas]
	set editorId [Info $oop vaultEditorId]

	set ctrl [expr {[lsearch -exact $mods ctrl] != -1}]
	set shift [expr {[lsearch -exact $mods shift] != -1}]

	set x [$canvas canvasx $x]
	set y [$canvas canvasy $y]

	SnapToGrid $oop x y

	# Control-click creates a new tile
	if {$ctrl} {
		set icon [NSVaultEditor::Info $editorId tool,icon]
		$canvas create widget $x $y -assign "icon $icon" -anchor center \
			-bordercolor Blue -borderwidth 0 -tags tile
		Info $oop dirty 1
		Synch $oop

		return
	}

	# Remove selection
	if {!$shift} {
		$canvas itemconfigure selected -borderwidth 0
		$canvas dtag selected
	}

	$canvas coords select $x $y $x $y
	$canvas itemconfigure select -state normal
	Info $oop select,hitItems {}
	Info $oop select,x $x
	Info $oop select,y $y
	Info $oop select 1

	return
}

# NSVaultPalette::Button1Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Button1Motion {oop x y {mods {}}} {

	set canvas [Info $oop canvas]

	if {[Info $oop select]} {
		set x1 [Info $oop select,x]
		set y1 [Info $oop select,y]
	    set x2 [$canvas canvasx $x]
	    set y2 [$canvas canvasy $y]

		$canvas coords select $x1 $y1 $x2 $y2

		set hitItemsPrev [Info $oop select,hitItems]
		set hitItems {}

		# Find items covered by the selection rectangle
		set list [$canvas find overlapping $x1 $y1 $x2 $y2]
		foreach itemId $list {
			if {[$canvas type $itemId] eq "widget"} {
				lappend hitItems $itemId
			}
		}

		foreach itemId $hitItemsPrev {
			# Was hit, not hit now
			if {[lsearch -exact $hitItems $itemId] == -1} {
				HighlightTile $oop $itemId -1
			}
		}

		foreach itemId $hitItems {
			# Is hit, was not hit before
			if {[lsearch -exact $hitItemsPrev $itemId] == -1} {
				HighlightTile $oop $itemId -1
			}
		}

		Info $oop select,hitItems $hitItems
	}

	return
}

# NSVaultPalette::ButtonRelease1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::ButtonRelease1 {oop x y {mods {}}} {

	set canvas [Info $oop canvas]

	if {[Info $oop select]} {
		$canvas itemconfigure select -state hidden
		Info $oop select 0
	}

	StopAutoScan $oop

	return
}

# NSVaultPalette::Button1Enter --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Button1Enter {oop x y} {

	StopAutoScan $oop

	return
}

# NSVaultPalette::Button1Leave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Button1Leave {oop x y} {

	AutoScan $oop $x $y

	return
}

# NSVaultPalette::AutoScan --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::AutoScan {oop x y} {

	set canvas [Info $oop canvas]

	if {![winfo exists $canvas]} return

	if {$y >= [winfo height $canvas]} {
		$canvas yview scroll 1 units
	} elseif {$y < 0} {
		$canvas yview scroll -1 units
	} elseif {$x >= [winfo width $canvas]} {
		$canvas xview scroll 1 units
	} elseif {$x < 0} {
		$canvas xview scroll -1 units
	} else {
		return
	}

	Info $oop after [after 50 "NSVaultPalette::AutoScan $oop $x $y"]

	return
}

# NSVaultPalette::StopAutoScan --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::StopAutoScan {oop} {

	if {[Info $oop after] eq ""} return
	after cancel [Info $oop after]
	Info $oop after ""

	return
}

# NSVaultPalette::TileButtonPress1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::TileButtonPress1 {oop x y {mods {}}} {

	set canvas [Info $oop canvas]

	set ctrl [expr {[lsearch -exact $mods ctrl] != -1}]
	set shift [expr {[lsearch -exact $mods shift] != -1}]

	Info $oop click,ctrl $ctrl
	Info $oop click,shift $shift
	Info $oop click,item [$canvas find withtag current]

	Info $oop ignoreClick 1

	set itemId [$canvas find withtag current]

	if {$shift} {
		HighlightTile $oop $itemId -1
	}

	# Click 
	set x [$canvas canvasx $x]
	set y [$canvas canvasy $y]
	SnapToGrid $oop x y
	Info $oop drag,x $x
	Info $oop drag,y $y
	Info $oop drag,motion 0
	Info $oop drag 1

	return
}

# NSVaultPalette::TileButton1Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::TileButton1Motion {oop x y {mods {}}} {

	set canvas [Info $oop canvas]

	if {[Info $oop drag]} {
		set x [$canvas canvasx $x]
		set y [$canvas canvasy $y]

		SnapToGrid $oop x y

		set dx [expr {$x - [Info $oop drag,x]}]
		set dy [expr {$y - [Info $oop drag,y]}]

		# Wait for initial click to move by 4 pixels
		if {![Info $oop drag,motion]} {
			if {abs($dx) > 4 || abs($dy) > 4} {
				Info $oop drag,motion 1

				# Temporarily hide selection while dragging
				$canvas itemconfigure selected -borderwidth 0

				$canvas raise selected

				# Control-drag to copy
				if {[Info $oop click,ctrl]} {
					set list [$canvas find withtag selected]
					foreach itemId $list {
						scan [$canvas coords $itemId] "%s %s" x2 y2
						set assign [$canvas itemcget $itemId -assign]
						$canvas create widget $x2 $y2 -assign $assign \
							-bordercolor Blue -borderwidth 0 -anchor center \
							-tags tile
						Info $oop dirty 1
					}
				}
			}
		}
		if {($dx || $dy) && [Info $oop drag,motion]} {

			# If clicked tile is selected, drag all selected tiles.
			# Otherwise drag only the clicked tile
			set itemId [Info $oop click,item]
			if {[lsearch -exact [$canvas itemcget $itemId -tags] selected] != -1} {
				$canvas move selected $dx $dy
			} else {
				$canvas move $itemId $dx $dy
			}
			Info $oop drag,x $x
			Info $oop drag,y $y

			Info $oop dirty 1
			Synch $oop
		}
	}

	return
}

# NSVaultPalette::TileButtonRelease1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::TileButtonRelease1 {oop x y {mods {}}} {

	set canvas [Info $oop canvas]
	set editorId [Info $oop vaultEditorId]

	if {[Info $oop drag] && [Info $oop drag,motion]} {
		$canvas itemconfigure selected -borderwidth 2
	} elseif {[Info $oop click,shift]} {
	} else {

		# Set the Vault Editor tool icon
		if {[NSVaultEditor::Info $editorId editMode] eq "icon"} {
			set assign [$canvas itemcget current -assign]
			set iconFG [lrange $assign 1 end]
			NSVaultEditor::SetToolIcon $editorId $iconFG
		}
	}

	return
}

# NSVaultPalette::HighlightTile --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::HighlightTile {oop itemId high} {

	set canvas [Info $oop canvas]

	# Toggle
	if {$high == -1} {
		if {[$canvas itemcget $itemId -borderwidth]} {
			set high 0
		} else {
			set high 1
		}
	}

	# On
	if {$high} {
		$canvas itemconfigure $itemId -borderwidth 2
		$canvas addtag selected withtag $itemId

	# Off
	} else {
		$canvas itemconfigure $itemId -borderwidth 0
		$canvas dtag $itemId selected
	}

	return
}

# NSVaultPalette::SnapToGrid --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::SnapToGrid {oop _x _y} {

	upvar $_x x $_y y

	set x [expr {int($x) / 9 * 9}]
	set y [expr {int($y) / 9 * 9}]

	return
}

# NSVaultPalette::GetToolIcon --
#
#	Set the icon of given tile.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::GetToolIcon {oop {tagOrId selected}} {

	set canvas [Info $oop canvas]
	set editorId [Info $oop vaultEditorId]

	set icon [NSVaultEditor::Info $editorId tool,icon]
	$canvas itemconfigure $tagOrId -assign "icon $icon"
	Info $oop dirty 1

	return
}

# NSVaultPalette::SetToolIcon --
#
#	Set editor tool icon using given tile.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::SetToolIcon {oop itemId} {

	set canvas [Info $oop canvas]
	set editorId [Info $oop vaultEditorId]

	if {[string equal [NSVaultEditor::Info $editorId editMode] "feature"]} return

	set assign [$canvas itemcget $itemId -assign]
	set icon [lrange $assign 1 end]

	NSVaultEditor::SetToolIcon $editorId $icon

	return
}

# NSVaultPalette::Delete --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Delete {oop {tagOrId selected}} {

	set canvas [Info $oop canvas]

	$canvas delete $tagOrId

	return
}

# NSVaultPalette::Read --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Read {oop} {

	set canvas [Info $oop canvas]

	set path [CPathTk vault palette-[Global config,prefix]]
	if {[file exists $path]} {
		set chan [open $path]
		set buf [read $chan]
		close $chan

		foreach list [split $buf \n] {
if {[llength $list] == 3} {
	scan [lindex $list 0] "%s %s" x y
	set assign [lindex $list 1]
	set assignbg [lindex $list 2]
	if {$assign eq "none 0"} {
		set assign $assignbg
	}
	$canvas create widget $x $y -assign $assign \
		-anchor center \
		-bordercolor Blue -borderwidth 0 -tags tile
}
			if {[llength $list] == 2} {
				scan [lindex $list 0] "%s %s" x y
				set assign [lindex $list 1]
				$canvas create widget $x $y -assign $assign \
					-anchor center -bordercolor Blue -borderwidth 0 \
					-tags tile
			}
		}
	}

	Synch $oop

	return
}

# NSVaultPalette::Write --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultPalette::Write {oop} {

	set canvas [Info $oop canvas]

	set path [CPathTk vault palette-[Global config,prefix]]
	if {[file exists $path]} {
		file rename -force $path $path.bak
	}
	set chan [open $path w]
	fconfigure $chan -translation lf
	
	foreach itemId [$canvas find withtag tile] {
		set assign [$canvas itemcget $itemId -assign]
		puts $chan [list [$canvas coords $itemId] $assign]
	}

	close $chan

	Info $oop dirty 0

	return
}
