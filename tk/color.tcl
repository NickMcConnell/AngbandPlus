# File: color.tcl

# Purpose: the Color Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSColorPreferences {

	variable Hook

# namespace eval NSColorPreferences
}

# NSColorPreferences::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::InitModule {} {

	global Angband
	variable Hook

	MsgCatInit color

	NSModule::LoadIfNeeded NSColorPicker

if 0 {

	# Silly: Map original angband TERM_XXX rgb values to indexes into
	# our standard 256-color palette.
	set Angband(color,TERM_DARK) 255
	set Angband(color,TERM_WHITE) 0
	set Angband(color,TERM_SLATE) 250
	set Angband(color,TERM_ORANGE) 17
	set Angband(color,TERM_RED) 217
	set Angband(color,TERM_GREEN) 196
	set Angband(color,TERM_BLUE) 204
	set Angband(color,TERM_UMBER) 101
	set Angband(color,TERM_L_DARK) 129
	set Angband(color,TERM_L_WHITE) 247
	set Angband(color,TERM_VIOLET) 30
	set Angband(color,TERM_YELLOW) 5
	set Angband(color,TERM_L_RED) 35
	set Angband(color,TERM_L_GREEN) 185
	set Angband(color,TERM_L_BLUE) 180
	set Angband(color,TERM_L_UMBER) 52

	# Silly: Map TERM_XXX colors to objects by tval.
	lappend tval_to_attr WHITE
	lappend tval_to_attr WHITE
	lappend tval_to_attr WHITE
	lappend tval_to_attr WHITE
	lappend tval_to_attr SLATE
	lappend tval_to_attr SLATE
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr UMBER
	lappend tval_to_attr SLATE
	lappend tval_to_attr WHITE
	lappend tval_to_attr WHITE
	lappend tval_to_attr WHITE
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr SLATE
	lappend tval_to_attr SLATE
	lappend tval_to_attr SLATE
	lappend tval_to_attr YELLOW
	lappend tval_to_attr ORANGE
	lappend tval_to_attr RED
	lappend tval_to_attr L_UMBER
	lappend tval_to_attr GREEN
	lappend tval_to_attr VIOLET
	lappend tval_to_attr WHITE
	lappend tval_to_attr L_BLUE
	lappend tval_to_attr YELLOW
	lappend tval_to_attr L_UMBER
	if {[variant ZANGBANDTK]} {
		lappend tval_to_attr WHITE
		lappend tval_to_attr L_BLUE
		lappend tval_to_attr L_GREEN
	}
	lappend tval_to_attr L_RED
	if {[variant ANGBANDTK KANGBANDTK]} {
		lappend tval_to_attr L_GREEN
	}
	if {[variant ZANGBANDTK]} {
		lappend tval_to_attr L_DARK
		lappend tval_to_attr ORANGE
		lappend tval_to_attr L_WHITE
	}

	# Get a list of tval's.
	set Angband(tval) {}
	lappend Angband(tval) TV_NONE TV_SKELETON TV_BOTTLE TV_JUNK TV_SPIKE TV_CHEST
	lappend Angband(tval) TV_SHOT TV_ARROW TV_BOLT TV_BOW TV_DIGGING
	lappend Angband(tval) TV_HAFTED TV_POLEARM TV_SWORD TV_BOOTS
	lappend Angband(tval) TV_GLOVES TV_HELM TV_CROWN TV_SHIELD TV_CLOAK
	lappend Angband(tval) TV_SOFT_ARMOR TV_HARD_ARMOR TV_DRAG_ARMOR
	lappend Angband(tval) TV_LITE TV_AMULET TV_RING TV_STAFF TV_WAND TV_ROD
	lappend Angband(tval) TV_SCROLL TV_POTION TV_FLASK TV_FOOD
	if {[variant ANGBANDTK KANGBANDTK]} { 
		lappend Angband(tval) TV_MAGIC_BOOK TV_PRAYER_BOOK TV_GOLD
	}
	if {[variant ZANGBANDTK]} {
		lappend Angband(tval) TV_LIFE_BOOK TV_SORCERY_BOOK TV_NATURE_BOOK
		lappend Angband(tval) TV_CHAOS_BOOK TV_DEATH_BOOK TV_TRUMP_BOOK
		lappend Angband(tval) TV_ARCANE_BOOK TV_GOLD
	}

	# Let NSValueManager keep track of colors assigned to each tval.
	# The value name is the tval name, and the value's value (?) is
	# an index into our 256-color palette.
	set i -1
	foreach tval $Angband(tval) {
		set color [lindex $tval_to_attr [incr i]]
		if {![string length $color]} {set color WHITE}
		set color $Angband(color,TERM_$color)
		set Angband(tval_to_attr,$tval) $color
		NSValueManager::Manage $tval $Angband(tval_to_attr,$tval)
	}
# NOT USED
}

	set Hook {}
	lappend Hook color_hook_list List
#	lappend Hook color_hook_map "Micro Map"
	lappend Hook color_hook_monster_bar "Monster Bar"
	lappend Hook color_hook_status Status
	lappend Hook hook_target Target
	lappend Hook hook_icons Icons

	NSObject::New NSColorPreferences

	return
}

# NSColorPreferences::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::CloseModule {} {

	catch {
		destroy [Window color]
	}

	return
}

# NSColorPreferences::NSColorPreferences --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::NSColorPreferences {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow color $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSColorPreferences::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSColorPreferences $oop $win

	bind $win <KeyPress-Escape> "NSColorPreferences::Close $oop"
	bind $win <Control-KeyPress-w> "NSColorPreferences::Close $oop"

	Info $oop activeItem -1
	Info $oop ignoreOpacity 1

	#
	# Global list of application windows
	#

	Global color,oop $oop
	Window color $win

	return
}

# NSColorPreferences::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Info {oop info args} {

	global NSColorPreferences

	# Verify the object
	NSObject::CheckObject NSColorPreferences $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSColorPreferences($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSColorPreferences($oop,$info)
			}
		}
	}

	return
}

# NSColorPreferences::InitWindow --
#
#	Create the toplevel associated with this object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::InitWindow {oop} {

	variable Hook

	set win .colorpreferences$oop
	toplevel $win
	wm title $win [mc Color]
	wm resizable $win no no

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSColorPreferences::Close $oop"

	Info $oop win $win

	# Menus
	InitMenus $oop

	MakeDivider $win.divider1 x

	# Tabs!
	set tabsId [NSObject::New NSTabs $win]
	foreach {hook label} $Hook {
		NSTabs::Add $tabsId [mc $label]
	}
	NSTabs::Info $tabsId invokeCmd "NSColorPreferences::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	# Frame for canvas and scrollbar
	set frame $win.frameCanvas
	frame $frame \
		-borderwidth 2 -relief sunken -width 320 -height 200

	# The main canvas, where the current thing to modify is displayed
	set canvas $frame.canvas
	canvas $canvas \
		-width 320 -height 200 -background White \
		-borderwidth 0 -highlightthickness 0 \
		-yscrollcommand "$frame.yscroll set"
	place $canvas \
		-x 0 -y 0

	Info $oop canvas $canvas

	# Create a scrollbar for the canvas. It only appears if needed.
	scrollbar $frame.yscroll \
		-orient vertical -command "$canvas yview"

	# Color picker
	set pickerId [NSObject::New NSColorPicker $win 32 8 10]
	for {set i 0} {$i < 256} {incr i} {
		lappend colors [palette set $i]
	}
	NSColorPicker::SetColors $pickerId $colors
	NSColorPicker::Info $pickerId command \
		"NSColorPreferences::ColorChanged $oop"
	bind $canvas <ButtonPress-1> "
		NSColorPreferences::Button1 $oop %x %y
	"
	Info $oop pickerId $pickerId

	# Color info label
	label $win.labelColor \
		-text ""
	bind [NSColorPicker::Info $pickerId canvas] <Motion> "
		$win.labelColor configure -text \"Index: \[NSColorPicker::GetIndex $pickerId %x %y]    \[palette set \[NSColorPicker::GetIndex $pickerId %x %y]]\"
	"
	bind [NSColorPicker::Info $pickerId canvas] <Leave> "
		$win.labelColor configure -text {}
	"

	# Opacity scale + labels
	set frame $win.frameOpacity
	frame $frame \
		-borderwidth 0
	frame $frame.label \
		-borderwidth 0
	label $frame.label.title \
		-text [mc Opacity] -borderwidth 0
	label $frame.label.value \
		-borderwidth 0
	scale $frame.opacity \
		-orient horizontal -label "" -showvalue no \
		-width 8 -sliderlength 12 -length 255 -from 0 -to 255 \
		-command "NSColorPreferences::OpacityChanged $oop"
	pack $frame.label.title \
		-side left -padx 5
	pack $frame.label.value \
		-side right -padx 5
	pack $frame.label \
		-side top -fill x
	pack $frame.opacity \
		-side top

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
		grid $win.divider1 \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew \
		-padx 5 -pady 0
	grid $win.frameCanvas \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news \
		-padx 5 -pady 0
	grid [NSColorPicker::Info $pickerId canvas] \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky n \
		-padx 5 -pady 5
	grid $win.labelColor \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky n \
		-padx 5 -pady 0
if 0 {
	grid $win.frameOpacity \
		-row 5 -column 0 -rowspan 1 -columnspan 1 -sticky n \
		-padx 5 -pady 5
}

	return
}

# NSColorPreferences::InitMenus --
#
#	Create menus in the toplevel associated with this object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::InitMenus {oop} {

	variable Hook

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSColorPreferences::SetupMenus $oop" \
		-identifier MENUBAR]
	Info $oop mbar $mbar

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSColorPreferences::MenuInvoke $oop"

	#
	# Color Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_COLOR
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_COLOR -label [mc Color] -underline 0 -identifier M_COLOR

	set entries {}

	set i 1
	foreach {hook label} $Hook {
		lappend entries [list -type radiobutton -label [mc $label] \
			-variable NSColorPreferences($oop,hook) -value $hook \
			-accelerator $i -identifier E_HOOK_$i]
		bind $win <KeyPress-$i> "NSColorPreferences::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_COLOR $entries


	return
}

# NSColorPreferences::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::SetupMenus {oop mbarID} {

	variable Hook

	set i 0
	foreach {hook label} $Hook {
		lappend identList E_HOOK_[incr i]
	}

	lappend identList E_CLOSE

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSColorPreferences::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::MenuInvoke {oop menuId ident} {

	variable Hook

	switch -glob -- $ident {
		E_HOOK_* {
			scan $ident "E_HOOK_%d" hookNum
			SetHook $oop [lindex $Hook [expr {($hookNum - 1) * 2}]]
		}
		E_CLOSE {Close $oop}
	}

	return
}

# NSColorPreferences::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::DisplayCmd {oop message first args} {

	variable Hook

	switch -- $message {
		preDisplay {
			if {[llength $args]} {
				set hook hook_[lindex $args 0]
				if {$hook ne [Info $oop hook]} {
					SetHook $oop $hook
				}
			} elseif {$first} {
				SetHook $oop [lindex $Hook 0]
			}
		}
		postDisplay {
		}
	}

	return
}

# NSColorPreferences::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Close {oop} {

	NSWindowManager::Undisplay color

	return
}

# NSColorPreferences::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::CallHook {oop message args} {

	return [uplevel #0 NSColorPreferences::[Info $oop hook] $oop $message $args]
}

# NSColorPreferences::SetHook --
#
#	Deletes all the canvas items for the current hook, then calls
#	the new hook with the "init" message to add items to the display.
#	The scrollbar is added if needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::SetHook {oop hook} {

	variable Hook

	set canvas [Info $oop canvas]
	set win [Info $oop win]

	# Delete old items
	$canvas delete all

	# Notice the hook must be in our namespace
	Info $oop hook $hook

	# Call the hook
	CallHook $oop init

	# Center all the items
	CenterAll $oop

	# Set the scrollregion to encompass all items.
	set bbox [$canvas bbox all]
	set width [lindex $bbox 2]
	set height [lindex $bbox 3]

	# Add or remove the scrollbar as required
	if {$height > [winfo height $canvas]} {
		place $win.frameCanvas.yscroll -y 0 -relx 1.0 -anchor ne \
			-relheight 1.0
		incr height 10
	} else {
		place forget $win.frameCanvas.yscroll
	}

	$canvas configure -scrollregion [list 0 0 $width $height]

	# No active item
	SetActiveItem $oop -1

	# Seems to need to be done after update
	$canvas yview moveto 0.0
	$win.frameCanvas.yscroll set {*}[$canvas yview]

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [expr {[lsearch -exact $Hook $hook] / 2}]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSColorPreferences::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::InvokeTab {oop tabsId tabId} {

	variable Hook
	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Hook [expr {$index * 2}]]

	return
}

# NSColorPreferences::ColorChanged --
#
#	Called when the color-picker selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::ColorChanged {oop pickerId index} {

	set canvas [Info $oop canvas]
	set win [Info $oop win]
	set hook [Info $oop hook]
	set item [Info $oop activeItem]

	if {$item == -1} return

	# Display the color info
	$win.labelColor configure -text "Index: $index    [palette set $index]"

	# Set the fill color of the active item
	Colorize $canvas $item $index

	# Call hook to synchronize other color
	set tagList [$canvas gettags $item]
	CallHook $oop set_color $item \
		[GetValueFromTag part : $tagList] $index

	return
}

# NSColorPreferences::OpacityChanged --
#
#	Called when the opacity-scale value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::OpacityChanged {oop value} {

	set canvas [Info $oop canvas]
	set hook [Info $oop hook]
	set win [Info $oop win]

	# Update the opacity-value label
	set label $win.frameOpacity.label.value
	$label configure -text $value

	if {[Info $oop ignoreOpacity]} return

	set item [Info $oop activeItem]
	if {$item == -1} return

	ChangeTagValue $canvas $item opacity : $value

	# Call hook to synchronize
	set tagList [$canvas gettags $item]
	CallHook $oop set_opacity $item \
		[GetValueFromTag part : $tagList] $value

	return
}

# NSColorPreferences::Button1 --
#
#	Respond to a ButtonPress-1 event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Button1 {oop x y} {

	# Find the topmost enabled item, or -1
	set itemId [PointToItem $oop $x $y]

	# Set the active item
	SetActiveItem $oop $itemId

	return
}

# NSColorPreferences::PointToItem --
#
#	Find the topmost enabled canvas item under the given point.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::PointToItem {oop x y} {

	global NSColorPreferences

	set canvas [Info $oop canvas]

	set x [$canvas canvasx $x]
	set y [$canvas canvasy $y]

	# Get the item(s) under the point.
	set itemIdList \
		[$canvas find overlapping $x $y [expr {$x + 1}] [expr {$y + 1}]]

	# No item is under that point
	if {![llength $itemIdList]} {return -1}

	# Get the topmost enabled item
	set hitItem -1
	foreach itemId $itemIdList {
		if {[lsearch -exact [$canvas gettags $itemId] enabled] != -1} {
			set hitItem $itemId
		}
	}

	# Result
	return $hitItem
}

# NSColorPreferences::SetActiveItem --
#
#	Sets the "active" or "selected" item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::SetActiveItem {oop itemId} {

	set canvas [Info $oop canvas]
	set pickerId [Info $oop pickerId]
	set win [Info $oop win]

	# If an item was hit, it may refer to another item to use.
	# For example, if a textual label is clicked, the item it
	# describes should be selected, not the label itself.
	if {$itemId != -1} {
		set tagList [$canvas gettags $itemId]
		if {[lsearch -glob $tagList "usepart*"] != -1} {
			set part [GetValueFromTag usepart : $tagList]
			set itemId [$canvas find withtag part:$part]
		}
	}

	# The activeItem is used by callbacks below
	Info $oop activeItem $itemId

	# When I set the opacity-scale value below, it calls OpacityChanged(),
	# but since it hasn't changed, I ignore the call
	Info $oop ignoreOpacity 1

	if {$itemId == -1} {
		NSColorPicker::ShowHideCursor $pickerId 0
		$win.frameOpacity.opacity set 255
		$win.frameOpacity.opacity configure -state disabled
	} else {
		set tagList [$canvas gettags $itemId]
		set index [GetValueFromTag color : $tagList]
		NSColorPicker::ShowHideCursor $pickerId 1
		NSColorPicker::SetCursor $pickerId $index 0
		set opacity [GetValueFromTag opacity : $tagList]
		if {$opacity == -1} {
			$win.frameOpacity.opacity set 255
			$win.frameOpacity.opacity configure -state disabled
		} else {
			$win.frameOpacity.opacity configure -state normal
			$win.frameOpacity.opacity set $opacity
		}
	}

	# Ack! The scale -command is evaluated as an idle handler! So we
	# must make sure OpacityChanged() is called before we reset the
	# value of "ignoreOpacity"
	update idletasks

	Info $oop ignoreOpacity 0

	return
}

# NSColorPreferences::CenterAll --
#
#	Centers all the canvas items in the display.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::CenterAll {oop} {

	set canvas [Info $oop canvas]
	set bbox [$canvas bbox all]
	set boxWidth [expr {[lindex $bbox 2] - [lindex $bbox 0]}]
	set boxHeight [expr {[lindex $bbox 3] - [lindex $bbox 1]}]
	set canvasWidth [winfo width $canvas]
	set canvasHeight [winfo height $canvas]
	if {$boxHeight > $canvasHeight} {
		incr canvasWidth -16
		set pady 10
	} else {
		set pady 0
	}
	set left [expr {($canvasWidth - $boxWidth) / 2}]
	if {$left < 0} {set left 0}
	set top [expr {($canvasHeight - $boxHeight) / 2}]
	if {$top < 0} {set top 0}
	$canvas move all [expr {$left - [lindex $bbox 0]}] \
		[expr {$pady + $top - [lindex $bbox 1]}]

	return
}

# NSColorPreferences::Add_TagAux --
#
#	Adds tags "enabled", "part:xxx", "color:xxx", and "opacity:xxx"
#	to the given canvas item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Add_TagAux {canvas item part color opacity} {

	lappend tagList enabled
	lappend tagList part:$part
	lappend tagList color:[palette nearest $color]
	lappend tagList opacity:$opacity
	$canvas itemconfigure $item -tags $tagList 

	return
}

# NSColorPreferences::Add_Tags --
#
#	Adds tags "enabled", "part:xxx", "color:xxx", and "opacity:xxx"
#	to the each of the given canvas items.
#
# Arguments:
#	canvas					The canvas to work on
#	itemList					The list of items to add tags to.
#	args					For each item, {part color opacity}
#
# Results:
#	What happened.

proc NSColorPreferences::Add_Tags {canvas itemList args} {

	foreach item $itemList {part color opacity} $args {
		Add_TagAux $canvas $item $part $color $opacity
	}

	return
}

# NSColorPreferences::Add_Line --
#
#	Creates a line item the given fill color.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Add_Line {canvas fill width args} {

	# Docs say "miter" is default but actually "round"
	return [$canvas create line \
		{*}$args -fill $fill -width $width -joinstyle miter]

	return
}

# NSColorPreferences::Add_Bevel --
#
#	Creates a rectangle consisting of two colored lines.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Add_Bevel {canvas x1 y1 x2 y2 light dark width} {

	lappend itemId [Add_Line $canvas $light $width $x1 $y2 $x1 $y1 $x2 $y1]
	lappend itemId [Add_Line $canvas $dark $width [expr {$x1 - $width / 2}] $y2 $x2 $y2 $x2 [expr {$y1 - $width / 2}]]
	return $itemId
}

# NSColorPreferences::Add_RectFilled --
#
#	Creates a rectangle with the given fill color.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Add_RectFilled {canvas x1 y1 x2 y2 fill} {

	return [$canvas create rectangle \
		$x1 $y1 $x2 $y2 -fill $fill -outline ""]
}

# NSColorPreferences::Add_RectOutline --
#
#	Creates a rectangle with the given outline color and width.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Add_RectOutline {canvas x1 y1 x2 y2 outline width} {

	return [$canvas create rectangle \
		$x1 $y1 $x2 $y2 -outline $outline -width $width]
}

# NSColorPreferences::Add_Text --
#
#	Creates a text item with given attributes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Add_Text {canvas x y anchor text font fill} {

	return [$canvas create text \
		$x $y -anchor $anchor -fill $fill -font $font -text $text]
}

# NSColorPreferences::ChangeTagValue --
#
#	Looks for a tag that starts with "$prefix$sep" and replaces the
#	remainder of that tag with the given value.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::ChangeTagValue {canvas item prefix sep value} {

	set tagList [$canvas gettags $item]
	set oldValue [GetValueFromTag $prefix $sep $tagList]
	$canvas dtag $item $prefix$sep$oldValue
	$canvas addtag $prefix$sep$value withtag $item	

	return
}

# NSColorPreferences::Colorize --
#
#	Changes the color of the given canvas item. The "color:xxx" tag
#	of the item is updated.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPreferences::Colorize {canvas item index} {

	# Update the "color:XXX" item tag
	ChangeTagValue $canvas $item color : $index

	switch [$canvas type $item] {
		rectangle {
			set fill [$canvas itemcget $item -fill]
			if {![string length $fill]} {
				set option -outline
			} else {
				set option -fill
			}
		}
		line -
		text {set option -fill}
	}
	$canvas itemconfigure $item $option [palette set $index]

	return
}

proc NSColorPreferences::color_hook_map {oop message args} {

	set canvas [Info $oop canvas]

	switch -- $message {

		init {
			set size 28
			set width 8
			set symbols [symbol names]
			set count [llength $symbols]
			set rows [expr {($count + 1) / 2}]
			for {set i 0} {$i < $count} {incr i} {
				set col [expr {$i / $rows}]
				set row [expr {$i % $rows}]
				set left [expr {$col * 130}]
				set top [expr {$row * ($size + 4)}]
				set mapSymbol [lindex $symbols $i]
				set inner [symbol cget $mapSymbol -inner]
				set outer [symbol cget $mapSymbol -outer]

				set x1 [expr {$left + $width / 2}]
				set y1 [expr {$top + $width / 2}]
				set x2 [expr {$left + $size - $width / 2}]
				set y2 [expr {$top + $size - $width / 2}]

				set rectId [Add_RectFilled $canvas $x1 $y1 $x2 $y2 $inner]
				$canvas itemconfigure $rectId \
					-tags "MIDDLE enabled usepart:$mapSymbol"

				set itemList [Add_RectOutline $canvas $x1 $y1 $x2 $y2 \
					$outer $width]
				Add_Tags $canvas $itemList $mapSymbol $outer -1

				# Label
				set textId [Add_Text $canvas [expr {$left + $size + 2}] \
					[expr {$top + $size / 2}] w $mapSymbol "Courier 9" 255]
				$canvas itemconfigure $textId -tags "enabled usepart:$mapSymbol"
			}
		}

		set_color {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]

			if {$part eq "MIDDLE"} {
				$canvas itemconfigure MIDDLE -fill [palette set $index]
			}

			# See NSMap::InitModule
			Value map,$part $index

			# Hack -- Redraw the Micro Map.
			[Global micromap,widget] wipe
		}

		set_opacity {
		}
	}

	return
}

proc NSColorPreferences::color_hook_status {oop message args} {

	set canvas [Info $oop canvas]

	switch -- $message {

		init {
			set padx 40
			set height 40
			set width [expr {[winfo width $canvas] - $padx * 2}]
			set bevelSize 8
			set centerH [expr {$width / 2}]
			set centerV [expr {$height / 2}]
			set font "Times 20"

			# Good: background
			set fill [Value statusGoodBG]
			set opacity [Value statusGoodBG2]
			set itemList [Add_RectFilled $canvas 0 0 $width $height $fill]
			Add_Tags $canvas $itemList statusGoodBG $fill $opacity

			# Good: bevel
			set fill [Value statusGoodBL]
			set opacity [Value statusGoodBL2]
			set fill2 [Value statusGoodBD]
			set opacity2 [Value statusGoodBD2]
			set itemList [Add_Bevel $canvas 0 0 $width $height $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList statusGoodBL $fill $opacity statusGoodBD $fill2 $opacity2

			# Good: text
			set fill [Value statusGoodText]
			set opacity [Value statusGoodText2]
			set itemList [Add_Text $canvas $centerH $centerV center [mc Good] $font $fill]
			Add_Tags $canvas $itemList statusGoodText $fill $opacity

			set y1 [expr {$height + $bevelSize + 8 + $bevelSize / 2}]
			set y2 [expr {$y1 + $height}]

			# Info: background
			set fill [Value statusInfoBG]
			set opacity [Value statusInfoBG2]
			set itemList [Add_RectFilled $canvas 0 $y1 $width $y2 $fill]
			Add_Tags $canvas $itemList statusInfoBG $fill $opacity

			# Info: bevel
			set fill [Value statusInfoBL]
			set opacity [Value statusInfoBL2]
			set fill2 [Value statusInfoBD]
			set opacity2 [Value statusInfoBD2]
			set itemList [Add_Bevel $canvas 0 $y1 $width $y2 $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList statusInfoBL $fill $opacity statusInfoBD $fill2 $opacity2

			# Info: text
			set fill [Value statusInfoText]
			set opacity [Value statusInfoText2]
			set itemList [Add_Text $canvas $centerH [expr {$y1 + $centerV}] center [mc Info] $font $fill]
			Add_Tags $canvas $itemList statusInfoText $fill $opacity

			incr y1 [expr {$height + $bevelSize + 8 + $bevelSize / 2}]
			set y2 [expr {$y1 + $height}]

			# Bad: background
			set fill [Value statusBadBG]
			set opacity [Value statusBadBG2]
			set itemList [Add_RectFilled $canvas 0 $y1 $width $y2 $fill]
			Add_Tags $canvas $itemList statusBadBG $fill $opacity

			# Bad: bevel
			set fill [Value statusBadBL]
			set opacity [Value statusBadBL2]
			set fill2 [Value statusBadBD]
			set opacity2 [Value statusBadBD2]
			set itemList [Add_Bevel $canvas 0 $y1 $width $y2 $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList statusBadBL $fill $opacity statusBadBD $fill2 $opacity2

			# Bad: text
			set fill [Value statusBadText]
			set opacity [Value statusBadText2]
			set itemList [Add_Text $canvas $centerH [expr {$y1 + $centerV}] center [mc Bad] $font $fill]
			Add_Tags $canvas $itemList statusBadText $fill $opacity
		}

		set_color {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value $part [palette set $index]
		}

		set_opacity {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value ${part}2 $index
		}
	}

	return
}

proc NSColorPreferences::color_hook_monster_bar {oop message args} {

	set canvas [Info $oop canvas]

	switch -- $message {

		init {
			set padx 40
			set height 30
			set width [expr {[winfo width $canvas] - $padx * 2}]
			set bevelSize 8
			set centerH [expr {$width / 2}]
			set centerV [expr {$height / 2}]

			# Name background
			set fill [Value healthNameBG]
			set opacity [Value healthNameBG2]
			set itemList [Add_RectFilled $canvas 0 0 $width $height $fill]
			Add_Tags $canvas $itemList healthNameBG $fill $opacity

			# Name text
			set fill [Value healthNameText]
			set opacity [Value healthNameText2]
			set itemList [Add_Text $canvas $centerH $centerV center [mc "Monster Name"] "Times 18" $fill]
			Add_Tags $canvas $itemList healthNameText $fill $opacity

			# Name bevel
			set fill [Value healthNameBL]
			set opacity [Value healthNameBL2]
			set fill2 [Value healthNameBD]
			set opacity2 [Value healthNameBD2]
			set itemList [Add_Bevel $canvas 0 0 $width $height $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList healthNameBL $fill $opacity healthNameBD $fill2 $opacity2

			set y1 [expr {$height + $bevelSize + 8 + $bevelSize / 2}]
			set y2 [expr {$y1 + $height}]

			# Bar done
			set fill [Value healthBarDone]
			set opacity [Value healthBarDone2]
			set itemList [Add_RectFilled $canvas 0 $y1 $centerH $y2 $fill]
			Add_Tags $canvas $itemList healthBarDone $fill $opacity

			# Bar todo
			set fill [Value healthBarToDo]
			set opacity [Value healthBarToDo2]
			set itemList [Add_RectFilled $canvas $centerH $y1 $width $y2 $fill]
			Add_Tags $canvas $itemList healthBarToDo $fill $opacity

			# Bar bevel
			set fill [Value healthBarBL]
			set opacity [Value healthBarBL2]
			set fill2 [Value healthBarBD]
			set opacity2 [Value healthBarBD2]
			set itemList [Add_Bevel $canvas 0 $y1 $width $y2 $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList healthBarBL $fill $opacity healthBarBD $fill2 $opacity2

			if {[variant KANGBANDTK ZANGBANDTK]} {

				# Friendly monster colors
				incr y1 [expr {$height + $bevelSize + 8 + $bevelSize / 2}]
				incr y2 [expr {$height + $bevelSize + 8 + $bevelSize / 2}]

				# Bar done (friend)
				set fill [Value friendBarDone]
				set opacity [Value friendBarDone2]
				set itemList [Add_RectFilled $canvas 0 $y1 $centerH $y2 $fill]
				Add_Tags $canvas $itemList friendBarDone $fill $opacity

				# Bar todo (friend)
				set fill [Value friendBarToDo]
				set opacity [Value friendBarToDo2]
				set itemList [Add_RectFilled $canvas $centerH $y1 $width $y2 $fill]
				Add_Tags $canvas $itemList friendBarToDo $fill $opacity

				# Bar bevel (friend)
				set fill [Value friendBarBL]
				set opacity [Value friendBarBL2]
				set fill2 [Value friendBarBD]
				set opacity2 [Value friendBarBD2]
				set itemList [Add_Bevel $canvas 0 $y1 $width $y2 $fill $fill2 $bevelSize]
				Add_Tags $canvas $itemList friendBarBL $fill $opacity friendBarBD $fill2 $opacity2
			}
		}

		set_color {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value $part [palette set $index]
		}

		set_opacity {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value ${part}2 $index
		}
	}

	return
}

proc NSColorPreferences::color_hook_list {oop message args} {

	global Angband

	set canvas [Info $oop canvas]

	switch -- $message {

		init {

			set y 18
			set y_delta 50

			if {[Platform unix]} {
				set font {Helvetica 12}
			}
			if {[Platform windows]} {
				set font {Courier 9}
			}

			# List background label & rect
			set color [Value listBG]
			set itemList [Add_Text $canvas 10 $y sw [mc "List Background"] \
				$font [palette set 255]]
			set itemList [Add_RectFilled $canvas 10 [expr {$y + 2}] 280 [expr {$y + 28}] $color]
			Add_Tags $canvas $itemList listBG $color -1

			# List highlight label & rect
			set color [Value listHilite]
			set itemList [Add_Text $canvas 10 [incr y $y_delta] sw [mc "List Highlight (Active)"] \
				$font [palette set 255]]
			set itemList [Add_RectFilled $canvas 10 [expr {$y + 2}] 280 [expr {$y + 28}] $color]
			Add_Tags $canvas $itemList listHilite $color -1

			# List highlight(inactive) label & rect
			set color [Value listInactive]
			set itemList [Add_Text $canvas 10 [incr y $y_delta] sw [mc "List Highlight (Inactive)"] \
				$font [palette set 255]]
			set itemList [Add_RectFilled $canvas 10 [expr {$y + 2}] 280 [expr {$y + 28}] $color]
			Add_Tags $canvas $itemList listInactive $color -1

			# Inventory colors label
			set itemList [Add_Text $canvas 10 [incr y $y_delta] sw [mc "Inventory Colors"] \
				$font [palette set 255]]

			set i 0
			set height 28
			set rows [expr {([llength $Angband(tval)] + 1) / 2}]
			foreach tval $Angband(tval) {
				scan $tval TV_%s text
				set text [string totitle [split $text _]]
				set col [expr {$i / $rows}]
				set row [expr {$i % $rows}]
				set top [expr {$row * $height + ($y + 2)}]
				set x1 [expr {$col * 140 + 10}]
				set y1 [expr {$top + 2}]
				set x2 [expr {$x1 + 130}]
				set y2 [expr {$top + $height - 2}]
				set color [Value $tval]
				set rectId [Add_RectFilled $canvas $x1 $y1 $x2 $y2 [palette set 255]]
				set itemList [Add_Text $canvas [expr {$x1 + ($x2 - $x1) / 2}] \
					[expr {$top + ($y2 - $y1) / 2}] center $text $font \
					$color]
				Add_Tags $canvas $itemList $tval $color -1
				incr i
			}
		}

		set_color {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value $part [palette set $index]
		}

		set_opacity {
		}
	}

	return
}

proc NSColorPreferences::hook_target {oop message args} {

	set canvas [Info $oop canvas]

	switch -- $message {

		init {
			set width 60
			set height 60
			set bevelSize 8
			set centerH [expr {$width / 2}]
			set centerV [expr {$height / 2}]

			# Background (los)
			set fill [Value targetBG]
			set opacity [Value targetBG2]
			set itemList [Add_RectFilled $canvas 0 0 $width $height $fill]
			Add_Tags $canvas $itemList targetBG $fill $opacity

			# Text (los)
			set fill [Value targetText]
			set opacity [Value targetText2]
			set itemList [Add_Text $canvas $centerH $centerV center "T" {Times 24} $fill]
			Add_Tags $canvas $itemList targetText $fill $opacity

			# Bevel (los)
			set fill [Value targetBL]
			set opacity [Value targetBL2]
			set fill2 [Value targetBD]
			set opacity2 [Value targetBD2]
			set itemList [Add_Bevel $canvas 0 0 $width $height $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList targetBL $fill $opacity targetBD $fill2 $opacity2

			set y1 [expr {$height + $bevelSize + 8 + $bevelSize / 2}]
			set centerV [expr {$y1 + $height / 2}]
			set y2 [expr {$y1 + $height}]

			# Background
			set fill [Value target2BG]
			set opacity [Value target2BG2]
			set itemList [Add_RectFilled $canvas 0 $y1 $width $y2 $fill]
			Add_Tags $canvas $itemList target2BG $fill $opacity

			# Text
			set fill [Value target2Text]
			set opacity [Value target2Text2]
			set itemList [Add_Text $canvas $centerH $centerV center "T" {Times 24} $fill]
			Add_Tags $canvas $itemList target2Text $fill $opacity

			# Bevel
			set fill [Value target2BL]
			set opacity [Value target2BL2]
			set fill2 [Value target2BD]
			set opacity2 [Value target2BD2]
			set itemList [Add_Bevel $canvas 0 $y1 $width $y2 $fill $fill2 $bevelSize]
			Add_Tags $canvas $itemList target2BL $fill $opacity target2BD $fill2 $opacity2
		}

		set_color {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value $part [palette set $index]
		}

		set_opacity {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value ${part}2 $index
		}
	}

	return
}

proc NSColorPreferences::hook_icons {oop message args} {

	set canvas [Info $oop canvas]

	switch -- $message {

		init {
			set font {Times 18}
			set bevelSize 8
			set width 200
			set height [expr {[font metrics $font -linespace] + $bevelSize * 2}]
			set centerH [expr {$width / 2}]

			set row 0
			foreach thing {character object monster uniques friend feature feature1 feature2} {

				set y1 [expr {$row * ($height + $bevelSize + 8) + 8 + $bevelSize / 2}]
				set centerV [expr {$y1 + $height / 2}]
				set y2 [expr {$y1 + $height}]

				# Background
				set fill [palette set [Value ascii,$thing,bg]]
				set opacity 255
				set itemList [Add_RectFilled $canvas 0 $y1 $width $y2 $fill]
				Add_Tags $canvas $itemList ascii,$thing,bg $fill $opacity

				# Text
				set fill White
				set opacity 255
				set itemList [Add_Text $canvas $centerH $centerV center $thing $font $fill]
#				Add_Tags $canvas $itemList targetText $fill $opacity
if 0 {
				# Bevel
				set fill [palette set [Value ascii,$thing,bg]]
				set opacity 255
				set fill2 [palette set [Value ascii,$thing,bg]]
				set opacity2 255
				set itemList [Add_Bevel $canvas 0 $y1 $width $y2 $fill $fill2 $bevelSize]
#			 Add_Tags $canvas $itemList targetBL $fill $opacity targetBD $fill2 $opacity2
}
				incr row
			}
		}

		set_color {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value $part $index
		}

		set_opacity {
			set item [lindex $args 0]
			set part [lindex $args 1]
			set index [lindex $args 2]
			Value ${part}2 $index
		}
	}

	return
}

# GetValueFromTag --
#
#	Looks through a list of strings for one of the form
#	"$prefix$sep$value" and returns $value.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc GetValueFromTag {prefix sep tagList} {

	set index [lsearch -glob $tagList $prefix$sep*]
	if {$index == -1} {
		error "GetValueFromTag: no matching tags"
	}
	set tag [lindex $tagList $index]
	return [lindex [split $tag $sep] 1]
}

proc FindNearestColor {rgb} {

	set rgb [string range $rgb 1 end]
	set r 0x[string range $rgb 0 1]
	set g 0x[string range $rgb 2 3]
	set b 0x[string range $rgb 4 5]

	set max 195076
	for {set i 0} {$i < 256} {incr i} {
		set rgb2 [string range [palette set $i] 1 end]
		set r2 0x[string range $rgb2 0 1]
		set g2 0x[string range $rgb2 2 3]
		set b2 0x[string range $rgb2 4 5]
		set diff [expr {$r - $r2}]
		set sum [expr {$diff * $diff}]
		set diff [expr {$g - $g2}]
		incr sum [expr {$diff * $diff}]
		set diff [expr {$b - $b2}]
		incr sum [expr {$diff * $diff}]
		if {$sum < $max} {
			set index $i
			set max $sum
		}
	}

	return $index
}
