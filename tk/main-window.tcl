# File: main-window.tcl

# Purpose: the Main Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# This global flag indicates that some part of the UI is busy doing
# stuff with possible [update] calls and the user shouldn't be entering
# new commands during that time.
Global UI_Busy 0

namespace eval NSMainWindow {

	variable Priv
	variable Progress

	variable tracking 0
	variable trackId ""
	variable trackId2 ""
	variable trackStepping 0
	variable trackStuck 0
	variable trackX
	variable trackY
	variable trackHPfrac
	variable trackPrev 0

# namespace eval NSMainWindow
}

# NSMainWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#
# Results:
#	What happened.

proc NSMainWindow::InitModule {} {

	global Display
	global PYPX

	MsgCatInit main

	NSModule::LoadIfNeeded NSBalloon
	NSModule::LoadIfNeeded NSMap
	NSModule::LoadIfNeeded NSMiscCanvas
	NSModule::LoadIfNeeded NSProgressCanvas
	NSModule::LoadIfNeeded NSWidget

	# The character's position
	set PYPX "0 0"

	# Keep track of active window (inventory, book, etc)
	set Display(window) none

	# Create the main window
	NSObject::New NSMainWindow

	return
}

# NSMainWindow::NSMainWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::NSMainWindow {oop} {

	InitWindow $oop

	HPBlinker_Init $oop

	# use_sound
	angband setting set use_sound [Value use_sound]
	Info $oop useSound [Value use_sound]

	# The Global[] variable is used as a checkbutton variable
	Global music,play [Value music,play]
if 0 {
	# Set ANGBAND_DIR_SOUND
	set path [eval file join [Value sound,directory]]
	if {[file exists $path] && [file isdirectory $path]} {
		angband game directory ANGBAND_DIR_SOUND $path
	}
}
	Setting ambient_delay [Value ambient_delay]
	Setting monster_delay [Value monster_delay]

	# Window positions
	Info $oop window,autosave [Value window,autosave]

	#
	# Global access
	#

	Window main [Info $oop win]
	Global main,oop $oop

	InitAutobar $oop
	BalloonInit $oop

	return
}

# NSMainWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::Info {oop info args} {

	global NSMainWindow

	# Verify the object
	NSObject::CheckObject NSMainWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMainWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMainWindow($oop,$info)
			}
		}
	}

	return
}

# NSMainWindow::InitWindow --
#
#	Creates the Main Window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::InitWindow {oop} {

	global Angband

	set win .main$oop
	toplevel $win
	wm title $win "[mc Main] - $Angband(name) $Angband(vers)"

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMainWindow::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Remember the window
	Info $oop win $win

	# Create the menus
	InitMenus $oop

	set frame $win.divider2
	MakeDivider $frame x

	#
	# Statusbar
	# There is a level of tomfoolery with the statusbar to prevent
	# a really long message causing the Main Window to change size.
	# This is in spite of the fact that many other windows do not
	# change size with long statusbar labels. I thought gridded
	# geometry solved the problem, but not in this case...
	#
	# The hack involves pack'ing the label in a frame, and turning
	# off pack propagation for that frame. Oh well.
	#

	# Font for all statusbars
	set font [Value font,statusBar]

	frame $win.statusBar \
		-borderwidth 0
	frame $win.statusBar.frameLabel \
		-borderwidth 0
	label $win.statusBar.frameLabel.label \
		-anchor w -text "Hello world!" -relief sunken -padx 2 \
		-foreground [Value main,statusbar,color] -background Black -font $font
	label $win.statusBar.center \
		-text "C" -relief sunken -width 2 -padx 0 -foreground White \
		-background Black -font $font
	label $win.statusBar.depth \
		-relief sunken -width 12 -padx 2 \
		-foreground White -background Black -font $font

	bind $win.statusBar.frameLabel.label <ButtonPress-3> \
		"NSMainWindow::ContextMenu_StatusBar $win.context %X %Y"

	# Used in various places
	Global main,statusBar $win.statusBar.frameLabel.label

	# Hack
	pack $win.statusBar.frameLabel.label -fill x
	pack propagate $win.statusBar.frameLabel no

	grid columnconfigure $win.statusBar 0 -weight 1
	grid columnconfigure $win.statusBar 1 -weight 0
	grid columnconfigure $win.statusBar 2 -weight 0
	grid rowconfigure $win.statusBar 0 -weight 0

	grid $win.statusBar.frameLabel \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar.center \
		-row 0 -column 1 -rowspan 1 -columnspan 1
	grid $win.statusBar.depth \
		-row 0 -column 2 -rowspan 1 -columnspan 1

	# The small "C" in the status bar can be clicked to recenter the
	# Main widget and Micro Map widget on the character's location.
	bind $win.statusBar.center <ButtonPress-1> {
		Global main,widget,center [WidgetCenter [Global main,widget]]
		WidgetCenter [Global micromap,widget]
	}
	bind $win.statusBar.center <Enter> "
		%W configure -foreground gray60
		NSMainWindow::StatusText $oop \"[mc status-center]\"
	"
	bind $win.statusBar.center <Leave> "
		%W configure -foreground White
		NSMainWindow::StatusText $oop {}
	"

	# When the depth display in the Main Window statusbar is clicked,
	# we toggle the "depth_in_feet" option.
	bind $win.statusBar.depth <ButtonPress-1> {
		angband setting set depth_in_feet [expr {![angband setting set depth_in_feet]}]
	}
	bind $win.statusBar.depth <Enter> "
		%W configure -foreground gray60
		NSMainWindow::StatusText $oop \"[mc status-depth]\"
	"
	bind $win.statusBar.depth <Leave> "
		%W configure -foreground White
		NSMainWindow::StatusText $oop {}
	"

	# When the dungeon level changes, update the depth display
	qebind $win.statusBar.depth <Py-depth> {
		NSMainWindow::DisplayDepth %W %c
	}

	# When the "depth_in_feet" option changes, update the depth display
	qebind $win.statusBar.depth <Setting-depth_in_feet> {
		NSMainWindow::DisplayDepth %W [angband player depth]
	}
	if {[variant OANGBANDTK]} {
		qebind $win.statusBar.depth <Setting-use_metric> {
			NSMainWindow::DisplayDepth %W [angband player depth]
		}
	}

	# Update ourself when the font,statusBar value changes
	qebind NSMainWindow <Value-font,statusBar> \
		"NSMainWindow::ValueChanged_font_statusBar"

	#
	# Message line when Message Window is closed
	#

	set frame $win.message
	frame $frame -background black -borderwidth 1 -relief sunken

	#
	# Frame for Misc/Progress info when Misc/Progress Windows are closed
	#

	set frame $win.misc
	frame $frame -background black -borderwidth 1 -relief sunken

	#
	# Progress info when Progress Window is closed
	#

	set progressId [NSObject::New NSProgressCanvas -parent $win.misc -layout tall]
	Info $oop progress,oop $progressId
	Info $oop progress,canvas [NSProgressCanvas::Info $progressId canvas]

	#
	# Misc info when Misc Window is closed
	#

	set miscId [NSObject::New NSMiscCanvas -parent $win.misc -layout tall \
		-toolbar no -progress $progressId]
	Info $oop misc,oop $miscId
	Info $oop misc,frame [NSMiscCanvas::Info $miscId frame]
	Info $oop misc,canvas [NSMiscCanvas::Info $miscId canvas]
	pack [Info $oop misc,frame] -side top

	#
	# Main widget
	#

	# Black background affects border color
	frame $win.mainframe \
		-borderwidth 1 -relief sunken -background Black

	# Get the icon dimensions
	set gwidth [icon width]
	set gheight [icon height]

	# This is a large monitor
	if {[ScreenWidth] >= 800} {
		set width [expr {15 * 32}]
		set height [expr {11 * 32}]

	# This is a small monitor
	} else {
		set width [expr {13 * 32}]
		set height [expr {9 * 32}]
	}

	set widgetId [NSObject::New NSWidget $win.mainframe \
		$width $height $gwidth $gheight]
	NSWidget::Info $widgetId examineCmd "NSMainWindow::ExamineLocation $oop mouse"
	NSWidget::Info $widgetId leaveCmd "NSMainWindow::Leave $oop"
	NSWidget::Info $widgetId centerCmd "NSMainWindow::WidgetCenter $oop"
	set widget [NSWidget::Info $widgetId widget]

	bind $widget <ButtonPress-1> "NSMainWindow::TrackPress $oop %x %y"
	bind $widget <Button1-Motion> "NSMainWindow::TrackMotion $oop %x %y"
	bind $widget <ButtonRelease-1> "NSMainWindow::TrackRelease $oop"

	bind $widget <Control-ButtonPress-1> "NSMainWindow::MouseCommand $oop %x %y +"
	bind $widget <Shift-ButtonPress-1> "NSMainWindow::MouseCommand $oop %x %y ."

	bind $win <Control-KeyPress-Shift_L> \
		"$widget configure -cursor fleur"
	bind $win <Control-KeyRelease-Shift_L> \
		"$widget configure -cursor {}"
	bind $win <Shift-KeyPress-Control_L> \
		"$widget configure -cursor fleur"
	bind $win <Shift-KeyRelease-Control_L> \
		"$widget configure -cursor {}"
	bind $widget <Control-Shift-ButtonPress-1> "
		NSWidget::Info $widgetId track,x %x
		NSWidget::Info $widgetId track,y %y
		NSWidget::Info $widgetId track,mouseMoved 0
	"
	bind $widget <Control-Shift-Button1-Motion> \
		"NSWidget::TrackOnce $widgetId %x %y"

	# Reset the cursor if showing the console
	bind $win <Control-KeyPress-C> \
		"$widget configure -cursor {}"

	bind $widget <ButtonPress-3> \
		"NSMainWindow::ButtonPress3 $oop %x %y %X %Y"
	bind $widget <Control-ButtonPress-3> \
		"NSRecall::PopupSelect_Use $win.context %X %Y"

	# When the pointer leaves the Main Window Widget, we clear the
	# statusbar text, in addition to the behaviour defined by the
	# NSWidget module.
#	bind $widget <Leave> "+NSMainWindow::StatusText $oop {}"

	# Do something when the character position changes.
	qebind $widget <Position> {NSMainWindow::PositionChanged %W %y %x}

	# Describe what is seen when grids are highlighted.
	# Except for the 'stay' command <Cursor-show> is always followed by
	# <Track-grid>.  So call ExamineLocation when the cursor is shown only.
	qebind $widget <Track-grid> {
		if {![Global cursor,requested]} {
			NSMainWindow::Info [Global main,oop] trackGrid,y %y
			NSMainWindow::Info [Global main,oop] trackGrid,x %x
			qeconfigure TrackGrid <Term-fresh> -active yes
		}
	}
	qebind TrackGrid <Term-fresh> "NSMainWindow::TrackGrid $oop"
	qeconfigure TrackGrid <Term-fresh> -active no

	# Remember the center of the Main Window Widget.
#	Global main,widget,center [angband player position]

	# When the dungeon level changes, recenter the Main Window Widget
	# on the character's location.
	qebind $widget <Dungeon-enter> {
		Global main,widget,center [WidgetCenter %W]
	}

	# When the player level changes, display big status message
	Info $oop Py_level [angband player level]
	qebind NSMainWindow <Py-level> \
		"NSMainWindow::Bind_Py_level $oop %c"

	# When the game begins waiting for a keypress, check if we are
	# tracking the mouse.
	qebind $widget <Inkey> "NSMainWindow::TrackOnce $oop"

	# Update the monster health bar when needed
	qebind $widget <Track-health> "NSMainWindow::UpdateHealthWho $oop %w %f"

# Debug: draw 1 spot
proc ::wipespot {y x} {
	scan [[Global main,widget] caveyx $x $y] "%d %d" y x
	[Global main,widget] wipespot $y $x
}
bind $widget <Shift-ButtonPress-3> "wipespot %y %x"

if {0} {
	set isoview $win.mainframe.isoview
	widget $isoview -width $width -height $height -style iso

	bind $isoview <ButtonPress-1> "NSMainWindow::TrackPress $oop %x %y"
	bind $isoview <Button1-Motion> "NSMainWindow::TrackMotion $oop %x %y"
	bind $isoview <ButtonRelease-1> "NSMainWindow::TrackRelease $oop"

	bind $isoview <Control-ButtonPress-1> "NSMainWindow::MouseCommand $oop %x %y +"
	bind $isoview <Shift-ButtonPress-1> "NSMainWindow::MouseCommand $oop %x %y ."

	bind $isoview <ButtonPress-3> \
		"NSMainWindow::ButtonPress3 $oop %x %y %X %Y"
	bind $isoview <Control-ButtonPress-3> \
		"NSRecall::PopupSelect_Use $win.context %X %Y"

	qebind $isoview <Position> "%W center %y %x ; set PYPX {%y %x}"
	bind $isoview <Configure> "%W configure -width %w -height %h ; %W wipe"
	qebind $isoview <Dungeon-enter> {
		%W center $PYPX
	}

	Global main,isoview $isoview
}

proc hittest {w x y} {
	variable HT
	set ht [$w hittest $x $y]
	if {$ht ne $HT} {
		if {[scan $ht "%d %d %d" hity hitx layer] == 3} {
			NSMainWindow::ExamineLocation [Global main,oop] mouse [Global main,widgetId] $hity $hitx
			if {0 && ![Global cursor,visible]} {
				$w configure -hit $layer -hitx $hitx -hity $hity
				$w wipe
			}
		}
		set HT $ht
	}
}
	variable HT ""
#	bind $widget <Motion> {+NSMainWindow::hittest %W %x %y}

	# The "big map", the map of the entire cave with scroll bars.
	# The user can change the scale via a popup menu, so we save
	# the desired scale.
	set scale [Value bigmap,scale]
	set width [expr {[$widget cget -width] - 16}]
	set height [expr {[$widget cget -height] - 16}]
	set mapId [NSObject::New NSMap $widget $width $height $scale $scale]
	set widget2 [NSMap::Info $mapId widget]

	# Visual indication of player/stair location
	$widget2 configure -blink yes

	NSMap::Info $mapId scaleCmd \
		"Value bigmap,scale \[NSWidget::Info [NSMap::Info $mapId widgetId] scale]"

	NSWidget::Info [NSMap::Info $mapId widgetId] examineCmd \
		"NSMainWindow::BigMapExamine $oop"

	NSWidget::Info [NSMap::Info $mapId widgetId] leaveCmd \
		"NSMainWindow::BigMapLeave $oop"

	# Hide the Big Map when clicked (but not dragged)
	bind $widget2 <ButtonRelease-1> {
		if {![NSWidget::Info [Global bigmap,widgetId] track,mouseMoved]} {
			angband keypress \033
		}
	}

	# Each NSMap widget has Left/Right etc bindings. Need this to
	# hide the map.
	bind $widget2 <KeyPress-Escape> {
		angband keypress \033
	}

	# Global access
	Global main,widgetId $widgetId
	Global main,widget $widget
	Global bigmap,mapId $mapId
	Global bigmap,widgetId [NSMap::Info $mapId widgetId]
	Global bigmap,widget [NSMap::Info $mapId widget]

	# This binding is called whenever the Main Window is resized
	# by the user.
	bind $widget <Configure> \
		"NSMainWindow::Configure $oop %w %h"

	pack $widget -expand yes -fill both

if {0} {
	qeunbind $widget <Position>
	$widget configure -noupdate yes
	grid forget $widget
	pack $isoview -expand yes -fill both
}

	#
	# Geometry
	#

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 0
	grid rowconfigure $win 2 -weight 1
	grid rowconfigure $win 3 -weight 0
	grid columnconfigure $win 0 -weight 0
	grid columnconfigure $win 1 -weight 1

	grid $win.divider2 \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.message \
		-row 1 -column 0 -columnspan 2 -sticky we
	grid $win.misc \
		-row 2 -column 0 -sticky ns
	grid $win.mainframe \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	#
	# Context menu
	#

	menu $win.context -tearoff 0

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win

	# Hack -- Visual feedback of whether the target is set or not.
	TargetSetup $oop

	# The monster health bar
	ProgressSetup $widget

if {0 && $::DEBUG} {
	# This creates a text item that displays the character's current
	# location as "y:YYY x:XXX"
	set itemId [$widget create text -x 0 \
		-y [expr {[$widget cget -height] * [$widget cget -gheight] - 1}] \
		-anchor sw -clipx yes -width 100 -height 16 -bevel yes \
		-fill [Value statusText] \
		-fill2 [Value statusText2] \
		-background [Value statusBG] \
		-background2 [Value statusBG2] \
		-bevellight [Value statusBL] \
		-bevellight2 [Value statusBL2] \
		-beveldark [Value statusBD] \
		-beveldark2 [Value statusBD2] \
		-visible yes]
	qebind $widget <Position> "+
		$widget itemconfigure $itemId -text {y:%y x:%x}
	"
}

if {0 && $::DEBUG} {
	set height 18

	# This creates a text item that displays the character's hitpoints
	set itemId [$widget create text -x 4 \
		-y [expr {[$widget cget -height] * [$widget cget -gheight] - 5 - $height * 2}] \
		-anchor sw -clipx yes -width 48 -height $height -bevel no \
		-font {Courier 10} \
		-fill [Value statusText] \
		-fill2 [Value statusText2] \
		-background 35 \
		-background2 [Value statusBG2] \
		-bevellight [Value statusBL] \
		-bevellight2 [Value statusBL2] \
		-beveldark [Value statusBD] \
		-beveldark2 [Value statusBD2] \
		-visible yes]
	qebind $widget <Py-hitpoints> "NSMainWindow::DisplayPoints $oop $itemId %c"

	# This creates a text item that displays the character's mana
	set itemId [$widget create text -x 4 \
		-y [expr {[$widget cget -height] * [$widget cget -gheight] - 5 - $height}] \
		-anchor sw -clipx yes -width 48 -height $height -bevel no \
		-font {Courier 10} \
		-fill [Value statusText] \
		-fill2 [Value statusText2] \
		-background 235 \
		-background2 [Value statusBG2] \
		-bevellight [Value statusBL] \
		-bevellight2 [Value statusBL2] \
		-beveldark [Value statusBD] \
		-beveldark2 [Value statusBD2] \
		-visible yes]
	qebind $widget <Py-mana> "NSMainWindow::DisplayPoints $oop $itemId %c"
}

	return
}

proc NSMainWindow::InitAutobar {oop} {

	set statusBar [Global main,statusBar]

	bind $statusBar <Enter> \
		"NSMainWindow::ShowAutobar $oop"

	bind NSAutobar <Motion> \
		"NSMainWindow::AutobarMotion $oop %x %y"
	bind NSAutobar <Leave> \
		"NSMainWindow::AutobarLeave $oop"
	set widget [Global main,widget]
	bindtags $widget [concat [bindtags $widget] NSAutobar]

	# Set to 1 when the mouse enters the bottom of the main widget
	Info $oop autobar,inside 0

	return
}

proc NSMainWindow::ShowAutobar {oop} {

	# Allow easy rebooting of the module
	if {[NSModule::LoadIfNeeded NSAutobar]} {

		set autobarId [Global autobar,oop]
		set statusBar [Global main,statusBar]

		bind $statusBar <Leave> \
			"NSAutobar::Event $autobarId leave-status"
	}

	set autobarId [Global autobar,oop]
	NSAutobar::Event $autobarId enter-status

	return
}

proc NSMainWindow::AutobarMotion {oop x y} {

	set widget [Global main,widget]
	if {$y > [winfo height $widget] - 32} {
		BalloonHide $oop
		ShowAutobar $oop
		Info $oop autobar,inside 1
	} elseif {[Info $oop autobar,inside] && ($y <= [winfo height $widget] - 32)} {
		Info $oop autobar,inside 0
		set autobarId [Global autobar,oop]
		NSAutobar::Event $autobarId leave-status
	}

	return
}

proc NSMainWindow::AutobarLeave {oop} {

	if {[Info $oop autobar,inside]} {
		Info $oop autobar,inside 0
		set autobarId [Global autobar,oop]
		NSAutobar::Event $autobarId leave-status
	}

	return
}

# NSMainWindow::InitMenus --
#
#	Initialize the menus for the Main Window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::InitMenus {oop} {

	global Angband

	set win [Info $oop win]

	set mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSMainWindow::SetupMenus $oop" -identifier MENUBAR]

	# Call our command when an entry is invoked
	NSMenu::Info $mbarId invokeCmd "NSMainWindow::MenuInvoke $oop"

	Info $oop mbarId $mbarId

	#
	# File Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_FILE
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_FILE -label [mc File] -underline 0 -identifier M_FILE

	set entries {}
	lappend entries [list -type command -label [mc Save] -identifier E_GAME_SAVE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Quit With Save"] -identifier E_GAME_EXIT]
	lappend entries [list -type command -label [mc Quit] -identifier E_GAME_ABORT]

	NSMenu::MenuInsertEntries $mbarId -end MENU_FILE $entries

	#
	# Inven Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_INVEN]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_INVEN -label [mc Inven] -underline 0 -identifier M_INVEN

	# Magic Menu
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_MAGIC]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc Activate] -identifier E_MAGIC_ACTIVATE]
	lappend entries [list -type command -label [mc "Aim Wand"] -identifier E_MAGIC_WAND]
	lappend entries [list -type command -label [mc "Drink Potion"] -identifier E_MAGIC_POTION]
	lappend entries [list -type command -label [mc "Read Scroll"] -identifier E_MAGIC_SCROLL]
	lappend entries [list -type command -label [mc "Use Staff"] -identifier E_MAGIC_STAFF]
	lappend entries [list -type command -label [mc "Zap Rod"] -identifier E_MAGIC_ROD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Browse] -identifier E_MAGIC_BROWSE]
	lappend entries [list -type command -label [mc Study] -identifier E_MAGIC_STUDY]

	NSMenu::MenuInsertEntries $mbarId -end MENU_MAGIC $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_USE]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc Destroy] -identifier E_USE_DESTROY]
	lappend entries [list -type command -label [mc Drop] -identifier E_USE_DROP]
	lappend entries [list -type command -label [mc "Pick Up"] -identifier E_USE_PICKUP]
	lappend entries [list -type command -label [mc "Take Off"] -identifier E_USE_TAKEOFF]
	lappend entries [list -type command -label [mc Wear/Wield] -identifier E_USE_WIELD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Eat Food"] -identifier E_USE_FOOD]
	lappend entries [list -type command -label [mc "Fire Missile"] -identifier E_USE_MISSILE]
	lappend entries [list -type command -label [mc "Fuel Light"] -identifier E_USE_FUEL]
	lappend entries [list -type command -label [mc "Jam Spike"] -identifier E_USE_SPIKE]
	lappend entries [list -type command -label [mc Throw] -identifier E_USE_THROW]

	NSMenu::MenuInsertEntries $mbarId -end MENU_USE $entries

	set entries {}
	lappend entries [list -type command -label [mc Equipment] -identifier E_INVEN_EQUIPMENT]
	lappend entries [list -type command -label [mc Inventory] -identifier E_INVEN_INVENTORY]
	lappend entries [list -type separator]
 	lappend entries [list -type cascade -menu MENU_MAGIC -label [mc Magic] -identifier M_MAGIC]
	lappend entries [list -type cascade -menu MENU_USE -label [mc Use] -identifier M_USE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Inspect] -identifier E_INVEN_INSPECT]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Inscribe] -identifier E_INVEN_INSCRIBE]
	lappend entries [list -type command -label [mc Uninscribe] -identifier E_INVEN_UNINSCRIBE]

	NSMenu::MenuInsertEntries $mbarId -end MENU_INVEN $entries

	#
	# Book Menu -- Hey, Steve!
	#

	if {[llength [angband player spell_book]]} {
		NSModule::LoadIfNeeded NSBookMenu
		NSObject::New NSBookMenu $mbarId
	}

	if {[variant ZANGBANDTK]} {
		#
		# Mindcraft Menu -- For Mindcrafter class only
		#

		if {[angband player class] eq "Mindcrafter"} {
			NSModule::LoadIfNeeded NSMindcraftMenu
			NSObject::New NSMindcraftMenu $mbarId
		}
	}

	#
	# Action Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_ACTION -label [mc Action] -underline 0 -identifier M_ACTION

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_ALTER]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc Alter] -identifier E_ACTION_ALTER]
	lappend entries [list -type command -label [mc Bash] -identifier E_ACTION_BASH]
	lappend entries [list -type command -label [mc Close] -identifier E_ACTION_CLOSE]
	lappend entries [list -type command -label [mc Disarm] -identifier E_ACTION_DISARM]
	lappend entries [list -type command -label [mc Open] -identifier E_ACTION_OPEN]
	lappend entries [list -type command -label [mc Tunnel] -identifier E_ACTION_TUNNEL]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_ALTER $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_LOOKING]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc Locate] -identifier E_ACTION_LOCATE]
	lappend entries [list -type command -label [mc Look] -identifier E_ACTION_LOOK]
	lappend entries [list -type command -label [mc Map] -identifier E_ACTION_MAP]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_LOOKING $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_RESTING]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc Rest] -identifier E_ACTION_REST]
	lappend entries [list -type command -label [mc "Stay (With Pickup)"] -identifier E_ACTION_STAY]
	lappend entries [list -type command -label [mc "Stay"] -identifier E_ACTION_STAY_TOGGLE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_RESTING $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_SEARCHING]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc Search] -identifier E_ACTION_SEARCH]
	lappend entries [list -type command -label [mc "Search Mode"] -identifier E_ACTION_SEARCH_MODE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_SEARCHING $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_MOVEMENT]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label [mc "Go Down"] -identifier E_ACTION_DOWN]
	lappend entries [list -type command -label [mc "Go Up"] -identifier E_ACTION_UP]
	lappend entries [list -type command -label [mc Run] -identifier E_ACTION_RUN]
	lappend entries [list -type command -label [mc "Walk (With Pickup)"] -identifier E_ACTION_WALK]
	lappend entries [list -type command -label [mc "Walk"] -identifier E_ACTION_WALK_TOGGLE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_MOVEMENT $entries

	set entries {}
 	lappend entries [list -type cascade -menu MENU_ACTION_ALTER -label [mc Alter] -identifier M_ACTION_ALTER]
 	lappend entries [list -type cascade -menu MENU_ACTION_LOOKING -label [mc Looking] -identifier M_ACTION_LOOKING]
 	lappend entries [list -type cascade -menu MENU_ACTION_MOVEMENT -label [mc Movement] -identifier M_ACTION_MOVEMENT]
 	lappend entries [list -type cascade -menu MENU_ACTION_RESTING -label [mc Resting] -identifier M_ACTION_RESTING]
 	lappend entries [list -type cascade -menu MENU_ACTION_SEARCHING -label [mc Searching] -identifier M_ACTION_SEARCHING]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Note] -identifier E_ACTION_NOTE]
	lappend entries [list -type command -label [mc Repeat] -identifier E_ACTION_REPEAT]
	lappend entries [list -type command -label [mc Target] -identifier E_ACTION_TARGET]
	if {[variant KANGBANDTK]} {
		lappend entries [list -type separator]
		lappend entries [list -type command -label [mc Pets] -identifier E_ACTION_PETS]
	}
	if {[variant OANGBANDTK]} {
		lappend entries [list -type separator]
		lappend entries [list -type command -label [mc "End Shapechange"] -identifier E_ACTION_SHAPE]
	}
	if {[variant ZANGBANDTK]} {
		lappend entries [list -type separator]
		lappend entries [list -type command -label [mc Pets] -identifier E_ACTION_PETS]
		lappend entries [list -type command -label [mc "Use Power"] -identifier E_ACTION_POWER]
	}

	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION $entries

	#
	# Other Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_OTHER
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_OTHER -label [mc Other] -underline 0 -identifier M_OTHER

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_PHOTO
	set entries {}
	lappend entries [list -type command -label [mc photo-new] -identifier E_PHOTO_NEW]
	lappend entries [list -type command -label [mc photo-open] -identifier E_PHOTO_OPEN]
	NSMenu::MenuInsertEntries $mbarId -end MENU_PHOTO $entries

	set entries {}
	lappend entries [list -type command -label [mc "Character Info"] -identifier E_OTHER_INFO]
	lappend entries [list -type command -label [mc Feeling] -identifier E_OTHER_FEELING]
	lappend entries [list -type command -label [mc "File Character"] -identifier E_OTHER_FILE]
	lappend entries [list -type command -label [mc "High Scores"] -identifier E_OTHER_SCORE]
	lappend entries [list -type command -label [mc Knowledge] -identifier E_OTHER_KNOWLEDGE]
	lappend entries [list -type command -label [mc "Message History"] -identifier E_OTHER_MESSAGES]
	lappend entries [list -type cascade -menu MENU_PHOTO -label [mc Photo] -identifier M_PHOTO]
	if {[variant KANGBANDTK]} {
		lappend entries [list -type command -label [mc "Quest Status"] -identifier E_OTHER_QUEST]
		lappend entries [list -type command -label [mc "Time Of Day"] -identifier E_OTHER_TIME]
	}
	if {[variant OANGBANDTK]} {
		lappend entries [list -type command -label [mc "Time Of Day"] -identifier E_OTHER_TIME]
	}
	if {[variant ZANGBANDTK]} {
		lappend entries [list -type command -label [mc "Quest Status"] -identifier E_OTHER_QUEST]
		lappend entries [list -type command -label [mc "Time Of Day"] -identifier E_OTHER_TIME]
	}
	NSMenu::MenuInsertEntries $mbarId -end MENU_OTHER $entries

	#
	# Options Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_PREFERENCES
		NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
			-menu MENU_PREFERENCES -label [mc Options] -underline 0 -identifier M_PREFERENCES

	set entries {}
	lappend entries [list -type command -label [mc Alternate] -identifier E_PREF_ALTERNATE]
	lappend entries [list -type command -label [mc Assign] -identifier E_PREF_ASSIGN]
	if {[file exists [Path borg borg.tcl]]} {
		lappend entries [list -type command -label [mc Borg] -identifier E_PREF_BORG]
	}
	lappend entries [list -type command -label [mc Color] -identifier E_PREF_COLOR]
	lappend entries [list -type command -label [mc Controls] -identifier E_PREF_CONTROLS]
	lappend entries [list -type command -label [mc Font] -identifier E_PREF_FONT]
	lappend entries [list -type command -label [mc Icons] -identifier E_PREF_ICONS]
	if {[Global music,avail]} {
		lappend entries [list -type command -label [mc Music] -identifier E_PREF_MUSIC]
	}
	lappend entries [list -type command -label [mc Options] -identifier E_PREF_OPTIONS]
	lappend entries [list -type command -label [mc "Prf Files"] -identifier E_PREF_PRF]
	if {[Value sound,wants]} {
		lappend entries [list -type command -label [mc Sound] -identifier E_PREF_SOUND]
	}
	lappend entries [list -type command -label [mc Sprite] -identifier E_PREF_SPRITE]
	lappend entries [list -type command -label [mc Squelch] -identifier E_PREF_SQUELCH]
	if {[Global music,avail] || [Value sound,wants]} {
		lappend entries [list -type separator]
	}
	if {[Global music,avail]} {
		lappend entries [list -type checkbutton -label [mc "Use Music"] \
			-variable ::Global(music,play) -identifier E_OTHER_MUSIC]
	}
	if {[Value sound,wants]} {
		lappend entries [list -type checkbutton -label [mc "Use Sound"] \
			-variable ::NSMainWindow($oop,useSound) -identifier E_OTHER_SOUND]
	}
	NSMenu::MenuInsertEntries $mbarId -end MENU_PREFERENCES $entries

	if {[file exists [CPathTk vault vault-editor.tcl]]} {

		#
		# Tool Menu
		#

		NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_TOOL
		NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
			-menu MENU_TOOL -label [mc Tool] -underline 0 -identifier M_TOOL

		set entries {}
		lappend entries [list -type command -label [mc "Vault Editor"] \
			-identifier E_TOOL_VAULT]
		NSMenu::MenuInsertEntries $mbarId -end MENU_TOOL $entries
	}

	#
	# Window Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_WINDOW
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_WINDOW -label [mc Window] -underline 0 -identifier M_WINDOW

	set entries {}
	lappend entries [list -type command -label [mc "Arrange Windows..."] -identifier E_WINDOW_DEFPOS]
if 0 {
	lappend entries [list -type command -label [mc "Maximize Windows..."] -identifier E_WINDOW_MAXIMIZE]
}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Save Window Positions"] -identifier E_WINDOW_SAVEPOS]
	lappend entries [list -type command -label [mc "Load Window Positions"] -identifier E_WINDOW_LOADPOS]
	lappend entries [list -type checkbutton -label [mc "AutoSave Positions"] \
		-variable ::NSMainWindow($oop,window,autosave) -identifier E_WINDOW_AUTOSAVE]
	if {[file exists [CPathTk choice-window.tcl]]} {
		Info $oop choiceWindow [Value choicewindow,show]
		lappend entries [list -type separator]
		lappend entries [list -type checkbutton -label [mc "Choice Window"] \
			-variable ::NSMainWindow($oop,choiceWindow) -identifier E_CHOICEWINDOW]
	}
	Info $oop messageWindow [Value message,float]
	lappend entries [list -type checkbutton -label [mc "Message Window"] \
		-variable ::NSMainWindow($oop,messageWindow) -identifier E_WINDOW_MESSAGE]
	Info $oop messagesWindow 0
	lappend entries [list -type checkbutton -label [mc "Messages Window"] \
		-variable ::NSMainWindow($oop,messagesWindow) -identifier E_WINDOW_MESSAGES]
	Info $oop mapWindow [Value micromap,float]
	lappend entries [list -type checkbutton -label [mc "Micro Map Window"] \
		-variable ::NSMainWindow($oop,mapWindow) -identifier E_WINDOW_MAP]
	Info $oop misc,float [Value misc,float]
	lappend entries [list -type checkbutton -label [mc "Misc Window"] \
		-variable ::NSMainWindow($oop,misc,float) -identifier E_WINDOW_MISC]
	Info $oop progress,float [Value progress,float]
	lappend entries [list -type checkbutton -label [mc "Progress Window"] \
		-variable ::NSMainWindow($oop,progress,float) -identifier E_WINDOW_PROGRESS]
	Info $oop recallWindow [Value recall,show]
	lappend entries [list -type checkbutton -label [mc "Recall Window"] \
		-variable ::NSMainWindow($oop,recallWindow) -identifier E_WINDOW_RECALL]
	NSMenu::MenuInsertEntries $mbarId -end MENU_WINDOW $entries

	#
	# Help Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_HELP
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_HELP -label [mc Help] -underline 0 -identifier M_HELP

	set entries {}
	lappend entries [list -type command -label [mc Help] -identifier E_HELP]
	lappend entries [list -type command -label [mc Tips] -identifier E_TIPS]
	lappend entries [list -type separator]
	lappend entries [list -type command \
		-label [format [mc "About %s..."] $Angband(name)] -identifier E_ABOUT]

	NSMenu::MenuInsertEntries $mbarId -end MENU_HELP $entries

	# Hack -- Accelerators depend on current keymap!
	SynchMenuAccel $oop 1
	qebind $win <Setting-rogue_like_commands> \
		"NSMainWindow::SynchMenuAccel $oop 0"
	qebind $win <Keymap> \
		"NSMainWindow::SynchMenuAccel $oop 0"

	# Hack -- Some labels depends on always_pickup
	AlwaysPickupChanged $oop
	qebind $win <Setting-always_pickup> \
		"NSMainWindow::AlwaysPickupChanged $oop"

NSMenu::SetIdentArray $mbarId

	return
}

# NSMainWindow::SetupMenus --
#
#	Called by NSMenus::_MenuPostCommand() to enable menu items before
#	posting a menu.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	mbarId					OOP ID of NSMenu object (the menubar).
#
# Results:
#	What happened.

proc NSMainWindow::SetupMenus {oop mbarId} {

	global Windows

	lappend identList E_OTHER_MUSIC E_OTHER_SOUND E_WINDOW_SAVEPOS E_WINDOW_DEFPOS \
		E_WINDOW_LOADPOS E_WINDOW_AUTOSAVE E_WINDOW_MAXIMIZE E_ABOUT E_TIPS
	lappend identList E_PREF_ASSIGN E_PREF_COLOR E_PREF_CONTROLS \
		E_PREF_FONT E_PREF_ICONS E_PREF_MUSIC E_PREF_SOUND \
		E_PREF_SPRITE E_PREF_ALTERNATE E_PREF_PRF E_PREF_SQUELCH
	lappend identList M_PHOTO E_PHOTO_NEW E_PHOTO_OPEN

	if {[file exists [Path borg borg.tcl]]} {
		lappend identList E_PREF_BORG
	}
	if {[file exists [CPathTk vault vault-editor.tcl]]} {
		lappend identList E_TOOL_VAULT
	}

	lappend identList E_CHOICEWINDOW E_WINDOW_MESSAGE E_WINDOW_MESSAGES \
		E_WINDOW_MAP E_WINDOW_MISC E_WINDOW_PROGRESS E_WINDOW_RECALL
	if {[info exists Windows(choice)]} {
		Info $oop choiceWindow [winfo ismapped [Window choice]]
	}
	Info $oop messageWindow [winfo ismapped [Window message]]
	if {[info exists Windows(message2)]} {
		Info $oop messagesWindow [winfo ismapped [Window message2]]
	}
	Info $oop mapWindow [winfo ismapped [Window micromap]]
	Info $oop misc,float [winfo ismapped [Window misc]]
	Info $oop recallWindow [winfo ismapped [Window recall]]

	if {[angband inkey_flags] eq "INKEY_CMD"} {
		lappend identList E_GAME_SAVE E_GAME_EXIT E_OTHER_FEELING \
			E_OTHER_INFO E_OTHER_FILE E_OTHER_KNOWLEDGE \
			E_OTHER_MESSAGES E_PREF_OPTIONS E_OTHER_SCORE E_HELP

		if {[variant KANGBANDTK]} {
			lappend identList E_OTHER_QUEST E_OTHER_TIME
		}
		if {[variant OANGBANDTK]} {
			lappend identList E_OTHER_TIME
		}
		if {[variant ZANGBANDTK]} {
			lappend identList E_OTHER_QUEST E_OTHER_TIME
		}
	}

	lappend identList E_GAME_ABORT

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSMainWindow::MenuSetupCmd --
#
#	Called when a menu is about to be posted. We use this to change the
#	setupMode of a menu so we don't need to pass a huge list of identifiers
#	to the MenuEnable() command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::MenuSetupCmd {oop menuId} {

	if {[angband inkey_flags] ne "INKEY_CMD"} {
		NSMenu::Info $menuId setupMode disabled
	} else {
		NSMenu::Info $menuId setupMode normal
	}

	return
}

# NSMainWindow::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::MenuInvoke {oop menuId ident} {

	switch -glob -- $ident {

		E_GAME_SAVE {DoUnderlyingCommand ^s}
		E_GAME_EXIT {DoUnderlyingCommand ^x}
		E_GAME_ABORT {QuitNoSave $oop}

		E_MAGIC_ACTIVATE {DoUnderlyingCommand A}
		E_MAGIC_WAND {DoUnderlyingCommand a}
		E_MAGIC_POTION {DoUnderlyingCommand q}
		E_MAGIC_SCROLL {DoUnderlyingCommand r}
		E_MAGIC_STAFF {DoUnderlyingCommand u}
		E_MAGIC_ROD {DoUnderlyingCommand z}
		E_MAGIC_BROWSE {DoUnderlyingCommand b}
		E_MAGIC_STUDY {DoUnderlyingCommand G}

		E_USE_DESTROY {DoUnderlyingCommand k}
		E_USE_DROP {DoUnderlyingCommand d}
		E_USE_PICKUP {DoUnderlyingCommand g}
		E_USE_TAKEOFF {DoUnderlyingCommand t}
		E_USE_WIELD {DoUnderlyingCommand w}
		E_USE_FOOD {DoUnderlyingCommand E}
		E_USE_MISSILE {DoUnderlyingCommand f}
		E_USE_FUEL {DoUnderlyingCommand F}
		E_USE_SPIKE {DoUnderlyingCommand j}
		E_USE_THROW {DoUnderlyingCommand v}

		E_INVEN_EQUIPMENT {DoUnderlyingCommand e}
		E_INVEN_INVENTORY {DoUnderlyingCommand i}
		E_INVEN_INSPECT {DoUnderlyingCommand I}
		E_INVEN_INSCRIBE {DoUnderlyingCommand \{}
		E_INVEN_UNINSCRIBE {DoUnderlyingCommand \}}

		E_ACTION_ALTER {DoUnderlyingCommand +}
		E_ACTION_BASH {DoUnderlyingCommand B}
		E_ACTION_CLOSE {DoUnderlyingCommand c}
		E_ACTION_DISARM {DoUnderlyingCommand D}
		E_ACTION_DOWN {DoUnderlyingCommand >}
		E_ACTION_OPEN {DoUnderlyingCommand o}
		E_ACTION_LOCATE {DoUnderlyingCommand L}
		E_ACTION_LOOK {DoUnderlyingCommand l}
		E_ACTION_MAP {DoUnderlyingCommand M}
		E_ACTION_NOTE {DoUnderlyingCommand :}
		E_ACTION_SHAPE {DoUnderlyingCommand \]}
		E_ACTION_PETS {
			if {[variant KANGBANDTK]} {
				DoUnderlyingCommand $
			}
			if {[variant ZANGBANDTK]} {
				DoUnderlyingCommand p
			}
		}
		E_ACTION_POWER {DoUnderlyingCommand U}
		E_ACTION_REPEAT {
			if {[variant ANGBANDTK KANGBANDTK]} {
				DoUnderlyingCommand ^V
			}
			if {[variant OANGBANDTK ZANGBANDTK]} {
				DoUnderlyingCommand n
			}
		}
		E_ACTION_REST {DoUnderlyingCommand R}
		E_ACTION_RUN {DoUnderlyingCommand .}
		E_ACTION_SEARCH {DoUnderlyingCommand s}
		E_ACTION_SEARCH_MODE {DoUnderlyingCommand S}
		E_ACTION_STAY {DoUnderlyingCommand ,}
		E_ACTION_STAY_TOGGLE {DoUnderlyingCommand g}
		E_ACTION_TARGET {DoUnderlyingCommand *}
		E_ACTION_TUNNEL {DoUnderlyingCommand T}
		E_ACTION_UP {DoUnderlyingCommand <}
		E_ACTION_WALK {DoUnderlyingCommand ";"}
		E_ACTION_WALK_TOGGLE {DoUnderlyingCommand -}

		E_PREF_ALTERNATE {
			NSModule::LoadIfNeeded NSAlternate
			NSWindowManager::Display alternate
		}
		E_PREF_ASSIGN {
			NSModule::LoadIfNeeded NSAssign
			NSWindowManager::Display assign
		}
		E_PREF_BORG {
			NSModule::LoadIfNeeded NSBorg
			NSWindowManager::Display borg
		}
		E_PREF_COLOR {
			NSModule::LoadIfNeeded NSColorPreferences
			NSWindowManager::Display color
		}
		E_PREF_CONTROLS {
			NSModule::LoadIfNeeded NSControls
			NSWindowManager::Display controls
		}
		E_PREF_FONT {
			NSModule::LoadIfNeeded NSFont
			NSWindowManager::Display font
		}
		E_PREF_ICONS {
			NSModule::LoadIfNeeded NSIconWindow
			NSWindowManager::Display icon
		}
		E_PREF_COLOR {
			NSModule::LoadIfNeeded NSColorPreferences
			NSWindowManager::Display color
		}
		E_PREF_MUSIC {
			NSModule::LoadIfNeeded NSMusic
			NSWindowManager::Display music
		}
		E_PREF_OPTIONS {DoUnderlyingCommand =}
		E_PREF_PRF {
			NSModule::LoadIfNeeded NSPrfWindow
			NSWindowManager::Display prf
		}
		E_PREF_SOUND {
			NSModule::LoadIfNeeded NSSound
			NSWindowManager::Display sound
		}
		E_PREF_SPRITE {
			NSModule::LoadIfNeeded NSSprite
			NSWindowManager::Display sprite
		}
		E_PREF_SQUELCH {
			NSModule::LoadIfNeeded NSSquelch
			NSWindowManager::Display squelch
		}

		E_PHOTO_NEW {
			NSModule::LoadIfNeeded NSPhotoWindow
			set photoId [NSObject::New NSPhotoWindow]
			NSPhotoWindow::New $photoId [Global main,widget]
		}
		E_PHOTO_OPEN {
			NSModule::LoadIfNeeded NSPhotoWindow
			set photoId [NSObject::New NSPhotoWindow]
			after 1 NSPhotoWindow::Open $photoId
		}

		E_OTHER_INFO {DoUnderlyingCommand C}
		E_OTHER_FEELING {DoUnderlyingCommand ^F}
		E_OTHER_FILE {FileCharacter [Info $oop win]}
		E_OTHER_SCORE {
			NSModule::LoadIfNeeded NSHighScore
			NSHighScore::Info [Global highscore,oop] interactive 0
			NSWindowManager::Display highscore
		}
		E_OTHER_KNOWLEDGE {DoUnderlyingCommand ~}
		E_OTHER_MESSAGES {DoUnderlyingCommand ^p}
		E_OTHER_QUEST {DoUnderlyingCommand ^Q}
		E_OTHER_TIME {DoUnderlyingCommand ^T}
		E_OTHER_MUSIC {ToggleMusic $oop}
		E_OTHER_SOUND {ToggleSound $oop}

		E_TOOL_VAULT {
			NSModule::LoadIfNeeded NSVaultEditor
			NSWindowManager::Display vaulteditor
		}

		E_WINDOW_DEFPOS {
			set title [mc dialog-title-defpos]
			set message [mc dialog-msg-defpos]
			set answer [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title $title -message $message]
			if {$answer eq "yes"} {
				HardcodeGeometry
			}
		}
		E_WINDOW_MAXIMIZE {
			set title [mc dialog-title-max]
			set message [mc dialog-msg-max]
			set answer [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title $title -message $message]
			if {$answer eq "yes"} {
				MaximizeWindows
			}
		}
		E_WINDOW_SAVEPOS {WriteGeometryFile}
		E_WINDOW_LOADPOS {
			set title [mc dialog-title-loadpos]
			if {![file exists [PathTk config geometry]]} {
				set message [mc dialog-msg-loadpos-fail]
				tk_messageBox -parent [Info $oop win] \
					-title $title -message $message
				return
			}
			set message [mc dialog-msg-loadpos]
			set answer [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title $title -message $message]
			if {$answer eq "yes"} {
				ReadGeometryFile
			}
		}
		E_WINDOW_AUTOSAVE {
			Value window,autosave [Info $oop window,autosave]
		}

		E_CHOICEWINDOW {
			if {[Info $oop choiceWindow]} {
				NSModule::LoadIfNeeded NSChoiceWindow
				NSWindowManager::Display choice
			} else {
				NSWindowManager::Undisplay choice
			}
		}
		E_WINDOW_MAP {
			if {[Info $oop mapWindow]} {
				NSWindowManager::Display micromap
			} else {
				NSWindowManager::Undisplay micromap
			}
		}
		E_WINDOW_MESSAGES {
			if {[Info $oop messagesWindow]} {
				NSModule::LoadIfNeeded NSMessagesWindow
				NSWindowManager::Display message2
			} else {
				NSWindowManager::Undisplay message2
			}
		}
		E_WINDOW_MESSAGE {
			if {[Info $oop messageWindow]} {
				NSWindowManager::Display message
				grid remove [Window main].message
				Global message,message [Window message].message
			} else {
				NSWindowManager::Undisplay message
				grid [Window main].message
				Global message,message [Window main].message.message
			}
			Value message,float [Info $oop messageWindow]
		}
		E_WINDOW_MISC {
			if {[Info $oop misc,float]} {
				DetachMisc $oop
				if {![Value progress,float]} {
					DetachProgress $oop
				}
				NSWindowManager::Display misc
			} else {
				NSWindowManager::Undisplay misc
				AttachMisc $oop
				if {![Value progress,float]} {
					AttachProgress $oop
if 0 {
					set width [[Info $oop misc,canvas] cget -width]
					[Info $oop progress,canvas] configure -width $width
					NSProgressCanvas::Arrange [Info $oop progress,oop]
					scan [[Info $oop misc,canvas] bbox all] "%d %d %d %d" left top right bottom
					place [Info $oop progress,canvas] -x 0 -y $bottom
}
				}
			}
		}
		E_WINDOW_PROGRESS {
			if {[Info $oop progress,float]} {
				if {[Info $oop misc,float]} {
					NSMiscWindow::DetachProgress [Global misc,oop]
				} else {
					DetachProgress $oop
				}
				NSWindowManager::Display progress
			} elseif {[Info $oop misc,float]} {
				NSWindowManager::Undisplay progress
				NSMiscWindow::AttachProgress [Global misc,oop]
			} else {
				NSWindowManager::Undisplay progress
				AttachProgress $oop
if 0 {
				set width [[Info $oop misc,canvas] cget -width]
				[Info $oop progress,canvas] configure -width $width
				NSProgressCanvas::Arrange [Info $oop progress,oop]
				scan [[Info $oop misc,canvas] bbox all] "%d %d %d %d" left top right bottom
				place [Info $oop progress,canvas] -x 0 -y $bottom
}
			}
		}
		E_WINDOW_RECALL {
			if {[Info $oop recallWindow]} {
				NSWindowManager::Display recall
			} else {
				NSWindowManager::Undisplay recall
			}
		}

		E_HELP {DoUnderlyingCommand ?}
		E_TIPS {
			NSModule::LoadIfNeeded NSTips 
			WindowBringToFront [Window tip]
		}
		E_ABOUT {AboutApplication}
		default {
			error "unhandled menu entry \"$ident\""
		}
	}

	return
}

# NSMainWindow::QuitNoSave --
#
#	Quit the game without saving. If the game is not asking for
#	a command, then call "game abort". Otherwise do a clean exit.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::QuitNoSave {oop} {

	global Angband

	set win [Info $oop win]

	# Ask the user to confirm quit without saving
	set answer [tk_messageBox -icon question -type yesno \
		-title [format [mc dialog-title-quit] $Angband(name)] \
		-message [mc dialog-msg-abort] -parent $win]
	if {$answer eq "no"} return

	# Check if game is waiting for a command
	if {[angband inkey_flags] ne "INKEY_CMD"} {
		angband game abort -noask
		return
	}

	# Quit without saving
	DoUnderlyingCommand ^Ey

	return
}

# NSMainWindow::Close --
#
#	Called when the user attempts to close the window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::Close {oop} {

	global Angband

	set win [Info $oop win]

	# Check if game is waiting for a command. If not, it isn't a
	# good time to quit.
	if {[angband inkey_flags] ne "INKEY_CMD"} {
		bell
		return
	}

	# Ask the user to confirm quit with save
	set answer [tk_messageBox -icon question -type yesno \
		-title [format [mc dialog-title-quit] $Angband(name)] \
		-message [mc dialog-msg-quit] -parent $win]
	if {$answer eq "no"} return

	# Save and quit
	DoCommandIfAllowed ^x

	return
}

# NSMainWindow::Configure --
#
#	Called when the Main Window widget changes size.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::Configure {oop width height} {

	variable Progress

	set widgetId [Global main,widgetId]
	set widget [Global main,widget]

	# Resize the main widget
	if {[NSWidget::Resize $widgetId $width $height]} {

		# Move the Monster Health Bar
		set x [expr {$width / 2}]
		set y [expr {$height - 8}]
		$widget itemconfigure $Progress(barId) -x $x -y $y
		$widget itemconfigure $Progress(textId) -x $x -y [expr {$y - 6}]

		# Move the target 'T' item
		set x [expr {$width - 6}]
		set y [expr {$height - 6}]
		$widget itemconfigure [Global target,itemId] -x $x -y $y

		# Arrange "status" items
		NSStatus::Configure
	}

	return
}

proc NSMainWindow::DisplayPoints {oop itemId value} {

	set widget [Global main,widget]
	$widget itemconfigure $itemId -text $value

	return
}

# NSMainWindow::TargetSetup --
#
#	One-time initialization. When the target is set, display an
#	image in the lower-left corner. Remove the image when the target
#	is unset. Display the image differently when the target is
#	projectable/not-projectable.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::TargetSetup {oop} {

	variable Target

	set win [Info $oop win]

	set widget [Global main,widget]
	set width [$widget cget -width]
	set height [$widget cget -height]
	set x [expr {$width - 6}]
	set y [expr {$height - 6}]

	if {[Platform unix]} {
		set font {Times 14 bold}
	}
	if {[Platform windows]} {
		set font {Times 11 bold}
	}

	set itemId [$widget create text -x $x -y $y -visible no \
		-anchor se -text T -clipx no -width 20 -height 20 -bevel yes \
		-font $font -justify center \
		-fill [Value targetText] \
		-fill2 [Value targetText2] \
		-background [Value targetBG] \
		-background2 [Value targetBG2] \
		-bevellight [Value targetBL] \
		-bevellight2 [Value targetBL2] \
		-beveldark [Value targetBD] \
		-beveldark2 [Value targetBD2]]

	qebind $widget <Target-set> {
		NSMainWindow::TargetSet %W %r
	}

	qebind $widget <Target-unset> "
		$widget itemconfigure $itemId -visible no
	"

	# Reserve the widget colors for the target image
	set data {
		-fill targetText 1
		-background targetBG 1
		-bevellight targetBL 1
		-beveldark targetBD 1
		-fill target2Text 0
		-background target2BG 0
		-bevellight target2BL 0
		-beveldark target2BD 0
	}
	foreach {option valueName visible} $data {
		set color [palette nearest [Value $valueName]]
		set opacity [Value ${valueName}2]
		$widget coloralloc $color $opacity
		set Target(alloc,$valueName) [list $color $opacity]

		qebind NSMainWindow <Value-$valueName> \
			"NSMainWindow::TargetSynch $valueName $option $visible"
		qebind NSMainWindow <Value-${valueName}2> \
			"NSMainWindow::TargetSynch ${valueName}2 ${option}2 $visible"
	}

	qebind $widget <Target-visibility> {
		if {[Global target,visible] != %v} {
			NSMainWindow::SetTargetColors %v
		}
	}

	Global target,itemId $itemId
	Global target,visible 0

	# This "cursor" is displayed during targetting/looking
	set itemId [$widget create cursor -color yellow -linewidth 2 -visible no]
	if {[icon style] eq "iso"} {
#		$widget itemconfigure $itemId -linewidth 1
	}

	qebind $widget <Term-fresh> "NSMainWindow::Fresh_Cursor $oop"
	qeconfigure $widget <Term-fresh> -active no

	if {$::DEBUG} {
		set ::debug_cursor 0
	}

	qebind $widget <Cursor-show> {
		Global cursor,x %x
		Global cursor,y %y
		qeconfigure %W <Term-fresh> -active yes
		if {$::DEBUG} {
			set ::debug_cursor 1
		}
		Global cursor,requested 1
	}

	qebind $widget <Cursor-hide> {
		if {[Global cursor,visible]} {
			NSMainWindow::DisplayCursor [Global main,oop] 0 -1 -1
		}
		qeconfigure [Global main,widget] <Term-fresh> -active no
		if {$::DEBUG} {
			set ::debug_cursor 0
		}
		Global cursor,requested 0
	}

	Global cursor,itemId $itemId
	Global cursor,visible 0
	Global cursor,requested 0

	return
}

# NSMainWindow::TargetSet --
#
#	Handle the <Target-set> quasi-event.
#
# Arguments:
#	widget					the main Widget
#	r_idx					r_info[] index, or zero
#
# Results:
#	What happened.

proc NSMainWindow::TargetSet {widget r_idx} {

	set itemId [Global target,itemId]

	if {$r_idx} {
		set text [angband r_info set $r_idx d_char]
	} else {
		set text ""
	}
	$widget itemconfigure $itemId -visible yes -text $text

	return
}

# NSMainWindow::SetTargetColors --
#
#	Sets the colors of the target indicator.
#
# Arguments:
#	visible						True if target in line-of-sight
#
# Results:
#	What happened.

proc NSMainWindow::SetTargetColors {visible} {

	variable Target

	set widget [Global main,widget]

	if {$visible} {
		set data {
			-fill targetText
			-background targetBG
			-bevellight targetBL
			-beveldark targetBD
		}
	} else {
		set data {
			-fill target2Text
			-background target2BG
			-bevellight target2BL
			-beveldark target2BD
		}
	}

	set command "$widget itemconfigure [Global target,itemId]"
	foreach {option valueName} $data {
		append command " $option [Value $valueName]"
		append command " ${option}2 [Value ${valueName}2]"
	}
	eval $command

	Global target,visible $visible

	return
}

# NSMainWindow::TargetSynch --
#
#	Called when any of the target indicator colors change.
#
# Arguments:
#	valueName				The name of the value.
#	option					Configuration option for the widget item.
#
# Results:
#	What happened.

proc NSMainWindow::TargetSynch {valueName option visible} {

	variable Target

	set widget [Global main,widget]
	set itemId [Global target,itemId]

	set name [string trimright $valueName 2]
	set color [lindex $Target(alloc,$name) 0]
	set opacity [lindex $Target(alloc,$name) 1]
	$widget colorderef $color $opacity

	set value [Value $valueName]
	if {[string match *2 $valueName]} {
		set opacity $value
	} else {
		set color [palette nearest $value]
	}
	$widget coloralloc $color $opacity
	set Target(alloc,$name) [list $color $opacity]

	if {[Global target,visible] == $visible} {
		$widget itemconfigure $itemId $option $value
	}

	return
}

# NSMainWindow::ProgressSetup --
#
#	Creates a Widget text item (for the monster name) and progress item
#	(for the monster hit points) in the Main Window.
#
# Arguments:
#	widget					The Widget to create the monster bar in.
#
# Results:
#	What happened.

proc NSMainWindow::ProgressSetup {widget} {

	variable Progress

	if {[variant KANGBANDTK ZANGBANDTK]} {

		# XXX Mega-Hack XXX
		# The monster health bar is displayed differently for friendly versus
		# non-friendly monsters. This is done by using a different set of
		# colors for each state. We don't want the progress item to repeatedly
		# allocate and deallocate the colors it uses, so we call the
		# "$widget coloralloc" command to preallocate each color used by
		# the progress item.
		#
		# When the user chooses new progress item colors via the Color
		# Preferences Window, we must deallocate those colors previously
		# pre-allocated, then pre-allocate the new colors.

		foreach name {BarDone BarToDo BarBL BarBD} {

			set color [palette nearest [Value health$name]]
			set opacity [Value health${name}2]
			$widget coloralloc $color $opacity
			set Progress(alloc,health$name) [list $color $opacity]

			set color [palette nearest [Value friend$name]]
			set opacity [Value friend${name}2]
			$widget coloralloc $color $opacity
			set Progress(alloc,friend$name) [list $color $opacity]
		}
	# KANGBANDTK, ZANGBANDTK
	}

	set width [$widget cget -width]
	set height [$widget cget -height]
	set x [expr {$width / 2}]
	set y [expr {$height - 8}]

	set data {
		healthBarDone done
		healthBarToDo todo
		healthBarBL bevellight
		healthBarBD beveldark
		healthBarDone2 done2
		healthBarToDo2 todo2
		healthBarBL2 bevellight2
		healthBarBD2 beveldark2
	}
	foreach {name varname} $data {
		set $varname [Value $name]
	}

	set Progress(barId) \
		[$widget create progressbar -x $x -y $y -width 150 -height 6 \
		-anchor s -done $done -done2 $done2 -todo $todo -todo2 $todo2 \
		-bevellight $bevellight -bevellight2 $bevellight2 \
		-beveldark $beveldark -beveldark2 $beveldark2 \
		-visible no]

	set data {
		healthNameText fill
		healthNameBG background
		healthNameBL bevellight
		healthNameBD beveldark
		healthNameText2 fill2
		healthNameBG2 background2
		healthNameBL2 bevellight2
		healthNameBD2 beveldark2
	}
	foreach {name varname} $data {
		set $varname [Value $name]
	}

	# Get the desired font
	set font [Value font,monster]

	# Calculate the height of a row
	set fontHeight [font metrics $font -linespace]

	# Fudge
	incr fontHeight 1

	# Initial width (will expand automatically)
	set nameWidth 150

	# Create a widget text item
	set Progress(textId) [$widget create text -x $x -y [expr {$y - 6}] \
		-visible no -anchor s -fill $fill -fill2 $fill2 \
		-background $background -background2 $background2 \
		-bevellight $bevellight -bevellight2 $bevellight2 \
		-beveldark $beveldark -beveldark2 $beveldark2 -font $font \
		-clipx yes -clipy yes -width $nameWidth -height $fontHeight \
		-bevel yes -expandx yes -expandy yes -padbottom 1]

	set data [list \
		healthBarDone -done \
		healthBarToDo -todo \
		healthBarBL -bevellight \
		healthBarBD -beveldark \
		healthBarDone2 -done2 \
		healthBarToDo2 -todo2 \
		healthBarBL2 -bevellight2 \
		healthBarBD2 -beveldark2 \
	]

	lappend data \
		healthNameText -fill \
		healthNameBG -background \
		healthNameBL -bevellight \
		healthNameBD -beveldark \
		healthNameText2 -fill2 \
		healthNameBG2 -background2 \
		healthNameBL2 -bevellight2 \
		healthNameBD2 -beveldark2

	if {[variant KANGBANDTK ZANGBANDTK]} {
		lappend data \
			friendBarDone -done \
			friendBarToDo -todo \
			friendBarBL -bevellight \
			friendBarBD -beveldark \
			friendBarDone2 -done2 \
			friendBarToDo2 -todo2 \
			friendBarBL2 -bevellight2 \
			friendBarBD2 -beveldark2
	}

	foreach {name option} $data {
		qebind NSMainWindow <Value-$name> \
			"NSMainWindow::ProgressSynch $name $option"
	}

	set Progress(visible) 0
	set Progress(current) 0
	set Progress(r_idx) 0
	if {[variant KANGBANDTK ZANGBANDTK]} {
		set Progress(friend) 0
	}

	# Update ourself when the font,monster value changes
	qebind NSMainWindow <Value-font,monster> \
		"NSMainWindow::ValueChanged_font_monster"

	return
}

# NSMainWindow::ValueChanged_font_monster --
#
#	Called when the font,monster value changes.
#	Updates the Monster Health Bar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::ValueChanged_font_monster {} {

	variable Progress

	# Get the desired font
	set font [Value font,monster]

	# Change the font. We rely on the -expandy option to resize the
	# bitmap for us.
	[Global main,widget] itemconfigure $Progress(textId) -font $font

	return
}

# NSMainWindow::ValueChanged_font_statusBar --
#
#	Called when the font,statusBar value changes.
#	Updates the Main Window statusbar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::ValueChanged_font_statusBar {} {

	set statusBar [Window main].statusBar

	# Get the desired font
	set font [Value font,statusBar]

	# Update the font. Too bad there isn't a -fontvar font variable
	$statusBar.frameLabel.label configure -font $font
	$statusBar.center configure -font $font
	$statusBar.depth configure -font $font

	return
}

# NSMainWindow::ProgressSynch --
#
#	Called when any of the healthName*, healthBar* or
#	friendBar* values change. Configures the monster health bar colors
#	as appropriate. Note the ugly song-and-dance number done to control
#	which colors are pre-allocated.
#
# Arguments:
#	name					The name of the value.
#	option					Configuration option for the widget item.
#
# Results:
#	What happened.

proc NSMainWindow::ProgressSynch {name option} {

	variable Progress

	set widget [Global main,widget]
	set value [Value $name]

	if {[variant ANGBANDTK OANGBANDTK]} {
		switch -glob $name {
			healthName* {
				$widget itemconfigure $Progress(textId) $option $value
			}
			healthBar* {
				$widget itemconfigure $Progress(barId) $option $value
			}
		}
	# ANGBANDTK, OANGBANDTK
	}

	if {[variant KANGBANDTK ZANGBANDTK]} {
		switch -glob $name {
			healthName* {
				$widget itemconfigure $Progress(textId) $option $value
			}
			healthBar*2 {
				set name2 [string trimright $name 2]
				$widget colorderef {*}$Progress(alloc,$name2)
				set color [lindex $Progress(alloc,$name2) 0]
				set Progress(alloc,$name2) [list $color $value]
				$widget coloralloc $color $value
				if {!$Progress(friend)} {
					$widget itemconfigure $Progress(barId) $option $value
				}
			}
			healthBar* {
				$widget colorderef {*}$Progress(alloc,$name)
				set opacity [lindex $Progress(alloc,$name) 1]
				set Progress(alloc,$name) [list [palette nearest $value] $opacity]
				$widget coloralloc [palette nearest $value] $opacity
				if {!$Progress(friend)} {
					$widget itemconfigure $Progress(barId) $option $value
				}
			}
			friendBar*2 {
				set name2 [string trimright $name 2]
				$widget colorderef {*}$Progress(alloc,$name2)
				set color [lindex $Progress(alloc,$name2) 0]
				set Progress(alloc,$name2) [list $color $value]
				$widget coloralloc $color $value
				if {$Progress(friend)} {
					$widget itemconfigure $Progress(barId) $option $value
				}
			}
			friendBar* {
				$widget colorderef {*}$Progress(alloc,$name)
				set opacity [lindex $Progress(alloc,$name) 1]
				set Progress(alloc,$name) [list [palette nearest $value] $opacity]
				$widget coloralloc [palette nearest $value] $opacity
				if {$Progress(friend)} {
					$widget itemconfigure $Progress(barId) $option $value
				}
			}
		}
	# KANGBANDTK, ZANGBANDTK
	}

	return
}

# NSRecall::TrackGrid --
#
#	Called as a <Term-fresh> quasi-event script.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::TrackGrid {oop} {

	qeconfigure TrackGrid <Term-fresh> -active no

	set y [Info $oop trackGrid,y]
	set x [Info $oop trackGrid,x]

	if {"$y $x" ne [NSWidget::Info [Global main,widgetId] examined]} {
		ExamineLocation $oop track [Global main,widgetId] $y $x {}
	}

	return
}

# NSMainWindow::DisplayCursor --
#
#	Shows the cursor in the main widget at the given cave
#	location. If the location is outside the visible area of the
#	widget, then the widget is centered on the location.
#
#	When hiding the cursor the widget is centered on the character
#	location if required.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	show				1 to show the cursor, 0 to hide it.
#	x, y				Cave location to place cursor at.
#
# Results:
#	What happened.

proc NSMainWindow::DisplayCursor {oop show x y} {

	if {0} {
		set isoview [Global main,isoview]
		if {$show} {
			$isoview configure -hit 1 -hitx $x -hity $y
		} else {
			$isoview configure -hit -1
		}
		$isoview wipe
		Global cursor,visible $show
		return
	}

	set widget [Global main,widget]

	# Show the cursor
	if {$show} {

		$widget itemconfigure [Global cursor,itemId] \
			-visible yes -x $x -y $y

		# Center the main widget unless the mouse is over the micromap.
		# This lets the user see spells flying offscreen.
		if {![NSUtils::HasCursor [Global micromap,widget]]} {
			if {![$widget visible $y $x]} {
				NSWidget::SetCenter [Global main,widgetId] $y $x
			}
		}

		Global cursor,visible 1

		# Remember the coordinates (could use "$widget itemcget -x")
		Global cursor,y $y
		Global cursor,x $x

	# Hide the cursor
	} else {

		$widget itemconfigure [Global cursor,itemId] \
			-visible no

		# Center the main widget unless the mouse is over the micromap.
		# This lets the user see spells flying offscreen.
		if {![NSUtils::HasCursor [Global micromap,widget]]} {
			if {[$widget center] ne [Global main,widget,center]} {
				NSWidget::SetCenter [Global main,widgetId] {*}[Global main,widget,center]
			}
		}

		# Hide the show_cave_balloon tooltip
		BalloonHide $oop

		Global cursor,visible 0
	}

	return
}

# NSRecall::Fresh_Cursor --
#
#	Called as a <Term-fresh> quasi-event script.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::Fresh_Cursor {oop} {

	ASSERT {$::debug_cursor == 1} \
		"Fresh_Cursor called with debug_cursor=0!"

	DisplayCursor $oop 1 [Global cursor,x] [Global cursor,y]

	ExamineLocation $oop cursor [Global main,widgetId] \
		[Global cursor,y] [Global cursor,x] {}

	Global cursor,requested 0

	return
}

# NSMainWindow::SynchMenuAccel --
#
#	Sets the accelerator option for certain menu entries depending on
#	the current keymap.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::SynchMenuAccel {oop force} {

	global NSMenu
	variable Priv

	# Since many keymaps may change when a pref file is read in, delay
	# configuring the menu accelerators until idle time.
	if {!$force} {

		if {![string length $Priv(keymap,afterId)]} {
			set Priv(keymap,afterId) \
				[after idle NSMainWindow::SynchMenuAccel $oop 1]
		}

		# The idle task was scheduled by a previous call, but this
		# call isn't from the idle task.
		return
	}

	# Important: clear the after id.
	set Priv(keymap,afterId) ""

	set mbarId [Info $oop mbarId]

	lappend data E_GAME_SAVE ^S
	lappend data E_GAME_EXIT ^X

	lappend data E_MAGIC_ACTIVATE A
	lappend data E_MAGIC_WAND a
	lappend data E_MAGIC_POTION q
	lappend data E_MAGIC_SCROLL r
	lappend data E_MAGIC_STAFF u
	lappend data E_MAGIC_ROD z
	lappend data E_MAGIC_BROWSE b
	lappend data E_MAGIC_STUDY G

	lappend data E_USE_DESTROY k
	lappend data E_USE_DROP d
	lappend data E_USE_PICKUP g
	lappend data E_USE_TAKEOFF t
	lappend data E_USE_WIELD w
	lappend data E_USE_FOOD E
	lappend data E_USE_MISSILE f
	lappend data E_USE_FUEL F
	lappend data E_USE_SPIKE j
	lappend data E_USE_THROW v

	lappend data E_INVEN_EQUIPMENT e
	lappend data E_INVEN_INVENTORY i
	lappend data E_INVEN_INSPECT I
	lappend data E_INVEN_INSCRIBE \{
	lappend data E_INVEN_UNINSCRIBE \}

	lappend data E_ACTION_ALTER +
	lappend data E_ACTION_BASH B
	lappend data E_ACTION_CLOSE c
	lappend data E_ACTION_DISARM D
	lappend data E_ACTION_DOWN >
	lappend data E_ACTION_LOCATE L
	lappend data E_ACTION_LOOK l
	lappend data E_ACTION_MAP M
	lappend data E_ACTION_NOTE :
	lappend data E_ACTION_OPEN o
	if {[variant ANGBANDTK KANGBANDTK]} {
		lappend data E_ACTION_REPEAT ^V
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		lappend data E_ACTION_REPEAT n
	}
	lappend data E_ACTION_REST R
	lappend data E_ACTION_RUN .
	lappend data E_ACTION_SEARCH s
	lappend data E_ACTION_SEARCH_MODE S
	lappend data E_ACTION_STAY ,
	lappend data E_ACTION_STAY_TOGGLE g
	lappend data E_ACTION_TARGET *
	lappend data E_ACTION_TUNNEL T
	lappend data E_ACTION_UP <
	lappend data E_ACTION_WALK ";"
	lappend data E_ACTION_WALK_TOGGLE -

	if {[variant KANGBANDTK]} {
		lappend data E_ACTION_PETS $
	}

	if {[variant OANGBANDTK]} {
		lappend data E_ACTION_SHAPE \]
	}

	if {[variant ZANGBANDTK]} {
		lappend data E_ACTION_PETS p
		lappend data E_ACTION_POWER U
	}

	lappend data E_HELP ?
	lappend data E_OTHER_FEELING ^F
	lappend data E_OTHER_INFO C
	lappend data E_OTHER_KNOWLEDGE ~
	lappend data E_OTHER_MESSAGES ^P
	if {[variant KANGBANDTK]} {
		lappend data E_OTHER_QUEST ^Q
		lappend data E_OTHER_TIME ^T
	}
	if {[variant OANGBANDTK]} {
		lappend data E_OTHER_TIME ^T
	}
	if {[variant ZANGBANDTK]} {
		lappend data E_OTHER_QUEST ^Q
		lappend data E_OTHER_TIME ^T
	}

	lappend data E_PREF_PRF @
	lappend data E_PREF_OPTIONS =

	foreach {ident key} $data {
		set entry [NSMenu::MenuFindEntry $mbarId $ident]
		if {$::DEBUG && ![llength $entry]} {
			error "can't find menu identifier \"$ident\""
		}
		set menuId [lindex $entry 0]
		set index [lindex $entry 1]
		set menu $NSMenu($menuId,menu)
		set keymaps [angband keymap find $key]
		if {[llength $keymaps]} {
			set string [lindex $keymaps 0]
#		regsub {\^} $string Ctrl+ string
			set string [string map {^ Ctrl+} $string]
			$menu entryconfigure $index -accelerator $string
		}
	}

	return
}

# NSMainWindow::AlwaysPickupChanged --
#
#	Called when the always_pickup option changes. I fiddle with the
#	appearance of some menu entries.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::AlwaysPickupChanged {oop} {

	set mbarId [Info $oop mbarId]

	if {[Setting always_pickup]} {
		set string1 " (With Pickup)"
		set string2 ""
	} else {
		set string1 ""
		set string2 " (With Pickup)"
	}

	NSMenu::EntryConfigure $mbarId E_ACTION_WALK \
		-label [mc Walk$string1]
	NSMenu::EntryConfigure $mbarId E_ACTION_WALK_TOGGLE \
		-label [mc Walk$string2]

	# In OAngband, 'g' always picks up, while ',' considers
	# always_pickup.
	if {[variant OANGBANDTK]} {
		set string2 " (With Pickup)"
	}

	NSMenu::EntryConfigure $mbarId E_ACTION_STAY \
		-label [mc Stay$string1]
	NSMenu::EntryConfigure $mbarId E_ACTION_STAY_TOGGLE \
		-label [mc Stay$string2]

	return
}

# NSMainWindow::MouseCmd --
#
#	Use to execute commands when a mouse button is pressed. The direction
#	is determined from the given widget coordinates.
#	Calls "angband keypress CMD DIR".
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	x					x coordinate in Widget (as returned by event)
#	y					y coordinate in Widget (as returned by event)
#	cmd					Command to invoke.
#
# Results:
#	What happened.

proc NSMainWindow::MouseCommand {oop x y cmd} {

	set widgetId [Global main,widgetId]

	set coords [NSWidget::PointToCave $widgetId $x $y]
	scan $coords "%d %d" caveY caveX
	set dirInfo [CaveToDirection $caveY $caveX]
	set charDir [lindex $dirInfo 0]

	if {$charDir != 5} {
		# Hide the show_cave_balloon tooltip
		BalloonHide $oop
		NSWidget::Info [Global main,widgetId] examined ""

		angband keypress \\$cmd$charDir
	}

	return
}

# NSMainWindow::TrackPress --
#
#	Set up mouse tracking when <ButtonPress-1> occurs. See TrackMotion()
#	and TrackOnce() as well.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	x					x coordinate in Widget (as returned by event)
#	y					y coordinate in Widget (as returned by event)
#
# Results:
#	What happened.

proc NSMainWindow::TrackPress {oop x y} {

	variable tracking
	variable track1st
	variable trackStepping
	variable trackX
	variable trackY

	set tracking 1
	set track1st 1
	set trackX $x
	set trackY $y

	BalloonHide $oop
	NSWidget::Info [Global main,widgetId] examined ""

	# Hack -- Allow drag during targetting
	if {[angband inkey_flags] eq "INKEY_TARGET"} {
		NSWidget::TrackPress [Global main,widgetId] $x $y
		return
	}

	variable trackHPfrac
	scan [angband player hitpoints] "%d %d %f" curhp maxhp hpfrac
	set trackHPfrac $hpfrac

	TrackOnce $oop

	set track1st 0

# July 13, 2004
if 0 {
	set trackStepping 1
	after [Setting mouse_repeat_delay] set NSMainWindow::trackStepping 0
}

	return
}

# NSMainWindow::TrackMotion --
#
#	Called to remember the cursor position when <Button1-Motion> occurs.
#	See TrackOnce() below as well.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	x					x coordinate in Widget (as returned by event)
#	y					y coordinate in Widget (as returned by event)
#
# Results:
#	What happened.

proc NSMainWindow::TrackMotion {oop x y} {

	variable trackX
	variable trackY
	variable trackStuck

	# Hack -- Allow drag during targetting
	if {[angband inkey_flags] eq "INKEY_TARGET"} {
		NSWidget::TrackOnce [Global main,widgetId] $x $y
		return
	}

	set trackX $x
	set trackY $y

	# July 13 2004
	if {$trackStuck} {
		TrackOnce $oop
	}

	return
}

# NSMainWindow::TrackOnce --
#
#	This command examines the result of "angband inkey_flags" and
#	takes some action depending on the value. During INKEY_MORE and
#	INKEY_DISTURB it calls "angband keypress" with a single space
#	character. During INKEY_DIR it calls "angband keypress" with the
#	corresponding direction character (0-9).
#
#	During INKEY_CMD it calls "angband keypress" with a direction
#	key (to move the character), but only if the grid is not blocked.
#
#	This command is usually called when the <Inkey> binding is invoked,
#	but if the character is unable to move it calls itself again as
#	an "after" command.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::mouse_repeat_interval {} {
	if {[angband player depth] == 0} {
		return [Setting mouse_repeat_town]
	}
	return [Setting mouse_repeat_interval]
}

# July 13 2004
proc NSMainWindow::TrackSchedule {oop {interval ""}} {

	variable track1st
	variable trackId2
	variable trackStepping
	variable trackStuck

	if {$track1st} {
		set delay [Setting mouse_repeat_delay]
	} elseif {$interval ne ""} {
		set delay $interval
	} else {
		set delay [mouse_repeat_interval]
	}

	set trackStepping 1
	set trackId2 [after $delay {
		set NSMainWindow::trackStepping 0
		NSMainWindow::TrackOnce [Global main,oop]
	}]

	set trackStuck 0

	return
}

# July 13 2004
set press 0
proc NSMainWindow::TrackOnce {oop} {

	variable tracking
	variable track1st
	variable trackX
	variable trackY
	variable trackStepping
	variable trackStuck

	# If the mouse isn't down, then do nothing. This command gets
	# called whenever the <Inkey> event is generated.
	if {!$tracking} return

#set ms [clock clicks -milliseconds]
#puts "TrackOnce after [expr $ms - $::press] stuck=$trackStuck"
#set ::press $ms

	# It is important to delay after each step, otherwise
	# the character won't be able to navigate cleanly, and -more-
	# messages may go zipping by.
	if {$trackStepping} return

#puts "...allow"

	# (1) Walking into a door with always_repeat
	# (2) Walking through rubble/tree (OAngband)
	if {!$track1st && [angband player command_rep]} return

	# Get the inkey_flags
	set flags [angband inkey_flags]

	# If the game is displaying the "-more-" message, feed the Term
	# with a single space character. This only works if the "quick_messages"
	# option is set.
	if {$flags eq "INKEY_MORE"} {
		angband keypress " "
		TrackSchedule $oop
		return
	}

	# If a repeated command is in progress, a mouse-click will disturb
	if {$flags eq "INKEY_DISTURB"} {
		angband keypress " "
		TrackSchedule $oop
		return
	}

	# July 4 2004
	# Ran into a store
	if {[angband store shopping]} {
		TrackRelease $oop
		return
	}

	set widgetId [Global main,widgetId]
	set widget [Global main,widget]

	set coords [NSWidget::PointToCave $widgetId $trackX $trackY]
	if {![string length $coords]} {
		set trackStuck 1
		return
	}
	scan $coords "%d %d" caveY caveX
	set dirInfo [CaveToDirection $caveY $caveX]
	set dirKey [lindex $dirInfo 0]
	set y [lindex $dirInfo 1]
	set x [lindex $dirInfo 2]

	# If the game is waiting for the user to enter a direction, then
	# feed the direction key into the Term.
	if {$flags eq "INKEY_DIR"} {
		angband keypress $dirKey
		TrackSchedule $oop
		return
	}

	# If the game is NOT asking for a command, then do nothing
	if {$flags ne "INKEY_CMD"} {
		return
	}

	# If the mouse is over the player grid, only move if this is
	# the initial mouse click. Otherwise the user may accidentally
	# "run on the spot".
	if {$dirKey == 5} {
		if {$track1st} {
			angband keypress \\,
			TrackSchedule $oop
		} else {
			set trackStuck 1
		}
		return
	}

	# If the grid in the desired direction is blocked, then we will
	# attempt to "slide" in that direction.
	if {[angband cave blocked $y $x]} {
		scan $::PYPX "%d %d" py px
		set xdiff [expr {abs($caveX - $px)}]
		set ydiff [expr {abs($caveY - $py)}]
		switch $dirKey {
			1 {
				if {$xdiff > $ydiff} {
					incr y -1
					set dirKey 4
				} else {
					incr x
					set dirKey 2
				}
			}
			3 {
				if {$xdiff > $ydiff} {
					incr y -1
					set dirKey 6
				} else {
					incr x -1
					set dirKey 2
				}
			}
			7 {
				if {$xdiff > $ydiff} {
					incr y 1
					set dirKey 4
				} else {
					incr x
					set dirKey 8
				}
			}
			9 {
				if {$xdiff > $ydiff} {
					incr y 1
					set dirKey 6
				} else {
					incr x -1
					set dirKey 8
				}
			}
		}
		if {[angband cave blocked $y $x]} {
			set trackStuck 1
			return
		}
	}

	# If the spacebar is down, we may get any number of Inkey
	# events per turn. To prevent "mouse command overflow" we
	# never feed the Term with more than one key per turn.
	if {[angband keycount]} return

	# If the character's hit points have dropped more than 15%, then
	# display a warning message and stop tracking. This is to prevent
	# insta-death by running into a nasty monster. This could be an
	# option.
	if 1 {

		# TODO: compare old maxhp to current maxhp, to prevent
		# warnings when exp level goes up.

		# Alway process the initial mouse click
		if {!$track1st} {

			variable trackHPfrac

			set oldFrac $trackHPfrac
			scan [angband player hitpoints] "%d %d %f" curhp maxhp hpfrac
			set trackHPfrac $hpfrac

			# Hit points dropped by over 15%
			if {$hpfrac < ($oldFrac * 0.85)} {

				# Message
				NSStatus::SetStatusMessage [mc Danger!] Danger! bad

				# The user must release and click again to move.
				TrackRelease $oop

				# No rescheduled track.
				return
			}
		}
	}

	# Move the character
	angband keypress "\\;$dirKey"

	# Hack -- Try to keep horizontal/vertical movement rates the same.
	if {[$widget cget -style] eq "icon"} {
		set gw [$widget cget -gwidth]
		set gh [$widget cget -gheight]
		if {!$track1st && ($gw != $gh)} {
			switch $dirKey {
				1 -
				3 -
				7 -
				9 { set dx 1 ; set dy 1 }
				2 -
				8 { set dx 0 ; set dy 1 }
				4 -
				6 { set dx 1 ; set dy 0 }
			}
			set dist [expr {sqrt($dx * $gw * $gw + $dy * $gh * $gh)}]
			if {$gw < $gh} {
				set max [expr {sqrt($gw * $gw)}]
				set ratio [expr {$gh / double($gw)}]
			} else {
				set max [expr {sqrt($gh * $gh)}]
				set ratio [expr {$gw / double($gh)}]
			}

			# Added this fudge factor cuz I'm too dumb to figure out the right way.
			if {$dist > $max} {
				set dist [expr {$dist * (2-sqrt($ratio))}]
			}

			set orig [mouse_repeat_interval]
			set interval [expr int($orig * $dist / $max)]
			TrackSchedule $oop $interval
			return
		}
	}

	# Hack -- Try to keep horizontal/vertical movement rates the same.
	if {[$widget cget -style] eq "iso"} {
		if {!$track1st} {
			switch $dirKey {
				1 -
				9 { set frac 1.6 }
				4 -
				6 { set frac 1.0 }
				2 -
				8 { set frac 1.0 }
				3 -
				7 { set frac 1.0 }
			}

			if {$frac != 1.0} {
				set orig [mouse_repeat_interval]
				set interval [expr int($orig * $frac)]
				TrackSchedule $oop $interval
				return
			}
		}
	}

	TrackSchedule $oop

	return
}

# NSMainWindow::TrackRelease --
#
#	Cancels mouse tracking when the mouse button is released.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::TrackRelease {oop} {

	variable trackId
	variable trackId2
	variable tracking
	variable trackStepping
	variable trackX
	variable trackY

	# One time I selected a menu command and received an error after releasing
	# the mouse-button
	if {!$tracking} return

	set tracking 0
	set trackStepping 0

	after cancel $trackId
	after cancel $trackId2

	# If the Widget wasn't dragged, then tell the game to target
	if {[angband inkey_flags] eq "INKEY_TARGET"} {
		set widgetId [Global main,widgetId]
		if {![NSWidget::Info $widgetId track,mouseMoved]} {
			set coords [NSWidget::PointToCave $widgetId $trackX $trackY]
			scan $coords "%d %d" caveY caveX
			angband keypress @$caveY\n$caveX\n
			return
		}
	}

	return
}

# NSMainWindow::ListFloorItems --
#
#	Returns a block of "char) name" text for items at the given location.
#	Used for the show_cave_balloon balloon.
#
# Arguments:
#	y, x				Cave location.
#
# Results:
#	What happened.

proc NSMainWindow::ListFloorItems {y x} {

	# Get info about this cave location
	angband cave info $y $x attrib

	# Get the object index
	set o_idx $attrib(o_idx)

	# Sanity check
	if {!$o_idx} return

	set result {}

	set index 0

	# Check each object in the stack
	while {$o_idx} {

		# Get info about this object
		array set attrib [angband o_list set $o_idx]

		# The object is marked
		if {$attrib(marked)} {

			# Get the char
			set char [string index "abcdefghijklmnopqrstuvw" $index]

			# Get the name
			set name [angband o_list info $o_idx desc]

			set color [default_tval_to_attr $attrib(tval)]

			# Append the description
			lappend result "$char) " {} "$name" $color "\n" {}

			incr index
		}

		# Get the next object in the stack
		set o_idx $attrib(next_o_idx)
	}

	# Trim trailing newline
	return [lreplace $result end-1 end]
}

# NSMainWindow::ExamineLocation --
#
#	Display a description of what the character sees at a given cave
#	location. Called as NSWidget(OOP,examineCmd) and by other code.
#
# Arguments:
#	oop					OOP ID MSMainWindow.
#	reason				"mouse", "cursor", "track", "bigmap", "micromap"
#	widgetId			OOP ID NSWidget.
#	y					y cave location.
#	x					x cave location.
#	modifiers			list of "control" and/or "shift"
#
# Results:
#	What happened.

proc NSMainWindow::ExamineLocation {oop reason widgetId y x modifiers} {

# dbwin "NSMainWindow::ExamineLocation reason=$reason $y,$x"

	variable tracking

	# Hack -- When we point to a location, the Recall Window may be
	# set with information, and we may want to interact with the
	# Recall Window to see the information. But if the mouse moves
	# over another grid (on the way to the Recall Window) the
	# information in the Recall Window may change. So we don't
	# examine cave locations when the Control key is down.
	if {$modifiers eq "control"} {
		BalloonHide $oop
		return
	}

	# Prevent error if a -more- prompt appears during level generation
	if {![angband cave exists]} return

	# Unused: PROJECT_HINT
	if {0 && [angband inkey_flags] eq "INKEY_TARGET"} {

		# Show affected grids
		angband keypress &$y\n$x\n

		# Don't bother describing the location, because it is overriden by
		# the <Track-grid> binding.
		return
	}

	# I notice a big slowdown when running, so avoid it
	if {[angband player running]} return

	# Describe the location
	set desc [angband cave examine $y $x]

	# Sometimes pop up a balloon over interesting grids.
	set widget [Global main,widget]
	if {$desc ne "" && !$tracking && [$widget visible $y $x] &&
		([Setting show_cave_balloon] || "shift" in $modifiers) &&
		("mouse" in $reason || "cursor" in $reason || "micromap" in $reason)} {
		set balloon [list $desc ""]
		if {[string match "*a pile of * items*" $desc]} {
			lappend balloon \n {}
			lappend balloon {*}[ListFloorItems $y $x]
		} else {
			angband cave info $y $x attrib
			set m_idx $attrib(m_idx)
			if {($m_idx > 0) && [angband m_list set $m_idx ml]} {
				if {[regexp {(.+) \((.+)\)} $desc v0 v1 v2]} {
					set balloon [list "$v1\n" "" $v2 [Value TERM_L_WHITE]]
				}
			}
		}
		scan [$widget bbox $y $x] "%s %s %s %s" left top right bottom
		set x2 [expr {[winfo rootx $widget] + $left + ($right - $left) / 2}]
		set y2 [expr {[winfo rooty $widget] + $bottom + 2}]
		BalloonShow $oop $balloon $x2 $y2 n
	} else {
#dbwin "NSMainWindow::ExamineLocation HIDE"
		BalloonHide $oop
	}

	# Debug: Show the coordinates
	if {$::DEBUG} {
		if {[string length $desc]} {
			append desc "   "
		}
		append desc "y=$y,x=$x"
	}

	# Set the statusbar text
	StatusText [Global main,oop] $desc

	# Hallucinating?
	if {[struct set player_type 0 image] > 0} return

	# Require valid location
	if {![angband cave in_bounds_fully $y $x]} return

	# Mega-Hack -- If the Recall Window is displaying a list of
	# choices, then we must not display anything.
	if {[string length [NSRecall::Info [Global recall,oop] hook]]} {
		return
	}

	# Get info about this cave location
	angband cave info $y $x attrib

	# A pile of objects is here
	if {[string match "*a pile of * items*" $desc]} {
		NSRecall::RecallStack $y $x

	# A quest entrance is here
	} elseif {[string match "*a quest entrance*" $desc]} {
if 1 {
		NSRecall::RecallQuest $attrib(special)
} else {
		if {[variant KANGBANDTK]} {
			set match [struct find quest_type \
				-limit 1 \
				-field status == QUEST_STATUS_TAKEN \
				-field vaultused == TRUE \
				-field questy == $y \
				-field questx == $x]

			# This should always be non-empty
			if {[llength $match]} {
				NSRecall::RecallQuest [lindex $match 0]
			}
		}
		if {[variant ZANGBANDTK]} {
			NSRecall::RecallQuest $attrib(special)
		}
}
	} else {

		set m_idx $attrib(m_idx)
		set o_idx $attrib(o_idx)

		# A monster is here
		if {($m_idx > 0) && [angband m_list set $m_idx ml]} {

			set r_idx [angband m_list set $m_idx r_idx]
			set friend [angband m_list info $m_idx friend]
			angband player health_who $m_idx
			angband player monster_race_idx $r_idx
			qegenerate <Track-health> [list w $m_idx f $friend]
			NSRecall::RecallMonster $r_idx

		# An object is here
		} elseif {$o_idx && [angband o_list set $o_idx marked]} {

			NSRecall::RecallObject cave $o_idx
		}
	}

	return
}

# NSMainWindow::Leave --
#
#	Handle the mouse leaving the Widget. Called as NSWidget(OOP,leaveCmd).
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	widgetId			OOP ID NSWidget.
#
# Results:
#	What happened.

proc NSMainWindow::Leave {oop widgetId} {

	# Unused: PROJECT_HINT
	if {0 && [angband inkey_flags] eq "INKEY_TARGET"} {

		# Show target grids at the cursor
		set y [Global cursor,y]
		set x [Global cursor,x]
		angband keypress &$y\n$x\n
		return
	}

	# Clear the statusbar prompt
	StatusText $oop ""

	# Hide show_cave_balloon tooltip
	BalloonHide $oop

	return
}

# NSMainWindow::BalloonInit --
#
#	Init the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::BalloonInit {oop} {

	set win [Global main,widget].balloon
	catch {destroy $win}
	toplevel $win -background black
	wm overrideredirect $win 1
	wm withdraw $win
if 1 {
	$win configure -borderwidth 1 -relief flat -background gray60
	text $win.text \
		-wrap none -font [Value font,autobar] \
		-borderwidth 0 -setgrid no -highlightthickness 0 \
		-padx 4 -pady 2 -background [Value listBG] -foreground White \
		-cursor ""
	pack $win.text -expand yes -fill both
} else {
	label $win.label \
		-relief flat \
		-background $::SystemInfoBackground -foreground black \
		-padx 2 -pady 0 -anchor w -justify left
	pack $win.label -side left -padx 1 -pady 1
}
	if {[Platform windows]} {
		wm attribute $win -alpha 0.85
	}
	if {[Platform unix]} {
		$win configure -cursor left_ptr
	}

	bind $win <Enter> "NSMainWindow::BalloonMotion $oop %X %Y"
	bind $win <Motion> "NSMainWindow::BalloonMotion $oop %X %Y"
	bind $win <ButtonPress> "focus [Window main]"

	Info $oop balloon,win $win

	return
}

# NSMainWindow::BalloonShow --
#
#	Init the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	text				List of {text color} to display.
#	x, y				Screen coords to display it
#	anchor				position window relative to x,y
#
# Results:
#	What happened.

proc NSMainWindow::BalloonShow {oop text x y {anchor nw}} {

	set win [Info $oop balloon,win]

if 1 {
	$win.text delete 1.0 end

	$win.text configure -font [Value font,choice]
	$win.text configure -background [Value listBG]

	set chunk 0
	foreach {string color} $text {
		if {$color ne ""} {
			$win.text insert end $string CHUNK$chunk
			$win.text tag configure CHUNK$chunk -foreground $color
		} else {
			$win.text insert end $string
		}
		incr chunk
	}

	set width 0
	for {set i 1} {$i <= [$win.text count -lines 1.0 end]} {incr i} {
		set lineWidth [$win.text count -update -xpixels $i.0 "$i.0 lineend"]
		if {$lineWidth > $width} {
			set width $lineWidth
		}
	}
	incr width [expr {[$win.text cget -padx] * 2}]
	incr width 2
	set height [$win.text count -update -ypixels 1.0 end]
	incr height [expr {[$win.text cget -pady] * 2}]
	incr height 2

} else {
	$win.label configure -text $text

	set width [winfo reqwidth $win.label]
	incr width 2
}
	switch -- $anchor {
		n {
			set x [expr {$x - $width / 2}]
		}
	}

	# Not too far left
	if {$x < 0} {set x 0}

	# Not too far right
	set screenWidth [ScreenWidth $win]
	if {$x + $width > $screenWidth } {
		set x [expr {$screenWidth - $width}]
	}

if 1 {
	wm geometry $win ${width}x${height}+$x+$y

	# Display and raise, don't change the focus
	wm deiconify $win

	update
} else {
	# Move the window offscreen and show it so the window is updated
	# before showing it in the desired position.  I often see the previous
	# window image for a moment before the window looks current. 
	wm geometry $win ${width}x${height}+$screenWidth+$y

	# Display and raise, don't change the focus
	wm deiconify $win

	# Must be [update], not [update idletasks] else the window appearance
	# at this point is the same as it was before!
	update

	wm geometry $win +$x+$y
#	update idletasks
}
	if {[Platform unix]} {
		raise $win
	}

#	update

    return
}

# NSMainWindow::BalloonHide --
#
#	Hide the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::BalloonHide {oop} {

	set win [Info $oop balloon,win]

	if {[winfo ismapped $win]} {
		wm withdraw $win
	}

	return
}

# NSMainWindow::BalloonMotion --
#
#	<Motion> in the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	X, Y				Screen coords of mouse pointer.
#
# Results:
#	What happened.

proc NSMainWindow::BalloonMotion {oop X Y} {

	set widget [Global main,widget]
	
	set x [expr {$X - [winfo rootx $widget]}]
	set y [expr {$Y - [winfo rooty $widget]}]

	NSWidget::Motion [Global main,widgetId] $x $y {}

	return
}

# NSMainWindow::WidgetCenter --
#
#	Handle the main Widget's center changing.
#	Called as NSWidget(OOP,centerCmd).
#
# Arguments:
#	oop					OOP ID NSWidget.
#
# Results:
#	What happened.

proc NSMainWindow::WidgetCenter {oop widgetId y x} {

angband cave track -1 -1
BalloonHide $oop
NSWidget::Info [Global main,widgetId] examined ""
return

	set widget [Global main,widget]

	if {[NSUtils::HasCursor $widget]} {
		set px [winfo pointerx .]
		set py [winfo pointery .]

		set wx [expr {$px - [winfo rootx $widget]}]
		set wy [expr {$py - [winfo rooty $widget]}]

		scan [$widget caveyx $wx $wy] "%d %d" cy cx
		ExamineLocation $oop mouse $widgetId $cy $cx {}
	}

	return
}

# NSMainWindow::CaveToDirection --
#
#	Given cave location y,x, determine the direction key relative
#	to the player location.
#
# Arguments:
#	y					y cave location.
#	x					x cave location.
#
# Results:
#	Return "dir y x", where dir is key to move, y/x is adjacent cave location
#	character would move to.

proc NSMainWindow::CaveToDirection {y x} {

	global PYPX

	scan $PYPX "%d %d" py px

	if {$y < $py} {
		set yyy 789
		incr py -1
	} elseif {$y > $py} {
		set yyy 123
		incr py
	} else {
		set yyy 456
	}

	if {$x < $px} {
		set dirKey [string index $yyy 0]
		incr px -1
	} elseif {$x > $px} {
		set dirKey [string index $yyy 2]
		incr px
	} else {
		set dirKey [string index $yyy 1]
	}

	return "$dirKey $py $px"
}

# NSMainWindow::StatusText --
#
#	Displays text in the status bar.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::StatusText {oop text} {

	set label [Global main,statusBar]
	if {[string compare $text [$label cget -text]]} {
		$label configure -text $text
	}

	return
}

# NSMainWindow::DisplayDepth --
#
#	Displays the dungeon level in the Main Window's status bar.
#
# Arguments:
#	label					The label widget to display the depth in.
#	depth					Current depth.
#
# Results:
#	What happened.

proc NSMainWindow::DisplayDepth {label depth} {

	if {[variant ANGBANDTK]} {
		if {$depth == 0} {
			set depthStr [mc Town]
		} else {
			if {[Setting depth_in_feet]} {
				set depthStr [format [mc "%d feet"] [expr {$depth * 50}]]
			} else {
				set depthStr [format [mc "Level %d"] $depth]
			}
		}
	}
	if {[variant OANGBANDTK]} {
		if {$depth == 0} {
			set depthStr [mc Town]
		} else {
			if {[Setting depth_in_feet]} {
				if {[Setting use_metric]} {
					set depthStr [format [mc "%d m"] [expr {$depth * 15}]]
				} else {
					set depthStr [format [mc "%d feet"] [expr {$depth * 50}]]
				}
			} else {
				set depthStr [format [mc "Level %d"] $depth]
			}
		}
	}
	if {[variant KANGBANDTK]} {
		if {[angband player inside_arena]} {
			set depthStr [mc Arena]
		} elseif {[angband player inside_quest]} {
			set depthStr [mc Quest]
		} elseif {$depth == 0} {
			set depthStr [mc Town/Wild]
		} else {
			if {[angband setting set depth_in_feet]} {
				set depthStr [format [mc "%d feet"] [expr {$depth * 50}]]
			} else {
				set depthStr [format [mc "Level %d"] $depth]
			}
		}
	}
	if {[variant ZANGBANDTK]} {
		if {[angband player inside_arena]} {
			set depthStr [mc Arena]
		} elseif {[angband player inside_quest]} {
			set depthStr [mc Quest]
		} elseif {$depth == 0} {
			set depthStr [angband cave wild_name]
		} else {
			if {[angband setting set depth_in_feet]} {
				set depthStr [format [mc "%d feet"] [expr {$depth * 50}]]
			} else {
				set depthStr [format [mc "Level %d"] $depth]
			}
		}
	}
	$label configure -text $depthStr

	return
}

# NSMainWindow::Bind_Py_level --
#
#	Handle <Py-level> quasi-event.
#
# Arguments:
#	level					The new experience level.
#
# Results:
#	What happened.

proc NSMainWindow::Bind_Py_level {oop level} {

	if {$level != [Info $oop Py_level]} {
		NSStatus::SetStatusMessage [format [mc "Level %d"] $level] Level info
		Info $oop Py_level $level
	}

	return
}

# NSMainWindow::ButtonPress3 --
#
#	Do something when Button 3 is pressed in the main widget.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	x y					Coords in Widget (as returned by event).
#	X Y					Global coords (as returned by event).
#
# Results:
#	What happened.

proc NSMainWindow::ButtonPress3 {oop x y X Y} {

	if {[Global UI_Busy]} return

	set win [Info $oop win]

	set flags [angband inkey_flags]

	# Hide the show_cave_balloon tooltip
	BalloonHide $oop
	NSWidget::Info [Global main,widgetId] examined ""

	# Popup item list
	if {$flags eq "INKEY_ITEM"} {
		NSPopup::SelectItem $win $X $Y
#		NSRecall::PopupSelect_Item $win.context $X $Y

	# Popup spell list
	} elseif {$flags eq "INKEY_SPELL"} {
		NSPopup::SelectSpell $win $X $Y
#		NSBookMenu::PopupSelect $win.context $X $Y

	} elseif {$flags eq "INKEY_CMD"} {
		scan [NSWidget::PointToCave [Global main,widgetId] $x $y] "%d %d" y2 x2
		if {"$y2 $x2" eq [angband player position]} {
#			tk_popup [NSMenu::Info [Info $oop mbarId] menu] $X $Y
			PopupSelect_Action $win.context $X $Y
		} else {
			# Run
			MouseCommand $oop $x $y .
		}

	# Set target
	} elseif {$flags eq "INKEY_DIR"} {
		scan [NSWidget::PointToCave [Global main,widgetId] $x $y] "%d %d" y2 x2
		angband keypress *@$y2\n$x2\n
	}

	if {[variant KANGBANDTK]} {
		# Popup pet commands
		if {$flags eq "INKEY_CMD_PET"} {
			NSRecall::PopupSelect_CmdPet $win.context $X $Y
		}
	}

	if {[variant OANGBANDTK]} {
		if {$flags eq "INKEY_ELE_ATTACK"} {
			NSPopup::SelectEleAttack $win $X $Y
#			NSRecall::PopupSelect_EleAttack $win.context $X $Y
		}
	}

	if {[variant ZANGBANDTK]} {
		# Popup pet commands
		if {$flags eq "INKEY_CMD_PET"} {
			NSPopup::SelectPetCommand $win $X $Y
#			NSRecall::PopupSelect_CmdPet $win.context $X $Y

		# Popup Mindcraft power list
		} elseif {$flags eq "INKEY_MINDCRAFT"} {
			NSPopup::SelectMindcraft $win $X $Y
#			NSMindcraftMenu::PopupSelect $win.context $X $Y

		# Popup racial/mutation power list
		} elseif {$flags eq "INKEY_POWER"} {
			NSPopup::SelectPower $win $X $Y
#			NSRecall::PopupSelect_Power $win.context $X $Y
		}
	}

	return
}

# NSMainWindow::PopupSelect_Action --
#
#	Do something when Button 3 is pressed in the main widget.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#	x y					Global coords (as returned by event).
#
# Results:
#	What happened.

proc NSMainWindow::PopupSelect_Action {menu x y} {

	global PYPX

	$menu delete 0 end
	destroy {*}[winfo children $menu]

	angband cave info {*}$PYPX attrib

	set up [const FEAT_LESS]
	set down [const FEAT_MORE]
	if {[variant KANGBANDTK]} {
		lappend up [const FEAT_QUEST_UP] [const FEAT_QUEST_EXIT]
		lappend down [const FEAT_QUEST_DOWN] [const FEAT_QUEST_ENTER]
	}
	if {[variant ZANGBANDTK]} {
		lappend up [const FEAT_QUEST_UP]
		lappend down [const FEAT_QUEST_DOWN]
	}
	if {$attrib(f_idx) in $up} {
		$menu add command -label [mc "Go Up"] -command {DoUnderlyingCommand <}
	} elseif {$attrib(f_idx) in $down} {
		$menu add command -label [mc "Go Down"] -command {DoUnderlyingCommand >}
	} else {
		set match [angband floor find]
		if {[llength $match] == 1} {
			# TODO: use floor item
		}
	}

	$menu add command -label [mc "Rest as needed"] -command {DoUnderlyingCommand R&\n}
	$menu add command -label [mc "Rest for HP/SP"] -command {DoUnderlyingCommand R*\n}

	if {[$menu index end] ne "none"} {
		$menu add separator
	}
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSMainWindow::UpdateHealthWho --
#
#	Called as a qebind <Track-health> script. Hides/shows/updates the
#	Monster Health Bar.
#
# Arguments:
#	oop						OOP ID of NSMainWindow object.
#	m_idx					m_list[] index of the tracked monster.
#	friend					1 if m_idx if a pet.
#
# Results:
#	What happened.

proc NSMainWindow::UpdateHealthWho {oop m_idx friend} {

	variable Progress

	set widget [Global main,widget]

	if {[struct set player_type 0 image] > 0} {
		set m_idx 0
	}

	# Hide the bar if visible and not tracking
	if {$m_idx == 0} {
		if {$Progress(visible)} {
			$widget itemconfigure $Progress(barId) -visible no
			$widget itemconfigure $Progress(textId) -visible no
			set Progress(visible) 0
		}
		return
	}

	# Show the bar if hidden
	if {!$Progress(visible)} {
		$widget itemconfigure $Progress(barId) -visible yes
		$widget itemconfigure $Progress(textId) -visible yes
		set Progress(visible) 1
	}

	if {[variant KANGBANDTK ZANGBANDTK]} { 
		# Change colors depending on friend status
		if {$friend != $Progress(friend)} {
			SetProgressColors $friend
			set Progress(friend) $friend
		}
	}

	# Set the progress
	array set attrib [angband m_list set $m_idx]
	if {$attrib(ml)} {
		set curhp [expr {($attrib(hp) > 0) ? $attrib(hp) : 0}]
		set current [expr {int((double($curhp) / $attrib(maxhp)) * 100)}]
	} else {
		set current 0
	}
	if {$current != $Progress(current)} {
		$widget itemconfigure $Progress(barId) \
			-current $current -maximum 100
		set Progress(current) $current
	}

	# Set the name
	if {$attrib(r_idx) != $Progress(r_idx)} {
		set name [angband r_info info $attrib(r_idx) name]
		$widget itemconfigure $Progress(textId) -text $name
		set Progress(r_idx) $attrib(r_idx)
	}

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

# NSMainWindow::SetProgressColors --
#
#	Called when the monster health bar goes from friend to non-friend mode.
#	Updates the monster health bar colors as appropriate.
#
# Arguments:
#	name1					See "trace" manual entry.
#	name2					See "trace" manual entry.
#	op						See "trace" manual entry.
#
# Results:
#	What happened.

proc NSMainWindow::SetProgressColors {friend} {

	variable Progress

	set widget [Global main,widget]

	if {$friend} {
		set data {
			friendBarDone done
			friendBarToDo todo
			friendBarBL bevellight
			friendBarBD beveldark
			friendBarDone2 done2
			friendBarToDo2 todo2
			friendBarBL2 bevellight2
			friendBarBD2 beveldark2
		}
	} else {
		set data {
			healthBarDone done
			healthBarToDo todo
			healthBarBL bevellight
			healthBarBD beveldark
			healthBarDone2 done2
			healthBarToDo2 todo2
			healthBarBL2 bevellight2
			healthBarBD2 beveldark2
		}
	}

	foreach {name varname} $data {
		set $varname [Value $name]
	}

	$widget itemconfigure $Progress(barId) \
		-done $done -done2 $done2 -todo $todo -todo2 $todo2 \
		-bevellight $bevellight -bevellight2 $bevellight2 \
		-beveldark $beveldark -beveldark2 $beveldark2

	return
}

# KANGBANDTK, ZANGBANDTK
}

# NSMainWindow::SelectWindow --
#
#	Make a window the frontmost active window.
#
# Arguments:
#	window				Index into Windows[] (inventory, book, etc)
#
# Results:
#	What happened.

proc NSMainWindow::SelectWindow {window} {

	if {[info exists NSWindowManager::Priv($window,win)]} {
		NSWindowManager::Display $window
		return
	}

	WindowBringToFront [Window $window]

	return
}

# NSMainWindow::WithdrawWindow --
#
#	Withdraw a window.
#
# Arguments:
#	window					Index into Windows[] (inventory, book, etc)
#
# Results:
#	What happened.

proc NSMainWindow::WithdrawWindow {window} {

	wm withdraw [Window $window]

	return
}

# NSMainWindow::Display --
#
#	Remove current window (if any), and select given window.
#
# Arguments:
#	window					Index into Windows[] (inventory, book, etc)
#
# Results:
#	What happened.

proc NSMainWindow::Display {window} {

	global Display

	if {$Display(window) ne "none" &&
		$Display(window) ne $window} {
		WithdrawWindow $Display(window)
	}

	SelectWindow $window

	set Display(window) $window

	return
}

# NSMainWindow::ToggleMusic --
#
#	Turn music on or off.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::ToggleMusic {oop} {

	Value music,play [Global music,play]

	if {[Value music,play]} {
		if {![music issong]} {
			PlayRandomSong
		}
	} else {
		if {[music issong]} {
			music stop
			qegenerate <Music-stop>
		}
	}

	return
}

# NSMainWindow::ToggleSound --
#
#	Calls "angband setting set use_sound" with the value of the
#	NSMainWindow(OOP-ID,useSound) variable (which is the variable for
#	a menu checkbutton entry).
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMainWindow::ToggleSound {oop} {

	angband setting set use_sound [Info $oop useSound]

	Value use_sound [Info $oop useSound]

	if {![Value use_sound]} {

		# Stop all sounds
		angband sound stop
	}

	return
}

# NSMainWindow::PositionChanged --
#
#	Called as a qebind <Position> script. Update the Main Window
#	when the character's position changes. Handles the "scroll_follow"
#	and "disturb_panel" options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::PositionChanged {widget y x} {

	global PYPX

if {$PYPX eq "$y $x"} return

	# Option: Keep character centered in the display
	if {[Value scroll_follow]} {

		if {![NSUtils::HasCursor [Global micromap,widget]]} {
			NSWidget::SetCenter [Global main,widgetId] $y $x
		}
		Global main,widget,center "$y $x"

	# Scroll when character crosses the edges of the display
	} else {
		scan [$widget center] "%d %d" oy ox
		scan [$widget bounds] "%d %d %d %d" y1 x1 y2 x2
		NSWidget::Size [Global main,widgetId] height width

		set ny $y
		set yscroll [ClipCenter ny $oy [angband cave height] $height]

		set nx $x
		set xscroll [ClipCenter nx $ox [angband cave width] $width]

		# Center the widget if needed
		if {$xscroll || $yscroll} {
			scan $PYPX "%d %d" opy opx
			if {abs($y - $opy) > 1 || abs($x - $opx) > 1} {
				set ny $y
				set nx $x
			}
			if {[angband setting set disturb_panel]} {
				angband player disturb
			}
		}

		if {![NSUtils::HasCursor [Global micromap,widget]]} {
			NSWidget::SetCenter [Global main,widgetId] $ny $nx
		}
		Global main,widget,center "$ny $nx"
	}

	# This global is read in various places
	set PYPX "$y $x"

	# Option: Recall objects under the character
	variable tracking
	variable trackStepping
	if {!$tracking || $trackStepping} {
		angband cave info $y $x attrib
		if {$attrib(o_idx)} {
			ExamineLocation [Global main,oop] position [Global main,widgetId] $y $x {}
		}
	}

	return
}

# FlashCanvasText --
#
#	Configure the fill color of a canvas item, then do it again later.
#
# Arguments:
#	canvas					Canvas widget the item is in.
#	tagOrId					The canvas item ID to manipulate.
#	color					The fill color.
#	num						Number of times to flash it.
#
# Results:
#	What happened.

global FlashCanvas

proc FlashCanvasTextAux {canvas tagOrId} {

	global FlashCanvas

	set num $FlashCanvas($canvas,$tagOrId,num)
	if {$num & 1} {
		set fill $FlashCanvas($canvas,$tagOrId,colorOff)
	} else {
		set fill $FlashCanvas($canvas,$tagOrId,colorOn)
	}
	$canvas itemconfigure $tagOrId -fill $fill

	incr num -1
	set FlashCanvas($canvas,$tagOrId,num) $num

	if {$num} {
		set id [after 250 "FlashCanvasTextAux $canvas $tagOrId"]
		set FlashCanvas($canvas,$tagOrId,afterId) $id
	} else {
		unset FlashCanvas($canvas,$tagOrId,afterId)
	}

	return
}

proc FlashCanvasText {canvas tagOrId colorOn colorOff num} {

	global FlashCanvas

	# Never set more than one "after" command for an item
	if {[info exists FlashCanvas($canvas,$tagOrId,afterId)]} {
		set id $FlashCanvas($canvas,$tagOrId,afterId)
		after cancel $id
	}

	set FlashCanvas($canvas,$tagOrId,colorOn) $colorOn
	set FlashCanvas($canvas,$tagOrId,colorOff) $colorOff
	set FlashCanvas($canvas,$tagOrId,num) $num

	FlashCanvasTextAux $canvas $tagOrId

	return
}

# FlashCanvasTextFill --
#
#	Returns the fill color for an canvas item. This routine should be
#	called if a canvas item may be "flashing".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FlashCanvasTextFill {canvas tagOrId} {

	global FlashCanvas

	if {[info exists FlashCanvas($canvas,$tagOrId,afterId)]} {
		return $FlashCanvas($canvas,$tagOrId,colorOff)
	} else {
		return [$canvas itemcget $tagOrId -fill]
	}
}

# DoCommandIfAllowed --
#
#	Feeds a string of bytes to the Term, but only if INKEY_CMD is set.
#
# Arguments:
#	string					String argument to "angband keypress"
#
# Results:
#	What happened.

proc DoCommandIfAllowed {string} {

	# Check if game is waiting for a command
	if {[angband inkey_flags] ne "INKEY_CMD"} return

	# Feed the Term
	angband keypress $string

	return
}

# DoUnderlyingCommand --
#
#	Feeds the string to "angband keypress", but prepends a slash
#	to bypass keymaps. This only works if request_command() is being
#	called to handle the \ escape character. INKEY_CMD is actually set
# 	when examining the inventory or equipment, and when browsing a book,
#	in which case this cannot be used.
#
# Arguments:
#	string					String argument to "angband keypress"
#
# Results:
#	What happened.

proc DoUnderlyingCommand {string} {

	# Check if game is waiting for a command
	if {[angband inkey_flags] ne "INKEY_CMD"} return

	# Feed the Term
	angband keypress \\$string

	return
}

# DoKeymapCmd --
#
#	Maps the given command char to the underlying command and calls
#	"angband keypress" with it. Some command chars can be represented
#	by the X11 keysym.
#
# Arguments:
#	prefix				Misc characters to prepend to command char
#	command				The underlying command char
#	suffix				Misc characters to append to command char
#
# Results:
#	What happened.

proc DoKeymapCmd {prefix command suffix} {

	switch -- $command {
		backslash {set command \\}
		braceleft {set command \{}
		braceright {set command \}}
		bracketleft {set command \[}
		bracketright {set command \]}
		quotedbl {set command \" ; #"}
	}
	set keymaps [angband keymap find $command]
	if {[llength $keymaps]} {
		angband keypress $prefix[lindex $keymaps 0]$suffix
	} else {
		bell
	}

	return
}

# Note: Setting a delay of 0 results in running after the mouse is
# released; setting a delay of 1 or more prevents this
proc ConfigureMouse {} {

	set win .mouse
	toplevel $win
	wm title $win "Mouse Settings"

	set scale $win.speed
	scale $scale \
		-orient horizontal -label "Tracking Delay" \
		-width 15 -sliderlength 20 -length 200 -from 0 -to 200 \
		-command "set ::NSMainWindow::trackDelay"

	$scale set $::NSMainWindow::trackDelay

	pack $scale


	set clicks [clock clicks]
	set text [time {after 1} 100]
	set diff [expr {[clock clicks] - $clicks}]

	Debug $text
	Debug "1 ms = [expr {$diff / 100}] clicks"

	return
}

proc TestRedrawSpeed {} {
	set widget [Global main,widget]
	set clicks [clock clicks]
	set text [time {$widget wipe ; update idletasks} 100]
	set diff [expr {[clock clicks] - $clicks}]
	Debug "TestRedrawSpeed: 100 redraws in $diff clicks"

	return
}

# BigMapExamine --
#
#	Called as NSWidget(OOP,examineCmd). Display a description of what the
#	character sees at a given cave location. Center the Widget in the
#	MicroMap window at the location also.
#
# Arguments:
#	oop					OOP ID of NSWidget.
#	y					y cave location.
#	x					x cave location.
#
# Results:
#	What happened.

proc NSMainWindow::BigMapExamine {oop widgetId y x modifiers} {

	# Center the detail widget at the given location
	[Global mapdetail,widget] center $y $x

	# Position the cursor in the detail widget
	[Global mapdetail,widget] itemconfigure [Global mapdetail,cursor] -y $y -x $x

	# Describe the location
	set desc [angband cave examine $y $x]

	# Pop up a balloon over interesting grids.
	set widget [Global mapdetail,widget]
	if {$desc ne ""} {
		set balloon [list $desc ""]
		if {[string match "*a pile of * items*" $desc]} {
			lappend balloon \n {}
			lappend balloon {*}[ListFloorItems $y $x]
		} else {
			angband cave info $y $x attrib
			set m_idx $attrib(m_idx)
			if {($m_idx > 0) && [angband m_list set $m_idx ml]} {
				if {[regexp {(.+) \((.+)\)} $desc v0 v1 v2]} {
					set balloon [list "$v1\n" "" $v2 [Value TERM_L_WHITE]]
				}
			}
		}
		scan [$widget bbox $y $x] "%s %s %s %s" left top right bottom
		set x2 [expr {[winfo rootx $widget] + $left + ($right - $left) / 2}]
		set y2 [expr {[winfo rooty $widget] + $bottom + 2}]
		NSMicroMapWindow::BalloonShow [Global micromap,oop] $balloon $x2 $y2 n
	} else {
		NSMicroMapWindow::BalloonHide [Global micromap,oop]
	}

	# Describe what is seen
	NSMainWindow::ExamineLocation $oop bigmap $widgetId $y $x {}

	return
}

# BigMapLeave --
#
#	Called as NSWidget(OOP,leaveCmd).
#
# Arguments:
#	oop					OOP ID of NSWidget.
#	y					y cave location.
#	x					x cave location.
#
# Results:
#	What happened.

proc NSMainWindow::BigMapLeave {oop widgetId} {

	[Global mapdetail,widget] center -100 -100
	NSMicroMapWindow::BalloonHide [Global micromap,oop]

	StatusText $oop {}

	return
}

# NSMainWindow::ContextMenu_StatusBar --
#
#	Pop up a context menu in the StatusBar to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::ContextMenu_StatusBar {menu x y} {

	$menu delete 0 end

	$menu add command -label [mc "Set Font"] \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font statusBar"
	$menu add command -label "Set Color" \
		-command {
			set color [tk_chooseColor -parent [Window main] \
				-initialcolor [Value main,statusbar,color]]
			if {$color ne ""} {
				Value main,statusbar,color $color
				[Global main,statusBar] configure -foreground $color
			}
		}
	$menu add command -label "Set Autobar Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font autobar"
	$menu add separator
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSMainWindow::TransientToMain --
#
#	Makes the given window a transient of the Main window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::TransientToMain {win} {

	wm transient $win [Window main]

	# Starting in 8.4b1 [wm transient] doesn't force "tool window" frame
	if {[Platform windows]} {
		wm attribute $win -toolwindow yes
	}

	return
}

# NSMainWindow::AttachMisc --
#
#	Attach the Misc Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::AttachMisc {oop} {

	NSMiscCanvas::Arrange [Info $oop misc,oop]
	grid [Info $oop win].misc

	qeconfigure [Info $oop misc,canvas] <Stat> -active yes
	foreach detail {armor_class exp gold level} {
		qeconfigure [Info $oop misc,canvas] <Py-$detail> -active yes
	}

	return
}

# NSMainWindow::DetachMisc --
#
#	Detach the Misc Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::DetachMisc {oop} {

	grid remove [Info $oop win].misc

	qeconfigure [Info $oop misc,canvas] <Stat> -active no
	foreach detail {armor_class exp gold level} {
		qeconfigure [Info $oop misc,canvas] <Py-$detail> -active no
	}

	return
}


# NSMainWindow::AttachProgress --
#
#	Attach the Progress Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::AttachProgress {oop} {

	set width [[Info $oop misc,canvas] cget -width]
	[Info $oop progress,canvas] configure -width $width
	NSProgressCanvas::Arrange [Info $oop progress,oop]
	scan [[Info $oop misc,canvas] bbox all] "%d %d %d %d" left top right bottom
	place [Info $oop progress,canvas] -x 0 -y $bottom

	foreach attrib {hitpoints mana food} {
		qeconfigure [Info $oop progress,canvas] <Py-$attrib> -active yes
	}
	qeconfigure [Info $oop progress,canvas] <Track-inventory> -active yes

	return
}

# NSMainWindow::DetachProgress --
#
#	Detach the Progress Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::DetachProgress {oop} {

	place forget [Info $oop progress,canvas]

	foreach attrib {hitpoints mana food} {
		qeconfigure [Info $oop progress,canvas] <Py-$attrib> -active no
	}
	qeconfigure [Info $oop progress,canvas] <Track-inventory> -active no

	return
}


# NSMainWindow::HPBlinker_Init --
#
#	Initialize low-hitpoint blinker.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::HPBlinker_Init {oop} {

return

	set widget [Global main,widget]

	Info $oop hpblink,itemId [$widget create rectangle -color red -outside yes]

	qebind HPBlinker <Py-hitpoints> \
		"NSMainWindow::HPBlinker_BindHP $oop %c %m %f"

	qebind HPBlinker <Position> \
		"NSMainWindow::HPBlinker_Position $oop %y %x"
	qeconfigure HPBlinker <Position> -active no

	Info $oop hpblink,afterId ""

	return
}

proc NSMainWindow::HPBlinker_BindHP {oop cur max frac} {

	set hitpoint_warn [Setting hitpoint_warn]
	if {$cur > ($max * $hitpoint_warn) / 10} {
		if {[Info $oop hpblink,afterId] ne ""} {
			HPBlinker_Update $oop 0
		}
		return
	}

	if {[Info $oop hpblink,afterId] ne ""} return

	qeconfigure HPBlinker <Position> -active yes
	HPBlinker_Position $oop {*}[angband player position]
	HPBlinker_Update $oop 3

	return
}

proc NSMainWindow::HPBlinker_Update {oop width} {

	set widget [Global main,widget]
	set itemId [Info $oop hpblink,itemId]

	if {$width <= 0} {
		$widget itemconfigure $itemId -visible no
		after cancel [Info $oop hpblink,afterId]
		Info $oop hpblink,afterId ""
		qeconfigure HPBlinker <Position> -active no
		return
	}

	$widget itemconfigure $itemId -visible yes -linewidth $width

	Info $oop hpblink,afterId [after 250 NSMainWindow::HPBlinker_Update $oop [expr {$width - 1}]]

	return
}

proc NSMainWindow::HPBlinker_Position {oop y x} {

	set widget [Global main,widget]
	set itemId [Info $oop hpblink,itemId]

	$widget itemconfigure $itemId -x1 $x -y1 $y -x2 $x -y2 $y

	return
}
