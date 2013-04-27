# File: main-window.tcl

# Purpose: the Main Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMainWindow {

	variable Priv
	
	variable tracking 0
	variable trackId 0
	variable trackStepping 0
	variable trackX
	variable trackY

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
	
	NSModule::LoadIfNeeded NSMap
	NSModule::LoadIfNeeded NSWidget
	NSModule::LoadIfNeeded NSTerm

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

	# Window positions
	Info $oop window,autosave [Value window,autosave]

	#
	# Global access
	#

	Window main [Info $oop win]
	Global main,oop $oop

	InitAutobar $oop

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
	wm title $win "Main - ZAngband"

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

	bind $win.statusBar.center <Enter> "
		%W configure -foreground gray60
		NSMainWindow::StatusText $oop {Click to recenter the display.}
	"
	bind $win.statusBar.center <Leave> "
		%W configure -foreground White
		NSMainWindow::StatusText $oop {}
	"

	# Update ourself when the font,statusBar value changes
	NSValueManager::AddClient font,statusBar \
		"NSMainWindow::ValueChanged_font_statusBar"

	#
	# Message line when Message Window is closed
	#

	set frame $win.message
	frame $frame -background black -borderwidth 1 -relief sunken

	#
	# Misc info when Misc Window is closed
	#

	set frame $win.misc
	frame $frame -background black -borderwidth 1 -relief sunken

	#
	# Main widget
	#

	# Black background affects border color
	frame $win.mainframe \
		-borderwidth 1 -relief sunken -background Black

	# Get the icon dimensions
	set gsize [icon size]

	# This is a large monitor
	if {[winfo screenwidth .] >= 800} {
		set width [expr {15 * 32}]
		set height [expr {11 * 32}]

	# This is a small monitor
	} else {
		set width [expr {13 * 32}]
		set height [expr {9 * 32}]
	}
	
	set widgetId [NSObject::New NSWidget $win.mainframe \
		$width $height $gsize $gsize]
	NSWidget::Info $widgetId leaveCmd NSMainWindow::Leave
	set widget [NSWidget::Info $widgetId widget]

	bind $widget <ButtonPress-1> "NSMainWindow::TrackPress $oop %x %y"
	bind $widget <Button1-Motion> "NSMainWindow::TrackMotion $oop %x %y"
	bind $widget <ButtonRelease-1> "NSMainWindow::TrackRelease $oop"

	bind $widget <Control-ButtonPress-1> "NSMainWindow::MouseCommand $oop %x %y +"
	bind $widget <Shift-ButtonPress-1> "NSMainWindow::MouseCommand $oop %x %y ."

	bind $widget <Control-Shift-ButtonPress-1> "
		NSWidget::Info $widgetId track,x %x
		NSWidget::Info $widgetId track,y %y
		NSWidget::Info $widgetId track,mouseMoved 0
	"
	bind $widget <Control-Shift-Button1-Motion> \
		"NSWidget::TrackOnce $widgetId %x %y"

	bind $widget <ButtonPress-3> \
		"NSMainWindow::ButtonPress3 $oop %x %y %X %Y"
	bind $widget <Control-ButtonPress-3> \
		"NSRecall::PopupSelect_Use $win.context %X %Y"

	# When the pointer leaves the Main Window Widget, we clear the
	# statusbar text, in addition to the behaviour defined by the
	# NSWidget module.
#	bind $widget <Leave> "+NSMainWindow::StatusText $oop {}"

	# Remember the center of the Main Window Widget.
	Global main,widget,center [angband player position]

	variable HT ""
	
	# The "big map", the map of the entire cave with scroll bars.
	# The user can change the scale via a popup menu, so we save
	# the desired scale.
	set scale [Value bigmap,scale]
	set width [expr $width - 16]
	set height [expr $height - 16]
	set mapId [NSObject::New NSMap $widget $width $height $scale $scale]
	set widget2 [NSMap::Info $mapId widget]

	NSMap::Info $mapId scaleCmd \
		"Value bigmap,scale \[NSWidget::Info [NSMap::Info $mapId widgetId] scale]"

	bind $widget2 <Leave> {+
		[Global mapdetail,widget] center -100 -100
		NSMainWindow::StatusText [Global main,oop] {}
	}

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
	
	# Create terms window
	
	set width [expr {16 * 80}]
	set height [expr {16 * 24}]
	
	set term .term
	
	toplevel $term
	
	wm title $term "Terminal"
	
	wm geometry $term +$width+$height
	wm minsize $term $width $height
	
	Term_KeyPress_Bind $term
	
	# Do stuff when window closes
	wm protocol $term WM_DELETE_WINDOW "NSTerm::Close $oop"
	
	set termId [NSObject::New NSTerm .term $width $height 16 16]
			
	update
	
	

	return
}

proc NSMainWindow::InitAutobar {oop} {

	set statusBar [Global main,statusBar]

	bind $statusBar <Enter> \
		"NSMainWindow::ShowAutobar $oop"

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
		-menu MENU_FILE -label "File" -underline 0 -identifier M_FILE

	set entries {}
	lappend entries [list -type command -label "Save" -identifier E_GAME_SAVE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Quit With Save" -identifier E_GAME_EXIT]
	lappend entries [list -type command -label "Quit" -identifier E_GAME_ABORT]

	NSMenu::MenuInsertEntries $mbarId -end MENU_FILE $entries

	#
	# Inven Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_INVEN]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_INVEN -label "Inven" -underline 0 -identifier M_INVEN

	# Magic Menu
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_MAGIC]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Activate" -identifier E_MAGIC_ACTIVATE]
	lappend entries [list -type command -label "Aim Wand" -identifier E_MAGIC_WAND]
	lappend entries [list -type command -label "Drink Potion" -identifier E_MAGIC_POTION]
	lappend entries [list -type command -label "Read Scroll" -identifier E_MAGIC_SCROLL]
	lappend entries [list -type command -label "Use Staff" -identifier E_MAGIC_STAFF]
	lappend entries [list -type command -label "Zap Rod" -identifier E_MAGIC_ROD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Browse" -identifier E_MAGIC_BROWSE]
	lappend entries [list -type command -label "Study" -identifier E_MAGIC_STUDY]

	NSMenu::MenuInsertEntries $mbarId -end MENU_MAGIC $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_USE]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Destroy" -identifier E_USE_DESTROY]
	lappend entries [list -type command -label "Drop" -identifier E_USE_DROP]
	lappend entries [list -type command -label "Pick Up" -identifier E_USE_PICKUP]
	lappend entries [list -type command -label "Take Off" -identifier E_USE_TAKEOFF]
	lappend entries [list -type command -label "Wear/Wield" -identifier E_USE_WIELD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Eat Food" -identifier E_USE_FOOD]
	lappend entries [list -type command -label "Fire Missle" -identifier E_USE_MISSILE]
	lappend entries [list -type command -label "Fuel Light" -identifier E_USE_FUEL]
	lappend entries [list -type command -label "Jam Spike" -identifier E_USE_SPIKE]
	lappend entries [list -type command -label "Throw" -identifier E_USE_THROW]

	NSMenu::MenuInsertEntries $mbarId -end MENU_USE $entries

	set entries {}
	lappend entries [list -type command -label "Equipment" -identifier E_INVEN_EQUIPMENT]
	lappend entries [list -type command -label "Inventory" -identifier E_INVEN_INVENTORY]
	lappend entries [list -type separator]
 	lappend entries [list -type cascade -menu MENU_MAGIC -label "Magic" -identifier M_MAGIC]
	lappend entries [list -type cascade -menu MENU_USE -label "Use" -identifier M_USE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Inspect" -identifier E_INVEN_INSPECT]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Inscribe" -identifier E_INVEN_INSCRIBE]
	lappend entries [list -type command -label "Uninscribe" -identifier E_INVEN_UNINSCRIBE]

	NSMenu::MenuInsertEntries $mbarId -end MENU_INVEN $entries

	#
	# Action Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_ACTION -label "Action" -underline 0 -identifier M_ACTION

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_ALTER]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Alter" -identifier E_ACTION_ALTER]
	lappend entries [list -type command -label "Bash" -identifier E_ACTION_BASH]
	lappend entries [list -type command -label "Close" -identifier E_ACTION_CLOSE]
	lappend entries [list -type command -label "Disarm" -identifier E_ACTION_DISARM]
	lappend entries [list -type command -label "Open" -identifier E_ACTION_OPEN]
	lappend entries [list -type command -label "Tunnel" -identifier E_ACTION_TUNNEL]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_ALTER $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_LOOKING]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Look" -identifier E_ACTION_LOOK]
	lappend entries [list -type command -label "Map" -identifier E_ACTION_MAP]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_LOOKING $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_RESTING]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Rest" -identifier E_ACTION_REST]
	lappend entries [list -type command -label "Stay (With Pickup)" -identifier E_ACTION_STAY]
	lappend entries [list -type command -label "Stay" -identifier E_ACTION_STAY_TOGGLE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_RESTING $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_SEARCHING]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Search" -identifier E_ACTION_SEARCH]
	lappend entries [list -type command -label "Search Mode" -identifier E_ACTION_SEARCH_MODE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_SEARCHING $entries

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_ACTION_MOVEMENT]
	NSMenu::Info $menuId setupCmd "NSMainWindow::MenuSetupCmd $oop"
	set entries {}
	lappend entries [list -type command -label "Go Down" -identifier E_ACTION_DOWN]
	lappend entries [list -type command -label "Go Up" -identifier E_ACTION_UP]
	lappend entries [list -type command -label "Run" -identifier E_ACTION_RUN]
	lappend entries [list -type command -label "Walk (With Pickup)" -identifier E_ACTION_WALK]
	lappend entries [list -type command -label "Walk" -identifier E_ACTION_WALK_TOGGLE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION_MOVEMENT $entries

	set entries {}
 	lappend entries [list -type cascade -menu MENU_ACTION_ALTER -label "Alter" -identifier M_ACTION_ALTER]
 	lappend entries [list -type cascade -menu MENU_ACTION_LOOKING -label "Looking" -identifier M_ACTION_LOOKING]
 	lappend entries [list -type cascade -menu MENU_ACTION_MOVEMENT -label "Movement" -identifier M_ACTION_MOVEMENT]
 	lappend entries [list -type cascade -menu MENU_ACTION_RESTING -label "Resting" -identifier M_ACTION_RESTING]
 	lappend entries [list -type cascade -menu MENU_ACTION_SEARCHING -label "Searching" -identifier M_ACTION_SEARCHING]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Note" -identifier E_ACTION_NOTE]
	lappend entries [list -type command -label "Repeat" -identifier E_ACTION_REPEAT]
	lappend entries [list -type command -label "Target" -identifier E_ACTION_TARGET]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Pets" -identifier E_ACTION_PETS]
	lappend entries [list -type command -label "Use Power" -identifier E_ACTION_POWER]

	NSMenu::MenuInsertEntries $mbarId -end MENU_ACTION $entries

	#
	# Other Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_OTHER
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_OTHER -label "Other" -underline 0 -identifier M_OTHER

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_PREFERENCES
	set entries {}
	lappend entries [list -type command -label "Font" -identifier E_PREF_FONT]
	lappend entries [list -type command -label "Options" -identifier E_PREF_OPTIONS]
	NSMenu::MenuInsertEntries $mbarId -end MENU_PREFERENCES $entries

	set entries {}
	lappend entries [list -type command -label "Character Info" -identifier E_OTHER_INFO]
	lappend entries [list -type command -label "Feeling" -identifier E_OTHER_FEELING]
	lappend entries [list -type command -label "Knowledge" -identifier E_OTHER_KNOWLEDGE]
	lappend entries [list -type command -label "Message History" -identifier E_OTHER_MESSAGES]
	lappend entries [list -type cascade -menu MENU_PREFERENCES -label "Preferences" -identifier M_PREFERENCES]
	lappend entries [list -type command -label "Quest Status" -identifier E_OTHER_QUEST]
	lappend entries [list -type command -label "Time Of Day" -identifier E_OTHER_TIME]

	NSMenu::MenuInsertEntries $mbarId -end MENU_OTHER $entries

	#
	# Window Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_WINDOW
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_WINDOW -label "Window" -underline 0 -identifier M_WINDOW

	set entries {}
	lappend entries [list -type command -label "Arrange Windows..." -identifier E_WINDOW_DEFPOS]
	lappend entries [list -type command -label "Maximize Windows..." -identifier E_WINDOW_MAXIMIZE]
		lappend entries [list -type separator]
	lappend entries [list -type command -label "Save Window Positions" -identifier E_WINDOW_SAVEPOS]
	lappend entries [list -type command -label "Load Window Positions" -identifier E_WINDOW_LOADPOS]
	lappend entries [list -type checkbutton -label "AutoSave Positions" \
		-variable ::NSMainWindow($oop,window,autosave) -identifier E_WINDOW_AUTOSAVE]
	if {[file exists [PathTk choice-window.tcl]]} {
		Info $oop choiceWindow [Value choicewindow,show]
		lappend entries [list -type separator]
		lappend entries [list -type checkbutton -label "Choice Window" \
			-variable ::NSMainWindow($oop,choiceWindow) -identifier E_CHOICEWINDOW]
	}
	Info $oop messageWindow [Value message,float]
	lappend entries [list -type checkbutton -label "Message Window" \
		-variable ::NSMainWindow($oop,messageWindow) -identifier E_WINDOW_MESSAGE]
	Info $oop messagesWindow 0
	lappend entries [list -type checkbutton -label "Messages Window" \
		-variable ::NSMainWindow($oop,messagesWindow) -identifier E_WINDOW_MESSAGES]
	Info $oop miscWindow [Value misc,float]
	lappend entries [list -type checkbutton -label "Misc Window" \
		-variable ::NSMainWindow($oop,miscWindow) -identifier E_WINDOW_MISC]
if 0 {
	lappend entries [list -type checkbutton -label "Progress Window" \
		-variable ::NSMainWindow($oop,progressWindow) \
		-identifier E_WINDOW_PROGRESS]
}
	Info $oop recallWindow [Value recall,show]
	lappend entries [list -type checkbutton -label "Recall Window" \
		-variable ::NSMainWindow($oop,recallWindow) -identifier E_WINDOW_RECALL]
	NSMenu::MenuInsertEntries $mbarId -end MENU_WINDOW $entries

	#
	# Help Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_HELP
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_HELP -label "Help" -underline 0 -identifier M_HELP

	set entries {}
	lappend entries [list -type command -label "Help" -identifier E_HELP]
	lappend entries [list -type command -label "Tips" -identifier E_TIPS]
	lappend entries [list -type separator]
	lappend entries [list -type command \
		-label "About ZAngband..." -identifier E_ABOUT]

	NSMenu::MenuInsertEntries $mbarId -end MENU_HELP $entries
	
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

	lappend identList E_WINDOW_SAVEPOS E_WINDOW_DEFPOS \
		E_WINDOW_LOADPOS E_WINDOW_AUTOSAVE E_WINDOW_MAXIMIZE E_ABOUT E_TIPS
	lappend identList M_PREFERENCES E_PREF_FONT

	lappend identList E_CHOICEWINDOW E_WINDOW_MESSAGE E_WINDOW_MESSAGES \
		E_WINDOW_MISC E_WINDOW_RECALL
	if {[info exists Windows(choice)]} {
		Info $oop choiceWindow [winfo ismapped [Window choice]]
	}
	Info $oop messageWindow [winfo ismapped [Window message]]
	if {[info exists Windows(message2)]} {
		Info $oop messagesWindow [winfo ismapped [Window message2]]
	}
	Info $oop miscWindow [winfo ismapped [Window misc]]
	Info $oop recallWindow [winfo ismapped [Window recall]]

	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		lappend identList E_GAME_SAVE E_GAME_EXIT E_OTHER_FEELING \
			E_OTHER_INFO E_OTHER_KNOWLEDGE \
			E_OTHER_MESSAGES E_PREF_OPTIONS E_HELP \
			E_OTHER_QUEST E_OTHER_TIME
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

	if {[string compare [angband inkey_flags] INKEY_CMD]} {
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
		E_GAME_ABORT {QuitNoSave}

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
		E_ACTION_LOOK {DoUnderlyingCommand l}
		E_ACTION_MAP {DoUnderlyingCommand M}
		E_ACTION_NOTE {DoUnderlyingCommand :}
		E_ACTION_SHAPE {DoUnderlyingCommand \]}
		E_ACTION_PETS {DoUnderlyingCommand p}
		E_ACTION_POWER {DoUnderlyingCommand U}
		E_ACTION_REPEAT {DoUnderlyingCommand n}
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
		
		E_PREF_FONT {
			NSModule::LoadIfNeeded NSFont
			NSWindowManager::Display font
		}
		E_PREF_OPTIONS {DoUnderlyingCommand =}

		E_OTHER_INFO {DoUnderlyingCommand C}
		E_OTHER_FEELING {DoUnderlyingCommand ^F}
		E_OTHER_KNOWLEDGE {DoUnderlyingCommand ~}
		E_OTHER_MESSAGES {DoUnderlyingCommand ^p}
		E_OTHER_QUEST {DoUnderlyingCommand ^Q}
		E_OTHER_TIME {DoUnderlyingCommand ^T}

		E_WINDOW_DEFPOS {
			set title "dialog-title-defpos"
			set message "dialog-msg-defpos"
			set answer [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title $title -message $message]
			if {[string equal $answer yes]} {
				HardcodeGeometry
			}
		}
		E_WINDOW_MAXIMIZE {
			set title "dialog-title-max"
			set message "dialog-msg-max"
			set answer [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title $title -message $message]
			if {[string equal $answer yes]} {
				MaximizeWindows
			}
		}
		E_WINDOW_SAVEPOS {WriteGeometryFile}
		E_WINDOW_LOADPOS {
			set title "dialog-title-loadpos"
			if {![file exists [PathTk config geometry]]} {
				set message "dialog-msg-loadpos-fail"
				tk_messageBox -parent [Info $oop win] \
					-title $title -message $message
				return
			}
			set message "dialog-msg-loadpos"
			set answer [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title $title -message $message]
			if {[string equal $answer yes]} {
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
		E_WINDOW_MESSAGES {
			if {[Info $oop messagesWindow]} {
				NSModule::LoadIfNeeded NSMessageWindow
				NSWindowManager::Display message2
			} else {
				NSWindowManager::Undisplay message2
			}
		}
		E_WINDOW_MESSAGE {
			if {[Info $oop messageWindow]} {
				wm deiconify [Window message]
				grid remove [Window main].message
				Global message,message [Window message].message
			} else {
				wm withdraw [Window message]
				grid [Window main].message
				Global message,message [Window main].message.message
			}
			Value message,float [Info $oop messageWindow]
		}
		E_WINDOW_MISC {
			if {[Info $oop miscWindow]} {
				wm deiconify [Window misc]
				grid remove [Window main].misc
				Global misc,canvas [Window misc].misc
				if {[Value misc,layout] == "wide"} {
					wm deiconify [Window progress]
				}
			} else {
				Value misc,layout tall
				wm withdraw [Window misc]
				wm withdraw [Window progress]
				grid [Window main].misc
				Global misc,canvas [Window main].misc.misc
			}
			Value misc,float [Info $oop miscWindow]
			if {[Value misc,layout] == "wide"} {
				NSMiscWindow::MiscArrangeWide
			} else {
				NSMiscWindow::MiscArrangeTall
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

	# Check if game is waiting for a command. If not, it isn't a
	# good time to quit.
	if {[string compare [angband inkey_flags] INKEY_CMD]} {
		bell
		return
	}

	# Ask the user to confirm quit with save
	set answer [tk_messageBox -icon question -type yesno \
		-title [format "dialog-title-quit" "ZAngband"] \
		-message "dialog-msg-quit"]
	if {[string equal $answer no]} return

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

	set widgetId [Global main,widgetId]
	set widget [Global main,widget]
	
	NSWidget::Resize $widgetId $width $height

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
	lappend data E_ACTION_LOOK l
	lappend data E_ACTION_MAP M
	lappend data E_ACTION_NOTE :
	lappend data E_ACTION_OPEN o
	lappend data E_ACTION_REPEAT n

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

	lappend data E_ACTION_PETS p
	lappend data E_ACTION_POWER U

	lappend data E_HELP ?
	lappend data E_OTHER_FEELING ^F
	lappend data E_OTHER_INFO C
	lappend data E_OTHER_KNOWLEDGE ~
	lappend data E_OTHER_MESSAGES ^P
	lappend data E_OTHER_QUEST ^Q
	lappend data E_OTHER_TIME ^T

	lappend data E_PREF_OPTIONS =
	
	foreach {ident key} $data {
		set entry [NSMenu::MenuFindEntry $mbarId $ident]
		if {$::DEBUG && ![llength $entry]} {
			error "can't find menu identifier \"$ident\""
		}
		set menuId [lindex $entry 0]
		set index [lindex $entry 1]
		set menu $NSMenu($menuId,menu)

		if 0 {
		
		set string [angband keymap find $key]
		regsub {\^} $string Ctrl+ string
		$menu entryconfigure $index -accelerator $string
		}
	}

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

	# Hack -- Allow drag during targetting
	if {[string equal [angband inkey_flags] INKEY_TARGET]} {
		NSWidget::TrackPress [Global main,widgetId] $x $y
		return
	}

	scan [angband player hitpoints] "%d %d" curhp maxhp

	TrackOnce $oop

	set track1st 0

	set trackStepping 1
	after 200 set NSMainWindow::trackStepping 0

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

	# Hack -- Allow drag during targetting
	if {[string equal [angband inkey_flags] INKEY_TARGET]} {
		NSWidget::TrackOnce [Global main,widgetId] $x $y
		return
	}

	set trackX $x
	set trackY $y

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
#	key (to move the character).
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

proc NSMainWindow::TrackOnce {oop} {

	variable tracking
	variable track1st
	variable trackX
	variable trackY
	variable trackId
	variable trackStepping

	# If the mouse isn't down, then do nothing. This command gets
	# called whenever the <Inkey> event is generated.
	if {!$tracking} return
if 0 {
	# Hack -- Allow drag during targetting
	if {[string equal [angband inkey_flags] INKEY_TARGET]} {
		NSWidget::TrackOnce [Global main,widgetId] $trackX $trackY
		return
	}
}
	# It is important to delay after taking the first step, otherwise
	# the character won't be able to navigate cleanly, and -more-
	# messages may go zipping by.
	if {$trackStepping} {
		set trackId [after 1 NSMainWindow::TrackOnce $oop]
		return
	}

	# (1) Walking into a door with always_repeat
	# (2) Walking through rubble/tree (OAngband)
	if {!$track1st && [angband player command_rep]} return

	# Get the inkey_flags
	set flags [angband inkey_flags]

	# If the game is displaying the "-more-" message, feed the Term
	# with a single space character. This only works if the "quick_messages"
	# option is set.
	if {[string equal $flags INKEY_MORE]} {
		angband keypress " "
		return
	}

	# If a repeated command is in progress, a mouse-click will disturb
	if {[string equal $flags INKEY_DISTURB]} {
		angband keypress " "
		return
	}

	set widgetId [Global main,widgetId]
	set widget [Global main,widget]

	set coords [NSWidget::PointToCave $widgetId $trackX $trackY]
	if {![string length $coords]} {
		set trackId [after 1 NSMainWindow::TrackOnce $oop]
		return
	}
	scan $coords "%d %d" caveY caveX
	set dirInfo [CaveToDirection $caveY $caveX]
	set dirKey [lindex $dirInfo 0]
	set y [lindex $dirInfo 1]
	set x [lindex $dirInfo 2]

	# If the game is waiting for the user to enter a direction, then
	# feed the direction key into the Term.
	if {[string equal $flags INKEY_DIR]} {
		angband keypress $dirKey
		return
	}

	# If the game is NOT asking for a command, then do nothing
	if {[string compare $flags INKEY_CMD]} {
		return
	}

	# If the mouse is over the player grid, only move if this is
	# the initial mouse click. Otherwise the user may accidentally
	# "run on the spot".
	if {!$track1st && ($dirKey == 5)} {
		set trackId [after 10 NSMainWindow::TrackOnce $oop]
		return
	}

	# If the spacebar is down, we may get any number of Inkey
	# events per turn. To prevent "mouse command overflow" we
	# never feed the Term with more than one key per turn.
	if {[angband keycount]} return

	# Move the character
	angband keypress $dirKey

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

	# If the Widget wasn't dragged, then tell the game to target
	if {[string equal [angband inkey_flags] INKEY_TARGET]} {
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



# NSMainWindow::Leave --
#
#	Handle the mouse leaving the Widget. Called as NSWidget(OOP,leaveCmd).
#
# Arguments:
#	oop					OOP ID NSWidget.
#
# Results:
#	What happened.

proc NSMainWindow::Leave {oop} {

	# Unused: PROJECT_HINT
	if {0 && [string equal [angband inkey_flags] INKEY_TARGET]} {

		# Show target grids at the cursor
		set y [Global cursor,y]
		set x [Global cursor,x]
		angband keypress &$y\n$x\n
		return
	}

	# Clear the statusbar prompt
	StatusText $oop ""

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

	if {$depth == 0} {
		set depthStr [angband cave wild_name]
	} else {
		set depthStr [format "Level %d" $depth]
	}
	$label configure -text $depthStr

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

	set win [Info $oop win]

	set flags [angband inkey_flags]

	# Run
	if {[string equal $flags INKEY_CMD]} {
		MouseCommand $oop $x $y .

	# Set target
	} elseif {[string equal $flags INKEY_DIR]} {
		scan [NSWidget::PointToCave [Global main,widgetId] $x $y] "%d %d" y2 x2
		angband keypress *@$y2\n$x2\n
	}

	return
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

	if {[string compare $Display(window) none] &&
		[string compare $Display(window) $window]} {
		WithdrawWindow $Display(window)
	}

	SelectWindow $window

	set Display(window) $window

	return
}


# NSMainWindow::PositionChanged --
#
#	Called as a qebind <Position> script. Update the Main Window
#	when the character's position changes. Handles the "disturb_panel" option.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMainWindow::PositionChanged {widget y x} {

	global PYPX

	# Keep character centered in the display
	$widget center $y $x
	Global main,widget,center "$y $x"

	# This global is read in various places
	set PYPX "$y $x"
	
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
	if {[string compare [angband inkey_flags] INKEY_CMD]} return

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
	if {[string compare [angband inkey_flags] INKEY_CMD]} return

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
		quotedbl {set command \"}
	}
	
	#set command [angband keymap find $command]
	angband keypress $prefix$command$suffix

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
		-command "set ::trackDelay"

	$scale set $::trackDelay

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

	$menu add command -label "Set Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font statusBar"
	$menu add command -label "Set Color" \
		-command {
			set color [tk_chooseColor -parent [Window main] \
				-initialcolor [Value main,statusbar,color]]
			if {$color != ""} {
				Value main,statusbar,color $color
				[Global main,statusBar] configure -foreground $color
			}
		}
	$menu add command -label "Set Autobar Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font autobar"
	$menu add separator
	$menu add command -label "Cancel"

	tk_popup $menu $x $y

	return
}

