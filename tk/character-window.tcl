# File: character-window.tcl

# Purpose: the Character Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSCharacterWindow {

	variable MenuString
	variable Priv

# namespace eval NSCharacterWindow
}

# NSCharacterWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::InitModule {} {

	variable Priv

	MsgCatInit player

	NSModule::LoadIfNeeded NSList

	set Priv(hook) {}
	lappend Priv(hook) info
	lappend Priv(hook) flag
	if {[variant ZANGBANDTK]} {
		lappend Priv(hook) mutation
		lappend Priv(hook) virtue
		lappend Priv(hook) note
	}

	NSObject::New NSCharacterWindow

	return
}

# NSCharacterWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::CloseModule {} {

	catch {
		destroy [Window character]
	}

	return
}

# NSCharacterWindow::NSCharacterWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::NSCharacterWindow {oop} {

	variable Priv

	Info $oop hook ""

	InitWindow $oop

	set win [Info $oop win]
	
	NSWindowManager::RegisterWindow character $win \
		"GetDefaultGeometry $win screen main2" \
		"NSCharacterWindow::SetupCmd $oop" \
		"NSCharacterWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSCharacterWindow $oop $win

	#
	# Global list of application windows
	#

	Global character,oop $oop
	Window character $win

	return
}

# NSCharacterWindow::~NSCharacterWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::~NSCharacterWindow {oop} {

	return
}

# NSCharacterWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::Info {oop info args} {

	global NSCharacterWindow

	# Verify the object
	NSObject::CheckObject NSCharacterWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSCharacterWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSCharacterWindow($oop,$info)
			}
		}
	}

	return
}

# NSCharacterWindow::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::InitWindow {oop} {

	variable Priv

	set win .character$oop
	toplevel $win
	wm title $win [mc Character]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSCharacterWindow::Close $oop"

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
	foreach hook $Priv(hook) {
		NSTabs::Add $tabsId [mc $hook]
	}
	NSTabs::Info $tabsId invokeCmd "NSCharacterWindow::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Content area. A black background frame is used to prevent annoying
	# flicker.
	#

	set frame $win.frame
	frame $frame \
		-borderwidth 1 -relief sunken
	frame $frame.background \
		-borderwidth 0 -background Black
	place $frame.background -x 0 -y 0 -relwidth 1.0 -relheight 1.0
	Info $oop frame $frame

	# A separate frame for each hook's stuff
	foreach hook $Priv(hook) {
		Info $oop frame,$hook [hook_$hook $oop init $frame]
	}

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
 
	grid $win.divider2 \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frame \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress-Escape> "NSCharacterWindow::Close $oop"
	bind $win <Control-KeyPress-w> "NSCharacterWindow::Close $oop"

	return
}

# NSCharacterWindow::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::InitMenus {oop} {

	variable MenuString
	variable Priv

	set win [Info $oop win]

	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSCharacterWindow::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSCharacterWindow::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSCharacterWindow::MenuInvoke $oop"

	#
	# Character Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_CHARACTER
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_CHARACTER -label [mc Character] -underline 0 \
		-identifier M_CHARACTER

	set entries {}
	set i 1
	foreach hook $Priv(hook) {
		lappend entries [list -type radiobutton -label [mc $hook] \
			-variable NSCharacterWindow($oop,radio,hook) -value $hook \
			-accelerator $i -identifier E_HOOK_$hook]
		bind $win <KeyPress-$i> "NSCharacterWindow::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Change Icon"] \
		-underline 0 -identifier E_ICON]
	lappend entries [list -type command -label [mc "Change Name"] \
		-underline 0 -identifier E_NAME]
	lappend entries [list -type command -label [mc "File Character"] \
		-underline 0 -identifier E_DUMP]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_CHARACTER $entries

	set MenuString(M_CHARACTER) \
		"Contains character-related commands."
	set MenuString(E_ICON) \
		"Changes the character's icon."
	set MenuString(E_NAME) \
		"Changes the character's name."
	set MenuString(E_DUMP) \
		"Dumps a character record to a text file."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSCharacterWindow::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::SetupMenus {oop mbarID} {

	variable Priv

	foreach hook $Priv(hook) {
		lappend identList E_HOOK_$hook
	}

	lappend identList E_NAME E_ICON
	lappend identList E_DUMP E_CLOSE

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSCharacterWindow::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::MenuSelect {oop menuId index ident} {

	variable MenuString
	variable Priv

	switch -glob -- $ident {
		{} {
			set desc {}
		}
		E_HOOK_* {
			set desc "Displays this page."
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

	set statusBar [Info $oop win].statusBar
	$statusBar cover set $desc
	if {($desc eq "") && ($menuId == [Info $oop mbarId])} {
		if {[$statusBar cover visible]} {
			$statusBar cover hide
		} else {
			$statusBar cover show
		}
	}

	return
}

# NSCharacterWindow::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_HOOK_* {
			scan $ident "E_HOOK_%s" hook
			SetHook $oop $hook
		}
		E_ICON {
			NSModule::LoadIfNeeded NSAssign
			NSWindowManager::Display assign character
		}
		E_NAME {NSGlobal::ChangeCharacterName [Info $oop win]}
		E_DUMP {FileCharacter [Info $oop win]}
		E_CLOSE {Close $oop}
	}

	return
}

# NSCharacterWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
			if {$first} {
				SetHook $oop info
			} else {
				CallHook $oop display
			}
		}
		postDisplay {
		}
		postWithdraw {
		}
	}

	return
}

# NSCharacterWindow::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::SetupCmd {oop} {

	variable Priv

	set frame [Info $oop frame]
	foreach hook $Priv(hook) {
		set frame [Info $oop frame,$hook]
		pack $frame -expand yes -fill both
		update idletasks
		pack forget $frame
	}

	return
}

# NSCharacterWindow::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::Close {oop} {

	angband keypress \033

	return
}

# NSCharacterWindow::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::CallHook {oop message args} {

	return [uplevel #0 NSCharacterWindow::hook_[Info $oop hook] $oop $message $args]
}

# NSCharacterWindow::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::SetHook {oop hook} {

	variable Priv

	set oldHook [Info $oop hook]
	if {$hook eq $oldHook} return
	if {[string length $oldHook]} {
		pack forget [Info $oop frame,$oldHook]
	}

	# Remember the hook
	Info $oop hook $hook

	set parent [Info $oop frame,$hook]
	pack $parent -expand yes -fill both

	CallHook $oop display

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

	[Info $oop win].statusBar itemconfigure t2 -text ""

	return
}

# NSCharacterWindow::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharacterWindow::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Priv(hook) $index]

	return
}

proc NSCharacterWindow::hook_info {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameInfo
			frame $frame \
				-borderwidth 0
			NSModule::LoadIfNeeded NSCharInfoCanvas
			set infoId [NSObject::New NSCharInfoCanvas $frame]
			Info $oop hook,info,oop $infoId
			return $frame
		}
		display {
			NSCharInfoCanvas::SetInfo [Info $oop hook,info,oop]
		}
	}

	return
}

proc NSCharacterWindow::hook_flag {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameFlags
			frame $frame \
				-borderwidth 0
			NSModule::LoadIfNeeded NSCharFlagsCanvas
			set flagsId [NSObject::New NSCharFlagsCanvas $frame]
			NSCharFlagsCanvas::Info $flagsId statusBar \
				[Info $oop win].statusBar
			Info $oop hook,flag,oop $flagsId
			return $frame
		}
		display {
			NSCharFlagsCanvas::SetInfo [Info $oop hook,flag,oop]

			# So <MouseWheel> works
			focus [NSCharFlagsCanvas::Info [Info $oop hook,flag,oop] canvas]
		}
	}

	return
}

if {[variant ZANGBANDTK]} {

proc NSCharacterWindow::hook_mutation {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameMutations
			set tree [NSList::New $frame]
			Info $oop hook,mutation,tree $tree

			return $frame
		}
		display {
			set tree [Info $oop hook,mutation,tree]
			NSList::Clear $tree
			foreach desc [angband player mutations] {
				set item [$tree item create]
				NSList::SetText $tree $item $desc
				$tree item lastchild root $item
			}
		}
	}

	return
}

proc NSCharacterWindow::hook_virtue {oop message args} {

	switch -- $message {
		init {
			return [Info $oop frame,mutation]
		}
		display {
			set tree [Info $oop hook,mutation,tree]
			NSList::Clear $tree
			foreach desc [angband player virtues] {
				set item [NSList::NewItem $tree]
				NSList::SetText $tree $item $desc
			}
		}
	}

	return
}

proc NSCharacterWindow::hook_note {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameNotes
			frame $frame \
				-borderwidth 0
			set text $frame.text
			text $text -foreground White -background [Value listBG] \
				-font [Value font,knowledge] \
				-xscrollcommand "$frame.xscroll set" \
				-yscrollcommand "$frame.yscroll set"
			scrollbar $frame.xscroll \
				-orient horizontal -command "$text xview"
			scrollbar $frame.yscroll \
				-orient vertical -command "$text yview"

			qebind NSCharacterWindow::hook_note <Value-font,knowledge> \
				"$text configure -font %c"
			qebind NSCharacterWindow::hook_note <Value-listBG> \
				"$text configure -background %c"

			NSUtils::SynchScrollBar $text $frame.yscroll
			NSUtils::SynchScrollBar $text $frame.xscroll

			grid rowconfig $frame 0 -weight 1
			grid rowconfig $frame 1 -weight 0
			grid columnconfig $frame 0 -weight 1
			grid columnconfig $frame 1 -weight 0
		
			grid $text \
				-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
			grid $frame.yscroll \
				-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
			grid $frame.xscroll \
				-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew

			Info $oop hook,note,text $text

			return $frame
		}
		display {
			set text [Info $oop hook,note,text]
			$text delete 1.0 end
			set name [string range [angband player base_name] 0 7].txt
			set path [Path lib save $name]
			if {[file exists $path]} {
				set id [open $path]
				$text insert end [read $id]
				close $id
			}
		}
	}

	return
}

# ZANGBANDTK
}
