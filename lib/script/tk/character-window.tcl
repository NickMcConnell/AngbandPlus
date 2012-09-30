# File: character-window.tcl

# Purpose: the Character Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
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

	set Priv(hook) {}
	lappend Priv(hook) HookInfo "Info"
	lappend Priv(hook) HookFlags "Flags"
	lappend Priv(hook) HookMutations "Mutations"
	lappend Priv(hook) HookVirtues "Virtues"
	lappend Priv(hook) HookNotes "Notes"

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
		"GetDefaultGeometry $win screen main" \
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
	wm title $win "Character"

	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSCharacterWindow::Close $oop"

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
	foreach {hook label} $Priv(hook) {
		Info $oop frame,$hook [$hook $oop init $frame]
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
		-menu MENU_CHARACTER -label "Character" -underline 0 \
		-identifier M_CHARACTER

	set entries {}
	set i 1
	foreach {hook label} $Priv(hook) {
		lappend entries [list -type radiobutton -label $label \
			-variable NSCharacterWindow($oop,radio,hook) -value $hook \
			-accelerator $i -identifier E_HOOK_$i]
		bind $win <KeyPress-$i> "NSCharacterWindow::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_CHARACTER $entries

	set MenuString(M_CHARACTER) \
		"Contains character-related commands."
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

	set i 0
	foreach {hook label} $Priv(hook) {
		lappend identList E_HOOK_[incr i]
	}

	lappend identList E_CLOSE

	NSMenu::MenuEnable $mbarID $identList

	[Info $oop win].statusBar cover show

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

	[Info $oop win].statusBar cover set $desc
	if {![string length $desc]} {
		if {$menuId == [Info $oop mbarId]} {
			[Info $oop win].statusBar cover hide
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
			scan $ident "E_HOOK_%d" hookNum
			SetHook $oop [lindex $Priv(hook) [expr {($hookNum - 1) * 2}]]
		}
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
				SetHook $oop HookInfo
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
	foreach {hook label} $Priv(hook) {
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

	return [uplevel #0 NSCharacterWindow::[Info $oop hook] $oop $message $args]
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
	if {[string equal $hook $oldHook]} return
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
	set tabId [NSTabs::GetNthId $tabsId [expr {[lsearch -exact $Priv(hook) $hook] / 2}]]
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
	SetHook $oop [lindex $Priv(hook) [expr {$index * 2}]]

	return
}

proc NSCharacterWindow::HookInfo {oop message args} {

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

proc NSCharacterWindow::HookFlags {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameFlags
			frame $frame \
				-borderwidth 0
			NSModule::LoadIfNeeded NSCharFlagsCanvas
			set flagsId [NSObject::New NSCharFlagsCanvas $frame]
			NSCharFlagsCanvas::Info $flagsId statusBar \
				[Info $oop win].statusBar
			Info $oop hook,flags,oop $flagsId
			return $frame
		}
		display {
			NSCharFlagsCanvas::SetInfo [Info $oop hook,flags,oop]
		}
	}

	return
}


proc NSCharacterWindow::HookMutations {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameMutations
			frame $frame \
				-borderwidth 0
			set textId [NSObject::New NSTexist $frame [Value font,knowledge] \
				60 10]
			set text [NSTexist::Info $textId text]
			$text configure -background [Value listBG] \
				-yscrollcommand "$frame.yscroll set"
			scrollbar $frame.yscroll \
				-orient vertical -command "$text yview"
			pack $text -side left -expand yes -fill both
			pack $frame.yscroll -side left -expand no -fill y
			Info $oop hook,mutations,textId $textId

			set id [NSValueManager::AddClient font,knowledge \
				"NSCharacterWindow::HookMutations $oop font_changed"]
			bind $text <Destroy> \
				"NSValueManager::RemoveClient font,knowledge $id"

			return $frame
		}
		display {
			set textId [Info $oop hook,mutations,textId]
			set textList {}
			set colorList {}
			foreach desc [angband player mutations] {
				lappend textList $desc
				lappend colorList White
			}
			NSTexist::SetList $textId $textList $colorList
		}
		font_changed {
			set textId [Info $oop hook,mutations,textId]
			set text [NSTexist::Info $textId text]
			$text configure -font [Value font,knowledge]
		}
	}

	return
}

proc NSCharacterWindow::HookVirtues {oop message args} {

	switch -- $message {
		init {
			return [Info $oop frame,HookMutations]
		}
		display {
			set textId [Info $oop hook,mutations,textId]
			set textList {}
			set colorList {}
			foreach desc [angband player virtues] {
				lappend textList $desc
				lappend colorList White
			}
			NSTexist::SetList $textId $textList $colorList
		}
	}

	return
}

proc NSCharacterWindow::HookNotes {oop message args} {

	switch -- $message {
		init {
			set frame [lindex $args 0].frameNotes
			frame $frame \
				-borderwidth 0
			set text $frame.text
			text $text -foreground White -background [Value listBG] \
				-font [Global font,sys,normal] \
				-xscrollcommand "$frame.xscroll set" \
				-yscrollcommand "$frame.yscroll set"
			scrollbar $frame.xscroll \
				-orient horizontal -command "$text xview"
			scrollbar $frame.yscroll \
				-orient vertical -command "$text yview"

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

			Info $oop hook,notes,text $text

			return $frame
		}
		display {
			set text [Info $oop hook,notes,text]
			$text delete 1.0 end
			set name [string range [angband player base_name] 0 7].txt
			set path [PathTk lib save $name]
			if {[file exists $path]} {
				set id [open $path]
				$text insert end [read $id]
				close $id
			}
		}
	}

	return
}
