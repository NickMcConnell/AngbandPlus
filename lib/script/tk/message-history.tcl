# File: message-history.tcl

# Purpose: the Message History Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMessageHistory {

	variable MenuString
	variable Priv

# namespace eval NSMessageHistory
}

# NSMessageHistory::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::InitModule {} {

	variable Priv

	set Priv(find,string) ""
	set Priv(find,index) end

	# Create the Message History Window
	NSObject::New NSMessageHistory

	return
}

# NSMessageHistory::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::CloseModule {} {

	catch {
		destroy [Window messages]
	}

	return
}

# NSMessageHistory::NSMessageHistory --
#
#	Create a message window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageHistory::NSMessageHistory {oop} {

	# Update ourself when the font,messages value changes
	Info $oop clientId,font,messages \
		[NSValueManager::AddClient font,messages \
			NSMessageHistory::ValueChanged_Font_Messages]

	# Option: Combine identical messages
	Info $oop combine 0
	
	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow messages $win \
		"GetDefaultGeometry $win main2 main" "" \
		"NSMessageHistory::DisplayCmd $oop"

	bind $win <KeyPress-f> "NSMessageHistory::Find $oop 0"
	bind $win <KeyPress-g> "NSMessageHistory::Find $oop 1"
	bind $win <Control-KeyPress-w> "NSMessageHistory::Close $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMessageHistory $oop $win

	#
	# Global list of application windows
	#

	Global messages,oop $oop
	Window messages $win

	return
}

# NSMessageHistory::~NSMessageHistory --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::~NSMessageHistory {oop} {

	NSValueManager::RemoveClient listBG [Info $oop clientId,listBG]
	NSValueManager::RemoveClient font,messages [Info $oop clientId,font,messages]

	return
}

# NSMessageHistory::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::Info {oop info args} {

	global NSMessageHistory

	# Verify the object
	NSObject::CheckObject NSMessageHistory $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMessageHistory($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMessageHistory($oop,$info)
			}
		}
	}

	return
}

# NSMessageHistory::InitWindow --
#
#	Create the window associated with the object.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageHistory::InitWindow {oop} {

	set win .messages$oop
	toplevel $win
	wm title $win "Message History"

	wm transient $win [Window main]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMessageHistory::Close $oop"

	# Set instance variables
	Info $oop win $win

	InitMenus $oop

	# Divider
	MakeDivider $win.divider1 x

	set frame $win.frameList
	frame $frame \
		-relief sunken -borderwidth 1

	set font [Value font,messages]
	set width 60
	set height 10

	set canvistId [NSObject::New NSTexist $frame $font $width $height]
	set canvas [NSTexist::Info $canvistId text]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$frame.yscroll set"
	$canvas configure -xscrollcommand "$frame.xscroll set"

	scrollbar $frame.yscroll \
		-orient vertical -command "$canvas yview"

	scrollbar $frame.xscroll \
		-orient horizontal -command "$canvas xview"

	NSTexist::Info $canvistId selectionCmd \
		"NSMessageHistory::SelectionChanged $oop"

	# This call updates the list background color whenever the
	# global list background color changes
	Info $oop clientId,listBG \
		[NSValueManager::AddClient listBG \
		"$canvas configure -background \[Value listBG]"]

	Info $oop canvistId $canvistId

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfig $win.frameList 0 -weight 1
	grid rowconfig $win.frameList 1 -weight 0
	grid columnconfig $win.frameList 0 -weight 1
	grid columnconfig $win.frameList 1 -weight 0

	grid $canvas \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid rowconfigure $win 2 -weight 0
	grid columnconfig $win 0 -weight 1

	if {[Platform windows]} {
		grid $win.divider1 \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew -pady 2
	}
	grid $win.frameList \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress> {
		angband keypress %A
	}

	# Synch the scrollbars when window is shown.
	NSUtils::SynchScrollBar $canvas $win.frameList.yscroll
	NSUtils::SynchScrollBar $canvas $win.frameList.xscroll

	return
}

# NSMessageHistory::InitMenus --
#
#	Create the menus associated with the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSMessageHistory::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbarId

	# Context-sensitive help
	NSMenu::Info $mbarId menuSelectCmd "NSMessageHistory::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbarId invokeCmd "NSMessageHistory::MenuInvoke $oop"

	#
	# Message Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_MESSAGE
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_MESSAGE -label "Message" -underline 0 \
		-identifier M_MESSAGE

	set entries {}
	lappend entries [list -type command -label "Dump Messages" \
		-underline 0 -identifier E_DUMP]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Find..." \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label "Find Again" \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Max Messages..." \
		-underline 0 -identifier E_MAX]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbarId -end MENU_MESSAGE $entries

	set MenuString(M_MESSAGE) \
		"Contains commands for displaying and searching messages."
	set MenuString(E_DUMP) \
		"Writes all the messages to a text file."
	set MenuString(E_FIND) \
		"Searches for a message containing a given string."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_MAX) \
		"Changes the number of messages displayed."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSMessageHistory::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::SetupMenus {oop mbarId} {

	variable Priv

	lappend identList E_DUMP E_COMBINE E_MAX E_FIND E_FIND_AGAIN E_CLOSE
		
	NSMenu::MenuEnable $mbarId $identList

	[Info $oop win].statusBar cover show

	return
}

# NSMessageHistory::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -- $ident {
		{} {
			set desc {}
		}
		E_COMBINE {
			if {[Value messages,combine]} {
				set desc "Displays each message separately."
			} else {
				set desc "Combines identical sequential messages into a single line."
			}
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
	if {[string length $desc]} {
	} else {
		if {$menuId == [Info $oop mbarId]} {
			[Info $oop win].statusBar cover hide
		}
	}

	return
}

# NSMessageHistory::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::MenuInvoke {oop menuId ident} {

	set win [Info $oop win]

	switch -glob -- $ident {
		E_DUMP {MessageDump $win}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_COMBINE {CombineMessages $oop}
		E_MAX {MaxMessages $oop}
		E_CLOSE {Close $oop}
	}

	return
}

# NSMessageHistory::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::DisplayCmd {oop message first} {

	global Angband

	switch -- $message {
		preDisplay {
			SetList $oop
		}
		postDisplay {
		}
	}

	return
}

# NSMessageHistory::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageHistory::Close {oop} {

	angband keypress \033

	return
}

# NSMessageHistory::SetList --
#
#	Display a list of recent messages in the Message Window. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessageHistory::SetList {oop} {

	set canvistId [Info $oop canvistId]
	set canvas [NSTexist::Info $canvistId text]

	set max [angband message count]
	if {$max > [Value messages,max]} {
		set max [Value messages,max]
	}
	set age {}

	# Option: Combine identical messages (ex "You hit it. (x3)")
	if {[Info $oop combine]} {
	
		set curMsg ""
		set count 0
		for {set i [expr {$max - 1}]} {$i >= 0} {incr i -1} {
			set nextMsg [angband message get $i]
			if {[string compare $curMsg $nextMsg]} {
				if {[string length $curMsg]} {
					set curMsg "  $curMsg"

					if {$count > 1} {
						append curMsg " (x$count)"
					}
					lappend textList $curMsg
					lappend colorList [Value [angband message color $curAge]]
					lappend age $curAge
				}
				set curMsg $nextMsg
				set curAge $i
				set count 1
			} else {
				incr count
			}
		}
		if {$count > 1} {
			append curMsg " (x$count)"
		}
		set curMsg "  $curMsg"
		lappend textList $curMsg
		lappend colorList [Value [angband message color $curAge]]
		lappend age $curAge

	# Don't combine identical messages
	} else {
		for {set i [expr {$max - 1}]} {$i >= 0} {incr i -1} {
			set curMsg [angband message get $i]
			set curMsg "  $curMsg"
			lappend textList $curMsg
			lappend colorList [Value [angband message color $i]]
			lappend age $i
		}
	}
	Info $oop age $age
	set max [llength $textList]

	NSTexist::SetList $canvistId $textList $colorList

	set row [expr {$max - 1}]
	NSTexist::See $canvistId $row
	NSTexist::UpdateSelection $canvistId $row {}

	focus $canvas

	return
}

# NSMessageHistory::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSMessageHistory::Find --
#
#	Simple search routine to look for a message.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::Find {oop again} {

	variable Priv

	set canvistId [Info $oop canvistId]
	set text [NSTexist::Info $canvistId text]

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a string
		set string [NSUtils::StringBox -title "Find" \
			-initial $Priv(find,string) -prompt "find-prompt" \
			-buttons [list "Find" "Cancel"] -parent [Info $oop win]]

		# User cancelled
		if {![string length $string]} return

		# Search again
		set Priv(find,string) $string
		set Priv(find,index) end
	}

	# Search for the string
	set Priv(find,index) [$text search -backwards -nocase \
		-- $Priv(find,string) $Priv(find,index)]

	# The string wasn't found
	if {![string length $Priv(find,index)]} {

		# Search from the end
		set Priv(find,index) end

		# Done
		return
	}

	scan $Priv(find,index) "%d.%d" line char
	incr line -1
	NSTexist::UpdateSelection $canvistId $line all
	NSTexist::See $canvistId $line

	return
}

# NSMessageHistory::MaxMessages --
#
#	Lets the user enter the maximum number of messages to be displayed
#	in the Message History.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::MaxMessages {oop} {

	# Ask the user for a number
	set string [NSUtils::StringBox -title "max-title" \
		-initial [Value messages,max] -prompt "max-prompt" \
		-buttons [list "OK" "Cancel"] -parent [Info $oop win] \
		-entrywidth 5 -type integer -message "max-message"]
	if {![string length $string]} return

	if {![scan $string "%d" max]} return
	if {$max < 1 || $max > 2048} return
	Value messages,max $max

	# Redisplay
	StatusBar $oop "Displaying..." 0
	update idletasks
	SetList $oop
	StatusBar $oop "Done." 1

	return
}

# NSMessageHistory::CombineMessages --
#
#	Called when the Combine menu entry is toggled.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::CombineMessages {oop} {

	Value messages,combine [Info $oop combine]
	
	# Redisplay
	StatusBar $oop "Displaying..." 0
	update idletasks
	SetList $oop
	StatusBar $oop "Done." 1

	return
}

# NSMessageHistory::ValueChanged_Font_Messages --
#
#	Called when the font,messages value changes.
#	Updates the Message History Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::ValueChanged_Font_Messages {} {

	set texistId [NSMessageHistory::Info [Global messages,oop] canvistId]
	[NSTexist::Info $texistId text] configure -font [Value font,messages]

	return
}

