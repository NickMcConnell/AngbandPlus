# File: message-history.tcl

# Purpose: the Message History Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
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

	MsgCatInit message

	NSModule::LoadIfNeeded NSList

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

	# Option: Combine identical messages
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		Info $oop combine [Value messages,combine]
	}
	if {[variant ZANGBANDTK]} {
		Info $oop combine 0
	}

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow messages $win \
		"GetDefaultGeometry $win main2 main2" "" \
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
	wm title $win [mc "Message History"]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMessageHistory::Close $oop"

	# Set instance variables
	Info $oop win $win

	InitMenus $oop

	# Divider
	MakeDivider $win.divider1 x

	set frame $win.frameList

	set font [Value font,messages]
	set width 60
	set height 10

	set tree [NSList::New $frame -font messages -columns 2]
	$tree style layout s0 eTxt -padx {2 4}
	$tree column configure 0 -expand no
	$tree column configure 1 -expand yes

	NSList::OnSelection $tree \
		"NSMessageHistory::SelectionChanged $oop %T %c %S %D"
	NSList::OnClick $tree \
		"NSMessageHistory::Click $oop %r"
	NSList::OnInvoke $tree \
		"NSMessageHistory::Click $oop %r"

	Info $oop tree $tree

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

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
		-menu MENU_MESSAGE -label [mc Message] -underline 0 \
		-identifier M_MESSAGE

	set entries {}
	lappend entries [list -type command -label [mc "Dump Messages"] \
		-underline 0 -identifier E_DUMP]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		lappend entries [list -type separator]
		lappend entries [list -type checkbutton -label [mc "Combine Messages"] \
			-variable ::NSMessageHistory($oop,combine) -onvalue 1 -offvalue 0 \
			-identifier E_COMBINE]
	}
if 0 {
	lappend entries [list -type command -label [mc "Max Messages..."] \
		-underline 0 -identifier E_MAX]
}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
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

# NSMessageHistory::Click --
#
#	When a selected message is clicked, play the sound associated with that
#	message (if any).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::Click {oop row} {

	set index [lindex [Info $oop age] $row]
	set list [angband message sound $index]
	if {![llength $list]} return

	# Stop other sounds
	angband sound stop

	angband sound play {*}$list

	return
}

# NSMessageHistory::SelectionChanged --
#
#	When a message is selected, play the sound associated with that
#	message (if any).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessageHistory::SelectionChanged {oop tree count select deselect} {

	if {![llength $select]} return

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	set index [lindex [Info $oop age] $row]
	set list [angband message sound $index]
	if {![llength $list]} return

	# Stop other sounds
	angband sound stop

	angband sound play {*}$list

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

	set tree [Info $oop tree]

	NSList::Clear $tree

	set max [angband message count]
if 0 {
	if {$max > [Value messages,max]} {
		set max [Value messages,max]
	}
}
	set age {}

	# Option: Combine identical messages (ex "You hit it. (x3)")
	if {[Info $oop combine]} {

		set curMsg ""
		set curSound ""
		set count 0
		for {set i [expr {$max - 1}]} {$i >= 0} {incr i -1} {
			set nextMsg [angband message get $i]
			set nextSound [angband message sound $i]
			if {$curMsg ne $nextMsg} {
				if {[string length $curMsg]} {
					if {$count > 1} {
						append curMsg " (x$count)"
					}
					lappend textList $curMsg
					lappend colorList [angband message color $curAge]
					lappend soundList $curSound
					lappend age $curAge
				}
				set curMsg $nextMsg
				set curSound $nextSound
				set curAge $i
				set count 1
			} else {
				incr count
			}
		}
		if {$count > 1} {
			append curMsg " (x$count)"
		}
		lappend textList $curMsg
		lappend colorList [angband message color $curAge]
		lappend soundList $curSound
		lappend age $curAge

	# Don't combine identical messages
	} else {
		for {set i [expr {$max - 1}]} {$i >= 0} {incr i -1} {
			set curMsg [angband message get $i]
			set curSound [angband message sound $i]
			lappend textList $curMsg
			lappend colorList [angband message color $i]
			lappend soundList $curSound
			lappend age $i
		}
	}
	Info $oop age $age
	set max [llength $textList]

	foreach text $textList color $colorList sound $soundList {
		set item [$tree item create]
		if {$sound ne ""} {
			NSList::SetText $tree $item *
		}
		NSList::SetTextEx $tree $item 1 $text
		if {$color ne "TERM_WHITE"} {
			NSList::SetTextFillEx $tree $item 1 [Value $color]
		}
		$tree item lastchild root $item
	}

	set row [expr {$max - 1}]
	NSList::Activate $tree "root child $row"

	focus $tree

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

	set tree [Info $oop tree]

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a string
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) -prompt [mc find-prompt] \
			-buttons [list [mc Find] [mc Cancel]] -parent [Info $oop win]]

		# User cancelled
		if {![string length $string]} return

		# Search again
		set Priv(find,string) $string
		set Priv(find,index) end
	}

	# Search for the string
	if {$Priv(find,index) eq "end"} {
		set Priv(find,index) [expr {[llength [Info $oop age]] - 1}]
	}
	for {set i $Priv(find,index)} {$i >= 0} {incr i -1} {
		set age [lindex [Info $oop age] $i]
		set text [angband message get $age]
		if {[string match -nocase *$Priv(find,string)* $text]} {
			break
		}
	}

	# The string wasn't found
	if {$i < 0} {

		# Search from the end
		set Priv(find,index) end

		# Done
		return
	}

	set Priv(find,index) [expr {$i - 1}]
	NSList::Activate $tree "root child $i"

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
	set string [NSUtils::StringBox -title [mc max-title] \
		-initial [Value messages,max] -prompt [mc max-prompt] \
		-buttons [list [mc OK] [mc Cancel]] -parent [Info $oop win] \
		-entrywidth 5 -type integer -message [mc max-message]]
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

