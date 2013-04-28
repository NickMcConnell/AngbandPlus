# File: keymap.tcl

# Purpose: the Keymap Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSKeymap {

	variable MenuString

# namespace eval NSKeymap
}

# NSKeymap::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::InitModule {} {

	MsgCatInit keymap

	InitImageIfNeeded Image_Open open.gif
	InitImageIfNeeded Image_Save save.gif

	NSModule::LoadIfNeeded NSToolbar

	NSObject::New NSKeymap

	return
}

# NSKeymap::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::CloseModule {} {

	catch {
		destroy [Window keymap]
	}

	return
}

# NSKeymap::NSKeymap --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::NSKeymap {oop} {

	# checkbutton menu entry
	Info $oop rogue_like [Setting rogue_like_commands]

	InitWindow $oop

	set win [Info $oop win]

	# Which key is selected
	Info $oop current ""

	# Radiobutton (must match inital NSKeyboard setting)
	Info $oop mode shift

	Info $oop afterId ""

	NSWindowManager::RegisterWindow keymap $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSKeymap::DisplayCmd $oop"

	qebind NSKeymap <Setting-rogue_like_commands> \
		"NSKeymap::UpdateKeymap $oop 0;
		set NSKeymap($oop,rogue_like) %c"
	qebind NSKeymap <Keymap> \
		"NSKeymap::UpdateKeymap $oop 0"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSKeymap $oop $win

	#
	# Global list of application windows
	#

	Global keymap,oop $oop
	Window keymap $win

	return
}

# NSKeymap::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::Info {oop info args} {

	global NSKeymap

	# Verify the object
	NSObject::CheckObject NSKeymap $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSKeymap($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSKeymap($oop,$info)
			}
		}
	}

	return
}

# NSKeymap::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::InitWindow {oop} {

	set win .keymap$oop
	toplevel $win
	wm title $win [mc Keymap]

	wm resizable $win no no

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSKeymap::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_Open -label [mc Open] \
		-showlabel yes -command "NSKeymap::KeymapLoad $oop"
	NSToolbar::AddTool $toolId -image Image_Save -label [mc Save] \
		-showlabel yes -command "NSKeymap::KeymapDump $oop"

	NSStatusText::StatusText [NSToolbar::GetTool $toolId 1] \
		$win.statusBar.label0 \
		"Read settings from an existing preferences file."
	NSStatusText::StatusText [NSToolbar::GetTool $toolId 2] \
		$win.statusBar.label0 \
		"Append keymaps to a new or existing preferences file."

	#
	# Divider
	#

#	frame $win.divider2 \
#		-borderwidth 1 -height 2 -relief groove

	#
	# Keymap
	#

	set keyboardId [NSObject::New NSKeyboard $win]
	NSKeyboard::Info $keyboardId command "NSKeymap::SelectionChanged $oop"
	set canvas [NSKeyboard::Info $keyboardId canvas]

	NSStatusText::StatusText $canvas \
		$win.statusBar.label0 \
		"Click to select a key."

	Info $oop keyboardId $keyboardId

	#
	# Action
	#

	set frame $win.frameFields
	frame $frame \
		-borderwidth 0
	label $frame.labelAction \
		-text [mc Action:]
	entry $frame.entryAction \
		-width 20
	pack $frame.labelAction \
		-side left -padx 2 -pady 5
	pack $frame.entryAction \
		-side left -fill x -padx 2 -pady 5

	Info $oop action,entry $frame.entryAction

	# Keymap action is updated on Return
	set entry $frame.entryAction
	bind $entry <KeyPress-Return> "NSKeymap::SetKeymapAction $oop"

	NSStatusText::StatusText $win.frameFields.entryAction \
		$win.statusBar.label0 \
		"Type an encoded action and hit Enter."

	#
	# Unmodified, Shift, Control
	#

	radiobutton $frame.unmodified \
		-text [mc Normal] -variable NSKeymap($oop,mode) -value unmodified \
		-command "NSKeyboard::SetMode $keyboardId unmodified"
	radiobutton $frame.shift \
		-text [mc Shift] -variable NSKeymap($oop,mode) -value shift \
		-command "NSKeyboard::SetMode $keyboardId shift"
	radiobutton $frame.control \
		-text [mc Control] -variable NSKeymap($oop,mode) -value control \
		-command "NSKeyboard::SetMode $keyboardId control"
	pack $frame.unmodified \
		-side left -padx 2 -pady 0
	pack $frame.shift \
		-side left -padx 2 -pady 0
	pack $frame.control \
		-side left -padx 2 -pady 0


	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar
#	frame $win.statusBar -relief flat -borderwidth 0
#	label $win.statusBar.label -anchor w -relief sunken -padx 2
#	pack $win.statusBar.label -side left -expand yes -fill both

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid rowconfig $win 3 -weight 0 -minsize 0
#	grid rowconfig $win 4 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
#	grid $win.divider2 -in $win \
#		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $canvas -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky {} -padx 2 -pady 2
	grid $win.frameFields -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky w
	grid $win.statusBar -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSKeymap::Close $oop"
	bind $win <Control-KeyPress-w> "NSKeymap::Close $oop"

	return
}

# NSKeymap::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::InitMenus {oop} {

	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSKeymap::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSKeymap::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSKeymap::MenuInvoke $oop"

	#
	# Keymap Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_KEYMAP
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_KEYMAP -label [mc Keymap] -underline 0 -identifier M_KEYMAP

	set entries {}
	lappend entries [list -type checkbutton -label [mc "Rogue-like Keyset"] \
		-variable NSKeymap($oop,rogue_like) -identifier E_ROGUE_LIKE]
	lappend entries [list -type command -label [mc "Dump Keymaps"] \
		-underline 0 -identifier E_KEYMAP_DUMP]
	lappend entries [list -type command -label [mc "Load Pref File"] \
		-underline 0 -identifier E_KEYMAP_LOAD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_KEYMAP $entries

	set MenuString(M_KEYMAP) \
		"Contains commands for saving and reading keymaps."
	set MenuString(E_KEYMAP_DUMP) \
		"Appends keymaps to a new or existing preferences file."
	set MenuString(E_KEYMAP_LOAD) \
		"Read settings from an existing preferences file."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSKeymap::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::SetupMenus {oop mbarId} {

	lappend identList E_ROGUE_LIKE E_KEYMAP_DUMP E_KEYMAP_LOAD E_CLOSE

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSKeymap::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -- $ident {
		{} {
			set desc {}
		}

		E_ROGUE_LIKE {
			if {[Setting rogue_like_commands]} {
				set desc "Uses the original keyset."
			} else {
				set desc "Uses the Rogue-like keyset."
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

# NSKeymap::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::MenuInvoke {oop menuId ident} {

	switch -glob -- $ident {
		E_ROGUE_LIKE {
			Setting rogue_like_commands [Info $oop rogue_like]
		}
		E_KEYMAP_DUMP {KeymapDump $oop}
		E_KEYMAP_LOAD {KeymapLoad $oop}
		E_CLOSE {Close $oop}
	}

	return
}

# NSKeymap::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			UpdateKeymap $oop 1
		}
		postDisplay {
			foreach pattern [qebind NSKeymap] {
				qeconfigure NSKeymap $pattern -active yes
			}
		}
		postWithdraw {
			foreach pattern [qebind NSKeymap] {
				qeconfigure NSKeymap $pattern -active no
			}
		}
	}

	return
}

# NSKeymap::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::Close {oop} {

	NSWindowManager::Undisplay keymap

	return
}

# NSKeymap::SelectionChanged --
#
#	Called by NSKeyboard when a key is selected, or when a key becomes
#	unselected (and no new key is selected). Synchronizes the action
#	entry with the keymap action of the selected key, or clears the
#	action entry when no key is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::SelectionChanged {oop key} {

	# Clear the action entry
	set entry [Info $oop action,entry]
	$entry delete 0 end

	if {[string length $key]} {
		$entry insert 0 [angband keymap action $key]
	}

	Info $oop current $key

	return
}

# NSKeymap::SetKeymapAction --
#
#	Grabs the text from the Action entry and calls "angband keymap" to
#	set the keymap action.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::SetKeymapAction {oop} {

	set key [Info $oop current]
	if {![string length $key]} return

	set entry [Info $oop action,entry]
	angband keymap action $key [$entry get]

	# Feedback on assigned/unassigned state
#	UpdateKeymap $oop

	return
}

# NSKeymap::UpdateKeymap --
#
#	Redisplay the keyboard. Needed because the keyset may change, and
#	because pref files may be read in.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::UpdateKeymap {oop force} {

	variable Priv

	# Since many keymaps may change when a pref file is read in, delay
	# updating the display until idle time.
	if {!$force} {

		if {[Info $oop afterId] eq ""} {
			Info $oop afterId \
				[after idle NSKeymap::UpdateKeymap $oop 1]
		}

		# The idle task was scheduled by a previous call, but this
		# call isn't from the idle task.
		return
	}
	after cancel [Info $oop afterId]
	Info $oop afterId ""

	set keyboardId [Info $oop keyboardId]
	NSKeyboard::Display $keyboardId

	return
}

# NSKeymap::KeymapLoad --
#
#	Read a preferences file from lib/user.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::KeymapLoad {oop} {

	if {[ProcessPrefFile [Info $oop win]]} return
#	UpdateKeymap $oop

	return
}

# NSKeymap::KeymapDump --
#
#	Get a filename from the user then append current keymaps to the given
#	file (or create a new file). The file goes inside the lib/user
#	directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeymap::KeymapDump {oop} {

	global Angband

	set parent [Info $oop win]

	set filename [tk_getSaveFile -initialfile [angband player base_name].prf \
		-initialdir [PathUser] -parent $parent]
	if {![string length $filename]} return

	if {![IsUserFile $filename]} {
		tk_messageBox -title "Pref File Error" -icon info -message \
			"Pref files must be saved in the lib/user directory."
		return
	}

	set filename [file tail $filename]
	if {[catch {angband game keymap_dump $filename} result]} {
		tk_messageBox -title "Pref File Error" -icon error -message $result
	}

	return
}



namespace eval NSKeyboard {

# namespace eval NSKeyboard
}

# NSKeyboard::NSKeyboard --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::NSKeyboard {oop parent} {

	set canvas $parent.keyboard$oop
	canvas $canvas \
		-background White -width 368 -height 132 \
		-highlightthickness 0 -borderwidth 1 -relief sunken \
		-takefocus 1

	bind $canvas <ButtonPress-1> \
		"NSKeyboard::Button1 $oop %x %y"
	bind $canvas <Button1-Motion> \
		"NSKeyboard::Motion1 $oop %x %y"

	Info $oop canvas $canvas
	Info $oop keys {}
	Info $oop command {}

	# Each key gets a tag like "text_N". We can't just use the ASCII
	# character as in "text_k", "text_?" because of the canvas boolean
	# operators.
	Info $oop nextId 0

	set x 0
	set y 0
	set width 24
	set height 28

	set offset $width

	foreach {key1 show1 key2 show2} [list ~ 1  ` 1  ! 1  1 1  @ 1  2 1  # 1  3 1  $ 1  4 1  % 1  5 1  ^ 1  6 1  & 1  7 1  * 1  8 2  ( 1  9 1  ) 1  0 1  _ 1  - 1  + 1 = 1] { 
		AddKey $oop $width $height $key1 $show1 $key2 $show2
		$canvas move temp $x $y
		$canvas dtag temp
		incr x $width
	}

	set x [incr offset [expr {$width / 2}]]
	incr y $height
	foreach {key1 show1 key2 show2} [list Q 1 q 0 W 1 w 0 E 1 e 0 R 1 r 0 T 1 t 0 Y 1 y 0 U 1 u 0 I 1 i 0 O 1 o 0 P 1 p 0 \{ 1 \[ 1 \} 1 \] 1 | 1 \\ 1] { 
		AddKey $oop $width $height $key1 $show1 $key2 $show2
		$canvas move temp $x $y
		$canvas dtag temp
		incr x $width
	}

	set x [incr offset [expr {$width / 2}]]
	incr y $height
	foreach {key1 show1 key2 show2} [list A 1 a 0 S 1 s 0 D 1 d 0 F 1 f 0 G 1 g 0 H 1 h 0 J 1 j 0 K 1 k 0 L 1 l 0 : 1 \; 1 \" 1 ' 1] { 
		AddKey $oop $width $height $key1 $show1 $key2 $show2
		$canvas move temp $x $y
		$canvas dtag temp
		incr x $width
	}

	set x [incr offset [expr {$width / 2}]]
	incr y $height
	foreach {key1 show1 key2 show2} [list Z 1 z 0 X 1 x 0 C 1 c 0 V 1 v 0 B 1 b 0 N 1 n 0 M 1 m 0 < 1 , 1 > 1 . 1 ? 1 / 1] { 
		AddKey $oop $width $height $key1 $show1 $key2 $show2
		$canvas move temp $x $y
		$canvas dtag temp
		incr x $width
	}

	$canvas move all 10 10

	$canvas create line 0 132 368 132 368 0 -fill gray90

	# We can display "shifted" keys or "unshifted" keys. Any key that
	# has two symbols displayed (such as @/2) will have the other key
	# grayed out. Keys with only one symbol (such as Q) will have that
	# symbol changed depending on the "shifted" state.

	# Click a key to select it
	Info $oop current ""
#	$canvas bind all <ButtonPress-1> \
#		"NSKeyboard::ClickKey $oop %x %y"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSKeyboard $oop $canvas

	# Start out in "shift" state
	Info $oop mode shift
	Display $oop

	return
}

# NSKeyboard::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::Info {oop info args} {

	global NSKeyboard

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSKeyboard($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSKeyboard($oop,$info)
			}
		}
	}

	return
}

# NSKeyboard::AddKey --
#
#	Create canvas items for a key.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::AddKey {oop width height char1 show1 char2 show2} {

	set canvas [Info $oop canvas]
	set nextId [Info $oop nextId]

	set char1Id $nextId

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}

	$canvas create rectangle 0 0 $width $height -fill White \
		-tags [list key_$char1Id rect_$char1Id temp]
	$canvas create text 8 7 -text $char1 -font $font \
		-tags [list key_$char1Id text_$char1Id temp] 
	if {$show2} {
		set char2Id [incr nextId]
		$canvas create text 8 19 -text $char2 -font $font \
			-tags [list key_$char1Id text_$char2Id temp]
	}

	Info $oop key,$char1,show $show2
	Info $oop key,$char1,key2 $char2

	Info $oop key2tag,$char1 $char1Id
	Info $oop tag2key,$char1Id $char1
	if {$show2} {
		Info $oop key2tag,$char2 $char2Id
		Info $oop tag2key,$char2Id $char2
	}

	set keys [Info $oop keys]
	lappend keys $char1
	Info $oop keys $keys

	Info $oop nextId [incr nextId]

	return
}

# NSKeyboard::PointToKey --
#
#	Returns the key containing the given point, or an empty string if
#	no key overlaps the point.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::PointToKey {oop x y} {

	set canvas [Info $oop canvas]

	set itemIdList [$canvas find overlapping $x $y [incr x] [incr y]]
	if {![llength $itemIdList]} {
		return ""
	}
	set itemId [lindex $itemIdList 0]
	
	set key ""
	foreach tag [$canvas gettags $itemId] {
		switch -glob -- $tag {
			key_* {
				set tag [string range $tag 4 end]
				return [Info $oop tag2key,$tag]
			}
		}
	}

	# Error...
	return ""
}

# NSKeyboard::SelectKey --
#
#	Removes highlighting from the current selected key, adds highlighting
#	to the given key. Calls the client's command if given.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::SelectKey {oop key} {

	set canvas [Info $oop canvas]
	set mode [Info $oop mode]
	
	set current [Info $oop current]
	if {[string length $current]} {
		if {$key eq $current} return
		set tag [Info $oop key2tag,$current]
		$canvas itemconfigure rect_$tag -width 1.0
	}
	Info $oop current $key
	
	if {[string length $key]} {
	
		set tag [Info $oop key2tag,$key]
		$canvas itemconfigure rect_$tag -width 2.0
		$canvas raise key_$tag
	
		if {$mode eq "control"} {
			set key ^$key
		} elseif {$mode eq "unmodified"} {
			set key [Info $oop key,$key,key2]
		}
	}

	set command [Info $oop command]
	if {[string length $command]} {
		uplevel #0 $command [list $key]
	}

	return
}

# NSKeyboard::SetMode --
#
#	Redisplay the keyboard in the "unmodified", "shift"  or "control" state.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::SetMode {oop mode} {

	switch -- $mode {
		unmodified -
		shift -
		control {
		}
		default {
			error "unknown mode \"$mode\""
		}
	}

	SelectKey $oop ""
	Info $oop mode $mode
	Display $oop

	return
}

# NSKeyboard::Display --
#
#	Configures the display.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::Display {oop} {

	set canvas [Info $oop canvas]
	set mode [Info $oop mode]

	if {$mode eq "control"} {

		foreach key [Info $oop keys] {
			set key2 [Info $oop key,$key,key2]
			set color Black
			if {[string length [angband keymap action ^$key]]} {set color Green}
			if {![string match \[A-Z\] $key]} {set color gray80}
			set tag [Info $oop key2tag,$key]
			if {[Info $oop key,$key,show]} {
				set tag2 [Info $oop key2tag,$key2]
				$canvas itemconfigure text_$tag -fill $color
				$canvas itemconfigure text_$tag2 -fill gray80
			} else {
				$canvas itemconfigure text_$tag -text $key -fill $color
			}
		}

	} elseif {$mode eq "shift"} {

		foreach key [Info $oop keys] {
			set key2 [Info $oop key,$key,key2]
			set color Black
			if {[string length [angband keymap action $key]]} {set color Green}
			set tag [Info $oop key2tag,$key]
			if {[Info $oop key,$key,show]} {
				set tag2 [Info $oop key2tag,$key2]
				$canvas itemconfigure text_$tag -fill $color
				$canvas itemconfigure text_$tag2 -fill gray80
			} else {
				$canvas itemconfigure text_$tag -text $key -fill $color
			}
		}
		
	} elseif {$mode eq "unmodified"} {

		foreach key [Info $oop keys] {
			set key2 [Info $oop key,$key,key2]
			set color Black
			if {[string length [angband keymap action $key2]]} {set color Green}
			set tag [Info $oop key2tag,$key]
			if {[Info $oop key,$key,show]} {
				set tag2 [Info $oop key2tag,$key2]
				$canvas itemconfigure text_$tag -fill gray80
				$canvas itemconfigure text_$tag2 -fill $color
			} else {
				$canvas itemconfigure text_$tag -text $key2 -fill $color
			}
		}
	}

	# Call client command
	set key [Info $oop current]
	if {[string length $key]} {
		if {$mode eq "control"} {
			set key ^$key
		} elseif {$mode eq "unmodified"} {
			set key [Info $oop key,$key,key2]
		}
		set command [Info $oop command]
		if {[string length $command]} {
			uplevel #0 $command [list $key]
		}
	}

	return
}

# NSKeyboard::Button1 --
#
#	Called when the canvas is clicked. If a key is hit, then select it
#	(unless in "control" mode and key isn't A-Z). If no active key is
#	hit, then deselect the current key.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::Button1 {oop x y} {

	focus [Info $oop canvas]
	
	set key [PointToKey $oop $x $y]
	if {![string length $key]} {
		SelectKey $oop ""
		return
	}

	# Some keys are disabled in "control" mode
	if {[Info $oop mode] eq "control" && ![string match \[A-Z\] $key]} {
		SelectKey $oop ""
		return
	}

	SelectKey $oop $key

	return
}

# NSKeyboard::Motion1 --
#
#	If the given point is over a (valid) key, then select it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSKeyboard::Motion1 {oop x y} {

	set key [PointToKey $oop $x $y]
	if {![string length $key]} {
		return
	}

	# Some keys are disabled in "control" mode
	if {[Info $oop mode] eq "control" && ![string match \[A-Z\] $key]} {
		return
	}

	SelectKey $oop $key

	return
}

