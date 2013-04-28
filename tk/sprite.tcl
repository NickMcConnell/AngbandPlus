# File: sprite.tcl

# Purpose: the Sprite Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSSprite {

	variable MenuString

# namespace eval NSSprite
}

# NSSprite::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::InitModule {} {

	MsgCatInit

	NSModule::LoadIfNeeded NSIconBrowser

	NSObject::New NSSprite

	return
}

# NSSprite::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::CloseModule {} {

	catch {
		destroy [Window sprite]
	}

	return
}

# NSSprite::NSSprite --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::NSSprite {oop} {

	Info $oop sprite,current -1
	Info $oop sprite,current2 -1
	Info $oop frame,current -1
	Info $oop frame,current2 -1
	Info $oop frame,lastIndex -1
	Info $oop ignoreSel 0

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow sprite $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSSprite::DisplayCmd $oop"

	bind $win <KeyPress-Escape> "NSSprite::Close $oop"
	bind $win <Control-KeyPress-w> "NSSprite::Close $oop"

	bind $win <KeyPress-n> "NSSprite::NewSprite $oop"
	bind $win <KeyPress-plus> "NSSprite::NewFrame $oop"
	bind $win <KeyPress-minus> "NSSprite::DeleteFrame $oop"

	qebind NSSprite <IconCfg> \
		"NSSprite::IconCfg $oop"

	#
	# Global list of application windows
	#

	Global sprite,oop $oop
	Window sprite $win

	return
}

# NSSprite::~NSSprite --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::~NSSprite {oop} {

	return
}

# NSSprite::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::Info {oop info args} {

	global NSSprite

	# Verify the object
	NSObject::CheckObject NSSprite $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSSprite($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSSprite($oop,$info)
			}
		}
	}

	return
}

# NSSprite::Init Window --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::InitWindow {oop} {

	global NSSprite

	set win .sprite$oop
	toplevel $win
	wm title $win "Sprite Editor"

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSSprite::Close $oop"

	Info $oop win $win

	# Menus
	InitMenus $oop

	# Frame around sprite list, frame list, etc on the left
	set frame $win.frame
	frame $frame \
		-borderwidth 0

	set iconSize [expr {$::NSIconBrowser::Priv(maxIconHeight) + 8}]

	# The list of sprites
	set frameSprite $frame.frameSprite
	NSList::New $frameSprite -text no -icon yes -wrap yes
	set tree $frameSprite.tree
	$tree configure -width 240 -height 160

	# When a sprite is selected, show the frames of it
	NSList::OnSelection $tree \
		"NSSprite::SelectionChanged_Sprite $oop %T %c %S %D"

	Info $oop sprite,tree $tree

	MakeDivider $frame.divider2 x

	# The list of frames in a sprite
	set frameFrame $frame.frameFrame
	NSList::New $frameFrame -text no -icon yes -yscrollbar no -xscrollbar yes
	set tree $frameFrame.tree
	$tree configure -width 240 -height $iconSize -orient horizontal

	NSList::OnSelection $tree \
		"NSSprite::SelectionChanged_Frame $oop %T %c %S %D"

	bindtags $tree [concat [bindtags $tree] NSSpriteBindTag$oop]
	bind NSSpriteBindTag$oop <Motion> \
		"NSSprite::Motion $oop frame %x %y"
	bind NSSpriteBindTag$oop <Leave> \
		"NSSprite::Leave $oop frame"

	Info $oop frame,tree $tree

	set frameConfig $frame.frameConfig
	frame $frameConfig \
		-borderwidth 0

	# Frame delay scale
	scale $frameConfig.scaleSpeed \
		-orient horizontal -label "Frame Delay Factor" -showvalue no \
		-width 8 -sliderlength 12 -length 170 -from 1 -to 100 \
		-variable NSSprite($oop,delay) \
		-command "NSSprite::ConfigSprite $oop delay"

	# Sprite reverse checkbutton
	Info $oop reverse 0
	checkbutton $frameConfig.reverse \
		-text "Reverse" -command "NSSprite::ConfigSprite\
		$oop reverse \$NSSprite($oop,reverse)" \
		-variable NSSprite($oop,reverse)
#		-variable NSSprite($oop,reverse) -justify left -borderwidth 0

	pack $frameConfig.scaleSpeed \
		-side left -anchor w
	pack $frameConfig.reverse \
		-side left -anchor w

	# Geometry of stuff on the left
	grid rowconfig $frame 0 -weight 1 -minsize 0
	grid rowconfig $frame 1 -weight 0 -minsize 0
	grid rowconfig $frame 2 -weight 0 -minsize 0
	grid rowconfig $frame 3 -weight 0 -minsize 0
	grid columnconfig $frame 0 -weight 1 -minsize 0
	grid $frameSprite \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.divider2 \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew \
		-padx 2 -pady 2
	grid $frameFrame \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $frameConfig \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky w

	# An NSIconBrowser lets the user examine all icon types
	set browserId [NSObject::New NSIconBrowser $win]
	set tree [NSIconBrowser::Info $browserId member,tree]
	NSList::OnSelection $tree \
		"NSSprite::SelectionChanged_Icon $oop %T %c %S %D"

	# Display progress while listing an icon type
	NSIconBrowser::Info $browserId clientCmd \
		"NSSprite::BrowserCmd $oop"

	# Display the icon type when the mouse is over a group icon
	NSIconBrowser::Info $browserId group,motionCmd \
		"NSSprite::BrowserMotionCmd $oop group"
	NSIconBrowser::Info $browserId group,leaveCmd \
		"NSSprite::BrowserMotionCmd $oop group"

	# Display the icon index when the mouse is over an icon
	NSIconBrowser::Info $browserId member,motionCmd \
		"NSSprite::BrowserMotionCmd $oop member"
	NSIconBrowser::Info $browserId member,leaveCmd \
		"NSSprite::BrowserMotionCmd $oop member"

	Info $oop browserId $browserId

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	# Progress bar used to display progress of listing icons
	set label [$win.statusBar itemcget t1 -label]
	set progId [NSObject::New NSProgress2 $label 225 10]
	[NSProgress2::Info $progId frame] configure -borderwidth 0
	Info $oop progId $progId

	# Clicking in label2 displays the icon assigned to the selected
	# member.
	bind [$win.statusBar itemcget t2 -label] <ButtonPress-1> \
		"NSSprite::DisplayIcon $oop"

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
	grid columnconfig $win 1 -weight 0 -minsize 0
	grid columnconfig $win 2 -weight 1 -minsize 0
 
	if {[Platform windows]} {
		grid [MakeDivider $win.divider1 x] \
			-row 0 -column 0 -rowspan 1 -columnspan 3 -sticky ew \
			-padx 0 -pady 2
	}
	grid $frame \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid [MakeDivider $win.divider2 y] \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky ns \
		-padx 4 -pady 2
	grid [NSIconBrowser::Info $browserId frame] \
		-row 1 -column 2 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 2 -column 0 -rowspan 1 -columnspan 3 -sticky ew

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSSprite $oop $win

	return
}

# NSSprite::InitMenus --
#
#	Create menus in the toplevel associated with this object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSSprite::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSSprite::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSSprite::MenuInvoke $oop"

	#
	# Sprite Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SPRITE
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_SPRITE -label "Sprite" -underline 0 -identifier M_SPRITE

	set entries {}
	lappend entries [list -type command -label "New Sprite" -accelerator n \
		-identifier E_NEW]
	lappend entries [list -type command -label "Insert Frame" \
		-accelerator plus -identifier E_INSERT_FRAME]
	lappend entries [list -type command -label "Delete Frame" \
		-accelerator minus -identifier E_DELETE_FRAME]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_SPRITE $entries

	set MenuString(M_SPRITE) \
		"Contains commands for using sprites."
	set MenuString(E_NEW) \
		"Creates a new sprite."
	set MenuString(E_INSERT_FRAME) \
		"Adds a new frame to the selected sprite."
	set MenuString(E_DELETE_FRAME) \
		"Removes the selected frame from the selected sprite."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSSprite::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::SetupMenus {oop mbarID} {

	set spriteIndex [Info $oop sprite,current]
	set frameIndex [Info $oop frame,current]

	lappend identList E_NEW E_CLOSE
	if {$spriteIndex != -1} {
		lappend identList E_INSERT_FRAME
		if {($frameIndex != -1) && ([sprite count $spriteIndex] > 2)} {
			lappend identList E_DELETE_FRAME
		}
	}

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSSprite::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		{} {
			set desc {}
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

# NSSprite::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_NEW {NewSprite $oop}
		E_INSERT_FRAME {NewFrame $oop}
		E_DELETE_FRAME {DeleteFrame $oop}
		E_CLOSE {Close $oop}
	}

	return
}

# NSSprite::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			Map $oop
		}
		postDisplay {
			if {$first} {
				set browserId [Info $oop browserId]
				set tree [NSIconBrowser::Info $browserId group,tree]
				NSIconBrowser::SetList_Group $browserId
				$tree select add "root child 0"
			}
		}
		postWithdraw {
			Unmap $oop
		}
	}

	return
}

# NSSprite::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::Close {oop} {

	NSWindowManager::Undisplay sprite

	return
}

# NSSprite::Map --
#
#	Do something when the toplevel is mapped.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::Map {oop} {

	SetSpriteList $oop

	# Restore the selected sprite
	set current [Info $oop sprite,current2]
	set treeS [Info $oop sprite,tree]
	if {$current != -1} {
		$treeS selection add "root child $current"
		$treeS see "root child $current"
	}

	# Restore the selected frame
	set current [Info $oop frame,current2]
	set treeF [Info $oop frame,tree]
	if {$current != -1} {
		$treeF selection add "root child $current"
		$treeF see "root child $current"
	}


	return
}

# NSSprite::Unmap --
#
#	Do something when unmapping the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::Unmap {oop} {

	# Because we are clearing the lists here, and don't want to
	# upset the user, we save the selected sprite/frame so it can
	# be restored in Map() below.
	Info $oop sprite,current2 [Info $oop sprite,current]
	Info $oop frame,current2 [Info $oop frame,current]

	NSList::Clear [Info $oop sprite,tree]

	return
}

# NSSprite::SetSpriteList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::SetSpriteList {oop} {

	set win [Info $oop win]
	set tree [Info $oop sprite,tree]

	# Clear the list
	NSList::Clear $tree

	# Get the number of sprites */
	set max [sprite count]

	# Add each sprite to the list
	for {set i 0} {$i < $max} {incr i} {

		# Append match to the list
		set item [$tree item create]
		NSList::SetIcon $tree $item "sprite $i"
		$tree item lastchild root $item
	}

	return
}

# NSSprite::SelectionChanged_Sprite --
#
#	When a sprite is selected, display the frames (icons) in that sprite.
#	Otherwise clear the frames list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::SelectionChanged_Sprite {oop tree count select deselect} {

	set win [Info $oop win]

	# No sprite was selected
	if {![llength $select]} {

		# Clear the frame list
		set treeF [Info $oop frame,tree]
		NSList::Clear $treeF

		# Disable controls
		$win.frame.frameConfig.scaleSpeed set 100
		$win.frame.frameConfig.scaleSpeed configure \
			-label "Frame Delay Factor: 100" -state disabled
		if {$::UseTile} {
			$win.frame.frameConfig.reverse state !selected
		} else {
			$win.frame.frameConfig.reverse deselect
			$win.frame.frameConfig.reverse configure \
				-state disabled
		}

		# No sprite is selected now
		Info $oop sprite,current -1
		return
	}

	# Get the (first) cell
	set item [lindex $select 0]
	set index [$tree item order $item -visible]

	# Display icons in that sprite
	SetFrameList $oop $index

	# Show delay and reverse info
	set delay [expr {[sprite configure $index -delay] / 5}]
	Info $oop delay $delay
	set win [Info $oop win]
	$win.frame.frameConfig.scaleSpeed configure \
		 -state normal -label "Frame Delay Factor: $delay"
	$win.frame.frameConfig.reverse configure \
		-state normal
	Info $oop reverse [sprite configure $index -reverse]

	# Remember which sprite is selected
	Info $oop sprite,current $index

	return
}

# NSSprite::SelectionChanged_Frame --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::SelectionChanged_Frame {oop tree count select deselect} {

	set win [Info $oop win]

	if {![llength $select]} {

		$win.statusBar itemconfigure t2 -text ""

		# No frame is selected now
		Info $oop frame,current -1
		return
	}

	# Remember which frame is selected
	set item [lindex $select 0]
	set frameIndex [$tree item order $item -visible]
	Info $oop frame,current $frameIndex

	# Get the icon for that frame
	set icon [lindex [sprite get [Info $oop sprite,current] $frameIndex] 0]

	# Select the frame in the icon browser, if the type is shown
	set browserId [Info $oop browserId]
	set iconType [NSIconBrowser::Info $browserId iconType]
	if {[string equal $iconType [lindex $icon 0]]} {
		NSIconBrowser::SeeIcon $browserId {*}$icon
	}

	# Display the type of icon
	$win.statusBar itemconfigure t2 -text $icon

	return
}

# NSSprite::SelectionChanged_Icon --
#
#	When an icon is selected, change the icon assigned to the selected
#	frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::SelectionChanged_Icon {oop tree count select deselect} {

	# Don't assign stuff during feedback
	if {[Info $oop ignoreSel]} return

	# Do nothing if no new cell was selected
	if {![llength $select]} return

	set selectedSprite [Info $oop sprite,current]
	if {$selectedSprite < 0} return
	set selectedFrame [Info $oop frame,current]
	if {$selectedFrame < 0} return

	# Get the (first) cell
	set item [lindex $select 0]
	set index [$tree item order $item -visible]

	# Get the type of icon to assign
	set browserId [Info $oop browserId]
	set iconType [NSIconBrowser::Info $browserId iconType]

	# Assign the icon to the frame
	sprite assign $selectedSprite $selectedFrame -type $iconType -index $index

	# Scroll the frame into view if it isn't already
	set treeF [Info $oop frame,tree]
	$treeF see "root child $selectedFrame"

	# Update the frame list
	NSList::SetIcon $treeF "root child $selectedFrame" "icon $iconType $index"

	return
}

# NSSprite::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSSprite::SetFrameList --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::SetFrameList {oop spriteIndex} {

	set tree [Info $oop frame,tree]

	# Clear the list
	NSList::Clear $tree

	# Add each frame to the list
	foreach frame [sprite get $spriteIndex] {

		# Append icon to the list
		set item [$tree item create]
		NSList::SetIcon $tree $item "icon [lindex $frame 0] [lindex $frame 1]"
		$tree item lastchild root $item
	}

	return
}

# NSSprite::NewSprite --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::NewSprite {oop} {

	set tree [Info $oop sprite,tree]

	# Every sprite must have at least 2 frames, or else!
	set spriteIndex [sprite create]
	sprite insert $spriteIndex 0 -type default -index 0
	sprite insert $spriteIndex 1 -type default -index 0
	sprite configure $spriteIndex -delay [expr {50 * 5}]

	# Display the new sprite and select it
	SetSpriteList $oop

	$tree activate "root child $spriteIndex"
	$tree selection add active
	$tree see active

	# Select the first new frame
	set tree [Info $oop frame,tree]
	NSList::Activate $tree "root child 0"

	return
}

# NSSprite::NewFrame --
#
#	Insert a new (blank) frame into the selected sprite.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::NewFrame {oop} {

	set selectedSprite [Info $oop sprite,current]
	if {$selectedSprite < 0} return

	set tree [Info $oop frame,tree]

	set frameIndex [Info $oop frame,current]
	if {$frameIndex == -1} {
		set frameIndex [sprite count $selectedSprite]
	}
	sprite insert $selectedSprite $frameIndex -type default -index 0

	# Display icons in that sprite
	SetFrameList $oop $selectedSprite

	NSList::Activate $tree "root child $frameIndex"

	return
}

# NSSprite::DeleteFrame --
#
#	Delete the selected frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::DeleteFrame {oop} {

	set spriteIndex [Info $oop sprite,current]
	if {$spriteIndex < 0} return
	set frameIndex [Info $oop frame,current]
	if {$frameIndex < 0} return

	# Program bombs if a sprite has less than 2 frames
	if {[sprite count $spriteIndex] == 2} {
		tk_messageBox -icon warning -title "Delete Frame" \
			-message "A sprite must have 2 frames or else!"
		return
	}

	set tree [Info $oop frame,tree]
	sprite delete $spriteIndex $frameIndex
	if {$frameIndex >= [sprite count $spriteIndex]} {
		incr frameIndex -1
	}

	# Display icons in that sprite
	SetFrameList $oop $spriteIndex

	NSList::Activate $tree "root child $frameIndex"

	return
}

# NSSprite::ConfigSprite --
#
#	Sets the frame delay of the selected sprite.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::ConfigSprite {oop option value} {

	set selectedSprite [Info $oop sprite,current]
	if {$selectedSprite < 0} return

	if {$option eq "delay"} {
		set win [Info $oop win]
		$win.frame.frameConfig.scaleSpeed configure \
			-label "Frame Delay Factor: $value"
		set value [expr {$value * 5}]
	}

	sprite configure $selectedSprite -$option $value

	return
}

# NSSprite::DisplayIcon --
#
#	When statusBar.label2 is clicked, we display the icon of the
#	selected frame, if any.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::DisplayIcon {oop} {

	set frameIndex [Info $oop frame,current]
	if {$frameIndex == -1} return

	set browserId [Info $oop browserId]
	set icon [lindex [sprite get [Info $oop sprite,current] $frameIndex] 0]
	scan $icon "%s %d" iconType iconIndex
	if {[lsearch -exact [NSIconBrowser::Info $browserId iconTypes] $iconType] != -1} {
		Info $oop display,ignoreSel 1
		NSIconBrowser::SeeIcon $browserId $iconType $iconIndex
		Info $oop display,ignoreSel 0
	}

	return
}

# NSSprite::Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::Motion {oop which x y} {

	set tree [Info $oop $which,tree]

	# Get the hit item
	set ident [$tree identify $x $y]
	if {[lindex $ident 0] eq "item"} {
		set item [lindex $ident 1]
		set index [$tree item order $item -visible]
	} else {
		set index -1
	}

	if {$index == [Info $oop $which,lastIndex]} {
		return
	}
	Info $oop $which,lastIndex $index

	if {$index == -1} {
		StatusBar $oop "" 0
		return
	}

	switch -- $which {
		frame {
		
			# Get the icon for that frame
			set icon [lindex [sprite get [Info $oop sprite,current] $index] 0]
		
			# Display the type of icon
			StatusBar $oop $icon 0
		}
	}
	
	return
}

# NSSprite::Leave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::Leave {oop which} {

	set win [Info $oop win]
	
	set index -1

	if {$index == [Info $oop $which,lastIndex]} {
		return
	}
	Info $oop $which,lastIndex $index

	StatusBar $oop "" 0

	return
}

# NSSprite::BrowserCmd --
#
#	Called by NSIconBrowser when displaying an icon type. Display
#	the progress of listing the icons. Note that this can
#	actually slow down listing the icons.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::BrowserCmd {oop action args} {

	set win [Info $oop win]
	set progId [Info $oop progId]
	
	switch -- $action {
		open {
			StatusBar $oop {} 0
			NSProgress2::Zero $progId
			pack [NSProgress2::Info $progId frame] -pady 1 -fill x
			update idletasks
			Info $oop oldLabel2 [$win.statusBar itemcget t2 -text]
		}
		update {
			set cur [lindex $args 0]
			set max [lindex $args 1]
			set bump [expr {(($max / 20) > 40) ? ($max / 20) : 40}]
			if {$cur && ($cur % $bump) == 0} {
				NSProgress2::SetDoneRatio $progId [expr {$cur / double($max)}]
				$win.statusBar itemconfigure t2 -text "$cur/$max"
				update idletasks
			}
		}
		close {
			NSProgress2::SetDoneRatio $progId 1.0
			update idletasks
			pack forget [NSProgress2::Info $progId frame]
			$win.statusBar itemconfigure t2 -text [Info $oop oldLabel2]
		}
	}

	return
}

# NSSprite::BrowserMotionCmd --
#
#	Called by NSIconBrowser.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::BrowserMotionCmd {oop which index} {

	set win [Info $oop win]
	set browserId [Info $oop browserId]

	# No icon was hit
	if {$index == -1} {
		StatusBar $oop "" 0
		return
	}

	# The mouse moved over the group canvas (ie, icon types)
	if {$which eq "group"} {
		set iconType [lindex [NSIconBrowser::Info $browserId iconTypes] $index]
		StatusBar $oop  "Icon type \"$iconType\"." 0
		return
	}

	set iconType [NSIconBrowser::Info $browserId iconType]
	set iconSpec "$iconType $index"

	StatusBar $oop "$iconSpec" 0

	return
}

# NSSprite::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSprite::IconCfg {oop} {

	NSIconBrowser::InitModule ; # FIXME
	set iconHeight [expr {$::NSIconBrowser::Priv(maxIconHeight) + 8}]
	[Info $oop frame,tree] configure -height $iconHeight

	if {[winfo ismapped [Info $oop win]]} {
		SetSpriteList $oop
	} else {
		Info $oop sprite,current2 -1
		Info $oop frame,current2 -1
	}

	return
}

proc DumpSprites {} {

	set win .sprites
	toplevel $win
	set text [text $win.text]
	pack $text

	set count [sprite count]
	for {set i 0} {$i < $count} {incr i} {
		$text insert end "set spriteId \[sprite create]\n"
		set j 0
		foreach frame [sprite get $i] {
			$text insert end "sprite insert \$spriteId $j -type [lindex $frame 0] -index [lindex $frame 1]\n"
			incr j
		}
		set delay [sprite configure $i -delay]
		set reverse [expr {[sprite configure $i -reverse] ? "yes" : "no"}]
		$text insert end "sprite configure \$spriteId -delay $delay -reverse $reverse\n\n"
	}

	return
}

