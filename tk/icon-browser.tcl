# File: icon-browser.tcl

# Purpose: an icon-type browser

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSIconBrowser {

	variable IconTypes

# namespace eval NSIconBrowser
}

# NSIconBrowser::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::InitModule {} {

	variable IconTypes
	variable Priv

	NSModule::LoadIfNeeded NSList

	set IconTypes {}

	# Also track max width of an icon type
	set maxWidth 0
	set maxHeight 0

	foreach iconType [icon gettypes] {
		switch -- $iconType {
			blank -
			default -
			none {
			}
			default {
				# Hack -- Don't show ascii type icons
				if {![icon ascii isascii $iconType]} {
					lappend IconTypes $iconType

					set width [icon width $iconType]
					if {$width > $maxWidth} {
						set maxWidth $width
					}

					set height [icon height $iconType]
					if {$height > $maxHeight} {
						set maxHeight $height
					}
				}
			}
		}
	}

	set Priv(maxIconWidth) $maxWidth
	set Priv(maxIconHeight) $maxHeight

	return
}

# NSIconBrowser::NSIconBrowser --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::NSIconBrowser {oop parent} {

	global NSIconBrowser
	variable IconTypes
	variable Priv

	Info $oop iconType ""
	Info $oop iconTypes $IconTypes
	Info $oop maxIconWidth $Priv(maxIconWidth)
	Info $oop maxIconHeight $Priv(maxIconHeight)

	# Call this command while displaying an icon type
	Info $oop clientCmd ""

	Info $oop group,leaveCmd {}
	Info $oop group,motionCmd {}

	Info $oop member,leaveCmd {}
	Info $oop member,motionCmd {}

	InitDisplay $oop $parent

	set groupTree [Info $oop group,tree]
	bindtags $groupTree \
		[concat [bindtags $groupTree] IconBrowserGroupBindTag$oop]
	bind IconBrowserGroupBindTag$oop <Motion> \
		"NSIconBrowser::Motion $oop group %x %y"
	bind IconBrowserGroupBindTag$oop <Leave> \
		"NSIconBrowser::Leave $oop group"
	Info $oop group,lastIndex -1

	set memberTree [Info $oop member,tree]
	bindtags $memberTree \
		[concat [bindtags $memberTree] IconBrowserMemberBindTag$oop]
	bind IconBrowserMemberBindTag$oop <Motion> \
		"NSIconBrowser::Motion $oop member %x %y"
	bind IconBrowserMemberBindTag$oop <Leave> \
		"NSIconBrowser::Leave $oop member"
	Info $oop member,lastIndex -1

	qebind NSIconBrowser$oop <IconCfg> \
		"NSIconBrowser::IconCfg $oop"

	return
}

# NSIconBrowser::~NSIconBrowser --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::~NSIconBrowser {oop} {

	qeunbind NSIconBrowser$oop

	return
}

# NSIconBrowser::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::Info {oop info args} {

	global NSIconBrowser

	# Verify the object
	NSObject::CheckObject NSIconBrowser $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSIconBrowser($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSIconBrowser($oop,$info)
			}
		}
	}

	return
}

# NSIconBrowser::InitDisplay --
#
#	Create our stuff.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::InitDisplay {oop parent} {

	global NSIconBrowser

	# This frame holds all our stuff
	set frame $parent.iconbrowser$oop
	frame $frame \
		-borderwidth 1 -relief sunken

	# So client can pack us
	Info $oop frame $frame

	#
	# Group List
	#

	set cellWidth [expr {[Info $oop maxIconWidth] + 8}]
	set cellHeight [expr {[Info $oop maxIconHeight] + 8}]

	set frameGroup $frame.frameGroup
	set tree [NSList::New $frameGroup -text 0 -icon 1]
	$frameGroup configure -borderwidth 0
	$tree configure -height 240 -width $cellWidth

	# When a group is selected, show the icons in it
	NSList::OnSelection $tree \
		"NSIconBrowser::SelectionChanged_Group $oop %T %c %S %D"

	Info $oop group,tree $tree

	#
	# Icon List
	#

	set frameIcon $frame.frameIcon
	NSList::New $frameIcon -text 0 -icon 1 -wrap 1
	$frameIcon configure -borderwidth 0
	set tree $frameIcon.tree
	$tree configure -width 240 -height 240

	Info $oop member,tree $tree

	#
	# Geometry
	#

	grid rowconfig $frame 0 -weight 1 -minsize 0
	grid columnconfig $frame 0 -weight 0 -minsize 0
	grid columnconfig $frame 1 -weight 1 -minsize 0
 
	grid $frameGroup \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $frameIcon \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSIconBrowser $oop $frame

	return
}


# NSIconBrowser::SelectionChanged_Group --
#
#	When an icon group is selected, display icons in that group.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::SelectionChanged_Group {oop tree count select deselect} {

	# Clear member list if no group is selected
	if {!$count} {
		set tree [Info $oop member,tree]
		NSList::Clear $tree
		return
	}

	# Get the (first) row
	set item [lindex [$tree selection get] 0]
	set row [NSList::Item2Row $tree $item]

	# Display icons in that group
	SetList_Member $oop [lindex [Info $oop iconTypes] $row]

	return
}

# NSIconBrowser::SetList_Group --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::SetList_Group {oop} {

	set tree [Info $oop group,tree]

	# Clear the list
	NSList::Clear $tree

	# Add each group
	foreach iconType [Info $oop iconTypes] {

		# Add this group to the list
		set item [$tree item create]
		NSList::SetIcon $tree $item "icon $iconType 0"
		$tree item lastchild root $item
	}

	# Hack -- Clear the icon list
	NSList::Clear [Info $oop member,tree]

	return
}

# NSIconBrowser::SetList_Member --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::SetList_Member {oop iconType} {

	set tree [Info $oop member,tree]
	set win [winfo toplevel $tree]

	# Get busy
	set cursor [$win cget -cursor]
	$win configure -cursor watch
	update

	# Call this *before* clearing the list
	set command [Info $oop clientCmd]
	if {[string length $command]} {
		uplevel #0 $command open
	}

	# Clear the list
	NSList::Clear $tree

	# Set cell size based on icon size
	set columnWidth [expr {[icon width $iconType] + 8}]
	set rowHeight [expr {[icon height $iconType] + 8}]

	# Get the number of icons of the given type */
	set max [icon count $iconType]

	if {1 || $max < 250} {

		# Collect info about each list item
		for {set i 0} {$i < $max} {incr i} {
			set item [$tree item create]
			NSList::SetIcon $tree $item "icon $iconType $i"
			$tree item lastchild root $item
		}

	} else {

		set bump [expr {$max / 20}]
		for {set i 0} {$i < $max} {incr i} {
			set item [$tree item create]
			NSList::SetIcon $tree $item "icon $iconType $i"
			$tree item lastchild root $item
			if {$i && ($i % $bump) == 0} {
				if {[string length $command]} {
					uplevel #0 $command update $i $max
				}
			}
		}
	}


	# Remember which type is currently displayed
	Info $oop iconType $iconType

	if {[string length $command]} {
		uplevel #0 $command close
	}

	# Display icons and restore the cursor
	update
	$win configure -cursor $cursor

	return
}

# NSIconBrowser::SeeIcon --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::SeeIcon {oop iconType iconIndex} {

	set row [lsearch -exact [Info $oop iconTypes] $iconType]

	# Select and display the group the icon is in
	set tree [Info $oop group,tree]
	NSList::Activate $tree "rnc $row 0"

	# Select and display the icon itself
	set tree [Info $oop member,tree]
	NSList::Activate $tree "root child $iconIndex"

	return
}

# NSIconBrowser::Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::Motion {oop which x y} {

	set tree [Info $oop $which,tree]

	# Get the hit cell
	set ident [$tree identify $x $y]
	if {($ident eq "") || ([lindex $ident 0] ne "item")} {
		set index -1
	} else {
		set item [lindex $ident 1]
		set index [NSList::Item2Row $tree $item]
	}

	if {$index == [Info $oop $which,lastIndex]} {
		return
	}
	Info $oop $which,lastIndex $index

	set command [Info $oop $which,motionCmd]
	if {[string length $command]} {
		uplevel #0 $command $index
	}

	return
}

# NSIconBrowser::Leave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::Leave {oop which} {

	set index -1

	if {$index == [Info $oop $which,lastIndex]} {
		return
	}
	Info $oop $which,lastIndex $index

	set command [Info $oop $which,leaveCmd]
	if {[string length $command]} {
		uplevel #0 $command -1
	}

	return
}

# NSIconBrowser::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconBrowser::IconCfg {oop} {

	variable IconTypes
	variable Priv

	# Get updated list of icon types
	# FIXME: Don't do it for every browser
	InitModule

	Info $oop iconTypes $IconTypes

	set cellWidth [expr {$Priv(maxIconWidth) + 8}]
	[Info $oop group,tree] configure -width $cellWidth

	SetList_Group $oop

	return
}
