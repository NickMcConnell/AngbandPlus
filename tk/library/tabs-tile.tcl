# File: tabs-tile.tcl

# Purpose: Tabs using ttk::notebook

#
# Copyright (c) 1997-2000 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTabs {

	variable Priv

# namespace eval NSTabs
}

proc NSTabs::NSTabs {oop parent} {

	Info $oop parent $parent
	Info $oop nextId 0
	Info $oop id {}
	Info $oop totalWidth 0
	Info $oop tabWidth {}
	Info $oop current ""
	Info $oop invokeCmd {}
	Info $oop rightClickCmd {}
	Info $oop validateCmd {}
	Info $oop active 0

	label $parent.xyz
	set font [$parent.xyz cget -font]
	destroy $parent.xyz
	Info $oop font $font

	ttk::style configure TNotebook.Tab -font $font

	InitDisplay $oop

	set nb [Info $oop canvas]
	bindtags $nb [concat TabsBindTag($oop) [bindtags $nb]]
	bind TabsBindTag($oop) <Destroy> "
		NSObject::Delete NSTabs $oop
	"
	bind TabsBindTag($oop) <ButtonPress-1> "
		set index \[%W index @%x,%y]
		if {\$index ne \"\"} {
			NSTabs::Invoke $oop \[lindex \[%W tabs] \$index]
		}
		break
    "
	bind TabsBindTag($oop) <ButtonPress-3> "
		NSTabs::RightClick $oop %x %y %X %Y
		break
    "
#	bind TabsBindTag($oop) <<NotebookTabChanged>> "NSTabs::Invoke $oop"

	return
}

proc NSTabs::Info {oop info args} {

	global NSTabs

	# Verify the object
	NSObject::CheckObject NSTabs $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSTabs($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSTabs($oop,$info)
			}
		}
	}

	return
}

proc NSTabs::InitDisplay {oop} {

	variable Priv

	set parent [Info $oop parent]
	set nb $parent.tabs$oop
	ttk::notebook $nb

	# Remember the notebook
	Info $oop canvas $nb

	# Right-click menu
	menu $nb.contextMenu -tearoff 0
	Info $oop contextMenu $nb.contextMenu

	return
}

proc NSTabs::Add {oop text {child ""}} {

	variable Priv

	set xoffset 2

	set nb [Info $oop canvas]
	if {$child eq ""} {
		set index [Info $oop nextId]
		set tabId $nb.tab$index
		Info $oop nextId [expr {$index + 1}]
	} else {
		set tabId $child
	}

	set id [Info $oop id]
	Info $oop id [lappend id $tabId]

	if {$child eq ""} {
		set child [frame $tabId]
	}
	$nb add $child -text $text

	return $tabId
}

proc NSTabs::SetText {oop tabId text} {

	set nb [Info $oop canvas]

	if {$tabId ni [Info $oop id]} {
		error "can't find tab with id \"$tabId\""
	}

	# Set the text
	$nb tab $tabId -text $text

	return
}

proc NSTabs::GetText {oop tabId} {

	set nb [Info $oop canvas]

	set index [GetTabIndex $oop $tabId]
	return [$nb tab $tabId -text]
}

proc NSTabs::Remove {oop tabId} {

	set nb [Info $oop canvas]

	set index [GetTabIndex $oop $tabId]

	$nb forget $tabId
	destroy $tabId

	set id [lreplace [Info $oop id] $index $index]
	Info $oop id $id

	# If the selected tab is removed, select the first tab
	if {$tabId == [Info $oop current]} {
		if {[llength $id]} {
			Bigger $oop [lindex $id 0]
			Info $oop current [lindex $id 0]
		} else {
			Info $oop current ""
		}
	}

	return
}

proc NSTabs::Invoke {oop {tabId ""}} {

	set nb [Info $oop canvas]
	if {$tabId eq ""} {
		set tabId [$nb select]
	}
	if {$tabId ni [Info $oop id]} {
		error "can't find tab with id \"$tabId\""
	}

	if {$tabId eq [Info $oop current]} return

	if {[Info $oop active]} {
		$nb select $tabId
		Info $oop current $tabId
	}

	set command [Info $oop validateCmd]
	if {[string length $command]} {
		if {[uplevel #0 $command $oop]} {
			return
		}
	}

	set command [Info $oop invokeCmd]
	if {[string length $command]} {
		uplevel #0 $command $oop $tabId
	}

	return
}

proc NSTabs::Bigger {oop tabId} {

	set nb [Info $oop canvas]

	if {$tabId ni [Info $oop id]} {
		error "can't find tab with id \"$tabId\""
	}

	$nb select $tabId

	return
}

proc NSTabs::Smaller {oop tabId} {

	set nb [Info $oop canvas]

	if {$tabId eq ""} return

	if {$tabId ni [Info $oop id]} {
		error "can't find tab with id \"$tabId\""
	}

	return
}

proc NSTabs::RightClick {oop x y X Y} {

	set command [Info $oop rightClickCmd]
	if {[string length $command]} {
		set nb [Info $oop canvas]

		set index [$nb index @$x,$y]
		if {$index eq ""} return
		set tabId [lindex [$nb tabs] $index]

		set menu [Info $oop contextMenu]
		$menu delete 0 end
		eval destroy [winfo children $menu]

		uplevel #0 $command $oop $tabId $X $Y $menu
	}

	return
}

proc NSTabs::GetNthId {oop index} {

	set id [Info $oop id]

	if {($index < 0) || ($index >= [llength $id])} {
		error "bad tab index \"$index\": must be between 0 and [expr {[llength $id] - 1}]"
	}
	return [lindex $id $index]
}

proc NSTabs::GetTabIndex {oop tabId} {

	set tabIds [Info $oop id]
	set index [lsearch -exact $tabIds $tabId]
	if {$index == -1} {
		error "bad tabId \"$tabId\": must be one of $tabIds"
	}
	return $index
}
