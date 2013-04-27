# File: menu.tcl

# Purpose: menu management

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMenu {

	variable Priv
	
	set Priv(debug) 0
	
	# Option: Use array instead of list for identifiers
	variable IdentArray 0

	if {![llength [info commands menuentrystate]]} {
		proc ::menuentrystate {menu index args} {
			if {[llength $args]} {
				return [$menu entryconfigure $index -state [lindex $args 0]]
			} else {
				return [$menu entrycget $index -state]
			}
		}
	}

# namespace eval NSMenu
}

# NSMenu::NSMenu --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	oop					OOP ID returned by NSObject::New
#	parent				OOP ID of parent menu or toplevel window pathname
#	menuDef				Menu definition

proc NSMenu::NSMenu {oop parent args} {

	global NSMenu
	variable Priv

	if {$Priv(debug)} {
		Debug "Parsing OOP ID #$oop > $args"
	}
	
	set Priv(postcommand) {}
	set Priv(ident) {}
	set Priv(entries) {}
	set Priv(options) {}
	eval _MenuParseMenu $args

	# Popup menu
	if {[string equal [string index $parent 0] "*"]} {

		set parent [string range $parent 1 end]
		set m $parent.menu$oop
		set topOop $oop

	} elseif {[string compare [string index $parent 0] "."]} {

		set topOop $NSMenu($parent,topOop)

		set m $NSMenu($parent,menu).menu$oop

		# Append OOP ID to menubar's list of OOP ID's
		lappend NSMenu($topOop,subOop) $oop

	} else {

		set m $parent.menubar$oop
		$parent configure -menu $m

		set topOop $oop
#		lappend NSMenu($topOop,subOop) $oop
	}

	if {$Priv(debug)} {
		Debug "Making menu $m $Priv(options)"
	}
	
	eval menu $m $Priv(options)

	if {$topOop == $oop} {
		$m configure -postcommand "NSMenu::_MenuPostCommand $topOop"
	} elseif {$Priv(postcommand) != {}} {
		$m configure -postcommand $Priv(postcommand)
	} elseif {[Platform unix]} {
		# Linux doesn't call the menubar's postcommand before
		# interacting with menus
		$m configure -postcommand "NSMenu::_MenuPostCommand $topOop"
	}

	set NSMenu($oop,menu) $m
	set NSMenu($oop,postcommand) $Priv(postcommand)
	set NSMenu($oop,ident) $Priv(ident)
	set NSMenu($oop,subIdent) {}
	set NSMenu($oop,subOop) {}
	set NSMenu($oop,topOop) $topOop
	set NSMenu($oop,menuSelectCmd) {}
	set NSMenu($oop,invokeCmd) {}
	set NSMenu($oop,setupMode) disabled
	set NSMenu($oop,setupCmd) {}

	# Call client command when entries are selected
	bind $m <<MenuSelect>> "NSMenu::MenuSelect $oop %W"
	
	# Destroy the object along with the widget (later)
	NSUtils::DestroyObjectWithWidget NSMenu $oop $m

	return
}

# NSMenu::~NSMenu --
#
#	Object destructor called by NSObject::Delete.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::~NSMenu {oop} {

	global NSMenu

if 0 {
	# When you bind to a menu, the bindings are also applied to
	# the clone of the menu. So I have to be careful not to
	# delete the menu object more than once.
	if {![info exists NSMenu($oop,topOop)]} {
		return
	}
}

	set topOop $NSMenu($oop,topOop)
	set index [lsearch -exact $NSMenu($topOop,subOop) $oop]
	if {$index == -1} {
		return
	}

	set NSMenu($topOop,subOop) [lreplace $NSMenu($topOop,subOop) $index $index]

	return
}

# NSMenu::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::Info {oop info args} {

	global NSMenu

	# Verify the object
	NSObject::CheckObject NSMenu $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMenu($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMenu($oop,$info)
			}
		}
	}

	return
}

# NSMenu::MenuInsertEntry --
#
#	Search menu tree for given identifier, then parse the menu
#	entry definition and insert the new entry before the entry
#	specified by the identifier.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::MenuInsertEntry {top pos ident args} {

	variable Priv

	if {$Priv(debug)} {
		Debug "MenuInsertEntry $top $pos $ident > $args"
	}
	
	set entry [MenuFindEntry $top $ident]
	if {$entry == {}} return

	set oop [lindex $entry 0]
	set index [lindex $entry 1]

	switch -- $pos {

		-first {set index 0}
		-end {set index end}
		-append {set index 1000}
		-before {}
		-after {incr index}
	}

	if {$index == "IS_MENU"} {
	}

	eval _MenuInsertEntry $oop $index $args

	return
}

# NSMenu::_MenuInsertEntry --
#
#	Parse entry definition and insert into menu widget.
#	Also insert matching identifier into list of identifiers.
#
# Arguments:
#	oop				OOP ID of this menu
#	beforeIdent				Entry identifier to insert just before, or end
#	entryDef				Menu entry definition
#
# Results:
#	What happened.

proc NSMenu::_MenuInsertEntry {oop index args} {

	global NSMenu
	variable Priv

	set menu $NSMenu($oop,menu)

	set Priv(command) {}
	set Priv(ident) {}
	set Priv(menu) {}
	set command [eval _MenuParseEntry $menu $index $args]

	if {$Priv(menu) != {}} {
		set topOop $NSMenu($oop,topOop)
		foreach subOop $NSMenu($topOop,subOop) {
			if {[string equal $NSMenu($subOop,ident) $Priv(menu)]} {
				lappend command -menu $NSMenu($subOop,menu)
			}
		}
	}

	# Hack -- MenuInvoke stuff
	if {$Priv(command) != {}} {
		lappend command -command $Priv(command)
	} elseif {$Priv(ident) != {}} {
		if {![string match "*sep*" $Priv(type)]} {
			lappend command -command "NSMenu::MenuInvoke $oop $Priv(ident)"
		}
	}

	eval $command

	if {$NSMenu::IdentArray} {
	if {$index == "end"} {
		set index [$menu index end]
	}
	set NSMenu($oop,subIdent,$Priv(ident)) $index
	} else {
	set NSMenu($oop,subIdent) [linsert $NSMenu($oop,subIdent) $index $Priv(ident)]
	}

	return
}

# NSMenu::MenuDeleteEntry --
#
#	Delete menu entry from menu widget and identifier from list
#	of identifiers.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::MenuDeleteEntry {top ident} {

	global NSMenu

	set entry [MenuFindEntry $top $ident]
	if {$entry == {}} return

	set oop [lindex $entry 0]
	set index [lindex $entry 1]
	set menu $NSMenu($oop,menu)

	if {$index == "IS_MENU"} {
		destroy $menu
	} else {
		$menu delete $index
		if {$NSMenu::IdentArray} {
		unset NSMenu($oop,subIdent,$ident)
		} else {
		set NSMenu($oop,subIdent) \
			[lreplace $NSMenu($oop,subIdent) $index $index]
		}
	}

	return
}

# NSMenu::_MenuSetupOne --
#
#	Disable all the entries in the given menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::_MenuSetupOne {oop} {

	global NSMenu

	set menu $NSMenu($oop,menu)
	
	set last [$menu index end]
	if {$last == "none"} return

	set command $NSMenu($oop,setupCmd)
	if {[string length $command]} {
		uplevel #0 $command $oop
	}
	set state $NSMenu($oop,setupMode)
	if {$state == ""} return
	
	for {set i 0} {$i <= $last} {incr i} {
		if {[$menu type $i] == "separator"} continue
		if {[string compare [$menu entrycget $i -state] $state]} {
			menuentrystate $menu $i $state
		}
	}

	return
}

# NSMenu::MenuSetup --
#
#	Set up each sub menu.
#
# Arguments:
#	toop						OOP ID of menubar
#
# Results:
#	What happened.

proc NSMenu::MenuSetup {toop} {

	global NSMenu

	set command $NSMenu($toop,setupCmd)
	if {[string length $command]} {
		uplevel #0 $command $toop
	}
	
	foreach subOop $NSMenu($toop,subOop) {
		_MenuSetupOne $subOop
	}

	return
}

# NSMenu::MenuEnable --
#
#	Search menu tree for given identifier and set state
#	of entry to normal.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::MenuEnable {oop identList} {

	global NSMenu
	variable Priv

	foreach ident $identList {

		set entry [MenuFindEntry $oop $ident]
		if {$entry == ""} continue

		set menuId [lindex $entry 0]
		set index [lindex $entry 1]
		set menu $NSMenu($menuId,menu)
		if {[string compare [$menu entrycget $index -state] normal]} {
			menuentrystate $menu $index normal
		}
	}

	return
}

# NSMenu::MenuMatchEntryOne --
#
#	Search given menu for identifiers matching pattern.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Return numerical index into menu or -1.

proc NSMenu::MenuMatchEntryOne {oop pattern} {

	global NSMenu

	set result {}

	if {![string match $pattern $NSMenu($oop,ident)]} {
		lappend result $ident
	}

	foreach ident $NSMenu($oop,subIdent) {
		if {![string match $pattern $ident]} {
			lappend result $ident
		}
	}

	return $result
}

# NSMenu::MenuMatchEntry --
#
#	Search given menu and submenus for identifiers matching pattern.
#
# Arguments:
#	oop					OOP ID of menubar
#	pattern					Pattern to match identifiers against
#
# Results:
#	Return list "oop index" or empty list.

proc NSMenu::MenuMatchEntry {oop pattern} {

	global NSMenu

	append result [MenuMatchPattern $oop $pattern]

	foreach subOop $NSMenu($oop,subOop) {
		append result [MenuMatchEntry $subOop $pattern]
	}

	return $result
}

# NSMenu::MenuFindEntryOne --
#
#	Search given menu only for identifier.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Return numerical index into menu or -1.

proc NSMenu::MenuFindEntryOne {oop ident} {

	global NSMenu

	if {[string equal $NSMenu($oop,ident) $ident]} {
		return IS_MENU
	}

	if {$NSMenu::IdentArray} {
	if {![info exists NSMenu($oop,subIdent,$ident)]} {return -1}
	return $NSMenu($oop,subIdent,$ident)
	} else {
	return [lsearch -exact $NSMenu($oop,subIdent) $ident]
	}
}

# NSMenu::MenuFindEntry --
#
#	Search given menu and submenus for identifier.
#
# Arguments:
#	oop					OOP ID of menubar
#	ident					identifier to look for
#
# Results:
#	Return list "oop index" or empty list.

proc NSMenu::MenuFindEntry {oop ident} {

	global NSMenu

	set topId $NSMenu($oop,topOop)
	if {[info exists NSMenu($topId,identArray,$ident)]} {
		return [lindex $NSMenu($topId,identArray,$ident) 0]
	}

	set index [MenuFindEntryOne $oop $ident]
	if {$index != -1} {
		return [list $oop $index]
	}

	foreach subOop $NSMenu($oop,subOop) {
		set index [MenuFindEntryOne $subOop $ident]
		if {$index != -1} {
			return [list $subOop $index]
		}
	}

	return ""
}

# NSMenu::_MenuParseMenu --
#
#	Given a menu definition, return a string such that "eval $string"
#	will create a menu widget.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::_MenuParseMenu {args} {

	variable Priv
 
	foreach {option value} $args {
	
		switch -- $option {
		
			-postcommand {set Priv(postcommand) $value}

			-identifier {set Priv(ident) $value}

			-entries {set Priv(entries) $value}

			-parent {set Priv(parent) $value}

			default {lappend Priv(options) $option $value}
		}
	}

	return
}

# NSMenu::_MenuParseEntry
#
#	Given a menu entry definition, return a list such that
#	"eval $list" will create a menu widget entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::_MenuParseEntry {menu index args} {

	variable Priv

	set result [list $menu insert $index]

    foreach {option value} $args {
    
        switch -- $option {
        
            -type {
            	lappend result $value
            	set Priv(type) $value
            }

            -menu {set Priv(menu) $value}

            -identifier {set Priv(ident) $value}

			-command {
				set Priv(command) $value
			}

            default {
				lappend result $option $value
			}
        }
    }

	if {$Priv(debug)} {
		Debug "_MenuParseEntry:\n  $entry\n  $result"
	}
	
    return $result
}

# NSMenu::_MenuPostCommand --
#
#	Called before posting a menu. Calls MenuSetup to configure the
#	state of each item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::_MenuPostCommand {toop} {

	global NSMenu
	variable Priv

	if {$Priv(debug)} {
		Debug "_MenuPostCommand $toop"
	}

	MenuSetup $toop

	set command $NSMenu($toop,postcommand)
	if {$command != {}} {
		uplevel #0 $command $toop
	}

	return
}

# NSMenu::MenuInsertEntries --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::MenuInsertEntries {top pos ident entries} {

	global NSMenu

	set entry [MenuFindEntry $top $ident]
	if {$entry == {}} return

	set oop [lindex $entry 0]
	set index [lindex $entry 1]

	switch -- $pos {

		-first {set index 0}
		-end {set index end}
		-append {set index 1000}
		-before {}
		-after {incr index}
	}

	if {$pos == "-end"} {
		set index [$NSMenu($oop,menu) index end]
		if {$index == "none"} {
			set index 0
		} else {
			incr index
		}
	}

	foreach entry $entries {
		eval _MenuInsertEntry $oop $index $entry
		incr index
	}

	return
}

# NSMenu::MenuSelect --
#
#	Called to handle the <<MenuSelect>> virtual event. This allows
#	the client to display help messages for menu entries.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::MenuSelect {oop menu} {

	global NSMenu

	# The clone menu is actually the one responding to the <<MenuSelect>>
	# virtual event, so I cannot use the object's menu to query the
	# active item!
#	set menu $NSMenu($oop,menu)

	# Get the client command
	set command $NSMenu($oop,menuSelectCmd)

	# If this menu doesn't have a menuSelectCmd, then use the command
	# for the topmost menu, if any.
	if {![string length $command]} {

		set topOop $NSMenu($oop,topOop)
		if {$oop == $topOop} {
			return
		}
		set command $NSMenu($topOop,menuSelectCmd)
		if {![string length $command]} {
			return
		}
	}

	# Get the index of the active menu entry
	set index [$menu index active]

	# No entry is selected
	if {$index == "none"} {
		set ident {}

	# An entry is selected
	} else {
		if {$index < [llength $NSMenu($oop,subIdent)]} {
			set ident [lindex $NSMenu($oop,subIdent) $index]
		} else {
			set ident $NSMenu($oop,ident)
		}
	}

	# Call the client command
	uplevel #0 $command $oop $index [list $ident]

	return
}

# NSMenu::MenuInvoke --
#
#	Called when a menu entry is invoked. Calls the client's invokeCmd
#	if present.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::MenuInvoke {oop ident} {

	global NSMenu

	# The clone menu is actually the one responding to the <<MenuSelect>>
	# virtual event, so I cannot use the object's menu to query the
	# active item!
#	set menu $NSMenu($oop,menu)

	# Get the client command
	set command $NSMenu($oop,invokeCmd)

	# If this menu doesn't have an invokeCmd, then use the command
	# for the topmost menu, if any.
	if {![string length $command]} {

		set topOop $NSMenu($oop,topOop)
		if {$oop == $topOop} {
			return
		}
		set command $NSMenu($topOop,invokeCmd)
		if {![string length $command]} {
			return
		}
	}

	# Call the client command
	uplevel #0 $command $oop [list $ident]

	return
}

# NSMenu::EntryConfigure --
#
#	Configure a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::EntryConfigure {top ident args} {

	set entry [MenuFindEntry $top $ident]
	if {$entry == {}} return

	set oop [lindex $entry 0]
	set index [lindex $entry 1]
	set menu [Info $oop menu]

	eval $menu entryconfigure $index $args

	return
}

# NSMenu::SetIdentArray --
#
#	A big hack. In an effort to speed lookup times, you can create a mapping
#	of identifiers -> menu,index. But if menu entries are added/deleted/moved
#	you must call this again.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::SetIdentArray {topId} {

	global NSMenu

	array unset NSMenu $topId,identArray,*
	foreach menuId $NSMenu($topId,subOop) {
		set index 0
		foreach ident $NSMenu($menuId,subIdent) {
			lappend NSMenu($topId,identArray,$ident) [list $menuId $index]
			incr index
		}
	}
	
	return
}

# NSMenu::BindAccels --
#
#	Extracts the acclerators from a heirarchy of menu widgets
#	and creates bindings on a list of windows which invoke the
#	menu item commands.
#
# Arguments:
#	oop					NSMenu OOP ID.
#	widgets				A list of widget pathnames
#	menu				Menu widget pathname to examine
#
# Results:
#	For any "Ctrl-keysym" or "keysym" accelerator options
#	found, a binding is created for <Control-keysym> or <Key-keysym>
#	for each of the widgets.

proc NSMenu::BindAccelsOne {oop widgets} {

	set menu [Info $oop menu]

	set lastIdx [$menu index last]
	if {"$lastIdx" == "none"} return

	set idx 0
	foreach ident [Info $oop subIdent] {

		# This is a command menu entry
		if {[$menu type $idx] == "command"} {

			# Get the acclerator
			set acc [$menu entrycget $idx -accelerator]
		    
		    if {$acc != ""} {
				if {$acc == "Esc"} {
					set acc Escape
				} elseif {$acc == "Del"} {
					set acc Delete
				} elseif {$acc == "`"} {
					if {[Platform unix]} {
						set acc grave
					}
				} elseif {$acc == "?"} {
					if {[Platform unix]} {
						set acc question
					}
				}
				if {[regsub -all {Ctrl\+(.)} $acc {\1} key]} {
			    	if {[string match {[A-Z]} $key]} {
			    		set key [string tolower $key]
			    	}
				    set acc "<Control-KeyPress-$key>"
				} else {
					set acc "<KeyPress-$acc>"
				}
if 0 {
				if {[regsub -all {(.)} $acc {\1} key]} {
			    	if {[string match {[A-Z]} $key]} {
			    		set key [string tolower $key]
			    	}
				    set acc <KeyPress-$key>"
				}
}
				foreach win $widgets {
				    bind $win $acc "NSMenu::TryInvoke $oop $ident ; break"
				}
			}
		}

		incr idx
	}

	return
}

proc NSMenu::BindAccels {top widgets} {

	foreach menuId [Info $top subOop] {
		BindAccelsOne $menuId $widgets
	}
	
	return
}

# NSMenu::TryInvoke --
#
#	Invoke a menu item. Has no effect if the menu item is disabled.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMenu::TryInvoke {oop ident} {

	set entry [MenuFindEntry $oop $ident]
	if {$entry == {}} return

	set menuId [lindex $entry 0]
	set index [lindex $entry 1]
	set menu [Info $menuId menu]
if 1 {
	set toop [Info $oop topOop]
	MenuSetup $toop

	set command [Info $toop postcommand]
	if {$command != {}} {
		uplevel #0 $command $toop
	}
} else {
	set command [$menu cget -postcommand]
	if {$command != {}} {
		uplevel #0 $command
	}
}

	$menu invoke $index

# So statusbar is "covered"
MenuSelect $toop [Info $toop menu]

	return
}

