# File: statusbar.tcl

# Purpose: A window statusbar

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSStatusBar {

	variable Priv
	variable Relief

	variable optionTable [list \
		"ints" "-sizes" "sizes" "1" doSizes \
		"ints" "-weights" "weights" "1" doWeights \
		"tags" "-tags" "tags" "tag1" doWeights \
	]

	if {[string equal $::tcl_platform(platform) unix]} {
		set font "Helvetica 12"
		set Relief sunken
	}
	if {[string equal $::tcl_platform(platform) windows]} {
		set font "{MS Sans Serif} 8"
		set Relief groove
	}

	variable optionTableItem [list \
		"font" "-font" "font" $font doFont \
		"string" "-text" "text" "" doText \
	]
}

# NSStatusBar::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::InitModule {} {

	return
}

# NSStatusBar::NSStatusBar --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::NSStatusBar {oop path args} {

	variable optionTable
	variable Priv

	frame $path \
		-borderwidth 0
	set frameCmd win98Canvas$path
	rename $path $frameCmd
	set Priv(oop,$path) $oop

	frame $path.pad \
		-borderwidth 0 -height 3
	label $path.cover \
		-borderwidth 0 -anchor nw -padx 2

	grid rowconfigure $path 0 -weight 0
	grid rowconfigure $path 1 -weight 0
	grid $path.pad \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $path.cover \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

	# This is the command seen by the outside
	set command $path
if 1 {
	interp alias {} ::$command {} ::NSStatusBar::Command $oop
} else {
	proc ::$command args \
		"eval NSStatusBar::Command $oop \$args"
}
	Info $oop command $command

	NSUtils::DestroyObjectWithWidget NSStatusBar $oop $path

	Info $oop frame $path
	Info $oop frameCmd $frameCmd
	Info $oop count 0

	foreach {type arg info default flags} $optionTable {
		Info $oop $info $default
	}

	eval Configure $oop $args

	return
}

# NSStatusBar::~NSStatusBar --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::~NSStatusBar {oop} {

	variable Priv

	catch {
		set frame [Info $oop frame]
		unset Priv(oop,$frame)
	}

	return
}

# NSStatusBar::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::Info {oop info args} {

	global NSStatusBar

	# Verify the object
	NSObject::CheckObject NSStatusBar $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSStatusBar($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSStatusBar($oop,$info)
			}
		}
	}

	return
}

# NSStatusBar::Command --
#
#	This is the command called for a toolbar button. The widget pathname
#	command is a wrapper around this.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::Command {oop command args} {

	switch -- $command {
		cget {
			# The Tk tabbing engine calls this!
			error "unknown option $args"
		}
		configure {
			return [eval Configure $oop $args]
		}
		cover {
			switch -- [lindex $args 0] {
				hide {
					grid remove [Info $oop frame].cover
				}
				set {
					[Info $oop frame].cover configure -text [lindex $args 1]
				}
				show {
					grid [Info $oop frame].cover
					raise [Info $oop frame].cover
				}
				default {
					error "unknown command \"$command [lindex $args 0]\""
				}
			}
		}
		frame {
			return [eval [Info $oop frameCmd] $args]
		}
		info {
			return [eval Info $oop $args]
		}
		itemcget {
			return [eval CgetItem $oop $args]
		}
		itemconfigure {
			return [eval ConfigureItem $oop $args]
		}
		default {
			error "unknown command \"$command\""
		}
	}
	
	return
}

# NSStatusBar::Configure --
#
#	Change configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::Configure {oop args} {

	variable optionTable

	foreach {type arg info default flags} $optionTable {
		foreach flag $flags {
			set doFlag($flag) 0
		}
	}

	foreach error {0 1} {

		# First pass: Set options to new values
		if {!$error} {
			foreach {option value} $args {
				set match 0
				foreach {type arg info default flags} $optionTable {
					if {$option == $arg} {
						set savedOptions($info) [Info $oop $info]
						Info $oop $info $value
						foreach flag $flags {
							set doFlag($flag) 1
						}
						set match 1
						break
					}
				}
				if {!$match} {
					error "unknown option \"$option\""
				}
			}

		# Second pass: restore options to old values.
		} else {
			foreach name [array names savedOptions] {
				Info $oop $name $savedOptions($name)
			}
		}

		# Success
		break
	}

	WorldChanged $oop

	if {$error} {
		error $errorString
	}

	return
}

if {[info tclversion] == 8.0} {
proc stringIsInteger {string} {
	return [regexp {^[0-9]+$} $string]
}
}

# NSStatusBar::ConfigureItem --
#
#	Change configuration options for a single item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::ConfigureItem {oop tagOrId args} {

	variable optionTableItem

	if {[info tclversion] == 8.0} {
		if {![stringIsInteger $tagOrId]} {
			set tagOrId [lsearch -exact [Info $oop tags] $tagOrId]
		}
	} else {
		if {![string is integer $tagOrId]} {
			set tagOrId [lsearch -exact [Info $oop tags] $tagOrId]
		}
	}
	if {($tagOrId < 0) || ($tagOrId >= [Info $oop count])} {
		error "no such item \"$tagOrId\""
	}
	set item item$tagOrId

	foreach {type arg info default flags} $optionTableItem {
		foreach flag $flags {
			set doFlag($flag) 0
		}
	}

	foreach error {0 1} {

		# First pass: Set options to new values
		if {!$error} {
			foreach {option value} $args {
				set match 0
				foreach {type arg info default flags} $optionTableItem {
					if {$option == $arg} {
						set savedOptions($info) [Info $oop $item,$info]
						Info $oop $item,$info $value
						foreach flag $flags {
							set doFlag($flag) 1
						}
						set match 1
						break
					}
				}
				if {!$match} {
					error "unknown option \"$option\""
				}
			}

		# Second pass: restore options to old values.
		} else {
			foreach name [array names savedOptions] {
				Info $oop $name $savedOptions($name)
			}
		}

		# Success
		break
	}

	WorldChanged $oop

	if {$error} {
		error $errorString
	}

	return
}

# NSStatusBar::CgetItem --
#
#	Return configuration info for a single item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::CgetItem {oop tagOrId option} {

	variable optionTableItem

	if {[info tclversion] == 8.0} {
		if {![stringIsInteger $tagOrId]} {
			set tagOrId [lsearch -exact [Info $oop tags] $tagOrId]
		}
	} else {
		if {![string is integer $tagOrId]} {
			set tagOrId [lsearch -exact [Info $oop tags] $tagOrId]
		}
	}
	if {($tagOrId < 0) || ($tagOrId >= [Info $oop count])} {
		error "no such item \"$tagOrId\""
	}
	set item item$tagOrId

	if {[string equal $option -label]} {
		return [Info $oop frame].label$tagOrId
	}

	foreach {type arg info default flags} $optionTableItem {
		if {[string equal $option $arg]} {
			return [Info $oop $item,$info]
		}
	}

	error "unknown option \"$option\""

	return
}

# NSStatusBar::WorldChanged --
#
#	Called when configuration options change.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusBar::WorldChanged {oop} {

	variable optionTableItem
	variable Relief

	set frame [Info $oop frame]

	if {![Info $oop count]} {
		set labelNum 0
		set column 0
		set item 0
		foreach size [Info $oop sizes] weight [Info $oop weights] tag [Info $oop tags] {
			if {$column} {
				grid columnconfigure $frame $column -weight 0
				grid [frame $frame.pad$labelNum -borderwidth 0 -width 2] \
					-row 1 -column $column -rowspan 1 -columnspan 1 -stick ns
				incr column
			}
			set label $frame.label$labelNum
			label $label \
				-anchor w -relief $Relief -borderwidth 1 -padx 2 -width $size
			grid columnconfigure $frame $column -weight $weight
			grid $label \
				-row 1 -column $column -rowspan 1 -columnspan 1 -sticky news

			foreach {type arg info default flags} $optionTableItem {
				Info $oop item$item,$info $default
			}

			incr column
			incr labelNum
			incr item
		}
		Info $oop count $item
		grid $frame.pad -columnspan $column
		grid $frame.cover -columnspan $column
		grid remove $frame.cover
	}

	# BUG -- It seems that when a Label widget is displayed, it clobbers
	# the display of any child in that label. This screws up my progressbar
	# hack (ex in the Assign Window).
	
	for {set i 0} {$i < [Info $oop count]} {incr i} {
		set font [Info $oop item$i,font]
		set text [Info $oop item$i,text]
		if {[string compare $text [$frame.label$i cget -text]]} {
			$frame.label$i configure -text $text -font $font
		} elseif {[string compare $font [$frame.label$i cget -font]]} {
			$frame.label$i configure -font $font
		}
	}

	return
}

# statusbar --
#
#	Call this to create a new statusbar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc statusbar {args} {

	set oop [eval NSObject::New NSStatusBar $args]
	return [NSStatusBar::Info $oop command]
}

