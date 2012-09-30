# File: updownentry.tcl

# Purpose: MS Windows-like entry + up-down control

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSUpDownEntry {

	variable Priv

	variable optionTable [list \
	]
}

# NSUpDownEntry::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::InitModule {} {

	NSModule::LoadIfNeeded NSUpDownControl

	return
}

# NSUpDownEntry::NSUpDownEntry --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::NSUpDownEntry {oop path args} {

	variable optionTable
	variable Priv

	set frame $path
	frame $frame -borderwidth 2 -relief sunken
	set frameCmd upDownEntry$frame
	rename $frame $frameCmd
	set Priv(oop,$path) $oop

	# This is the command seen by the outside
	set command $path
	interp alias {} ::$command {} ::NSUpDownEntry::Command $oop
	Info $oop command $command

	# The entry
	entry $frame.entry -borderwidth 0

	# The up-down control
	updowncontrol $frame.udc -widtharrow 3 -heightarrow 2 \
		-width 11 -height [winfo reqheight $frame.entry]

	pack $frame.entry -side left -fill x
	pack $frame.udc -side right -fill y

	NSUtils::DestroyObjectWithWidget NSUpDownEntry $oop $frame

	Info $oop frame $frame
	Info $oop frameCmd $frameCmd
	Info $oop entry $frame.entry
	Info $oop udc $frame.udc
	
	foreach {type arg info default flags} $optionTable {
		Info $oop $info $default
	}

	eval Configure $oop $args
		
	return
}

# NSUpDownEntry::~NSUpDownEntry --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::~NSUpDownEntry {oop} {

	return
}

# NSUpDownEntry::Command --
#
#	This is the command called for an up-down control. The widget pathname
#	command is a wrapper around this.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::Command {oop command args} {

	switch -- $command {
		cget -
		delete -
		icursor -
		insert -
		selection {
			return [eval [Info $oop entry] $command $args]
		}
		configure {
			return [eval Configure $oop $args]
		}
		entry {
			return [eval [Info $oop entry] $args]
		}
		info {
			return [eval Info $oop $args]
		}
		udc {
			return [eval [Info $oop udc] $args]
		}
		default {
			error "unknown command \"$command\""
		}
	}
	
	return
}

# NSUpDownEntry::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::Info {oop info args} {

	global NSUpDownEntry

	# Verify the object
	NSObject::CheckObject NSUpDownEntry $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSUpDownEntry($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSUpDownEntry($oop,$info)
			}
		}
	}

	return
}

# NSUpDownEntry::Configure --
#
#	Change configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::Configure {oop args} {

	variable optionTable

	if {[llength $args] & 1} {
		error "wrong number of arguments: must be \"[Info $oop command] configure ?option value?...\""
	}

	foreach {type arg info default flags} $optionTable {
		foreach flag $flags {
			set doFlag($flag) 0
		}
	}

	foreach error {0 1} {

		# First pass: Set options to new values
		if {!$error} {
			foreach {option value} $args {
				set skipIt 0
				switch -- $option {
					-command {
						[Info $oop udc] configure $option $value
						set skipIt 1
					}
					-font -
					-width -
					-state {
						[Info $oop entry] configure $option $value
						set skipIt 1
					}
				}
				if {!$skipIt} {
					set match 0
					foreach {type arg info default flags} $optionTable {
						if {[string equal $option $arg]} {
							set savedOptions($info) [Info $oop $info]
							if {[string equal $type boolean]} {
								set value [NSUtils::GetBoolean $value]
							}
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

# NSUpDownEntry::WorldChanged --
#
#	Called when configuration options change.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownEntry::WorldChanged {oop} {

	return
}

# updownentry --
#
#	Call this to create a new up-down entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc updownentry {args} {

	set oop [eval NSObject::New NSUpDownEntry $args]
	return [NSUpDownEntry::Info $oop command]
}


