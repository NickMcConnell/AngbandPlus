# File: config.tcl

# Purpose: icon configuration management

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSConfig {

	variable Priv

# namespace eval NSConfig
}

# NSConfig::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::InitModule {} {

	# Read tk/config, which contains a list of icon configurations
	# and the current icon configuration.
	ReadConfigFile

	# Set the default set of files to pass to SourceOne. These
	# can be overridden by scripts to use common configuration
	# files. See ShareConfigFile() below.
	SetPrefix [Value config,prefix]

	return
}

# NSConfig::ReadConfigFile --
#
#	Reads the tk/config file, which holds a list of configuration
#	prefixes, along with descriptive text for each prefix. Each
#	prefix can be used to read and write certain icon configuration
#	files.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::ReadConfigFile {} {

	variable Priv

	if {[catch {open [PathTk config config]} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"config\" file for reading:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	while {![eof $fileId]} {

		# Read a line
		set count [gets $fileId list]
		if {$count == -1} break

		# Save the text, so it can be written out later
		lappend Priv(text) $list

		if {$count == 0} continue

		switch -- [lindex $list 0] {
			Config: {
				lappend Priv(config) [lindex $list 1] [lindex $list 2]
			}
		}
	}

	close $fileId

	return
}

# NSConfig::Load --
#
#	Processes the set of files for the "current" configuration set.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::Load {} {

	# Get the current configuration prefix
	set prefix [Global config,prefix]
	
	angband_load note $prefix

	# Try "prefix.cfg"
	SourceOne $prefix.cfg
	
	# Try prefixNN.cfg
	regsub {(16|24|32)} $prefix NN prefix
	SourceOne $prefix.cfg
	return
}

# NSConfig::InitIcons --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::InitIcons {iconSize} {

	angband init_icons $iconSize [winfo depth .]

	return
}

# NSConfig::SetPrefix --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::SetPrefix {prefix} {

	Value config,prefix $prefix

	Global config,prefix $prefix
	Global config,assign $prefix-assign
	Global config,town $prefix-town
	Global config,postop $prefix-postop

	return
}

# NSConfig::ShareConfigFile --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::ShareConfigFile {which file} {

	switch -- $which {
		assign -
		town {
			Global config,$which $file
		}

		default {
			error "unknown config file \"$which\""
		}
	}

	return
}

# NSConfig::SourceOne --
#
#	Looks for the given file in the tk/config directory. If it
#	exists, it is sourced at the global level. This command is
#	usually called from a icon configuration file, type ".cfg".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::SourceOne {fileName {required 0}} {

	set fileName [file tail $fileName]
	set path [PathTk config $fileName]
	if {[file exists $path]} {
		uplevel #0 source $path
		return
	}
	if {$required} {
		error "can't find file \"$fileName\""
	}

	return
}

# NSConfig::Source --
#
#	Looks for the given file in the tk/config directory. If it
#	exists, it is sourced in the given namespace. This command is
#	usually called from a icon configuration file, type ".cfg".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::Source {fileName namespace} {

	set fileName [file tail $fileName]
	set fileName [PathTk config $fileName]
	if {[file exists $fileName]} {
		${namespace}::Source $fileName
	}

	return
}

# NSConfig::FileLibData --
#
#	Takes the "tail" of the given file name, and appends it to the
#	complete pathname of the image directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::FileLibData {file} {

	set file [file tail $file]
	return [PathTk image $file]
}

# NSConfig::FindImage --
#
#	Find an image file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::FindImageFile {imageFile} {

	set path [PathTk image $imageFile]
	if {[file exists $path]} {
		return $path
	}
	set path [PathTk image dg [file tail $imageFile]]
	if {[file exists $path]} {
		return $path
	}
	error "icon image file \"$imageFile\" was not found"

	return
}


# NSConfig::NoMoreIcons --
#
#	This is a big silly hack called when all the icon types have
#	been created. It is used just so I can update the progress bar
#	during startup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::NoMoreIcons {} {

	global AngbandPriv
	
	set canvas $AngbandPriv(load,win).canvas
	$canvas itemconfigure message -text "Assigning icons..."
	update

	return
}


# Config::Assign --
#
#	A namespace with commands called when the tk/config/assign file
#	is sourced.
#

namespace eval Config::Assign {

	variable Priv

#namespace eval Config::Assign
}

# Evaluate a script
proc Config::Assign::Source {path} {

	source $path

	return
}

# Add an assign type
proc Config::Assign::Assign {type} {

	variable Priv

	lappend Priv(assignType) $type

	return
}

# Add an icon type
proc Config::Assign::Type {type} {

	variable Priv

	lappend Priv(type) $type

	return
}

# Start assigning to this group
proc Config::Assign::Group {group} {

	variable Priv

	set Priv(group) $group

	return
}

proc Config::Assign::Feat {light background} {

	variable Priv

	feature configure $Priv(member) -light $light -background $background

	return
}
