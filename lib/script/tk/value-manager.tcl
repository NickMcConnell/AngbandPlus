# File: value-manager.tcl

# Purpose: the Value Manager

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# The Value Manager records various values, and informs clients when those
# values are modified.

namespace eval NSValueManager {

	variable Command
	variable Value
	variable ValuePriv

# namespace eval NSValueManager
}

# NSValueManager::InitModule --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::InitModule {} {

	global Angband
	variable Value
	variable ValuePriv

	# Each client gets a unique ID
	set ValuePriv(clientId) 0

	# These are the default values. The tk/config/value file is sourced
	# below to set user-defined values.
	
	Manage listBG [palette set 253]
	Manage listHilite [palette set 158]
	Manage listInactive [palette set 249]

	# These are the standard Angband terminal colors mapped to
	# entries in our 256-color palette. They can be accessed via
	# "Value TERM_XXX".
	
	Manage TERM_DARK [palette set 255]
	Manage TERM_WHITE [palette set 0]
	Manage TERM_SLATE [palette set 250]
	Manage TERM_ORANGE [palette set 17]
	Manage TERM_RED [palette set 217]
	Manage TERM_GREEN [palette set 227]
	Manage TERM_BLUE [palette set 204]
	Manage TERM_UMBER [palette set 101]
	Manage TERM_L_DARK [palette set 129]
	Manage TERM_L_WHITE [palette set 247]
	Manage TERM_VIOLET [palette set 30]
	Manage TERM_YELLOW [palette set 5]
	Manage TERM_L_RED [palette set 35]
	Manage TERM_L_GREEN [palette set 185] ; # ~LawnGreen
	Manage TERM_L_BLUE [palette set 180] ; # ~turquoise2
	Manage TERM_L_UMBER [palette set 52]
	
	Manage tip,current 1000
	Manage tip,show 1

	Manage progress,showBars 1
	Manage progress,showNumbers 1

	Manage recall,show 1
	Manage recall,showicon 1

	Manage record,dump 1
	Manage record,message 1
	Manage record,photo 1

	Manage bigmap,scale 4
	Manage map,detail high

	Manage main,statusbar,color White ; # #80FFFF
	Manage message,float 0

	Manage misc,mode,exp 1
	Manage misc,mode,armor_class 1
	Manage misc,textLabels [expr {[winfo screenwidth .] < 800}]
	Manage misc,layout tall
	Manage misc,float 0

	Manage messages,max 256

	Manage settings,showUnused 0

	# Icon configuration
	Manage config,prefix dg32

	Manage choicewindow,show 0
	Manage choicewindow,autoexpand 1
	Manage choicewindow,showicon 0

	Manage inventory,alwaysOnTop 1
	Manage inventory,style new

	Manage store,style new

	Manage message2window,show 0

	Manage font,autobar [Global font,sys,normal]
	Manage font,choice [Global font,sys,normal]
	Manage font,inventory [Global font,sys,normal]
	Manage font,knowledge [Global font,sys,normal]
	Manage font,magic [Global font,sys,normal]
	Manage font,message [Global font,sys,normal]
	Manage font,messages [Global font,sys,normal]
	Manage font,misc [Global font,sys,small]
	Manage font,miscPopup [Global font,sys,normal]
	Manage font,monster [Global font,sys,normal]
	Manage font,recall [Global font,sys,large]
	Manage font,options [Global font,sys,normal]
	Manage font,status [Global font,sys,normal]
	Manage font,statusBar [Global font,sys,normal]
	Manage font,store [Global font,sys,normal]

	### One-time warnings to the user.

	# Show Setup Window first time
	Manage warning,setup 0

#	Manage options,autosave 1
	Manage window,autosave 1
	
	LoadValueFile

	return
}

# NSValueManager::CloseModule --
#
#	Called before the game exits. Dumps all values to the tk/config/value
#	file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::CloseModule {} {

	global Angband
	variable Value
	variable Write

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"value\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	foreach name [lsort [array names Value]] {
		if {[info exists Write($name)]} {
			set value [eval $Write($name) $name]
		} else {
			set value [Get $name]
		}
		puts $fileId [list Manage $name $value]
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config value]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# NSValueManager::LoadValueFile --
#
#	Read tk\config\value.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::LoadValueFile {} {

	variable Value
	variable Read

	set fileName [PathTk config value]
	if {[file exists $fileName]} {
		source $fileName
	}

	foreach name [array names Read] {
		if {[info exists Value($name)]} {
			eval $Read($name) $name
		}
	}

	return
}

# NSValueManager::AddClient --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::AddClient {name command} {

	variable Command
	variable ValuePriv

	set clientId [incr ValuePriv(clientId)]
	set Command($name,$clientId) $command
	return $clientId
}

# NSValueManager::RemoveClient --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::RemoveClient {name clientId} {

	variable Command

	unset Command($name,$clientId)

	return
}

# NSValueManager::Manage --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Manage {name value} {

	variable Value

	set Value($name) $value

	return
}

# NSValueManager::Set --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Set {name value} {

	variable Value

	set Value($name) $value
	Changed $name

	return
}

# NSValueManager::Get --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Get {name} {

	variable Value

	return $Value($name)
}

# NSValueManager::Changed --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSValueManager::Changed {name} {

	variable Command

	foreach name [array names Command $name,*] {
		catch {uplevel #0 $Command($name)} err
	}

	return
}


# ColorFromValue --
#
#	Return the palette RGB color for the value with the given name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ColorFromValue {valueName} {

	set index [NSValueManager::Get $valueName]
	return [palette set $index]
}

# Value --
#
#	Convenience interface to NSValueManager::Get/Set
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Value {name args} {

	# Set info
	if {[llength $args]} {
		NSValueManager::Set $name [lindex $args 0]

	# Get info
	} else {
		return [NSValueManager::Get $name]
	}

	return
}

# ListBackgroundChanged --
#
#	Configures the background color of the given canvas to the
#	color of the "listBG" value.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ListBackgroundChanged {canvas} {

	$canvas configure -background [Value listBG]

	return
}

# default_tval_to_attr --
#
#	Returns the color associated with the given object tval.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc default_tval_to_attr {tval} {

	return [Value $tval]
}

proc DumpValueManager {} {

	set win .dumpvalue
	toplevel $win
	text $win.text
	pack $win.text

	foreach name [lsort [array names NSValueManager::Value]] {
		set value [NSValueManager::Get $name]
		$win.text insert end "Manage $name $value\n"
	}

	return
}

