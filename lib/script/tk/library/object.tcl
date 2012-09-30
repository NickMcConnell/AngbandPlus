# File: object.tcl

# Purpose: object-oriented commands for Tcl

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# The code in this file was adapted from work by Jean-Luc Fontaine
# (jfontain@pluton.experdata.fr) as part of his "textundo 1.0" package.
# I changed it to use the namespace facility introduced in Tcl 8.

namespace eval NSObject {

	variable classNewId 0
	variable realObjectId

# namespace eval NSObject
}

# NSObject::New --
#
#	Create a new object of the given class by getting a new
#	unique OOP ID and calling the class constructor (if present).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSObject::New {className args} {

	variable classNewId
	variable realObjectId

	# Use local variable for id so New() can be called recursively
	set oop [incr classNewId]

	# Keep track of existing objects
	set realObjectId($className,$oop) 1

	# Call the constructor, if any
	if {[llength [namespace eval ::$className info procs $className]] > 0} {

		# Avoid catch to track errors
		eval ${className}::$className $oop $args
	}

	return $oop
}

# NSObject::Delete --
#
#	Delete object of given class by calling destructor and
#	deleting any global arrays associated with the object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSObject::Delete {className oop} {

	global $className
	variable realObjectId

	# Verify the object
	CheckObject $className $oop
	
	# Call the destructor, if any
	if {[llength [namespace eval ::$className info procs ~$className]] > 0} {

		# Avoid catch to track errors
		${className}::~$className $oop
	}

	# Unset any array elements
	array unset $className $oop,*

	# Forget this object
	unset realObjectId($className,$oop)

	return
}

# NSObject::CheckObject --
#
#	Return an error if the given info doesn't specify a known object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSObject::CheckObject {className oop} {

	variable realObjectId
	
	if {![info exists realObjectId($className,$oop)]} {
		error "no such object $className $oop"
	}

	return
}
