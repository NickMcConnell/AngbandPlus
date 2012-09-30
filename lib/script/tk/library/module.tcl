# File: module.tcl

# Purpose: record info about modules, load and reload them

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSModule {

	variable Priv
	
	# We remember the file modification time when the source file
	# of a module is initially read. Later, the module can be automatically
	# reloaded if the file modification time changes.
	set Priv(autoReload) 1

# namespace eval NSModule
}

proc NSModule::AddModule {module source} {

	variable Priv

	set Priv(loaded,$module) 0
	set Priv(source,$module) $source
	set Priv(mtime,$module) 0

	return
}

proc NSModule::Exists {module} {

	variable Priv

	return [info exists Priv(loaded,$module)]
}

proc NSModule::AddModuleIfNeeded {module source} {

	variable Priv

	if {![Exists $module]} {
		AddModule $module $source
	}

	return
}

proc NSModule::LoadIfNeeded {module} {

	variable Priv

	if {![Exists $module]} {
		error "no such module \"$module\""
	}
	
	# Already been loaded
	if {$Priv(loaded,$module)} {

		if {$Priv(autoReload)} {
			if {[file mtime $Priv(source,$module)] > $Priv(mtime,$module)} {
				# Nothing
			} else {
				return 0
			}
		} else {
			return 0
		}

		if {[llength [info commands ::${module}::CloseModule]]} {
			::${module}::CloseModule
		}
	}

	# Clean up previously-/partially-loaded module
	if {[llength [namespace children :: $module]]} {
		namespace delete ::$module
	}

	# Source the file
	uplevel #0 source [list $Priv(source,$module)]

	# Init the module
	${module}::InitModule

	# Okay, it's loaded
	set Priv(loaded,$module) 1

	# Remember the modification time for auto-reloading
	set Priv(mtime,$module) [file mtime $Priv(source,$module)]

	return 1
}

proc NSModule::RebootModule {module} {

	variable Priv

	# Verify existence, Close() and delete
	CloseModule $module
	
	# Load the module
	LoadIfNeeded $module

	return
}

proc NSModule::CloseModule {module} {

	variable Priv

	if {![Exists $module]} {
		error "no such module \"$module\""
	}

	# Already been loaded
	if {$Priv(loaded,$module)} {

		if {[llength [info commands ::${module}::CloseModule]]} {
			::${module}::CloseModule
		}
		namespace delete ::$module
		set Priv(loaded,$module) 0
	}

	return
}

proc NSModule::IndexLoad {path} {

	variable Priv

	set Priv(dir,index) [file dirname $path]
	source $path

	return
}

proc NSModule::IndexOne {module name} {

	variable Priv

	set path [file join $Priv(dir,index) $name]
	AddModuleIfNeeded $module $path

	return
}

