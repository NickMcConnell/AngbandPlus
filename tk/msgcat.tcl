# File: msgcat.tcl

# Purpose: internationalization support for AngbandTk

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# Load the "msgcat" package
package require msgcat

# Set to 1 for Japanese translations etc
set JAPANESE 0

if {!$JAPANESE} {

	# English
	::msgcat::mclocale en

} else {

	# Japanese
	::msgcat::mclocale ja
}

# MsgCatInit --
#
#	Sets up msgcat support for a module.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MsgCatInit {args} {

	global Angband

	# Delete all translations for calling namespace (helps when reloading modules)
	set ns [uplevel {namespace current}]
	foreach loc $::msgcat::Loclist {
		if {[info tclversion] >= 8.5} {
			if {[dict exists $::msgcat::Msgs $loc $ns]} {
				dict unset ::msgcat::Msgs $loc $ns
			}
		} else {
			array unset ::msgcat::Msgs $loc,$ns,*
		}
	}

	# Import all the msgcat commands into the calling namespace, so we
	# can write "mc Foo" instead of "::msgcat::mc Foo".
	uplevel {namespace import ::msgcat::*}

if 0 {
	# Hack -- Load shiftjis format msgcat file
	proc x-mcload {file} {
		set chan [open $file]
		fconfigure $chan -encoding shiftjis
		set buf [read $chan]
		close $chan
		uplevel eval $buf
		return
	}
	uplevel x-mcload [list [file join $::Angband(dir,msgs) common ja.msg]]
	foreach dir $args {
		uplevel x-mcload [list [file join $::Angband(dir,msgs) $dir ja.msg]]
	}
	return
}

	# XXX Hack -- Always include the "common" translations
	uplevel ::msgcat::mcload [list [file join $::Angband(dir,msgs) common]]

	# Load the message file from each given directory
	foreach dir $args {
		uplevel ::msgcat::mcload [list [file join $::Angband(dir,msgs) $dir]]
	}

	return
}

# msgcat::mcunknown --
#
#	Report an error if no translation exists for the given string.
#	Return the given string just for fun.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MsgCatUnknown {ns} {

	global msgcatId
	global msgcatUnknown

	if {[winfo exists [focus]]} {
		set parent [winfo toplevel [focus]]
	} else {
		set parent .
	}
	if {[llength $msgcatUnknown($ns)] > 10} {
		set msgcatUnknown($ns) [lrange $msgcatUnknown($ns) 0 9]
		lappend msgcatUnknown($ns) ...
	}
if 1 {
	puts "Msgcat Error: Translation(s) not found.\nUpdate your msg file.\nIn namespace \"$ns\"\n\n[join $::msgcatUnknown($ns) \n]"
} else {
	tk_messageBox -title "Msgcat Error" -parent $parent \
		-message "Translation(s) not found. Update your msg file.\nIn namespace \"$ns\"\n\n[join $::msgcatUnknown($ns) \n]"
}
	unset msgcatUnknown($ns)
	unset msgcatId($ns)

	return
}

proc msgcat::mcunknown {locale src} {

	global msgcatId
	global msgcatUnknown

    set ns [uplevel 2 [list ::namespace current]]

	lappend msgcatUnknown($ns) $src
	if {![info exists msgcatId($ns)]} {
		set msgcatId($ns) [after idle MsgCatUnknown $ns]
	}

	# Only report once
	uplevel [list ::msgcat::mcset $locale $src]

    return $src
}

if 0 {
proc msgcat::mc {src} {
	set ns [uplevel {namespace current}]
	foreach loc $::msgcat::loclist {
		if {[info exists ::msgcat::msgs($loc,$ns,$src)]} {
			return *$::msgcat::msgs($loc,$ns,$src)
		}
	}
	# we have not found the translation
	return [uplevel 1 [list [namespace origin mcunknown] \
		$::msgcat::locale $src]]
}
}
