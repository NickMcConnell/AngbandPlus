# File: status-text.tcl

# Purpose: associate a help string with any widget

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

#	Associate a help string with a given widget, and display that
#	string when the mouse enters the widget. The help text is
#	displayed in a label.

namespace eval NSStatusText {

	variable Priv

# namespace eval NSStatusText
}

# NSStatusText::StatusText --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusText::StatusText {widget label string} {

	variable Priv

	set Priv($widget,label) $label
	set Priv($widget,text) $string
	set Priv($widget,command) {}

	bindtags $widget [concat [bindtags $widget] StatusText_BindTag]

	return
}

# NSStatusText::StatusCommand --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusText::StatusCommand {widget label command} {

	variable Priv

	set Priv($widget,label) $label
	set Priv($widget,text) {}
	set Priv($widget,command) $command

	bindtags $widget [concat [bindtags $widget] StatusText_BindTag]

	return
}

# NSStatusText::SetText --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusText::SetText {widget} {

	variable Priv

	set label $Priv($widget,label)
	set Priv($widget,oldText) [$label cget -text]
	
	if {[string length $Priv($widget,text)]} {
		set helpText $Priv($widget,text)
		$label configure -text $helpText
	} else {
		uplevel #0 $Priv($widget,command)
	}

	return
}

# NSStatusText::ResetText --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStatusText::ResetText {widget} {

	variable Priv

	set label $Priv($widget,label)
	set oldText $Priv($widget,oldText)
	$label configure -text $oldText

	return
}

bind StatusText_BindTag <Any-Enter> {
	NSStatusText::SetText %W
}

bind StatusText_BindTag <Any-Leave> {
	NSStatusText::ResetText %W
}

bind StatusText_BindTag <Destroy> {
	unset NSStatusText::Priv(%W,label)
	unset NSStatusText::Priv(%W,text)
}

