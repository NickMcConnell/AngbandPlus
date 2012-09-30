# File: buttonlabel.tcl

# Purpose: a label that behaves like a button

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSButtonLabel {

	variable Priv

# namespace eval NSButtonLabel
}

# NSButtonLabel::ButtonLabel --
#
#	Create bindings on the given label widget so it behaves "like a button."
#	Also remember the given command so it can be called when the label
#	is clicked.
#
# Arguments:
#	widget					Which label widget to button-ize
#	command					Command plus args to invoke
#
# Results:
#	What happened.

proc NSButtonLabel::ButtonLabel {widget command} {

	variable Priv

	array set Priv [list "$widget,command" $command]

	bindtags $widget [concat [bindtags $widget] ButtonLabelBindTags]

	return
}

# NSButtonLabel::_ButtonLabelRelease --
#
#	Called when Button-1 is released for the given label widget.
#	If the pointer is in the label then the remembered command is
#	evaluated.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSButtonLabel::_ButtonLabelRelease {widget x y} {

	variable Priv

	$widget configure -relief raised

	set x [expr {$x + [winfo rootx $widget]}]
	set y [expr {$y + [winfo rooty $widget]}]
	if {[winfo containing $x $y] != "$widget"} return

	uplevel #0 $Priv($widget,command)

	return
}

#
# These bindings are applied to the ButtonLabelBindTags bindtag
#

bind ButtonLabelBindTags <Enter> {
	%W configure -relief raised
}
bind ButtonLabelBindTags <Button1-Enter> {
	%W configure -relief sunken
}
bind ButtonLabelBindTags <Button-1> {
	%W configure -relief sunken
}
bind ButtonLabelBindTags <Leave> {
	%W configure -relief flat
}
bind ButtonLabelBindTags <Button1-Leave> {
	%W configure -relief raised
}

bind ButtonLabelBindTags <ButtonRelease-1> {
	NSButtonLabel::_ButtonLabelRelease %W %x %y
}

bind ButtonLabelBindTags <Destroy> {
	unset NSButtonLabel::Priv(%W,command)
}

