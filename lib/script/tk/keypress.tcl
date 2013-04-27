# File: keypress.tcl

# Purpose: KeyPress-related event bindings

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

bind Term_KeyPress_BindTag <Control-Shift-KeyPress> {

	angband keypress \037Control-Shift-%K\015
}

bind Term_KeyPress_BindTag <Control-Shift_L> {
}

bind Term_KeyPress_BindTag <Shift-Control_L> {
}

bind Term_KeyPress_BindTag <Control-KeyPress> {

	# Special Control-KeyPress (ex Control-F1)
	if {![string length %A] || [string length %K] > 1} {
		angband keypress \037Control-%K\015

	# Ascii Control-KeyPress
	} else {
		angband keypress %A
	}
}

bind Term_KeyPress_BindTag <Control_L> {
}

bind Term_KeyPress_BindTag <Shift-KeyPress> {

	# Special Shift-KeyPress (ex Shift-F1)
	if {![string length %A]} {
		angband keypress \037Shift-%K\015

	# Ascii Shift-KeyPress
	} else {
		angband keypress %A
	}
}

bind Term_KeyPress_BindTag <Shift_L> {
}

bind Term_KeyPress_BindTag <Alt-KeyPress> {

	angband keypress \037Alt-%K\015
}

bind Term_KeyPress_BindTag <Alt_L> {
}

bind Term_KeyPress_BindTag <KeyPress> {

	# Special KeyPress (ex F1)
	if {![string length %A]} {
		angband keypress \037%K\015

	# Normal keys with no modifiers
	} else {
		angband keypress %A
	}
}

bind Term_KeyPress_BindTag <Escape> {

	angband keypress \033
}

bind Term_KeyPress_BindTag <Return> {

	angband keypress \r
}

bind Term_KeyPress_BindTag <Tab> {

	angband keypress \t
}

bind Term_KeyPress_BindTag <BackSpace> {

	angband keypress \010
}

bind Term_KeyPress_BindTag <Delete> {

	angband keypress \010
}


# Term_KeyPress_Bind --
#
#	Adds the Term_KeyPress_BindTag to the end of the list of bindtags
#	for the given window. This means KeyPress events are fed to the
#	Term via the "angband keypress" command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Term_KeyPress_Bind {win} {

	bindtags $win [concat [bindtags $win] Term_KeyPress_BindTag]

	return
}
