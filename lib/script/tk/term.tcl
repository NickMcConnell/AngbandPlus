# File: term.tcl

# Purpose: the Main Terminal

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.

namespace eval NSTerm {


# namespace eval NSTerm
}

# NSTerm::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTerm::InitModule {} {

}

# NSTerm::NSTerm --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTerm::NSTerm {oop parent width height gwidth gheight} {

	variable win

	set widget $parent.term$oop
		
	#update idletasks
	

	widget $widget -width $width -height $height \
		-gwidth $gwidth -gheight $gheight -term 1
	

	# Shift-drag does nothing
	bind $widget <Shift-Button1-Motion> break

	bind $widget <ButtonPress-1> break
	bind $widget <Button1-Motion> break
	
	# Disable tracking when dragging
	bind $widget <Button1-Enter> break
	bind $widget <Button1-Leave> break
	
	# This binding is called whenever the window is resized
	# by the user.
	bind $widget <Configure> \
		"NSTerm::Resize $oop %w %h"
	
	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $widget
	
	pack $widget -expand yes -fill both
	
	return
}


proc NSTerm::Resize {oop width height} {

	variable win

	.term.term$oop configure -width $width -height $height

	return 1
}


proc NSTerm::Close {oop} {

	#wm iconify .term.term$oop
}
	
