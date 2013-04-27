# File: autobar.tcl

# Purpose: the Autobar display and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSAutobar {

# namespace eval NSAutobar
}

# NSAutobar::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::InitModule {} {

	InitImageIfNeeded Image_ButtonActivate button-activate.gif
	InitImageIfNeeded Image_ButtonFood button-food.gif
	InitImageIfNeeded Image_ButtonPotion button-potion.gif
	InitImageIfNeeded Image_ButtonScroll button-scroll.gif
	InitImageIfNeeded Image_ButtonRod button-rod.gif
	InitImageIfNeeded Image_ButtonWand button-wand.gif
	InitImageIfNeeded Image_ButtonStaff button-staff.gif
	InitImageIfNeeded Image_ButtonDown button-down.gif
	InitImageIfNeeded Image_ButtonUp button-up.gif

	InitImageIfNeeded Image_ButtonBook button-book.gif

	NSObject::New NSAutobar

	return
}

# NSAutobar::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::CloseModule {} {

	catch {
		destroy [Global autobar,canvas]
	}

	return
}

# NSAutobar::MInfo --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::MInfo {info args} {

	variable Priv

	# Set info
	if {[llength $args]} {
		set Priv($info) [lindex $args 0]

	# Get info
	} else {
		return $Priv($info)
	}

	return
}

# NSAutobar::NSAutobar --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSAutobar::NSAutobar {oop} {

	Info $oop busy 0
	Info $oop current ""
	Info $oop request,id ""
	Info $oop showing 0

	Info $oop bar,visible 0
	Info $oop win,visible 0
	Info $oop whoHasCursor ""
	Info $oop after ""

	Info $oop nextButton 0

	InitDisplay $oop

	set canvas [Info $oop canvas]
	set wText [Info $oop text]

	# Update ourself when the list highlight color changes
	Info $oop clientId,listHilite \
		[NSValueManager::AddClient listHilite \
			"$wText tag configure HOT \
			-background \[Value listHilite]"]

	# Update ourself when the font changes
	Info $oop clientId,font,autobar \
		[NSValueManager::AddClient font,autobar \
			"NSAutobar::ValueChanged_font_autobar $oop"]
		
	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSAutobar $oop $canvas

	Global autobar,canvas $canvas
	Global autobar,oop $oop

	return
}

# NSAutobar::~NSAutobar --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::~NSAutobar {oop} {

	NSValueManager::RemoveClient font,autobar [Info $oop clientId,font,autobar]
	NSValueManager::RemoveClient listHilite [Info $oop clientId,listHilite]

	return
}

# NSAutobar::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::Info {oop info args} {

	global NSAutobar

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSAutobar($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSAutobar($oop,$info)
			}
		}
	}

	return
}

# NSAutobar::InitDisplay --
#
#	Create the display.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSAutobar::InitDisplay {oop} {

	set widget [Global main,widget]

	#
	# The main canvas, holding the buttons
	#

	set canvas $widget.autobar$oop
	canvas $canvas \
		-borderwidth 0 -relief flat -highlightthickness 0 \
		-background Black -height 22 -width 200 \
		-scrollregion {0 0 0 0}

	Info $oop canvas $canvas

	bind $canvas <Enter> \
		"NSAutobar::Event $oop enter-bar"
	bind $canvas <Leave> \
		"NSAutobar::Event $oop leave-bar"

	#
	# Buttons
	# Some buttons invoke a command when clicked, such as "go down".
	# Other buttons display a list of item or spell choices.
	# If a pop-up button has no valid choices, then it isn't displayed
	# in the bar.
	#

	set x 2
	NewButton $oop -image Image_ButtonActivate -popup 1 -hook item \
		-args [list equipment A -activate yes] \
		-message "Activate an equipment item"

	$canvas configure -width [incr x 20]

	$canvas create rectangle \
		0 0 [expr {$x - 1}] 21 -outline gray60 -tags border

	#
	# Popup window of choices
	#

	set win $canvas.popup
	toplevel $win
	$win configure -borderwidth 1 -relief flat -background gray60
	wm overrideredirect $win yes
	wm transient $win [Window main]

	if {[Platform unix]} {
		$win configure -cursor arrow
	}

	# Start out withdrawn (hidden)
	wm withdraw $win

	Info $oop win $win

	set wText $win.text
	text $wText \
		-wrap none -font [Value font,autobar] \
		-borderwidth 0 -setgrid no -highlightthickness 0 \
		-padx 4 -pady 2 -background Black -foreground White -cursor ""

	# Bypass default Text bindings
	bindtags $wText [list $wText $win all]

	pack $wText \
		-expand yes -fill both

	Info $oop text $wText

	# Fiddle with the selection for list behaviour
	$wText tag configure HOT -foreground White \
		-background [Value listHilite]

	$wText tag bind HOT <ButtonPress-1> \
		"NSAutobar::Invoke $oop"
	$wText tag bind TEXT <Motion> \
		"NSAutobar::Motion $oop \[$wText index {@%x,%y linestart}]"
	$wText tag bind HOT <Leave> \
		"NSAutobar::Motion $oop {}"

	bind $win <Enter> \
		"NSAutobar::Event $oop enter-win"
	bind $win <Leave> "
		NSAutobar::Motion $oop {}
		NSAutobar::Event $oop leave-win
	"

	return
}

# NSAutobar::NewButton --
#
#	Add a new button to the Autobar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::NewButton {oop args} {

	set canvas [Info $oop canvas]

	# Get the next unique id for this button
	set num [incr ::NSAutobar($oop,nextButton)]

	set config(-command) ""
	array set config $args

	set image $config(-image)
	set command $config(-command)
	set message $config(-message)

	# The buttons are positioned in ShowBar().
	set x 0
	set y 0

	# Focus rectangle
	$canvas create rectangle $x $y [expr {$x + 17}] [expr {$y + 17}] \
		-tags [list button button$num border$num]

	# Image
	$canvas create image [expr {$x + 1}] [expr {$y + 1}] -image $image \
		-anchor nw -tags "button button$num img$num"

	# Show popup on mouse-over
	if {![string length $command]} {

		$canvas bind img$num <Enter> "
			$canvas itemconfigure border$num -outline gray60
			NSMainWindow::StatusText $oop [list $message]
			NSAutobar::EnterButton $oop $num
		"

		Info $oop button,hook,$num $config(-hook)
		Info $oop button,args,$num $config(-args)

	# Click to invoke command
	} else {
		$canvas bind img$num <Enter> "
			$canvas itemconfigure border$num -outline gray60
			NSMainWindow::StatusText $oop [list $message]
			NSAutobar::Event $oop enter-button2
		"
		$canvas bind img$num <ButtonPress-1> "
			$canvas move button$num 1 1
			$command
		"
		$canvas bind img$num <ButtonRelease-1> \
			"$canvas move button$num -1 -1"

		Info $oop button,hook,$num ""
	}

	$canvas bind img$num <Leave> "
		$canvas itemconfigure border$num -outline Black
		NSAutobar::Event $oop leave-button
		NSMainWindow::StatusText $oop {}
	"

	return
}

# NSAutobar::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::SetHook {oop hook} {

	if {[string length $hook]} {
		Info $oop hook $hook
		CallHook $oop open
	} elseif {[string length [Info $oop hook]]} {
		Info $oop hook ""
	}

	return
}

# NSAutobar::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::CallHook {oop message args} {

	return [uplevel #0 NSAutobar::[Info $oop hook] $oop $message $args]
}

# NSAutobar::EnterButton --
#
#	Display popup of choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::EnterButton {oop buttonNum} {

	set win [Info $oop win]
	set canvas [Info $oop canvas]
	set wText [Info $oop text]

	set hook [Info $oop button,hook,$buttonNum]
	set hookArgs [Info $oop button,args,$buttonNum]
	
	if {[lsearch -exact [angband inkey_flags] INKEY_CMD] == -1} return

	set x [Info $oop button,x,$buttonNum]
	incr x [expr {[winfo rootx $canvas] + 9}]
	set y [winfo rooty $canvas]

	# Set the list
	SetHook $oop hook_$hook
	$wText delete 1.0 end
	eval CallHook $oop set_list $hookArgs

	set width [Info $oop maxWidth]
	incr width [expr {[$wText cget -padx] * 2}]

	set height [Info $oop maxHeight]
	incr height [expr {[$wText cget -pady] * 2}]

	incr width [expr {[$win cget -borderwidth] * 2}]
	incr height [expr {[$win cget -borderwidth] * 2}]

	# x is middle
	incr x [expr {0 - $width / 2}]
	if {$x < [winfo rootx $canvas]} {
		set x [winfo rootx $canvas]
	}

	# Given y is bottom
	incr y -$height

	set screenWidth [winfo screenwidth .]
	if {$x + $width > $screenWidth} {
		incr x [expr {$screenWidth - ($x + $width)}]
	}
	set screenHeight [winfo screenheight .]
	if {$y + $height > $screenHeight} {
		incr y [expr {$screenHeight - ($y + $height)}]
	}

	wm geometry $win ${width}x${height}+${x}+$y

	# Perhaps show the window later
	Event $oop enter-button

	return
}

# NSAutobar::Event --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::Event {oop event} {

	set who [Info $oop whoHasCursor]

	switch -- $event {
		enter-status {
			if {[lsearch -exact [angband inkey_flags] INKEY_CMD] == -1} return
			set who status
		}
		leave-status {
			set who ""
		}
		enter-bar {
			# The <Enter> binding for a button is invoked before
			# the binding for the canvas. So we don't want to
			# forget we are in a button
			if {[string compare $who button]} {
				set who bar
			}
		}
		leave-bar {
			set who ""
		}
		enter-win {
			set who win
		}
		leave-win {
			set who ""
		}
		enter-button {
			set who button
		}
		enter-button2 {
			set who button2
		}
		leave-button {
			set who bar
		}
	}

	Info $oop whoHasCursor $who
	if {[string match enter-* $event]} {
		set delay 10
	} else {
		set delay 200
	}
	after cancel [Info $oop after]
	Info $oop after [after $delay NSAutobar::CheckWhoHasCursor $oop]

	return
}

# NSAutobar::CheckWhoHasCursor --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::CheckWhoHasCursor {oop} {

	set who [Info $oop whoHasCursor]

	switch -- $who {
		bar {
			HideWin $oop
		}
		button {
			ShowWin $oop
		}
		button2 {
			HideWin $oop
		}
		status {
			HideWin $oop
			ShowBar $oop
		}
		win {
			
		}
		default {
			HideWin $oop
			HideBar $oop
		}
	}

	Info $oop after ""

	return
}

# NSAutobar::ShowBar --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::ShowBar {oop} {

	set canvas [Info $oop canvas]

	# The bar is already shown
	if {[Info $oop bar,visible]} return

	# Hide all the buttons
	$canvas itemconfigure button -state hidden

	# Display buttons which have some commands
	set x 3
	set y 3

	# Check each button
	for {set i 1} {$i <= [Info $oop nextButton]} {incr i} {

		# Get the hook for this button (if any)
		set hook [Info $oop button,hook,$i]

		# This button has a hook
		if {[string length $hook]} {

			# Set the hook
			SetHook $oop hook_$hook

			# Get the extra args to pass to the hook
			set args [Info $oop button,args,$i]

			# See if there are any valid choices
			if {[eval CallHook $oop has_cmd $args]} {

				# Position the button horizontally
				scan [$canvas coords img$i] "%s %s" cx cy
				$canvas move button$i [expr {$x - $cx}] [expr {$y - $cy}]
				$canvas itemconfigure button$i -state ""

				# Remember the x coordinate
				Info $oop button,x,$i $x

				# Leave space for another button
				incr x 20
			}

		# Button has no hook, always show it
		} else {

			# Position the button horizontally
			scan [$canvas coords img$i] "%s %s" cx cy
			$canvas move button$i [expr {$x - $cx}] [expr {$y - $cy}]
			$canvas itemconfigure button$i -state ""

			# Remember the x coordinate
			Info $oop button,x,$i $x

			# Leave space for another button
			incr x 20
		}
	}

	# Resize the bar, and position the border rectangle
	set width [expr {$x - 1}]
	$canvas configure -width $width
	$canvas coords border 0 0 [expr {$width - 1}] 21

	# Now slide the bar into view
	set height [winfo reqheight $canvas]
	foreach frac [list 0.1 0.2 0.3 0.5 0.7 1] {
		set dy [expr {0 - $height * $frac}]
		place $canvas -relx 0.5 -rely 1.0 -y $dy -anchor n
		update idletasks
	}

	Info $oop bar,visible 1

	return
}

# NSAutobar::HideBar --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::HideBar {oop} {

	if {![Info $oop bar,visible]} return

	set canvas [Info $oop canvas]

	# Now slide the bar out of view
	set height [winfo reqheight $canvas]
	foreach frac [list 0.7 0.4 0.1] {
		set dy [expr {0 - $height * $frac}]
		place $canvas -relx 0.5 -rely 1.0 -y $dy -anchor n

		# Kind of slow if mouse is whizzing around...
		update
	}

	place forget $canvas

	Info $oop bar,visible 0

	return
}

# NSAutobar::ShowWin --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::ShowWin {oop} {

	if {[Info $oop win,visible]} return

	set win [Info $oop win]

	wm deiconify $win
	if {[Platform unix]} {
		raise $win
	}

	Info $oop win,visible 1

	return
}

# NSAutobar::HideWin --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::HideWin {oop} {

	if {![Info $oop win,visible]} return

	set win [Info $oop win]

	wm withdraw $win

	Info $oop win,visible 0

	return
}

# NSAutobar::Invoke --
#
#	Called when a list item is clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::Invoke {oop} {

	set textBox [Info $oop text]
	set index [Info $oop current]
	set row [expr {[lindex [split $index .] 0] - 1}]

	HideWin $oop
	HideBar $oop

	CallHook $oop invoke $row

	return
}

# NSAutobar::Motion --
#
#	Called when the mouse moves in a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::Motion {oop index} {

	set textBox [Info $oop text]

	# If you invoke an item, hold down the mouse, and drag...
	if {![string length [Info $oop hook]]} return

	# No tracking while menu is up
#	if {[Info $oop busy]} return

	# See if the item has changed
	if {$index == [Info $oop current]} return

	# An item is highlighted
	if {[string length [Info $oop current]]} {

		# Remove highlighting
		UnhighlightItem $oop [Info $oop current]
	}

	# An item is under the pointer
	if {[string length $index]} {

		# Highlight the item
		HighlightItem $oop $index
	}

	# Remember which item is highlighted
	Info $oop current $index

	return
}

# NSAutobar::HighlightItem --
#
#	Highlights a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::HighlightItem {oop index} {

	set textBox [Info $oop text]
	set row [expr {[lindex [split $index .] 0] - 1}]

	# Highlight the item
	$textBox tag add HOT $index "$index lineend"
	$textBox tag raise HOT

	# Call the hook (to set the icon, for example)
	CallHook $oop highlight $row

	return
}

# NSAutobar::UnhighlightItem --
#
#	Removes highlighting from a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::UnhighlightItem {oop index} {

	set win [Info $oop win]
	set textBox [Info $oop text]

	# Unhighlight the item
	$textBox tag remove HOT 1.0 end

	return
}

# NSAutobar::HasCursor --
#
#	See if the cursor is over the window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::HasCursor {oop} {

	set pointerx [winfo pointerx .]
	set pointery [winfo pointery .]
	set window [winfo containing $pointerx $pointery]
	if {![string length $window]} {
		return 0
	}
	if {[string compare [winfo toplevel $window] [Info $oop win]]} {
		return 0
	}
	return 1
}


# NSAutobar::ValueChanged_font_autobar --
#
#	Called when the font,autobar value changes.
#	Updates the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::ValueChanged_font_autobar {oop} {

	set text [Info $oop text]
	$text configure -font [Value font,autobar]

	return
}
