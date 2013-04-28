# File: autobar.tcl

# Purpose: the Autobar display and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSAutobar {

# namespace eval NSAutobar
}

proc ScaleImage {image x y} {
	set scaleImg [image create photo]
	$scaleImg copy $image -zoom $x $y
	$image blank
	$image copy $scaleImg
	image delete $scaleImg
	return
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

	MsgCatInit inven misc-win

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

if 0 {
	ScaleImage Image_ButtonActivate 2 2
	ScaleImage Image_ButtonFood 2 2
	ScaleImage Image_ButtonPotion 2 2
	ScaleImage Image_ButtonScroll 2 2
	ScaleImage Image_ButtonRod 2 2
	ScaleImage Image_ButtonWand 2 2
	ScaleImage Image_ButtonStaff 2 2
	ScaleImage Image_ButtonDown 2 2
	ScaleImage Image_ButtonUp 2 2
	ScaleImage Image_ButtonBook 2 2
}
	MInfo buttonWidth [Setting autobar_button_size]
	MInfo buttonHeight [Setting autobar_button_size]

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
	qebind NSAutobar <Value-listHilite> \
		"$wText tag configure HOT \
		-background \[Value listHilite]"

	# Update ourself when the font changes
	qebind NSAutobar <Value-font,autobar> \
		"NSAutobar::ValueChanged_font_autobar $oop"

	# Update ourself when the button size changes
	qebind NSAutobar <Setting-autobar_button_size> \
		"NSAutobar::SetButtonSize $oop %c"

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
		-background Black -height [expr {[MInfo buttonHeight] + 4}] \
		-width 200 -scrollregion {0 0 0 0}

	Info $oop canvas $canvas

	bind $canvas <Enter> \
		"NSAutobar::Event $oop enter-bar"
	bind $canvas <Leave> \
		"NSAutobar::Event $oop leave-bar"

	# Canvas border
	$canvas create rectangle 0 0 1 1 -outline gray60 -tags border

	#
	# Buttons
	# Some buttons invoke a command when clicked, such as "go down".
	# Other buttons display a list of item or spell choices.
	# If a pop-up button has no valid choices, then it isn't displayed
	# in the bar.
	#

	NewButton $oop -image Image_ButtonActivate -popup 1 -hook item \
		-args [list equipment A -activate yes] \
		-message [mc "Activate an equipment item"]
	NewButton $oop -image Image_ButtonFood -popup 1 -hook item \
		-args [list {inventory floor} E -tval TV_FOOD] \
		-message [mc "Eat an inventory item"]
	NewButton $oop -image Image_ButtonPotion -popup 1 -hook item \
		-args [list {inventory floor} q -tval TV_POTION] \
		-message [mc "Drink a potion in inventory"]
	NewButton $oop -image Image_ButtonScroll -popup 1 -hook item \
		-args [list {inventory floor} r -tval TV_SCROLL] \
		-message [mc "Read a scroll in inventory"]
	NewButton $oop -image Image_ButtonRod -popup 1 -hook item \
		-args [list {inventory floor} z -tval TV_ROD] \
		-message [mc "Zap a rod in inventory"]
	NewButton $oop -image Image_ButtonWand -popup 1 -hook item \
		-args [list {inventory floor} a -tval TV_WAND] \
		-message [mc "Aim a wand in inventory"]
	NewButton $oop -image Image_ButtonStaff -popup 1 -hook item \
		-args [list {inventory floor} u -tval TV_STAFF] \
		-message [mc "Use a staff in inventory"]
if 0 {
	NewButton $oop -image Image_ButtonUp -popup 0 \
		-command "DoUnderlyingCommand <" \
		-message [mc "Go up a dungeon level"]
	NewButton $oop -image Image_ButtonDown -popup 0 \
		-command "DoUnderlyingCommand >" \
		-message [mc "Go down a dungeon level"]
}

	# One button for each spell book
	foreach tval [angband player spell_book] {

		switch -- [angband player prayer_or_spell] {
			prayer {
				set message [mc "Recite or study a prayer"]
			}
			spell {
				set message [mc "Cast or study a spell"]
			}
		}

		# Find all books of this type
		set books [angband k_info find -tval $tval]

		# Check each book
		foreach bookNum $books {

			# Some classes can't use high-level books
			if {![llength [angband spell find $bookNum]]} continue

			NewButton $oop -image Image_ButtonBook -popup 1 -hook spell \
				-args [list $bookNum] \
				-message $message
		}
	}

	if {[variant ZANGBANDTK]} {
		NewButton $oop -image Image_ButtonActivate -popup 1 -hook mindcraft \
			-args {} \
			-message [mc "Use Mindcraft"]
		NewButton $oop -image Image_ButtonActivate -popup 1 -hook power \
			-args {} \
			-message [mc "Use racial/mutation power"]
	}

	#
	# Popup window of choices
	#

	set win $canvas.popup
	toplevel $win
	$win configure -borderwidth 1 -relief flat -background gray60

	# Start out withdrawn (hidden)
	wm withdraw $win

	wm overrideredirect $win yes
	wm transient $win [Window main]

	if {[Platform unix]} {
		$win configure -cursor arrow
	}

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

	set bw [MInfo buttonWidth]
	set bh [MInfo buttonHeight]

	# The buttons are positioned in ShowBar().
	set x 0
	set y 0

	# Add some stuff above the button that looks like the canvas background
	# and border so the mouse doesn't leave the button while still in the
	# canvas.  This should all be redone with a <Motion> binding on the
	# canvas to figure out which button the mouse is over.
	$canvas create line $x [expr {$y - 2}] [expr {$x + $bw - 1}] [expr {$y - 2}] \
		-fill gray60 -tags "button button$num"
	$canvas create line $x [expr {$y - 1}] [expr {$x + $bw - 1}] [expr {$y - 1}] \
		-fill [$canvas cget -background] -tags "button button$num"

	# Focus rectangle
	$canvas create rectangle $x $y [expr {$x + $bw - 1}] [expr {$y + $bh - 1}] \
		-fill [$canvas cget -background] \
		-tags [list button button$num border$num]

	# Image
	$canvas create image [expr {$x + $bw / 2}] [expr {$y + $bh / 2}] -image $image \
		-anchor center -tags "button button$num img$num"

	# Show popup on mouse-over
	if {$command eq ""} {

		$canvas bind button$num <Enter> "
			$canvas itemconfigure border$num -outline gray60
			NSMainWindow::StatusText $oop [list $message]
			NSAutobar::EnterButton $oop $num
		"

		Info $oop button,hook,$num $config(-hook)
		Info $oop button,args,$num $config(-args)

	# Click to invoke command
	} else {
		$canvas bind button$num <Enter> "
			$canvas itemconfigure border$num -outline gray60
			NSMainWindow::StatusText $oop [list $message]
			NSAutobar::Event $oop enter-button2
		"
		$canvas bind button$num <ButtonPress-1> "
			$canvas move button$num 1 1
			$command
		"
		$canvas bind button$num <ButtonRelease-1> \
			"$canvas move button$num -1 -1"

		Info $oop button,hook,$num ""
	}

	$canvas bind button$num <Leave> "
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

	if {$hook ne ""} {
		Info $oop hook $hook
		CallHook $oop open
	} elseif {[Info $oop hook] ne ""} {
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

# NSAutobar::NewListItem --
#
#	Create a new list item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::NewListItem {oop char string color args} {

	set wText [Info $oop text]

	$wText insert end "$char\) " TEXT $string \
		[list TEXT_$char TEXT] "\n" TEXT
	$wText tag configure TEXT_$char -foreground $color

	lappend ::NSAutobar($oop,match) $args

	return
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

	# Hack -- Recall the book
	switch -- $hook {
		spell {
			NSRecall::RecallObjectKind [lindex $hookArgs 0]
		}
	}

	if {"INKEY_CMD" ni [angband inkey_flags]} return
	if {[angband store shopping]} return

	set x [Info $oop button,x,$buttonNum]
	incr x [expr {[winfo rootx $canvas] + [MInfo buttonWidth] / 2}]
	set y [winfo rooty $canvas]

	# Set the list
	SetHook $oop hook_$hook
	$wText delete 1.0 end
	Info $oop match {}
	CallHook $oop set_list {*}$hookArgs

	set width 0
	for {set i 1} {$i <= [$wText count -lines 1.0 end]} {incr i} {
		set lineWidth [$wText count -update -xpixels $i.0 "$i.0 lineend"]
		if {$lineWidth > $width} {
			set width $lineWidth
		}
	}
	incr width [expr {[$wText cget -padx] * 2}]

	set height [$wText count -update -ypixels 1.0 end]
	incr height [expr {[$wText cget -pady] * 2}]

	incr width [expr {[$win cget -borderwidth] * 2}]
	incr height [expr {[$win cget -borderwidth] * 2}]

	# x is middle
	incr x [expr {0 - $width / 2}]
	if {$x < [winfo rootx $canvas] - 20} {
		set x [expr {[winfo rootx $canvas] - 20}]
	}

	# Given y is bottom
	incr y -$height

	set screenWidth [ScreenWidth]
	if {$x + $width > $screenWidth} {
		incr x [expr {$screenWidth - ($x + $width)}]
	}
	set screenHeight [ScreenHeight]
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
			if {"INKEY_CMD" ni [angband inkey_flags]} return
			if {[angband store shopping]} return
			set who status
		}
		leave-status {
			set who ""
		}
		enter-bar {
			# The <Enter> binding for a button is invoked before
			# the binding for the canvas. So we don't want to
			# forget we are in a button
			if {$who ne "button"} {
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
	set x 2
	set y 2

	# Check each button
	for {set i 1} {$i <= [Info $oop nextButton]} {incr i} {

		# Get the hook for this button (if any)
		set hook [Info $oop button,hook,$i]

		# This button has a hook
		if {$hook ne ""} {

			# Set the hook
			SetHook $oop hook_$hook

			# Get the extra args to pass to the hook
			set args [Info $oop button,args,$i]

			# See if there are any valid choices
			if {[CallHook $oop has_cmd {*}$args]} {

				# Position the button horizontally
				scan [$canvas coords border$i] "%s %s" cx cy
				$canvas move button$i [expr {$x - $cx}] [expr {$y - $cy}]
				$canvas itemconfigure button$i -state ""

				# Remember the x coordinate
				Info $oop button,x,$i $x

				# Leave space for another button
				incr x [expr {[MInfo buttonWidth] + 2}]
			}

		# Button has no hook, always show it
		} else {

			# Position the button horizontally
			scan [$canvas coords border$i] "%s %s" cx cy
			$canvas move button$i [expr {$x - $cx}] [expr {$y - $cy}]
			$canvas itemconfigure button$i -state ""

			# Remember the x coordinate
			Info $oop button,x,$i $x

			# Leave space for another button
			incr x [expr {[MInfo buttonWidth] + 2}]
		}
	}

	# Resize the bar, and position the border rectangle
	set width [expr {$x - 1 + 1}]
	$canvas configure -width $width
	$canvas coords border 0 0 [expr {$width - 1}] [expr {[MInfo buttonHeight] + 3}]

	# Now slide the bar into view
	set height [winfo reqheight $canvas]
	foreach frac [list 0.1 0.2 0.3 0.5 0.7 1] {
		set dy [expr {0 - $height * $frac}]
		place $canvas -relx 0.5 -rely 1.0 -y $dy -anchor n

		set ms [clock milliseconds]
		update idletasks
		set diff [expr {[clock milliseconds] - $ms}]
		if {$diff < 10} {
			after [expr {10 - $diff}]
		}
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
		set ms [clock milliseconds]
		update
		set diff [expr {[clock milliseconds] - $ms}]
		if {$diff < 10} {
			after [expr {10 - $diff}]
		}
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

	CallHook $oop invoke {*}[lindex [Info $oop match] $row]

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
	if {[Info $oop hook] eq ""} return

	# No tracking while menu is up
#	if {[Info $oop busy]} return

	# See if the item has changed
	if {$index == [Info $oop current]} return

	# An item is highlighted
	if {[Info $oop current] ne ""} {

		# Remove highlighting
		UnhighlightItem $oop [Info $oop current]
	}

	# An item is under the pointer
	if {$index ne ""} {

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
	$textBox tag add HOT $index "$index lineend + 1 chars"
	$textBox tag raise HOT

	# Call the hook (to set the icon, for example)
	CallHook $oop highlight {*}[lindex [Info $oop match] $row]

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

	return [NSUtils::HasCursor [Info $oop win] 1]
}

if {[variant OANGBANDTK]} {

proc NSAutobar::hook_ele_attack {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		set_list {

			set textBox [Info $oop text]

			# These colors must match those below
			set data [list \
				a Fire [Value TERM_RED] \
				b Cold [Value TERM_L_WHITE] \
				c Acid [Value TERM_L_DARK] \
				d Electricity [Value TERM_L_BLUE] \
			]

			# This bit of code must match that in cmd5.c
			set num [expr {([angband player level] - 20) / 7}]
			if {$num <= 0} {
				set num 1
			}
			set data [lrange $data 0 [expr {$num * 3 - 1}]]

			# Process each attack
			foreach {char attack color} $data {

				# Append the character and description
				NewListItem $oop $char $attack $color \
					$char
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args char
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

# OANGBANDTK
}

proc GetFloorItemSelectChars {floorIndex floorItemList allowInven} {

	if {$allowInven} {
		set toggleChar -
	} else {
		set toggleChar ""
	}

	# Two different options make floor selection complicated
	if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
		set easy_floor [Setting easy_floor]
	}
	if {[variant OANGBANDTK]} {
		set easy_floor 1
	}
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		set query_flag [Setting floor_query_flag]
	}
	if {[variant ZANGBANDTK]} {
		set query_flag [Setting other_query_flag]
	}

	set count [llength $floorItemList]

	if {$easy_floor} {

		# multiple items
		if {$count > 1} {
			set itemChar [string index "abcdefghijklmnopqrstuvw" $floorIndex]
			return $toggleChar$itemChar

		# one item
		} else {
			if {!$allowInven || $query_flag} {
				set itemChar a
			} else {
				set itemChar ""
			}
			return $toggleChar$itemChar
		}

	# without easy_floor
	} else {

		# multiple items
		if {$count > 1} {
			if {$query_flag} {
				set prompt [string repeat n $floorIndex]y
				return $toggleChar$prompt

			# without query_flag only top item may be selected
			} else {
				return $toggleChar
			}

		# one item
		} else {
			if {$query_flag} {
				set prompt y
			} else {
				set prompt ""
			}
			return $toggleChar$prompt
		}
	}
}

proc NSAutobar::hook_item {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		has_cmd {
			set other [lassign $args wheres char]
			foreach where $wheres {
				set itemList [angband $where find {*}$other]
				if {[llength $itemList]} {
					return 1
				}
			}
			return 0
		}

		set_list {

			set other [lassign $args wheres char]

			Info $oop cmdChar $char

			set textBox [Info $oop text]

			# Build a list {where N index}
			set match {}
			foreach where $wheres {
				set itemList [angband $where find {*}$other]
				# Add a separator
				if {[llength $itemList] && [llength $match]} {
					lappend match - - -
				}
				set n 0
				foreach index $itemList {
					lappend match $where $n $index
					incr n
				}
			}

			# Process each item
			foreach {where n index} $match {

				# Separator
				if {$where eq "-"} {
					$textBox insert end "   [string repeat - 30]   \n"
					lappend ::NSAutobar($oop,match) {}
					continue
				}

				# Get item info
				angband $where info $index attrib

				if {$where eq "floor"} {
					set attrib(char) [string index "abcdefghijklmnopqrstuvw" \
						$n]
				}

				# Get the color
				set color [default_tval_to_attr $attrib(tval)]

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $color \
					$where $n $index
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			lassign $args where n index
			set cmdChar [Info $oop cmdChar]
			if {$where eq "floor"} {
				set allowInven 0
				set floorItemList {}
				foreach {where2 n2 index2} [Info $oop match] {
					if {$where2 eq "floor"} {
						lappend floorItemList $index2
					} else {
						set allowInven 1
					}
				}
				set itemChar [GetFloorItemSelectChars $n $floorItemList $allowInven]
			} else {
				angband $where info $index attrib
				set itemChar $attrib(char)
			}
			DoUnderlyingCommand $cmdChar$itemChar
		}

		highlight {
			lassign $args where n index
			NSRecall::RecallObject $where $index
		}
	}

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

proc NSAutobar::hook_cmd_pet {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		set_list {
			set textBox [Info $oop text]

			# Process each command
			foreach {char label} [NSRecall::PetCmdInfo mode] {

				if {$char eq $mode} {
					set color [Value TERM_L_BLUE]
				} else {
					set color White
				}

				# Append the character and description
				NewListItem $oop $char $label $color \
					$char
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			if {![Info $oop choosing]} return
			lassign $args char
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

proc NSAutobar::hook_mindcraft {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		has_cmd {
			if {[angband player class] eq "Mindcrafter"} {
				foreach power [angband mindcraft get] {
					angband mindcraft info $power attrib
					if {$attrib(okay)} {
						return 1
					}
				}
			}
			return 0
		}

		set_list {

			set textBox [Info $oop text]

			# Get the list of mindcraft powers
			set powerList [angband mindcraft get]

			# Process each power
			foreach power $powerList {

				angband mindcraft info $power attrib
				if {!$attrib(okay)} continue

				set color White

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $color \
					$power $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			lassign $args power char
			DoKeymapCmd {} m $char
		}

		highlight {
			lassign $args power char
			NSRecall::RecallMindcraft $power
		}
	}

	return
}

proc NSAutobar::hook_power {oop message args} {

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

	switch -- $message {

		open {
		}

		close {
		}

		has_cmd {
			if {[llength [angband power get]]} {
				return 1
			}
			return 0
		}

		set_list {

			set textBox [Info $oop text]

			# Get the list powers
			set powerList [angband power get]

			set i 0

			# Check each power
			foreach power $powerList {

				# Get information about this power
				angband power info $power attrib

				# Get the power char
				set attrib(char) [string index $powerChars $i]

				set fill White
				if {$attrib(chance) == 100 ||
					$attrib(level) > [angband player level]} {
					set fill gray70
				}

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $fill \
					$power $attrib(char)

				incr i
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			lassign $args power char
			DoKeymapCmd {} U $char
		}

		highlight {
			lassign $args power char
			NSRecall::RecallPower $power
		}
	}

	return
}

# ZANGBANDTK
}

proc NSAutobar::hook_spell {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		has_cmd {

			# Get the book k_idx
			set bookNum [lindex $args 0]

			# Find this book in the inventory
			set index [angband inventory find -k_idx $bookNum -limit 1]

			# The book isn't being carried
			if {$index == {}} {

				# No valid choices
				return 0
			}

			# Check each spell
			foreach spell [angband spell find $bookNum -verify_mana yes] {

				# Get information about the spell
				angband spell info $bookNum $spell attrib

				# The spell is unknown
				if {$attrib(info) eq "unknown"} {

					# The spell can't be studied
					if {![angband player new_spells] ||
						($attrib(level) > [angband player level])} {

						# Ignore this spell
						continue
					}
				}

				# The spell is forgotten
				if {$attrib(info) eq "forgotten"} {

					# Ignore this spell
					continue
				}

				# A known/study-able spell exists
				return 1
			}

			# No valid choices
			return 0
		}

		set_list {

			# Get the book number
			set bookNum [lindex $args 0]

			# Find this book in the inventory
			set index [angband inventory find -k_idx $bookNum -limit 1]

			# The player isn't carrying this book
			if {$index == {}} {
				return
			}

			# Get info about the book
			angband inventory info $index iattrib

			# Remember the book char
			Info $oop hook,bookChar $iattrib(char)

			# Remember the book k_idx
			Info $oop hook,bookNum $bookNum

			set textBox [Info $oop text]

			# Get player mana
			set pymana [lindex [angband player mana] 0]

			# Get a list of legal spells
			set spellList [angband spell find $bookNum -verify_mana yes]

			# Process each spell
			foreach spell $spellList {

				# Get information about this spell
				angband spell info $bookNum $spell attrib

				# Colorize the spell name (from OAngband)
				switch -- $attrib(info) {
					forgotten {
						# The spell is forgotten
						if {$attrib(info) eq "forgotten"} {

							# Ignore this spell
							continue
						}
					}
					unknown {
						set fill gray70

						# The character can learn this spell
						if {[angband player new_spells] && 
							($attrib(level) <= [angband player level])} {
							set fill [Value TERM_L_GREEN]

						# The spell can't be studied
						} else {
							continue
						}

						set status study
					}
					untried {
						set fill [Value TERM_L_BLUE]
						set status untried
					}
					default {
						set fill White
						if {$attrib(mana) > $pymana} {
							set fill gray70
						}
						set status cast
					}
				}

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $fill \
					$spell $status
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			lassign $args index status

			# Append spell char by default
			set doSpellChar 1

			# Examine the spell status
			switch -- $status {
				study {
					set command "DoUnderlyingCommand G"

					# Hack -- Bypass the "You can learn N new spells" prompt
					if {[variant ZANGBANDTK]} {
						append command "\\n"
					}

					# Study a random prayer
					set spellType [angband player prayer_or_spell]
					if {$spellType eq "prayer"} {
						set doSpellChar 0
					}
				}
				cast -
				untried {
					GetMagicInfo charCmd magicType
					set command "DoUnderlyingCommand $charCmd"
				}
			}

			# Book char in inventory
			append command [Info $oop hook,bookChar]

			# Add spell char if needed
			if {$doSpellChar} {

				# Book k_idx
				set bookNum [Info $oop hook,bookNum]

				# Get information about this spell
				angband spell info $bookNum $index attrib

				# Spell char in book
				append command $attrib(char)
			}

			# Feed the Term
			eval $command
		}

		highlight {
			lassign $args index status
			set bookNum [Info $oop hook,bookNum]
			NSRecall::RecallSpell $bookNum $index
		}
	}

	return
}

# NSAutobar::ValueChanged_font_autobar --
#
#	Called when the font,autobar value changes.
#	Updates the autobar list.
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

# NSAutobar::SetButtonSize --
#
#	Called when the autobar_button_size setting changes.
#	Updates the size of the buttons.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAutobar::SetButtonSize {oop size} {

	# FIXME: This gets called whenever the Options Window is displayed
	if {$size == [MInfo buttonWidth]} return

	if {$size < 18 || $size > 32} {
		set size 18
	}
	MInfo buttonWidth $size
	MInfo buttonHeight $size

	set canvas [Info $oop canvas]
	$canvas configure -height [expr {[MInfo buttonHeight] + 4}]

	# The buttons are resized below but positioned by ShowBar
	set x 0
	set y 0
	set bw $size
	set bh $size

	# Check each button
	for {set i 1} {$i <= [Info $oop nextButton]} {incr i} {
		$canvas coords border$i $x $y [expr {$x + $bw - 1}] [expr {$y + $bh - 1}]
		$canvas coords img$i [expr {$x + $bw / 2}] [expr {$y + $bh / 2}]
	}

	return
}

