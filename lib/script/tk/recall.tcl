# File: recall.tcl

# Purpose: the Recall Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSRecall {

	variable Priv

# namespace eval NSRecall
}

# NSRecall::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::InitModule {} {

	variable Priv

	set Priv(icon,valid) 0
	set Priv(icon,known) 0

	set oop [NSObject::New NSRecall]

	# Update ourself when the font for the Recall Window changes
	NSValueManager::AddClient font,recall \
		"NSRecall::ValueChanged_font_recall $oop"

	return
}

# NSRecall::NSRecall --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::NSRecall {oop} {

	Info $oop showIcon [Value recall,showicon]

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow recall $win \
		"NSRecall::GeometryCmd $oop" \
		"" \
		"NSRecall::DisplayCmd $oop"

	# If the Choice Window is displayed, we don't show choices in
	# the Recall Window.
	Info $oop clientId,choicewindow \
		[NSValueManager::AddClient choicewindow,show {
			NSRecall::SetHook [Global recall,oop] ""
		}]

	if {$::DEBUG} {
		set ::debug_display 0
	}

	Info $oop hook ""
	Info $oop busy 0
	Info $oop expanded 0
	Info $oop current ""
	Info $oop inConfigure 0

	# Kind of information currently displayed
	Info $oop display ""

Info $oop monsterMem ""

	#
	# Global list of application windows
	#

	Global recall,oop $oop
	Window recall $win

	return
}

# NSRecall::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Info {oop info args} {

	global NSRecall

	# Verify the object
	NSObject::CheckObject NSRecall $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSRecall($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSRecall($oop,$info)
			}
		}
	}

	return
}

# NSRecall::InitWindow --
#
#	Create a recall window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::InitWindow {oop} {

	set win .recall$oop
	toplevel $win
	wm title $win "Recall"

	wm transient $win [Window main]

	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $win
	
	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSRecall::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Turn off geometry propagation for the window
	pack propagate $win no

	# Set instance variables
	Info $oop win $win

	set frame $win.frame
	frame $frame -relief sunken -borderwidth 1 -background Black

	# Canvas to display icon
	set iconSize [expr {[icon size] + 8}]
	set canvas $frame.icon
	canvas $canvas \
		-borderwidth 0 -width $iconSize -height $iconSize -background Black \
		-highlightthickness 0
if 0 {
	$canvas create widget \
		6 6 -tags icon
}
	$canvas create rectangle \
		4 4 [expr {6 + [icon size] + 1}] [expr {6 + [icon size] + 1}] \
		-outline Black -tags focus
if 0 {
# Problems with highlight when Knowledge window appears, so skip it
	$canvas bind icon <Enter> {
#		%W itemconfigure focus -outline gray60
	}
	$canvas bind icon <Leave> {
		%W itemconfigure focus -outline Black
	}
	$canvas bind icon <ButtonPress-1> "
		$canvas move icon 1 1
		set CanvasButtonDown 1
	"
	$canvas bind icon <Button1-Enter> "
		$canvas move icon 1 1
		set CanvasButtonDown 1
	"
	$canvas bind icon <Button1-Leave> "
		$canvas move icon -1 -1
		set CanvasButtonDown 0
	"
	$canvas bind icon <ButtonRelease-1> "
		if {\$CanvasButtonDown} {
			$canvas move icon -1 -1
			update idletasks
			NSRecall::DisplayKnowledge $oop
		}
	"
}
	# Create an arrow which appears when there is content out of site
	set x [expr {$iconSize / 2}]
	$canvas create polygon [expr {$x - 3}] 46 [expr {$x + 3}] 46 \
		$x 49 -fill Red -outline Red -tags arrow

	set wrap word
	text $frame.text \
		-wrap $wrap -width 1 -height 1 -font [Value font,recall] \
		-borderwidth 0 -setgrid no -highlightthickness 0 \
		-padx 4 -pady 2 -background Black -foreground White -cursor ""
	bindtags $frame.text [list $frame.text $win all]

	pack $frame \
		-expand yes -fill both

	grid rowconfig $frame 0 -weight 1
	grid columnconfig $frame 0 -weight 0
	grid columnconfig $frame 1 -weight 1

	grid $frame.icon -in $frame \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.text -in $frame \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news

	if {![Info $oop showIcon]} {
		grid remove $frame.icon
	}

	# Set instance variables
	Info $oop icon $frame.icon
	Info $oop text $frame.text

	# Window expands and contracts as the mouse enters and leaves it
	bindtags $win [concat [bindtags $win] RecallBindTag]
	bind RecallBindTag <Enter> "NSRecall::Expand $oop"
	bind RecallBindTag <Leave> "NSRecall::Contract $oop"

	# When the window changes size, reposition the indicator arrow
	bind $frame.text <Configure> \
		"NSRecall::Configure $oop"

	# Fiddle with the selection for list behaviour
	$frame.text tag configure HOT -foreground White \
		-background [Value listHilite]

	$frame.text tag bind HOT <ButtonPress-1> \
		"NSRecall::Invoke $oop \[$frame.text index {@%x,%y linestart}]"
	$frame.text tag bind TEXT <Motion> \
		"NSRecall::Motion $oop \[$frame.text index {@%x,%y linestart}]"
	$frame.text tag bind HOT <Leave> \
		"NSRecall::Motion $oop {}"

	if {[Platform unix]} {
	
		# When the inactive window is clicked, I get a <Leave> event
		# followed by an <Enter> event. The <Leave> Contract()'s the window
		# and removes the highlight (if any).
	
		bind RecallWindowBindTag <Leave> "
			if {!\[NSRecall::HasCursor $oop]} {
				NSRecall::Contract $oop
			}
		"
		$frame.text tag bind HOT <Leave> "
			if {!\[NSRecall::CursorHot $oop %x %y]} {
				NSRecall::Motion $oop {}
			}
		"
		bind $win <Leave> "
			if {!\[NSRecall::HasCursor $oop]} {
				NSRecall::Motion $oop {}
			}
		"
	
		proc CursorHot {oop x y} {
			set text [Info $oop text]
			if {![llength [$text tag ranges HOT]]} {return 0}
			set index [$text index @$x,$y]
			if {[$text compare $index < HOT.first] ||
				[$text compare $index > HOT.last]} {return 0}
			return 1
		}
	}

	#
	# Context Menu
	#
	
	set menu $win.context
	menu $menu -tearoff 0
	bind $frame.icon <ButtonPress-3> \
		"NSRecall::ContextMenu $oop $menu %X %Y"
	bind $frame.text <ButtonPress-3> \
		"NSRecall::ContextMenu $oop $menu %X %Y"

	return
}

# NSRecall::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
		}
		postDisplay {
			Value recall,show 1
		}
		postWithdraw {
			SetHook $oop ""
			Value recall,show 0
		}
	}

	return
}

# NSRecall::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]
	set spacing 0

	set left 0
	set top 0
	set right [winfo screenwidth .]
	set bottom [winfo screenheight .]

	set x [NSToplevel::FrameLeft $winMain]
	if {[Value choicewindow,show]} {
		set width [NSToplevel::ContentWidth $win \
			[expr {[NSToplevel::TotalWidth $winMain] / 2}]]
	} else {
		set width [NSToplevel::TotalWidth $winMain]
		if {$width > 400} {
			set width 400
		}
		set width [NSToplevel::ContentWidth $win $width]
	}

	set y [expr {[NSToplevel::FrameBottom $winMain] + $spacing}]
	if {$bottom - $y < 100} {
		set y [expr {$bottom - 100}]
		set height [NSToplevel::ContentHeight $win 100]
	} elseif {($y + [NSToplevel::TotalHeight $win]) < $bottom} {
		set height [winfo height $win]
	} else {
		set height [expr {$bottom - [NSToplevel::FrameBottom $winMain]}]
		set height [NSToplevel::ContentHeight $win $height]
	}

	return ${width}x$height+$x+$y
}

# NSRecall::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::Close {oop} {

	NSWindowManager::Undisplay recall

	return
}



# NSRecall::RecallSpell --
#
#	Show info about a spell.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallSpell {bookNum index} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]
	
	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get information about the spell
	angband spell info $bookNum $index attrib

	# Get the book icon
	set icon [angband k_info info $bookNum icon]

	# Color
	switch -- $attrib(info) {
		unknown {
			set color gray70
			
			# The character can learn this spell
			if {[angband player new_spells] && 
				($attrib(level) <= [angband player level])} {
				set color [Value TERM_L_GREEN]
			}
		}
		untried {
			set color [Value TERM_L_BLUE]
		}
		default {
			set color White
		}
	}

	# Get the name
	set name $attrib(name):

	# Get the memory
	set memory [angband spell memory $bookNum $index]

	# Extra info
	if {[string length $memory]} {
		append memory \n
	}
	append memory "Level $attrib(level)  Mana $attrib(mana) \
		Fail $attrib(chance)%"
	if {[string length $attrib(info)]} {
		append memory "\n$attrib(info)"
	}

	# Set the text
	SetText $oop $icon $color $name $memory

	return
}


# NSRecall::SetText --
#
#	Description.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::SetText {oop icon color title text} {

	global NSRecall
	variable Priv

	set win [Info $oop win]
	set textBox [Info $oop text]
	
	# If we are in "list mode", then do not set the text. This may
	# happen if we are waiting for an object to be chosen, and the
	# user highlights an object in the Inventory Window, which would
	# ordinarily display the object memory.
	if {[string length [Info $oop hook]]} {
		return
	}

	# Display the icon
	[Info $oop icon] itemconfigure icon -assign $icon

	# Delete
	$textBox delete 1.0 end

	# Insert title if any
	if {[string length $title]} {

		# Title (color?)
		$textBox insert end $title\n
		$textBox tag add TAG_STYLE 1.0 {end -1 chars}
		$textBox tag configure TAG_STYLE -foreground $color
	}

	# Insert text if any
	set text [string trim $text]
	if {[string length $text]} {

		# Text
		$textBox insert end $text
	}

	set Priv(icon,valid) 0

	# Synchronize the indicator arrow
	ContentChanged $oop
	
	return
}

# NSRecall::IconChanged --
#
#	The icon of the recalled monster/object is displayed in the
#	Recall Window. If that monster or object is assigned a different
#	icon, we want to update the display. This is called as a
#	qebind command on the "Assign" quasi-event.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::IconChanged {oop to toindex assign} {

	variable Priv

	if {!$Priv(icon,valid)} return
	if {[string equal $to $Priv(icon,to)] && ($toindex == $Priv(icon,toindex))} {
		[Info $oop icon] itemconfigure icon -assign $assign
	}		

	return
}


# NSRecall::DisplayKnowledge --
#
#	Display the Knowledge Window for the displayed monster or object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::DisplayKnowledge {oop} {

	variable Priv

	if {!$Priv(icon,valid)} return

	# This can't work when an unknown flavored object is displayed
	if {!$Priv(icon,known)} return

	if {[string compare $Priv(icon,to) monster] &&
		[string compare $Priv(icon,to) object]} {
		return
	}

	angband_display knowledge show $Priv(icon,to) $Priv(icon,toindex)

	return
}


# NSRecall::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::SetHook {oop hook} {

	if {[string length $hook]} {
		Info $oop hook NSRecall::$hook
		CallHook $oop open
		if {$::DEBUG} {
			set ::debug_display 1
		}
	} elseif {[string length [Info $oop hook]]} {
		Info $oop hook ""
		Restore $oop
		if {$::DEBUG} {
			set ::debug_display 0
		}
	}

	return
}

# NSRecall::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::CallHook {oop message args} {

	return [uplevel #0 [Info $oop hook] $oop $message $args]
}

# NSRecall::Fresh_Display --
#
#	Calls the hook to set the list, if required. Called as a command
#	on the "Term-fresh" quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Fresh_Display {oop} {

	ASSERT {$::debug_display == 1} \
		"Fresh_Display called with debug_display=0!"

	CallHook $oop fresh

	# If the cursor is inside the Recall Window, we will attempt to
	# expand it.
	set pointerx [winfo pointerx .]
	set pointery [winfo pointery .]
	set toplevel [winfo containing $pointerx $pointery]
	if {[string length $toplevel] && \
		[string equal [winfo toplevel $toplevel] [Info $oop win]]} {
		Expand $oop
	}

	return
}

# NSRecall::SetList --
#
#	Clears the recall text, sets the icon to "none 0" and calls the
#	hook to set the text.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::SetList {oop} {

	set win [Info $oop win]
	set textBox [Info $oop text]

	# Clear the text
	$textBox delete 1.0 end

	# Clear the icon
	[Info $oop icon] itemconfigure icon -assign {icon none 0}

	# Call the hook to set the list
	CallHook $oop set_list

	# Something is displayed
	Info $oop display something

	# No item is highlighted
	Info $oop current ""	

	# Synchronize the indicator arrow
	ContentChanged $oop

	return
}

# NSRecall::Invoke --
#
#	Called when a list item is clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Invoke {oop index} {

	set textBox [Info $oop text]
set index [Info $oop current]
	set row [expr {[lindex [split $index .] 0] - 1}]

	CallHook $oop invoke $row

	return
}

# NSRecall::Motion --
#
#	Called when the mouse moves in a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Motion {oop index} {

	# If you invoke an item, hold down the mouse, and drag...
	if {![string length [Info $oop hook]]} return

	# No tracking while menu is up
	if {[Info $oop busy]} return

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

# NSRecall::HighlightItem --
#
#	Highlights a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::HighlightItem {oop index} {

	set textBox [Info $oop text]
	set row [expr {[lindex [split $index .] 0] - 1}]

	# Highlight the item
	$textBox tag add HOT $index "$index lineend"
	$textBox tag raise HOT

	# Call the hook (to set the icon, for example)
	CallHook $oop highlight $row

	return
}

# NSRecall::UnhighlightItem --
#
#	Removes highlighting from a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::UnhighlightItem {oop index} {

	set win [Info $oop win]
	set textBox [Info $oop text]

	# Unhighlight the item
	$textBox tag remove HOT 1.0 end

	# Clear the icon
	[Info $oop icon] itemconfigure icon -assign {icon none 0}

	return
}

# NSRecall::HasCursor --
#
#	See if the cursor is over the window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::HasCursor {oop} {

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

# NSRecall::Expand --
#
#	Resizes the Recall Window to display all of the information in it.
#	Does nothing if the window is already expanded. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Expand {oop} {

	variable Priv

#	if {![string length [Info $oop hook]]} return
	if {[Info $oop busy]} return
	if {[Info $oop expanded]} return

	set win [Info $oop win]
	set textBox [Info $oop text]

	set textHeight [winfo height $textBox]
	set lineHeight [font metrics [Value font,recall] -linespace]

	# Hack -- In order to find out how much space is taken up by the
	# text in the text widget, I create a canvas text item with the
	# proper attributes and calculate its size. The width is width-8
	# and height-4 because of the internal padding of the text
	# widget. I added 2 to each adjustment as a hack.
	set padx [$textBox cget -padx]
	set pady [$textBox cget -pady]
	set itemId [[Info $oop icon] create text 1 1 -font [Value font,recall] \
		-width [expr {[winfo width $textBox] - $padx * 2 - 1}] -anchor nw \
		-text [$textBox get 1.0 end]]
	set bbox [[Info $oop icon] bbox $itemId]
	set height [expr {[lindex $bbox 3] - [lindex $bbox 1] + $pady * 2 + 2}]

	# Hmmm... Is there a trailing newline, or what?
	incr height -$lineHeight

	# Delete the temp canvas item
	[Info $oop icon] delete $itemId

	set winHeight [winfo height $win]
	set winWidth [winfo width $win]

	if {$height <= $winHeight} return

	# If the window is closer to the top of the screen, then
	# expand downwards, otherwise expand upwards.
	set top [NSToplevel::FrameTop $win]
	set topDist $top
	if {$topDist < 0} {set topDist 0}
	set bottom [NSToplevel::FrameBottom $win]
	set bottomDist [expr {[winfo screenheight $win] - $bottom}]
	if {$bottomDist < 0} {set bottomDist 0}
	if {$topDist < $bottomDist} {
		set expandUp 0
	} else {
		set expandUp 1
	}

	# Save the current window geometry
	Info $oop geometry [wm geometry $win]

	Info $oop busy 1

	raise $win
	set x [NSToplevel::FrameLeft $win]
	if {$expandUp} {
		set y [expr {[NSToplevel::FrameTop $win] - ($height - $winHeight)}]
	} else {
		set y [NSToplevel::FrameTop $win]
	}
	wm geometry $win ${winWidth}x$height+$x+$y
	update

	Info $oop expanded 1
	Info $oop busy 0

	# If the cursor moved outside the Recall Window, collapse it
	if {![HasCursor $oop]} {
		Contract $oop
	}

	return
}

# NSRecall::Contract --
#
#	Restores the window geometry to the size it was before it was
#	expanded. Does nothing if the window is not expanded. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Contract {oop} {

	if {[Info $oop busy]} return
	if {![Info $oop expanded]} return

	Info $oop busy 1

	set win [Info $oop win]
	wm geometry $win [Info $oop geometry]
	update

	Info $oop expanded 0
	Info $oop busy 0

	return
}

# NSRecall::Restore --
#
#	Description. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Restore {oop} {

	if {![string length [Info $oop display]]} return
	SetText $oop {icon none 0} {} {} {}
	Contract $oop
	Info $oop display ""

	return
}

# NSRecall::ContextMenu --
#
#	When the window is right-clicked, pop up a menu of options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::ContextMenu {oop menu x y} {

	set text [Info $oop text]

	$menu delete 0 end

	$menu add command -label "Set Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font recall"
	$menu add checkbutton -label "Show Icon" \
		-variable ::NSRecall($oop,showIcon) \
		-command "NSRecall::OptionChanged $oop showIcon showicon"
	$menu add separator
	$menu add command -label "Cancel"

	# Hack -- Try to prevent collapsing while popup is visible.
	# It would be nice if "winfo ismapped $menu" worked
	Info $oop busy 1

	# Pop up the menu
	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
	}

	Info $oop busy 0

	set index ""
	if {[NSUtils::HasCursor $text]} {
		set x [expr {[winfo pointerx $text] - [winfo rootx $text]}]
		set y [expr {[winfo pointery $text] - [winfo rooty $text]}]
		set index2 [$text index @$x,$y]
		foreach tag [$text tag names $index2] {
			if {[string equal $tag TEXT]} {
				set index "$index2 linestart"
				break
			}
		}
	}
	Motion $oop $index

	return
}

# NSRecall::OptionChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::OptionChanged {oop info keyword} {

	set setting [Info $oop $info]
	Value recall,$keyword $setting
	switch -- $keyword {
		showicon {
			if {$setting} {
				grid [Info $oop icon]
			} else {
				grid remove [Info $oop icon]
			}
		}
	}

	return
}

# NSRecall::Configure --
#
#	Called as a <Configure> event script. Positions the indicator
#	arrow (the one which tells us if there is more information out
#	of site) near the bottom of the window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Configure {oop} {

	set win [Info $oop win]
	set canvas [Info $oop icon]
	set text [Info $oop text]

	scan [$canvas bbox arrow] "%s %s %s %s" left top right bottom
	set height [winfo height $text]
	$canvas move arrow 0 [expr {$height - $bottom - 4}]

	ContentChanged $oop

	return
}

# NSRecall::ContentChanged --
#
#	Called when the information displayed has changed. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::ContentChanged {oop} {

	set win [Info $oop win]
	set canvas [Info $oop icon]
	set text [Info $oop text]

	scan [$text yview] "%f %f" top bottom
	if {$bottom < 1} {
		set fill Red
	} else {
		set fill [$canvas cget -background]
	}

	$canvas itemconfigure arrow -fill $fill -outline $fill

	return
}

# NSRecall::Choose --
#
#	Handle <Choose> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Choose {oop what show args} {

	if {[lsearch -exact [list cmd_pet ele_attack item] \
		$what] == -1} return

	if {!$show} {
		SetHook $oop {}
		return
	}

	switch -- $what {
		cmd_pet {
			SetHook $oop hook_cmd_pet
		}
		ele_attack {
			SetHook $oop hook_ele_attack
		}
	}

	return
}


proc NSRecall::MenuSelect {menu hook} {

	set index [$menu index active]
	eval $hook [Global recall,oop] menu_select $menu $index

	return
}


proc NSRecall::PetCmdInfo {_mode} {

	upvar $_mode mode

	set letters abcdefgh
	set index -1

	if {[llength [angband player pets]]} {
		set char [string index $letters [incr index]]
		lappend data $char "Dismiss pets"
	}

#	set dist [struct set player_type 0 pet_follow_distance]
	set mode ""

	set char [string index $letters [incr index]]
	lappend data $char "Stay close"
	if {$dist == [const PET_CLOSE_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char "Follow me"
	if {$dist == [const PET_FOLLOW_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char "Seek and destroy"
	if {$dist == [const PET_DESTROY_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char "Give me space"
	if {$dist == [const PET_SPACE_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char "Stay away"]	if {$dist == [const PET_AWAY_DIST]} {
		set mode $char
	}
		
	set char [string index $letters [incr index]]
	lappend data $char "Allow open doors"

	set char [string index $letters [incr index]]
	lappend data $char "Allow pickup items"

	return $data
}

proc NSRecall::hook_cmd_pet {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		set_list {
		
			set textBox [Info $oop text]

			# Keep a list of invoke chars
			set match {}
			
			# Process each command
			foreach {char label} [PetCmdInfo mode] {

				if {[string equal $char $mode]} {
					set color [Value TERM_L_BLUE]
				} else {
					set color White
				}

				# Append the character and description
				$textBox insert end "$char\) " TEXT $label \
					[list ITEM_$char TEXT] "\n"
				$textBox tag configure ITEM_$char -foreground $color

				# Keep a list of chars and colors
				lappend match $char
				lappend colors $color
			}
		
			# Delete trailing newline
			$textBox delete "end - 1 chars"
		
			# Keep a list of chars and colors
			Info $oop match $match
			Info $oop color $colors
		}

		get_color {
			set row [lindex $args 0]
			return [lindex [Info $oop color] $row]
		}

		invoke {
			set row [lindex $args 0]
			set char [lindex [Info $oop match] $row]
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

# NSRecall::PopupSelect_CmdPet --
#
#	Show a pop-up menu of pet commands.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::PopupSelect_CmdPet {menu x y} {

	global PopupResult

	set PopupResult 0

	# Clear the menu
	$menu delete 0 end

	set num 0
	foreach {char name} [PetCmdInfo mode] {

		if {[string equal $char $mode]} {
			set ::PopupCheck 1
			$menu add checkbutton -label "$char $name" \
				-command "angband keypress $char ; set PopupResult 1" \
				-underline 0 -variable ::PopupCheck
		} else {
			$menu add command -label "$char $name" \
				-command "angband keypress $char ; set PopupResult 1" \
				-underline 0
		}

		incr num
	}

	$menu add separator
	$menu add command -label "Cancel"

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	tk_popup $menu $x $y [expr {$num / 2}]

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
	}

	# If the user unposts the menu without choosing an entry, then
	# I want to feed Escape into the Term. I tried binding to the <Unmap>
	# event but it isn't called on Windows(TM).
	after idle {
		if {!$PopupResult} {
			angband keypress \033
		}
	}

	return
}


proc NSRecall::hook_xxx {oop message args} {

	switch -- $message {

		set_list {
		}

		get_color {
		}
	}

	return
}

# NSRecall::ValueChanged_font_recall --
#
#	Called when the font,recall value changes.
#	Updates the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::ValueChanged_font_recall {oop} {

	[Info $oop text] configure -font [Value font,recall]

	return
}
