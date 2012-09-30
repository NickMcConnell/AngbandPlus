# File: utils.tcl

# Purpose: various support routines

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# WindowBringToFront --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc WindowBringToFront {windowPath} {

	set focus [focus -lastfor $windowPath]
	if {![winfo ismapped $windowPath]} {
		wm state $windowPath normal
		if {[Platform unix]} {
			# Needed on X11 or big delay
			update
		}
	}
	raise $windowPath
	focus $focus

	return
}

# WindowPosition --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc WindowPosition {win x y} {

	# Save the current focus
#	set oldFocus [focus]

    # Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $win
    update idletasks
    set x2 [expr {([winfo screenwidth $win] - [winfo reqwidth $win]) / $x \
	    - [winfo vrootx $win]}]
    set y2 [expr {([winfo screenheight $win] - [winfo reqheight $win]) / $y \
	    - [winfo vrooty $win]}]
    wm geometry $win +$x2+$y2

	# Seems required on Windows
	update idletasks

    wm deiconify $win
#wm state $win normal
#update idletasks

	# Restore focus
#	catch {focus $oldFocus}

	return
}

namespace eval NSUtils {

	variable Priv

	set Priv(grabDepth) 0

# namespace eval NSUtils
}

# NSUtils::GrabSave --
#
#	Sets a grab on the given window, saving the current focus and
#	grab for a later call to GrabRelease().
#
# Arguments:
#	arg1				about arg1
#
# Results:
#	What happened.

proc NSUtils::GrabSave {win} {

	variable Priv

	set depth [incr Priv(grabDepth)]

	set Priv(grab,$depth,win) $win
	set Priv(grab,$depth,oldFocus) [focus]
	set Priv(grab,$depth,oldGrab) [grab current $win]
	if {$Priv(grab,$depth,oldGrab) != ""} {
		set Priv(grab,$depth,grabStatus) \
			[grab status $Priv(grab,$depth,oldGrab)]
	}
	grab $win

	return
}

# NSUtils::GrabRelease --
#
#	Releases the grab and restores the focus saved in GrabSave().
#
# Arguments:
#	arg1				about arg1
#
# Results:
#	What happened.

proc NSUtils::GrabRelease {win} {

	variable Priv

	set depth $Priv(grabDepth)
	incr Priv(grabDepth) -1

	if {$Priv(grab,$depth,win) != $win} {
		error "grab release on wrong window"
	}

	catch {focus $Priv(grab,$depth,oldFocus)}
	grab release $win
	if {$Priv(grab,$depth,oldGrab) != ""} {
		if {$Priv(grab,$depth,grabStatus) == "global"} {
		    grab -global $Priv(grab,$depth,oldGrab)
		} else {
		    grab $Priv(grab,$depth,oldGrab)
		}
	}

	array unset Priv grab,$depth,*

	return
}

# NSUtils::StringBox --
#
#	Put up a window to get a string from the user.
#
# Arguments:
#	arg1				about arg1
#
# Results:
#	What happened.

proc NSUtils::StringBox {args} {

    variable Priv

    set win .stringbox

	set title "Enter a string"
	set initial ""
	set entryWidth 30
	set message ""
	set prompt String
	set buttons [list OK Cancel]
	set parent [winfo toplevel [focus]]
	set resultVar ""
	set maxLen 0
	set type ""

	foreach {option value} $args {
		switch -- $option {
			-buttons -
			-title  -
			-type -
			-initial -
			-message -
			-prompt -
			-parent {
				set [string range $option 1 end] $value
			}
			-entrywidth {
				if {![string is integer -strict $value]} {
					error "expected integer but got \"$value\""
				}
				set entryWidth $value
			}
			-result {
				set resultVar $value
			}
			-maxlen {
				if {![string is integer -strict $value]} {
					error "expected integer but got \"$value\""
				}
				set maxLen $value
			}
			default {
				error "unknown option \"$option\""
			}
		}
	}

    toplevel $win
    wm title $win $title
    wm resizable $win no no
    wm transient $win $parent

	if {[string length $message]} {
		set msg $win.message
		message $msg \
			-text $message -width 280 -anchor w
	}

	set frame $win.frame
	frame $frame \
		-borderwidth 0
    label $frame.label -text "$prompt:"
    entry $frame.text -width $entryWidth

	if {[string equal $type integer]} {
		if {$maxLen} {
			$frame.text configure \
				-validatecommand "expr [string is integer %P] && (\[string length %P] <= $maxLen)" \
				-invalidcommand bell -validate key
		} else {
			$frame.text configure \
				-validatecommand "string is integer %P" \
				-invalidcommand bell -validate key
		}
	} elseif {$maxLen} {
		$frame.text configure \
			-validatecommand "expr \[string length %P] <= $maxLen" \
			-invalidcommand bell -validate key
	}

	# Initial text
    $frame.text insert 0 $initial

	set frame $win.frameButton
	frame $frame \
		-borderwidth 0

	set n 1
	foreach text $buttons {
	    button $frame.button$n \
			-text $text -command "set NSUtils::Priv(result) $n" \
			-width 9
		pack $frame.button$n \
			-side left -padx 5 -pady 0
		incr n
	}
	incr n -1

	# Set the left-most button to the default button
    $frame.button1 configure -default active

	SetDefaultButton $win $frame.button1
	
    # Return key selects default button
    bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"

    # Escape key selects "cancel" button
    bind $win.frame.text <KeyPress-Escape> \
		"tkButtonInvoke $frame.button$n"

	if {[string length $message]} {
		pack [frame $win.framePad1 -borderwidth 0 -height 10] \
			-side top
		pack $msg \
			-side top -padx 5 -pady 0 -fill x
	}
	pack $win.frame.label \
		-side left -padx 5 -pady 0
	pack $win.frame.text \
		-side left -padx 5 -pady 0
	pack $win.frame \
		-side top -padx 5 -pady 10 -anchor w
	pack $win.frameButton \
		-side top -padx 5 -pady 0 -anchor e
	pack [frame $win.framePad2 -borderwidth 0 -height 10] \
		-side top

	# <Destroy> handler sets Priv(result)
	bind $win <Destroy> "set NSUtils::Priv(result) 2"

	# Position window
	WindowPosition $win 2 3

	# Set up a grab and claim focus too
	GrabSave $win
    focus $win.frame.text

    # Select the text
    $win.frame.text selection range 0 end

    # Wait for a button press
    set Priv(result) ""
    tkwait variable NSUtils::Priv(result)

	# Release grab and reset focus
	GrabRelease $win
	
    set result ""

    if {[winfo exists $win]} {
        switch $Priv(result) {
			1 {
				set result [$win.frame.text get]
			}
		}
    }

    # Maybe the window is already destroyed
    catch {
		bind $win <Destroy> {}
		destroy $win
	}

	if {$resultVar != ""} {
		upvar $resultVar resultCode
		set resultCode [expr {$UtilsPriv(result) == 1}]
	}

    return $result
}

# NSUtils::ZapLabel --
#
#	Clears the given label (if it exists). Usually called as an
#	"after" command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::ZapLabel {label} {

	set text [$label cget -text]
	regsub -all \" $text "\\\"" text

	after 1200 "
		if {\[winfo exists $label]} {
			if {\[string equal \[$label cget -text] \"$text\"]} {
				$label configure -text {}
			}
		}
	"

	return
}

# NSUtils::GetBoolean --
#
#	Almost the same as Tcl_GetBoolean(). Given a string, return
#	0 if the string is 0/false/no/off, return 1 if the string is
#	1/true/yes/on. Case insensitive. Generate an error otherwise.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::GetBoolean {string} {

	if {[string is boolean -strict $string]} {
		return [string is true $string]
	}

	error "expected boolean value but got \"$string\""
}

# NSUtils::HasCursor --
#
#	Return 1 if the cursor is over the given window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::HasCursor {window} {

	set pointerx [winfo pointerx $window]
	set pointery [winfo pointery $window]
	set window2 [winfo containing $pointerx $pointery]
	if {![string length $window2]} {
		return 0
	}
	if {[string compare $window2 $window]} {
		return 0
	}
	return 1
}

# NSUtils::HasFocus --
#
#	Return 1 if the given window has the focus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::HasFocus {window} {

	if {[string equal [focus -displayof $window] $window]} {
		return 1
	}
	return 0
}

# NSUtils::ToplevelHasFocus --
#
#	Return 1 if any childof the given toplevel (or itself) has the focus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::ToplevelHasFocus {top} {

	set focus [focus -displayof $top]
	if {[string equal $focus $top]} {
		return 1
	}
	if {[string match $top* $focus]} {
		return 1
	}
	return 0
}

# NSUtils::DestroyObjectWithWidget --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::DestroyObjectWithWidget {class oop widget} {

	set tag DestroyObjectBindTag$class$oop
	bindtags $widget [concat [bindtags $widget] $tag]
	bind $tag <Destroy> "
		if {\[string equal \$DestroyObjectWithWidget($class,$oop) %W\]} {
			NSObject::Delete $class $oop
			unset DestroyObjectWithWidget($class,$oop)
		}

	"

	# Hack -- Because a menu clone may end up with the bind tag we
	# added to the menu, we will ignore attempts to destroy the object
	# if the original widget isn't being destroyed (ie, the clone is)
	global DestroyObjectWithWidget
	set DestroyObjectWithWidget($class,$oop) $widget

	return
}

# NSUtils::TempFileName --
#
#	Return an unused file name in the given directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::TempFileName {dir} {

	set index 0
	set fileName [file join $dir [format tempFile%04d $index]]
	while {[file exists $fileName]} {
		incr index
		set fileName [file join $dir [format tempFile%04d $index]]
	}
	
	return $fileName
}

# NSUtils::SetDefaultButton --
#
#	Specifies that a given Button should be the default button. Under
#	Win32, when a Button receives the input focus it also becomes the
#	default button; when no Button has the input focus, the toplevel's
#	default button becomes the active button once again.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

bind Button <ButtonPress-1> {+
	if {[%W cget -state] != "disabled"} {
		focus %W
	}
}
bind Radiobutton <ButtonPress-1> {+
	if {[%W cget -state] != "disabled"} {
		focus %W
	}
}
bind Checkbutton <ButtonPress-1> {+
	if {[%W cget -state] != "disabled"} {
		focus %W
	}
}

proc NSUtils::FocusIn_DefaultButton {widget} {

	global DefaultButton

	set toplevel [winfo toplevel $widget]
	if {![info exists DefaultButton(default,$toplevel)]} return

	# A Button has gained the input focus
	if {[winfo class $widget] == "Button"} {

		# The previous Button is no longer the default
		set current $DefaultButton(current,$toplevel)
		if {($current != $widget) && [winfo exists $current]} {
			$current configure -default normal
		}

		# The Button with the focus is now the default
		if {[$widget cget -default] != "active"} {
			$widget configure -default active
		}

		# Remember the current default button
		set DefaultButton(current,$toplevel) $widget

		# Done
		return
	}

	# At this point, a non-Button received the input focus
	
	# Get the default button for the toplevel
	set button $DefaultButton(default,$toplevel)
	if {![winfo exists $button]} return

	# The previous Button is no longer the default
	set current $DefaultButton(current,$toplevel)
	if {($current != $button) && [winfo exists $current]} {
		$current configure -default normal
	}

	# The toplevel's default Button is now the default
	if {[$button cget -default] != "active"} {
		$button configure -default active
		set DefaultButton(current,$toplevel) $button
	}

	return
}

proc NSUtils::SetDefaultButton {toplevel button} {

	global DefaultButton

	set DefaultButton(default,$toplevel) $button
	set DefaultButton(current,$toplevel) $button

	bind $toplevel <FocusIn> {+
		NSUtils::FocusIn_DefaultButton %W
	}
	
	return
}

# NSUtils::InvokeDefaultButton --
#
#	Find a button with "-state active" and invoke it. If there is
#	no such button then do nothing. Typically you bind this to
#	a toplevel for the <KeyPress-Return> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::InvokeDefaultButton {widget} {

	foreach child [winfo children $widget] {
		if {[winfo class $child] == "Button"} {
			if {[$child cget -default] == "active"} {
				tkButtonInvoke $child
				break
			}
		} else {
			InvokeDefaultButton $child
		}
	}
	
	return
}

# NSUtils::SynchScrollBar --
#
#	There is a bug in the Windows Tk which prevents a scroll bar
#	from synchronizing when the window is not mapped. So I bind to
#	the <Map> event and synch the scroll bar here.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::SynchScrollBar {window scrollbar {moveto 0}} {

	if {![Platform windows]} return

	set tag SynchScrlBarBindTag$scrollbar
	bindtags $scrollbar [concat [bindtags $scrollbar] $tag]

	switch -- [$scrollbar cget -orient] {
		horizontal {
			set view xview
		}
		vertical {
			set view yview
		}
	}

	bind $tag <Map> "
		eval $scrollbar set \[$window $view]
		if {$moveto} {
			$window $view moveto 0.0
		}
	"

	bind $tag <Destroy> "
		bind $tag <Map> {}
		bind $tag <Destroy {}
	"

	return
}

# NSUtils::ReadLink --
#
#	On Unix, "file readlink" a link, if it is a link.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUtils::ReadLink {path} {

	if {![Platform unix]} {
		return $path
	}
	if {![file exists $path]} {
		return $path
	}
	if {[string equal [file type $path] link]} {
		return [file readlink $path]
	}
	return $path
}

	# The MS Windows version of Tk supports platform-specific color names
	# all beginning with "System". Each of these colors is set here for
	# each platform.
	switch -- $tcl_platform(platform) {
		macintosh -
		unix {
			# These values from tkUnixDefault.h
			set SystemButtonFace #d9d9d9 ; # NORMAL_BG
			set SystemHighlight #c3c3c3 ; # SELECT_BG
			set SystemHighlightText Black ; # SELECT_FG

			# Hack -- Override
			set SystemHighlight DarkBlue
			set SystemHighlightText White

			# These values guessed
			set SystemButtonHighlight White
			set SystemButtonShadow #808080
			set System3dLight $SystemButtonFace
			set System3dDarkShadow Black

			set SystemInfoBackground White
		}
		windows {
			set SystemButtonFace SystemButtonFace
			set SystemButtonHighlight SystemButtonHighlight
			set SystemButtonShadow SystemButtonShadow
			set SystemHighlight SystemHighlight
			set SystemHighlightText SystemHighlightText
			set System3dLight System3dLight
			set System3dDarkShadow System3dDarkShadow
			set SystemInfoBackground SystemInfoBackground
		}
	}

