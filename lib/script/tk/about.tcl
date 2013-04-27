# File: about.tcl

# Purpose: the About Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSAbout {

	variable Priv

# namespace eval NSAbout
}

# NSAbout::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAbout::InitModule {} {

	variable Priv

	InitWindow

	# Position the window the first time
	WindowPosition $Priv(win) 2 3

	return
}

# NSAbout::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAbout::CloseModule {} {

	variable Priv

	catch {
		destroy $Priv(win)
	}

	return
}

# NSAbout::InitWindow --
#
#	Create the About Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAbout::InitWindow {} {

	global Angband
	variable Priv
	
	set win .about
	toplevel $win
	wm title $win "About ZAngband"
	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSAbout::Close"

	set Priv(win) $win

	# Click/Escape to hide the window
	bind $win <ButtonPress-1> "NSAbout::Close"
	bind $win <KeyPress-Escape> "NSAbout::Close"

	set width 350
	set height 250
	set height2 40

	set fg Black
	set bg White

	set canvas $win.canvas
	canvas $canvas \
		-borderwidth 0 -highlightthickness 0 \
		-width $width -height $height -background $bg

	set font [Global font,sys,normal]
	set rowHeight [font metrics $font -linespace]
	for {set row 1} {$row <= 9} {incr row} {
		set fill gray[expr {$row * 10}]
		$canvas create text [expr {$width / 2}] \
			[expr {$height - $height2 - 6 - $row * $rowHeight}] \
			-fill $fill -font $font -anchor n -tags row$row
	}
	
	pack $canvas -padx 1 -pady 1
	
	if {[Platform unix]} {
		set font {Times 24 bold}
		set font2 {Times 12}
	}
	if {[Platform windows]} {
		set font {Times 18 bold}
		set font2 {Times 10}
	}
	set anchor nw
	set x 11
	set y 11
	set lineSpace [font metrics $font -linespace]

	# Create a "shadow" for the text below
	$canvas create text $x $y -font $font -anchor $anchor -fill gray \
		-text "ZAngband"
	$canvas create text $x [expr {$y + $lineSpace}] -font $font2 \
		-text "Copyright (c) 1997-2001 Tim Baker" -anchor $anchor -fill gray

	# Draw text over the shadow created above
	incr x -1
	incr y -1
	$canvas create text $x $y -text "ZAangband" \
		-font $font -anchor $anchor -fill $fg
	$canvas create text $x [expr {$y + $lineSpace}] -font $font2 \
		-text "Copyright (c) 1997-2001 Tim Baker" -anchor $anchor -fill $fg \
		-tags copyright

	# Scrolling text
	scan [$canvas bbox copyright] "%s %s %s %s" left top right bottom
	set height3 [expr {$height - ($height2 + 8) - ($bottom + 8)}]
	canvas $canvas.canvas \
		-borderwidth 0 -highlightthickness 0 \
		-width $width -height $height3 -background gray90
	place $canvas.canvas -x 0 -y [expr {$bottom + 8}] -anchor nw
	
	# Rectangle at bottom
	$canvas create rectangle 0 [expr {$height - $height2}] $width $height \
		-fill $fg
	set text "ZAngband is running on:"
	append text "\nTcl $::tcl_patchLevel   Tk $::tk_patchLevel"
	set os $::tcl_platform(os)

	append text "   $os $::tcl_platform(osVersion)"
	$canvas create text [expr {$width / 2}] [expr {$height - $height2 + 6}] \
		-text $text -fill gray80 -justify center -anchor n -tags message

	# Add some text to the scrolling canvas
	set canvas $canvas.canvas

	set data [list \
		h1 ZAngbandTk \
		txt "Tim Baker" \
		br {} \
		h1 ZAngband \
		txt "ZAngband Dev Team" \
		br {} \
		h1 Graphics \
		txt "David E. Gervais" \
		txt "Adam Bolt" \
		txt "Aaron Funk" \
	]
	lappend data \
		br {} \
		h1 "The AngbandTk Home Page" \
		txt "http://persweb.direct.ca/dbaker/angbandtk.html/" \
		br {} \
		h1 "Mailing List" \
		txt "http://www.groups.yahoo.com/group/angbandtk/" \
		txt "mailto:angbandtk@yahoogroups.com" \
		br {} \
		h1 "Thangorodrim - The Angband Page" \
		txt "http://thangorodrim.angband.org/" \
		br {} \
		h1 "Tcl and Tk" \
		txt "ActiveState" \
		txt "http://www.activestate.com/"
	if {[Platform windows]} {
		lappend data \
			br {} \
			h1 "The Cygwin Project" \
			txt "http://sourceware.cygnus.com/cygwin/" \
	}

	set y 0
	set x [expr {[winfo reqwidth $canvas] / 2}]
	unset font
	if {[Platform unix]} {
		set font(h1) "Helvetica 14 bold"
		set font(txt) "Helvetica 12"
	}
	if {[Platform windows]} {
		set font(h1) "{MS Sans Serif} 10 bold"
		set font(txt) "{MS Sans Serif} 9"
	}
	foreach {tag text} $data {
		switch -- $tag {
			h1 -
			txt {
				set id [$canvas create text $x $y -justify center -anchor n \
					-font $font($tag) -text $text]
				scan [$canvas bbox $id] "%s %s %s %s" left top right bottom
				incr y [expr {$bottom - $top}]
			}
			br {
				incr y 12
			}
		}
	}

	set Priv(canvas) $canvas
	set Priv(idle) {}
	set Priv(delay) 40

	return
}

# NSAbout::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAbout::Close {} {

	variable Priv

	wm withdraw $Priv(win)
	after cancel $Priv(idle)

	return
}

# NSAbout::Scroll --
#
#	Called as an "after" command to scroll the text in the About Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAbout::Scroll {} {

	variable Priv

	set canvas $Priv(canvas)

	if {![winfo exists $canvas]} return
	
	if {$::DEBUG} {
		if {![winfo ismapped $Priv(win)]} {
			tk_messageBox -message "NSAbout::Scroll while window unmapped"
			return
		}
	}
	
	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	set height [expr {$bottom - $top}]
	set y $top

	if {$bottom <= 0} {
		set y [winfo height $canvas]
	} else {
		incr y -1
	}

	$canvas move all 0 [expr {$y - $top}]

	set Priv(idle) [after $Priv(delay) NSAbout::Scroll]

	return
}

# NSAbout::About --
#
#	Called as an "after" command to scroll the text in the About Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAbout::About {} {

	variable Priv

	set canvas $Priv(canvas)

	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	set y [winfo height $canvas]
	$canvas move all 0 [expr {$y - $top}]

	WindowBringToFront $Priv(win)
	
	set Priv(idle) [after $Priv(delay) NSAbout::Scroll]

	return
}
