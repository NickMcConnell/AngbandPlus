# File: balloon.tcl

# Purpose: Balloon help/tool tip

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBalloon {

	variable Priv

	set Priv(first) 0
	set Priv(showing) 0
	set Priv(win) .balloon
	
	set Priv(delay) 0
	set Priv(delayId) {}
	
	bind BalloonBindTag <Enter> {
	    set NSBalloon::Priv(showing) 0
	    set NSBalloon::Priv(first) 1
	    if {!$NSBalloon::Priv(delay)} {
	    	set NSBalloon::Priv(delay) $NSBalloon::Priv(delay,%W)
	    }
	    set NSBalloon::Priv(id) \
			[after $NSBalloon::Priv(delay) \
			{NSBalloon::Balloon %W}]
	}
	
	bind BalloonBindTag <ButtonPress> {
	    set NSBalloon::Priv(first) 0
	    NSBalloon::Kill %W
	}
	
	bind BalloonBindTag <Leave> {
	    set NSBalloon::Priv(first) 0
	    NSBalloon::Kill %W
	}
	
	bind BalloonBindTag <Motion> {
	    if {$NSBalloon::Priv(showing) == 0} {
	        after cancel $NSBalloon::Priv(id)
	        set NSBalloon::Priv(id) \
	        	[after $NSBalloon::Priv(delay) {NSBalloon::Balloon %W}]
	    }
	}

# namespace eval NSBalloon
}

# NSBalloon::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBalloon::InitModule {} {

    variable Priv

	set Priv(win) .balloon
	catch {destroy $Priv(win)}
	toplevel $Priv(win) -background black
	wm overrideredirect $Priv(win) 1
	wm withdraw $Priv(win)
	label $Priv(win).label \
		-relief flat \
		-background $::SystemInfoBackground -foreground black \
		-padx 2 -pady 0 -anchor w
	pack $Priv(win).label -side left -padx 1 -pady 1

	if {[Platform unix]} {
		$Priv(win) configure -cursor left_ptr
	}

	return
}

proc NSBalloon::Show {text x y {anchor nw}} {

    variable Priv

	$Priv(win).label configure -text $text

if 1 {
	set width [winfo reqwidth $Priv(win).label]
	incr width 2
} else {
	wm geometry $Priv(win) +[winfo screenwidth .]+0
	update idletasks
	set width [winfo reqwidth $Priv(win)]
}
	switch -- $anchor {
		n {
			set x [expr {$x - $width / 2}]
		}
	}

	# Not too far left
	if {$x < 0} {set x 0}

	# Not too far right
	set screenWidth [winfo screenwidth $Priv(win)]
	if {$x + $width > $screenWidth } {
		set x [expr {$screenWidth - $width}]
	}

	wm geometry $Priv(win) +$x+$y
	update idletasks

	# Display and raise, don't change the focus
	wm deiconify $Priv(win)

	if {[Platform unix]} {
		raise $Priv(win)
	}

#	update

    return
}

proc NSBalloon::Hide {} {

    variable Priv

	wm withdraw $Priv(win)

	return
}

proc NSBalloon::Set {window message} {

	variable Priv

	if {![info exists Priv($window)]} {
		bindtags $window [concat [bindtags $window] BalloonBindTag]
		set Priv(delay,$window) 500
	}
	if {![info exists Priv(delay,$window)]} {
		set Priv(delay,$window) 500
	}

	set Priv($window) $message

	return
}

proc NSBalloon::Set_Canvas {canvas tagOrId message} {

    variable Priv

	foreach id [$canvas find withtag $tagOrId] {
		if {![info exists Priv($canvas,$id)]} {
			$canvas addtag BalloonBindTag withtag $id
		}
		set Priv($canvas,$id) $message
	}
	
	if {![info exists Priv(canvas,$canvas)]} {
		set Priv(canvas,$canvas) 1
		
		$canvas bind BalloonBindTag <Enter> {
		    set NSBalloon::Priv(showing) 0
		    set NSBalloon::Priv(first) 1
		    if {!$NSBalloon::Priv(delay)} {
		    	set NSBalloon::Priv(delay) $NSBalloon::Priv(delay,%W)
		    }
		    set NSBalloon::Priv(id) \
				[after $NSBalloon::Priv(delay) \
				{NSBalloon::Balloon_Canvas %W}]
		}
		
		$canvas bind BalloonBindTag <ButtonPress> {
		    set NSBalloon::Priv(first) 0
		    NSBalloon::Kill %W
		}
		
		$canvas bind BalloonBindTag <Leave> {
		    set NSBalloon::Priv(first) 0
		    NSBalloon::Kill %W
		}
		
		$canvas bind BalloonBindTag <Motion> {
		    if {$NSBalloon::Priv(showing) == 0} {
		        after cancel $NSBalloon::Priv(id)
		        set NSBalloon::Priv(id) \
		        	[after $NSBalloon::Priv(delay) {
		        		NSBalloon::Balloon_Canvas %W
		        	}]
		    }
		}
	}

	if {![info exists Priv(delay,$canvas)]} {
		set Priv(delay,$canvas) 500
	}

	return
}

proc NSBalloon::Delay {window delay} {

	variable Priv

	set Priv(delay,$window) $delay

	return
}

proc NSBalloon::Kill {window} {

    variable Priv

    after cancel $Priv(id)

	if {$Priv(showing)} {
        Hide
		after cancel $Priv(delayId)
		set afterId [after 1000 {
			set NSBalloon::Priv(delay) 0
		}]
		set Priv(delayId) $afterId
	}

    set Priv(showing) 0

    return
}

proc NSBalloon::Balloon {window} {

    variable Priv

	if {![winfo exists $window]} return

	set message $Priv($window)
	if {![string length $message]} return

    if {$Priv(first) == 1} {

        set Priv(first) 2

        set x [expr {[winfo rootx $window] + ([winfo width $window] / 2)}]
        set y [expr {[winfo rooty $window] + [winfo height $window] + 4}]

		Show $message $x $y n

        set Priv(showing) 1

		after cancel $Priv(delayId)
		set Priv(delay) 10
    }

    return
}

proc NSBalloon::Balloon_Canvas {canvas} {

	variable Priv

	if {![winfo exists $canvas]} return

	set id [$canvas find withtag current]
	if {![llength $id]} return

	set message $Priv($canvas,$id)
	if {![string length $message]} return

	if {$Priv(first) == 1} {

		set Priv(first) 2

		scan [$canvas bbox $id] "%s %s %s %s" left top right bottom
		set x [expr {[winfo rootx $canvas] + $left + ($right - $left) / 2}]
		set y [expr {[winfo rooty $canvas] + $bottom + 2}]

		Show $message $x $y n

		set Priv(showing) 1

		after cancel $Priv(delayId)
		set Priv(delay) 10
	}

	return
}
