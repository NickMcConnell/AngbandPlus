# File: tabs.tcl

# Purpose: MS Windows style tabs

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTabs {

	variable Priv
	
	if {[Platform unix]} {
		set Priv(font) {Helvetica 12}
	}
	if {[Platform windows]} {
		set Priv(font) {{MS Sans Serif} 8}
	}

# namespace eval NSTabs
}

proc NSTabs::NSTabs {oop parent} {

	variable Priv

	Info $oop parent $parent
	Info $oop nextId 0
	Info $oop id {}
	Info $oop totalWidth 0
	Info $oop tabWidth {}
	Info $oop current -1
	Info $oop invokeCmd {}
	Info $oop validateCmd {}
	Info $oop font $Priv(font)
	Info $oop active 0

	InitDisplay $oop

	set canvas [Info $oop canvas]
	bindtags $canvas [concat [bindtags $canvas] TabsBindTag($oop)]
	bind TabsBindTag($oop) <Destroy> "
		NSObject::Delete NSTabs $oop
	"

	return
}

proc NSTabs::Info {oop info args} {

	global NSTabs

	# Verify the object
	NSObject::CheckObject NSTabs $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSTabs($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSTabs($oop,$info)
			}
		}
	}

	return
}

proc NSTabs::InitDisplay {oop} {

	variable Priv

	set parent [Info $oop parent]
	set canvas $parent.tabs$oop
	canvas $canvas \
		-height 24 -width 0 -scrollregion {0 0 1000 21} -highlightthickness 0

	# These two lines form the divider line along the bottom of the tabs
	$canvas create line 0 22 1000 22 -fill $::SystemButtonHighlight \
		-width 1.0 -tags ridge
	$canvas create line 0 23 1000 23 -fill $::System3dLight -width 1.0 \
		-tags ridge

	# This line obscures the bottom half of the divider line for the
	# current tab only
	$canvas create line 0 23 0 23 -fill [$canvas cget -background] \
		-width 1.0 -tags obscure

	# Remember the canvas
	Info $oop canvas $canvas

	return
}

proc NSTabs::Add {oop text} {

	variable Priv

	set xoffset 2

	set canvas [Info $oop canvas]
	set index [Info $oop nextId]
	set tabId tab$index
	set x [expr {[Info $oop totalWidth] + $xoffset}]

	# Find the text width
	set width [expr {[font measure [Info $oop font] $text] + 8 * 2}]

	# Hack -- MS Sans Serif has 1-pixel whitespace to right of last char
	incr width -1

	# Keep a list of tab widths
	set tabWidth [Info $oop tabWidth]
	lappend tabWidth $width
	Info $oop tabWidth $tabWidth

	set bg [$canvas cget -background]

	# Outer line
	$canvas create line $x 22 $x 6 [expr {$x + 2}] 4 \
		[expr {$x + $width - 2}] 4 -fill $::SystemButtonHighlight \
		-width 1.0 -tags $tabId
	$canvas create line [expr {$x + $width - 2}] 5 \
		[expr {$x + $width - 1}] 6 [expr {$x + $width - 1}] 23 \
		-fill $::System3dDarkShadow -width 1.0 \
		-tags $tabId

	# Inner line
	$canvas create line [expr {$x + 1}] 22 [expr {$x + 1}] 6 \
		[expr {$x + 2}] 5 [expr {$x + $width - 2}] 5 \
		-fill $::System3dLight -width 1.0 -tags $tabId
	$canvas create line [expr {$x + $width - 2}] 6 \
		[expr {$x + $width - 2}] 23 -fill $::SystemButtonShadow -width 1.0 \
		-tags $tabId

	# Rectangle
	$canvas create rectangle [expr {$x + 2}] 6 [expr {$x + $width - 2}] 22 \
		-outline "" -fill $bg -tags $tabId

	# Text
	# Hack -- If *text* width is odd number of pixels (tab width is then
	# even because of the hack above), move it left one pixel.
	set d [expr {($width & 1) == 0}]
	$canvas create text [expr {$x + $width / 2 - $d}] 13 -anchor center \
		-font [Info $oop font] -text $text -tags [list $tabId text$tabId]

	$canvas bind $tabId <ButtonPress-1> "
		NSTabs::Invoke $oop $tabId
	"
	$canvas bind $tabId <Control-ButtonPress-1> "
		NSTabs::Dump $oop
	"

	Info $oop totalWidth [expr {[Info $oop totalWidth] + $width}]
	$canvas configure -width [expr {[Info $oop totalWidth] + $xoffset + 3}]
	Info $oop nextId [expr {$index + 1}]

	set id [Info $oop id]
	Info $oop id [lappend id $tabId]

	if {[Info $oop current] != -1} {
		$canvas raise ridge
		$canvas raise [Info $oop current]
	}

	return $tabId
}

proc NSTabs::SetText {oop tabId text} {

	set canvas [Info $oop canvas]

	set index [lsearch -exact [Info $oop id] $tabId]
	if {$index == -1} {
		error "can't find tab with id \"$tabId\""
	}
	set widthOld [lindex [Info $oop tabWidth] $index]

	# Set the text
	$canvas itemconfigure text$tabId -text $text

	# Calculate the new width
	set widthNew [expr {[font measure [Info $oop font] $text] + 8 * 2}]

	# Hack -- MS Sans Serif has 1-pixel whitespace to right of last char
	incr widthNew -1

	# Calculate the difference in old/new width
	set widthDiff [expr {$widthNew - $widthOld}]

	if {!$widthDiff} return

	# Move tabs by difference in tab widths
	foreach tabId2 [lrange [Info $oop id] [expr {$index + 1}] end] {
		$canvas move $tabId2 $widthDiff 0
	}

	# Resize this tab
	IncrTabSize $oop $tabId 0 0 $widthDiff 0

	# Update width of this tab
	Info $oop tabWidth [lreplace [Info $oop tabWidth] $index $index $widthNew]

	# Remember out current width
	Info $oop totalWidth [expr {[Info $oop totalWidth] + $widthDiff}]

	# Hack -- Set canvas width
	set xoffset 2
	$canvas configure -width [expr {[Info $oop totalWidth] + $xoffset + 3}]

	return
}

proc NSTabs::Remove {oop tabId} {

	set canvas [Info $oop canvas]

	set index [lsearch -exact [Info $oop id] $tabId]
	if {$index == -1} {
		error "can't find tab with id \"$tabId\""
	}
	set width [lindex [Info $oop tabWidth] $index]

	$canvas delete $tabId
	
	set id [lreplace [Info $oop id] $index $index]
	Info $oop id $id

	set tabWidth [lreplace [Info $oop tabWidth] $index $index]
	Info $oop tabWidth $tabWidth

	# Move tabs to right left by width of removed tab
	foreach tabId2 [lrange $id $index end] {
		$canvas move $tabId2 -$width 0
	}

	# Remember out current width
	Info $oop totalWidth [expr {[Info $oop totalWidth] - $width}]

	# If the selected tab is removed, select the first tab
	if {$tabId == [Info $oop current]} {
		if {[llength $id]} {
			Bigger $oop [lindex $id 0]
			Info $oop current [lindex $id 0]
		} else {
			Info $oop current -1
		}
	}

	set xoffset 2
	$canvas configure -width [expr {[Info $oop totalWidth] + $xoffset + 3}]

	return
}

proc NSTabs::Invoke {oop tabId} {

	set index [lsearch -exact [Info $oop id] $tabId]
	if {$index == -1} {
		error "can't find tab with id \"$tabId\""
	}

	if {$tabId == [Info $oop current]} return

	set command [Info $oop validateCmd]
	if {[string length $command]} {
		if {[uplevel #0 $command $oop]} {
			return
		}
	}

	set canvas [Info $oop canvas]

	if {[Info $oop active]} {
		Smaller $oop [Info $oop current]
		Bigger $oop $tabId
		Info $oop current $tabId
	}

	set command [Info $oop invokeCmd]
	if {[string length $command]} {
		uplevel #0 $command $oop $tabId
	}

	return
}

proc NSTabs::IncrTabSize {oop tabId left top right bottom} {

	set canvas [Info $oop canvas]

	set index [lsearch -exact [Info $oop id] $tabId]
	if {$index == -1} {
		error "can't find tab with id \"$tabId\""
	}

	set items [$canvas find withtag $tabId]

	set index 0

	set lineId [lindex $items $index]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + $left}]
	set y1 [expr {[lindex $coords 1] + $bottom}]
	set x2 [expr {[lindex $coords 2] + $left}]
	set y2 [expr {[lindex $coords 3] + $top}]
	set x3 [expr {[lindex $coords 4] + $left}]
	set y3 [expr {[lindex $coords 5] + $top}]
	set x4 [expr {[lindex $coords 6] + $right}]
	set y4 [expr {[lindex $coords 7] + $top}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4

	set lineId [lindex $items [incr index]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + $right}]
	set y1 [expr {[lindex $coords 1] + $top}]
	set x2 [expr {[lindex $coords 2] + $right}]
	set y2 [expr {[lindex $coords 3] + $top}]
	set x3 [expr {[lindex $coords 4] + $right}]
	set y3 [expr {[lindex $coords 5] + $bottom}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3

	set lineId [lindex $items [incr index]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + $left}]
	set y1 [expr {[lindex $coords 1] + $top}]
	set x2 [expr {[lindex $coords 2] + $left}]
	set y2 [expr {[lindex $coords 3] + $top}]
	set x3 [expr {[lindex $coords 4] + $left}]
	set y3 [expr {[lindex $coords 5] + $top}]
	set x4 [expr {[lindex $coords 6] + $right}]
	set y4 [expr {[lindex $coords 7] + $top}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4

	set lineId [lindex $items [incr index]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + $right}]
	set y1 [expr {[lindex $coords 1] + $top}]
	set x2 [expr {[lindex $coords 2] + $right}]
	set y2 [expr {[lindex $coords 3] + $bottom}]
	$canvas coords $lineId $x1 $y1 $x2 $y2

	set rectId [lindex $items [incr index]]
	set coords [$canvas coords $rectId]
	set x1 [expr {[lindex $coords 0] + $left}]
	set y1 [expr {[lindex $coords 1] + $top}]
	set x2 [expr {[lindex $coords 2] + $right}]
	set y2 [expr {[lindex $coords 3] + $bottom}]
	$canvas coords $rectId $x1 $y1 $x2 $y2

	set textId [lindex $items [incr index]]
	set width [expr {int($x2 - $x1)}]
	set d [expr {($width & 1) == 0}]
	set coords [$canvas coords $textId]
	set x [expr {$x1 + $width / 2 - $d}]
	set y [expr {[lindex $coords 1] + ($top + $bottom) / 2}]
	$canvas coords $textId $x $y

	return
}

proc NSTabs::Bigger {oop tabId} {

	set canvas [Info $oop canvas]

	set index [lsearch -exact [Info $oop id] $tabId]
	if {$index == -1} {
		error "can't find tab with id \"$tabId\""
	}

	set items [$canvas find withtag $tabId]

	set index2 0

	set lineId [lindex $items $index2]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] - 2}]
	set y1 [lindex $coords 1]
if {$index == 0} {set y1 [expr {$y1 + 1}]}
	set x2 [expr {[lindex $coords 2] - 2}]
	set y2 [expr {[lindex $coords 3] - 2}]
	set x3 [expr {[lindex $coords 4] - 2}]
	set y3 [expr {[lindex $coords 5] - 2}]
	set x4 [expr {[lindex $coords 6] + 2}]
	set y4 [expr {[lindex $coords 7] - 2}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4

	set lineId [lindex $items [incr index2]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + 2}]
	set y1 [expr {[lindex $coords 1] - 2}]
if {$index == 0} {set y1 [expr {$y1 + 1}]}
	set x2 [expr {[lindex $coords 2] + 2}]
	set y2 [expr {[lindex $coords 3] - 2}]
	set x3 [expr {[lindex $coords 4] + 2}]
	set y3 [lindex $coords 5]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3

	set lineId [lindex $items [incr index2]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] - 2}]
	set y1 [lindex $coords 1]
	set x2 [expr {[lindex $coords 2] - 2}]
	set y2 [expr {[lindex $coords 3] - 2}]
	set x3 [expr {[lindex $coords 4] - 2}]
	set y3 [expr {[lindex $coords 5] - 2}]
	set x4 [expr {[lindex $coords 6] + 2}]
	set y4 [expr {[lindex $coords 7] - 2}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4

	set lineId [lindex $items [incr index2]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + 2}]
	set y1 [expr {[lindex $coords 1] - 2}]
	set x2 [expr {[lindex $coords 2] + 2}]
	set y2 [lindex $coords 3]
	$canvas coords $lineId $x1 $y1 $x2 $y2

	set rectId [lindex $items [incr index2]]
	set coords [$canvas coords $rectId]
	set x1 [expr {[lindex $coords 0] - 2}]
	set y1 [expr {[lindex $coords 1] - 2}]
	set x2 [expr {[lindex $coords 2] + 2}]
	set y2 [expr {[lindex $coords 3] + 1}]
	$canvas coords $rectId $x1 $y1 $x2 $y2

	set textId [lindex $items [incr index2]]
	$canvas move $textId 0 -2

	$canvas raise ridge
	$canvas raise $tabId

	# Use expr not incr because values are floats
if {$index} {
	set x1 [expr {$x1 - 2}]
}
	set x2 [expr {$x2 + 2}]
	$canvas coords obscure $x1 23 $x2 23
	$canvas raise obscure

	return
}

proc NSTabs::Smaller {oop tabId} {

	set canvas [Info $oop canvas]

	if {$tabId == -1} return

	set index [lsearch -exact [Info $oop id] $tabId]
	if {$index == -1} {
		error "can't find tab with id \"$tabId\""
	}
	
	set items [$canvas find withtag $tabId]

	set index2 0

	set lineId [lindex $items $index2]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + 2}]
	set y1 [lindex $coords 1]
if {$index == 0} {set y1 [expr {$y1 - 1}]}
	set x2 [expr {[lindex $coords 2] + 2}]
	set y2 [expr {[lindex $coords 3] + 2}]
	set x3 [expr {[lindex $coords 4] + 2}]
	set y3 [expr {[lindex $coords 5] + 2}]
	set x4 [expr {[lindex $coords 6] - 2}]
	set y4 [expr {[lindex $coords 7] + 2}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4

	set lineId [lindex $items [incr index2]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] - 2}]
	set y1 [expr {[lindex $coords 1] + 2}]
if {$index == 0} {set y1 [expr {$y1 - 1}]}
	set x2 [expr {[lindex $coords 2] - 2}]
	set y2 [expr {[lindex $coords 3] + 2}]
	set x3 [expr {[lindex $coords 4] - 2}]
	set y3 [lindex $coords 5]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3

	set lineId [lindex $items [incr index2]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] + 2}]
	set y1 [lindex $coords 1]
	set x2 [expr {[lindex $coords 2] + 2}]
	set y2 [expr {[lindex $coords 3] + 2}]
	set x3 [expr {[lindex $coords 4] + 2}]
	set y3 [expr {[lindex $coords 5] + 2}]
	set x4 [expr {[lindex $coords 6] - 2}]
	set y4 [expr {[lindex $coords 7] + 2}]
	$canvas coords $lineId $x1 $y1 $x2 $y2 $x3 $y3 $x4 $y4

	set lineId [lindex $items [incr index2]]
	set coords [$canvas coords $lineId]
	set x1 [expr {[lindex $coords 0] - 2}]
	set y1 [expr {[lindex $coords 1] + 2}]
	set x2 [expr {[lindex $coords 2] - 2}]
	set y2 [lindex $coords 3]
	$canvas coords $lineId $x1 $y1 $x2 $y2

	set rectId [lindex $items [incr index2]]
	set coords [$canvas coords $rectId]
	set x1 [expr {[lindex $coords 0] + 2}]
	set y1 [expr {[lindex $coords 1] + 2}]
	set x2 [expr {[lindex $coords 2] - 2}]
	set y2 [expr {[lindex $coords 3] - 1}]
	$canvas coords $rectId $x1 $y1 $x2 $y2

	set textId [lindex $items [incr index2]]
	$canvas move $textId 0 2

	return
}

proc NSTabs::Dump {oop} {

	set canvas [Info $oop canvas]

	foreach tabId [Info $oop id] width [Info $oop tabWidth] {
		append result "\t$tabId '[$canvas coords text$tabId]' width=$width\n"
	}

	return
}

proc NSTabs::GetNthId {oop index} {

	set id [Info $oop id]
	
	if {($index < 0) || ($index > [llength $id])} {
		error "bad tab index \"$index\": must be between 0 and [expr {[llength $id] - 1}]"
	}
	return [lindex $id $index]
}

