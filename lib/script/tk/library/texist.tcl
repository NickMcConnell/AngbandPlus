# File: texist.tcl

# Purpose: like a NSCanvist, but using a Text widget

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTexist {

	variable Priv
	set Priv(scan,afterId) {}
	set Priv(canvistPrev) -1

# namespace eval NSTexist
}

# NSTexist::NSTexist --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::NSTexist {oop parent font width height} {

	global NSTexist
	variable Priv

	set tbox $parent.texist$oop
    text $tbox \
		-font $font -width $width -height $height -wrap none \
		-borderwidth 0 -selectborderwidth 0 -highlightthickness 0 \
		-cursor {} -state disabled -exportselection no
	bindtags $tbox [list $tbox [winfo toplevel $parent] all]

	#
	# Do stuff when the widget is clicked
	#

	bind $tbox <ButtonPress-1> "NSTexist::Button1 $oop %x %y 0"
	bind $tbox <Button1-Motion> "NSTexist::Motion1 $oop %x %y"
	bind $tbox <Double-ButtonPress-1> "NSTexist::Double1 $oop %x %y"
	bind $tbox <ButtonRelease-1> "NSTexist::Release1 $oop %x %y"
	bind $tbox <Button1-Leave> "NSTexist::Leave1 $oop %x %y"
	bind $tbox <Button1-Enter> "NSTexist::CancelRepeat $oop"

	# KeyPress bindings
	bind $tbox <KeyPress-Home> "$tbox yview moveto 0 ; break"
	bind $tbox <KeyPress-End> "$tbox yview moveto 1 ; break"
	bind $tbox <KeyPress-Prior> "$tbox yview scroll -1 pages ; break"
	bind $tbox <KeyPress-Next> "$tbox yview scroll 1 pages ; break"
	bind $tbox <KeyPress-Up> "NSTexist::UpDown $oop -1 ; break"
	bind $tbox <KeyPress-Down> "NSTexist::UpDown $oop +1 ; break"

	#
	# The Control key adds to the current selection.
	#

	bind $tbox <Control-ButtonPress-1> "NSTexist::Button1 $oop %x %y 1"
	
	#
	# Remember to destroy the object along with the canvas
	#

	bind $tbox <Destroy> "+
		NSObject::Delete NSTexist $oop
	"

	set Priv(stroke) 0

	set NSTexist($oop,text) $tbox
	set NSTexist($oop,invokeCmd) {}
	set NSTexist($oop,selectionCmd) {}
	set NSTexist($oop,count) 0
	set NSTexist($oop,selection) {}
	set NSTexist($oop,stroke) 0
	set NSTexist($oop,nextId) 0

	return
}

# NSTexist::~NSTexist --
#
#	Object destructor called by NSObject::Delete.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::~NSTexist {oop} {
}

# NSTexist::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Info {oop info args} {

	global NSTexist

	# Verify the object
	NSObject::CheckObject NSTexist $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSTexist($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSTexist($oop,$info)
			}
		}
	}

	return
}

# NSTexist::Insert --
#
#	Insert a row at the given index.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Insert {oop index text color} {

	set tbox [Info $oop text]
	set count [Info $oop count]

	if {$index == "end"} {set index $count}
	if {$index < 0} {set index 0}
	if {$index > $count} {set index $count}

	# Get unique Id for this row (a tag)
	set rowId [expr {[Info $oop nextId] + 1}]
	Info $oop nextId $rowId

	# Insert the new row
	$tbox configure -state normal
	set index2 [expr {$index + 1}].0
	$tbox insert $index2 $text\n row$rowId
	$tbox tag configure row$rowId -foreground $color
	$tbox configure -state disabled

	# This row is not selected
	Info $oop selection \
		[linsert [Info $oop selection] $index 0]

	# One more row added
	Info $oop count [expr {[Info $oop count] + 1}]

	return
}

# NSTexist::SetList --
#
#	Set this list with text and colors (for speed).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::SetList {oop textList colorList} {

	set tbox [Info $oop text]

	Delete $oop 0 end

	# Get unique Id for this row (a tag)
	set rowId [expr {[Info $oop nextId] + 1}]

	$tbox configure -state normal
	set count 0
	set index 1
	set selection {}

	# Insert the new rows
	foreach text $textList color $colorList {
		$tbox insert $index.0 $text\n row$rowId
		$tbox tag configure row$rowId -foreground $color
		lappend selection 0
		incr rowId
		incr count
		incr index
	}

	# Delete trailing newline
	$tbox delete "end - 1 chars"

	$tbox configure -state disabled

	Info $oop selection $selection
	Info $oop nextId $rowId
	Info $oop count $count

	return
}

# NSTexist::Delete --
#
#	Delete one or more rows from the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Delete {oop index1 index2} {

	set tbox [Info $oop text]
	set count [Info $oop count]

	# Nothing to delete
	if {$count == 0} return

	if {$index1 >= $count} {set index1 [expr {$count - 1}]}
	if {$index1 < 0} {set index1 0}
	if {$index2 == "end"} {set index2 $count}
	if {$index2 >= $count} {set index2 [expr {$count - 1}]}
	if {$index2 < 0} {set index2 0}

	set num [expr {$index2 - $index1 + 1}]
	if {!$num} return

	# Call client's selectionCmd if given
	set command [Info $oop selectionCmd]
	if {[string length $command]} {
		set deselect {}
		for {set row $index1} {$row <= $index2} {incr row} {
			if {[IsRowSelected $oop $row]} {
				lappend deselect $row
			}
		}
		if {[llength $deselect]} {
			eval $command $oop [list {}] [list $deselect]
		}
	}

	#
	# Delete each row
	#

	$tbox configure -state normal
	set start [expr {$index1 + 1}].0
	set end [expr {$index2 + 2}].0
	$tbox delete $start $end
	$tbox configure -state disabled

	# Delete selection info for deleted rows
	Info $oop selection \
		[lreplace [Info $oop selection] $index1 $index2]

	# Debug
	if {$num > $count} {
		NSUtils::ProgError "NSTexist::Delete: $num > $count"
		set $num $count
	}

	Info $oop count [expr {[Info $oop count] - $num}]

	return
}

# NSTexist::RemoveSelection --
#
#	Remove the selection from all rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::RemoveSelection {oop} {

	set row 0
	foreach state [Info $oop selection] {
		if {[IsRowSelected $oop $row]} {
			DeselectRow $oop $row
		}
		incr row
	}

	return
}

# NSTexist::SelectRow --
#
#	Select the given row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::SelectRow {oop row} {

	set tbox [Info $oop text]

	set index [expr {$row + 1}].0
	$tbox tag add sel $index "$index lineend + 1 chars"

	# Mark the row as selected
	Info $oop selection \
		[lreplace [Info $oop selection] $row $row 1]

	return
}

# NSTexist::DeselectRow --
#
#	Deselect the given row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::DeselectRow {oop row} {

	set tbox [Info $oop text]

	# Mark the row as un-selected
	Info $oop selection \
		[lreplace [Info $oop selection] $row $row 0]

	#
	set index [expr {$row + 1}].0
	$tbox tag remove sel $index "$index lineend + 1 chars"

	return
}

# NSTexist::IsRowSelected --
#
#	Is a given row selected?
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::IsRowSelected {oop row} {

	return [lindex [Info $oop selection] $row]
}

# NSTexist::UpdateSelection --
#
#	Select and deselect some rows.
#	When the selection changes, call client's routine (if any).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::UpdateSelection {oop selected deselected} {

	# "Selected" takes precedence over "deselected"

	set doneRows {}

	# Deselect "all"
	if {([llength $deselected] == 1) && ($deselected == "all")} {
		set deselected {}
		foreach row [Selection $oop] {
			lappend deselected $row
		}
	}

	set newlySelected {}
	foreach row $selected {
		if {[lsearch -exact $doneRows $row] >= 0} continue
		lappend doneRows $row
		if {[IsRowSelected $oop $row]} continue
		lappend newlySelected $row
	}

	set newlyDeselected {}
	foreach row $deselected {
		if {[lsearch -exact $doneRows $row] >= 0} continue
		lappend doneRows $row
		if {![IsRowSelected $oop $row]} continue
		lappend newlyDeselected $row
	}

	if {[llength $newlySelected] || [llength $newlyDeselected]} {

		lsort -integer $newlySelected
		lsort -integer $newlyDeselected

		# Select rows
		foreach row $newlySelected {
			SelectRow $oop $row
		}

		# Deselect rows
		foreach row $newlyDeselected {
			DeselectRow $oop $row
		}

		# Call client's selectionCmd if given
		set command [Info $oop selectionCmd]
		if {[string length $command]} {
			eval $command $oop [list $newlySelected $newlyDeselected]
		}
	}

	return
}

# NSTexist::Selection --
#
#	Return a list of row indexes of all currently selected rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Returns list of indexes or empty list if no rows are
#	selected.

proc NSTexist::Selection {oop} {

	set selection {}
	set row 0
	foreach state [Info $oop selection] {
		if {$state} {
			lappend selection $row
		}
		incr row
	}

	return $selection
}

# NSTexist::Button1 --
#
#	Handle ButtonPress-1 event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Button1 {oop x y extend} {

	variable Priv

	set tbox [Info $oop text]

	# Claim the input focus
	focus $tbox

	# Get the hit row.
	set row [PointToRow $oop $x $y]

	# List rows to select/deselect
	set select {}
	set deselect {}

	# No item was hit
	if {$row == -1} {

		# Unselect all rows if not extending selection.
		if {!$extend} {
			set deselect all
		}

		# Remember no cell was hit
		set Priv(canvistPrev) -1

	# An item was hit
	} else {

		# The row is currently selected
		if {[IsRowSelected $oop $row]} {

			# Control-click toggles selection
			if {$extend} {
				set deselect $row
			} else {
				set deselect all
				set select $row
			}

		# Row was not selected
		} else {

			# Unselect all rows if not extending selection.
			if {!$extend} {
				set deselect all
			}

			# Select the hit row
			set select $row
		}

		# Remember the current row
		set Priv(canvistPrev) $row
	}

	# Update the selection
	UpdateSelection $oop $select $deselect

	return
}

# NSTexist::Release1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Release1 {oop x y} {

	CancelRepeat $oop

	return
}

# NSTexist::Motion1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Motion1 {oop x y} {

	variable Priv

	set tbox [Info $oop text]

	# Don't track if initial click was outside any cell
	if {$Priv(canvistPrev) == -1} return

	# Get the hit row.
	set row [PointToRow $oop $x $y]

	# No item was hit
	if {$row == -1} {

	# An item was hit
	} else {

		# Same row as last time
		if {$row == $Priv(canvistPrev)} return

		UpdateSelection $oop $row all

		set Priv(canvistPrev) $row
	}

	return
}

# NSTexist::Leave1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Leave1 {oop x y} {

	AutoScan $oop

	return
}

# NSTexist::Double1 --
#
#	Call client's command when canvas double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::Double1 {oop x y} {

	set command [Info $oop invokeCmd]
	if {[string length $command]} {
		eval $command $oop $x $y
	}

	return
}

# NSTexist::AutoScan --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::AutoScan {oop} {

	variable Priv

	set tbox [Info $oop text]
	if {![winfo exists $tbox]} return

	set pointerx [winfo pointerx $tbox]
	set pointery [winfo pointery $tbox]
	if {[winfo containing $pointerx $pointery] == "$tbox"} return

	set x [expr {$pointerx - [winfo rootx $tbox]}]
	set y [expr {$pointery - [winfo rooty $tbox]}]

    if {$x >= [winfo width $tbox]} {
		$tbox xview scroll 1 units
    } elseif {$x < 0} {
		$tbox xview scroll -1 units
    }

	if {$y >= [winfo height $tbox]} {
		$tbox yview scroll 1 units
    } elseif {$y < 0} {
		$tbox yview scroll -1 units
    }

	Motion1 $oop $x $y

	set Priv(scan,afterId) [after 50 NSTexist::AutoScan $oop]

	return
}

# NSTexist::CancelRepeat --
#
#	Cancel auto-scrolling "after" command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::CancelRepeat {oop} {

	variable Priv

	after cancel $Priv(scan,afterId)
	set Priv(scan,afterId) {}

	return
}

# NSTexist::PointToRow --
#
#	Finds the row containing the given point.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::PointToRow {oop x y} {

	set tbox [Info $oop text]
	scan [$tbox index @${x},$y] "%d.%d" row ignore
	incr row -1
	if {$row == [Info $oop count]} {
		return -1
	}
	return $row
}

# NSTexist::UpDown --
#
#	Handle KeyPress-Up and KeyPress-Down.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::UpDown {oop delta} {

	set tbox [Info $oop text]
	set selection [Selection $oop]
	set max [expr {[Info $oop count] - 1}]

	if {[llength $selection]} {
		set row [expr {[lindex $selection 0] + $delta}]
		if {$row < 0} {
			set row $max
		} elseif {$row > $max} {
			set row 0
		}
	} else {
		if {$delta > 0} {
			set row 0
		} else {
			set row $max
		}
	}

	UpdateSelection $oop $row $selection
	See $oop $row

	return
}

# NSTexist::See --
#
#	Scroll the given row into view. If it is the row above the currently-
#	visible top row, then scroll up one row. If it is the row below the
#	currently-visible bottom row, then scroll down one row. Otherwise
#	attempt to center the row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTexist::See {oop row} {

	set tbox [Info $oop text]
	$tbox see "$row.0 + 1 lines"

	return
}

