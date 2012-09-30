# File: canvist.tcl

# Purpose: a 1-dimension list using a canvas

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSCanvist {

	variable Priv

	set Priv(scan,afterId) {}
	set Priv(canvistPrev) -1

# namespace eval NSCanvist
}

# NSCanvist::NSCanvist --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::NSCanvist {oop parent rowHgt wid hgt newRowCmd highlightCmd} {

	global NSCanvist
	variable Priv

	set c $parent.canvist$oop
    canvas $c \
        -scrollregion [list 0 0 $wid 0] -width $wid -height $hgt \
        -relief flat -background white -highlightthickness 0 \
        -yscrollincrement $rowHgt -takefocus 1

	#
	# Do stuff when the canvas is clicked
	#

	bind $c <ButtonPress-1> "NSCanvist::Button1 $oop %x %y 0"
	bind $c <Button1-Motion> "NSCanvist::Motion1 $oop %x %y"
	bind $c <Double-ButtonPress-1> "NSCanvist::Double1 $oop %x %y"
	bind $c <ButtonRelease-1> "NSCanvist::Release1 $oop %x %y"
	bind $c <Button1-Leave> "NSCanvist::Leave1 $oop %x %y"
	bind $c <Button1-Enter> "NSCanvist::CancelRepeat $oop"

	# KeyPress bindings
	bind $c <KeyPress-Home> "$c yview moveto 0 ; break"
	bind $c <KeyPress-End> "$c yview moveto 1 ; break"
	bind $c <KeyPress-Prior> "$c yview scroll -1 pages ; break"
	bind $c <KeyPress-Next> "$c yview scroll 1 pages ; break"
	bind $c <KeyPress-Up> "NSCanvist::UpDown $oop -1 ; break"
	bind $c <KeyPress-Down> "NSCanvist::UpDown $oop +1 ; break"

	#
	# The Control key toggles selected rows.
	#

	bind $c <Control-ButtonPress-1> "NSCanvist::Button1 $oop %x %y 1"
	
	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSCanvist $oop $c

	# Allows client to draw selection depending on focus
	bindtags $c [concat [bindtags $c] NSCanvistBindTag$oop]
	bind NSCanvistBindTag$oop <FocusIn> \
		"NSCanvist::Activate $oop 1"
	bind NSCanvistBindTag$oop <FocusOut> \
		"NSCanvist::Activate $oop 0"

	set Priv(stroke) 0

	set NSCanvist($oop,canvas) $c
	set NSCanvist($oop,rowHgt) $rowHgt
	set NSCanvist($oop,newRowCmd) $newRowCmd
	set NSCanvist($oop,highlightCmd) $highlightCmd
	set NSCanvist($oop,invokeCmd) {}
	set NSCanvist($oop,selectionCmd) {}
	set NSCanvist($oop,count) 0
	set NSCanvist($oop,nextRowTag) 0
	set NSCanvist($oop,rowTags) {}
	set NSCanvist($oop,selection) {}
	set NSCanvist($oop,rowsEnabled) 1
	set NSCanvist($oop,nearest) 0
	set NSCanvist($oop,stroke) 0
	set NSCanvist($oop,trackIgnore) 0
	set NSCanvist($oop,clickCmd) {}

	# Total hack -- PDjam module uses Drag & Drop
	set NSCanvist($oop,dragSpecial) 0

	return
}

# NSCanvist::~NSCanvist --
#
#	Object destructor called by NSObject::Delete.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::~NSCanvist {oop} {

	return
}

# NSCanvist::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Info {oop info args} {

	global NSCanvist

	# Verify the object
	NSObject::CheckObject NSCanvist $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSCanvist($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSCanvist($oop,$info)
			}
		}
	}

	return
}

# NSCanvist::Insert --
#
#	Insert a row at the given index.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Insert {oop index args} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)
	set count $NSCanvist($oop,count)
	set rowHgt $NSCanvist($oop,rowHgt)

	if {$index == "end"} {set index $count}
	if {$index < 0} {set index 0}
	if {$index > $count} {set index $count}

	set y [expr {$rowHgt * $index}]

	#
	# Move following rows down by one.
	#

	if {$index < $count} {
		foreach rowTag [lrange $NSCanvist($oop,rowTags) $index end] {
			$canvas move $rowTag 0 $rowHgt
		}
	}
	
	#
	# The newRowCmd returns a list of all items added that are
	# on the new row. They get tagged with a common "group tag"
	# of the form ":N" where N is some integer.
	#

	set itemIdList [uplevel #0 $NSCanvist($oop,newRowCmd) $oop $y $args]
	set rowTag ":$NSCanvist($oop,nextRowTag)"
	foreach itemId $itemIdList {
		$canvas addtag $rowTag withtag $itemId
	}

	# Insert
	if {$index < $count} {

		# Remember the tag applied to all items on this row
		set NSCanvist($oop,rowTags) \
			[linsert $NSCanvist($oop,rowTags) $index $rowTag]
	
		# This row is not selected
		set NSCanvist($oop,selection) \
			[linsert $NSCanvist($oop,selection) $index 0]

	# Append
	} else {
		lappend NSCanvist($oop,rowTags) $rowTag
		lappend NSCanvist($oop,selection) 0
	}
	
	incr NSCanvist($oop,count)
	incr NSCanvist($oop,nextRowTag)
	Synch $oop

	return
}

# NSCanvist::InsertMany --
#
#	Insert multiple rows at the given index.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::InsertMany {oop index itemList} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)
	set count $NSCanvist($oop,count)
	set rowHgt $NSCanvist($oop,rowHgt)

	set itemListCount [llength $itemList]
	if {!$itemListCount} return
	
	if {$index == "end"} {set index $count}
	if {$index < 0} {set index 0}
	if {$index > $count} {set index $count}

	set y [expr {$rowHgt * $index}]

	#
	# Move following rows down.
	#

	if {$index < $count} {
		set offset [expr {$itemListCount * $rowHgt}]
		foreach rowTag [lrange $NSCanvist($oop,rowTags) $index end] {
			$canvas move $rowTag 0 $offset
		}
	}

	set newRowTag {}
	set newSelected {}
	
	foreach item $itemList {
	
		#
		# The newRowCmd returns a list of all items added that are
		# on the new row. They get tagged with a common "group tag"
		# of the form ":N" where N is some integer.
		#
	
		set itemIdList [uplevel #0 $NSCanvist($oop,newRowCmd) $oop $y $item]
		set rowTag ":$NSCanvist($oop,nextRowTag)"
		foreach itemId $itemIdList {
			$canvas addtag $rowTag withtag $itemId
		}

		# Remember the tag applied to all items on this row
		lappend newRowTag $rowTag

		# This row is not selected
		lappend newSelected 0

		incr NSCanvist($oop,nextRowTag)

		incr y $rowHgt
	}
	
	if {$index < $count} {
		set NSCanvist($oop,rowTags) \
			[eval linsert [list $NSCanvist($oop,rowTags)] $index $newRowTag]
		set NSCanvist($oop,selection) \
			[eval linsert [list $NSCanvist($oop,selection)] $index $newSelected]
	} else {
		eval lappend NSCanvist($oop,rowTags) $newRowTag
		eval lappend NSCanvist($oop,selection) $newSelected
	}
	
	incr NSCanvist($oop,count) $itemListCount
#	incr NSCanvist($oop,nextRowTag) $count

	Synch $oop

	return
}

# NSCanvist::Delete --
#
#	Delete one or more rows from the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Delete {oop index1 index2} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)
	set count $NSCanvist($oop,count)
	set rowHgt $NSCanvist($oop,rowHgt)

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
	set command $NSCanvist($oop,selectionCmd)
	if {[string length $command]} {
		set deselect {}
		for {set row $index1} {$row <= $index2} {incr row} {
			if {[IsRowSelected $oop $row]} {
				set NSCanvist($oop,selection) \
					[lreplace $NSCanvist($oop,selection) $row $row 0]
				lappend deselect $row
			}
		}
		if {[llength $deselect]} {
			uplevel #0 $command $oop [list {} $deselect]
		}
	}

	#
	# Delete all canvas items on each deleted row
	#

	foreach rowTag [lrange $NSCanvist($oop,rowTags) $index1 $index2] {
		$canvas delete $rowTag
	}

	#
	# Move following rows up.
	#

	incr index2
	foreach rowTag [lrange $NSCanvist($oop,rowTags) $index2 end] {
		$canvas move $rowTag 0 -[expr {$rowHgt * $num}]
	}

	# Delete row tags from list of row tags for deleted rows.
	incr index2 -1
	set NSCanvist($oop,rowTags) [lreplace $NSCanvist($oop,rowTags) $index1 $index2]

	# Delete selection info for deleted rows
	set NSCanvist($oop,selection) \
		[lreplace $NSCanvist($oop,selection) $index1 $index2]

	# Debug
	if {$num > $count} {
		NSUtils::ProgError "NSCanvist::Delete: $num > $count"
		set $num $count
	}

	incr NSCanvist($oop,count) -$num

	Synch $oop

	return
}

# NSCanvist::DeleteAll --
#
#	Delete all the rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::DeleteAll {oop} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)

	# Call client's selectionCmd if given
	set command $NSCanvist($oop,selectionCmd)
	if {[string length $command]} {
		set selection [Selection $oop]
		if {[llength $selection]} {
			set NSCanvist($oop,selection) {}
			uplevel #0 $command $oop [list {} $selection]
		}
	}

	# Bye-bye, suckers!
	$canvas delete all

	set NSCanvist($oop,count) 0
	set NSCanvist($oop,rowTags) {}
	set NSCanvist($oop,selection) {}

	Synch $oop

	return
}

# NSCanvist::_GetRowTag --
#
#	Get the tag common to all items on a row containing the given
#	item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::_GetRowTag {oop tagOrId} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)

	# Get list of tags for item
	set tagList [$canvas gettags $tagOrId]

	# Items without enabled tag are considered "disabled"
	if {[lsearch $tagList "enabled"] == -1} {return {}}

	# Search list of tags for grouping tag (eg ":1", ":2" etc)
	set idx [lsearch $tagList ":*"]

	return [lindex $tagList $idx]
}

# NSCanvist::RemoveSelection --
#
#	Remove the selection from all rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::RemoveSelection {oop} {

if 1 {
	UpdateSelection $oop {} all
} else {
	global NSCanvist

	set row 0
	foreach state $NSCanvist($oop,selection) {
		if {[IsRowSelected $oop $row]} {
			DeselectRow $oop $row
		}
		incr row
	}
}
	return
}

# NSCanvist::SelectRow --
#
#	Select the given row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::SelectRow {oop row} {

	global NSCanvist

	# Get the widget command
	set canvas $NSCanvist($oop,canvas)

	set rowTag [lindex $NSCanvist($oop,rowTags) $row]
	set itemIdList [$canvas find withtag $rowTag]

	# Call user's command to highlight this row
	uplevel #0 $NSCanvist($oop,highlightCmd) $oop 1 $itemIdList

	# Mark the row as selected
	set NSCanvist($oop,selection) \
		[lreplace $NSCanvist($oop,selection) $row $row 1]

	return
}

# NSCanvist::DeselectRow --
#
#	Deselect the given row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::DeselectRow {oop row} {

	global NSCanvist

	# Get the widget command
	set canvas $NSCanvist($oop,canvas)

	# Get list of items on this row
	set rowTag [lindex $NSCanvist($oop,rowTags) $row]
	set itemIdList [$canvas find withtag $rowTag]

	# Mark the row as un-selected
	set NSCanvist($oop,selection) \
		[lreplace $NSCanvist($oop,selection) $row $row 0]

	# Call user's command to un-highlight this row
	uplevel #0 $NSCanvist($oop,highlightCmd) $oop 0 $itemIdList

	return
}

# NSCanvist::IsRowSelected --
#
#	Is a given row selected?
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::IsRowSelected {oop row} {

	global NSCanvist

	set count [expr {$NSCanvist($oop,count) - 1}]
	if {($row < 0) || ($row > $count)} {
		error "bad row \"$row\": must be from 0 to $count"
	}
	return [lindex $NSCanvist($oop,selection) $row]
}

# NSCanvist::UpdateSelection --
#
#	Select and deselect some rows.
#	When the selection changes, call client's routine (if any).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::UpdateSelection {oop selected deselected} {

	global NSCanvist

	# "Selected" takes precedence over "deselected"

	set doneRows {}

	if {([llength $selected] == 1) && ($selected == "all")} {
		set selected {}
		set count [Info $oop count]
		for {set row 0} {$row < $count} {incr row} {
			lappend selected $row
		}
	}
	if {([llength $deselected] == 1) && ($deselected == "all")} {
		set deselected [Selection $oop]
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
		set command $NSCanvist($oop,selectionCmd)
		if {[string length $command]} {
			uplevel #0 $command $oop [list $newlySelected $newlyDeselected]
		}
	}

	return
}

# NSCanvist::Selection --
#
#	Return a list of row indexes of all currently selected rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Returns list of indexes or empty list if no rows are
#	selected.

proc NSCanvist::Selection {oop} {

	global NSCanvist

	set selection {}
	set row 0
	foreach state $NSCanvist($oop,selection) {
		if {$state} {
			lappend selection $row
		}
		incr row
	}

	return $selection
}

# NSCanvist::Button1 --
#
#	Handle ButtonPress-1 event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Button1 {oop x y extend} {

	global NSCanvist
	variable Priv

	set c $NSCanvist($oop,canvas)

	# Claim the input focus
	focus $c

	# Get the hit row.
	set row [PointToRow $oop $x $y]

	# List rows to select/deselect
	set select {}
	set deselect {}

	set callClickCmd 0
	
	# No item was hit
	if {$row == -1} {

		# Unselect all rows if not extending selection.
		if {!$extend} {
			set deselect all
		}

		# Prepare for drag
		if {[Info $oop stroke]} {
			itemMark $c $x $y
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
				set callClickCmd 1
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

	if {$callClickCmd} {
		set command [Info $oop clickCmd]
		if {[string length $command]} {
			uplevel #0 $command $oop $row
		}
	}
	
	return
}

# NSCanvist::Release1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Release1 {oop x y} {

	global NSCanvist
	variable Priv

	set canvas $NSCanvist($oop,canvas)

	itemSelect $oop
	set Priv(stroke) 0
	$canvas delete area

	CancelRepeat $oop

	return
}

# NSCanvist::Motion1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Motion1 {oop x y} {

	variable Priv

	set canvas [Info $oop canvas]

	# Don't track while UpdateSelection() is in progress
	if {[Info $oop trackIgnore]} return

	# Don't track if initial click was outside any cell
	if {$Priv(canvistPrev) == -1} return

	# When mouse tracking (but not the initial click) we find
	# the cell nearest to the given location, even if the location
	# is outside any cell, or even the canvas boundary.
	Info $oop nearest 1

	# Get the hit row.
	set row [PointToRow $oop $x $y]

	Info $oop nearest 0

	# No item was hit
	if {($row == -1) || $Priv(stroke)} {

		if {[Info $oop stroke]} {

			# Drag out selection box
			itemStroke $canvas $x $y
		}

	# An item was hit
	} else {

		# Same row as last time
		if {$row == $Priv(canvistPrev)} return

		if {![Info $oop dragSpecial]} {
			Info $oop trackIgnore 1
			UpdateSelection $oop $row all
			Info $oop trackIgnore 0
		}

		set Priv(canvistPrev) $row
	}

	return
}

# NSCanvist::Leave1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Leave1 {oop x y} {

	AutoScan $oop

	return
}

# NSCanvist::Double1 --
#
#	Call client's command when canvas double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Double1 {oop x y} {

	global NSCanvist

	set command $NSCanvist($oop,invokeCmd)
	if {[string length $command]} {
		uplevel #0 $command $oop $x $y
	}

	return
}

# NSCanvist::AutoScan --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::AutoScan {oop} {

	global NSCanvist
	variable Priv

	set canvas $NSCanvist($oop,canvas)
	if {![winfo exists $canvas]} return

	# Don't track while UpdateSelection() is in progress
	if {[Info $oop trackIgnore]} return

	set pointerx [winfo pointerx $canvas]
	set pointery [winfo pointery $canvas]
	if {[winfo containing $pointerx $pointery] == "$canvas"} return

	set x [expr {$pointerx - [winfo rootx $canvas]}]
	set y [expr {$pointery - [winfo rooty $canvas]}]

	set scrollRgn [$canvas cget -scrollregion]
	set scrollWidth [expr {[lindex $scrollRgn 2] - [lindex $scrollRgn 0]}]
	set scrollHeight [expr {[lindex $scrollRgn 3] - [lindex $scrollRgn 1]}]

	if {[winfo width $canvas] < $scrollWidth} {
	    if {$x >= [winfo width $canvas]} {
			$canvas xview scroll 1 units
	    } elseif {$x < 0} {
			$canvas xview scroll -1 units
	    }
	}

	if {[winfo height $canvas] < $scrollHeight} {
		if {$y >= [winfo height $canvas]} {
			$canvas yview scroll 1 units
	    } elseif {$y < 0} {
			$canvas yview scroll -1 units
	    }
	}

	Motion1 $oop $x $y

	set Priv(scan,afterId) [after 50 NSCanvist::AutoScan $oop]

	return
}

# NSCanvist::CancelRepeat --
#
#	Cancel auto-scrolling "after" command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::CancelRepeat {oop} {

	variable Priv

	after cancel $Priv(scan,afterId)
	set Priv(scan,afterId) {}

	return
}

# NSCanvist::Synch --
#
#	Sets the scroll region of the canvas to the row height
#	multiplied by the number of items in the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Synch {oop} {

	global NSCanvist

	set c $NSCanvist($oop,canvas)

	# The canvist height is (num rows) * (row height)
	set rowHgt $NSCanvist($oop,rowHgt)
	set height [expr {$rowHgt * $NSCanvist($oop,count)}]

	# Get the scroll region and change the height
	set scrollRegion [lreplace [$c cget -scrollregion] 3 3 $height]
	$c configure -scrollregion $scrollRegion

	return
}

# NSCanvist::ItemRow --
#
#	Return the row index the given item is on
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::ItemRow {oop tagOrId} {

	global NSCanvist

	set rowTag [_GetRowTag $oop $tagOrId]
	if {$rowTag == {}} {return -1}
	return [lsearch -exact $NSCanvist($oop,rowTags) $rowTag]
}

# NSCanvist::PointToRow --
#
#	Finds the row containing the given point. If the rowsEnabled option
#	is set, returns the row containing the point, or -1 of no row
#	contains the point. If the nearest option is also set, returns the
#	row closest to the given point, even if the point is outside any
#	row.
#
#	If the rowsEnabled option is not set, returns the row for which an
#	enabled canvas item contains the point, otherwise returns -1.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::PointToRow {oop x y} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)

	# Option: Don't check for enabled items, just hit the row
	if {$NSCanvist($oop,rowsEnabled)} {
		set rows [Info $oop count]
		set rowHeight [Info $oop rowHgt]
		set row [expr {int([$canvas canvasy $y] / $rowHeight)}]

		# Option: Find nearest hit row (used for mouse tracking)
		if {[Info $oop nearest]} {
			if {$row < 0} {
				set row 0
			} elseif {$row >= $rows} {
				set row [expr {$rows - 1}]
			}

			# Restrict to visible rows only
			set rowTop [expr {int([$canvas canvasy 0 $rowHeight] / $rowHeight)}]
			set rowBottom [expr {int([$canvas canvasy [winfo height $canvas] $rowHeight] / $rowHeight - 1)}]
			if {$row < $rowTop} {
				set row $rowTop
			} elseif {$row > $rowBottom} {
				set row $rowBottom
			}
		}

		if {$row < $rows && $row >= 0} {
			return $row
		}
		return -1
	}

	set x [$canvas canvasx $x]
	set y [$canvas canvasy $y]

	# Get the item(s) under the point.
	set itemIdList [$canvas find overlapping $x $y [expr {$x + 1}] [expr {$y + 1}]]

	# No item is under that point
	if {![llength $itemIdList]} {return -1}

	# Get the topmost enabled item
	foreach itemId $itemIdList {
		if {[lsearch -exact [$canvas gettags $itemId] enabled] != -1} {
			return [ItemRow $oop $itemId]
		}
	}

	# No enabled item is overlapping the given location
	return -1
}

# NSCanvist::UpDown --
#
#	Handle KeyPress-Up and KeyPress-Down.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::UpDown {oop delta} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)

	set selection [Selection $oop]
	set max [expr {$NSCanvist($oop,count) - 1}]
	if {$max < 0} return

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

# NSCanvist::See --
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

proc NSCanvist::See {oop row} {

	global NSCanvist

	set canvas $NSCanvist($oop,canvas)
	set rowHeight $NSCanvist($oop,rowHgt)
	set scrollRgn [$canvas cget -scrollregion]
	set height [lindex $scrollRgn 3]

	set rowTop [expr {int([$canvas canvasy 0 $rowHeight] / $rowHeight)}]
	set rowBottom [expr {int($rowTop + [winfo height $canvas] / $rowHeight - 1)}]

	if {($row >= $rowTop) && ($row <= $rowBottom)} {

	} elseif {$row == $rowTop - 1} {
		$canvas yview scroll -1 units

	} elseif {$row == $rowBottom + 1} {
		$canvas yview scroll +1 units

	} else {
		set top [expr {($row * $rowHeight - [winfo height $canvas] / 2) \
			/ double($height)}]
		$canvas yview moveto $top
	}

	return
}



# Utility procedures for stroking out a rectangle
# Adopted from Tk "Widget Demo"

proc NSCanvist::itemMark {c x y} {

    variable Priv
    
    set Priv(areaX1) [$c canvasx $x]
    set Priv(areaY1) [$c canvasy $y]
	set Priv(areaX2) $Priv(areaX1)
	set Priv(areaY2) $Priv(areaY1)
    $c delete area
	set Priv(stroke) 1

	return
}

proc NSCanvist::itemStroke {c x y} {

    variable Priv

	if {!$Priv(stroke)} return
    set x [$c canvasx $x]
    set y [$c canvasy $y]
    if {($Priv(areaX1) != $x) && ($Priv(areaY1) != $y)} {
		$c delete area
		$c addtag area withtag [$c create rect $Priv(areaX1) \
			 $Priv(areaY1) $x $y -outline Grey]
		set Priv(areaX2) $x
		set Priv(areaY2) $y
    }

	return
}

proc NSCanvist::itemSelect {oop} {

	global NSCanvist
	variable Priv

	if {!$Priv(stroke)} return

	# Gotta delete it or its included in the list!
	$NSCanvist($oop,canvas) delete area

    if {($Priv(areaX1) == $Priv(areaX2)) || \
		($Priv(areaY1) == $Priv(areaY2))} return

	# Find all items overlapping the selection rectangle
	set list [$NSCanvist($oop,canvas) find overlapping \
		$Priv(areaX1) $Priv(areaY1) \
		$Priv(areaX2) $Priv(areaY2)]

	set doneRows {}
	set select {}
	set deselect {}

	foreach index $list {

		# Some items are not "enabled"
		if {[_GetRowTag $oop $index] == {}} continue

		# Get the row this item is on
		set row [ItemRow $oop $index]

		# Already processed this row
		if {[lsearch -exact $doneRows $row] != -1} continue

		# Select this row
		lappend select $row

		# Remember we did this row
		lappend doneRows $row
	}

	# Update the selection
	UpdateSelection $oop $select $deselect

	return
}

# NSCanvist::Activate --
#
#	Called when the focus enters or leaves the canvas. Calls the
#	client highlight routine for each selected row. This is so
#	the client can highlight differently depending on whether the
#	canvas has the focus or not.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist::Activate {oop activate} {

	foreach row [Selection $oop] {
		SelectRow $oop $row
	}

	return
}

# FindItemByTag --
#
#	Return a list of canvas itemIds from the given list of item ids
#	which are tagged with the given tag.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FindItemByTag {canvas itemIdList tag} {

	set result {}
	foreach itemId $itemIdList {
		set tagList [$canvas gettags $itemId]
		if {[lsearch -exact $tagList $tag] != -1} {
			lappend result $itemId
		}
	}

	return $result
}
