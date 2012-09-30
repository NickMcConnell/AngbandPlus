# File: canvist2.tcl

# Purpose: a 1- or 2-dimensional list using a canvas

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSCanvist2 {

	variable Priv
	set Priv(scan,afterId) {}
	set Priv(canvistPrev) -1

# namespace eval NSCanvist2
}

# NSCanvist2::NSCanvist2 --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::NSCanvist2 {oop parent columnWidth rowHeight wid hgt} {

	global NSCanvist2
	variable Priv

	set c $parent.canvist$oop
    canvas $c \
        -scrollregion [list 0 0 $wid 0] -width $wid -height $hgt \
        -relief flat -background white -highlightthickness 0 \
        -xscrollincrement $columnWidth -yscrollincrement $rowHeight \
		-takefocus 1

	# Do stuff when the canvas is clicked
	bind $c <ButtonPress-1> "NSCanvist2::Button1 $oop %x %y 0"
	bind $c <Button1-Motion> "NSCanvist2::Motion1 $oop %x %y"
	bind $c <Double-ButtonPress-1> "NSCanvist2::Double1 $oop %x %y"
	bind $c <ButtonRelease-1> "NSCanvist2::Release1 $oop %x %y"
	bind $c <Button1-Leave> "NSCanvist2::Leave1 $oop %x %y"
	bind $c <Button1-Enter> "NSCanvist2::CancelRepeat $oop"

	# KeyPress bindings
	bind $c <KeyPress-Home> "$c yview moveto 0 ; break"
	bind $c <KeyPress-End> "$c yview moveto 1 ; break"
	bind $c <KeyPress-Prior> "$c yview scroll -1 pages ; break"
	bind $c <KeyPress-Next> "$c yview scroll 1 pages ; break"
	bind $c <KeyPress-Up> "NSCanvist2::UpDown $oop -1 ; break"
	bind $c <KeyPress-Down> "NSCanvist2::UpDown $oop +1 ; break"
	bind $c <KeyPress-Left> "NSCanvist2::LeftRight $oop -1 ; break"
	bind $c <KeyPress-Right> "NSCanvist2::LeftRight $oop +1 ; break"

	# The Control key toggles the selected state of the hit item.
	bind $c <Control-ButtonPress-1> "NSCanvist2::Button1 $oop %x %y 1"

	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSCanvist2 $oop $c

	# Remember to destroy the object along with the canvas
	bindtags $c [concat [bindtags $c] NSCanvist2BindTag$oop]

	# Allows client to draw selection depending on focus
	bind NSCanvist2BindTag$oop <FocusIn> \
		"NSCanvist2::Activate $oop 1"
	bind NSCanvist2BindTag$oop <FocusOut> \
		"NSCanvist2::Activate $oop 0"

	set NSCanvist2($oop,canvas) $c
	set NSCanvist2($oop,columns) 0
	set NSCanvist2($oop,rows) 0
	set NSCanvist2($oop,count) 0
	set NSCanvist2($oop,columnWidth) $columnWidth
	set NSCanvist2($oop,rowHeight) $rowHeight
	set NSCanvist2($oop,newItemCmd) {}
	set NSCanvist2($oop,highlightCmd) {}
	set NSCanvist2($oop,invokeCmd) {}
	set NSCanvist2($oop,selectionCmd) {}
	set NSCanvist2($oop,nextCellTag) 0
	set NSCanvist2($oop,cellTags) {}
	set NSCanvist2($oop,selection) {}
	set NSCanvist2($oop,cellsEnabled) 1
	set NSCanvist2($oop,nearest) 0
	set NSCanvist2($oop,trackIgnore) 0
	set NSCanvist2($oop,clickCmd) {}

	# Total hack -- PDjam module uses Drag & Drop
	set NSCanvist2($oop,dragSpecial) 0

	return
}

# NSCanvist2::~NSCanvist2 --
#
#	Object destructor called by NSObject::Delete.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::~NSCanvist2 {oop} {
}

# NSCanvist2::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Info {oop info args} {

	global NSCanvist2

	# Verify the object
	NSObject::CheckObject NSCanvist2 $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSCanvist2($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSCanvist2($oop,$info)
			}
		}
	}

	return
}

# NSCanvist2::Append --
#
#	We don't allow inserting single cells. All you can do is
#	append a new cell to the end of all existing cells.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Append {oop args} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)
	set index $NSCanvist2($oop,count)
	set columns $NSCanvist2($oop,columns)
	set columnWidth $NSCanvist2($oop,columnWidth)
	set rowHeight $NSCanvist2($oop,rowHeight)

	set x [expr {($index % $columns) * $columnWidth}]
	set y [expr {($index / $columns) * $rowHeight}]

	#
	# The newItemCmd returns a list of all items added that are
	# in the new cell. They get tagged with a common "cell tag"
	# of the form ":N" where N is some integer.
	#

	set itemIdList [uplevel #0 $NSCanvist2($oop,newItemCmd) $oop $x $y $args]
	set cellTag ":$NSCanvist2($oop,nextCellTag)"
	foreach itemId $itemIdList {
		$canvas addtag $cellTag withtag $itemId
	}

	# Remember the tag applied to all items in this cell
	lappend NSCanvist2($oop,cellTags) $cellTag

	# This cell is not selected
	lappend NSCanvist2($oop,selection) 0

	incr index
	if {$index % $columns} {
		set NSCanvist2($oop,rows) [expr {$index / $columns + 1}]
	} else {
		set NSCanvist2($oop,rows) [expr {$index / $columns}]
	}

	incr NSCanvist2($oop,count)
	incr NSCanvist2($oop,nextCellTag)

	Synch $oop

	return
}

# NSCanvist2::AppendMany --
#
#	We don't allow inserting single cells. All you can do is
#	append cells to the end of all existing cells.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::AppendMany {oop itemList} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)
	set index $NSCanvist2($oop,count)
	set columns $NSCanvist2($oop,columns)
	set columnWidth $NSCanvist2($oop,columnWidth)
	set rowHeight $NSCanvist2($oop,rowHeight)

	set itemListCount [llength $itemList]
	if {!$itemListCount} return

	foreach item $itemList {

		set x [expr {($index % $columns) * $columnWidth}]
		set y [expr {($index / $columns) * $rowHeight}]

		#
		# The newItemCmd returns a list of all items added that are
		# in the new cell. They get tagged with a common "cell tag"
		# of the form ":N" where N is some integer.
		#
	
		set itemIdList [uplevel #0 $NSCanvist2($oop,newItemCmd) $oop $x $y $item]
		set cellTag ":$NSCanvist2($oop,nextCellTag)"
		foreach itemId $itemIdList {
			$canvas addtag $cellTag withtag $itemId
		}
		incr NSCanvist2($oop,nextCellTag)
	
		# Remember the tag applied to all items in this cell
		lappend NSCanvist2($oop,cellTags) $cellTag
	
		# This cell is not selected
		lappend NSCanvist2($oop,selection) 0

		incr index
	}

	if {$index % $columns} {
		set NSCanvist2($oop,rows) [expr {$index / $columns + 1}]
	} else {
		set NSCanvist2($oop,rows) [expr {$index / $columns}]
	}

	incr NSCanvist2($oop,count) $itemListCount

	Synch $oop

	return
}

# NSCanvist2::DeleteAll --
#
#	We don't allow deleting single cells. All you can do is
#	delete all existing cells.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::DeleteAll {oop} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)

	# Call client's selectionCmd if given
	set command $NSCanvist2($oop,selectionCmd)
	if {[string length $command]} {
		set selection [Selection $oop]
		if {[llength $selection]} {
			eval $command $oop {{} $selection}
		}
	}

	# Bye-bye, suckers!
	$canvas delete all

	set NSCanvist2($oop,count) 0
	set NSCanvist2($oop,rows) 0
	set NSCanvist2($oop,cellTags) {}
	set NSCanvist2($oop,selection) {}

	Synch $oop

	return
}

# NSCanvist2::GetCellTagForItem --
#
#	Get the tag common to all items in the cell containing the given
#	canvas item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::GetCellTagForItem {oop tagOrId} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)

	# Get list of tags for item
	set tagList [$canvas gettags $tagOrId]

	# Search list of tags for grouping tag (eg ":1", ":2" etc)
	set idx [lsearch $tagList ":*"]

	return [lindex $tagList $idx]
}

# NSCanvist2::SelectCell --
#
#	Select the given cell, without affecting the selection state of
#	any other cells. You don't normally call this command directly.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::SelectCell {oop index} {

	global NSCanvist2

	# Get canvas
	set canvas $NSCanvist2($oop,canvas)

	set cellTag [lindex $NSCanvist2($oop,cellTags) $index]
	set itemIdList [$canvas find withtag $cellTag]

	# Call user's command to highlight this cell
	uplevel #0 $NSCanvist2($oop,highlightCmd) $oop 1 $itemIdList

	# Mark the cell as selected
	set NSCanvist2($oop,selection) \
		[lreplace $NSCanvist2($oop,selection) $index $index 1]

	return
}

# NSCanvist2::DeselectCell --
#
#	Deselect the given cell, without affecting the selection state of
#	any other cells. You don't normally call this command directly.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::DeselectCell {oop index} {

	global NSCanvist2

	# Get the canvas
	set canvas $NSCanvist2($oop,canvas)

	# Get list of items on this cell
	set cellTag [lindex $NSCanvist2($oop,cellTags) $index]
	set itemIdList [$canvas find withtag $cellTag]

	# Mark the row as un-selected
	set NSCanvist2($oop,selection) \
		[lreplace $NSCanvist2($oop,selection) $index $index 0]

	# Call user's command to un-highlight this row
	uplevel #0 $NSCanvist2($oop,highlightCmd) $oop 0 $itemIdList

	return
}

# NSCanvist2::IsCellSelected --
#
#	Is a given cell selected?
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::IsCellSelected {oop index} {

	global NSCanvist2

	return [lindex $NSCanvist2($oop,selection) $index]
}

# NSCanvist2::UpdateSelection --
#
#	Select and deselect some cells.
#	When the selection changes, call client's "selectionCmd" (if any).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::UpdateSelection {oop selected deselected} {

	global NSCanvist2

	# "Selected" takes precedence over "deselected"

	set doneCells {}

	if {([llength $deselected] == 1) && ($deselected == "all")} {
		set deselected {}
		foreach cell [Selection $oop] {
			lappend deselected $cell
		}
	}

	set newlySelected {}
	foreach index $selected {
		if {[lsearch -exact $doneCells $index] >= 0} continue
		lappend doneCells $index
		if {[IsCellSelected $oop $index]} continue
		lappend newlySelected $index
	}

	set newlyDeselected {}
	foreach index $deselected {
		if {[lsearch -exact $doneCells $index] >= 0} continue
		lappend doneCells $index
		if {![IsCellSelected $oop $index]} continue
		lappend newlyDeselected $index
	}

	if {[llength $newlySelected] || [llength $newlyDeselected]} {

		lsort -integer $newlySelected
		lsort -integer $newlyDeselected

		# Select cells
		foreach index $newlySelected {
			SelectCell $oop $index
		}

		# Deselect cells
		foreach index $newlyDeselected {
			DeselectCell $oop $index
		}

		# Call client's selectionCmd if given
		set command $NSCanvist2($oop,selectionCmd)
		if {[string length $command]} {
			eval $command $oop {$newlySelected $newlyDeselected}
		}
	}

	return
}

# NSCanvist2::Selection --
#
#	Return a list of indexes of all currently selected cells.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Returns list of indexes or empty list if no cells are
#	selected.

proc NSCanvist2::Selection {oop} {

	global NSCanvist2

	set selection {}
	set index 0
	foreach state $NSCanvist2($oop,selection) {
		if {$state} {
			lappend selection $index
		}
		incr index
	}

	return $selection
}

# NSCanvist2::Button1 --
#
#	Handle ButtonPress-1 event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Button1 {oop x y extend} {

	global NSCanvist2
	variable Priv

	set c $NSCanvist2($oop,canvas)

	# Claim the input focus
	focus $c

	# Get the hit cell.
	set index [PointToCell $oop $x $y]

	# List cells to select/deselect
	set select {}
	set deselect {}

	set callClickCmd 0

	# No item was hit
	if {$index == -1} {

		# Unselect all cells if not extending selection.
		if {!$extend} {
			set deselect all
		}

		# Remember no cell was hit
		set Priv(canvistPrev) -1

	# An item was hit
	} else {

		# The row is currently selected
		if {[IsCellSelected $oop $index]} {

			# Control-click toggles selection
			if {$extend} {
				set deselect $index
			} else {
				set deselect all
				set select $index
				set callClickCmd 1
			}

		# Cell was not selected
		} else {

			# Unselect all cells if not extending selection.
			if {!$extend} {
				set deselect all
			}

			# Select the hit cell
			set select $index
		}

		# Remember the current cell
		set Priv(canvistPrev) $index
	}

	# Update the selection
	Info $oop trackIgnore 1
	UpdateSelection $oop $select $deselect
	Info $oop trackIgnore 0

	if {$callClickCmd} {
		set command [Info $oop clickCmd]
		if {[string length $command]} {
			uplevel #0 $command $oop $index
		}
	}

	return
}

# NSCanvist2::Release1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Release1 {oop x y} {

	global NSCanvist2
	variable Priv

	set Priv(canvistPrev) -1
	CancelRepeat $oop

	return
}

# NSCanvist2::Motion1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Motion1 {oop x y} {

	global NSCanvist2
	variable Priv

	set canvas $NSCanvist2($oop,canvas)

	# Don't track while UpdateSelection() is in progress
	if {[Info $oop trackIgnore]} return

	# Don't track if initial click was outside any cell
	if {$Priv(canvistPrev) == -1} return

	# When mouse tracking (but not the initial click) we find
	# the cell nearest to the given location, even if the location
	# is outside any cell, or even the canvas boundary.
	Info $oop nearest 1

	# Get the hit cell.
	set index [PointToCell $oop $x $y]

	Info $oop nearest 0

	# No item was hit
	if {$index == -1} {

	# An item was hit
	} else {

		# Same cell as last time
		if {$index == $Priv(canvistPrev)} return

		Info $oop trackIgnore 1
		if {!$NSCanvist2($oop,dragSpecial)} {
			UpdateSelection $oop $index all
		}
		Info $oop trackIgnore 0

		set Priv(canvistPrev) $index
	}

	return
}

# NSCanvist2::Leave1 --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Leave1 {oop x y} {

	AutoScan $oop

	return
}

# NSCanvist2::Double1 --
#
#	Call client's command when canvas double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Double1 {oop x y} {

	global NSCanvist2

	set command $NSCanvist2($oop,invokeCmd)
	if {[string length $command]} {
		eval $command $oop $x $y
	}

	return
}

# NSCanvist2::AutoScan --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::AutoScan {oop} {

	global NSCanvist2
	variable Priv

	set canvas $NSCanvist2($oop,canvas)
	if {![winfo exists $canvas]} return

	# Don't track while UpdateSelection() is in progress
	if {[Info $oop trackIgnore]} return

	# Get the pointer location
	set pointerx [winfo pointerx $canvas]
	set pointery [winfo pointery $canvas]

	# Don't scan when the pointer is inside the canvas
	if {[winfo containing $pointerx $pointery] == "$canvas"} return

	# Root coords -> window coords
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

	set Priv(scan,afterId) [after 50 NSCanvist2::AutoScan $oop]

	return
}

# NSCanvist2::CancelRepeat --
#
#	Cancel auto-scrolling "after" command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::CancelRepeat {oop} {

	variable Priv

	after cancel $Priv(scan,afterId)
	set Priv(scan,afterId) {}

	return
}

# NSCanvist2::Synch --
#
#	Sets the scroll region of the canvas according to the number of
#	cells and columns.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::Synch {oop} {

	global NSCanvist2

	set c $NSCanvist2($oop,canvas)

	if {$NSCanvist2($oop,rows) == 1} {

		# The canvist width is (count) * (col width)
		set columnWidth $NSCanvist2($oop,columnWidth)
		set width [expr {$columnWidth * $NSCanvist2($oop,count)}]

		# The canvist height is (1) * (row height)
		set height $NSCanvist2($oop,rowHeight)
		
	} else {

		# The canvist width is (num cols) * (col width)
		set columnWidth $NSCanvist2($oop,columnWidth)
		set width [expr {$columnWidth * $NSCanvist2($oop,columns)}]
	
		# The canvist height is (num rows) * (row height)
		set rowHeight $NSCanvist2($oop,rowHeight)
		set height [expr {$rowHeight * $NSCanvist2($oop,rows)}]
	}

	# Get the scroll region and change the width and height
	set scrollRegion [$c cget -scrollregion]
	set scrollRegion [lreplace $scrollRegion 2 2 $width]
	set scrollRegion [lreplace $scrollRegion 3 3 $height]
	$c configure -scrollregion $scrollRegion

	return
}

# NSCanvist2::ItemToCell --
#
#	Return the cell the given canvas item is in.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::ItemToCell {oop tagOrId} {

	global NSCanvist2

	set cellTag [GetCellTagForItem $oop $tagOrId]
	return [lsearch -exact $NSCanvist2($oop,cellTags) $cellTag]
}

# NSCanvist2::PointToCell --
#
#	Finds the cell containing the given point. If the cellsEnabled option
#	is set, returns the cell containing the point, or -1 of no cell
#	contains the point. If the nearest option is also set, returns the
#	cell closest to the given point, even if the point is outside any
#	cell.
#
#	If the cellsEnabled option is not set, returns the cell for which an
#	enabled canvas item contains the point, otherwise returns -1.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::PointToCell {oop x y} {

	global NSCanvist2

	# Get the canvas
	set canvas $NSCanvist2($oop,canvas)

	# Convert window- to canvas-coordinates
	set x [$canvas canvasx $x]
	set y [$canvas canvasy $y]

	# Option: Don't check for enabled items, just hit the cell
	if {[Info $oop cellsEnabled]} {
		set count [Info $oop count]
		set columns [Info $oop columns]
		set columnWidth [Info $oop columnWidth]
		set column [expr {int($x / $columnWidth)}]
		set rows [Info $oop rows]
		set rowHeight [Info $oop rowHeight]
		set row [expr {int($y / $rowHeight)}]

		# Option: Find nearest hit cell (used for mouse tracking)
		if {[Info $oop nearest]} {
			if {$column < 0} {
				set column 0
			} elseif {$column >= $columns} {
				set column [expr {$columns - 1}]
			}
			if {$row < 0} {
				set row 0
			} elseif {$row >= $rows} {
				set row [expr {$rows - 1}]
			}
			if {$rows == 1} {
				if {$column >= $count} {
					set column [expr {$count - 1}]
				}
			}

			# Restrict to visible cells only
			set columnLeft [expr {int([$canvas canvasx 0 $columnWidth] / $columnWidth)}]
			set columnRight [expr {int($columnLeft + [winfo width $canvas] / $columnWidth - 1)}]
			set rowBottom [expr {int([$canvas canvasx [winfo width $canvas] $columnWidth] / $columnWidth - 1)}]
			if {$column < $columnLeft} {
				set column $columnLeft
			} elseif {$column > $columnRight} {
				set column $columnRight
			}

			set rowTop [expr {int([$canvas canvasy 0 $rowHeight] / $rowHeight)}]
			set rowBottom [expr {int([$canvas canvasy [winfo height $canvas] $rowHeight] / $rowHeight - 1)}]
			if {$row < $rowTop} {
				set row $rowTop
			} elseif {$row > $rowBottom} {
				set row $rowBottom
			}
		}
	
		if {$column < $columns && $column >= 0} {
			if {$row < $rows && $row >= 0} {
				set index [expr {$column + $row * $columns}]
				if {$index < $count} {
					return $index
				} elseif {[Info $oop nearest]} {
					return [expr {$column + ($row - 1) * $columns}]
				}
			}
		}
		return -1
	}

	# Get the item(s) under the point.
	set itemIdList \
		[$canvas find overlapping $x $y [expr {$x + 1}] [expr {$y + 1}]]

	# No item is under that point
	if {![llength $itemIdList]} {return -1}

	# Get the topmost enabled item
	foreach itemId $itemIdList {
		if {[lsearch -exact [$canvas gettags $itemId] enabled] != -1} {
			return [ItemToCell $oop $itemId]
		}
	}

	# No enabled item is overlapping the given location
	return -1
}

# NSCanvist2::LeftRight --
#
#	Handle KeyPress-Left and KeyPress-Right.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::LeftRight {oop delta} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)
	set count $NSCanvist2($oop,count)
	set columns $NSCanvist2($oop,columns)

	set selection [Selection $oop]
	set max [expr {$columns - 1}]
	if {$max < 0} return
	
	if {[llength $selection]} {
		set index [lindex $selection 0]
		set column [expr {$index % $columns + $delta}]
		if {$column < 0} {
			set column $max
		} elseif {$column > $max} {
			set column 0
		}
	} else {
		set index 0
		if {$delta > 0} {
			set column 0
		} else {
			set column $max
		}
	}

	set index [expr {$column + ($index / $columns) * $columns}]
	if {$index >= $count} {
		if {$delta > 0} {
			set index [expr {($index / $columns) * $columns}]
		} else {
			set index [expr ($count - 1) % $columns + \
				($index / $columns) * $columns]
		}
	}
	UpdateSelection $oop $index $selection
	See $oop $index

	return
}

# NSCanvist2::UpDown --
#
#	Handle KeyPress-Up and KeyPress-Down.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::UpDown {oop delta} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)
	set count $NSCanvist2($oop,count)
	set columns $NSCanvist2($oop,columns)

	set selection [Selection $oop]
	set max [expr {$NSCanvist2($oop,rows) - 1}]
	if {$max < 0} return
	
	if {[llength $selection]} {
		set index [lindex $selection 0]
		set row [expr {$index / $columns + $delta}]
		if {$row < 0} {
			set row $max
		} elseif {$row > $max} {
			set row 0
		}
	} else {
		set index 0
		if {$delta > 0} {
			set row 0
		} else {
			set row $max
		}
	}

	set index [expr {$index % $columns + $row * $columns}]
	if {$index >= $count} {
		if {$delta > 0} {
			set index [expr {$index % $columns}]
		} else {
			set index [expr {$index % $columns + ($max - 1) * $columns}]
		}
	}
	UpdateSelection $oop $index $selection
	See $oop $index

	return
}

# NSCanvist2::See --
#
#	Scroll the given cell into view. If it is in the row above the currently-
#	visible top row, then scroll up one row. If it is the row below the
#	currently-visible bottom row, then scroll down one row. Otherwise
#	attempt to center the row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvist2::See {oop index} {

	global NSCanvist2

	set canvas $NSCanvist2($oop,canvas)
	set columns $NSCanvist2($oop,columns)
	set rows $NSCanvist2($oop,rows)
	set columnWidth $NSCanvist2($oop,columnWidth)
	set rowHeight $NSCanvist2($oop,rowHeight)
	set width [expr {$columns * $columnWidth}]
	set height [expr {$rows * $rowHeight}]

	set column [expr {$index % $columns}]
	set row [expr {$index / $columns}]

	# Scroll horizontally
	set colLeft \
		[expr {int([$canvas canvasx 0 $columnWidth] / $columnWidth)}]
	set colRight \
		[expr {int($colLeft + [winfo width $canvas] / $columnWidth - 1)}]

	if {($column >= $colLeft) && ($column <= $colRight)} {

	} elseif {$column == $colLeft - 1} {
		$canvas xview scroll -1 units

	} elseif {$column == $colRight + 1} {
		$canvas xview scroll +1 units

	} else {
		set left [expr {($column * $columnWidth - [winfo width $canvas] / 2) \
			/ double($width)}]
		$canvas xview moveto $left
	}

	# Scroll vertically
	set rowTop \
		[expr {int([$canvas canvasy 0 $rowHeight] / $rowHeight)}]
	set rowBottom \
		[expr {int($rowTop + [winfo height $canvas] / $rowHeight - 1)}]

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

# NSCanvist2::Activate --
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

proc NSCanvist2::Activate {oop activate} {

	foreach index [Selection $oop] {
		SelectCell $oop $index
	}

	return
}


if 0 {

proc TestCanvistItemCmd {canvistId x y number} {

	global NSCanvist2

	set canvas $NSCanvist2($canvistId,canvas)
	set columnWidth $NSCanvist2($canvistId,columnWidth)
	set rowHeight $NSCanvist2($canvistId,rowHeight)

	lappend itemIdList [$canvas create rectangle [expr {$x + 2}] [expr {$y + 2}] \
		[expr {$x + $columnWidth - 2}] [expr {$y + $rowHeight - 2}] -fill {} \
		-outline {} -tags {enabled sel}]

	lappend itemIdList [$canvas create text [expr {$x + $columnWidth / 2}] \
		[expr {$y + $rowHeight / 2}] -text $number -anchor center \
		-font {Times 18 bold} -tags enabled]

	return $itemIdList
}

proc TestCanvistHighlightCmd {canvistId state args} {

	global NSCanvist2

	set canvas $NSCanvist2($canvistId,canvas)
	set idRect [lindex $args 0]
	set idText [lindex $args 1]

	if {$state} {

		$canvas itemconfigure $idRect -fill $::SystemHighlight -outline $::SystemHighlight
		$canvas itemconfigure $idText -fill $::SystemHighlightText

	} else {

		$canvas itemconfigure $idRect -fill {} -outline {}
		$canvas itemconfigure $idText -fill Black
	}

	return
}

proc TestCanvist2 {} {

	global NSCanvist2

	set win .testcanvist2
	toplevel $win

	set canvistId [NSObject::New NSCanvist2 $win 40 40 160 160]
	set NSCanvist2($canvistId,columns) 6
	set NSCanvist2($canvistId,newItemCmd) TestCanvistItemCmd
	set NSCanvist2($canvistId,highlightCmd) TestCanvistHighlightCmd

	set canvas $NSCanvist2($canvistId,canvas)
	$canvas configure -xscrollcommand "$win.xscroll set"
	$canvas configure -yscrollcommand "$win.yscroll set"

    scrollbar $win.xscroll \
        -borderwidth 0 -command "$canvas xview" \
        -orient horizontal
    scrollbar $win.yscroll \
        -borderwidth 0 -command "$canvas yview" \
        -orient vertical

    grid rowconfig $win 0 -weight 1 -minsize 0
    grid columnconfig $win 0 -weight 1 -minsize 0

    grid $canvas \
        -row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
    grid $win.yscroll \
        -row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
    grid $win.xscroll \
        -row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	
	for {set i 0} {$i < 29} {incr i} {
		NSCanvist2::Append $canvistId $i
	}

	return
}

TestCanvist2

# if 0
}
