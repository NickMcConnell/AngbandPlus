# File: canvas-listbox.tcl

# Purpose: a 1-dimensional list using a canvas

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSCanvasListbox {

	variable optionTable [list \
		"pixels" "-height" "height" "100" "" \
		"pixels" "-width" "width" "100" "" \
		"pixels" "-rowheight" "rowHgt" "10" "" \
		"command" "-newcommand" "newRowCmd" "" "" \
		"command" "-highlightcommand" "highlightCmd" "" "" \
		"command" "-invokecommand" "invokeCmd" "" "" \
		"command" "-selectioncommand" "selectionCmd" "" "" \
		"boolean" "-rowsenabled" "rowsEnabled" "1" "" \
		"command" "-clickcommand" "clickCmd" "" "" \
		"boolean" "-dragspecial" "dragSpecial" "0" "" \
		"string" "-selectmode" "selectMode" "browse" "" \
	]

# namespace eval NSCanvasListbox
}

# NSCanvasListbox::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::InitModule {} {
}

# NSCanvasListbox::NSCanvasListbox --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::NSCanvasListbox {oop path args} {

	variable optionTable

	set canvas $path
	canvas $canvas \
		-relief flat -background white -highlightthickness 0 -takefocus 1 \
		-scrollregion {0 0 0 0}
	set canvasCmd canvasListbox$canvas
	rename $canvas ::$canvasCmd

	set command $path
	proc ::$command args \
		"eval NSCanvasListbox::Command $oop \$args"
	Info $oop command $command

	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSCanvasListbox $oop $canvas

	bindtags $canvas [concat [bindtags $canvas] Listbox]

	Info $oop canvas $canvas
	Info $oop canvasCmd $canvasCmd
	Info $oop count 0
	Info $oop nextRowTag 0
	Info $oop rowTags {}
	Info $oop selection {}
	Info $oop nearest 0
	Info $oop stroke 0
	Info $oop trackIgnore 0
	Info $oop active 0
	Info $oop selectAnchor 0

	foreach {type arg info default flags} $optionTable {
		Info $oop $info $default
	}

	eval Configure $oop $args

	return
}

# NSCanvasListbox::~NSCanvasListbox --
#
#	Object destructor called by NSObject::Delete.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::~NSCanvasListbox {oop} {

	return
}

# NSCanvasListbox::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::Info {oop info args} {

	global NSCanvasListbox

	# Verify the object
	NSObject::CheckObject NSCanvasListbox $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSCanvasListbox($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSCanvasListbox($oop,$info)
			}
		}
	}

	return
}

# NSCanvasListbox::Command --
#
#	The widget pathname command is a wrapper around this.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::Command {oop command args} {

	set argc [llength $args]
	set numElements [Info $oop count]

	switch -- $command {
		activate {
			set index [GetListboxIndex $oop [lindex $args 0] 0]
			if {$index >= $numElements} {
				set index [expr {$numElements - 1}]
			}
			if {$index < 0} {
				set index 0
			}
			Info $oop active $index
		}
		canvas {
			return [eval [Info $oop canvasCmd] $args]
		}
		cget {
			set option [lindex $args 0]
			switch -- $option {
				-selectmode {
					return [Info $oop selectMode]
				}
				default {
					return [eval [Info $oop canvasCmd] cget $args]
				}
			}
		}
		configure {
			return [eval Configure $oop $args]
		}
		curselection {
			return [Selection $oop]
		}
		delete {
			set first [GetListboxIndex $oop [lindex $args 0] 0]
			if {$first < $numElements} {
				if {$argc == 3} {
					set last [GetListboxIndex $oop [lindex $args 1] 0]
				} else {
					set last $first
				}
				Delete $oop $first $last
			}
		}
		index {
			set string [lindex $args 0]
			return [GetListboxIndex $oop $string 1]
		}
		info {
			return [eval Info $oop $args]
		}
		insert {
			set index [GetListboxIndex $oop [lindex $args 0] 0]
			return [Insert $oop $index [lindex $args 1]]
		}
		nearest {
			set y [lindex $args 0]
			set rowsEnabled [Info $oop rowsEnabled]
			Info $oop rowsEnabled 1
			set nearest [Info $oop nearest]
			Info $oop nearest 1
			set row [PointToRow $oop 0 $y]
			Info $oop rowsEnabled $rowsEnabled
			Info $oop nearest $nearest
			return $row
		}
		oop {
			return $oop
		}
		see {
			set index [GetListboxIndex $oop [lindex $args 0] 0]
			return [See $oop $index]
		}
		selection {
			set option [lindex $args 0]
			set first [GetListboxIndex $oop [lindex $args 1] 0]
			if {$argc == 3} {
				set last [GetListboxIndex $oop [lindex $args 2] 0]
			} else {
				set last $first
			}
			set numElements [Info $oop count]
			switch -- $option {
				anchor {
					if {$first >= $numElements} {
						set first [expr {$numElements - 1}]
					}
					if {$first < 0} {
						set first 0
					}
					Info $oop selectAnchor $first
				}
				clear {
					ListboxSelect $oop $first $last 0
				}
				includes {
					if {($first < 0) || ($first >= $numElements)} {
						return 0
					}
					return [lindex [Info $oop selection] $first]
				}
				set {
					ListboxSelect $oop $first $last 1
				}
			}
		}
		size {
			return [Info $oop count]
		}
		updatesel {
			return [eval UpdateSelection $oop args]
		}
		xview {
			return [eval [Info $oop canvasCmd] xview $args]
		}
		yview {
			return [eval [Info $oop canvasCmd] yview $args]
		}
		default {
			error "unknown command \"$command\""
		}
	}
	
	return
}

# NSCanvasListbox::Configure --
#
#	Change configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::Configure {oop args} {

	variable optionTable

	if {[llength $args] & 1} {
		error "wrong number of arguments: must be \"[Info $oop command] configure ?option value?...\""
	}

	foreach {type arg info default flags} $optionTable {
		foreach flag $flags {
			set doFlag($flag) 0
		}
	}

	foreach error {0 1} {

		# First pass: Set options to new values
		if {!$error} {
			foreach {option value} $args {
				set match 0
				foreach {type arg info default flags} $optionTable {
					if {[string equal $option $arg]} {
						set savedOptions($info) [Info $oop $info]
						if {[string equal $type boolean]} {
							set value [NSUtils::GetBoolean $value]
						}
						Info $oop $info $value
						foreach flag $flags {
							set doFlag($flag) 1
						}
						set match 1
						break
					}
				}
				if {!$match} {
					error "unknown option \"$option\""
				}
			}

		# Second pass: restore options to old values.
		} else {
			foreach name [array names savedOptions] {
				Info $oop $name $savedOptions($name)
			}
		}

		# Success
		break
	}

	WorldChanged $oop

	if {$error} {
		error $errorString
	}

	return
}

# NSCanvasListbox::WorldChanged --
#
#	Called when configuration options change.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::WorldChanged {oop} {

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	$canvasCmd configure -height [Info $oop height] -width [Info $oop width] \
		-yscrollincrement [Info $oop rowHgt]

	return
}

# NSCanvasListbox::GetListboxIndex --
#
#	Parse an index into a listbox and return either its value
#	or an error.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::GetListboxIndex {oop string endIsSize} {

	set numElements [Info $oop count]

	if {[string equal $string active]} {
		return [Info $oop active]
	}
	if {[string equal $string anchor]} {
		return [Info $oop selectAnchor]
	}
	if {[string equal $string end]} {
		if {$endIsSize} {
			return $numElements
		}
		return [expr {$numElements - 1}]
	}
	if {[string match @*,* $string]} {
		scan $string @%d,%d x y
		return [Command $oop nearest $y]
	}
	if {[string is integer -strict $string]} {
		return $string
	}
	error "bad listbox index \"$string\": must be active, anchor, end, @x,y, or a number"

	return
}

# NSCanvasListbox::ListboxSelect --
#
#	Select or deselect one or more elements in a listbox.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::ListboxSelect {oop first last doSelect} {

	set numElements [Info $oop count]

	if {$last < $first} {
		set temp $first
		set first $last
		set last $temp
	}
	if {($last < 0) || ($first >= $numElements)} {
		return
	}	
	if {$first < 0} {
		set first 0
	}
	if {$last >= $numElements} {
		set last [expr {$numElements - 1}]
	}
	set select {}
	set deselect {}
	if {$doSelect} {
		for {set row $first} {$row <= $last} {incr row} {
			lappend select $row
		}
	} else {
		for {set row $first} {$row <= $last} {incr row} {
			lappend deselect $row
		}
	}
	UpdateSelection $oop $select $deselect

	return
}

# NSCanvasListbox::Insert --
#
#	Insert one or more rows at the given index.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::Insert {oop index itemList} {

	global NSCanvasListbox

	set self [Info $oop command]
	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]
	set count [Info $oop count]
	set rowHgt [Info $oop rowHgt]

	set itemListCount [llength $itemList]
	if {!$itemListCount} return
	
	if {$index < 0} {set index 0}
	if {$index > $count} {set index $count}

	set y [expr {$rowHgt * $index}]

	#
	# Move following rows down.
	#

	if {$index < $count} {
		set offset [expr {$itemListCount * $rowHgt}]
		foreach rowTag [lrange [Info $oop rowTags] $index end] {
			$canvasCmd move $rowTag 0 $offset
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
	
		set itemIdList [uplevel #0 [Info $oop newRowCmd] $self $y $item]
		set rowTag :[Info $oop nextRowTag]
		foreach itemId $itemIdList {
			$canvasCmd addtag $rowTag withtag $itemId
		}

		# Remember the tag applied to all items on this row
		lappend newRowTag $rowTag

		# This row is not selected
		lappend newSelected 0

		incr NSCanvasListbox($oop,nextRowTag)

		incr y $rowHgt
	}
	
	if {$index < $count} {
		Info $oop rowTags \
			[eval linsert [list [Info $oop rowTags]] $index $newRowTag]
		Info $oop selection \
			[eval linsert [list [Info $oop selection]] $index $newSelected]
	} else {
		eval lappend NSCanvasListbox($oop,rowTags) $newRowTag
		eval lappend NSCanvasListbox($oop,selection) $newSelected
	}
	
	incr NSCanvasListbox($oop,count) $itemListCount
	incr NSCanvasListbox($oop,nextRowTag) $count

	if {$index <= [Info $oop selectAnchor]} {
		incr NSCanvasListbox($oop,selectAnchor) $itemListCount
	}

	set numElements [Info $oop count]
	if {$index <= [Info $oop active]} {
		incr NSCanvasListbox($oop,active) $itemListCount
		if {([Info $oop active] >= $numElements) && $numElements} {
			Info $oop active [expr {$numElements - 1}]
		}
	}

	Synch $oop

	return
}

# NSCanvasListbox::Delete --
#
#	Delete one or more rows from the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::Delete {oop first last} {

	global NSCanvasListbox

	set self [Info $oop command]
	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]
	set count [Info $oop count]
	set rowHgt [Info $oop rowHgt]

	# Nothing to delete
	if {$count == 0} return

	if {$first >= $count} {set first [expr {$count - 1}]}
	if {$first < 0} {set first 0}
	if {$last eq "end"} {set last $count}
	if {$last >= $count} {set last [expr {$count - 1}]}
	if {$last < 0} {set last 0}

	set num [expr {$last - $first + 1}]
	if {!$num} return

	# Efficiency -- Delete all the entries
	if {$num == $count} {
		DeleteAll $oop
		return
	}

	# Call client's selectionCmd if given
	set command [Info $oop selectionCmd]
	if {[string length $command]} {
		set deselect {}
		for {set row $first} {$row <= $last} {incr row} {
			if {[IsRowSelected $oop $row]} {
				Info $oop selection \
					[lreplace [Info $oop selection] $row $row 0]
				lappend deselect $row
			}
		}
		if {[llength $deselect]} {
			uplevel #0 $command $self [list {} $deselect]
		}
	}

	#
	# Delete all canvas items on each deleted row
	#

	foreach rowTag [lrange [Info $oop rowTags] $first $last] {
		$canvasCmd delete $rowTag
	}

	#
	# Move following rows up.
	#

	incr last
	foreach rowTag [lrange [Info $oop rowTags] $last end] {
		$canvasCmd move $rowTag 0 -[expr {$rowHgt * $num}]
	}

	# Delete row tags from list of row tags for deleted rows.
	incr last -1
	Info $oop rowTags [lreplace [Info $oop rowTags] $first $last]

	# Delete selection info for deleted rows
	Info $oop selection \
		[lreplace [Info $oop selection] $first $last]

	# Debug
	if {$num > $count} {
		NSUtils::ProgError "NSCanvasListbox::Delete: $num > $count"
		set $num $count
	}

	incr NSCanvasListbox($oop,count) -$num

	if {$first <= [Info $oop selectAnchor]} {
		incr NSCanvasListbox($oop,selectAnchor) -$num
		if {[Info $oop selectAnchor] < $first} {
			Info $oop selectAnchor $first
		}
	}
	if {[Info $oop active] > $last} {
		incr NSCanvasListbox($oop,active) -$num
	} elseif {[Info $oop active] >= $first} {
		Info $oop active first
		if {([Info $oop active] >= $numElements) && $numElements} {
			Info $oop active [expr {$numElements - 1}]
		}
	}
	Synch $oop

	return
}

# NSCanvasListbox::DeleteAll --
#
#	Delete all the rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::DeleteAll {oop} {

	set self [Info $oop command]
	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	# Call client's selectionCmd if given
	set command [Info $oop selectionCmd]
	if {[string length $command]} {
		set selection [Selection $oop]
		if {[llength $selection]} {
			Info $oop selection {}
			uplevel #0 $command $self [list {} $selection]
		}
	}

	# Bye-bye, suckers!
	$canvasCmd delete all

	Info $oop count 0
	Info $oop rowTags {}
	Info $oop selection {}

	Info $oop active 0
	Info $oop selectAnchor 0

	Synch $oop

	return
}

# NSCanvasListbox::_GetRowTag --
#
#	Get the tag common to all items on a row containing the given
#	item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::_GetRowTag {oop tagOrId} {

	global NSCanvasListbox

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	# Get list of tags for item
	set tagList [$canvasCmd gettags $tagOrId]

	# Items without enabled tag are considered "disabled"
	if {[lsearch $tagList "enabled"] == -1} {return {}}

	# Search list of tags for grouping tag (eg ":1", ":2" etc)
	set idx [lsearch $tagList ":*"]

	return [lindex $tagList $idx]
}

# NSCanvasListbox::SelectRow --
#
#	Select the given row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::SelectRow {oop row} {

	set self [Info $oop command]
	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	set rowTag [lindex [Info $oop rowTags] $row]
	set itemIdList [$canvasCmd find withtag $rowTag]

	# Call user's command to highlight this row
	uplevel #0 [Info $oop highlightCmd] $self 1 $itemIdList

	# Mark the row as selected
	Info $oop selection \
		[lreplace [Info $oop selection] $row $row 1]

	return
}

# NSCanvasListbox::DeselectRow --
#
#	Deselect the given row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::DeselectRow {oop row} {

	set self [Info $oop command]
	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	# Get list of items on this row
	set rowTag [lindex [Info $oop rowTags] $row]
	set itemIdList [$canvasCmd find withtag $rowTag]

	# Mark the row as un-selected
	Info $oop selection \
		[lreplace [Info $oop selection] $row $row 0]

	# Call user's command to un-highlight this row
	uplevel #0 [Info $oop highlightCmd] $self 0 $itemIdList

	return
}

# NSCanvasListbox::IsRowSelected --
#
#	Is a given row selected?
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::IsRowSelected {oop row} {

	set count [expr {[Info $oop count] - 1}]
	if {($row < 0) || ($row > $count)} {
		error "bad row \"$row\": must be from 0 to $count"
	}
	return [lindex [Info $oop selection] $row]
}

# NSCanvasListbox::UpdateSelection --
#
#	Select and deselect some rows.
#	When the selection changes, call client's routine (if any).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::UpdateSelection {oop selected deselected} {

	set self [Info $oop command]

	# "Selected" takes precedence over "deselected"

	set doneRows {}

	if {([llength $deselected] == 1) && ($deselected eq "all")} {
		set deselected [Selection $oop]
	}

	set newlySelected {}
	foreach row $selected {
		if {[lsearch -integer $doneRows $row] >= 0} continue
		lappend doneRows $row
		if {[IsRowSelected $oop $row]} continue
		lappend newlySelected $row
	}

	set newlyDeselected {}
	foreach row $deselected {
		if {[lsearch -integer $doneRows $row] >= 0} continue
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
			uplevel #0 $command $self [list $newlySelected $newlyDeselected]
		}
	}

	return
}

# NSCanvasListbox::Selection --
#
#	Return a list of row indexes of all currently selected rows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Returns list of indexes or empty list if no rows are
#	selected.

proc NSCanvasListbox::Selection {oop} {

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

# NSCanvasListbox::Synch --
#
#	Sets the scroll region of the canvas.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::Synch {oop} {

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	# The canvist height is (num rows) * (row height)
	set rowHgt [Info $oop rowHgt]
	set height [expr {$rowHgt * [Info $oop count]}]
	if {$height < [winfo height $canvas]} {
		set height [winfo height $canvas]
	}

	# Get the scroll region and change the height
	set scrollRegion [lreplace [$canvasCmd cget -scrollregion] 3 3 $height]
	$canvasCmd configure -scrollregion $scrollRegion

	return
}

# NSCanvasListbox::ItemToRow --
#
#	Return the row index the given item is on
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCanvasListbox::ItemToRow {oop tagOrId} {

	set rowTag [_GetRowTag $oop $tagOrId]
	if {$rowTag == {}} {return -1}
	return [lsearch -exact [Info $oop rowTags] $rowTag]
}

# NSCanvasListbox::PointToRow --
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

proc NSCanvasListbox::PointToRow {oop x y} {

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	# Option: Don't check for enabled items, just hit the row
	if {[Info $oop rowsEnabled]} {
		set rows [Info $oop count]
		set rowHeight [Info $oop rowHgt]
		set row [expr {int([$canvasCmd canvasy $y] / $rowHeight)}]

		# Option: Find nearest hit row (used for mouse tracking)
		if {[Info $oop nearest]} {
			if {$row < 0} {
				set row 0
			} elseif {$row >= $rows} {
				set row [expr {$rows - 1}]
			}

			# Restrict to visible rows only
			set rowTop [expr {int([$canvasCmd canvasy 0 $rowHeight] / $rowHeight)}]
			set rowBottom [expr {int([$canvasCmd canvasy [winfo height $canvas] $rowHeight] / $rowHeight - 1)}]
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

	set x [$canvasCmd canvasx $x]
	set y [$canvasCmd canvasy $y]

	# Get the item(s) under the point.
	set itemIdList [$canvasCmd find overlapping $x $y [expr {$x + 1}] [expr {$y + 1}]]

	# No item is under that point
	if {![llength $itemIdList]} {return -1}

	# Get the topmost enabled item
	foreach itemId $itemIdList {
		if {[lsearch -exact [$canvasCmd gettags $itemId] enabled] != -1} {
			return [ItemToRow $oop $itemId]
		}
	}

	# No enabled item is overlapping the given location
	return -1
}

# NSCanvasListbox::See --
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

proc NSCanvasListbox::See {oop row} {

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]

	set rowHeight [Info $oop rowHgt]
	set scrollRgn [$canvasCmd cget -scrollregion]
	set height [lindex $scrollRgn 3]

	set rowTop [expr {int([$canvasCmd canvasy 0 $rowHeight] / $rowHeight)}]
	set rowBottom [expr {int($rowTop + [winfo height $canvas] / $rowHeight - 1)}]

	if {($row >= $rowTop) && ($row <= $rowBottom)} {

	} elseif {$row == $rowTop - 1} {
		$canvasCmd yview scroll -1 units

	} elseif {$row == $rowBottom + 1} {
		$canvasCmd yview scroll +1 units

	} else {
		set top [expr {($row * $rowHeight - [winfo height $canvas] / 2) \
			/ double($height)}]
		$canvasCmd yview moveto $top
	}

	return
}

# canvaslistbox --
#
#	Call this to create a new NSCanvasListbox.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc canvaslistbox {path args} {

	set oop [eval NSObject::New NSCanvasListbox $path $args]
	return [NSCanvasListbox::Info $oop command]
}



### Testing
proc clb_new {clb y text} {

	set canvas [$clb info canvasCmd]
	set lineHeight [$clb info rowHgt]
	set font {{MS Sans Serif} 8}

	set canvasWidth [$canvas cget -width]

	# Selection rectangle
	lappend itemIdList [$canvas create rectangle 0 $y \
		$canvasWidth [expr {$y + $lineHeight}] -fill "" -outline "" \
		-tags selrect]

	# Text
	lappend itemIdList [$canvas create text 2 [expr {$y + 1}] \
		-text $text -anchor nw -font $font -fill Black \
		-tags hilite]

	return $itemIdList
}
proc clb_highlight {clb state args} {

	set canvas [$clb info canvasCmd]
	set itemIdList $args

	set idRect [FindItemByTag $canvas $itemIdList selrect]
	set idText [FindItemByTag $canvas $itemIdList hilite]

	if {$state} {
		set fill $::SystemHighlight
		$canvas itemconfigure $idRect -fill $fill -outline $fill
		set fill $::SystemHighlightText
		$canvas itemconfigure $idText -fill $fill

	} else {
		$canvas itemconfigure $idRect -fill "" -outline ""
		$canvas itemconfigure $idText -fill Black
	}

	return
} 
proc clb {} {

	NSModule::LoadIfNeeded NSCanvasListbox
	catch {destroy .clb}
	toplevel .clb
	set clb .clb.c
	set font {{MS Sans Serif} 8}
	set rowHgt [font metrics $font -linespace]
	canvaslistbox $clb -height 100 -width 200 -newcommand "clb_new $clb"
	$clb configure -highlightcommand "clb_highlight $clb" -rowheight $rowHgt
	pack $clb -expand yes -fill both

	bindtags $clb Listbox

	$clb insert end Hello
	$clb insert end World!
	$clb insert end "Here is an item"
	$clb insert end "Another item"

	return
}
