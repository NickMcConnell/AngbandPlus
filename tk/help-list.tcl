# File: help-list.tcl

# Purpose: the Help Window contents list

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSHelpList {

	variable Book
}

# NSHelpList::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::InitModule {} {

	InitImageIfNeeded Image_BookClosed ms-book-closed.gif
	InitImageIfNeeded Image_BookOpen ms-book-open.gif
	InitImageIfNeeded Image_HelpPage ms-help-page.gif

	return
}

# NSHelpList::NSHelpList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::NSHelpList {oop parent} {

	variable Book

	Info $oop book,count 0
	set Book($oop,book) {}
	set Book($oop,0,parent) 0
	set Book($oop,0,open) 1

	set frame $parent.helplist$oop
	frame $frame \
		-borderwidth 0

	if {[Platform unix]} {
		set font {Helvetica 12}
	}
	if {[Platform windows]} {
		set font {Helvetica 8}
	}
	set rowHgt [font metrics $font -linespace]
	if {$rowHgt < 16} {set rowHgt 16}
	if {[ScreenWidth] > 1024} {
		set width 250
	} else {
		set width 170
	}
	set height 100
	set canvistId [NSObject::New NSCanvist $frame $rowHgt $width $height \
		"NSHelpList::NewItemCmd $oop" "NSHelpList::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]

	$canvas configure -yscrollcommand "$frame.yscroll set" \
		-xscrollcommand "$frame.xscroll set"
	scrollbar $frame.yscroll \
		-orient vertical -command "$canvas yview"
	scrollbar $frame.xscroll \
		-orient horizontal -command "$canvas xview"

	NSCanvist::Info $canvistId rowsEnabled 0

	# Remove all the NSCanvist bindings, and add our own. In the future
	# each NSCanvist should have a generic NSCanvistBindTag for bindings.
	foreach pattern [bind $canvas] {
		bind $canvas $pattern {}
	}
	bind $canvas <ButtonPress-1> \
		"NSHelpList::Button1 $oop %x %y"
	bind $canvas <Configure> \
		"NSHelpList::Configure $oop"
	bind $canvas <Motion> \
		"NSHelpList::Motion $oop %x %y"
	bind $canvas <Leave> \
		"NSHelpList::Motion $oop -1 -1"

	# Set the geometry. The scrollbars are only mapped if needed.
	grid rowconfigure $frame 0 -weight 1
	grid rowconfigure $frame 1 -weight 0
	grid columnconfigure $frame 0 -weight 1
	grid columnconfigure $frame 1 -weight 0
	grid $canvas \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid remove $frame.yscroll
	grid remove $frame.xscroll
	Info $oop scrollbar,vert 0
	Info $oop scrollbar,horz 0

	Info $oop canvistId $canvistId
	Info $oop canvas $canvas
	Info $oop font $font
	Info $oop frame $frame
	Info $oop ignoreSel 0
	Info $oop prev -1
	Info $oop pageCmd ""
	Info $oop balloon,visible 0

	NSUtils::DestroyObjectWithWidget NSHelpList $oop $parent

	BalloonInit $oop

	return
}

# NSHelpList::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Info {oop info args} {

	global NSHelpList

	# Verify the object
	NSObject::CheckObject NSHelpList $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSHelpList($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSHelpList($oop,$info)
			}
		}
	}

	return
}

# NSHelpList::BookAdd --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::BookAdd {oop parent title clientData} {

	global NSHelpList
	variable Book

	set book [incr NSHelpList($oop,book,count)]
	set Book($oop,$book,child) {}
	set Book($oop,$book,clientData) $clientData
	if {$parent} {
		set Book($oop,$book,depth) [expr {$Book($oop,$parent,depth) + 1}]
	} else {
		set Book($oop,$book,depth) 0
	}
	set Book($oop,$book,open) 0
	set Book($oop,$book,parent) $parent
	set Book($oop,$book,title) $title
	lappend Book($oop,$parent,child) book $book $clientData

	return $book
}

# NSHelpList::PageAdd --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::PageAdd {oop book title clientData} {

	variable Book

	lappend Book($oop,$book,child) page $title $clientData

	return
}

# NSHelpList::PageInsert --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::PageInsert {oop index book title clientData} {

	variable Book

	set Book($oop,$book,child) [linsert $Book($oop,$book,child) $index \
		page $title $clientData]

	return
}

# NSHelpList::SetListAux --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::SetListAux {oop book} {

	global NSHelpList
	variable Book

	set canvistId [Info $oop canvistId]

	if {$book} {
		set depth $Book($oop,$book,depth)
		set depthPlusOne [expr {$depth + 1}]
		NSCanvist::Insert $canvistId end book $depth $Book($oop,$book,title) \
			$Book($oop,$book,open)
		lappend NSHelpList($oop,row2child) [list book $book 0]
		if {!$Book($oop,$book,open)} return
	} else {
		set depth 0
		set depthPlusOne 0
	}
	
	set index 0
	foreach {type info clientData} $Book($oop,$book,child) {
		switch -- $type {
			book {
				SetListAux $oop $info
			}
			page {
				NSCanvist::Insert $canvistId end page $depthPlusOne $info
				lappend NSHelpList($oop,row2child) [list page $book $index]
			}
		}
		incr index
	}

	return
}

# NSHelpList::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::SetList {oop} {

	variable Book

	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]

	NSCanvist::DeleteAll $canvistId

	Info $oop prev -1
	Info $oop row2child {}
	SetListAux $oop 0

	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	set left 0
	set top 0
	$canvas configure -scrollregion "$left $top $right $bottom"

	Configure $oop

	return
}

# NSHelpList::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::NewItemCmd {oop canvistId y type depth text {open 0}} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set rowHgt [NSCanvist::Info $canvistId rowHgt]
	set font [Info $oop font]

	set fontHgt [font metrics $font -linespace]
	set diff [expr {int([expr {($rowHgt - $fontHgt) / 2}])}]

	set diff 0
	set textWid [font measure $font $text]
	set imgWid 20
	set left [expr {$imgWid + $depth * 20}]

	# Selection rectangle
	lappend itemIdList [$canvas create rectangle [expr {$left - 1}] \
		[expr {$y + $diff}] \
		[expr {$left + $textWid + 1}] [expr {$y + $fontHgt}] \
		-fill "" -outline "" -tags selrect]

	# Image
	switch -- $type {
		book {
			if {$open} {
				set image Image_BookOpen
			} else {
				set image Image_BookClosed
			}
		}
		page {
			set image Image_HelpPage
		}
	}
	set iw [image width $image]
	set ih [image height $image]
	lappend itemIdList [$canvas create image \
		[expr {$depth * 20 + ($imgWid - $iw) / 2}] \
		[expr {$y + ($rowHgt - $ih) / 2}] \
		-image $image -anchor nw -tags {enabled image}]

	# Text
	lappend itemIdList [$canvas create text $left [expr {$y + $diff}] \
		-text $text -anchor nw -font $font -fill Black -tags {enabled text}]

	return $itemIdList
}

# NSHelpList::HighlightItemCmd --
#
#	Called by NSCanvist::Select() to highlight a row.
#
# Arguments:
#	oop						OOP ID. See above.
#	canvistId				OOP ID of NSCanvist object.
#	state					1 or 0 highlight state.
#	args					List of canvas item ids
#
# Results:
#	What happened.

proc NSHelpList::HighlightItemCmd {oop canvistId state args} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set itemIdList $args

	set idRect [FindItemByTag $canvas $itemIdList selrect]
	set idText [FindItemByTag $canvas $itemIdList text]

	if {1 || [NSUtils::HasFocus $canvas]} {
		set fill [Global SystemHighlight]
		set fillText [Global SystemHighlightText]
	} else {
		set fill [Value listInactive]
		set fillText Black
	}

	if {$state} {
		$canvas itemconfigure $idRect -fill $fill -outline $fill
		$canvas itemconfigure $idText -fill $fillText
	} else {
		$canvas itemconfigure $idRect -fill "" -outline ""
		$canvas itemconfigure $idText -fill Black
	}

	return
}

# NSHelpList::IsParent --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::IsParent {oop parent book} {

	variable Book

	if {$parent == 0} {
		return [expr {$book != 0}]
	}
	set book2 $Book($oop,$book,parent)
	while {$book2} {
		if {$book2 == $parent} {
			return 1
		}
		set book2 $Book($oop,$book2,parent)
	}
	return 0
}

# NSHelpList::Expand --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Expand {oop book} {

	variable Book

	if {$Book($oop,$book,open)} return
	set Book($oop,$book,open) 1
	set parent $Book($oop,$book,parent)
	if {$parent} {
		Expand $oop $parent
	}

	return
}

# NSHelpList::Collapse --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Collapse {oop book} {

	variable Book

	if {!$Book($oop,$book,open)} return
	set Book($oop,$book,open) 0
	foreach {type book clientData} $Book($oop,$book,child) {
		if {$type eq "book"} {
			Collapse $oop $book
		}
	}

	return
}

# NSHelpList::Toggle --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Toggle {oop book} {

	variable Book

	if {$Book($oop,$book,open)} {
		Collapse $oop $book
	} else {
		Expand $oop $book
	}

	return
}

# NSHelpList::Button1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Button1 {oop x y} {

	set canvistId [Info $oop canvistId]

	set row [NSCanvist::PointToRow $canvistId $x $y]
	if {$row == -1} return
	set child [lindex [Info $oop row2child] $row]
	SetSelection $oop $child
	
	return
}

# NSHelpList::SetSelection --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::SetSelection {oop child} {

	variable Book

	set canvistId [Info $oop canvistId]

	set selection [NSCanvist::Selection $canvistId]
	scan $child "%s %d %d" typeNew bookNew indexNew
	set doList 0
	if {[llength $selection]} {
		set row [lindex $selection 0]
		set childOld [lindex [Info $oop row2child] $row]
		scan $childOld "%s %d %d" typeOld bookOld indexOld

		# A different book was selected, and the old book is not
		# an ancestor of the new book.
		if {($bookOld != $bookNew) && (!$bookNew || ![IsParent $oop $bookOld $bookNew])} {

			# Collapse the old book and its children
			Collapse $oop $bookOld

			set doList 1

			# The new book is not an ancestor of the old book, so
			# collapse the ancestors of the old book up to any common
			# parent.
			if {!$bookNew || ![IsParent $oop $bookNew $bookOld]} {
				set parentNew $Book($oop,$bookNew,parent)
				set parentOld $Book($oop,$bookOld,parent)
				while {$parentOld && ($parentOld != $parentNew)} {
					Collapse $oop $parentOld
					set parentOld $Book($oop,$parentOld,parent)
				}
			}
		}
	}

	switch -- $typeNew {
		book {
			Toggle $oop $bookNew
			set doList 1
		}
		page {
			if {$bookNew && !$Book($oop,$bookNew,open)} {
				Expand $oop $bookNew
				set doList 1
			}
		}
	}

	if {$doList} {
		SetList $oop
	}

	set row 0
	foreach child2 [Info $oop row2child] {
		if {$child2 eq $child} {
			NSCanvist::UpdateSelection $canvistId $row all
			NSCanvist::See $canvistId $row
			break
		}
		incr row
	}
	update

	switch -- $typeNew {
		book {
			if {$bookNew && ![Info $oop ignoreSel]} {
				set command [Info $oop pageCmd]
				if {[string length $command]} {
					set title $Book($oop,$bookNew,title)
					set clientData $Book($oop,$bookNew,clientData)
					uplevel #0 $command [list $title $clientData]
				}
			}
		}
		page {
			if {![Info $oop ignoreSel]} {
				set command [Info $oop pageCmd]
				if {[string length $command]} {
					set title [lindex $Book($oop,$bookNew,child) \
						[expr {$indexNew * 3 + 1}]]
					set clientData [lindex $Book($oop,$bookNew,child) \
						[expr {$indexNew * 3 + 2}]]
					uplevel #0 $command [list $title $clientData]
				}
			}
		}
	}

	return
}

# NSHelpList::ConfigureRow --
#
#	Configure a canvas item on the row of a list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::ConfigureRow {oop row tag args} {

	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]
	
	set rowTag [lindex [NSCanvist::Info $canvistId rowTags] $row]
	set itemIdList [$canvas find withtag $rowTag]
	set itemId [FindItemByTag $canvas $itemIdList $tag]
	$canvas itemconfigure $itemId {*}$args

	return
}

# NSHelpList::Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Motion {oop x y} {

	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]

	if {$x == -1 && $y == -1} {
		set row -1
	} else {
		set row [NSCanvist::PointToRow $canvistId $x $y]
	}
	
	set prev [Info $oop prev]
	if {$row == $prev} {
		return
	}
	if {$prev != -1} {
		if {[NSCanvist::IsRowSelected $canvistId $prev]} {
			if {1 || [NSUtils::HasFocus $canvas]} {
				set fill [Global SystemHighlightText]
			} else {
				set fill Black
			}
		} else {
			set fill Black
		}
		ConfigureRow $oop $prev text -fill $fill -font [Info $oop font]
	}
	if {$row == -1} {
		Info $oop prev -1
		Balloon $oop $x $y
		return
	}
	
	if {[NSCanvist::IsRowSelected $canvistId $row]} {
		if {1 || [NSUtils::HasFocus $canvas]} {
			set fill [Global SystemHighlightText]
		} else {
			set fill Black
		}
	} else {
		set fill Blue
	}
	ConfigureRow $oop $row text -fill $fill -font "[Info $oop font] underline"
	Info $oop prev $row

	Balloon $oop $x $y

	return
}

# NSHelpList::BalloonInit --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::BalloonInit {oop} {

	set canvas [Info $oop canvas]
	set win $canvas.balloon

	toplevel $win -background Black
	wm overrideredirect $win yes
	wm withdraw $win
	label $win.label \
		-text "Hello, world!" -relief flat \
		-background [Global SystemInfoBackground] -foreground black \
		-padx 2 -pady 0 -anchor w -font [Info $oop font]
	pack $win.label -side left -padx 1 -pady 1

	bind $win.label <ButtonPress-1> "
		wm withdraw $win
		NSHelpList::Info $oop balloon,visible 0
		update
		NSHelpList::Button1 $oop {*}\[NSHelpList::Info $oop balloon,xy]
	"
	bind $win.label <Leave> "
		wm withdraw $win
		NSHelpList::Info $oop balloon,visible 0
	"

	if {[Platform unix]} {
		$win configure -cursor left_ptr
	}

	Info $oop balloon,win $win

	return
}

# NSHelpList::Balloon --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Balloon {oop x y} {

	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]
	set win [Info $oop balloon,win]

	# Hack -- When the balloon window appears, "Motion -1 -1" gets called,
	# which would hide the balloon
	if {($x == -1) && ($y == -1)} {
		if {[Info $oop balloon,visible]} {
			return
		}
	}

	set row [Info $oop prev]
	if {$row == -1} {
		if {[Info $oop balloon,visible]} {
			wm withdraw $win
			Info $oop balloon,visible 0
		}
		return
	}

	set cleft [$canvas canvasx 0]
	set cright [$canvas canvasx [winfo width $canvas]]
	set ctop [$canvas canvasy 0]
	
	set rowTag [lindex [NSCanvist::Info $canvistId rowTags] $row]
	set itemIdList [$canvas find withtag $rowTag]
	set itemId [FindItemByTag $canvas $itemIdList text]
	scan [$canvas bbox $itemId] "%s %s %s %s" left top right bottom
	if {($left >= $cleft) && ($right <= $cright)} {
		return
	}

	Info $oop balloon,xy "$x $y"

	set text [$canvas itemcget $itemId -text]
	$win.label configure -text $text

	set cleft [expr {int($cleft)}]
	set ctop [expr {int($ctop)}]
	set x [expr {[winfo rootx $canvas] + $left - $cleft - 4}]
	set y [expr {[winfo rooty $canvas] + $top - $ctop - 3}]
	if {$x < 0} {set x 0}

	wm geometry $win +$x+$y
	update idletasks

	# Display and raise, don't change the focus
	wm deiconify $win
	update

	if {[Platform unix]} {
		raise $win
	}

#	update

	Info $oop balloon,visible 1

	after idle NSHelpList::TestPointer $oop

	return
}

# NSHelpList::TestPointer --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::TestPointer {oop} {

	set canvistId [Info $oop canvistId]
	set canvas [Info $oop canvas]
	set win [Info $oop balloon,win]

	if {![Info $oop balloon,visible]} return
	if {[winfo containing {*}[winfo pointerxy $win]] ne "$win.label"} {
		wm withdraw $win
		Info $oop balloon,visible 0
	}

	return
}

# NSHelpList::Configure --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::Configure {oop} {

	set canvas [Info $oop canvas]
	set frame [Info $oop frame]

	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	set left 0
	set top 0

	set doVert 0
	set doHorz 0
	set height [winfo height $canvas]
	set width [winfo width $canvas]
	if {$bottom - $top > $height} {
		set doVert 1
		if {![Info $oop scrollbar,vert]} {
			incr width -[winfo reqwidth $frame.yscroll]
		}
	}
	if {$right - $left > $width} {
		set doHorz 1
		if {![Info $oop scrollbar,horz]} {
			incr height -[winfo reqheight $frame.xscroll]
		}
		if {$bottom - $top > $height} {
			set doVert 1
		}
	}
	if {$doVert != [Info $oop scrollbar,vert]} {
		set width [winfo reqwidth $canvas]
		if {$doVert} {
			grid $frame.yscroll
			incr width -[winfo reqwidth $frame.yscroll]
		} else {
			grid remove $frame.yscroll
			incr width +[winfo reqwidth $frame.yscroll]
		}
		$canvas configure -width $width
		Info $oop scrollbar,vert $doVert
	}
	if {$doHorz != [Info $oop scrollbar,horz]} {
		set height [winfo reqheight $canvas]
		if {$doHorz} {
			grid $frame.xscroll
			incr height -[winfo reqheight $frame.xscroll]
		} else {
			grid remove $frame.xscroll
			incr height +[winfo reqheight $frame.xscroll]
		}
		$canvas configure -height $height
		Info $oop scrollbar,horz $doHorz
	}

	return
}

# NSHelpList::SelectPage --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::SelectPage {oop command} {

	variable Book

	set canvistId [Info $oop canvistId]

	foreach name [array names Book $oop,*,clientData] {
		scan $name $oop,%d,clientData book
		set data $Book($name)
		if {[eval $command [list $data]]} continue
		Info $oop ignoreSel 1
		SetSelection $oop [list book $book 0]
		Info $oop ignoreSel 0
		return
	}

	foreach name [array names Book $oop,*,child] {
		scan $name $oop,%d,child book
		set index -1
		foreach {type info data} $Book($name) {
			incr index
			if {$type ne "page"} continue
			if {[eval $command [list $data]]} continue
			Info $oop ignoreSel 1
			SetSelection $oop [list page $book $index]
			Info $oop ignoreSel 0
			return
		}
	}

	Info $oop ignoreSel 1
	SetSelection $oop [list book 0 0]
	Info $oop ignoreSel 0

	return
}

# NSHelpList::ToHtmlAux --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::ToHtmlAux {oop book} {

	global NSHelpList
	variable Book
	variable Html

	if {$book} {
		set clientData $Book($oop,$book,clientData)
		set html $Book($oop,$book,title)
		if {[string length $clientData]} {
			set html "<a href=\"$clientData\">$html</a>"
		}
		if {$Book($oop,$book,parent)} {
			append Html "<li>$html</li>\n"
		} else {
			append Html "<h2>$html</h2>\n"
		}
		if {[llength $Book($oop,$book,child)]} {
			append Html "<ul>\n"
		}
	}

	foreach {type info clientData} $Book($oop,$book,child) {
		switch -- $type {
			book {
				ToHtmlAux $oop $info
			}
			page {
				if {[string length $clientData]} {
					regsub -all " " $clientData %20 clientData
					set html "<a href=\"$clientData\">$info</a>"
				} else {
					set html $info
				}
				if {$book} {
					set html "<li>$html</li>"
				}
				append Html $html\n
			}
		}
	}
	if {$book && [llength $Book($oop,$book,child)]} {
		append Html "</ul>\n"
	}

	return
}

# NSHelpList::ToHtml --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelpList::ToHtml {oop} {

	variable Html

	set Html {}
	ToHtmlAux $oop 0

	return $Html
}

