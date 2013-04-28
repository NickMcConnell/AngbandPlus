# File: record.tcl

# Purpose: the Character Record Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSRecord {

	variable MenuString
	variable Priv

# namespace eval NSRecord
}

# NSRecord::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::InitModule {} {

	variable Priv

	MsgCatInit record

	package require TclZip

	set Priv(image,y) -1
	set Priv(image,x) -1

	NSObject::New NSRecord

	return
}

# NSRecord::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::CloseModule {} {

	catch {
		RemoveTemps [Global record,oop]
		destroy [Window record]
	}

	return
}

# NSRecord::NSRecord --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::NSRecord {oop} {

	Info $oop temp,dump ""
	Info $oop temp,message ""
	Info $oop temp,photo ""
	Info $oop temp,photoText ""

	Info $oop record ""
	Info $oop imageName ""

	Info $oop display,entry {}
	Info $oop display,widget text

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow record $win \
		"NSRecord::GeometryCmd $oop" "" "NSRecord::DisplayCmd $oop"

	bind $win <KeyPress-Escape> "NSRecord::Close $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSRecord $oop $win

	#
	# Global list of application windows
	#

	Global record,oop $oop
	Window record $win

	return
}

# NSRecord::~NSRecord --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::~NSRecord {oop} {

	# bindings are deleted with the text widget

	return
}

# NSRecord::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::Info {oop info args} {

	global NSRecord

	# Verify the object
	NSObject::CheckObject NSRecord $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSRecord($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSRecord($oop,$info)
			}
		}
	}

	return
}

# NSRecord::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::InitWindow {oop} {

	set win .record$oop
	toplevel $win
	wm title $win [mc "Character Record"]

	# Start out withdrawn (hidden)
	wm withdraw $win

	TransientToWin $win [Window highscore]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSRecord::Close $oop"

	Info $oop win $win

	# Menus
	InitMenus $oop

	#
	# Tabs!
	#

	set tabsId [NSObject::New NSTabs $win]
	NSTabs::Info $tabsId invokeCmd "NSRecord::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	MakeDivider $win.divider1 x

	#
	# Text and scrollbars
	#

	set frame $win.frameContent
	frame $frame \
		-borderwidth 1 -relief sunken
	text $frame.text \
		-borderwidth 0 -highlightthickness 0 \
		-width 80 -height 16 -background Black \
		-foreground White -cursor {} -wrap none \
		-font [Value font,fixed] \
		-xscrollcommand "$frame.xscroll set" \
		-yscrollcommand "$frame.yscroll set"
	scrollbar $frame.xscroll \
		-orient horizontal -command "$frame.text xview"
	scrollbar $frame.yscroll \
		-orient vertical -command "$frame.text yview"

	Info $oop text $frame.text
	Info $oop xscroll $frame.xscroll
	Info $oop yscroll $frame.yscroll

	qebind $frame.text <Value-font,fixed> \
		"NSRecord::ValueChanged_font_fixed $oop"

	#
	# Canvas. This is used to display the photo image, if any.
	# The scroll behaviour is desireable over that of the Text
	# widget, plus the Text widget takes some time to create
	# the image instance each time.
	#

	# Photo canvas
	NSModule::LoadIfNeeded NSPhoto
	set photoId [NSObject::New NSPhoto $frame]
	NSPhoto::Info $photoId examineCmd \
		"NSRecord::ExamineCmd $oop"
	set canvas [NSPhoto::Info $photoId canvas]
	$canvas configure -background Black -yscrollincrement 1 \
		-xscrollincrement 1 -cursor fleur
	Info $oop photoId $photoId

	# Scroll the preview image when the mouse is dragged
	$canvas bind image <ButtonPress-1> "
		set NSRecord::Priv(image,x) %x
		set NSRecord::Priv(image,y) %y
	"
	$canvas bind image <Button1-Motion> \
		"NSRecord::ImageDrag $oop %x %y"

	Info $oop canvas $canvas

	# Statusbar
	MakeStatusBar $win.statusBar 20

	# Content geometry
	grid rowconfigure $frame 0 -weight 1
	grid rowconfigure $frame 1 -weight 0
	grid columnconfigure $frame 0 -weight 1
	grid columnconfigure $frame 1 -weight 0

	grid $frame.text \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	# Swap the canvas/text as needed
	grid $canvas \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid remove $canvas

	# This is to avoid an annoying flicker when swapping the text
	# and canvas.
#	grid [frame $frame.background -borderwidth 0 -background Black] \
#		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news

	# Window geometry
	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 0
	grid rowconfigure $win 2 -weight 1
	grid rowconfigure $win 3 -weight 0
	grid columnconfigure $win 0 -weight 1

	if {[Platform windows]} {
		grid $win.divider1 \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frameContent \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	NSTabs::Add $tabsId [mc Dump]
	NSTabs::Add $tabsId [mc Messages]
	NSTabs::Add $tabsId [mc Photo]

	return
}

# NSRecord::InitMenus --
#
#	Create menus in the toplevel associated with this object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSRecord::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSRecord::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSRecord::MenuInvoke $oop"

	#
	# Record Menu
	#

	set menuId [NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_RECORD]
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_RECORD -label [mc Record] -underline 0 -identifier M_RECORD
	NSMenu::Info $menuId setupMode normal

	set entries {}
	lappend entries [list -type command -label [mc "Save As..."] \
		-identifier E_SAVE_AS]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_RECORD $entries

	set MenuString(M_RECORD) \
		"Contains commands for saving files."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSRecord::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::SetupMenus {oop mbarID} {

	return
}

# NSRecord::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		{} {
			set desc {}
		}

		E_SAVE_AS {
			switch -- [Info $oop display,what] {
				dump {set string "character dump"}
				message {set string "message log"}
				photo {set string "photo image"}
			}
			set desc "Saves the displayed $string to a new file."
		}

		default {
			if {[info exists MenuString($ident)]} {
				set desc $MenuString($ident)
			} else {
				set menu [NSMenu::Info $menuId menu]
				set desc [$menu entrycget $index -label]
			}
		}
	}

	set statusBar [Info $oop win].statusBar
	$statusBar cover set $desc
	if {($desc eq "") && ($menuId == [Info $oop mbarId])} {
		if {[$statusBar cover visible]} {
			$statusBar cover hide
		} else {
			$statusBar cover show
		}
	}

	return
}

# NSRecord::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_SAVE_AS {SaveAs $oop}
		E_CLOSE {Close $oop}
	}

	return
}

# NSRecord::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
			if {[llength $args]} {
				set record [lindex $args 0]
				SetRecord $oop $record

				set entryList [Info $oop display,entry]
				if {[llength $entryList]} {
					set entry [lindex $entryList 0]
					if {$entry eq "photo"} {
					} else {
						SetDisplay $oop $entry
					}
				}
			}
		}
		postDisplay {
			set entryList [Info $oop display,entry]
			if {[llength $entryList]} {
				set entry [lindex $entryList 0]
				if {$entry eq "photo"} {
					SetDisplay $oop photo
				}
			}
			set widget [Info $oop [Info $oop display,widget]]
			[Info $oop xscroll] set {*}[$widget xview]
			[Info $oop yscroll] set {*}[$widget yview]
		}
		reDisplay {
		}
		postWithdraw {
			RemoveTemps $oop
		}
	}

	return
}

# NSRecord::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::GeometryCmd {oop} {

	set win [Info $oop win]
	if {[angband player is_dead]} {
		set width [font measure [Value font,fixed] [string repeat "W" 80]]
		set height 400
#		set width [winfo width $win]
#		set height [winfo height $win]
		set x [expr {([ScreenWidth $win] - $width) / 2 \
			- [winfo vrootx $win]}]
		set y [expr {([ScreenHeight $win] - $height) / 3 \
			- [winfo vrooty $win]}]
		return ${width}x$height+$x+$y
	}
	return [GetDefaultGeometry $win reqwidth main]
}

# NSRecord::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::Close {oop} {

	NSWindowManager::Undisplay record

	return
}

# NSRecord::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::InvokeTab {oop tabsId tabId} {

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetDisplay $oop [lindex [Info $oop display,entry] $index]

	return
}

# NSRecord::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSRecord::SetRecord --
#
#	Reads in information from the given record.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::SetRecord {oop record} {

	global Angband
	variable Priv

	set win [Info $oop win]
	set tabsId [Info $oop tabsId]

	if {[Info $oop record] eq $record} {
		return
	}

	# Remove all the tabs
	set i 0
	foreach tabId [NSTabs::Info $tabsId id] {
		NSTabs::Remove $tabsId $tabId
		bind $win <KeyPress-[incr i]> {}
	}

	# List of entries displayed (ie, what the tabs are for)
	Info $oop display,entry {}

	# Delete temporary files associated with the current record
	RemoveTemps $oop

	set archive [PathUser $record]
	if {![file exists $archive]} {
		return
	}

	set tabList {}

	set zipCmd [zip zipCmd]
	$zipCmd read $archive
	set i 0
	set entryList [$zipCmd glob]
	foreach entry [list dump.txt message.txt photo.gif photo.txt] \
			info [list dump message photo photoText] \
			title [list Dump Messages Photo PhotoText] {

		if {[lsearch -exact $entryList $entry] == -1} continue
		set tempFile [NSUtils::TempFileName $Angband(dir)]
		$zipCmd extract $entry $tempFile
		Info $oop temp,$info $tempFile

		if {$entry eq "photo.txt"} continue

		NSTabs::Add $tabsId [mc $title]
		lappend tabList $info

		bind $win <KeyPress-[incr i]> "NSRecord::SetDisplay $oop $info"
	}
	rename $zipCmd ""

	Info $oop display,entry $tabList
	Info $oop record $record

	# Window title
	set title [mc "Character Record"]
	if {[lsearch -exact $Priv(record,list) $record] != -1} {
		array set recordArray $Priv(record,$record)
		if {[info exists recordArray(who)]} {
			set title [format [mc "Character Record for %s"] $recordArray(who)]
		}
	}
	wm title $win $title

	return
}

# NSRecord::SetDisplayAux --
#
#	Configures the Canvas or Text widget as the current display method.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::SetDisplayAux {oop display} {

	set canvas [Info $oop canvas]
	set text [Info $oop text]
	set xscroll [Info $oop xscroll]
	set yscroll [Info $oop yscroll]

	set current [Info $oop display,widget]
	if {$current eq $display} return

	switch -- $display {
		canvas {
			set widget1 $text
			set widget2 $canvas
		}
		text {
			set widget1 $canvas
			set widget2 $text
		}
	}

	grid remove $widget1
	grid $widget2

	$widget1 configure -xscrollcommand "" -yscrollcommand ""
	$widget2 configure -xscrollcommand "$xscroll set" \
		-yscrollcommand "$yscroll set"

	$xscroll configure -command "$widget2 xview"
	$yscroll configure -command "$widget2 yview"

	Info $oop display,widget $display

	return
}

# NSRecord::SetDisplay --
#
#	Display something.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::SetDisplay {oop display} {

	set canvas [Info $oop canvas]
	set text [Info $oop text]

	set displayList [Info $oop display,entry]

	switch -- $display {
		dump -
		message {
			SetDisplayAux $oop text
			$text delete 1.0 end
			set tempFile [Info $oop temp,$display]
			if {[string length $tempFile]} {
				catch {
					set fileId [open $tempFile]
					$text insert end [read $fileId]
					close $fileId
				}
			}
			[Info $oop xscroll] set {*}[$text xview]
			[Info $oop yscroll] set {*}[$text yview]
		}
		photo {
			$text delete 1.0 end
			SetDisplayAux $oop canvas
			update
			set tempFile [Info $oop temp,photo]
			if {[string length $tempFile]} {
				set imageName [Info $oop imageName]
				if {![string length $imageName]} {
					StatusBar $oop [mc "Reading image file..."] 0
					update idletasks
					set imageName Image_Record$oop
					image create photo $imageName -file $tempFile -gamma 0.9
					Info $oop imageName $imageName
					$canvas itemconfigure image -image $imageName
					$canvas configure -scrollregion [$canvas bbox image]
					StatusBar $oop [mc "Done."] 1
				}
			}
			[Info $oop xscroll] set {*}[$canvas xview]
			[Info $oop yscroll] set {*}[$canvas yview]

			set tempFile [Info $oop temp,photoText]
			if {[string length $tempFile]} {
				NSPhoto::ReadPhotoText [Info $oop photoId] $tempFile
			}
		}
	}

	Info $oop display,what $display

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $displayList $display]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSRecord::ImageDrag --
#
#	Handles <Button1-Motion> events in the photo image. This allows
#	the user to scroll the image by clicking and dragging the mouse.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::ImageDrag {oop x y} {

	variable Priv

	# Get the canvas
	set canvas [Info $oop canvas]

	# Calculate the distance the pointer moved
	set dx [expr {$x - $Priv(image,x)}]
	set dy [expr {$y - $Priv(image,y)}]

	# Require minimum movement
	if {abs($dx) < 5} {
		set dx 0
	}
	if {abs($dy) < 5} {
		set dy 0
	}

	# Scroll the canvas
	set bbox [$canvas bbox all]
	set width [expr {[lindex $bbox 2] - [lindex $bbox 0]}]
	if {$width > [winfo width $canvas]} {
		$canvas xview scroll [expr -$dx] units
	}
	set height [expr {[lindex $bbox 3] - [lindex $bbox 1]}]
	if {$height > [winfo height $canvas]} {
		$canvas yview scroll [expr -$dy] units
	}

	# Remember the current pointer position
	if {$dx} {
		set Priv(image,x) $x
	}
	if {$dy} {
		set Priv(image,y) $y
	}

	return
}

# NSRecord::RemoveTemps --
#
#	Deletes any temporary files associated with the current record.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::RemoveTemps {oop} {

	set canvas [Info $oop canvas]

	# Reset the canvas image so our image instance is deleted below
	$canvas itemconfigure image -image Image_Empty

	foreach name [list dump message photo photoText] {
		set path [Info $oop temp,$name]
		if {[file exists $path]} {
			file delete $path
		}
		Info $oop temp,$name ""
	}

	set imageName [Info $oop imageName]
	if {[string length $imageName]} {
		image delete $imageName
		Info $oop imageName ""
	}

	Info $oop record ""

	return
}

# NSRecord::LoadRecords --
#
#	Reads the score.txt entry from each of the character records on
#	disk, keeping info about which character each record applies to.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::LoadRecords {} {

	global Angband
	variable Priv

	array unset Priv record,*

	# Build a list of records we know about
	set Priv(record,list) {}

	# Get a list of all the character records in the lib/user directory.
	# A character record is a zip archive with a name like "dump0001.zip".
	set pattern dump*.zip
	set fileList [lsort -dictionary [glob -nocomplain -directory [PathUser] $pattern]]

	# Check each file
	foreach file $fileList {

		# Read the contents of the zip archive
		set zipCmd [zip zipCmd]
		$zipCmd read $file

		# Make sure there is a score.txt file in the archive
		if {[lsearch -exact [$zipCmd glob] score.txt] == -1} {

			# Clean up the zip archive manager
			rename $zipCmd ""

			# Check the next record
			continue
		}

		set buffer [$zipCmd extract -tomemory score.txt]
		set scoreInfoList [split $buffer \n]

		# Clean up the zip archive manager
		rename $zipCmd ""

		# Build a list of name/value pairs
		set record {}

		foreach string $scoreInfoList {

			# Look for lines such as ".race Human"
			if {![regexp {\.([^"]*) "?([^"]*)"?} $string ignore field value]} continue

			# Convert some fields to match those field names and/or
			# values found in the scores.raw file.
			if {$field eq "class"} {
				lappend record p_c [lsearch -exact [angband info class_name] $value]
			} elseif {$field eq "race"} {
				lappend record p_r [lsearch -exact [angband info race_name] $value]
			} elseif {$field eq "sex"} {
				if {$value eq "Male"} {
					set value m
				} elseif {$value eq "Female"} {
					set value f
				}
			}

			# Build a list of name/value pairs
			lappend record $field $value
		}

		# Remember this record
		set name [file tail $file]
		set Priv(record,$name) $record

		# Build a list of records we know about
		lappend Priv(record,list) $name
	}

	return
}

# NSRecord::MatchRecordsToScores --
#
#	Given a list of scores, return a list of records matching
#	each score.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::MatchRecordsToScores {scoreList} {

	variable Priv

	set recordList $Priv(record,list)

	set result {}
	foreach score $scoreList {
		lappend result ""
	}

	set scoreIndex 0

	# Check each score
	foreach score $scoreList {

		array set scoreArray $score

		set recordIndex 0

		# Check each record
		foreach record $recordList {

			array set recordArray $Priv(record,$record)

			# Assume the records match
			set match 1

			# Compare the record to the score
			foreach {name value} $Priv(record,$record) {

				# Only compare field names matching those in
				# the score.
				if {![info exists scoreArray($name)]} continue

				# OAngband uses 2.8.3 for scores, not 0.4.0
				if {$name eq "what"} continue

				# Compare the fields
				if {$recordArray($name) ne $scoreArray($name)} {

					# Not a match
					set match 0

					# Stop
					break
				}
			}

			# The record matches the score
			if {$match} {

				# Remember which record goes with the score
				lset result $scoreIndex $record

				# Remove the record from the list
				set recordList [lreplace $recordList $recordIndex $recordIndex]

				# Stop
				break
			}

			incr recordIndex
		}

		incr scoreIndex
	}

	return $result
}

# NSRecord::ExamineCmd --
#
#	Describe what is at the given cave location.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::ExamineCmd {oop photoId y x} {

	global NSPhoto

	if {[info exists NSPhoto($photoId,examine,$y,$x)]} {
		StatusBar $oop [NSPhoto::Info $photoId examine,$y,$x] 0
		switch -- [NSPhoto::Info $photoId what,$y,$x] {
			monster {
				NSRecall::RecallMonster [NSPhoto::Info $photoId idx,$y,$x]
			}
			object {
				NSRecall::RecallObjectKind [NSPhoto::Info $photoId idx,$y,$x]
			}
		}
	} else {
		StatusBar $oop "" 0
	}

	return
}

# NSRecord::SaveAs --
#
#	Save part of the character record to a new file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::SaveAs {oop} {

	global Angband
	variable Priv

	set display [Info $oop display,what]
	set record [Info $oop record]
	set win [Info $oop win]

	set charName "Untitled"
	if {[lsearch -exact $Priv(record,list) $record] != -1} {
		array set recordArray $Priv(record,$record)
		if {[info exists recordArray(who)]} {
			set charName $recordArray(who)
		}
	}

	switch -- $display {
		dump {
			set path [tk_getSaveFile -initialfile $charName.txt \
				-initialdir $Angband(dir) -parent $win]
		}
		message {
			set path [tk_getSaveFile -initialfile $charName.msg \
				-initialdir $Angband(dir) -parent $win]
		}
		photo {
			set path [tk_getSaveFile -initialfile $charName.gif \
				-initialdir $Angband(dir) -parent $win]
		}
	}
	if {![string length $path]} return
	set fileToCopy [Info $oop temp,$display]

	if {[file exists $fileToCopy]} {
		file copy -force $fileToCopy $path
	}

	return
}

# NSRecord::ValueChanged_font_fixed --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecord::ValueChanged_font_fixed {oop} {

	[Info $oop text] configure -font [Value font,fixed]

	return
}
