# File: choose-game.tcl

# Purpose: the Choose Game Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSChooseGame {

	variable Priv

# namespace eval NSChooseGame
}

# NSChooseGame::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::InitModule {} {

	variable Priv

	package require treectrl
	option add *TreeCtrl.UseTheme 1

	MsgCatInit startup

	if {[llength [Value savefile]]} {
		set Priv(recent,path) [file join {*}[Value savefile]]
	} else {
		set Priv(recent,path) ""
	}

	set Priv(page) [list living dead backup]

	NSObject::New NSChooseGame

	return
}

# NSChooseGame::NSChooseGame --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::NSChooseGame {oop} {

	InitWindow $oop

	set win [Info $oop win]

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSChooseGame $oop $win

	Info $oop current -1
	Info $oop page living

	# Create a progress window
	set progId [NSObject::New NSProgressDialog]
	Info $oop progId $progId

	# Sort filenames by Name, Version or Description
	Info $oop sortBy 0

	# Configure with the progress window
	set win [NSProgressDialog::Info $progId win]
	wm title $win [mc scan-title]
	wm transient $win .

	# Position and display the progress window
	WindowPosition $win 2 3
	focus [NSProgressDialog::Info $progId win]
	update

	# Prevent long filenames resizing the progress window
	wm geometry $win [wm geometry $win]

	# Let the user choose a saved game
	ModalLoop $oop

	return
}

# NSChooseGame::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::Info {oop info args} {

	global NSChooseGame

	# Verify the object
	NSObject::CheckObject NSChooseGame $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSChooseGame($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSChooseGame($oop,$info)
			}
		}
	}

	return
}

# NSChooseGame::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::InitWindow {oop} {

	variable Priv

	set win .choosegame$oop
	toplevel $win
	wm title $win [mc choose-title]

	wm withdraw $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "bell"

	wm transient $win .
	wm resizable $win no no

	Info $oop win $win

	frame $win.framePrompt \
		-borderwidth 0
	frame $win.framePrompt.filler1 \
		-borderwidth 0 -height 5
	message $win.framePrompt.message \
		-anchor w -width 400 -text [mc choose-prompt] -borderwidth 0
	pack $win.framePrompt.filler1 \
		-side top -fill x
	pack $win.framePrompt.message \
		-side top -fill x

	#
	# Tabbed frame
	#
	
	set tabsId [NSObject::New NSTabs $win]
	set notebook [NSTabs::Info $tabsId canvas]

	foreach page $Priv(page) {
		set id [NSTabs::Add $tabsId [mc choose-tab-$page]]
		$notebook tab $id -padding 8
	}
	NSTabs::Info $tabsId invokeCmd "NSChooseGame::InvokeTab $oop"
	NSTabs::Info $tabsId active 1

	Info $oop tabsId $tabsId

	#
	# List + Scrollbar
	#

	set frameList $notebook.frameList
	frame $frameList \
		-borderwidth 1 -relief solid

	set font [Value font,system]
	set fontHgt [font metrics $font -linespace]
	set rowHgt [expr {$fontHgt + 1}]
	if {$rowHgt < 20} { set rowHgt 20 }
	Info $oop font $font

	set tree [treectrl $frameList.tree -borderwidth 0 -highlightthickness 0 \
		-font $font -width 450 -showbuttons no \
		-showlines no -showroot no -itemheight $rowHgt]
	$tree notify bind $tree <Scroll-y> \
		"$frameList.yscroll set %l %u"
	scrollbar $frameList.yscroll \
		-command "$tree yview"

	foreach tag {Name Version Description} {
		$tree column create -text [mc choose-$tag] -tag $tag -textpadx 4
	}
	$tree column configure Name -expand yes
	$tree column configure Description -expand yes

	# Hack -- set list height
	scan [$tree column bbox 0] "%d %d %d %d" left top right bottom
	$tree configure -height [expr {($bottom - $top) + $rowHgt * 11}]

	$tree notify install <Header-invoke>
	$tree notify bind $tree <Header-invoke> \
		"NSChooseGame::SortBy $oop %C"

	$tree notify bind $tree <Selection> \
		"NSChooseGame::SelectionChanged $oop %T %c %S %D"

	bind $tree <Double-ButtonPress-1> \
		"NSUtils::InvokeButton $win.frameButton.open"

	# Create elements and styles
	$tree state define recent
	$tree element create eSel rect \
		-fill [list [Global SystemHighlight] selected #dedede {}] \
		-showfocus no
	# -font [list  [BoldFont $font] {alive !bkp}]
	$tree element create eTxt text -font [list [BoldFont $font] recent $font {}] \
		-fill [list [Global SystemHighlightText] selected Black {}]
	$tree style create s0
	$tree style elements s0 {eSel eTxt}
	$tree style layout s0 eSel -iexpand xy -pady {1 0} -detach yes
	$tree style layout s0 eTxt -expand ns -padx {4 20}
	$tree configure -defaultstyle {s0 s0 s0}

	Info $oop tree $tree

	pack $tree \
		-side left -expand yes -fill both
	pack $frameList.yscroll \
		-side right -fill y

	set frame $win.frameButton
	frame $frame \
		-borderwidth 0
	button $frame.open \
		-text [mc choose-Open] -command "NSChooseGame::Open $oop" \
		-width 9 -default active
	button $frame.browse \
		-text [mc Browse...] -command "NSChooseGame::Browse $oop" -width 9
	button $frame.cancel \
		-text [mc Cancel] -command "NSChooseGame::Info $oop result cancel" \
		-width 9

	NSUtils::CheckButtonWidth $frame.open
	NSUtils::CheckButtonWidth $frame.browse
	NSUtils::CheckButtonWidth $frame.cancel

	switch -- [Platform] {
		windows {
			set padx 5
		}
		unix {
			set padx 2
		}
	}
set padx 10

	pack $frame.open \
		-side left -padx 0
	pack $frame.browse \
		-side left -padx $padx
	pack $frame.cancel \
		-side left -padx 0

	label $win.fileName \
		-anchor w -justify left -font $font -text "\n"

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid rowconfigure $win 2 -weight 0
	grid rowconfigure $win 3 -weight 0
	grid columnconfigure $win 0 -weight 1
	grid columnconfigure $win 1 -weight 0

	grid $win.framePrompt \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -padx 10 -pady 0 -sticky ew
	grid $notebook \
		-row 1 -column 0 -rowspan 1 -columnspan 2 -padx 10 -pady 5 -sticky news
	grid $win.fileName \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -padx 20 -sticky ew
	grid $win.frameButton \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -padx 10 -pady 5
	grid [frame $win.filler1 -borderwidth 0 -height 5] \
		-row 3 -column 1 -rowspan 1 -columnspan 2 -padx 10 -sticky ew

place $notebook.frameList -x 0 -y 0

	NSUtils::SetDefaultButton $win $win.frameButton.open

#	bind $win <KeyPress-o> \
#		"NSUtils::InvokeButton $win.frameButton.open"
#	bind $win <KeyPress-b> \
#		"NSUtils::InvokeButton $win.frameButton.browse"
	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $win.frameButton.cancel"
	bind $win <KeyPress-Delete> \
		"NSChooseGame::Delete $oop"
	bind $win <KeyPress-BackSpace> \
		"NSChooseGame::Delete $oop"

	# Select a row in the list as the user types
	Info $oop typing,string ""
	Info $oop typing,click 0
	bind $tree <KeyPress> "NSChooseGame::Typing $oop %A"

	return
}

# NSChooseGame::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::Close {oop} {

	destroy [Info $oop win]
#	NSObject::Delete NSChooseGame $oop

#	namespace delete ::NSChooseGame

	return
}

# NSChooseGame::SelectionChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::SelectionChanged {oop tree count select deselect} {

	set win [Info $oop win]

	if {![llength $select]} {
		$win.fileName configure -text "\n"
		$win.frameButton.open configure -state disabled
		Info $oop current -1
		return
	}

	set item [lindex $select 0]
	set row [$tree item order $item -visible]
	set path [lindex [Info $oop pathList] $row]
	set pad "    "
	if {[Platform unix]} {
		set pad "  "
	}
	$win.fileName configure -text "[mc choose-file]\n$pad\"[file tail $path]\""

	Info $oop current $row
	$win.frameButton.open configure -state normal

	return
}

# NSChooseGame::ReadSavefileInfo --
#
#	Read the tk\config\savefile file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::ReadSavefileInfo {oop} {

	global Angband
	variable Priv

	set file [PathTk config savefile]
	if {![file exists $file]} return

	set fileId [open $file]
	catch {
		foreach string [split [read $fileId] \n] {
			if {![string length $string]} continue
			if {[string index $string 0] eq "#"} continue
			set count [scan $string \
				{"%[^\"]" %d %d %s "%[^\"]" "%[^\"]" "%[^\"]" %d %d %d %d"} \
				file mtime size version name race class level \
				depth max_depth is_dead]
			# "
			if {$count == 11} {
				set Priv(savefile,$file) [list file $file mtime $mtime \
					size $size version $version name $name race $race \
					class $class \
					lev $level depth $depth max_depth $max_depth \
					is_dead $is_dead]
			} elseif {$count == 4} {
				set Priv(savefile,$file) [list file $file mtime $mtime \
					size $size version $version name ""]
			}
		}
	}
	close $fileId

	return
}

# NSChooseGame::ModalLoop --
#
#	Present the dialog, interact, then withdraw.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::ModalLoop {oop} {

	global Angband
	global NSChooseGame
	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]
	set font [Info $oop font]
	set progId [Info $oop progId]

	set savefileInfo {}

	# Read tk\config\savefile, which contains info about each savefile.
	# This is faster than calling "angband game savefile_info".
	ReadSavefileInfo $oop

	# Create tk\config\savefile.
	set fileId [openlf [PathTk config savefile]]
	puts $fileId "# Automatically generated. Do not edit.\n"

	set glob [glob -nocomplain -directory [Path lib save] -tails *]
	set fileList [lsort -dictionary $glob]
	set count [llength $fileList]
	set i 0
	foreach file $fileList {

		set path [Path lib save $file]
		catch {
			set path [LongName $path]
		}

		# Ignore too-small files here, and hope the
		# version check in savefile_info() catches other
		# non-savefiles
		if {[file size $path] < 5000} continue

		set addFile 1
		set doScan 1

		# See if info for this savefile exists in our cache
		if {[info exists Priv(savefile,$file)]} {
			array set attrib $Priv(savefile,$file)

			# If this file appears the same as what was cached, then
			# there is no need to scan it again.
			if {($attrib(mtime) == [file mtime $path]) && \
				($attrib(size) == [file size $path])} {
				set doScan 0
			}
		}

		# Parse this file (it might not be a savefile)
		if {$doScan} {
			if {[catch {
				set attrib(name) ""
				angband game savefile_info $path attrib
			}]} {
				set addFile 0
			}
		}

		# This isn't a valid savefile
		if {!$addFile} {
			incr i
			continue
		}

		# If the name is empty it is an old savefile
		if {![string length $attrib(name)]} {
			lappend savefileInfo [list $file $path $attrib(version) "" 0]
			puts $fileId "\"$file\" [file mtime $path] [file size $path]\
				$attrib(version)"

		# We have complete info about this savefile
		} else {
			set other [format [mc choose-savefile] $attrib(lev) $attrib(race) \
				$attrib(class) $attrib(depth) $attrib(max_depth)]
			lappend savefileInfo [list $attrib(name) $path $attrib(version) \
				$other $attrib(is_dead)]
			puts -nonewline $fileId "\"$file\" [file mtime $path]\
				[file size $path] $attrib(version)"
			puts -nonewline $fileId " \"$attrib(name)\" \"$attrib(race)\"\
				\"$attrib(class)\" $attrib(lev)"
			puts $fileId " $attrib(depth) $attrib(max_depth) $attrib(is_dead)"
		}

		incr i
		NSProgressDialog::Info $progId prompt [format [mc scan-file] $file]
		NSProgressDialog::Info $progId ratio [expr {$i / double($count)}]
		update idletasks
	}

	# Close tk\config\savefile
	close $fileId

	NSProgressDialog::Info $progId ratio 1.0
	NSProgressDialog::Info $progId prompt [mc scan-display]
	update idletasks

	# Assume the list will be empty
	$win.frameButton.open configure -state disabled

	Info $oop pathList {}
	Info $oop savefileInfo $savefileInfo

	# Are there any files?
	if {[llength $savefileInfo]} {

		# Set the list, with all files so we can get the max item width
		SetList $oop 1

		# Set the column widths so they don't change when flipping pages
		foreach C [$tree column list] {
			$tree column configure $C -width [$tree column width $C]
		}

		# Set the list width
		scan [$tree column bbox 2] "%d %d %d %d" left top right bottom
		if {$right > [$tree cget -width]} {
			$tree configure -width $right
		}

		# Set the list, with the current page only
		SetList $oop

		NSToplevel::NaturalSize $win ""

		# Select the initial file
		set recent [Info $oop recent]
		if {$recent != -1} {
			$tree activate "root child $recent"
			$tree selection modify active all
			$tree see active -center y
		}
	} else {

		NSToplevel::NaturalSize $win ""
	}

	NSProgressDialog::Close $progId

set frameList [winfo parent $tree]
set tab0 [NSTabs::GetNthId [Info $oop tabsId] 0]
place $frameList -x [winfo x $tab0] -y [winfo y $tab0]
update
foreach tab [NSTabs::Info [Info $oop tabsId] id] {
	$tab configure -height [winfo reqheight $frameList] \
		-width [winfo reqwidth $frameList]
}

	# Freeze the geometry so the selected-savefile label doesn't cause
	# the window to get wider.
	update idletasks
	wm geometry $win [wm geometry $win]

	# Position window
	WindowPosition $win 2 3

	# Weird. The yscrollcommand gets called 3 times,
	# but still the scrollbar stays at the top...
	update
#	eval $win.frameList.yscroll set [$tree yview]

	# Set up a grab and claim focus too
	NSUtils::GrabSave $win
	focus $tree

	# Wait
	Info $oop result ""
	tkwait variable NSChooseGame($oop,result)

	# Release grab and reset focus
	NSUtils::GrabRelease $win

	# Hack -- Update the button
	update

	switch -- [Info $oop result] {
		browse -
		open {
			Close $oop
			set path $Priv(recent,path)
			Value savefile [file split $path]
			after idle angband game open [list $path]
		}
		cancel {
			Close $oop
		}
	}

	return
}

# NSUtils::RadioBox --
#
#	Put up a dialog with radiobuttons.
#
# Arguments:
#	arg1				about arg1
#
# Results:
#	What happened.

proc NSUtils::RadioBox {args} {

    variable Priv

    set win .radiobox

	set title "radiobox-title"
	set initial ""
	set message "radiobox-message"
	set buttons [list OK Cancel]
	set choices [list "Choice A" a "Choice B" b]
	set parent [winfo toplevel [focus]]
	set resultVar ""

	foreach {option value} $args {
		switch -- $option {
			-buttons -
			-choices -
			-title  -
			-initial -
			-message -
			-parent {
				set [string range $option 1 end] $value
			}
			-result {
				set resultVar $value
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
	if {$::tcl_platform(platform) eq "windows"} {
#		wm attribute $win -toolwindow yes
	}

	if {[string length $message]} {
		set msg $win.message
		label $msg -text $message
#		message $msg \
#			-text $message -width 280 -anchor w
	}

	set frame $win.frameRadio
	frame $frame \
		-borderwidth 0
	set n 1
    foreach {label value} $choices {
		radiobutton $frame.radio$n -text $label \
			-variable ::NSUtils::Priv(radiobox,radio) -value $value
		pack $frame.radio$n \
			-side top -anchor w
		incr n
	}
	set Priv(radiobox,radio) [lindex $choices 1]

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
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $frame.button$n"

	if {[string length $message]} {
		pack [frame $win.framePad1 -borderwidth 0 -height 10] \
			-side top
		pack $msg \
			-side top -padx 5 -pady 0 -fill x
	}
	pack $win.frameRadio \
		-side top -padx 20 -pady 10 -anchor w
	pack $win.frameButton \
		-side top -padx 5 -pady 0 -anchor n
	pack [frame $win.framePad2 -borderwidth 0 -height 10] \
		-side top

	# <Destroy> handler sets Priv(result)
	bind $win <Destroy> "set NSUtils::Priv(result) 2"

	# Position window
	WindowPosition $win 2 3

	update

	# Set up a grab and claim focus too
	GrabSave $win
	focus $win.frameRadio.radio1

	# Wait for a button press
	set Priv(result) ""
	tkwait variable NSUtils::Priv(result)

	# Release grab and reset focus
	GrabRelease $win
	
	set result ""

	if {[winfo exists $win]} {
		switch $Priv(result) {
			1 {
				set result $Priv(radiobox,radio)
			}
		}
	}

	# Maybe the window is already destroyed
	catch {
		bind $win <Destroy> {}
		destroy $win
	}

	if {$resultVar ne ""} {
		upvar $resultVar resultCode
		set resultCode [expr {$Priv(result) == 1}]
	}

    return $result
}

# NSChooseGame::HandleBackupFile --
#
#	Handle the user opening a backup file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::HandleBackupFile {oop path} {

	if {![string match *.bkp $path]} {
		return $path
	}

	set win [Info $oop win]

	set file [file tail $path]
	set choice1 [format [mc backup-choice1] [string range $file 0 end-4]]
	set choice2 [mc backup-choice2]
	set choice3 [mc backup-choice3]

	set answer [NSUtils::RadioBox -parent $win \
		-buttons [list [mc OK] [mc Cancel]] \
		-title [mc title-backup] -message [format [mc msg-backup] $file] \
		-choices [list $choice1 choice1 $choice2 choice2 $choice3 choice3]]

	switch -- $answer {
		choice1 {
			set pathBkp $path
			set path [string range $path 0 end-4]
			file rename -force $pathBkp $path
		}
		choice2 {
			set pathRename [tk_getSaveFile -parent $win \
				-initialdir [Path lib save] \
				-initialfile [file tail $path]]
			if {$pathRename eq ""} return
			file rename -force $path $pathRename
			set path $pathRename
		}
		choice3 {
		}
		default {
			return
		}
	}

	return $path
}

# NSChooseGame::Open --
#
#	Called when the "Open" button is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::Open {oop} {

	variable Priv

	set index [Info $oop current]
	set pathList [Info $oop pathList]
	set path [lindex $pathList $index]

	set path [HandleBackupFile $oop $path]
	if {$path eq ""} return

	set Priv(recent,path) $path
	Info $oop result open

	return
}

# NSChooseGame::Browse --
#
#	Choose a savefile and play a game.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::Browse {oop} {

	variable Priv

	set win [Info $oop win]

	# Somebody reported that the file list is empty on Windows NT 3.5
	# and suggested I use an initial file name of *.*.
	set types {
		{{All Files} *}
	}
	set fileName [tk_getOpenFile -filetypes $types -parent $win \
		-initialfile *.* \
		-initialdir [Path lib save]]
	if {$fileName eq ""} return
	
	if {[catch {
		set path [LongName $fileName]
	}]} {
		set path $fileName
	}

	set path [HandleBackupFile $oop $path]
	if {$path eq ""} return

	set Priv(recent,path) $path
	Info $oop result browse

	return
}

# NSProjectList::SortBy --
#
#	Sets the list again, using the given sorting method.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::SortBy {oop index} {

	Info $oop sortBy $index
	SetList $oop

	return
}

# NSProjectList::SetList --
#
#	Set the list of savefiles.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::SetList {oop {all 0}} {

	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]
	set savefileInfo [Info $oop savefileInfo]
	set font [Info $oop font]
	set sortBy [Info $oop sortBy]

	# Remember the selection when re-sorting
	set current [Info $oop current]
	if {$current != -1} {
		set pathCurrent [lindex [Info $oop pathList] $current]
	} else {
		set pathCurrent ""
	}

	# Clear the list
	$tree item delete all

	# Underline the column title according to the sorting method
	set frame $win.frameList.header
	foreach i {0 1 2} {
		if {$i == $sortBy} {
			$tree column configure $i -font [FontAddStyle $font underline]
		} else {
			$tree column configure $i -font $font
		}
	}

	set allElems $savefileInfo
	set savefileInfo {}
	foreach elem $allElems {
		lassign $elem name path version other dead
		if {[string match *.bkp $path]} {
			switch -- [Info $oop page] {
				living {
				}
				dead {
				}
				backup {
					lappend savefileInfo $elem
				}
			}
		} elseif {$dead} {
			switch -- [Info $oop page] {
				living {
				}
				dead {
					lappend savefileInfo $elem
				}
				backup {
				}
			}
		} else {
			switch -- [Info $oop page] {
				living {
					lappend savefileInfo $elem
				}
				dead {
				}
				backup {
				}
			}
		}
	}

	if {$all} {
		set savefileInfo [Info $oop savefileInfo]
	}

	# Determine the element index to sort by, and sort them.
	# Sorting by name and version works as expected. Sorting by
	# description ends up sorting by experience level.
	set index [lindex [list 0 2 3] $sortBy]
	set savefileInfo [lsort -dictionary -index $index $savefileInfo]

	# Remember the row of the recently-used file
	set recent -1

	# Remember the row of the previously-selected file
	set current -1

	# Track the row number
	set row 0

	# Remember the file path for each row
	set pathList {}

	# Remember the character name for each row
	set nameList {}

	# Check each savefile
	foreach elem $savefileInfo {

		set name [lindex $elem 0]
		set path [lindex $elem 1]
		set version [lindex $elem 2]
		set other [lindex $elem 3]

		set item [$tree item create]
		$tree item text $item 0 $name 1 $version 2 $other
		$tree item lastchild root $item

		# Remember the row of the recently-used file
		if {$Priv(recent,path) eq $path} {
			$tree item state set $item recent
			set recent $row
		}

		# Remember the row of the previously-selected file
		if {$pathCurrent eq $path} {
			set current $row
		}

		# Remember the file path for each row
		lappend pathList $path

		# Remember the character name for each row
		lappend nameList $name

		# Track the row number
		incr row
	}

	# Remember the file path for each row
	Info $oop pathList $pathList

	# Remember the character name for each row
	Info $oop nameList $nameList

	# Remember the row of the recently-used file
	Info $oop recent $recent

	# Restore the previous selection when re-sorting
	if {$current != -1} {
		$tree activate "root child $current"
		$tree selection modify active all
		$tree see active -center y
	}

	return
}

# NSChooseGame::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	Info $oop page [lindex $Priv(page) $index]

	SetList $oop

	# Select the initial file
	set recent [Info $oop recent]
	if {$recent != -1} {
		set tree [Info $oop tree]
		$tree activate "root child $recent"
		$tree selection modify active all
		$tree see active -center y
	}

	return
}

# NSChooseGame::Typing --
#
#	Highlight a savefile as the user types in a name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::Typing {oop string} {

	if {![string match \[A-Za-z0-9\] $string]} return

	set tree [Info $oop tree]
	set current [Info $oop current]
	set nameList [Info $oop nameList]

	# This starts at row 0 if current is -1
	incr current

	set max [expr {[$tree numitems] - 1}]

	set clickPrev [Info $oop typing,click]
	set clickCur [clock milliseconds]

	# Eh? "clock clicks" goes negative on my Linux box.
	if {[Platform unix] && $clickCur < 0} {
		set clickCur [expr abs($clickCur)]
	}

	if {abs($clickCur - $clickPrev) < 300} {
		set string [Info $oop typing,string]$string
	}
	Info $oop typing,click $clickCur
	Info $oop typing,string $string

	set string [string tolower $string]

	for {set row $current} {$row < $max} {incr row} {
		set name [string tolower [lindex $nameList $row]]
		if {[string match $string* $name]} {
			$tree activate "root child $row"
			$tree selection modify active all
			$tree see active -center y
			return
		}
	}

	for {set row 0} {$row < $current} {incr row} {
		set name [string tolower [lindex $nameList $row]]
		if {[string match $string* $name]} {
			$tree activate "root child $row"
			$tree selection modify active all
			$tree see active -center y
			return
		}
	}

	return
}

# NSChooseGame::Delete --
#
#	Delete the selected savefile if the user agrees.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChooseGame::Delete {oop} {

	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]

	set index [Info $oop current]
	if {$index == -1} return

	set pathList [Info $oop pathList]
	set path [lindex $pathList $index]

	set msg [format [mc choose-delete-msg] [file nativename $path]]
	set answer [tk_messageBox -title [mc choose-delete-title] \
		-message $msg -type yesno -icon warning -parent $win]
	if {$answer eq "no"} return

	# Deleting the "recent" file?
	if {$index == [Info $oop recent]} {
		set Priv(recent,path) ""
		Value savefile {}
	}

	set savefileInfo [Info $oop savefileInfo]
	set n 0
	foreach elem $savefileInfo {
		lassign $elem name path2 version other dead
		if {$path eq $path2} {
			Info $oop savefileInfo [lreplace [Info $oop savefileInfo] $n $n]
			break
		}
		incr n
	}

	file delete $path

	SetList $oop

	return
}
