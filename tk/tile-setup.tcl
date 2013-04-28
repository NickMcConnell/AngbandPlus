# File: setup.tcl

# Purpose: the Setup Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSSetup {

# namespace eval NSSetup
}

# NSSetup::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::InitModule {} {

	variable Priv

	MsgCatInit startup

	NSModule::LoadIfNeeded NSTabbedFrame

	# Get a list of sound libraries
	if {[Platform unix]} {
		set libs [list bass-stream openal sdl]
	}
	if {[Platform windows]} {
		set libs [list bass bass-stream directsound fmod openal wavemix]
	}
	set Priv(sound-libs) {}
	set Priv(has-sound-libs) 0
	foreach lib $libs {
		set shlib sound-$lib[info sharedlibextension]
		if {[file exists [CPath lib $shlib]]} {
			lappend Priv(sound-libs) $lib
			set Priv(has-sound-libs) 1
		}
	}

	# Get a list of music libraries
	if {[Platform unix]} {
		set libs {}
	}
	if {[Platform windows]} {
		set libs [list bass fmod midas]
	}
	set Priv(music-libs) {}
	set Priv(has-music-libs) 0
	foreach lib $libs {
		set shlib music-$lib[info sharedlibextension]
		if {[file exists [CPath lib $shlib]]} {
			lappend Priv(music-libs) $lib
			set Priv(has-music-libs) 1
		}
	}

	# Create list of installed variants
	set glob [glob -nocomplain -types d -directory [CPath variant] *]
	set glob [lsort -dictionary $glob]
	set Priv(variant,name) {}
	set Priv(variant,shlib) {}
	foreach path $glob {
		set shlib [file join $path angband[info sharedlibextension]]
		if {[file exists $shlib]} {
			lappend Priv(variant,name) [file tail $path]
			lappend Priv(variant,shlib) $shlib
		}
	}

	set Priv(variant,sel) -1
	set path [CPathTk config variant]
	if {[file exists $path]} {
		set chan [open $path]
		set string [string trim [gets $chan]]
		close $chan
		set i [lsearch -exact $Priv(variant,name) $string]
		if {$i == -1} {
			set Priv(variant,always) 0
		} else {
			set Priv(variant,always) 1
			set Priv(variant,sel) $i
		}
	} else {
		set Priv(variant,always) 0
	}

	set Priv(page) {}
	lappend Priv(page) Icon
	lappend Priv(page) Music
	lappend Priv(page) Sound
	if {[llength $Priv(variant,name)]} {
		lappend Priv(page) Variant
	}

	NSObject::New NSSetup

	return
}

# NSSetup::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::CloseModule {} {

	variable Priv

	catch {destroy $Priv(win)}

	return
}

# NSSetup::NSSetup --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::NSSetup {oop} {

	variable Priv

	set Priv(icon,prefix) [Global config,prefix]
	set Priv(music,wants) [Value music,wants]
	set Priv(music,dll) [Value music,dll]
	set Priv(sound,wants) [Value sound,wants]
	set Priv(sound,dll) [Value sound,dll]

	InitWindow $oop
	set win [Info $oop win]

	# Destroy the object along with the toplevel
	NSUtils::DestroyObjectWithWidget NSSetup $oop $win

	set Priv(win) $win

	WindowPosition $win 2 3
	focus $win

	# Set the preview image
	set Priv(preview,row) -1
	Preview $oop

	ModalLoop $oop

	return
}

# NSSetup::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::Info {oop info args} {

	global NSSetup

	# Verify the object
	NSObject::CheckObject NSSetup $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSSetup($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSSetup($oop,$info)
			}
		}
	}

	return
}

# NSSetup::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::InitWindow {oop} {

	variable Priv

	set win .setup$oop
	toplevel $win
	wm title $win [mc setup-title]

	wm transient $win .
#	wm withdraw $win
	wm resizable $win no no

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSSetup::Close $oop"

	Info $oop win $win

	# Notebook
	set nb [notebook $win.notebook]
	Info $oop notebook $nb
	foreach page $Priv(page) {
		$nb add [InitPage_$page $oop] -text [mc setup-$page]
	}
	pack $nb -side top -padx 6 -pady 6 -expand yes -fill both

	set frame $win.frameButtons
	frame $frame \
		-borderwidth 0
	button $frame.buttonOK \
		-text [mc OK] -width 9 -command "set NSSetup::Result ok" \
		-default active
	button $frame.buttonCancel \
		-text [mc Cancel] -width 9 -command "set NSSetup::Result cancel"

	NSUtils::CheckButtonWidth $frame.buttonOK
	NSUtils::CheckButtonWidth $frame.buttonCancel

	if {0 && [Platform windows]} {
		$frame.buttonOK configure -pady 0
		$frame.buttonCancel configure -pady 0 
	}

	NSUtils::SetDefaultButton $win $frame.buttonOK
	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $win.frameButtons.buttonCancel"

	switch -- [Platform] {
		windows {
			set padx 6
		}
		unix {
			set padx 6
		}
	}

	pack $win.frameButtons \
		-side top -padx $padx -pady 0 -anchor e
	pack $win.frameButtons.buttonCancel \
		-side right -padx 0 -pady 0
	pack $win.frameButtons.buttonOK \
		-side right -padx $padx -pady 0
	pack [frame $win.pad -borderwidth 0 -height 6] \
		-side top -fill x

	return
}

# NSSetup::Close --
#
#	Called by WM_DELETE_WINDOW protocol handler.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::Close {oop} {

	destroy [Info $oop win]

	return
}

# NSSetup::ModalLoop --
#
#	Loop until done.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::ModalLoop {oop} {

	variable Priv
	variable Result

	set win [Info $oop win]

	# Set up a grab and claim focus too
	NSUtils::GrabSave $win
	focus $win

	# Wait for a button press
	set Result cancel
	tkwait variable NSSetup::Result

	# Release grab and reset focus
	NSUtils::GrabRelease $win

	if {$Result eq "ok"} {

		NSConfig::SetPrefix $Priv(icon,prefix)

		Value music,wants $Priv(music,wants)
		Value music,dll $Priv(music,dll)

		Value sound,wants $Priv(sound,wants)
		Value sound,dll $Priv(sound,dll)

		set path [CPathTk config variant]
		if {$Priv(variant,always)} {
			set row [$Priv(variant,listbox) curselection]
			set name [lindex $Priv(variant,name) $row]
			set chan [open $path w]
			puts $chan $name
			close $chan
		} elseif {[file exists $path]} {
			file delete $path
		}
	}

	destroy $win

	return
}

# NSSetup::SetPage --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::SetPage {oop page} {

	variable Priv

	set nb [Info $oop notebook]
	$nb select [lsearch -exact $Priv(page) $page]

	return
}

# NSSetup::InitPage_Icon --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::InitPage_Icon {oop} {

	variable Priv

	set content [Info $oop notebook].icon

	frame $content \
		-borderwidth 0

	set frameListOuter $content.frameListOuter
	frame $frameListOuter \
		-borderwidth 0
	label $frameListOuter.label \
		-text [mc icons-prompt]
	set frameList $frameListOuter.frameList
	frame $frameList \
		-borderwidth 1 -relief sunken
	listbox $frameList.list \
		-height 5 -width 35 -background White -borderwidth 0 \
		-yscrollcommand "$frameList.yscroll set" -highlightthickness 0
	scrollbar $frameList.yscroll \
		-command "$frameList.list yview"

	# Double-click selects an item
	bind $frameList.list <Double-ButtonPress-1> {
		set NSSetup::Result ok
	}

	set framePreview $content.framePreview
	frame $framePreview \
		-borderwidth 1 -relief sunken
	set width [expr {[winfo reqwidth $frameList.list] + \
		[winfo reqwidth $frameList.yscroll]}]
	canvas $framePreview.canvas \
		-width $width -height 150 -cursor fleur \
		-yscrollincrement 1 -xscrollincrement 1 \
		-borderwidth 0 -highlightthickness 0

	pack $framePreview.canvas

	# Create the preview photo image. The gamma correction is to
	# fix a color problem
	image create photo Image_Preview -gamma 1.1
	bind $framePreview.canvas <Destroy> {+image delete Image_Preview}

	# Create a canvas image item
	$framePreview.canvas create image 0 0 -anchor nw -image Image_Preview \
		-tags image

	# Scroll the preview image when the mouse is dragged
	$framePreview.canvas bind image <ButtonPress-1> "
		set NSSetup::Priv(preview,x) %x
		set NSSetup::Priv(preview,y) %y
	"
	$framePreview.canvas bind image <Button1-Motion> \
		"NSSetup::PreviewMotion $oop %x %y"

	pack $framePreview \
		-side top -pady {8 0}
	pack $frameListOuter \
		-side top -pady {0 8}
	pack $frameListOuter.label \
		-side top -anchor w
	pack $frameList \
		-side top
	pack $frameList.list \
		-side left -fill both
	pack $frameList.yscroll \
		-side right -fill y

	# When a list item is selected, show a preview image
	bind $frameList.list <<ListboxSelect>> \
		"NSSetup::Preview $oop"

	Info $oop icon,listbox $frameList.list
	Info $oop icon,canvas $framePreview.canvas

	pack $content -expand yes -fill both

	# Set the list
	set listbox [Info $oop icon,listbox]
	foreach prefix $NSConfig::Priv(prefix) {
		$listbox insert end [mc $prefix]
		lappend prefixList $prefix
	}

	# Select the current icon set
	set row [lsearch -exact $prefixList $Priv(icon,prefix)]
	if {$row != -1} {
		$listbox selection set $row
		$listbox see $row
	}

	return $content
}

# NSSetup::Preview --
#
#	If the list selection changed, looks for a preview image for the
#	selected icon configuration and sets the preview image to it.
#	Otherwise the preview image is blanked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::Preview {oop} {

	global Angband
	variable Priv

	# Get the list
	set list [Info $oop icon,listbox]

	# Get the selected row
	set row [lindex [$list curselection] 0]
	if {![string length $row]} return

	# If this is the same row as before, do nothing.
	# This is needed for double-clicking
	if {$row == $Priv(preview,row)} return

	# Remember the selected row
	set Priv(preview,row) $row

	# Get the configuration prefix for this row
	set prefix [lindex $NSConfig::Priv(prefix) $row]

	# Get the pathname of an appropriate image file
	set path [CPathTk image preview-$prefix.gif]

	# The image file exists
	if {[file exists $path]} {

		# Get the canvas
		set canvas [Info $oop icon,canvas]

		# Remember the cursor
		set cursor [$canvas cget -cursor]

		# Clear the cursor (so the stop watch will show)
		$canvas configure -cursor {}

		# Get busy
		[Info $oop win] configure -cursor watch
		update

		# Configure the image. Too slow!
		Image_Preview configure -file $path

		# Get the image dimensions
		set imageWidth [image width Image_Preview]
		set imageHeight [image height Image_Preview]

		# Set the scroll region
		$canvas configure -scrollregion [list 0 0 $imageWidth $imageHeight]

		# Get the canvas dimensions
		set canvasWidth [winfo width $canvas]
		set canvasHeight [winfo height $canvas]

		# Scroll the canvas so the image is centered
		set x [expr {($imageWidth - $canvasWidth) / 2.0 / $imageWidth}]
		$canvas xview moveto $x
		set y [expr {($imageHeight - $canvasHeight) / 2.0 / $imageHeight}]
		$canvas yview moveto $y

		# Not busy
		[Info $oop win] configure -cursor {}
		$canvas configure -cursor $cursor

	# No appropriate image file exists
	} else {

		# Blank the image
		Image_Preview blank
	}

	set Priv(icon,prefix) $prefix

	return
}

# NSSetup::PreviewMotion --
#
#	Handles <Button1-Motion> events in the preview image
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::PreviewMotion {oop x y} {

	variable Priv

	# Get the canvas
	set canvas [Info $oop icon,canvas]

	# Calculate the distance the pointer moved
	set dx [expr {$x - $Priv(preview,x)}]
	set dy [expr {$y - $Priv(preview,y)}]

	# Require minimum movement
	if {abs($dx) < 5} {
		set dx 0
	}
	if {abs($dy) < 5} {
		set dy 0
	}

	# Scroll the canvas
	$canvas xview scroll [expr -$dx] units
	$canvas yview scroll [expr -$dy] units

	# Remember the current pointer position
	if {$dx} {
		set Priv(preview,x) $x
	}
	if {$dy} {
		set Priv(preview,y) $y
	}

	return
}

# NSSetup::InitPage_Music --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::InitPage_Music {oop} {

	variable Priv

	set content [Info $oop notebook].music

	frame $content \
		-borderwidth 0

	message $content.labelMusic \
		-text [mc music-prompt] -width 300

	checkbutton $content.checkUseMusic \
		-text [mc music-use] -variable ::NSSetup::Priv(music,wants) \
		-command "NSSetup::Synch_Music $oop"
	ttk::labelframe $content.lframe \
		-labelwidget $content.checkUseMusic

	pack $content.labelMusic \
		-side top -anchor w
	pack $content.lframe \
		-side top -expand yes -fill both -padx 8 -pady {0 8}

	if {$Priv(has-music-libs)} {

		label $content.lframe.label1 \
			-text [mc music-lib]
		pack $content.lframe.label1 \
			-side top -anchor w -padx 10

		foreach lib $Priv(music-libs) {

			radiobutton $content.lframe.radio#$lib \
				-text [mc music-use-$lib] -variable ::NSSetup::Priv(music,dll) \
				-value $lib -command "$content.lframe.msgDescrip configure \
					-text \"[mc music-prompt-$lib]\""

			pack $content.lframe.radio#$lib \
				-side top -anchor w -padx 20

			set Priv(music,radio,$lib) $content.lframe.radio#$lib
		}

		label $content.lframe.label2 \
			-text [mc music-desc]
		message $content.lframe.msgDescrip \
			-width 250 -anchor w \
			-text [mc music-prompt-[lindex $Priv(music-libs) 0]]
		pack $content.lframe.label2 \
			-side top -anchor w -padx 10
		pack $content.lframe.msgDescrip \
			-side top -anchor w -padx 20

	} else {

		set msg $content.lframe.msgNone
		message $msg -width 200 -text [mc music-no-libs]
		pack $msg -side top -expand yes -fill both

		set Priv(music,wants) 0
		$content.checkUseMusic configure -state disabled
	}

	if 0 {
		set content2 [NSTitleFrame::Info $titleFrameId content]
		label $content2.labelDir -text "Music Directory:"
		entry $content2.entryDir
		button $content2.buttonBrowse -text "Browse..." -width 9
		pack $content2.labelDir -side top -anchor w -padx 10
		pack $content2.entryDir -side top -fill both -padx 10
		pack $content2.buttonBrowse -side top -anchor e -padx 10 -pady 5

		if {[llength [Value music,directory]]} {
			set path [file join {*}[Value music,directory]]
			catch {set path [LongName $path]}
			$content2.entryDir insert 0 [file nativename $path]
#		$content2.entryDir selection range 0 end
		}
	}

	pack $content -expand yes -fill both

	Synch_Music $oop

	return $content
}

# NSSetup::Synch_Music --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::Synch_Music {oop} {

	variable Priv

	if {$Priv(music,wants)} {
		set state normal
	} else {
		set state disabled
	}

	foreach lib $Priv(music-libs) {
		$Priv(music,radio,$lib) configure -state $state
	}

	return
}

# NSSetup::InitPage_Sound --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::InitPage_Sound {oop} {

	variable Priv

	set content [Info $oop notebook].sound

	frame $content \
		-borderwidth 0

	message $content.labelSound \
		-text [mc sound-prompt] -width 300

	checkbutton $content.checkUseSound \
		-text [mc sound-use] -variable ::NSSetup::Priv(sound,wants) \
		-command "NSSetup::Synch_Sound $oop"
	ttk::labelframe $content.lframe \
		-labelwidget $content.checkUseSound

	pack $content.labelSound \
		-side top -anchor w

	if {$Priv(has-sound-libs)} {

		label $content.lframe.label1 \
			-text [mc sound-lib]
		pack $content.lframe.label1 \
			-side top -anchor w -padx 10

		foreach lib $Priv(sound-libs) {

			radiobutton $content.lframe.radio#$lib \
				-text [mc sound-use-$lib] -variable NSSetup::Priv(sound,dll) \
				-value $lib -command "$content.lframe.msgDescrip configure \
					-text \"[mc sound-prompt-$lib]\""

			pack $content.lframe.radio#$lib \
				-side top -anchor w -padx 20

			set Priv(sound,radio,$lib) $content.lframe.radio#$lib
		}

		label $content.lframe.label2 \
			-text [mc sound-desc]
		message $content.lframe.msgDescrip \
			-width 250 -anchor w \
			-text [mc sound-prompt-$Priv(sound,dll)]
		pack $content.lframe.label2 \
			-side top -anchor w -padx 10
		pack $content.lframe.msgDescrip \
			-side top -anchor w -padx 20

	# No sound libraries found
	} else {
		set msg $content.lframe.msgNone
		message $msg -width 200 -text [mc sound-no-libs]
		pack $msg -side top -expand yes -fill both

		set Priv(sound,wants) 0
		$content.checkUseSound configure -state disabled
	}

	pack $content.lframe \
		-side top -expand yes -fill both -padx 8 -pady {0 8}

	Synch_Sound $oop

	return $content
}

# NSSetup::Synch_Sound --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::Synch_Sound {oop} {

	variable Priv

	if {$Priv(sound,wants)} {
		set state normal
	} else {
		set state disabled
	}

	foreach lib $Priv(sound-libs) {
		$Priv(sound,radio,$lib) configure -state $state
	}

	return
}

# NSSetup::InitPage_Variant --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSetup::InitPage_Variant {oop} {

	variable Priv

	set content [Info $oop notebook].variant

	frame $content \
		-borderwidth 0

	set frame $content.frameList
	frame $frame -borderwidth 1 -relief sunken
	listbox $frame.listbox -width 35 -height 6 \
		-yscrollcommand "$frame.yscroll set" \
		-listvariable ::NSSetup::Priv(variant,name) \
		-borderwidth 0 -highlightthickness 0 -background White \
		-exportselection no
	scrollbar $frame.yscroll -orient vertical -command "$frame.listbox yview"
	pack $frame.listbox $frame.yscroll -side left -fill y

	checkbutton $content.check -text [mc variant-always] \
		-variable ::NSSetup::Priv(variant,always)

	pack $frame $content.check -side top -pady {8 0}

	set Priv(variant,listbox) $frame.listbox

	pack $content -expand yes -fill both

	if {$Priv(variant,sel) != -1} {
		$frame.listbox selection set $Priv(variant,sel)
	}

	return $content
}
