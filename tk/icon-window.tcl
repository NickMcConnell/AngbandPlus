# File: icon-window.tcl

# Purpose: the Icon Window

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSIconWindow {

	variable MenuString

# namespace eval NSIconWindow
}

# NSIconWindow::InitModule -- 
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::InitModule {} {

	variable Priv

	MsgCatInit startup icon

	NSModule::LoadIfNeeded NSList
	NSModule::LoadIfNeeded NSColorPicker

	set Priv(page) {}
	lappend Priv(page) config
	lappend Priv(page) ascii

	NSObject::New NSIconWindow

	return
}

# NSIconWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::CloseModule {} {

	catch {
		destroy [Window icon]
	}

	return
}

# NSIconWindow::NSIconWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::NSIconWindow {oop} {

	variable Priv

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow icon $win \
		"NSIconWindow::GeometryCmd $oop" \
		"NSIconWindow::SetupCmd $oop" \
		"NSIconWindow::DisplayCmd $oop"

	# Update ourself when the font,asii value changes
	qebind NSIconWindow <Value-font,ascii> \
		"NSIconWindow::ValueChanged_font_ascii $oop"

	qebind NSIconWindow <IconCfg> \
		"NSIconWindow::IconCfg $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSIconWindow $oop $win

	#
	# Global list of application windows
	#

	Global icon,oop $oop
	Window icon $win

	return
}

# NSIconWindow::~NSIconWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::~NSIconWindow {oop} {

	return
}

# NSIconWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::Info {oop info args} {

	global NSIconWindow

	# Verify the object
	NSObject::CheckObject NSIconWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSIconWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSIconWindow($oop,$info)
			}
		}
	}

	return
}

# NSIconWindow::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::InitWindow {oop} {

	variable Priv

	set win .icon$oop
	toplevel $win
	wm title $win [mc title]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSIconWindow::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	frame $win.divider2 \
		-borderwidth 1 -height 2 -relief groove

	#
	# Tabs
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach page $Priv(page) {
		NSTabs::Add $tabsId [mc tab-$page]
	}
	NSTabs::Info $tabsId invokeCmd "NSIconWindow::InvokeTab $oop"
	NSTabs::Info $tabsId active 1

	Info $oop tabsId $tabsId

	#
	# Frame for Configuration page
	#

	set frame $win.frameCfg
	frame $frame \
		-borderwidth 0

	#
	# Icon configuration list
	#

	set tree [NSList::New $frame.frameList]
	$tree configure -width 300

	NSList::OnSelection $tree \
		"NSIconWindow::SelectionChanged $oop %T %c %S %D"

	NSList::OnInvoke $tree \
		"NSIconWindow::Switch $oop"

	Info $oop icon,tree $tree

	#
	# Icon configuration preview image
	#

	set framePreview $frame.framePreview
	frame $framePreview \
		-borderwidth 1 -relief sunken
	canvas $framePreview.canvas \
		-width 300 -height 150 -cursor fleur \
		-yscrollincrement 1 -xscrollincrement 1 \
		-borderwidth 0 -highlightthickness 0 \
		-background Black

	Info $oop icon,canvas $framePreview.canvas

	# Create the preview photo image. The gamma correction is to
	# fix a color problem
	image create photo Image_Preview -gamma 1.1
	bind $framePreview.canvas <Destroy> {+image delete Image_Preview}

	# Create a canvas image item
	$framePreview.canvas create image 0 0 -anchor nw -image Image_Preview \
		-tags image

	# Scroll the preview image when the mouse is dragged
	$framePreview.canvas bind image <ButtonPress-1> "
		set NSIconWindow::Priv(preview,x) %x
		set NSIconWindow::Priv(preview,y) %y
	"
	$framePreview.canvas bind image <Button1-Motion> \
		"NSIconWindow::PreviewMotion $oop %x %y"

	pack $framePreview.canvas -expand yes -fill both

	#
	# Icon configuration "Switch" button
	#

	button $frame.buttonSwitch \
		-text [mc [Global config,prefix]] \
		-command "NSIconWindow::Switch $oop" -state disabled

	Info $oop icon,button $frame.buttonSwitch

	grid rowconfigure $frame 0 -weight 2
	grid rowconfigure $frame 1 -weight 1
	grid rowconfigure $frame 2 -weight 0
	grid columnconfigure $frame 0 -weight 1

	grid $frame.frameList -row 0 -column 0 -sticky news
	grid $frame.framePreview -row 1 -column 0 -sticky news -padx 4 -pady 4
	grid $frame.buttonSwitch -row 2 -column 0 -pady {0 4}

	#
	# Frame for Ascii page
	#

	set frame $win.frameAscii
	frame $frame \
		-borderwidth 0

####

	set frame $frame.frameFont
	labelframe $frame \
		-text [mc ascii-frame-font]
	label $frame.label \
		-text ??? -wrap 400
	button $frame.button \
		-text [mc button-change] -command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font ascii"

	pack $frame.label -side left -anchor w -padx 6 -pady {2 6}
	pack $frame.button -side right -anchor e -padx {0 6} -pady {2 6}
	pack $frame -side top -expand no -fill x -padx 6 -pady 10

	Info $oop ascii,font,label $frame.label
	ValueChanged_font_ascii $oop

####

	set frame $win.frameAscii.frameColor
	labelframe $frame \
		-text [mc ascii-frame-color]
	label $frame.label \
		-text [mc ascii-label-color] -wrap 400
	button $frame.button \
		-text [mc button-change] -command "NSModule::LoadIfNeeded NSColorPreferences ; NSWindowManager::Display color icons"

	pack $frame.label -side left -anchor w -padx 6 -pady {2 6}
	pack $frame.button -side right -anchor e -padx {0 6} -pady {2 6}
	pack $frame -side top -expand no -fill x -padx 6 -pady {0 10}

####

	set frame $win.frameAscii.frameSquare
	labelframe $frame \
		-text [mc ascii-frame-size]
	set ::NSIconWindow($oop,ascii,square) [Value ascii,square]
	checkbutton $frame.cb \
		-text [mc ascii-button-square] \
		-variable NSIconWindow($oop,ascii,square) \
		-command "Value ascii,square \$NSIconWindow($oop,ascii,square)"

	pack $frame.cb -side left -padx {0 6} -pady {2 6}
	pack $frame -side top -expand no -fill x -padx 6 -pady {0 10}

####

	set button [button $win.frameAscii.buttonUpdate \
		-text [mc ascii-button-update] -command "NSIconWindow::UpdateAscii $oop"]

	Info $oop ascii,update,button $button

	# Enable/disable the Update button
	IconCfg $oop

	pack $button -side top

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0
	grid rowconfig $win 1 -weight 0
	grid rowconfig $win 2 -weight 1
	grid rowconfig $win 3 -weight 0
	grid columnconfig $win 0 -weight 1

	grid $win.divider2 \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frameCfg \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	grid $win.frameAscii \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid remove $win.frameAscii

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSIconWindow::Close $oop"

	return
}

# NSIconWindow::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::InitMenus {oop} {

	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSIconWindow::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSIconWindow::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSIconWindow::MenuInvoke $oop"

	#
	# Icons Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_ICONS
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_ICONS -label [mc M_ICONS] -underline 0 -identifier M_ICONS

	set entries {}
#	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_ICONS $entries

	set MenuString(E_CLOSE) \
		"Closes the window."

	NSMenu::BindAccels $mbar $win

	return
}

# NSIconWindow::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::SetupMenus {oop mbarId} {

	set tree [Info $oop icon,tree]

	lappend identList E_CLOSE

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSIconWindow::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -- $ident {
		{} {
			set desc {}
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

# NSIconWindow::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::MenuInvoke {oop menuId ident} {

	set win [Info $oop win]
	set tree [Info $oop icon,tree]

	switch -glob -- $ident {
		E_CLOSE {
			Close $oop
		}
	}

	return
}

# NSIconWindow::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::SetupCmd {oop} {

	set win [Info $oop win]

	set widthMax 0

	SetPage $oop ascii
	update idletasks
	scan [wm geometry $win] {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
	if {$width > $widthMax} {
		set widthMax $width
	}

	SetPage $oop config
	update idletasks
	scan [wm geometry $win] {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
	if {$width > $widthMax} {
		set widthMax $width
	}
\
	# Remember max requested width for each page. See [GeometryCmd].
	Info $oop setup,width $widthMax

	return
}

# NSIconWindow::GeometryCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::GeometryCmd {oop} {

	set win [Info $oop win]

	set geometry [GetDefaultGeometry $win reqwidth main]
	scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
	set width [Info $oop setup,width]

	return ${width}x$height$xs$x$ys$y
}

# NSIconWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::DisplayCmd {oop message first} {

	set tree [Info $oop icon,tree]

	switch -- $message {
		preDisplay {
			if {$first} {
				SetList $oop
				SetPage $oop config

				set row [lsearch -exact $::NSConfig::Priv(prefix) [Global config,prefix]]
				NSList::Activate [Info $oop icon,tree] "root child $row"
			}
		}
		postDisplay {
		}
	}

	return
}

# NSIconWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::Close {oop} {

	NSWindowManager::Undisplay icon

	return
}

# NSIconWindow::SetList --
#
#	Set the list of .prf files.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::SetList {oop} {

	set tree [Info $oop icon,tree]

	# Clear the list
	NSList::Clear $tree

	# Iterate over configurations
	foreach prefix $NSConfig::Priv(prefix) {
		set item [$tree item create]
		NSList::SetText $tree $item [mc $prefix]
		$tree item lastchild root $item
	}

	return
}

# NSIconWindow::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::SelectionChanged {oop tree count select deselect} {

	set selection [$tree selection get]
	if {$count != 1} {
		Image_Preview blank
		Info $oop icon,prefix ""
		[Info $oop icon,button] configure -state disabled \
			-text [mc [Global config,prefix]]
		return
	}

	set item [lindex $selection 0]
	set row [NSList::Item2Row $tree $item]

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

		# Configure the image
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

	Info $oop icon,prefix $prefix

	if {$prefix ne [Global config,prefix]} {
		[Info $oop icon,button] configure -state normal -text [mc $prefix]
	} else {
		[Info $oop icon,button] configure -state disabled -text [mc $prefix]
	}

	return
}


# NSIconWindow::SetPage --
#
#	Display a page.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::SetPage {oop page} {

	variable Priv

	switch -- $page {
		config {
			grid remove [Info $oop win].frameAscii
			grid [Info $oop win].frameCfg
		}
		ascii {
			grid remove [Info $oop win].frameCfg
			grid [Info $oop win].frameAscii
		}
	}

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $Priv(page) $page]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}


# NSIconWindow::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetPage $oop [lindex $Priv(page) $index]

	return
}

# NSIconWindow::PreviewMotion --
#
#	Handles <Button1-Motion> events in the preview image
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::PreviewMotion {oop x y} {

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
	if {[winfo width $canvas] < [image width Image_Preview]} {
		$canvas xview scroll [expr -$dx] units
	}
	if {[winfo height $canvas] < [image height Image_Preview]} {
		$canvas yview scroll [expr -$dy] units
	}

	# Remember the current pointer position
	if {$dx} {
		set Priv(preview,x) $x
	}
	if {$dy} {
		set Priv(preview,y) $y
	}

	return
}

# NSIconWindow::Switch --
#
#	Change the game's icon configuration
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::Switch {oop} {

	NSConfig::SwitchConfig [Info $oop icon,prefix]

	[Info $oop icon,button] configure -state disabled \
		-text [mc [Global config,prefix]]

	return
}

# NSIconWindow::ValueChanged_font_ascii --
#
#	Called when the font,knowledge value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::ValueChanged_font_ascii {oop} {

	set font [Value font,ascii]

	array set actual [font actual $font]
	set fontName "$actual(-family) $actual(-size)"
	if {$actual(-weight) eq "bold"} {
		append fontName " bold"
	}
	if {$actual(-slant) eq "italic"} {
		append fontName " italic"
	}

	[Info $oop ascii,font,label] configure -text $fontName

	return
}

# NSIconWindow::UpdateAscii --
#
#	Update the current "ascii" icon configuration.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::UpdateAscii {oop} {

	NSConfig::SwitchConfig [Global config,prefix] 1

	return
}

# NSIconWindow::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSIconWindow::IconCfg {oop} {

	set hasAscii 0
	foreach type [icon gettypes] {
		if {[icon ascii isascii $type]} {
			set hasAscii 1
			break
		}
	}
	if {$hasAscii} {
		[Info $oop ascii,update,button] configure -state normal
	} else {
		[Info $oop ascii,update,button] configure -state disabled
	}

	return
}
