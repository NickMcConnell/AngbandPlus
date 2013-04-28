# File: help-html.tcl

# Purpose: the Help Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSHelp {

	variable Image

	variable MenuString
	variable Priv

# namespace eval NSHelp
}

# NSHelp::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::InitModule {} {

	variable Image
	variable Priv

	MsgCatInit help

	package require Tkhtml [TKHTMLVERSION]

	NSModule::LoadIfNeeded NSHelpList
	NSModule::LoadIfNeeded NSWin98ToolbarButton

	InitImageIfNeeded Image_Hide ms-hide.gif
	InitImageIfNeeded Image_Show ms-show.gif

	InitImageIfNeeded Image_Back ms-back.gif
	InitImageIfNeeded Image_Next ms-next.gif
	InitImageIfNeeded Image_Refresh ms-refresh.gif

	InitImageIfNeeded Image_HideGray ms-hide.gif
	Image_HideGray configure -palette 32

	InitImageIfNeeded Image_ShowGray ms-show.gif
	Image_ShowGray configure -palette 32

	InitImageIfNeeded Image_BackGray ms-back.gif
	Image_BackGray configure -palette 32
	
	InitImageIfNeeded Image_NextGray ms-next.gif
	Image_NextGray configure -palette 32

	InitImageIfNeeded Image_RefreshGray ms-refresh.gif
	Image_RefreshGray configure -palette 32

	if {![ImageExists Image_BackDisabled]} {
		image create photo Image_BackDisabled
		DisableImage Image_Back Image_BackDisabled
	}
	
	if {![ImageExists Image_NextDisabled]} {
		image create photo Image_NextDisabled
		DisableImage Image_Next Image_NextDisabled
	}
	
	if {![ImageExists Image_RefreshDisabled]} {
		image create photo Image_RefreshDisabled
		DisableImage Image_Refresh Image_RefreshDisabled
	}

	set Image(nextId) 0
	set Image(names) {}

	set Priv(find,string) ""
	set Priv(find,index) 1.0

	set Priv(history,index) -1
	set Priv(history,links,label) {}
	set Priv(history,links,path) {}
	set Priv(history,links,title) {}
	set Priv(history,yview) {}

	if {[variant ZANGBANDTK]} {
		ZHelp::ReadHelpIndex
	}

	NSObject::New NSHelp

	return
}

# NSHelp::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::CloseModule {} {

	catch {
		destroy [Window help]
	}

	return
}

# NSHelp::NSHelp --
#
#	Create a help window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHelp::NSHelp {oop} {

	Info $oop busy 0
	Info $oop curFile ""
	Info $oop inPageCmd 0

	InitWindow $oop

	set win [Info $oop win]
	set listId [Info $oop listId]

if 0 {
	# Add a ReadMe book with readme files
	set bookId [NSHelpList::BookAdd $listId 0 "ReadMe Files" {}]
	set globList [concat \
		[glob -nocomplain -directory [Path ReadMe] *] \
		[glob -nocomplain -directory [CPath ReadMe] *]]
	foreach path [lsort -dictionary $globList] {
		NSHelpList::PageAdd $listId $bookId [file tail $path] $path
	}
}

	ReadContents $oop [PathTk doc contents]

	# See if TANG is installed
	set path [CPathTk doc TANG.html]
	if {[file exists $path] && [file isfile $path]} {
		ReadContents $oop [CPathTk doc contents-TANG]
	}

	# See if the Variant FAQ is installed
	set path [CPathTk doc variant-faq.txt]
	if {[file exists $path] && [file isfile $path]} {
		ReadContents $oop [CPathTk doc contents-FAQ]
	}

	# See if the ZAngband Knowledge Base is installed
	if {0 && [variant ZANGBANDTK]} {
		set path [Path ZKB]
		if {[file exists $path] && [file isdirectory $path]} {
			ReadContents $oop [PathTk doc contents-zkb]
		} else {
			NSHelpList::PageAdd $listId 0 "ZAngband Knowledge Base" zkb.html
		}
	}

	variable Priv
	set Priv(html,@contents) "<TITLE>Table of Contents</TITLE>"
	append Priv(html,@contents) "<H1>Table of Contents</H1>"
	append Priv(html,@contents) [NSHelpList::ToHtml $listId]

	# Mega-Hack -- Insert after converting the contents list to
	# HTML so it doesn't appear in the HTML.
	NSHelpList::PageInsert $listId 0 0 "Table of Contents" @contents

	NSHelpList::SetList $listId

	DisplayPageByUrl $oop @contents ""

if 0 {
	# Convert the contents list to a hyperlinked Table of Contents
	set path [PathTk doc contents.html]
		if {![file exist $path] || ([file size $path] <= 1)} {
		set fileId [openlf $path]
		puts $fileId "<HTML><HEAD><TITLE>Table of Contents</TITLE></HEAD><BODY>\n"
		puts $fileId [NSHelpList::ToHtml $listId]
		puts $fileId "</BODY></HTML>"
		close $fileId
	}

	DisplayPageByUrl $oop $path ""
}

	NSWindowManager::RegisterWindow help $win \
		"NSHelp::GeometryCmd $oop" "" "NSHelp::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSHelp $oop $win

	#
	# Global list of application windows
	#

	Global help,oop $oop
	Window help $win

	return
}

# NSHelp::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Info {oop info args} {

	global NSHelp

	# Verify the object
	NSObject::CheckObject NSHelp $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSHelp($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSHelp($oop,$info)
			}
		}
	}

	return
}

# NSHelp::InitWindow --
#
#	Create a help window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHelp::InitWindow {oop} {

	variable Priv

	set win .help$oop
	toplevel $win
	wm title $win Help

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win
 
	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSHelp::Close $oop"

	# Set instance variables
	Info $oop win $win

	# Create menus
	InitMenus $oop

	#
	# Navigation buttons
	#

	set frame $win.toolbar
	frame $frame \
		-borderwidth 2 -relief groove
#	frame $frame.divider1 \
#		-borderwidth 1 -relief groove -height 2
	set showImage yes
	set showLabel yes
	win98button $frame.contents \
		-label [mc Hide] -command "NSHelp::ToggleContents $oop" \
		-imageactive Image_Hide \
		-imagenormal Image_HideGray \
		-showimage $showImage -showlabel $showLabel
	win98button $frame.back \
		-label [mc Back] -command "NSHelp::Back $oop 1" \
		-imageactive Image_Back \
		-imagenormal Image_BackGray \
		-imagedisabled Image_BackDisabled \
		-showimage $showImage -showlabel $showLabel \
		-hasmenu yes -menucommand "NSHelp::Win98MenuCmd_Back $oop"
	win98button $frame.next \
		-label [mc Next] -command "NSHelp::Next $oop 1" \
		-imageactive Image_Next \
		-imagenormal Image_NextGray \
		-imagedisabled Image_NextDisabled \
		-showimage $showImage -showlabel $showLabel \
		-hasmenu yes -menucommand "NSHelp::Win98MenuCmd_Next $oop"
	win98button $frame.reload \
		-label [mc Refresh] -command "NSHelp::Reload $oop" \
		-imageactive Image_Refresh \
		-imagenormal Image_RefreshGray \
		-imagedisabled Image_RefreshDisabled \
		-showimage $showImage -showlabel $showLabel

	set menuId [NSObject::New NSMenu [Info $oop mbarId] -postcommand ";" \
		-tearoff 0 -identifier MENU_TOOLBAR]
	Info $oop toolbar,menu [NSMenu::Info $menuId menu]

	Info $oop button,contents $frame.contents
	Info $oop button,back $frame.back
	Info $oop button,next $frame.next
	Info $oop button,reload $frame.reload

#	pack $frame.divider1 \
#		-side top -fill x -pady 2
	pack $frame.contents \
		-side left -padx 0 -pady 0
	pack [MakeDivider $frame.divider2 y] \
		-side left -padx 0 -pady 2 -fill y
	pack $frame.back \
		-side left -padx 0 -pady 0
	pack $frame.next \
		-side left -padx 2 -pady 0
	pack $frame.reload \
		-side left -padx 0 -pady 0

#	frame $win.divider1 \
#		-borderwidth 1 -relief groove -height 2

	if {$::UseTile} {
		ttk::panedwindow $win.splitterH -orient horizontal
	} else {
		panedwindow $win.splitterH -orient horizontal -opaqueresize true -sashrelief sunken
	}

	#
	# Contents list
	#

	set frame $win.frameList
	frame $frame \
		-borderwidth 0
	frame $frame.pad \
		-borderwidth 0
	frame $frame.frameOuter \
		-borderwidth 2 -relief raised
	frame $frame.frameOuter.frameInner \
		-borderwidth 2 -relief sunken

if 0 {
	set tabsId [NSObject::New NSTabs $frame]
	NSTabs::Add $tabsId [mc Contents]
	NSTabs::Bigger $tabsId [NSTabs::GetNthId $tabsId 0]
	set canvasTabs [NSTabs::Info $tabsId canvas]
	set height [winfo reqheight $canvasTabs]
	$frame.pad configure -height $height
}
	set frame2 $frame.frameOuter.frameInner
	set listId [NSObject::New NSHelpList $frame2]
	NSHelpList::Info $listId pageCmd "NSHelp::PageCmd $oop"
	set canvistId [NSHelpList::Info $listId canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	Info $oop listId $listId

	pack [NSHelpList::Info $listId frame] -expand yes -fill both
	pack $frame.pad -side top -fill x
	pack $frame.frameOuter.frameInner -expand yes -fill both -padx 6 -pady 6
	pack $frame.frameOuter -expand yes -fill both -padx {6 0}
if 0 {
	place $canvasTabs -in $frame.frameOuter -bordermode outside \
		-x 0 -y [expr {0 - $height + 2}]
	raise [NSTabs::Info $tabsId canvas]
}
	#
	# Html and scroll bars
	#

	set frameHtml $win.frameHtml
	frame $frameHtml \
		-relief sunken -borderwidth 2
	set whtml $frameHtml.html

	scrollbar $frameHtml.yscroll \
		-orient vertical -command "$whtml yview"

	scrollbar $frameHtml.xscroll \
		-orient horizontal -command "$whtml xview"

if {[TKHTMLVERSION 1]} {
	# Note: TkHTML has -marginwidth and -marginheight. -marginwidth
	# is extra space added to left/right sides, -marginheight is
	# extra space added to top only. Both options are automatically
	# reset to 9 when "$whtml clear" is called. I want -padx 4, so
	# I set -marginwidth/height to zero every time. These options are
	# used to handle the <body> tag in HTML.

	html $whtml \
		-yscrollcommand "$frameHtml.yscroll set" \
		-xscrollcommand "$frameHtml.xscroll set" \
		-width 100 -height 100 -borderwidth 0 -takefocus 1 \
		-highlightthickness 0 -padx 4 -pady 4 \
		-background White -foreground Black \
		-hyperlinkcommand "NSHelp::HyperlinkCmd $oop"

	$whtml token handler META "NSHelp::Token_META $oop"
}
if {[TKHTMLVERSION 3]} {
	html $whtml \
		-yscrollcommand "$frameHtml.yscroll set" \
		-xscrollcommand "$frameHtml.xscroll set" \
		-width 100 -height 100

	$whtml handler node a "NSHelp::Handler_node_a $oop"
	$whtml handler attribute a "NSHelp::Handler_attr_a $oop"
}

	Info $oop whtml $whtml

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20
	
	# Add some KeyPress bindings to the widget
	bind $win <KeyPress-Home> \
		"$whtml yview moveto 0"
	bind $win <KeyPress-End> \
		"$whtml yview moveto 1"
	bind $win <KeyPress-Prior> \
		"$whtml yview scroll -1 pages"
	bind $win <KeyPress-Next> \
		"$whtml yview scroll +1 pages"
	bind $win <KeyPress-Up> \
		"$whtml yview scroll -1 units"
	bind $win <KeyPress-Down> \
		"$whtml yview scroll +1 units"

if {[TKHTMLVERSION 1]} {
	if {[info tclversion] >= 8.1} {
		bind $whtml.x <ButtonPress-1> \
			"NSHelp::Button1 $oop %x %y"
		bind $whtml.x <Motion> \
			"NSHelp::Motion1 $oop %x %y"
		$whtml configure -fontcommand "NSHelp::FontCmd $oop"
		$whtml configure -imagecommand "NSHelp::ImageCmd $oop"
	}

	if {[info tclversion] == 8.0} {
		# Set some sensible font faces and sizes
		$whtml configure -fontfamily {Times %d}
		$whtml configure -fontsize {8 10 12 14 18 24 24}
		
		$whtml configure -cwfontfamily {Courier %d}
		$whtml configure -cwfontsize {6 8 10 12 16 22 22}
		
		$whtml configure -boldfontfamily {Times %d bold}
		$whtml configure -boldfontsize {8 10 12 14 18 24 24}
		
		$whtml configure -italicfontfamily {Times %d italic}
		$whtml configure -italicfontsize {8 10 12 14 18 24 24}

		$whtml configure -tablecellspacing 2
	}
}
if {[TKHTMLVERSION 3]} {
		bind $whtml <ButtonPress-1> \
			"NSHelp::Button1 $oop %x %y"
		bind $whtml <Motion> \
			"NSHelp::Motion1 $oop %x %y"
		bind $whtml <MouseWheel> {
			%W yview scroll [expr {- (%D / 120) * 4}] units
		}
}
	$win.splitterH add $win.frameList
	$win.splitterH add $win.frameHtml

	grid rowconfig $frameHtml 0 -weight 1 -minsize 0
	grid columnconfig $frameHtml 0 -weight 1 -minsize 0
	grid $whtml \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frameHtml.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $frameHtml.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid rowconfigure $win 2 -weight 0
#	grid rowconfigure $win 3 -weight 0
if 1 {
	grid columnconfigure $win 0 -weight 1
} else {
	grid columnconfigure $win 0 -weight 0
	grid columnconfigure $win 1 -weight 1 -minsize 0
}
	grid $win.toolbar \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
#	grid $win.divider1 \
#		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew -pady 2
if 1 {
	grid $win.splitterH \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
} else {
	grid $win.frameList \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news -padx 2
	grid $frameHtml \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky news
}
	grid $win.statusBar \
		-row 2 -column 0 -rowspan 1 -columnspan 2 -sticky ew

#	NSUtils::SetDefaultButton $win $win.toolbar.reload

#	bind $win <KeyPress-Return> \
#		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> "NSHelp::Close $oop"
	bind $win <Control-KeyPress-w> "NSHelp::Close $oop"
	bind $win <KeyPress-f> "NSHelp::Find $oop 0"
	bind $win <KeyPress-g> "NSHelp::Find $oop 1"

	return
}

# NSHelp::InitMenus --
#
#	Create the menus for a new editor window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#

proc NSHelp::InitMenus {oop} {

	global NSMenu
	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSHelp::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSHelp::MenuSelect $oop"

	#
	# Help Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_HELP
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_HELP -label [mc Help] -underline 0 -identifier M_HELP

	lappend entries [list -type command -label "Hide/Show Contents" \
		-command "NSHelp::ToggleContents $oop" \
		-underline 0 -identifier E_CONTENTS]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Find..."] \
		-command "NSHelp::Find $oop 0" \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-command "NSHelp::Find $oop 1" \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Close"] \
		-command "NSHelp::Close $oop" -underline 0 \
		-accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_HELP $entries

if 0 {
	#
	# Section Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SECTION
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_SECTION -label "Section" -underline 0 \
		-identifier M_SECTION

	set identCnt 0
	set entries {}

	set data [list \
		"Table Of Contents" index.html \
		"-" "" \
		"About AngbandTk" about.html \
		"Bugs and Limitations" bugs.html \
		"Changes" changes.html \
		"The Interface" interface.html
	]
	if {[variant ANGBANDTK KANGBANDTK]} {
		lappend data \
			"-" "" \
			"General Information" general.html \
			"Creating A Character" birth.html \
			"Exploring The Dungeon" dungeon.html \
			"Attacking Monsters" attack.html \
			"Playing the Game" playing.html \
			"Command Descriptions" command.html \
			"Option Descriptions" option.html \
			"Version Information" version.html
	}
	if {[variant OANGBANDTK]} {
		lappend data \
			"-" "" \
			"Fundamentals" general.html \
			"Creating a character" birth.html \
			"Commands and Options" command.html \
			"Magic and Combat" combat.html \
			"Exploring the Dungeon" explore.html
	}
	if {[variant ZANGBANDTK]} {
		lappend data \
			"-" "" \
			"General Information" general.html \
			"Creating A Character" birth.html \
			"Exploring The Dungeon" dungeon.html \
			"Attacking Monsters" attack.html \
			"Command Descriptions" command.html \
			"Option Descriptions" option.html \
			"Version Information" version.html \
			"-" "" \
			"ZAngband Magic System" magic.html
	}

	foreach {label file} $data {
		if {$label eq "-"} {
			lappend entries [list -type separator]
			continue
		}
		incr identCnt
		lappend entries [list -type radiobutton -label $label \
			-value $file -variable NSHelp::Priv(radio) \
			-command [list NSHelp::DisplayPageByUrl $oop $file $label] \
			-identifier E_HELP_$identCnt]
	}

	NSMenu::MenuInsertEntries $mbar -end MENU_SECTION $entries

	Info $oop identCnt $identCnt
}

	set MenuString(M_HELP) \
		"Contains commands for using the help system."
	set MenuString(E_FIND) \
		"Searches the current page for some text."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSHelp::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::SetupMenus {oop mbarId} {

	set frame [Info $oop win].frameList
	if {[winfo ismapped $frame]} {
		NSMenu::EntryConfigure $mbarId E_CONTENTS -label [mc "Hide Contents"]
	} else {
		NSMenu::EntryConfigure $mbarId E_CONTENTS -label [mc "Show Contents"]
	}
	lappend identList E_CONTENTS E_FIND E_FIND_AGAIN E_CLOSE
if 0 {
	for {set i 1} {$i <= [Info $oop identCnt]} {incr i} {
		lappend identList E_HELP_$i
	}
}
	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSHelp::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {

		E_CONTENTS {
			if {[winfo ismapped [Info $oop win].frameList]} {
				set desc "Hides the contents list."
			} else {
				set desc "Shows the contents list."
			}
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

# NSHelp::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::DisplayCmd {oop message first {url ""} {interactive 1}} {

	set win [Info $oop win]
	set whtml [Info $oop whtml]
	
	switch -- $message {
		preDisplay {
			Info $oop interactive $interactive
			if {[string length $url]} {
				DisplayPageByUrl $oop $url
			}
		}
		postDisplay {
			focus $whtml
			$win.frameHtml.xscroll set {*}[$whtml xview]
			$win.frameHtml.yscroll set {*}[$whtml yview]
		}
	}

	return
}

# NSHelp::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font [FontCmd $oop 2 fixed]
	}
	set widthList [winfo reqwidth $win.frameList]
	set width [expr {$widthList + [font measure $font "W"] * 82 + 16}]

	scan [angband system workarea] "%d %d %d %d" left top right bottom
	if {[Platform unix]} {
		set left 0
		set top 0
		set right [ScreenWidth]
		set bottom [ScreenHeight]
	}

	# If this is being called in HardcodeGeometry(), then the window
	# should be positioned offscreen.
	set screenWidth [ScreenWidth]
	if {[Global HardcodeGeometry]} {
		set x [OffscreenOffsetX]
	} else {
		set x 0
	}
	set y [NSToplevel::FrameTop $winMain]
	set width [NSToplevel::ContentWidth $win $width]
#	set height [NSToplevel::ContentHeight $win [expr {$bottom - $y}]]
	set height [NSToplevel::ContentHeight $win [NSToplevel::TotalHeight $winMain]]
	return ${width}x$height+$x+$y
}

proc NSHelp::GeometryCmd {oop} {

	set win [Info $oop win]

	return [GetDefaultGeometry $win main2 main2]
}

# NSHelp::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Close {oop} {

	# Clear the text (and hope the window is actually closed)
#	[Info $oop win].text clear

	if {[Info $oop interactive]} {
		angband keypress \033
	} else {
		angband_display help hide
	}

	return
}

# NSHelp::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSHelp::Win98MenuCmd_Back --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Win98MenuCmd_Back {oop button} {

	variable Priv

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]
	$menu delete 0 end

	for {set i [expr {$Priv(history,index) - 1}]} {$i >= 0} {incr i -1} {
		$menu add command -label [lindex $Priv(history,links,title) $i] \
			-command "after idle NSHelp::DisplayPageByHistory $oop $i"
	}

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	after idle $button hidemenu

	return
}

# NSHelp::Win98MenuCmd_Next --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Win98MenuCmd_Next {oop button} {

	variable Priv

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set count [llength $Priv(history,links,path)]

	set menu [Info $oop toolbar,menu]
	$menu delete 0 end

	for {set i [expr $Priv(history,index) + 1]} {$i < $count} {incr i} {
		$menu add command -label [lindex $Priv(history,links,title) $i] \
			-command "after idle NSHelp::DisplayPageByHistory $oop $i"
	}

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	after idle $button hidemenu

	return
}

proc NSHelp::Reset {oop} {

	set whtml [Info $oop whtml]

if {[TKHTMLVERSION 1]} {
	$whtml clear
	$whtml configure -marginheight 0 -marginwidth 0
}
if {[TKHTMLVERSION 3]} {
	$whtml reset
	$whtml style "
		html {
			font-family: [list [mc font,varwid]];
			font-size: [lindex [mc font,size] 1]pt;
		}
		H1 { font-size: 1.8em }
		H2 { font-size: 1.5em }
		H3 { font-size: 1.2em }
	"
}
	return
}

# NSHelp::SetText --
#
#	Read a help file's contents into the text display. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHelp::SetText {oop filePath {force 0}} {

	variable Priv

	set win [Info $oop win]
	set whtml [Info $oop whtml]

	set filePath [LongName $filePath]
	set fileName [file tail $filePath]
	if {!$force && [string equal $filePath [Info $oop curFile]]} {
		return
	}
 
	# Now attempt to open the file
	if {[catch {open $filePath} fileId]} {
		return
	}

	StatusBar $oop "Displaying [file nativename $filePath]..." 0
	update idletasks

	if {[info tclversion] >= "8.1"} {
if {[TKHTMLVERSION 1]} {
		$whtml configure -base $filePath
}
		FreeImages $oop
	}

if 1 {
	catch {
		if {[file extension $filePath] eq ".html"} {
			set data [read $fileId]
		} else {
			set buf [read $fileId]

			# Translate < and > to &lt; and &gt;
			regsub -all < $buf {\&lt;} buf
			regsub -all > $buf {\&gt;} buf

			set data <pre>$buf</pre>
		}

		Info $oop parse,encoding ""
		Info $oop parse,restart 0

		Reset $oop

if {[TKHTMLVERSION 1]} {
		$whtml parse $data
		if {[Info $oop parse,restart]} {
			set encoding [Info $oop parse,encoding]
			$whtml parse [encoding convertfrom $encoding $data]
		}
}
if {[TKHTMLVERSION 3]} {
		$whtml parse $data
		if {[Info $oop parse,restart]} {
			set encoding [Info $oop parse,encoding]
			$whtml parse [encoding convertfrom $encoding $data]
		}
}
	}
} else {

	catch {
	
		# Delete the old text and insert the new
		$whtml clear

		if {[file extension $filePath] ne ".html"} {
			$whtml parse <pre>
		}

		while {![eof $fileId]} {
			set buf [read $fileId 5048]
			$whtml parse $buf
		}
		
		if {[file extension $filePath] ne ".html"} {
			$whtml parse </pre>
		}
	}
}

	# Close the file
	close $fileId

	# Radiobutton menu entries
	set Priv(radio) $fileName

	# Remember the currently-displayed page so we don't read it in again
	Info $oop curFile $filePath

	StatusBar $oop "Done." 1

	return
}

# NSHelp::SetHtmlOther --
#
#	Display HTML from a non-file source. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHelp::SetHtmlOther {oop info {force 0}} {

	variable Priv

	set win [Info $oop win]
	set whtml [Info $oop whtml]

	if {[info tclversion] >= "8.1"} {
if {[TKHTMLVERSION 1]} {
		$whtml configure -base [PathTk doc no-such-file]
}
		FreeImages $oop
	}

	Reset $oop

	# ZANGBANDTK
	if {[string match @z,* $info]} {
		scan $info @z,%s link
		set list [split $link #]
		if {[llength $list] == 1} {
			lappend list *
		}
		set file [lindex $list 0]
		set tag [lindex $list 1]
		if {[info tclversion] >= "8.1"} {
if {[TKHTMLVERSION 1]} {
			$whtml configure -base $info
}
		}
		if {[string match *.hlp $file]} {
			set buffer [ZHelp::HlpToHtml $file]
			$whtml parse $buffer
		} else {
			set buffer [ZHelp::GetTextByTag $file $tag]
			$whtml parse <pre>
			$whtml parse $buffer
			$whtml parse </pre>
		}
	} else {
		$whtml parse $Priv(html,$info)
	}

	Info $oop curFile $info

	return
}

# NSHelp::SynchButtons --
#
#	Enable or disable the toolbar buttons depending on the history.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::SynchButtons {oop} {

	variable Priv

	set back [Info $oop button,back]
	set next [Info $oop button,next]
	set reload [Info $oop button,reload]
	
	set index $Priv(history,index)
	set count [llength $Priv(history,links,path)]

	if {$index > 0} {
		if {[$back info state] eq "disabled"} {
			$back configure -state normal
		}
	} else {
		if {[$back info state] ne "disabled"} {
			$back configure -state disabled
		}
	}
	if {$index < $count - 1} {
		if {[$next info state] eq "disabled"} {
			$next configure -state normal
		}
	} else {
		if {[$next info state] ne "disabled"} {
			$next configure -state disabled
		}
	}
	if {[string length [Info $oop curFile]]} {
		$reload configure -state normal
	} else {
		$reload configure -state disabled
	}

	return
}

# NSHelp::SaveState --
#
#	Saves the state of the current page.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::SaveState {oop} {

	variable Priv

	if {$Priv(history,index) == -1} return

	set win [Info $oop win]
	set whtml [Info $oop whtml]

	# Remember the scroll position of the current document
	set yview [lindex [$whtml yview] 0]
	lset Priv(history,yview) $Priv(history,index) $yview

	return
}

if {[TKHTMLVERSION 3]} {

proc NSHelp::Handler_node_a {oop node} {
#	puts "Handler_node_a"
	if {[$node attribute -default "" href] ne ""} {
		$node dynamic set link
	}
}
proc NSHelp::Handler_attr_a {oop node attr val} {
#	puts "Handler_attr_a $val"
	# This is never called for some reason
}

# TKHTMLVERSION 3
}

# NSHelp::HyperlinkCmd --
#
#	Called when the mouse moves into or out of a hyperlink, and when
#	a hyperlink is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::HyperlinkCmd {oop click url} {

	set win [Info $oop win]
	set whtml [Info $oop whtml]

	regsub -all %20 $url " " url

	if {[string length $url]} {
		if {[string match @* [file tail $url]]} {
			set url [file tail $url]
		}
	}

	# Mouse click in hyperlink
	if {$click} {

		if {[string length $url]} {
			after idle NSHelp::DisplayPageByUrl $oop [list $url]
		}
		
	# Mouse entered/left a hyperlink
	} else {

		if {[string length $url]} {
			if {[string match @* $url]} {
				StatusBar $oop $url 0
			} else {
				regexp {([^#]+)#?([^#]*)?} $url ignore filePath label
				set fileName [file tail $filePath]
				set doLabel 0
				if {[file exists $filePath]} {
					# Nothing
				} elseif {[file exists [Path lib file $fileName]]} {
					set url [Path lib file $fileName]
					set doLabel 1
				} elseif {[file exists [Path lib help $fileName]]} {
					set url [Path lib help $fileName]
					set doLabel 1
				} elseif {[file exists [PathTk doc $fileName]]} {
					set url [PathTk doc $fileName]
					set doLabel 1
				} elseif {[file exists [CPathTk doc $fileName]]} {
					set url [CPathTk doc $fileName]
					set doLabel 1
				} else {
					# Error
					set url "? $url"
				}
				if {$doLabel && [string length $label]} {
					append url #$label
				}
				StatusBar $oop [file nativename $url] 0
			}
			set cursor hand2
		} else {
			StatusBar $oop {} 0
			set cursor {}
		}

		# The TkHTML widget recomputes everything for every configure call,
		# so try to be efficient.
if {[TKHTMLVERSION 1]} {
		if {[$whtml cget -cursor] ne $cursor} {
			$whtml configure -cursor $cursor
		}
}
if {[TKHTMLVERSION 3]} {
		set cursorWin [winfo parent $whtml]
		if {[$cursorWin cget -cursor] ne $cursor} {
			$cursorWin configure -cursor $cursor
		}
}
	}

	return
}

# NSHelp::DisplayPageByUrl --
#
#	Display a page by URL.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::DisplayPageByUrl {oop url {title ""}} {

	variable Priv

	if {[Info $oop busy]} return

	set win [Info $oop win]
	set whtml [Info $oop whtml]
	set oldFile [Info $oop curFile]

	regexp {([^#]+)#?([^#]*)?} $url ignore filePath label
	set fileName [file tail $filePath]

	if {[string match @* $url]} {
	
	} else {
	
		# Look for the given file
		if {[file exists $filePath]} {
			# Nothing
	
		# Look in lib\file
		} elseif {[file exists [Path lib file $fileName]]} {
			set filePath [Path lib file $fileName]
	
		# Look in lib\help
		} elseif {[file exists [Path lib help $fileName]]} {
			set filePath [Path lib help $fileName]
	
		# Look in tk\doc
		} elseif {[file exists [PathTk doc $fileName]]} {
			set filePath [PathTk doc $fileName]

		# Look in CommonTk\tk\doc
		} elseif {[file exists [CPathTk doc $fileName]]} {
			set filePath [CPathTk doc $fileName]
	
		# Can't find the file
		} else {
			tk_messageBox -message "Cannot display URL \"$url\""
			return
		}
	}

	# "update" calls can screw us over, if a button is clicked
	Info $oop busy 1

	# Select the page in the contents list
	if {![Info $oop inPageCmd]} {
		set clientData $filePath
		if {[string length $label]} {
			append clientData "#$label"
		}
		NSHelpList::SelectPage [Info $oop listId] \
			"NSHelp::SelectPageCmd $oop [list $clientData]"
	}

	SaveState $oop

	if {[string match @* $url]} {

		SetHtmlOther $oop $url

#		set title ""
	} else {

		SetText $oop $filePath
	
		set title [file nativename [LongName $filePath]]
	}

	# Extract the document title from <TITLE></TITLE> tags
if {[TKHTMLVERSION 1]} {
	# Could use "token handler" command
	set start [$whtml token find TITLE]
	set end [$whtml token find /TITLE]
	if {[llength $start] && [llength $end]} {
		set start [lindex [lindex $start 0] 0]
		set end [lindex [lindex $end 0] 0]
		incr start ; incr end -1
		set title [$whtml text ascii $start $end]
	}
}
if {[TKHTMLVERSION 3]} {
	set node [lindex [$whtml search title] 0]
	if {$node ne ""} {
		set title ""
		foreach child [$node children] {
			append title [$child text]
		}
	}
}

	set index $Priv(history,index)

	if {![string length $title]} {
		if {$filePath eq $oldFile} {
			set title [lindex $Priv(history,links,title) $index]
		} else {
			set title $fileName
		}
	}

	# Delete history information after the current index
	set Priv(history,links,label) [lrange $Priv(history,links,label) 0 $index]
	set Priv(history,links,path) [lrange $Priv(history,links,path) 0 $index]
	set Priv(history,links,title) [lrange $Priv(history,links,title) 0 $index]
	set Priv(history,yview) [lrange $Priv(history,yview) 0 $index]

	# Remember link information for this page
	lappend Priv(history,links,label) $label
	lappend Priv(history,links,path) $filePath
	lappend Priv(history,links,title) $title
	lappend Priv(history,yview) 0.0

	# One more link was added
	incr Priv(history,index)

	# Only save 20 links
	if {$Priv(history,index) >= 20} {
		set Priv(history,links,label) [lrange $Priv(history,links,label) end-19 end]
		set Priv(history,links,path) [lrange $Priv(history,links,path) end-19 end]
		set Priv(history,links,title) [lrange $Priv(history,links,title) end-19 end]
		set Priv(history,yview) [lrange $Priv(history,yview) end-19 end]
		set Priv(history,index) 19
	}

	# If a label was given, then scroll to it
	if {[string length $label]} {
if {[TKHTMLVERSION 1]} {
		$whtml yview $label
}
if {[TKHTMLVERSION 3]} {
		set node [$whtml search "a\[name=\"$label\"\]"]
		if {$node ne ""} {
			$whtml yview $node
		}
}
	}

	# Enable/disable the toolbar buttons to reflect the position in
	# the history list
	SynchButtons $oop

	# Set the window title
	wm title $win [format [mc wm-title] $title $::Angband(name)]

	Info $oop busy 0

	return
}

# NSHelp::DisplayPageByHistory --
#
#	Display the n'th page in the history.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::DisplayPageByHistory {oop index} {

	variable Priv

	ASSERT {($index >= 0) && ($index < [llength $Priv(history,links,path)])} \
		"invalid history index \"$index\""

	if {[Info $oop busy]} return

	set win [Info $oop win]
	set whtml [Info $oop whtml]

	set label [lindex $Priv(history,links,label) $index]
	set filePath [lindex $Priv(history,links,path) $index]
	set title [lindex $Priv(history,links,title) $index]
	set yview [lindex $Priv(history,yview) $index]

	Info $oop busy 1

	# Select the page in the contents list
	if {![Info $oop inPageCmd]} {
		set clientData $filePath
		if {[string length $label]} {
			append clientData "#$label"
		}
		NSHelpList::SelectPage [Info $oop listId] \
			"NSHelp::SelectPageCmd $oop $clientData"
	}

	SaveState $oop

	if {[string match @* $filePath]} {
		if {[string length $label]} {
			append filePath #$label
		}
		SetHtmlOther $oop $filePath
	} else {
		SetText $oop $filePath
	}
	$whtml yview moveto $yview

	set Priv(history,index) $index

	# Enable/disable the toolbar buttons to reflect the position in
	# the history list
	SynchButtons $oop

	# Set the window title
	wm title $win [format [mc wm-title] $title $::Angband(name)]

	Info $oop busy 0

	return
}

# NSHelp::Back --
#
#	Go to the previous page in the history list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Back {oop delta} {

	variable Priv

	# Update button
	update idletasks

	DisplayPageByHistory $oop [expr {$Priv(history,index) - $delta}]

	return
}

# NSHelp::Next --
#
#	Go to the next page in the history list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Next {oop delta} {

	variable Priv

	# Update button
	update idletasks

	DisplayPageByHistory $oop [expr {$Priv(history,index) + $delta}]

	return
}

# NSHelp::Reload --
#
#	Redisplay the current page.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Reload {oop} {

	variable Priv

	set whtml [Info $oop whtml]
	set yview [lindex [$whtml yview] 0]

	set index $Priv(history,index)
	set url [lindex $Priv(history,links,path) $index]
	set label [lindex $Priv(history,links,label) $index]
	if {[string match @* $url]} {
		if {[string length $label]} {
			append url #$label
		}
		SetHtmlOther $oop $url
	} else {
		SetText $oop $url 1
	}

	$whtml yview moveto $yview

	return
}

# NSHelp::Find --
#
#	Simple search routine to search current file for a string.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Find {oop again} {

	variable Priv

	set win [Info $oop win]
	set whtml [Info $oop whtml]

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a string
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) -prompt [mc find-prompt] \
			-buttons [list [mc Find] [mc Cancel]] -parent $win]
		if {![string length $string]} return

		set Priv(find,string) $string
		set Priv(find,index) 1.0
	}

if 1 {
	set index [$whtml text find $Priv(find,string) nocase after $Priv(find,index)]
	if {![string length $index]} {
		set index [$whtml text find $Priv(find,string) nocase after 1.0]
	}
} else {
	set index [$whtml search -nocase -- $Priv(find,string) \
		$Priv(find,index)]
}
	if {![string length $index]} return

	focus $whtml

	scan $index "%s %s" index1 index2
scan $index2 %d.%d tokenNumber charIndex
set index2 $tokenNumber.[incr charIndex]
	$whtml selection set $index1 $index2
if 0 {
	scan [$whtml coords $index1] "%s %s %s %s" x1 y1 x2 y2
	scan [$whtml coords] "%s %s" x3 y3
	$whtml yview moveto [expr {1.0 * $y1 / $y3}]
} else {
	$whtml see $index
}

	set Priv(find,index) $index2

	return
}

if {[info tclversion] >= "8.1"} {

# NSHelp::Button1 --
#
#	Handle <ButtonPress-1>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Button1 {oop x y} {

	set whtml [Info $oop whtml]
if {[TKHTMLVERSION 1]} {
	$whtml selection clear

	set command [$whtml cget -hyperlinkcommand]
	if {![string length $command]} return
	set url [lindex [$whtml href $x $y] 0]
	eval $command 1 [list $url]
}
if {[TKHTMLVERSION 3]} {
	focus $whtml

	set nodelist [$whtml node $x $y]
	set topnode [lindex $nodelist end]
	if {$topnode ne ""} {
		if {[$topnode tag] eq ""} { set topnode [$topnode parent] }
		if {$topnode eq ""} { set topnode $whtml node] }
		
		set href [$topnode attribute -default "" href]
		if {$href ne ""} {
			HyperlinkCmd $oop 1 $href
		}
	}
}
	return
}

# NSHelp::Motion1 --
#
#	Handle <Motion>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Motion1 {oop x y} {

	set whtml [Info $oop whtml]

if {[TKHTMLVERSION 1]} {
	set command [$whtml cget -hyperlinkcommand]
	if {![string length $command]} return
	set url [lindex [$whtml href $x $y] 0]
	eval $command 0 [list $url]
}
if {[TKHTMLVERSION 3]} {
	set nodelist [$whtml node $x $y]
	set topnode [lindex $nodelist end]
	if {$topnode ne ""} {
		if {[$topnode tag] eq ""} { set topnode [$topnode parent] }
		if {$topnode eq ""} { set topnode $whtml node] }
		
		set href [$topnode attribute -default "" href]
		HyperlinkCmd $oop 0 $href
	}
}
	return
}

# NSHelp::FontCmd --
#
#	Font selector for TkHtml.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::FontCmd {oop size style} {

	set bold [lsearch -exact $style bold]
	set italic [lsearch -exact $style italic]
	set fixed [lsearch -exact $style fixed]
	set fontStyle $style
	set sizeList [mc font,size]
	if {$fixed >= 0} {
		set fontFamily [mc font,fixed]
		set fontStyle [lreplace $fontStyle $fixed $fixed]
	} else {
		set fontFamily [mc font,varwid]
	}
	incr size -1
	set fontSize [lindex $sizeList $size]
	return "$fontFamily -$fontSize $fontStyle"
}

# NSHelp::ImageCmd --
#
#	Image loader for TkHtml.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ImageCmd {oop src args} {

	variable Image

	if {[info exists Image(src,$src)]} {
		return $Image(src,$src)
	}
	if {[file exists $src]} {
		set image Image_Help[incr Image(nextId)]
		image create photo $image -file $src
		set Image(src,$src) $image
		lappend Image(names) $image
		return $image
	}
	
	return ""
}

# NSHelp::FreeImages --
#
#	Free any images loaded.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::FreeImages {oop} {

	variable Image

	foreach image $Image(names) {
		image delete $image
	}
	set Image(names) {}

	array unset Image src,*

	return
}

# Tcl 8.1+
}

# NSHelp::ReadContents --
#
#	Read a contents file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ReadContents {oop path} {

	set listId [Info $oop listId]

	if {![file exists $path]} {
		tk_messageBox -message "Can't find contents file \"$path\""
		return
	}
	set fileId [open $path]
	set lineList [split [read $fileId] \n]
	close $fileId

	set ROOT ""

	set stack 0
	foreach string $lineList {
		if {[scan $string ".%s" type] != 1} continue
		switch -- $type {
			book {
				set url ""
				if {[scan $string {.%s "%[^"]" %s} type title url] >= 2} {
					if {[string match {ROOT/*} $url]} {
						set url [file join $ROOT [string range $url 5 end]]
					}
					set parent [lindex $stack end]
					set book [NSHelpList::BookAdd $listId $parent $title $url]
					lappend stack $book
				} else {
					error "bad content line \"$string\""
				}
			}
			include {
				if {[scan $string {.%s "%[^"]"} type path] == 2} {
					set list {}
					foreach elem $path {
						if {$elem eq "ANGBAND_DIR"} {
							set elem [Path]
						}
						lappend list $elem
					}
					ReadContents $oop [file join {*}$list]
				}
			}
			page {
				if {[scan $string {.%s "%[^"]" %s} type title url] == 3} {
					if {[string match {ROOT/*} $url]} {
						set url [file join $ROOT [string range $url 5 end]]
					}
					set book [lindex $stack end]
					NSHelpList::PageAdd $listId $book $title $url
				} else {
					error "bad content line \"$string\""
				}
			}
			glob {
				set glob {}
				set list {}
				set buf [string range $string 6 end]
				foreach path [split $buf ,] {
					foreach elem $path {
						if {$elem eq "ANGBAND_DIR"} {
							set elem [Path]
						}
						if {$elem eq "COMMON_DIR"} {
							set elem [CPath]
						}
						lappend list $elem
					}
					set dir [file join {*}$list]
					set glob [concat $glob [glob -nocomplain -directory $dir *]]
				}
				foreach file [lsort -dictionary $glob] {
					set title [file tail $file]
					NSHelpList::PageAdd $listId $book $title $file
				}
			}
			root {
				if {[scan $string {.%s "%[^\"]"} type path] == 2} {
					set list {}
					foreach elem $path {
						if {$elem eq "ANGBAND_DIR"} {
							set elem [Path]
						}
						lappend list $elem
					}
					set ROOT [file join {*}$list]
				}
			}
			stop {
				set stack [lrange $stack 0 end-1]
				if {![llength $stack]} {
					error "extra .stop in contents"
				}
			}
			default {
				error "unknown content type \"$type\""
			}
		}
	}

	if {[llength $stack] != 1} {
		error "missing .stop in contents"
	}

	return
}

# NSHelp::PageCmd --
#
#	Called when a page is selected in the contents list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::PageCmd {oop title url} {

	if {![string length $url]} return
	Info $oop inPageCmd 1
	DisplayPageByUrl $oop $url $title
	Info $oop inPageCmd 0

	return
}

# NSHelp::SelectPageCmd --
#
#	Called by NSHelpList::SelectPage() to find a page to select.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::SelectPageCmd {oop url data} {

	if {![string length $data]} {
		return 1
	}
	if {$url eq $data} {
		return 0
	}

	regexp {([^#]+)#?([^#]*)?} $url ignore filePath1 label1
	set fileName1 [file tail $filePath1]

	regexp {([^#]+)#?([^#]*)?} $data ignore filePath label
	set fileName [file tail $filePath]

	if {$fileName1 ne $fileName} {
		return 1
	}

	if {[file exists $filePath]} {
		set url2 $filePath
	} elseif {[file exists [Path lib file $fileName]]} {
		set url2 [Path lib file $fileName]
	} elseif {[file exists [Path lib help $fileName]]} {
		set url2 [Path lib help $fileName]
	} elseif {[file exists [PathTk doc $fileName]]} {
		set url2 [PathTk doc $fileName]
	} elseif {[file exists [CPathTk doc $fileName]]} {
		set url2 [CPathTk doc $fileName]
	} else {
		return 1
	}

	if {[string length $label]} {
		append url2 #$label
	}
	if {$url eq $url2} {
		return 0
	}

	return 1
}

# NSHelp::ToggleContents --
#
#	Hide or show the contents list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ToggleContents {oop} {

	set whtml [Info $oop whtml]
	set frame [Info $oop win].frameList

	# Attempt to preserve the current scroll position. Since "$whtml see"
	# attempts to center the given index, we get the index currently
	# displayed near the middle of the HTML widget.
	set y [expr {[winfo height $whtml] / 2}]
if 1 { set y 0 }
	set index [$whtml index @0,$y]
	
	if {[winfo ismapped $frame]} {
#		grid remove $frame
		[Info $oop win].splitterH forget $frame
		set label Show
		set image Show
	} else {
#		grid $frame
		if {$::UseTile} {
			[Info $oop win].splitterH insert 0 $frame
		} else {
			[Info $oop win].splitterH add $frame -before [Info $oop win].frameHtml
		}
		set label Hide
		set image Hide
	}

	[Info $oop button,contents] configure \
		-label $label \
		-imageactive Image_$image \
		-imagenormal Image_${image}Gray

	# Restore the scroll position (after updating the geometry)
	update idletasks
if 1 {
	scan [$whtml coords $index] "%s %s %s %s" x1 y1 x2 y2
	scan [$whtml coords] "%s %s" x3 y3
	$whtml yview moveto [expr {1.0 * $y1 / $y3}]
} else {
	$whtml see $index
}

	return
}

# NSHelp::Token_META --
#
#	TkHTML token handler.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::Token_META {oop tokenNumber tagName argList} {

	if {[Info $oop parse,restart]} return

	set whtml [Info $oop whtml]

	array set d $argList

#dbwin "Token_META: $argList\n"

	# Look for <meta content="text/html; charset=ENCODING">
	if {![info exists d(content)]} return
	if {![regexp -nocase {charset=([^;]*)} $d(content) {} encoding]} return

	# Convert to Tcl encoding name
	regsub _ $encoding {} encoding
	set encoding [string tolower $encoding]
	switch -glob -- $encoding {
		iso-* {
			set encoding iso[string range $encoding 4 end]
		}
		windows-* {
			set encoding cp[string range $encoding 8 end]
		}
	}

#dbwin "Token_META: encoding is '$encoding'\n"

	# The encoding is supported by Tcl
	if {[lsearch -exact [encoding names] $encoding] != -1} {

		# The encoding isn't the system encoding
		if {[encoding system] ne $encoding} {

#dbwin "Token_META: restarting parse\n"

			# Stop all parsing
			$whtml clear

			# Remember the encoding
			Info $oop parse,encoding $encoding

			# Remember to reparse later
			Info $oop parse,restart 1
		}
	}

	return
}

if {[variant ZANGBANDTK]} {

# Some commands for using the newly improved ZAngband hyperlinked
# help files. This will be unused when the Html docs come out!
namespace eval NSHelp::ZHelp {

	variable TagList
}

# NSHelp::ZHelp::GetTextByTag --
#
#	Get the text for the given tag (tag is "A", "B", "C" etc).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ZHelp::GetTextByTag {file tag2} {

	set path [Path lib help $file]

	set tagList [GetTagList $file]
	if {![llength $tagList]} {
		if {[catch {open $path} fileId]} {
			return ""
		}
		set result [read $fileId]
		close $fileId

		# Translate < and > to &lt; and &gt;
		regsub -all < $result {\&lt;} result
		regsub -all > $result {\&gt;} result

		return $result
	}

	set tagIndex 0
	foreach tagg $tagList {
		scan $tagg "%s %d %d" tag offset1 offset2
		if {$tag eq $tag2} {
			break
		}
		incr tagIndex
	}

	if {[catch {open $path} fileId]} {
		return ""
	}

	set start $offset2
	seek $fileId $start
	if {$tagIndex + 1 < [llength $tagList]} {
		scan [lindex $tagList [expr {$tagIndex + 1}]] "%s %d %d" tag offset1 offset2
		set end $offset1
		set size [expr {$end - $start}]
		set text [read $fileId $size]
	} else {
		set end -1
		set text [read $fileId]
	}
	
	close $fileId

	# Translate < and > to &lt; and &gt;
	regsub -all \< $text {\&lt;} text
	regsub -all \> $text {\&gt;} text

	return [string trimright $text]
}

# NSHelp::ZHelp::HlpToHtml --
#
#	Read links from a .hlp file, returning Html.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ZHelp::HlpToHtml {file} {

	set path [Path lib help $file]
	if {[catch {open $path} fileId]} {
		return ""
	}

	catch {
		# Line 1 is the title
		set title [string trimright [gets $fileId] .]
		append result "<h1>$title</h1>\n"
		append result <ul>
		while 1 {
			set count [gets $fileId line]
			if {$count < 0} break
			if {[regexp {\(.\) (.*) \(([^ ]+\.txt.*)\)} $line ignore text link]} {
				append result "<li><a href=@z,$link>[string trim $text]</a></li>"
			}
		}
		append result </ul>
	} error

	close $fileId

	return $result
}

# NSHelp::ZHelp::GetTagList --
#
#	Parse a .txt file for a list of tags (a tag looks like <A>, <B> etc).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ZHelp::GetTagList {file} {

	variable TagList

	if {[info exists TagList($file)]} {
		return $TagList($file)
	}

	set path [Path lib help $file]
	if {[catch {open $path} fileId]} {
		return {}
	}

	# XXX Note: Help files must be in unix format or the offsets are wrong!
	# I guess if I use "fconfigure -translation binary/lf"

	catch {
		set top 0
		set offset1 0
		set result {}
		while 1 {
			set count [gets $fileId line]
			if {$count < 0} break
			set offset2 [tell $fileId]
			if {[regexp {^\*\*\*\*\* \<([0-9A-Za-z]+)\>} $line ignore tag]} {
				if {!$top && $offset1} {
					lappend result "* 0 0"
					set top 1
				}
				lappend result "$tag $offset1 $offset2"
			}
			set offset1 [tell $fileId]
		}
	} error

	close $fileId

	set TagList($file) $result

	return $result
}

# NSHelp::ZHelp::GenHelpIndex --
#
#	Parse each lib/help/*.txt file, a create tk/doc/help-index with
#	a list of tags in each file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ZHelp::GenHelpIndex {} {

	if {[catch {openlf [PathTk doc help-index]} writeId]} {
		return
	}

	set wd [pwd]
	cd [Path lib help]
	catch {
		foreach file [lsort -dictionary [glob *.txt]] {
			set tagList [GetTagList $file]
			puts $writeId "Tag $file $tagList"
		}
	}
	cd $wd

	close $writeId

	return
}

# NSHelp::ZHelp::ReadHelpIndex --
#
#	Read tk/doc/help-index, creating it if necessary.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ZHelp::ReadHelpIndex {} {

	if {![file exists [PathTk doc help-index]]} {
		GenHelpIndex
	}
	source [PathTk doc help-index]

	return
}

# NSHelp::ZHelp::Tag --
#
#	Called when sourcing tk/doc/help-index.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHelp::ZHelp::Tag {file args} {

	variable TagList

	set TagList($file) $args

	return
}

# ZANGBANDTK
}
