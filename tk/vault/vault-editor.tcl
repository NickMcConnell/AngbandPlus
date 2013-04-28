# File: vault-editor.tcl

# Purpose: the Vault Editor Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSVaultEditor {

	variable MenuString
	variable Priv
	variable Plugin

# namespace eval NSVaultEditor
}

# NSVaultEditor::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::InitModule {} {

	variable Priv

	InitImageIfNeeded Image_EyeDrop eyedrop.gif vault
	InitImageIfNeeded Image_Flood flood.gif vault
	InitImageIfNeeded Image_Pencil paint.gif vault
	InitImageIfNeeded Image_RectOutline rect-outline.gif vault
	InitImageIfNeeded Image_RectFilled rect-filled.gif vault
	InitImageIfNeeded Image_Select select.gif vault
	InitImageIfNeeded Image_Erase erase.gif vault

	NSModule::AddModuleIfNeeded NSVaultPalette [CPathTk vault vault-palette.tcl]
	NSModule::LoadIfNeeded NSIconBrowser
if 0 {
	# XXX Hack -- Remember the "default" symbols for each feature
	set vaultId [vault create -height 1 -width 1]
	set MAX_F_IDX [angband f_info max]
	for {set f_idx 0} {$f_idx < $MAX_F_IDX} {incr f_idx} {
		set Priv(symbol,$f_idx) [vault symbol $vaultId $f_idx]
	}
	vault delete $vaultId
}
	if {[variant KANGBANDTK ZANGBANDTK]} {
		ReadTPref
		ReadQPref
	}

	# Font
	if {[lsearch -exact [icon gettypes] vault] == -1} {
		icon createtype vault -font "Courier 24 bold"

		Global vault,attr,monster [icon ascii count]
		icon ascii create -foreground [palette set 0] \
			-background [palette set 222]

		Global vault,attr,object [icon ascii count]
		icon ascii create -foreground [palette set 0] \
			-background [palette set 232]
	}

	Plugin::ReadIndex

	# Create the Vault Editor Window
	set oop [NSObject::New NSVaultEditor]
	set Priv(editorId) $oop

	NSModule::RebootModule NSVaultPalette
	set oop2 $NSVaultPalette::Priv(oop)
	Info $oop pal,oop $oop2
	Info $oop pal,win [NSVaultPalette::Info $oop2 win]

	Replace_Init $oop

	return
}

# NSVaultEditor::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::CloseModule {} {

	variable Priv

	catch {
		destroy [NSVaultEditor::Info $Priv(editorId) win]
	}

	return
}

# NSVaultEditor::NSVaultEditor --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::NSVaultEditor {oop} {

	variable Priv

	InitVaults $oop

	qebind NSVaultEditor$oop <IconCfg> \
		"NSVaultEditor::IconCfg $oop"

#	set height [vault height $vaultId]
#	set width [vault width $vaultId]

	InitWindow $oop

	set win [Info $oop win]

	Info $oop ignoreSel 0
	Info $oop ignoreExamine 0
	Info $oop ignoreView 0
	Info $oop ignoreTrack 0
	Info $oop clip,bounds ""
	Info $oop tool,tool Pencil
	Info $oop tool,feature 1
	Info $oop tool,letter .
	Info $oop tool,symbol sym.floor0
	Info $oop tool,command Feedback_Pencil
	Info $oop clobberIcons 0
	Info $oop track,press 0
	Info $oop pal,show 0

	Info $oop dragsel 0
	Info $oop select,visible 0
	Info $oop select,bounds ""
	Info $oop sel,float 0

	Info $oop fullScreen 0
	$win.statusBar itemconfigure t2 -text "HIDE"

	Info $oop mode,default 1
	Info $oop mode,toggle 1
	Info $oop mode,town 1

	# The name of the .txt file we opened last (town, quest)
	Info $oop path,text ""

	# The name of the .vlt file we are editing
	Info $oop path,vault ""

	NSWindowManager::RegisterWindow vaulteditor $win \
		"NSVaultEditor::GeometryCmd $oop" \
		"NSVaultEditor::SetupCmd $oop" \
		"NSVaultEditor::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSVaultEditor $oop $win

	VaultSetup $oop

	Info $oop mode MODE_VAULT
#	SetMode $oop MODE_VAULT
	SetTitle $oop Untitled

	Info $oop tool,icon {none 0}
	Info $oop save,icon [Info $oop tool,icon]

	Info $oop layer 1
	[Info $oop button,layer1] configure -relief sunken

	Info $oop editMode ""
	SetEditMode $oop icon
	SetToolIcon $oop [Info $oop tool,icon]

	SetTool $oop Pencil
	[Info $oop button,Pencil] configure -relief sunken

	# Initialize entire vault to "floor"
	Erase $oop

	return
}

# NSVaultEditor::~NSVaultEditor --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::~NSVaultEditor {oop} {

	vault delete [Info $oop display,vaultId]
	vault delete [Info $oop clip,vaultId]
	vault delete [Info $oop edit,vaultId]
	vault delete [Info $oop undo,display,vaultId]
	vault delete [Info $oop undo,edit,vaultId]

	qebind NSVaultEditor$oop <IconCfg> {}

	return
}

# NSVaultEditor::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Info {oop info args} {

	global NSVaultEditor

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSVaultEditor($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSVaultEditor($oop,$info)
			}
		}
	}
}

# NSVaultEditor::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::InitWindow {oop} {

	variable Priv

	set win .vaultEditor$oop
	toplevel $win
	wm title $win "Vault Editor"

	# Start out withdrawn (hidden)
	wm withdraw $win

#	TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSVaultEditor::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	frame $win.divider2 \
		-borderwidth 1 -height 2 -relief groove

	#
	# Panedwindow
	#

	set pwH $win.pwH
	panedwindow $pwH \
		-orient horizontal -sashrelief sunken

	frame $win.frameLeft \
		-borderwidth 0

	#
	# Tool display
	#

	set frame $win.frameTool
	frame $frame \
		-borderwidth 0
	lappend tools Image_Pencil Pencil "Pencil tool"
	lappend tools Image_RectFilled RectFill "Filled rectangle tool"
	lappend tools Image_RectOutline RectOutline "Outline rectangle tool"
	lappend tools Image_EyeDrop EyeDrop "Eyedrop tool"
	lappend tools Image_Select Marquee "Selection tool"
	lappend tools Image_Flood Flood "Flood fill tool"
	lappend tools Image_Erase Erase "Erase tool"
	foreach {image tool string} $tools {
		set label [label $frame.[string tolower $tool] \
			-image $image -relief raised]
		bind $label <ButtonPress-1> \
			"NSVaultEditor::SetTool $oop $tool"
		NSStatusText::StatusText $label $win.statusBar.label0 $string
		Info $oop button,$tool $label
	}

	set iconW $NSIconBrowser::Priv(maxIconWidth)
	set iconH $NSIconBrowser::Priv(maxIconHeight)

	canvas $frame.canvas \
		-width $iconW -height $iconH \
		-borderwidth 1 -relief sunken -highlightthickness 0 \
		-scrollregion {0 0 0 0}

	# Current icon
	$frame.canvas create widget [expr {$iconW / 2}] 0 -anchor n -tags tool

	# Clicking the canvas displays the tool icon.
	$frame.canvas bind tool <ButtonPress-1> \
		"NSVaultEditor::DisplayIcon $oop icon"

	NSStatusText::StatusText $frame.canvas $win.statusBar.label0 \
		"Current icon or feature. Click to display in the list."

	Info $oop canvasTool $frame.canvas

	# Layer buttons
	set frame2 $frame.frameLayer
	frame $frame2 \
		-borderwidth 0
	foreach layer {4 3 2 1} {
		set label [label $frame2.layer$layer -text $layer \
			-width 2 -height 1 -relief raised]
		bind $label <ButtonPress-1> \
			"NSVaultEditor::SetLayer $oop $layer"
		pack $label -side top
		NSStatusText::StatusText $label $win.statusBar.label0 "Edit icon layer $layer"
		Info $oop button,layer$layer $label
	}

	# Layer icons
	canvas $frame.canvas2 \
		-width $iconW -height [expr {$iconH * 4}] \
		-borderwidth 1 -relief sunken -highlightthickness 0 \
		-scrollregion {0 0 0 0}
	foreach layer {4 3 2 1} {
		$frame.canvas2 create widget [expr {$iconW / 2}] \
			[expr {(4 - $layer) * $iconH}] -anchor n -tags icon$layer
	}

	Info $oop layer,canvas $frame.canvas2

	#
	# Widget + scrollbars
	#

	set vaultId [Info $oop display,vaultId]
	set mapId [NSObject::New NSMap $win 32 32 [icon width] [icon height]]
	set widgetId [NSMap::Info $mapId widgetId]

	[NSMap::Info $mapId frame] configure -borderwidth 1 -relief sunken

	set widget [NSMap::Info $mapId widget]
	$widget configure -vaultnum $vaultId

	# Hack -- Hittest floor tile, not actual icon in isometric view
	NSWidget::Info $widgetId caveyx 1

	# Cursor
	Info $oop widget,cursor [$widget create cursor -x 0 -y 0 -linewidth 1 \
		-visible yes]

	# Selection rectangle
	Info $oop widget,sel [$widget create rectangle -x1 0 -y1 0 -x2 1 -y2 1 \
		-visible no]

	NSWidget::Info $widgetId examineCmd \
		"NSVaultEditor::ExamineCmd $oop"

	NSMap::Info $mapId viewCmd \
		"NSVaultEditor::ViewCmd $oop"

	bind $widget <Leave> "+
		$widget itemconfigure [Info $oop widget,cursor] -visible no
	"

	bind $widget <ButtonPress-1> \
		"NSVaultEditor::Press1 $oop %x %y"
	bind $widget <Button1-Motion> \
		"NSVaultEditor::Motion1 $oop %x %y"
	bind $widget <ButtonRelease-1> \
		"NSVaultEditor::Release1 $oop %x %y"
	bind $widget <Button1-Leave> \
		"NSVaultEditor::AutoScan $oop"
	bind $widget <Leave> "+
		NSVaultEditor::StatusBar $oop {} 0
	"
	bind $widget <Control-ButtonPress-1> \
		"NSVaultEditor::Press1 $oop %x %y 1"

	# Track while shift is down
	bind $widget <Shift-Motion> {}
	bind $widget <Shift-Leave> break

	# Shift-click to drag view
	bind $win <KeyPress-Shift_L> \
		"$widget configure -cursor fleur"
	bind $win <KeyRelease-Shift_L> \
		"$widget configure -cursor {}"
	bind $widget <Shift-ButtonPress-1> "
		NSWidget::Info $widgetId track,x %x
		NSWidget::Info $widgetId track,y %y
		NSWidget::Info $widgetId track,mouseMoved 0
	"
	bind $widget <Shift-Button1-Motion> \
		"NSWidget::TrackOnce $widgetId %x %y"
	bind $widget <Shift-Button1-Leave> break
	bind $widget <Shift-ButtonRelease-1> "
		if {\[string equal \[$widget cget -style] map] && \
			!\[NSWidget::Info $widgetId track,mouseMoved]} {
			NSMenu::TryInvoke [Info $oop mbarId] E_SCALE
			eval NSMap::SetView $mapId \[NSWidget::Info $widgetId examined]
		}
	"

	Info $oop mapId $mapId
	Info $oop widgetId $widgetId
	Info $oop widget $widget

	#
	# Frame for stuff on the right
	#

	set pwV $win.pwV
	panedwindow $pwV \
		-orient vertical -sashrelief sunken

	#
	# Micro Map
	#

	set map2Id [NSObject::New NSMap $win 20 20 4 4]
	set widget2Id [NSMap::Info $map2Id widgetId]
	set widget2 [NSMap::Info $map2Id widget]

	$widget2 configure -vaultnum $vaultId
	set view [$widget2 create rectangle -x1 0 -y1 0 -x2 1 -y2 1 -linewidth 1 \
		-visible yes]

	NSWidget::Info $widget2Id examineCmd \
		"NSVaultEditor::ExamineCmd_Map $oop"

	bind $widget2 <Leave> "+
		eval NSMap::SetView $mapId \[NSVaultEditor::Info $oop widget,center]
		$widget itemconfigure \[NSVaultEditor::Info $oop widget,cursor] -visible no
	"

	# Click the micro map to set the view of the main map
	bind $widget2 <ButtonRelease-1> "
		if {!\[NSWidget::Info $widget2Id track,mouseMoved]} {
			NSVaultEditor::Info $oop widget,center \[$widget center]
		}
	"

	Info $oop map,mapId $map2Id
	Info $oop map,widgetId $widget2Id
	Info $oop map,widget $widget2
	Info $oop map,view $view

	#
	# Frame for tabs + icon/feature lists
	#

	frame $win.frameList \
		-borderwidth 0

	#
	# Tabs for icon/feature lists
	#

	set tabsId [NSObject::New NSTabs $win.frameList]
	NSTabs::Add $tabsId Icon
	NSTabs::Add $tabsId Feature
	NSTabs::Info $tabsId invokeCmd "NSVaultEditor::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# An NSIconBrowser lets the user examine all icon types
	#

	set browserId [NSObject::New NSIconBrowser $win]
	set treeM [NSIconBrowser::Info $browserId member,tree]
	NSList::OnSelection $treeM \
		"NSVaultEditor::SelectionChanged_Icon $oop %T %c %S %D"
	NSList::OnClick $treeM \
		"NSVaultEditor::Click_Icon $oop %r"

	# 3 columns, 4/5 rows
	set cols 2
	if {$iconW <= 32} {
		incr cols
	}
	set rows 3
	if {$iconH <= 32} {
		incr rows 2
	}
	set treeG [NSIconBrowser::Info $browserId group,tree]
	$treeG configure \
		-height [expr {($iconH + 8) * $rows}]
	$treeM configure \
		-width [expr {($iconW + 8) * $cols}] \
		-height [expr {($iconH + 8) * $rows}]

	NSIconBrowser::Info $browserId iconTypes [concat none [NSIconBrowser::Info $browserId iconTypes]]

	# Display progress while listing an icon type
	NSIconBrowser::Info $browserId clientCmd \
		"NSVaultEditor::BrowserCmd $oop"

	# Display the icon type when the mouse is over a group icon
	NSIconBrowser::Info $browserId group,motionCmd \
		"NSVaultEditor::BrowserMotionCmd $oop group"
	NSIconBrowser::Info $browserId group,leaveCmd \
		"NSVaultEditor::BrowserMotionCmd $oop group"

	# Display the icon index when the mouse is over an icon
	NSIconBrowser::Info $browserId member,motionCmd \
		"NSVaultEditor::BrowserMotionCmd $oop member"
	NSIconBrowser::Info $browserId member,leaveCmd \
		"NSVaultEditor::BrowserMotionCmd $oop member"

	NSStatusText::StatusText $treeG \
		$win.statusBar.label0 \
		"List of icon types."
	NSStatusText::StatusText $treeM \
		$win.statusBar.label0 \
		"List of icons of the selected type."

	Info $oop browserId $browserId

	#
	# Feature List
	#

	set frame $win.frameFeature
	set treeF [NSList::New $frame -icon yes]

	# Do something when a row is selected
	NSList::OnSelection $treeF \
		"NSVaultEditor::SelectionChanged_Feature $oop %T %c %S %D"

	NSStatusText::StatusText $frame $win.statusBar.label0 \
		"List of features you can draw into the vault."

	Info $oop feature,tree $treeF

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 6 6 20

	# Progress bar used to display progress of listing icons
	set label [$win.statusBar itemcget t1 -label]
	set progId [NSObject::New NSProgress2 $label 225 10]
	[NSProgress2::Info $progId frame] configure -borderwidth 0
	Info $oop progId $progId

	set label [$win.statusBar itemcget t2 -label]
	$label configure -text "HIDE" -anchor center
	bind $label <ButtonPress-1> \
		"NSMenu::TryInvoke [Info $oop mbarId] E_LAYOUT"

	set label [$win.statusBar itemcget t3 -label]
	$label configure -anchor center
	bind $label <ButtonPress-1> \
		"NSVaultEditor::ToggleEditMode $oop"

	NSStatusText::StatusText $win.statusBar.label1 $win.statusBar.label0 \
		"Click to toggle layout."
	NSStatusText::StatusText $win.statusBar.label2 $win.statusBar.label0 \
		"Click to toggle edit mode."

	#
	# Geometry
	#

	grid rowconfigure $win.frameTool 0 -weight 0
	grid rowconfigure $win.frameTool 1 -weight 0
	grid rowconfigure $win.frameTool 2 -weight 0
	grid rowconfigure $win.frameTool 3 -weight 0
	grid rowconfigure $win.frameTool 4 -weight 0
	grid rowconfigure $win.frameTool 5 -weight 0
	grid rowconfigure $win.frameTool 6 -weight 1
	grid columnconfigure $win.frameTool 0 -weight 0
	grid columnconfigure $win.frameTool 1 -weight 0
	set row 0
	set column 0
	foreach {image tool string} $tools {
		set label [Info $oop button,$tool]
		grid $label -row $row -column $column
		if {[incr column] == 2} {
			incr row
			set column 0
		}
	}
	grid $win.frameTool.canvas \
		-row 4 -column 0 -columnspan 2 -pady 3 -sticky n
	grid $win.frameTool.frameLayer \
		-row 5 -column 0 -columnspan 2 -sticky n
	grid $win.frameTool.canvas2 \
		-row 6 -column 0 -columnspan 2 -sticky s

	grid rowconfigure $win.frameLeft 0 -weight 1
	grid columnconfigure $win.frameLeft 0 -weight 0
	grid columnconfigure $win.frameLeft 1 -weight 1

	grid $win.frameTool \
		-in $win.frameLeft -row 0 -column 0 -sticky ns
	grid [NSMap::Info $mapId frame] \
		-in $win.frameLeft -row 0 -column 1 -sticky news

	grid columnconfigure $win.frameList 0 -weight 1
	grid rowconfigure $win.frameList 0 -weight 0
	grid rowconfigure $win.frameList 1 -weight 1
	grid [NSTabs::Info $tabsId canvas] -in $win.frameList -row 0 -column 0 -sticky ew
	grid [NSIconBrowser::Info $browserId frame] -in $win.frameList -row 1 -column 0 -sticky news

	grid $win.frameFeature -in $win.frameList -row 1 -column 0 -sticky news
	grid remove $win.frameFeature

	$pwV add [NSMap::Info $map2Id frame]
	$pwV add $win.frameList

	$pwH add $win.frameLeft
	$pwH add $pwV

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid rowconfigure $win 2 -weight 0
	grid columnconfigure $win 0 -weight 1

	grid $win.divider2 -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $pwH \
		-row 1 -column 0 -sticky news
	grid $win.statusBar -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	bind $win <KeyPress-1> "NSVaultEditor::SetLayer $oop 1"
	bind $win <KeyPress-2> "NSVaultEditor::SetLayer $oop 2"
	bind $win <KeyPress-3> "NSVaultEditor::SetLayer $oop 3"
	bind $win <KeyPress-4> "NSVaultEditor::SetLayer $oop 4"

	return
}

# NSVaultEditor::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::InitMenus {oop} {

	variable MenuString
	variable Priv
	variable Plugin

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSVaultEditor::MenuSetup $oop" -identifier MENUBAR]
	Info $oop mbarId $mbarId

	# Context-sensitive help
	NSMenu::Info $mbarId menuSelectCmd "NSVaultEditor::MenuSelect $oop"

	# Handle menu commands
	NSMenu::Info $mbarId invokeCmd "NSVaultEditor::MenuInvoke $oop"

	#
	# File
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_FILE
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_FILE -label File -underline 0 \
		-identifier M_FILE

	# File -> Vault
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 \
		-identifier MENU_FILE_VAULT]
	NSMenu::Info $menuId setupMode normal
if 0 {
	# File -> Quest
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 \
		-identifier MENU_FILE_QUEST]
	NSMenu::Info $menuId setupMode normal
}
	# File -> Town
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 \
		-identifier MENU_FILE_TOWN]
	NSMenu::Info $menuId setupMode normal

	SetupFileMenu $oop

	set entries {}
	lappend entries [list -type command -label "New Vault" \
		-underline 0 -accelerator $mod+N -identifier E_NEW]
	lappend entries [list -type cascade -label "Open Vault" \
		-menu MENU_FILE_VAULT -identifier E_OPEN]
	if {[variant KANGBANDTK ZANGBANDTK]} {
if 1 {
	lappend entries [list -type command -label "Open Quest..." \
		-underline 5 -identifier E_QUEST]
} else {
	lappend entries [list -type cascade -label "Open Quest" \
		-menu MENU_FILE_QUEST -identifier E_QUEST]
}
	lappend entries [list -type cascade -label "Open Town" \
		-menu MENU_FILE_TOWN -identifier E_TOWN]
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Save" \
		-underline 0 -accelerator $mod+S -identifier E_SAVE]
	lappend entries [list -type command -label "Save As..." \
		-underline 5 -identifier E_SAVE_AS]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Close" \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_FILE $entries

	#
	# Edit
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_EDIT
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_EDIT -label Edit -underline 0 \
		-identifier M_EDIT

	# Edit -> Selection
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 \
		-identifier MENU_EDIT_SELECTION]
	set entries {}
	lappend entries [list -type command -label "Erase Layer" \
		-underline 0 -identifier E_ERASE_LAYER]
	lappend entries [list -type command -label "Up 1 layer" \
		-underline 0 -identifier E_LAYER_UP]
	lappend entries [list -type command -label "Down 1 layer" \
		-underline 0 -identifier E_LAYER_DOWN]
	NSMenu::MenuInsertEntries $mbarId -end MENU_EDIT_SELECTION $entries

	set entries {}
	lappend entries [list -type command -label "Undo" \
		-underline 0 -accelerator $mod+Z -identifier E_UNDO]
	lappend entries [list -type command -label "Copy" \
		-underline 0 -accelerator $mod+C -identifier E_COPY]
	lappend entries [list -type command -label "Paste" \
		-underline 0 -accelerator $mod+V -identifier E_PASTE]
	lappend entries [list -type command -label "Delete" \
		-underline 0 -accelerator Del -identifier E_DELETE]
	lappend entries [list -type cascade -label "Selection" \
		-menu MENU_EDIT_SELECTION -identifier E_SELECTION]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Select All" \
		-underline 7 -accelerator $mod+A -identifier E_SELECT_ALL]
	lappend entries [list -type command -label "Select None" \
		-underline 7 -accelerator $mod+D -identifier E_SELECT_NONE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_EDIT $entries

	#
	# Vault
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_VAULT
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_VAULT -label Vault -underline 0 \
		-identifier M_VAULT
	set entries {}
	lappend entries [list -type command -label "Set Dimensions..." \
		-underline 0 -identifier E_SIZE]
	lappend entries [list -type command -label "Set Default Icons..." \
		-underline 0 -identifier E_DEFAULT]
	lappend entries [list -type command -label "Erase..." \
		-underline 0 -identifier E_ERASE]
	lappend entries [list -type command -label "Update Town Vault" \
		-underline 0 -identifier E_TO_TOWN]
	lappend entries [list -type command -label "Vault To Text" \
		-underline 0 -identifier E_TEXT]
	lappend entries [list -type separator]
	lappend entries [list -type checkbutton -label "Clobber Icons" \
		-variable NSVaultEditor($oop,clobberIcons) -identifier E_CLOBBER]
	NSMenu::MenuInsertEntries $mbarId -end MENU_VAULT $entries

	#
	# Plugin
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_PLUGIN]
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_PLUGIN -label Plugins -underline 0 \
		-identifier M_PLUGIN
	set menuDepth 0
	set menuNum 0
	set menuStack $menuId
	set menuStackIdent MENU_PLUGIN
	set Plugin(menuIdentList) {}
	foreach {depth label ident} $Plugin(menuInfo) {

		# Pop the menu stack if depth decreased
		if {$depth < $menuDepth} {
			set menuStack [lrange $menuStack 0 end-1]
			set menuStackIdent [lrange $menuStackIdent 0 end-1]
			incr menuDepth -1
		}

		# Menu
		if {$ident eq ""} {

			# Get the parent menu
			set parentMenuId [lindex $menuStack end]
			set parentMenuIdent [lindex $menuStackIdent end]

			# Create a new menu
			set menu2Id [NSObject::New NSMenu $parentMenuId -tearoff 0 \
				-identifier MENU_PLUGIN_$menuNum]

			# Append to parent menu
			NSMenu::MenuInsertEntry $mbarId -end $parentMenuIdent \
				-type cascade -menu MENU_PLUGIN_$menuNum -label $label \
				-identifier M_PLUGIN_$menuNum

			# Increase menu depth
			incr menuDepth

			# Push onto stack
			lappend menuStack $menu2Id
			lappend menuStackIdent MENU_PLUGIN_$menuNum

			lappend Plugin(menuIdentList) M_PLUGIN_$menuNum

			# Another menu was created
			incr menuNum

			continue
		}

		# Get the parent menu
		set parentMenuId [lindex $menuStack end]
		set parentMenuIdent [lindex $menuStackIdent end]

		# Add plugin entry
		NSMenu::MenuInsertEntry $mbarId -end $parentMenuIdent \
			-type command -label $label -identifier E_PLUGIN_$ident
	}

	#
	# Display
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_DISPLAY
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_DISPLAY -label Display -underline 0 \
		-identifier M_DISPLAY
	set entries {}
	lappend entries [list -type command -label "Toggle Edit Mode" \
		-underline 12 -accelerator Esc -identifier E_MODE]
	lappend entries [list -type command -label "Toggle Layout" \
		-underline 7 -accelerator F1 -identifier E_LAYOUT]
	lappend entries [list -type command -label "Toggle Scale" \
		-underline 7 -accelerator ` -identifier E_SCALE]
	lappend entries [list -type separator]
	lappend entries [list -type checkbutton -label "Palette" \
		-variable ::NSVaultEditor($oop,pal,show) \
		-underline 0 -identifier E_PALETTE]
	lappend entries [list -type checkbutton -label "Replace Tool" \
		-variable ::NSVaultEditor($oop,replace,show) \
		-underline 0 -identifier E_REPLACE]
	NSMenu::MenuInsertEntries $mbarId -end MENU_DISPLAY $entries

	#
	# Help
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_HELP
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_HELP -label Help -underline 0 \
		-identifier M_HELP
	set entries {}
	if {[variant ZANGBANDTK]} {
		lappend entries [list -type command -label "Town Number" \
			-underline 0 -identifier E_TOWN_NUMBER]
	}
	if {[variant KANGBANDTK]} {
		lappend entries [list -type command -label "Plot Number" \
			-underline 0 -identifier E_PLOT]
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Help" \
		-underline 0 -accelerator ? -identifier E_HELP]
	NSMenu::MenuInsertEntries $mbarId -end MENU_HELP $entries

	set MenuString(M_FILE) \
		"Contains commands for using files."
	set MenuString(E_NEW) \
		"Start a new blank vault."
	set MenuString(E_OPEN) \
		"Read in a vault file."
	set MenuString(E_QUEST) \
		"Read a q000000N.txt file."
	set MenuString(E_TOWN) \
		"Read a t000000N.txt file."
	set MenuString(E_SAVE) \
		"Save the current vault."
	set MenuString(E_SAVE_AS) \
		"Save the current vault as a new file."
	set MenuString(E_CLOSE) \
		"Closes the window."

	set MenuString(M_EDIT) \
		"Contains commands for editing."

	set MenuString(M_VAULT) \
		"Contains commands for using vaults."
	set MenuString(E_SIZE) \
		"Set vault height and width."
	set MenuString(E_DEFAULT) \
		"Set icons based on features."
	set MenuString(E_ERASE) \
		"Erase the current vault."
	set MenuString(E_TO_TOWN) \
		"Copy the current vault to the town vault."
	set MenuString(E_TEXT) \
		"Convert vault features to text representation."

	set MenuString(M_DISPLAY) \
		"Contains commands for changing the display."
	set MenuString(E_MODE) \
		"Change the edit mode."
	set MenuString(E_SCALE) \
		"Swap the scale of the display."

	set MenuString(M_HELP) \
		"Contains commands for getting help."
	set MenuString(E_TOWN_NUM) \
		"Display the current town number."
	set MenuString(E_PLOT) \
		"Display the character's plot number."
	set MenuString(E_HELP) \
		"Display help for the Vault Editor."

	NSMenu::BindAccels $mbarId $win

	return
}

# NSVaultEditor::MenuSetup --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::MenuSetup {oop mbarID} {

	variable Plugin

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]

	set isSel [$widget itemcget $sel -visible]

	lappend identList E_NEW E_OPEN E_QUEST E_TOWN E_SAVE_AS E_CLOSE
	if {[string length [Info $oop path,vault]]} {
		lappend identList E_SAVE
	}
	lappend identList E_UNDO E_SELECT_ALL
	if {$isSel} {
		lappend identList E_COPY E_DELETE E_SELECTION E_SELECT_NONE
	}
	if {[string length [Info $oop clip,bounds]]} {
		lappend identList E_PASTE
	}
	if {[Info $oop editMode] eq "icon"} {
		lappend identList E_ERASE_LAYER E_LAYER_UP E_LAYER_DOWN
	}
	lappend identList E_SIZE E_ERASE E_CLOBBER E_TEXT
	if {[Info $oop mode,default]} {
		lappend identList E_DEFAULT
	}
	if {[Info $oop mode,town]} {
		lappend identList E_TO_TOWN
	}

	Info $oop pal,show [winfo ismapped [Info $oop pal,win]]
	Info $oop replace,show [winfo ismapped [Info $oop replace,win]]
	lappend identList E_LAYOUT E_SCALE E_PALETTE E_REPLACE
	if {[Info $oop mode,toggle]} {
		lappend identList E_MODE
	}
	lappend identList E_PLOT E_TOWN_NUMBER E_HELP

	foreach ident $Plugin(menuIdentList) {
		lappend identList $ident
	}

	foreach ident $Plugin(list) {
		if {$Plugin($ident,file) ne ""} {
			set mtime [file mtime [CPathTk vault plugins $Plugin($ident,file)]]
			if {$mtime > $Plugin($ident,mtime)} {
				source [CPathTk vault plugins $Plugin($ident,file)]
				set Plugin($ident,mtime) $mtime
			}
		}
		if {($Plugin($ident,mode) eq "both") || ($Plugin($ident,mode) == [Info $oop editMode])} {
			if {$Plugin($ident,select)} {
				if {!$isSel} continue
			}
			lappend identList E_PLUGIN_$ident
		}
	}

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSVaultEditor::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		E_CLOBBER {
			if {[Info $oop clobberIcons]} {
				set desc "Preserve icon assignments when drawing features."
			} else {
				set desc "Clear icon assignments when drawing features."
			}
		}
		E_LAYOUT {
			if {[Info $oop fullScreen]} {
				set desc "Show the map and icon browser."
			} else {
				set desc "Hide the map and icon browser."
			}
		}
		E_PALETTE {
			if {[Info $oop pal,show]} {
				set desc "Hide the palette."
			} else {
				set desc "Show the palette."
			}
		}
		E_REPLACE {
			if {[Info $oop replace,show]} {
				set desc "Hide the replace tool."
			} else {
				set desc "Show the replace tool."
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

# NSVaultEditor::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::MenuInvoke {oop menuId ident args} {

	set win [Info $oop win]

	switch -glob -- $ident {
		E_NEW {
			SetVaultSize $oop [Info $oop max,height] [Info $oop max,width]
			Erase $oop
			SetMode $oop MODE_VAULT
			SetEditMode $oop icon
			SetTitle $oop Untitled
			Info $oop path,text ""
			Info $oop path,vault ""
		}
		E_OPEN {
			if {[llength $args]} {
				set path [lindex $args 0]
			} else {
				set types {
					{{Vault Files} ".vlt" VALT}
					{{All files} {*}}
				}
				set path [tk_getOpenFile -filetypes $types -parent $win \
					-initialdir [PathTk config] \
					-title "Choose vault file"]
				SetupFileMenu $oop
			}
			if {[string length $path]} {
				set message "The icons will be read in from the chosen vault file.\n"
				append message "If you choose to read in the vault features they will\n"
				append message "overwrite whatever is in the editor window now.\n\n"
				append message "Do you want the vault features to be read in as well?"
				set answer [tk_messageBox -title "Read Vault Features" \
					-parent $win -type yesno -message $message]
				NSMap::SetView [Info $oop mapId] 0 0
				NSMap::SetView [Info $oop map,mapId] 0 0
				SetMode $oop MODE_VAULT
				SetTitle $oop [file tail $path]
				ReadVault $oop $path $answer
			}
		}
		E_QUEST {
if 1 {
			OpenQuest $oop
} else {
			if {[llength $args]} {
				set path [lindex $args 0]
			} else {
				set path [tk_getOpenFile -parent $win \
					-initialdir [Path lib edit] \
					-title "Choose quest file"]
				SetupFileMenu $oop
			}
			if {[string length $path]} {
				Info $oop path,text [file tail $path]
				Info $oop path,vault ""
				Erase $oop
				ReadQuest $oop $path
			}
}
		}
		E_TOWN {
			if {[llength $args]} {
				set path [lindex $args 0]
			} else {
				set path [tk_getOpenFile -parent $win \
					-initialdir [Path lib edit] \
					-title "Choose town file"]
				SetupFileMenu $oop
			}
			if {[string length $path]} {
				Info $oop path,text [file tail $path]
				Info $oop path,vault ""
				Erase $oop
				ReadTown $oop $path
			}
		}
		E_SAVE {Save $oop [Info $oop path,vault]}
		E_SAVE_AS {
			set types {
				{{Vault Files} ".vlt" VALT}
				{{All files} {*}}
			}
			set file [file tail [Info $oop path,vault]]
			if {![string length $file]} {
				set file [file rootname [Info $oop path,text]]
				if {[string length $file]} {
					append file -[Global config,prefix].vlt
				} else {
					set file untitled-[Global config,prefix].vlt
				}
			}
			set path [tk_getSaveFile -filetypes $types -parent $win \
				-initialfile $file \
				-initialdir [PathTk config]]
			if {[string length $path]} {
				Save $oop $path
			}
		}
		E_CLOSE {Close $oop}
		E_UNDO {Undo $oop}
		E_COPY {Copy $oop}
		E_PASTE {Paste $oop}
		E_DELETE {Delete $oop}
		E_ERASE_LAYER {
			scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2
			EraseLayerIcons $oop display [Info $oop layer] $y1 $x1 $y2 $x2
		}
		E_LAYER_UP {
			set layer1 [Info $oop layer]
			set layer2 [expr {$layer1 + 1}]
			MoveLayer $oop $layer1 $layer2
		}
		E_LAYER_DOWN {
			set layer1 [Info $oop layer]
			set layer2 [expr {$layer1 - 1}]
			MoveLayer $oop $layer1 $layer2
		}
		E_SELECT_ALL {
			SetTool $oop Marquee
			set x2 [expr {[vault width [Info $oop display,vaultId]] - 1}]
			set y2 [expr {[vault height [Info $oop display,vaultId]] - 1}]
			Select $oop 0 0 $x2 $y2
		}
		E_SELECT_NONE {
			Deselect $oop
		}
		E_MODE {ToggleEditMode $oop}
		E_LAYOUT {
			if {[Info $oop fullScreen]} {
				$win.pwH add $win.pwV
				set fullScreen 0
				set text HIDE
			} else {
				$win.pwH forget $win.pwV
				set fullScreen 1
				set text SHOW
			}
			Info $oop fullScreen $fullScreen
			$win.statusBar itemconfigure t2 -text $text
		}
		E_SCALE {
			set displaySize [NSWidget::Info [Info $oop map,widgetId] scale]
			set mapSize [NSWidget::Info [Info $oop widgetId] scale]
			NSWidget::SetScale [Info $oop widgetId] $displaySize
			NSWidget::SetScale [Info $oop map,widgetId] $mapSize
			if {$displaySize == [icon width]} {
				[Info $oop map,widget] itemconfigure [Info $oop map,view] \
					-visible yes
			} else {
				[Info $oop map,widget] itemconfigure [Info $oop map,view] \
					-visible no
			}
		}
		E_PALETTE {
			if {[Info $oop pal,show]} {
				wm deiconify [Info $oop pal,win]
				raise [Info $oop pal,win]
			} else {
				wm withdraw [Info $oop pal,win]
			}
		}
		E_REPLACE {
			if {[Info $oop replace,show]} {
				wm deiconify [Info $oop replace,win]
				raise [Info $oop replace,win]
			} else {
				wm withdraw [Info $oop replace,win]
			}
		}
		E_SIZE {AskForSize $oop}
		E_DEFAULT {
			set answer [tk_messageBox -title "Set Default Icons" -parent $win \
				-type yesno -message "Really set icons to their defaults?"]
			if {[string equal $answer "no"]} return
			SetDefaultIcons $oop
		}
		E_ERASE {
			set answer [tk_messageBox -title "Erase Vault" -parent $win \
				-type yesno -message "Really erase the vault?"]
			if {[string equal $answer "no"]} return
			Erase $oop
		}
		E_TO_TOWN {
			if {[angband player depth]} return
			set vaultId [Info $oop display,vaultId]
			set vaultTownId [Global vault,current]
			vault current $vaultTownId
			set y2 [expr {[vault height $vaultId] - 1}]
			set x2 [expr {[vault width $vaultId] - 1}]
			foreach plane {icon1 icon2 icon3 icon4} {
				vault copy $plane $vaultId 0 0 $y2 $x2 $vaultTownId 0 0
			}
			vault copy feature $vaultId 0 0 $y2 $x2 $vaultTownId 0 0
			[Global main,widget] wipe
		}
		E_TEXT {
			VaultToText $oop
		}
		E_PLUGIN_* {
			variable Plugin
			scan $ident E_PLUGIN_%s plugin
			set cmd [lindex [split $plugin #] 0]
			eval $cmd $oop $Plugin($plugin,args)
		}
		E_TOWN_NUMBER {
			set message "Town Number: [struct set player_type 0 town_num]"
			tk_messageBox -parent $win -title "Town Number" -message $message
		}
		E_PLOT {
			set message "Plot Number: [struct set player_type 0 plot_num]"
			tk_messageBox -parent $win -title "Plot Number" -message $message
		}
		E_HELP {
			angband_display help show \
				[CPathTk vault vault-editor.html] 0
		}
	}

	return
}

# NSVaultEditor::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::DisplayCmd {oop message first} {

	variable Priv

	set win [Info $oop win]

	switch -- $message {
		preDisplay {
		}
		postDisplay {
			if {$first} {
				NSMap::SetView [Info $oop mapId] 0 0
				NSMap::SetView [Info $oop map,mapId] 0 0
				focus [Info $oop widget]

				set browserId [Info $oop browserId]
				set treeG [NSIconBrowser::Info $browserId group,tree]
				NSIconBrowser::SetList_Group $browserId
				NSList::Activate $treeG "root child 0"

				if {0 && [angband player depth]} {
					set message "You should be on the town level when "
					append message "editing a vault to ensure that the proper "
					append message "icon assignments are made."
					tk_messageBox -parent [Info $oop win] \
						-parent $win -title "Vault Editor" -message $message
				}
			}
			if {[Info $oop pal,show]} {
# not needed if transient Aug 9 2004
#				wm deiconify [Info $oop pal,win]
			}
			if {[Info $oop replace,show]} {
# not needed if transient Aug 9 2004
#				wm deiconify [Info $oop replace,win]
			}
		}
		preWithdraw {
			set win2 [Info $oop pal,win]
			if {[winfo ismapped $win2]} {
# not needed if transient Aug 9 2004
#				wm withdraw $win2
				Info $oop pal,show 1
			} else {
				Info $oop pal,show 0
			}
			set win2 [Info $oop replace,win]
			if {[winfo ismapped $win2]} {
# not needed if transient Aug 9 2004
#				wm withdraw $win2
				Info $oop replace,show 1
			} else {
				Info $oop replace,show 0
			}
		}
		postWithdraw {
		}
	}

	return
}

# NSVaultEditor::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]
	set x 0
	set y 0
	set width [NSToplevel::ContentWidth $win [winfo screenwidth $win]]
	scan [angband system workarea] "%d %d %d %d" left top right bottom
	if {[Platform unix]} {
		set left 0
		set top 0
		set right [winfo screenwidth .]
		set bottom [winfo screenheight .]
	}
	set height [NSToplevel::ContentHeight $win [expr {$bottom - $top}]]
	return ${width}x$height+$x+$y
}

# NSVaultEditor::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetupCmd {oop} {

	set win [Info $oop win]

	set x [expr {int([winfo width $win.pwH] * 0.75)}]
	$win.pwH sash place 0 $x 0

	set y [expr {int([winfo height $win.pwV] * 0.5)}]
	$win.pwV sash place 0 0 $y

if 0 { # Aug 9 2004
	set win [Info $oop win]
	set browserId [Info $oop browserId]

	# Set the minimum size of the icon browser cell so that it doesn't
	# change size when swapping in the feature list. For some reason
	# the height and width are larger than the requested dimensions.
#	scan [grid bbox $win.frameRight 1 1] "%d %d %d %d" x y width height
	grid rowconfigure $win.frameRight 1 \
		-minsize [winfo reqheight [NSIconBrowser::Info $browserId frame]]
	grid columnconfigure $win.frameRight 0 \
		-minsize [winfo reqwidth [NSIconBrowser::Info $browserId frame]]

	grid $win.frameRight.frameFeature
	update idletasks
	grid remove $win.frameRight.frameFeature
}
	return
}

# NSVaultEditor::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Close {oop} {

	NSWindowManager::Undisplay vaulteditor

	return
}

# NSVaultEditor::SetupFileMenu --
#
#	Set the File -> Quest and File -> Town submenus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetupFileMenu {oop} {

	set mbarId [Info $oop mbarId]

	# Get a list of vault files in tk\config
	set menuInfo [NSMenu::MenuFindEntry $mbarId MENU_FILE_VAULT]
	set menuId [lindex $menuInfo 0]
	set menu [NSMenu::Info $menuId menu]
	$menu delete 0 end
	set glob [glob -nocomplain -directory [PathTk config] *.vlt]
	set glob [lsort -dictionary $glob]
	set count 0
	foreach path $glob {
		set name [file tail $path]
		$menu add command -label $name -columnbreak [expr {$count % 15 == 0}] \
			-command [list NSVaultEditor::MenuInvoke $oop $menuId E_OPEN $path]
		incr count
	}
	if {[$menu index end] != "none"} {
		$menu add separator
	}
	$menu add command -label "Other..." \
		-command "NSVaultEditor::MenuInvoke $oop $menuId E_OPEN"

if 0 {
	# Get a list of quest files in lib\edit
	set menuInfo [NSMenu::MenuFindEntry $mbarId MENU_FILE_QUEST]
	set menuId [lindex $menuInfo 0]
	set menu [NSMenu::Info $menuId menu]
	$menu delete 0 end
	set glob [glob -nocomplain -directory [Path lib edit] q*.txt]
	set glob [lsort -dictionary $glob]
	set count 0
	foreach path $glob {
		set name [file tail $path]
		if {[string equal $name "q_info.txt"]} continue
		if {[string equal $name "q_pref.txt"]} continue
		$menu add command -label $name -columnbreak [expr {$count % 15 == 0}] \
			-command [list NSVaultEditor::MenuInvoke $oop $menuId E_QUEST $path]
		incr count
	}
	if {[$menu index end] != "none"} {
		$menu add separator
	}
	$menu add command -label "Other..." \
		-command "NSVaultEditor::MenuInvoke $oop $menuId E_QUEST"
}

	# Get a list of town files in lib\edit
	set menuInfo [NSMenu::MenuFindEntry $mbarId MENU_FILE_TOWN]
	set menuId [lindex $menuInfo 0]
	set menu [NSMenu::Info $menuId menu]
	$menu delete 0 end
	set pattern t*.txt
	set glob [glob -nocomplain -directory [Path lib edit] $pattern]
	set glob [lsort -dictionary $glob]
	set count 0
	foreach path $glob {
		set name [file tail $path]
		if {[string equal $name "t_info.txt"]} continue
		if {[string equal $name "t_pref.txt"]} continue
		$menu add command -label $name -columnbreak [expr {$count % 15 == 0}] \
			-command [list NSVaultEditor::MenuInvoke $oop $menuId E_TOWN $path]
		incr count
	}
	if {[$menu index end] != "none"} {
		$menu add separator
	}
	$menu add command -label "Other..." \
		-command "NSVaultEditor::MenuInvoke $oop $menuId E_TOWN"

	return
}

# NSVaultEditor::InitVaults --
#
#	vault create.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::InitVaults {oop} {

	# Vault for display
	set vaultId [vault create]
	Info $oop display,vaultId $vaultId

	# Remember max dimensions
	Info $oop max,width [vault width $vaultId]
	Info $oop max,height [vault height $vaultId]

	# Vault for switching edit modes
	Info $oop edit,vaultId [vault create]

	# Display shares data with Edit
	vault share feature $vaultId [Info $oop edit,vaultId]
	vault share symbol $vaultId [Info $oop edit,vaultId]
	vault share letter $vaultId [Info $oop edit,vaultId]
	vault share symbol $vaultId [Info $oop edit,vaultId]

	# Vault for copy/paste
	Info $oop clip,vaultId [vault create]

	# Up-to-date vault, minus the last draw operation. Used for undo.
	# We need two vaults to parallel Display and Edit.
	Info $oop undo,display,vaultId [set v1 [vault create]]
	Info $oop undo,edit,vaultId [set v2 [vault create]]
	vault share feature $v1 $v2
	vault share letter $v1 $v2
	vault share symbol $v1 $v2

	return
}

# NSVaultEditor::IconCfg --
#
#	Called when the game's icon configuration changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::IconCfg {oop} {

	InitVaults $oop

	[Info $oop widget] configure -vaultnum [Info $oop display,vaultId]
	[Info $oop map,widget] configure -vaultnum [Info $oop display,vaultId]

	# Font
	if {[lsearch -exact [icon gettypes] vault] == -1} {
		icon createtype vault -font "Courier 24 bold"

		Global vault,attr,monster [icon ascii count]
		icon ascii create -foreground [palette set 0] \
			-background [palette set 222]

		Global vault,attr,object [icon ascii count]
		icon ascii create -foreground [palette set 0] \
			-background [palette set 232]
	}

	MenuInvoke $oop "" E_NEW
	VaultSetup $oop
	SetFeatureList $oop

	return
}

# NSVaultEditor::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label $win.statusBar.label0
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSVaultEditor::Press1 --
#
#	Handle <ButtonPress-1> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Press1 {oop x y {control 0}} {

	variable Priv

	set vaultId [Info $oop display,vaultId]
	set widgetId [Info $oop widgetId]
	set widget [Info $oop widget]

	scan [NSWidget::PointToCave $widgetId $x $y] "%d %d" y x

	set height [vault height $vaultId]
	set width [vault width $vaultId]

	if {$y < 0} {
		set y 0
	}
	if {$y >= $height} {
		set y [expr {$height - 1}]
	}
	if {$x < 0} {
		set x 0
	}
	if {$x >= $width} {
		set x [expr {$width - 1}]
	}

	Info $oop anchorPos "$y $x"
	Info $oop curPos "$y $x"
	Info $oop endPos "$y $x"

	if {[InSelection $oop $y $x] && ![Info $oop dragsel]} {
		Info $oop tool,prev,tool [Info $oop tool,tool]
		Info $oop tool,prev,cmd [Info $oop tool,command]
		Info $oop tool,tool DragSel
		Info $oop tool,command Feedback_DragSel
	}

	Info $oop click,ctrl $control

	Info $oop track,press 1
	Info $oop track,release 0

	Info $oop track,first 1
	TrackOnce $oop
	Info $oop track,first 0

	return
}

# NSVaultEditor::Motion1 --
#
#	Handle <Button1-Motion> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Motion1 {oop x1 y1} {

	variable Priv

	set vaultId [Info $oop display,vaultId]
	set widgetId [Info $oop widgetId]
	set widget [Info $oop widget]
	set cursor [Info $oop widget,cursor]

	if {$x1 >= [winfo width $widget]} {
		set x1 [winfo width $widget]
		incr x1 -1
	} elseif {$x1 < 0} {
		set x1 0
	}
	if {$y1 >= [winfo height $widget]} {
		set y1 [winfo height $widget]
		incr y1 -1
	} elseif {$y1 < 0} {
		set y1 0
	}
	scan [NSWidget::PointToCave $widgetId $x1 $y1] "%d %d" y x

	set height [vault height $vaultId]
	set width [vault width $vaultId]

	if {$y < 0} {
		set y 0
	}
	if {$y >= $height} {
		set y [expr {$height - 1}]
	}
	if {$x < 0} {
		set x 0
	}
	if {$x >= $width} {
		set x [expr {$width - 1}]
	}

	if {[string equal [Info $oop curPos] "$y $x"]} return
	Info $oop prevPos [Info $oop curPos]
	Info $oop curPos "$y $x"

	if {[Info $oop prevPos] ne ""} {
		scan [Info $oop prevPos] "%d %d" y2 x2
		Info $oop track,dy,prev [expr {$y - $y2}]
		Info $oop track,dx,prev [expr {$x - $x2}]
	}

	scan [Info $oop anchorPos] "%d %d" y2 x2
	Info $oop track,dy [expr {$y - $y2}]
	Info $oop track,dx [expr {$x - $x2}]

	$widget itemconfigure $cursor -x $x -y $y -visible yes

	TrackOnce $oop

	Info $oop endPos "$y $x"

	return
}

# NSVaultEditor::Release1 --
#
#	Handle <ButtonRelease-1> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Release1 {oop x y} {

	# Tk Bug: button-release in menu arrives in window
	if {![Info $oop track,press]} return
	Info $oop track,press 0

	Info $oop track,release 1
	Info $oop curPos ""
	Motion1 $oop $x $y

	return
}

# NSVaultEditor::TrackOnce --
#
#	Draw stuff when the mouse moves.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::TrackOnce {oop} {

	variable Priv

	set vaultId [Info $oop display,vaultId]
	set widget [Info $oop widget]

	scan [Info $oop curPos] "%d %d" y x

	eval [Info $oop tool,command] $oop $y $x

	set widgetId [Info $oop widgetId]
	ExamineCmd $oop $widgetId $y $x {}

	return
}

# NSVaultEditor::SetLayer --
#
#	Change the current icon layer.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetLayer {oop layer} {

	set oldLayer [Info $oop layer]
	if {$layer == $oldLayer} return

	[Info $oop button,layer$oldLayer] configure -relief raised
	[Info $oop button,layer$layer] configure -relief sunken

	Info $oop layer $layer

	return
}

# NSVaultEditor::SetTool --
#
#	Change the current drawing tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetTool {oop tool} {

	set oldTool [Info $oop tool,tool]
	if {$tool == $oldTool} return

	[Info $oop button,$oldTool] configure -relief raised
	[Info $oop button,$tool] configure -relief sunken

	# Remember the previous tool
	if {[string equal $tool "EyeDrop"]} {
		Info $oop tool,eyedrop $oldTool
	}

	Info $oop tool,tool $tool
	Info $oop tool,command Feedback_$tool

	if {[string equal $tool "Marquee"]} {
		set visible yes
	} else {
		set visible no
	}

	# Hide marquee when tool changes
	Deselect $oop

	return
}

# NSVaultEditor::SetToolIcon --
#
#	Set the icon used for drawing.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetToolIcon {oop icon} {

	set canvas [Info $oop canvasTool]

	$canvas itemconfigure tool -assign "icon $icon" -assignbg "icon none 0"
	Info $oop tool,icon $icon

Replace_SetIcon $oop $icon

	return
}

# NSVaultEditor::CopyIcons --
#
#	Copy all icon layers from a region of one vault to another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::CopyIcons {oop source y1 x1 y2 x2 dest y3 x3} {

	set vaultIdSrc [Info $oop $source,vaultId]
	set vaultIdDst [Info $oop $dest,vaultId]

	foreach plane {icon1 icon2 icon3 icon4} {
		vault copy $plane $vaultIdSrc $y1 $x1 $y2 $x2 $vaultIdDst $y3 $x3
	}

	return
}

# NSVaultEditor::CopyIconsAll --
#
#	Copy all icon layers from one vault to another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::CopyIconsAll {oop source dest} {

	set vaultIdSrc [Info $oop $source,vaultId]
	set vaultIdDst [Info $oop $dest,vaultId]

	set y2 [expr {[vault height $vaultIdSrc] - 1}]
	set x2 [expr {[vault width $vaultIdSrc] - 1}]

	CopyIcons $oop $source 0 0 $y2 $x2 $dest 0 0

	return
}

# NSVaultEditor::CopyArea --
#
#	Copy a region of one vault to another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::CopyArea {oop source y1 x1 y2 x2 dest y3 x3} {

	set vaultIdSrc [Info $oop $source,vaultId]
	set vaultIdDst [Info $oop $dest,vaultId]

	CopyIcons $oop $source $y1 $x1 $y2 $x2 $dest $y3 $x3
	vault copy feature $vaultIdSrc $y1 $x1 $y2 $x2 $vaultIdDst $y3 $x3
	vault copy letter $vaultIdSrc $y1 $x1 $y2 $x2 $vaultIdDst $y3 $x3
	vault copy symbol $vaultIdSrc $y1 $x1 $y2 $x2 $vaultIdDst $y3 $x3

	return
}

# NSVaultEditor::CopyAll --
#
#	Copy all of one vault to another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::CopyAll {oop source dest} {

	set vaultIdSrc [Info $oop $source,vaultId]
	set vaultIdDst [Info $oop $dest,vaultId]

	set y2 [expr {[vault height $vaultIdSrc] - 1}]
	set x2 [expr {[vault width $vaultIdSrc] - 1}]

	CopyArea $oop $source 0 0 $y2 $x2 $dest 0 0

	return
}

# NSVaultEditor::CopyPlaneAll --
#
#	Copy all of a single plane from one vault to another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::CopyPlaneAll {oop plane source dest} {

	set vaultIdSrc [Info $oop $source,vaultId]
	set vaultIdDst [Info $oop $dest,vaultId]

	set y2 [expr {[vault height $vaultIdSrc] - 1}]
	set x2 [expr {[vault width $vaultIdSrc] - 1}]

	vault copy $plane $vaultIdSrc 0 0 $y2 $x2 $vaultIdDst 0 0

	return
}

# NSVaultEditor::ToUndo --
#
#	Copy the current vault to the undo vault.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ToUndo {oop} {

	CopyAll $oop display undo,display
	CopyAll $oop edit undo,edit

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]
	Info $oop undo,select,visible [$widget itemcget $sel -visible]
	Info $oop undo,select,bounds [Info $oop select,bounds]
	Info $oop undo,select,float [Info $oop sel,float]

	return
}

# NSVaultEditor::FromUndo --
#
#	Copy the undo vault to the current vault.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::FromUndo {oop} {

	CopyAll $oop undo,display display
	CopyAll $oop undo,edit edit

	return
}

# NSVaultEditor::Undo --
#
#	Undo the last drawing operation.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Undo {oop} {

	vault swap [Info $oop display,vaultId] [Info $oop undo,display,vaultId]
	vault swap [Info $oop edit,vaultId] [Info $oop undo,edit,vaultId]

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]
	set visible [Info $oop undo,select,visible]
	set bounds [Info $oop undo,select,bounds]
	set float [Info $oop undo,select,float]
	Info $oop undo,select,visible [$widget itemcget $sel -visible]
	Info $oop undo,select,bounds [Info $oop select,bounds]
	Info $oop undo,select,float [Info $oop sel,float]
	if {$visible} {
		eval Select $oop $bounds
		Info $oop sel,float $float
	} else {
		Deselect $oop
	}

	return
}

# NSVaultEditor::Copy --
#
#	Copy the selected area to the Clip vault.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Copy {oop} {

	scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2
	CopyArea $oop display $y1 $x1 $y2 $x2 clip $y1 $x1
	Info $oop clip,bounds "$y1 $x1 $y2 $x2"

	return
}

# NSVaultEditor::Paste --
#
#	Copy from Clip to Display.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Paste {oop} {

	set vaultId [Info $oop display,vaultId]
	set widgetId [Info $oop widgetId]
	set widget [Info $oop widget]

if 0 {
	set pos [NSWidget::Info $widgetId examined]
	if {[string equal $pos ""]} return
	scan $pos "%d %d" y3 x3
}
	set bounds [Info $oop clip,bounds]
	if {[string equal $bounds ""]} return
	scan $bounds "%d %d %d %d" y1 x1 y2 x2

	# Paste to middle of display
	set widthClip [expr {$x2 - $x1 + 1}]
	set heightClip [expr {$y2 - $y1 + 1}]
	scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
	set heightW [expr {$y_max - $y_min + 1}]
	set widthW [expr {$x_max - $x_min + 1}]
	scan [$widget center] "%d %d" cy cx
	set left $x_min
	set top $y_min
# FIXME: verify paste fits in the vault
	set col [expr {($widthW - $widthClip) / 2}]
	set row [expr {($heightW - $heightClip) / 2}]
	if {$col < 0} { set col 0 }
	if {$row < 0} { set row 0 }
	set y3 [expr {$top + $row}]
	set x3 [expr {$left + $col}]
	set heightV [vault height $vaultId]
	set widthV [vault width $vaultId]
	if {$y3 + $heightClip >= $heightV} {
		incr y3 [expr {$heightV - ($y3 + $heightClip)}]
	} elseif {$y3 < 0} {
		set y3 0
	}
	if {$x3 + $widthClip >= $widthV} {
		incr x3 [expr {$widthV - ($x3 + $widthClip)}]
	} elseif {$x3 < 0} {
		set x3 0
	}

	# Save undo info before paste
	ToUndo $oop

	# Copy from Clipboard to Display
	CopyArea $oop clip $y1 $x1 $y2 $x2 display $y3 $x3

	# Create a floating selection for dragging
	set y4 [expr {$y3 + ($y2 - $y1)}]
	set x4 [expr {$x3 + ($x2 - $x1)}]
	SetTool $oop Marquee
	Select $oop $y3 $x3 $y4 $x4
	Info $oop sel,float 1
	Info $oop dragsel,erase 0

return

	# When pasting new features, must set icons correctly
	if {[string equal [Info $oop editMode] "icon"]} {
		variable feat2icon
		set vaultEditId [Info $oop edit,vaultId]
		for {set y $y1} {$y <= $y2} {incr y} {
			set iconRow [set iconbgRow {}]
			for {set x $x1} {$x <= $x2} {incr x} {
				set feat [vault get $vaultEditId feature $y $x]
				lappend iconRow $feat2icon($feat)
				lappend iconbgRow $feat2iconbg($feat)
			}
			vault put $vaultEditId icon2 [list $iconRow] $y3 $x3
			vault put $vaultEditId icon1 [list $iconbgRow] $y3 $x3
		}
	}

	return
}

# NSVaultEditor::VaultPut --
#
#	Put icons and features into the vault.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::VaultPut {oop y x iconList featureList letterList symbolList} {

	set editMode [Info $oop editMode]
	set vaultId [Info $oop display,vaultId]

	# Editing icons
	if {[string equal $editMode "icon"]} {
		vault put $vaultId icon[Info $oop layer] $iconList $y $x

	# Editing features
	} else {
		vault put $vaultId icon1 $iconList $y $x
		vault put $vaultId feature $featureList $y $x
		vault put $vaultId letter $letterList $y $x
		vault put $vaultId symbol $symbolList $y $x

		# Option: clear icon in Edit for these features
		if {[Info $oop clobberIcons]} {
			set iconList2 {}
			foreach row $iconList {
				foreach col $row {
					lappend iconRow {none 0}
				}
				lappend iconList2 $iconRow
			}
			set vaultId [Info $oop edit,vaultId]
			foreach layer {1 2 3 4} {
				vault put $vaultId icon$layer $iconList2 $y $x
			}
		}
	}

	return
}

# NSVaultEditor::Feedback_EyeDrop --
#
#	Handle the eyedrop tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_EyeDrop {oop y x} {

	set vaultId [Info $oop display,vaultId]

	if {[Info $oop track,release]} {
		SetTool $oop [Info $oop tool,eyedrop]
		return
	}

	# Editing icons
	if {[string equal [Info $oop editMode] "icon"]} {

		set layer [Info $oop layer]

		# Control-click for top icon
		if {[Info $oop click,ctrl]} {
			foreach plane {icon4 icon3 icon2 icon1} {
				set icon [vault get $vaultId $plane $y $x]
				if {$icon != "none 0"} {
					break
				}
			}

		# Click to pickup icon of current layer(s)
		} else {
			set icon [vault get $vaultId icon$layer $y $x]
		}
		SetToolIcon $oop $icon

	# Editing features
	} else {
		set icon [vault get $vaultId icon1 $y $x]
		Info $oop tool,feature [vault get $vaultId feature $y $x]
		Info $oop tool,letter [vault get $vaultId letter $y $x]
		Info $oop tool,symbol [vault get $vaultId symbol $y $x]
		SetToolIcon $oop $icon

		# Tool icon looks like actual feature (unless ascii)
		variable char2ass
		variable char2assbg
		set letter [vault get $vaultId letter $y $x]
		set ass $char2ass($letter)
		set assbg $char2assbg($letter)
		set canvas [Info $oop canvasTool]
		$canvas itemconfigure tool -assign $ass -assignbg $assbg
	}

	return
}

proc NSVaultEditor::bounds {min value max} {

	if {$value < $min} {
		return $min
	}
	if {$value > $max} {
		return $max
	}
	return $value
}

# NSVaultEditor::Flood --
#
#	Recursively change icons matching "icon" into "icon2".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Flood {oop vaultId y x} {

	variable Priv

	# Avoid infinite loop (in case of bug)
	if {[incr Priv(flood,count)] >= 500} return

	set y_min [bounds 0 [expr {$y - 1}] $Priv(flood,y_max)]
	set y_max [bounds 0 [expr {$y + 1}] $Priv(flood,y_max)]

	set x_min [bounds 0 [expr {$x - 1}] $Priv(flood,x_max)]
	set x_max [bounds 0 [expr {$x + 1}] $Priv(flood,x_max)]

	if {[Info $oop editMode] eq "icon"} {
		set plane icon[Info $oop layer]
	} else {
		set plane feature
	}

	VaultPut $oop $y $x $Priv(flood,new,iconList) \
		$Priv(flood,new,featureList) \
		$Priv(flood,new,letterList) \
		$Priv(flood,new,symbolList)

	for {set y2 $y_min} {$y2 <= $y_max} {incr y2}  {
		for {set x2 $x_min} {$x2 <= $x_max} {incr x2} {
			if {($x == $x2) && ($y == $y2)} continue
			set value [vault get $vaultId $plane $y2 $x2]
			if {[string compare $value $Priv(flood,old,$plane)]} continue
			Flood $oop $vaultId $y2 $x2
		}
	}

	return
}

# NSVaultEditor::Feedback_Flood --
#
#	Handle the flood tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_Flood {oop y x} {

	variable Priv

	if {![Info $oop track,first]} return

	ToUndo $oop

	set vaultId [Info $oop display,vaultId]
	set layer [Info $oop layer]

	StatusBar $oop "Flood fill in progress..." 0
	update idletasks

	set Priv(flood,count) 0

	set Priv(flood,y_max) [expr {[vault height $vaultId] - 1}]
	set Priv(flood,x_max) [expr {[vault width $vaultId] - 1}]

	set Priv(flood,old,icon$layer) [vault get $vaultId icon$layer $y $x]
	set Priv(flood,old,feature) [vault get $vaultId feature $y $x]
	set Priv(flood,old,letter) [vault get $vaultId letter $y $x]

	set Priv(flood,new,icon$layer) [Info $oop tool,icon]
	set Priv(flood,new,iconList) [list [list [Info $oop tool,icon]]]
	set Priv(flood,new,feature) [Info $oop tool,feature]
	set Priv(flood,new,featureList) [list [list [Info $oop tool,feature]]]
	set Priv(flood,new,letter) [Info $oop tool,letter]
	set Priv(flood,new,letterList) [list [list [Info $oop tool,letter]]]
	set Priv(flood,new,symbol) [Info $oop tool,symbol]
	set Priv(flood,new,symbolList) [list [list [Info $oop tool,symbol]]]

	# Control-click to change all matching grids in the selected region
	if {0 && [Info $oop click,ctrl]} {
		scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2
		set plane [Info $oop editMode]
		if {$plane eq "icon"} {
			set plane icon$layer
		}
		for {set y $y1} {$y <= $y2} {incr y} {
			for {set x $x1} {$x <= $x2} {incr x} {
				set value [vault get $vaultId $plane $y $x]
				if {[string compare $value $Priv(flood,old,$plane)]} continue
				VaultPut $oop $y $x $Priv(flood,new,iconList) \
					$Priv(flood,new,featureList) \
					$Priv(flood,new,letterList) \
					$Priv(flood,new,symbolList)
				incr Priv(flood,count)
			}
		}
	} else {	
		Flood $oop $vaultId $y $x
	}

	StatusBar $oop "Replaced $Priv(flood,count) grids." 1

	return
}

# NSVaultEditor::Feedback_Pencil --
#
#	Handle the pencil tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_Pencil {oop y x} {

	if {[Info $oop track,first]} {
		ToUndo $oop
	}

	if {[Info $oop track,release]} {
		return
	}

	set vaultId [Info $oop display,vaultId]

	if {[Info $oop editMode] eq "icon"} {
		set icon [Info $oop tool,icon]
		scan $icon "%s %d" type index
		set layer [Info $oop layer]
		# Transparent. Never put in layer 1
		if {[icon transparent $type $index]} {
			if {[Info $oop click,ctrl]} {
				foreach layer {2 3 4} {
					set icon2 [vault get $vaultId icon$layer $y $x]
					if {$icon2 eq "none 0"} break
				}
			} else {
				# Always draw NONE in current layer
				if {$layer == 1 && [string compare $icon {none 0}]} {
					set layer 2
				}
			}
		# Not transparent. Always put in layer 1
		} else {
			set layer 1
		}
		set iconList [list [list $icon]]
		vault put $vaultId icon$layer $iconList $y $x

		# Show the icons in the layers
		set canvas [Info $oop layer,canvas]
		foreach layer {1 2 3 4} {
			$canvas itemconfigure icon$layer \
				-assign "icon [vault get $vaultId icon$layer $y $x]"
		}

	} else {
		set iconList [list [list [Info $oop tool,icon]]]
		set featureList [list [list [Info $oop tool,feature]]]
		set letterList [list [list [Info $oop tool,letter]]]
		set symbolList [list [list [Info $oop tool,symbol]]]
		VaultPut $oop $y $x $iconList $featureList $letterList $symbolList
	}

	return
}

# NSVaultEditor::Feedback_Pencil --
#
#	Handle the eraser tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_Erase {oop y x} {

	if {[Info $oop track,first]} {
		ToUndo $oop
	}

	if {[Info $oop track,release]} {
		return
	}

	set vaultId [Info $oop display,vaultId]

	if {[Info $oop editMode] eq "icon"} {

		set layer [Info $oop layer]
		set iconList [list [list {none 0}]]

		# Control-click erases top-most layer
		if {[Info $oop click,ctrl]} {
			foreach plane {icon4 icon3 icon2 icon1} {
				set icon [vault get $vaultId $plane $y $x]
				if {$icon != "none 0"} {
					break
				}
			}
		} else {
			set plane icon$layer
		}
		vault put $vaultId $plane $iconList $y $x
	} else {
		set iconList [list [list {none 0}]]
		set featureList [list [list 1]]
		set letterList [list [list .]]
		set symbolList [list [list sym.floor0]]
		VaultPut $oop $y $x $iconList $featureList $letterList $symbolList
	}

	return
}

# NSVaultEditor::Feedback_Marquee --
#
#	Handle the marquee/selection tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_Marquee {oop y x} {

	if {[Info $oop track,release]} {
		return
	}

	scan [Info $oop anchorPos] "%d %d" y2 x2

	if {$y < $y2} {
		set top $y
		set bottom $y2
	} else {
		set top $y2
		set bottom $y
	}

	if {$x < $x2} {
		set left $x
		set right $x2
	} else {
		set left $x2
		set right $x
	}

	if {$left == $right} {
#		incr right
	}
	if {$top == $bottom} {
#		incr bottom
	}

	Select $oop $top $left $bottom $right

	return
}

# NSVaultEditor::Feedback_DragSel --
#
#	Handle dragging selected area.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_DragSel {oop y x} {

	set vaultId [Info $oop display,vaultId]
	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]

	if {[Info $oop track,first]} {
		if {![Info $oop sel,float]} {
			ToUndo $oop
			# Use the clipboard to remember what is being dragged
			Copy $oop
			Info $oop dragsel,erase [expr {![Info $oop click,ctrl]}]
			Info $oop sel,float 1
		}
		Info $oop dragsel,bounds,start [Info $oop select,bounds]
		Info $oop dragsel,bounds,end [Info $oop select,bounds]

		# Hide selection during drag
		$widget itemconfigure $sel -visible no
		return
	}

	if {[Info $oop track,release]} {
		# Show the selection again in the final location
		eval Select $oop [Info $oop dragsel,bounds,end]
		Info $oop sel,float 1
		Info $oop tool,tool [Info $oop tool,prev,tool]
		Info $oop tool,command [Info $oop tool,prev,cmd]
		return
	}

	scan [Info $oop clip,bounds] "%d %d %d %d" y1 x1 y2 x2
	scan [Info $oop dragsel,bounds,start] "%d %d %d %d" y3 x3 y4 x4

	set dy [Info $oop track,dy]
	set dx [Info $oop track,dx]

	set height [vault height $vaultId]
	set width [vault width $vaultId]

	if {$y3 + $dy < 0} {
		incr dy [expr {0 - ($y3 + $dy)}]
	} elseif {$y4 + $dy >= $height} {
		incr dy [expr {$height - ($y4 + $dy) - 1}]
	}
	if {$x3 + $dx < 0} {
		incr dx [expr {0 - ($x3 + $dx)}]
	} elseif {$x4 + $dx >= $width} {
		incr dx [expr {$width - ($x4 + $dx) - 1}]
	}

	incr y3 $dy
	incr x3 $dx
	incr y4 $dy
	incr x4 $dx

	FromUndo $oop
	if {[Info $oop dragsel,erase]} {
		EraseArea $oop display $y1 $x1 $y2 $x2
	}
	CopyArea $oop clip $y1 $x1 $y2 $x2 display $y3 $x3

	Info $oop dragsel,bounds,end "$y3 $x3 $y4 $x4"

	return
}

# NSVaultEditor::Feedback_RectAux --
#
#	Draw a filled or unfilled rectangle.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_RectAux {oop y x filled} {

	if {[Info $oop track,first]} {
		ToUndo $oop
	}

	if {[Info $oop track,release]} {
# Oct 29 2004
[Info $oop widget] itemconfigure [Info $oop widget,sel] -visible no
		return
	}

	set editMode [Info $oop editMode]
	set vaultId [Info $oop display,vaultId]

	scan [Info $oop anchorPos] "%d %d" y2 x2

	if {$y < $y2} {
		set top $y
		set bottom $y2
	} else {
		set top $y2
		set bottom $y
	}

	if {$x < $x2} {
		set left $x
		set right $x2
	} else {
		set left $x2
		set right $x
	}

# Oct 29 2004
[Info $oop widget] itemconfigure [Info $oop widget,sel] -x1 $left -y1 $top -x2 $right -y2 $bottom -visible yes

	FromUndo $oop

	set icon [Info $oop tool,icon]
	set feature [Info $oop tool,feature]
	set letter [Info $oop tool,letter]
	set symbol [Info $oop tool,symbol]

	if {$filled} {

		set iconList [set featureList [set letterList {}]]
		for {set y $top} {$y <= $bottom} {incr y} {
			set rowIcon {}
			set rowFeature {}
			set rowLetter {}
			set rowSymbol {}
			for {set x $left} {$x <= $right} {incr x} {
				lappend rowIcon $icon
				lappend rowFeature $feature
				lappend rowLetter $letter
				lappend rowSymbol $symbol
			}
			lappend iconList $rowIcon
			lappend featureList $rowFeature
			lappend letterList $rowLetter
			lappend symbolList $rowSymbol
		}
		VaultPut $oop $top $left $iconList $featureList $letterList $symbolList

	# Unfilled
	} else {

		# Left/Right
		set iconList [set featureList [set letterList [set symbolList {}]]]
		for {set y $top} {$y <= $bottom} {incr y} {
			lappend iconList [list $icon]
			lappend featureList [list $feature]
			lappend letterList [list $letter]
			lappend symbolList [list $symbol]
		}
		VaultPut $oop $top $left $iconList $featureList $letterList $symbolList
		if {$left != $right} {
			VaultPut $oop $top $right $iconList $featureList $letterList $symbolList
		}

		# Top/Bottom
		set iconRow [set featureRow [set letterRow [set symbolRow {}]]]
		for {set x $left} {$x <= $right} {incr x} {
			lappend iconRow $icon
			lappend featureRow $feature
			lappend letterRow $letter
			lappend symbolRow $symbol
		}
		set iconList [list $iconRow]
		set featureList [list $featureRow]
		set letterList [list $letterRow]
		set symbolList [list $symbolRow]
		VaultPut $oop $top $left $iconList $featureList $letterList $symbolList
		if {$top != $bottom} {
			VaultPut $oop $bottom $left $iconList $featureList $letterList $symbolList
		}
	}

	return
}

# NSVaultEditor::Feedback_RectFill --
#
#	Handle the filled-rectangle tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_RectFill {oop y x} {

	return [Feedback_RectAux $oop $y $x yes]
}

# NSVaultEditor::Feedback_RectOutline --
#
#	Handle the outline-rectangle tool.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Feedback_RectOutline {oop y x} {

	return [Feedback_RectAux $oop $y $x no]
}

# NSVaultEditor::InSelection --
#
#	Determine if the given location in the current selection.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::InSelection {oop y x} {

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]

	if {![$widget itemcget $sel -visible]} {
		return 0
	}

	scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2

	if {$x >= $x1 && $x <= $x2 && $y >= $y1 && $y <= $y2} {
		return 1
	}

	return 0
}

# NSVaultEditor::Select --
#
#	Select area.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Select {oop y1 x1 y2 x2} {

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]

	$widget itemconfigure $sel -x1 $x1 -y1 $y1 -x2 $x2 -y2 $y2 -visible yes
	Info $oop select,bounds "$y1 $x1 $y2 $x2"
	Info $oop sel,float 0

	return
}

# NSVaultEditor::Deselect --
#
#	Remove the selection.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Deselect {oop} {

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]

	$widget itemconfigure $sel -visible no
	Info $oop select,bounds ""
	Info $oop sel,float 0

	return
}

# NSVaultEditor::Delete --
#
#	Erase selected area, or remove floating selection.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Delete {oop} {

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]

	scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2

	if {[Info $oop sel,float]} {
		vault swap [Info $oop display,vaultId] [Info $oop undo,display,vaultId]
		vault swap [Info $oop edit,vaultId] [Info $oop undo,edit,vaultId]
		Info $oop undo,select,visible 1
		Info $oop undo,select,bounds "$y1 $x1 $y2 $x2"
		Info $oop undo,select,float 1
		Deselect $oop
		return
	}

	ToUndo $oop
	EraseArea $oop display $y1 $x1 $y2 $x2
	Deselect $oop

	return
}

# NSVaultEditor::MoveLayer --
#
#	Move selected icons from one layer to another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::MoveLayer {oop layer1 layer2} {

	# Only non-transparent on layer 1
	if {$layer1 == 1} {
		error "can't move layer 1 up"
	}

	# Only transparent on layer 2, 3, 4
	if {$layer2 == 1} {
		error "can't move layer 2 down"
	}

	set vaultId [Info $oop display,vaultId]

	ToUndo $oop

	scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2

	# Get start layer
	set iconList [vault get $vaultId icon$layer1 $y1 $x1 $y2 $x2]

	# Put to dest layer
	vault put $vaultId icon$layer2 $iconList $y1 $x1

	# Erase start layer
	for {set x $x1} {$x <= $x2} {incr x} {
		lappend row {none 0}
	}
	vault put $vaultId icon$layer1 [list $row] $y1 $x1 $y2 $x2

	return
}

# NSVaultEditor::SelectionChanged_Icon --
#
#	Called when an icon is selected in the icon browser.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SelectionChanged_Icon {oop tree count select deselect} {

	# Don't assign stuff during feedback
	if {[Info $oop ignoreSel]} return

	# Do nothing if no new cell was selected
	if {![llength $select]} return

	# Get the (first) cell
	set item [lindex $select 0]
	set index [NSList::Item2Row $tree $item]

	# Get the icon type
	set browserId [Info $oop browserId]
	set iconType [NSIconBrowser::Info $browserId iconType]

	# Change the foreground icon
	SetToolIcon $oop "$iconType $index"

	return
}

# NSVaultEditor::Click_Icon --
#
#	Do something when a selected icon is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Click_Icon {oop index} {

	# Get the icon type
	set browserId [Info $oop browserId]
	set iconType [NSIconBrowser::Info $browserId iconType]

	# Change the foreground icon
	SetToolIcon $oop "$iconType $index"

	return
}

# NSVaultEditor::BrowserCmd --
#
#	Called by NSIconBrowser when displaying an icon type. Display
#	the progress of listing the icons. Note that this can
#	actually slow down listing the icons.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::BrowserCmd {oop action args} {

	set win [Info $oop win]
	set progId [Info $oop progId]

	switch -- $action {
		open {
			Info $oop oldText1 [$win.statusBar itemcget t1 -text]
			Info $oop oldText4 [$win.statusBar itemcget t4 -text]
			StatusBar $oop {} 0
			NSProgress2::Zero $progId
			pack [NSProgress2::Info $progId frame] -pady 1 -fill x
			update idletasks
		}
		update {
			set cur [lindex $args 0]
			set max [lindex $args 1]
			set bump [expr {(($max / 20) > 40) ? ($max / 20) : 40}]
			if {$cur && ($cur % $bump) == 0} {
				NSProgress2::SetDoneRatio $progId [expr {$cur / double($max)}]
				$win.statusBar itemconfigure t4 -text "$cur/$max"
				update idletasks
			}
		}
		close {
			NSProgress2::SetDoneRatio $progId 1.0
			update idletasks
			pack forget [NSProgress2::Info $progId frame]
			$win.statusBar itemconfigure t1 -text [Info $oop oldText1]
			$win.statusBar itemconfigure t4 -text [Info $oop oldText4]
		}
	}

	return
}

# NSVaultEditor::BrowserMotionCmd --
#
#	Called by NSIconBrowser.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::BrowserMotionCmd {oop which index} {

	set win [Info $oop win]
	set browserId [Info $oop browserId]

	# No icon was hit
	if {$index == -1} {
		$win.statusBar itemconfigure t4 -text ""
		return
	}

	# The mouse moved over the group canvas (ie, icon types)
	if {[string equal $which "group"]} {
		set iconType [lindex [NSIconBrowser::Info $browserId iconTypes] $index]
		$win.statusBar itemconfigure t4 -text "Icon type \"$iconType\"."
		return
	}

	set iconType [NSIconBrowser::Info $browserId iconType]
	set iconSpec "$iconType $index"

	$win.statusBar itemconfigure t4 -text "$iconSpec"

	return
}

# NSVaultEditor::Autoscan --
#
#	Perform automatic scrolling based on the cursor position.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::AutoScan {oop} {

	variable Priv

	set widgetId [Info $oop widgetId]
	set widget [Info $oop widget]

	if {![winfo exists $widget]} return
	if {[Info $oop ignoreTrack]} return

	set pointerx [winfo pointerx $widget]
	set pointery [winfo pointery $widget]
	if {[string equal [winfo containing $pointerx $pointery] "$widget"]} return

	set x [expr {$pointerx - [winfo rootx $widget]}]
	set y [expr {$pointery - [winfo rooty $widget]}]

	set delta 1
	if {[string equal [$widget cget -style] map]} {
		set delta 10
	}

    if {$x >= [winfo width $widget]} {
		NSWidget::xview $widgetId scroll $delta units
    } elseif {$x < 0} {
		NSWidget::xview $widgetId scroll -$delta units
    }

	if {$y >= [winfo height $widget]} {
		NSWidget::yview $widgetId scroll $delta units
    } elseif {$y < 0} {
		NSWidget::yview $widgetId scroll -$delta units
    }

	Motion1 $oop $x $y

	set Priv(scan,afterId) [after 50 NSVaultEditor::AutoScan $oop]

	return
}

# NSVaultEditor::ReadTown --
#
#	Read a t0000*.txt file. Features are read in. The user is asked if
#	default icons should be assigned. If a matching .vlt file exists, it is
#	read in as well.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ReadTown {oop path} {

	variable TPref
	variable building
	variable char2feat
	variable char2icon
	variable char2symbol

	SetMode $oop MODE_TOWN
	SetTitle $oop [file tail $path]
	SetEditMode $oop icon

	set win [Info $oop win]
	set vaultId [Info $oop display,vaultId]
	set vaultEditId [Info $oop edit,vaultId]

	StatusBar $oop "Reading [file tail $path]..." 0
	update idletasks

	set fileId [open $path]
	set buf [read $fileId]
	close $fileId

	# Hack -- t0000005.txt has 2 layouts, we want the first layout
	set sawTown 0

	set layout {}
	set feats(list,letter) {}
	set feats(list,feat) {}
	array unset building

	set inExpr 0

	foreach string [split $buf \n] {

		# Skip blank lines
		if {![string length $string]} continue

		# Skip comments
		if {[string index $string 0] eq "#"} continue

		# Expression
		if {[string equal ?:1 $string]} {
			set inExpr 0
			continue
		}

		# Expression
		if {[string match \\?:* $string]} {
			set inExpr 1
			continue
		}

		if {$inExpr} continue

		# Feature
		if {[string match F:* $string]} {
			set list [split $string :]
			set letter [lindex $list 1]
			set feat [lindex $list 2]
			set feats(char2feat,$letter) $feat
			set feats(char2F,$letter) [lrange $list 1 end]
			lappend feats(list,letter) $letter
			continue
		}

		# Building
		if {[string match B:*:N:* $string]} {
			set list [split $string :]
			set index [lindex $list 1]
			set name [lindex $list 3]
			set building(name,$index) $name
			continue
		}

		# Dungeon
		if {[string match D:* $string]} {
			lappend layout [string range $string 2 end]
			continue
		}
	}

	StatusBar $oop "Done." 1

	# List of char,feat,F
	set theList {}
	set letters {}

	# Prepend letters from t000000N.txt
	foreach letter $feats(list,letter) {
		lappend theList $letter $feats(char2feat,$letter) $feats(char2F,$letter) 1
		lappend letters $letter
	}

	# Prepend unique letters from t_pref.txt
	set theList2 {}
	foreach letter $TPref(list,letter) {
		if {[lsearch -exact $letters $letter] == -1} {
			set feat $TPref(char2feat,$letter)
			set F $TPref(char2F,$letter)
			lappend theList2 $letter $feat $F 0
		}
	}
	set theList [concat $theList2 $theList]

	# Set icon assignments for each letter
	SetLetters $oop $theList

	# Set the list of features
	SetFeatureList $oop

	# Resize the vault(s)
	set height [llength $layout]
	set width [string length [lindex $layout 0]]
	SetVaultSize $oop $height $width

	StatusBar $oop "Setting vault features, icons and letters..." 0
	update idletasks

	# Build a list of unhandled vault characters
	set unknownChar {}

	# Set the vault features and icons from the D: lines
	set y 0
	foreach string $layout {
		set iconRow {}
		set featRow {}
		set letterRow {}
		set symbolRow {}
		foreach char [split $string ""] {

			# This is a known letter
			if {[info exists char2icon($char)]} {
				lappend iconRow $char2icon($char)
				lappend featRow $char2feat($char)
				lappend letterRow $char
				lappend symbolRow $char2symbol($char)

			# This is an unknown letter
			} else {
				set icon "vault [icon ascii index vault $char] \
					[Global vault,attr,monster]"
				lappend iconRow $icon
				lappend featRow 0
				lappend letterRow $char
				lappend symbolRow sym.blank

				# Keep a list of unknown letters
				if {[lsearch -exact $unknownChar $char] == -1} {
					lappend unknownChar $char
				}
			}
		}
		vault put $vaultEditId icon1 [list $iconRow] $y 0
		vault put $vaultEditId feature [list $featRow] $y 0
		vault put $vaultEditId letter [list $letterRow] $y 0
		vault put $vaultEditId symbol [list $symbolRow] $y 0
		incr y
	}

	StatusBar $oop "Done." 0

	if {[llength $unknownChar]} {
		set string [join $unknownChar ", "]
		tk_messageBox -parent $win -title "Unknown Character" \
			-message "These characters were not recognized:\n$string"
	}

	#
	# Now read in the corresponding .vlt file, it one exists
	#
	set name [file tail $path]
	set name [file rootname $name]-[Global config,prefix].vlt
	set path [PathTk config $name]
#	Info $oop path,vault $path
	if {[file exists $path]} {
		set doVault [tk_messageBox -title "Read Vault Icons" -parent $win \
			-type yesno -message "Read icons from $name?"]
		if {$doVault} {
			ReadVault $oop $path no
		}
	}

	ToUndo $oop
	Info $oop clip,bounds ""

	[Info $oop widget] wipe
	[Info $oop map,widget] wipe

	return
}

# NSVaultEditor::Save --
#
#	Save the current vault to the given .vlt file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Save {oop path} {

	if {[Info $oop editMode] eq "icon"} {
		set vaultId [Info $oop display,vaultId]
	} else {
		set vaultId [Info $oop edit,vaultId]
	}

	vault write $vaultId $path

	Info $oop path,vault $path

	SetTitle $oop [file tail $path]

	return
}

# NSVaultEditor::ReadVault --
#
#	Read a .vlt file. Icons are always read in. Features are read in
#	if requested.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ReadVault {oop path doFeat} {

	variable feat2char
	variable feat2symbol

	# Make sure we are editing icons
	SetEditMode $oop icon

	# Create a temp vault so we can get the size
	set v [vault create -file $path]
	Info $oop temp,vaultId $v

	SetVaultSize $oop [vault height $v] [vault width $v]

	# Read the icons/features
#	vault read [Info $oop clip,vaultId] $path

	# Copy the icons to Display
	CopyIconsAll $oop temp display

	# Copy the features to Display only if asked
	if {$doFeat} {
		CopyPlaneAll $oop feature temp display

		# Set letters and symbols based on features
		set vaultId [Info $oop display,vaultId]
		set y 0
		foreach featList [vault get $vaultId feature] {
			set letterList {}
			set symbolList {}
			foreach feat $featList {
				if {[info exists feat2char($feat)]} {
					lappend letterList $feat2char($feat)
					lappend symbolList $feat2symbol($feat)
				} else {
					lappend letterList .
					lappend symbolList sym.floor0
				}
			}
			vault put $vaultId letter [list $letterList] $y 0
			vault put $vaultId symbol [list $symbolList] $y 0
			incr y
		}
	}

	vault delete $v
	Info $oop temp,vaultId ""

	# Nothing to paste
	Info $oop clip,bounds ""

	# Nothing to undo
	ToUndo $oop

	Info $oop path,vault $path
	SetTitle $oop [file tail $path]

	return
}

# NSVaultEditor::EraseLayerIcons --
#
#	Set icons in area to NONE.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::EraseLayerIcons {oop vault layer y1 x1 y2 x2} {

	set vaultId [Info $oop $vault,vaultId]

	set rowIcon {}
	for {set x $x1} {$x <= $x2} {incr x} {
		lappend rowIcon {none 0}
	}
	vault put $vaultId icon$layer [list $rowIcon] $y1 $x1 $y2 $x2

	return
}

# NSVaultEditor::EraseArea --
#
#	Set icons and features to defaults.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::EraseArea {oop vault y1 x1 y2 x2} {

	set vaultId [Info $oop $vault,vaultId]

	# Erase to floor
	set rowIcon {}
	set rowFeature {}
	set rowLetter {}
	set rowSymbol {}
	for {set x $x1} {$x <= $x2} {incr x} {
		lappend rowIcon {none 0}
		lappend rowFeature 1
		lappend rowLetter .
		lappend rowSymbol sym.floor0
	}
	foreach plane {icon1 icon2 icon3 icon4} {
		vault put $vaultId $plane [list $rowIcon] $y1 $x1 $y2 $x2
	}
	vault put $vaultId feature [list $rowFeature] $y1 $x1 $y2 $x2
	vault put $vaultId letter [list $rowLetter] $y1 $x1 $y2 $x2
	vault put $vaultId symbol [list $rowSymbol] $y1 $x1 $y2 $x2

	return
}

# NSVaultEditor::Erase --
#
#	Set icons and features to defaults.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Erase {oop} {

	set vaultId [Info $oop display,vaultId]
	set height [vault height $vaultId]
	set width [vault width $vaultId]

	# Erase to floor
	set rowIcon {}
	set rowFeature {}
	set rowLetter {}
	set rowSymbol {}
	for {set x 0} {$x < $width} {incr x} {
		lappend rowIcon {none 0}
		lappend rowFeature 1
		lappend rowLetter .
		lappend rowSymbol sym.floor0
	}
	set y2 [expr {$height - 1}]
	set x2 [expr {$width - 1}]
	foreach plane {icon1 icon2 icon3 icon4} {
		vault put $vaultId $plane [list $rowIcon] 0 0 $y2 $x2
	}
	vault put $vaultId feature [list $rowFeature] 0 0 $y2 $x2
	vault put $vaultId letter [list $rowLetter] 0 0 $y2 $x2
	vault put $vaultId symbol [list $rowSymbol] 0 0 $y2 $x2

	# Erase other vault too
	CopyIconsAll $oop display edit

	ToUndo $oop

	NSMap::SetView [Info $oop mapId] 0 0
	NSMap::SetView [Info $oop map,mapId] 0 0

	return
}

# NSVaultEditor::SetDefaultIcons --
#
#	Set all the icons to their defaults, based on feature type.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetDefaultIcons {oop} {

	variable feat2icon
	variable feat2iconbg

	if {[string equal [Info $oop editMode] "icon"]} {
		set vaultId [Info $oop display,vaultId]
	} else {
		set vaultId [Info $oop edit,vaultId]
	}

	set y 0
	foreach row [vault get $vaultId feature] {
		set iconRow {}
		set iconbgRow {}
		foreach feat $row {
			lappend iconRow $feat2icon($feat)
			lappend iconbgRow $feat2iconbg($feat)
		}
		vault put $vaultId icon4 [list [list {none 0}]]
		vault put $vaultId icon3 [list [list {none 0}]]
		vault put $vaultId icon2 [list $iconRow] $y 0
		vault put $vaultId icon1 [list $iconbgRow] $y 0
		incr y
	}

	return
}

# NSVaultEditor::ExamineCmd --
#
#	Called when the mouse moves over the main map.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ExamineCmd {oop widgetId y x modifiers} {

	variable char2name
	variable feat2name

	set vaultId [Info $oop display,vaultId]
	set widget [Info $oop widget]
	set cursor [Info $oop widget,cursor]

	if {($y < 0) || ($y >= [vault height $vaultId])} {
		$widget itemconfigure $cursor -visible no
		StatusBar $oop "" 0
		return
	}
	if {($x < 0) || ($x >= [vault width $vaultId])} {
		$widget itemconfigure $cursor -visible no
		StatusBar $oop "" 0
		return
	}

	$widget itemconfigure $cursor -x $x -y $y -visible yes

	if {[Info $oop editMode] eq "icon"} {
		set layer [Info $oop layer]
		set icon [vault get $vaultId icon$layer $y $x]
	} else {
		set icon [vault get $vaultId icon1 $y $x]
	}
	set feat [vault get $vaultId feature $y $x]
	set letter [vault get $vaultId letter $y $x]

	# Describe the feature
	if {[info exists char2name($letter)]} {
		set name $char2name($letter)
	} elseif {[info exists feat2name($feat)]} {
		set name $feat2name($letter)
	} else {
		set name ?[angband f_info info $feat name]
	}

	set iconStr $icon
	StatusBar $oop "\[y: $y x:$x] - icon ($icon) - feat #$feat \"$name\" - letter `$letter'" 0

	if {[NSWidget::Info [Info $oop map,widgetId] scale] == [icon width]} {
		if {![Info $oop ignoreExamine]} {
			NSMap::SetView [Info $oop map,mapId] $y $x
		}
	}

	# Show the icons in the layers
	set canvas [Info $oop layer,canvas]
	foreach layer {1 2 3 4} {
		$canvas itemconfigure icon$layer \
			-assign "icon [vault get $vaultId icon$layer $y $x]"
	}

	set widget [Info $oop widget]
	if {[InSelection $oop $y $x]} {
		$widget configure -cursor fleur
	} else {
		$widget configure -cursor {}
	}

	return
}

# NSVaultEditor::ExamineCmd_Map --
#
#	Called when the mouse moves over the MicroMap.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ExamineCmd_Map {oop widgetId y x modifiers} {

	set mapId [Info $oop mapId]

	if {[NSWidget::Info $widgetId scale] != [icon width]} {
		Info $oop ignoreView 1
		NSMap::SetView $mapId $y $x
		Info $oop ignoreView 0
		[Info $oop widget] itemconfigure [Info $oop widget,cursor] \
			-x $x -y $y -visible yes
	}
	Info $oop ignoreExamine 1
	ExamineCmd $oop [Info $oop widgetId] $y $x $modifiers
	Info $oop ignoreExamine 0

	return
}

# NSVaultEditor::ViewCmd --
#
#	Called when main map scrolls. Update the size and position of the
#	bounds rectangle in the micro map.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ViewCmd {oop} {

	set widget [Info $oop widget]
	set widget2 [Info $oop map,widget]
	set view [Info $oop map,view]

	scan [$widget center] "%d %d" centery centerx

	scan [$widget bounds] "%d %d %d %d" top left bottom right

	$widget2 itemconfigure $view -x1 $left -y1 $top -x2 $right -y2 $bottom

	if {![Info $oop ignoreView]} {
		Info $oop widget,center [$widget center]
	}

	return
}

# NSVaultEditor::VaultSetup --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::VaultSetup {oop} {

	variable building
	variable TPref

	set theList {}
	if {[variant KANGBANDTK ZANGBANDTK]} {
		foreach letter $TPref(list,letter) {
			lappend theList $letter $TPref(char2feat,$letter) $TPref(char2F,$letter) 0
		}
	}

	array unset building

	SetLetters $oop $theList

	return
}

# NSVaultEditor::GetFeatureName --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::GetFeatureName {oop f_idx} {

	variable building

	set name [angband f_info info $f_idx name]

	if {[variant KANGBANDTK ZANGBANDTK]} {
		if {$f_idx >= [const FEAT_BLDG_HEAD] && $f_idx <= [const FEAT_BLDG_TAIL]} {
			set index [expr {$f_idx - [const FEAT_BLDG_HEAD]}]
			if {[variant KANGBANDTK]} {
				# Handle gap in the building f_info[] indices
				if {$index > 7} {
					incr index -3
				}
			}
			if {[info exists building(name,$index)]} {
				set name $building(name,$index)
			}
		}
	}

	return $name
}

# NSVaultEditor::SetLetters --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetLetters {oop theList} {

	variable Priv
	variable char2feat
	variable char2ass
	variable char2assbg
	variable char2icon
	variable char2name
	variable char2symbol
	variable feat2char
	variable feat2icon
	variable feat2iconbg
	variable feat2name
	variable feat2symbol
	variable row2char
	variable row2feat
	variable row2ass
	variable row2assbg
	variable row2icon
	variable row2name
	variable row2symbol

	set vaultId [Info $oop display,vaultId]

	tryunset char2feat
	tryunset char2ass
	tryunset char2assbg
	tryunset char2icon
	tryunset char2name
	tryunset char2symbol
	tryunset feat2char
	tryunset feat2icon
	tryunset feat2iconbg
	tryunset feat2name
	tryunset feat2symbol
	tryunset row2char
	tryunset row2feat
	tryunset row2ass
	tryunset row2assbg
	tryunset row2icon
	tryunset row2name
	tryunset row2symbol

	foreach feat [angband f_info find] {
		set feat2icon($feat) {none 0}
		set feat2iconbg($feat) {none 0}
	}

	if {[variant ANGBANDTK OANGBANDTK]} {
		set theList {}
		foreach feat [angband f_info find] {
			lappend theList . $feat "" 0
		}
	}

	foreach {char feat F aschar} $theList {

		# This is a feature
		if {!$aschar && (![llength $F] || ([llength $F] == 3))} {

			# Get the feature name
			set name [GetFeatureName $oop $feat]

			# Handle mimic
			set mimic [struct set feature_type $feat mimic]

			# Foreground
			set assign [assign set feature $mimic]
			scan $assign {%s %s %d} assignType iconType iconIndex
			switch -- $assignType {
				alternate {
#					set icon [lindex [alternate get $iconType] 0]
					set icon {none 0}
				}
				icon {
					set icon "$iconType $iconIndex"
				}
				sprite {
					set icon {none 0}
				}
				default {
					error "can't use assignment type \"$assignType\""
				}
			}

			# Background
			set featBG [feature configure $mimic -background]
			set assignbg [assign set feature $featBG]
			if {$mimic == $featBG} {
				set iconBG $icon
				set assignBG "icon none 0"
				set icon {none 0}
			} else {
				scan $assignbg {%s %s %d} assignType iconType iconIndex
				switch -- $assignType {
					alternate {
#						set iconBG [lindex [alternate get $iconType] 0]
						set iconBG {none 0}
					}
					icon {
						set iconBG "$iconType $iconIndex"
					}
					sprite {
						set iconBG {none 0}
					}
					default {
						error "can't use assignment type \"$assignType\""
					}
				}
			}

			set symbol [symbol assign feature $feat]

			set isfeat 1

		# A special F: (monster, object, trap)
		} else {

			set name F:[join $F :]

			set icon {none 0}
			set iconBG "vault [icon ascii index vault $char]\
				[Global vault,attr,monster]"
			set assign "icon $icon"
			set assignbg "icon $iconBG"

			set symbol [symbol assign feature $feat]

			set isfeat 0
		}

		set char2feat($char) $feat
		set char2ass($char) $assign
		set char2assbg($char) $assignbg
		if {$isfeat} {
			set char2icon($char) "none 0"
		} else {
			set char2icon($char) $iconBG
		}
		set char2name($char) $name
		set char2symbol($char) $symbol

		if {$isfeat} {
			set feat2char($feat) $char
			set feat2icon($feat) $icon
			set feat2iconbg($feat) $iconBG
			set feat2name($feat) $name
			set feat2symbol($feat) $symbol
		}

		lappend row2char $char
		lappend row2feat $feat
		lappend row2ass $assign
		lappend row2assbg $assignbg
		if {$isfeat} {
			lappend row2icon "none 0"
		} else {
			lappend row2icon $iconBG
		}
		lappend row2name $name
		lappend row2symbol $symbol
	}

	return
}

# NSVaultEditor::SetMode --
#
#	Change the mode to "vault", MODE_TOWN or MODE_QUEST.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetMode {oop mode} {

	set oldMode [Info $oop mode]
	set vaultId [Info $oop display,vaultId]
	set mbarId [Info $oop mbarId]

	if {[string equal $oldMode $mode]} return
	Info $oop mode $mode

if 0 {
	switch -- $oldMode {
		MODE_QUEST {
			NSMenu::MenuDeleteEntry $mbarId M_QUEST
		}
	}
}
	switch -- $mode {
		MODE_QUEST {
if 0 {
			NSMenu::MenuInsertEntry $mbarId -after M_VAULT -type cascade \
				-menu MENU_QUEST -label Quest -underline 0 -identifier M_QUEST
}	
			Info $oop mode,default 1
			Info $oop mode,toggle 1
			Info $oop mode,town 0

			SetEditMode $oop icon

			NSList::Clear [Info $oop feature,tree]
		}
		MODE_VAULT -
		MODE_TOWN {
			Info $oop mode,default 1
			Info $oop mode,toggle 1
			Info $oop mode,town 1
#			VaultSetup $oop
#			SetFeatureList $oop
		}
	}

	return
}

# NSVaultEditor::SetEditMode --
#
#	Change the edit mode to icon or feature.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetEditMode {oop mode} {

	variable row2ass
	variable row2assbg
	variable row2icon

	set win [Info $oop win]
	set browserId [Info $oop browserId]
	set vaultId [Info $oop display,vaultId]

	if {$mode == [Info $oop editMode]} return
	Info $oop editMode $mode

	# Update Undo with the last change
#	ToUndo $oop

	# Swap Display and Edit
	vault swap $vaultId [Info $oop edit,vaultId]
	vault swap [Info $oop undo,display,vaultId] [Info $oop undo,edit,vaultId]

	if {[Info $oop mode,toggle]} {
		$win.statusBar itemconfigure t3 \
			-text [string range [string toupper $mode] 0 3]
	} else {
		$win.statusBar itemconfigure t3 -text ""
	}

	switch -- $mode {
		icon {
			grid remove $win.frameFeature
			grid [NSIconBrowser::Info $browserId frame] -in $win.frameList
			SetToolIcon $oop [Info $oop save,icon]
		}
		feature {
			grid remove [NSIconBrowser::Info $browserId frame]
			grid $win.frameFeature -in $win.frameList

			# Set the feature list if it is being displayed for the
			# first time
			set treeF [Info $oop feature,tree]
			if {[$treeF numitems] == 1} {
				SetFeatureList $oop
			}

			Info $oop save,icon [Info $oop tool,icon]

			if {[$treeF selection count]} {
				set item [lindex [$treeF selection get] 0]
				$treeF selection clear
			} else {
				set item "root child 0"
			}

			# Select row so tool icon set properly
			NSList::Activate $treeF $item
		}
	}

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact {icon feature} $mode]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSVaultEditor::ToggleEditMode --
#
#	Change the edit mode to whatever it isn't set to now.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ToggleEditMode {oop} {

	if {![Info $oop mode,toggle]} return

	if {[string equal [Info $oop editMode] "icon"]} {
		set newMode feature
	} else {
		set newMode icon
	}
	NSVaultEditor::SetEditMode $oop $newMode

	set widgetId [Info $oop widgetId]
	set pos [NSWidget::Info $widgetId examined]
	if {$pos ne ""} {
		ExamineCmd $oop $widgetId {*}$pos {}
	}

	return
}

# NSVaultEditor::SetVaultSize --
#
#	Change the size of the vault.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetVaultSize {oop height width} {

	if {$height < 1 || $width < 1} {
		set height [Info $oop max,height]
		set width [Info $oop max,width]
	}

	set v [Info $oop display,vaultId]
	vault height $v $height
	vault width $v $width

	set v [Info $oop edit,vaultId]
	vault height $v $height
	vault width $v $width

	set v [Info $oop clip,vaultId]
	vault height $v $height
	vault width $v $width

	set v [Info $oop undo,display,vaultId]
	vault height $v $height
	vault width $v $width

	set v [Info $oop undo,edit,vaultId]
	vault height $v $height
	vault width $v $width

	Info $oop clip,bounds ""

	NSMap::SetView [Info $oop mapId] 0 0
	NSMap::SetView [Info $oop map,mapId] 0 0

	return
}

# NSVaultEditor::DisplayIcon --
#
#	Display the tool icon in the browser.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::DisplayIcon {oop which} {

	variable row2char

	switch -- [Info $oop editMode] {
		icon {
			set browserId [Info $oop browserId]
			set iconTypes [NSIconBrowser::Info $browserId iconTypes]
			set icon [Info $oop tool,$which]

			scan $icon "%s %d" iconType iconIndex
			if {[lsearch -exact $iconTypes $iconType] != -1} {
				Info $oop ignoreSel 1
				NSIconBrowser::SeeIcon $browserId $iconType $iconIndex
				Info $oop ignoreSel 0
			}
		} 
		feature {
			set treeF [Info $oop feature,tree]
			set letter [Info $oop tool,letter]
			set row [lsearch -exact $row2char $letter]
			NSList::Activate $treeF "root child $row"
		}
	}

	return
}

# NSVaultEditor::SetFeatureList --
#
#	Set the feature list with valid "vault" features.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetFeatureList {oop} {

	variable row2feat
	variable row2ass
	variable row2assbg
	variable row2name

	set tree [Info $oop feature,tree]

	NSList::Clear $tree

	set itemList {}
	foreach f_idx $row2feat ass $row2ass assbg $row2assbg name $row2name {
		if {0 && [string equal $iconbg "none 0"]} {
			set assign [assign set feature $f_idx]
		}
		set item [$tree item create]
		NSList::SetIcon $tree $item $ass
		$tree item element configure $item 0 eAss -bg $assbg
		NSList::SetText $tree $item $name
		$tree item lastchild root $item
	}

	return
}

# NSVaultEditor::SelectionChanged_Feature --
#
#	Do something when a feature list item is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SelectionChanged_Feature {oop tree count select deselect} {

	variable row2ass
	variable row2assbg
	variable row2char
	variable row2icon
	variable row2feat
	variable row2symbol

	if {![llength $select]} return
	set row [NSList::Item2Row $tree [lindex $select 0]]

	Info $oop tool,feature [lindex $row2feat $row]
	Info $oop tool,letter [lindex $row2char $row]
	Info $oop tool,symbol [lindex $row2symbol $row]

	# Actually draw "none 0" (unless ascii)
	SetToolIcon $oop [lindex $row2icon $row]

	# Tool icon looks like actual feature (unless ascii)
	set ass [lindex $row2ass $row]
	set assbg [lindex $row2assbg $row]
	set canvas [Info $oop canvasTool]
	$canvas itemconfigure tool -assign $ass -assignbg $assbg

	return
}

# NSVaultEditor::VaultToText --
#
#	Convert vault features to textual symbols. The displayed text can
#	be saved to a file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::VaultToText {oop} {

	variable Priv

	set vaultId [Info $oop display,vaultId]

	set win .vault2text
	if {![winfo exists $win]} {

		toplevel $win
		wm geometry $win +20+20
		wm transient $win [Info $oop win]

		menu $win.menubar -tearoff 0
		menu $win.menubar.file -tearoff 0
		$win.menubar.file add command -label "Save As..." -underline 5 \
			-command "NSVaultEditor::VaultToText_Save $oop"
		$win.menubar.file add separator
		$win.menubar.file add command -label "Close" -underline 0 \
			-command "destroy $win"
		$win.menubar add cascade -menu $win.menubar.file -label "File" \
			-underline 0
		$win configure -menu $win.menubar

		frame $win.frame \
			-relief sunken -borderwidth 1
		set frame $win.frame

		if {$::UseTile} {
			scrollbar $frame.yscroll \
				-orient vertical -command "$frame.text yview"

			scrollbar $frame.xscroll \
				-orient horizontal -command "$frame.text xview"
		} else {
			scrollbar $frame.yscroll \
				-orient vertical -command "$frame.text yview" \
				-highlightthickness 0

			scrollbar $frame.xscroll \
				-orient horizontal -command "$frame.text xview" \
				-highlightthickness 0
		}

		text $frame.text \
			-yscrollcommand "$frame.yscroll set" -wrap none \
			-xscrollcommand "$frame.xscroll set" \
			-width 82 -height 24 -font [Value font,fixed] -borderwidth 0 \
			-highlightthickness 0

		pack $frame -expand yes -fill both

		grid rowconfig $frame 0 -weight 1 -minsize 0
		grid columnconfig $frame 0 -weight 1 -minsize 0

		grid $frame.text \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
		grid $frame.yscroll \
			-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
		grid $frame.xscroll \
			-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

		set Priv(vaultText) $win.frame.text
	}
	set text $Priv(vaultText)
	$text delete 1.0 end

	WindowBringToFront $win
	update

	foreach row [vault get $vaultId letter] {
		$text insert end D:[join $row ""]\n
	}

	return
}

# NSVaultEditor::VaultToText_Save --
#
#	Let the user choose a file to save the vault text to.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::VaultToText_Save {oop} {

	variable Priv

	set win [winfo toplevel $Priv(vaultText)]

	set types {
		{{Text Files} ".txt" TEXT}
		{{All files} {*}}
	}
	set file untitled.txt
	set path [tk_getSaveFile -filetypes $types -parent $win \
		-initialfile $file -initialdir [Path lib edit]]
	if {[string length $path]} {
		set fileId [open $path w]
		catch {
			puts -nonewline $fileId [$Priv(vaultText) get 1.0 end]
		}
		close $fileId
	}

	return
}

proc NSVaultEditor::ReadQuest {oop path} {

	variable building
	variable QPref
	variable TPref
	variable char2icon
	variable char2feat
	variable char2symbol

	set win [Info $oop win]
	set vaultId [Info $oop display,vaultId]
	set vaultEditId [Info $oop edit,vaultId]

	SetMode $oop MODE_QUEST
	SetEditMode $oop icon
	SetTitle $oop [file tail $path]

	# Get the quest number from the file name
	scan [file tail $path] "q%d.txt" questNum

	StatusBar $oop "Reading [file tail $path]..." 0
	update idletasks

	# Read the file to a list of strings
	set fileId [open $path]
	set lines [split [read $fileId] \n]
	close $fileId

	set layout {}
	set feats(list,letter) {}
	set feats(list,feat) {}
	array unset building

	foreach string $lines {

		# Feature
		if {[string match F:* $string]} {
			set list [split $string :]			
			set letter [lindex $list 1]
			set feat [lindex $list 2]
			set feats(char2feat,$letter) $feat
			set feats(char2F,$letter) [lrange $list 1 end]
			lappend feats(list,letter) $letter
			continue
		}

		# Dungeon
		if {[string match D:* $string]} {
			lappend layout [string range $string 2 end]
			continue
		}
	}

	# No dungeon for this quest
	if {![llength $layout]} {
	}

	# List of char,feat,F
	set theList {}
	set letters {}

	# Prepend letters from q000000N.txt
	foreach letter $feats(list,letter) {
		lappend theList $letter $feats(char2feat,$letter) $feats(char2F,$letter) 1
		lappend letters $letter
	}

	# Prepend unique letters from q_pref.txt
	set theList2 {}
	foreach letter $QPref(list,letter) {
		if {[lsearch -exact $letters $letter] == -1} {
			set feat $QPref(char2feat,$letter)
			set F $QPref(char2F,$letter)
			lappend theList2 $letter $feat $F 0
		}
	}

	# Prepend unique letters from t_pref.txt
	set theList3 {}
	foreach letter $TPref(list,letter) {
		if {[lsearch -exact $letters $letter] == -1} {
			set feat $TPref(char2feat,$letter)
			set F $TPref(char2F,$letter)
			lappend theList3 $letter $feat $F 0
		}
	}
	set theList [concat $theList3 $theList2 $theList]

	# Set icon assignments for each letter
	SetLetters $oop $theList

	# Set the list of features
	SetFeatureList $oop

	# Resize the vault(s)
	set height [llength $layout]
	set width [string length [lindex $layout 0]]
	SetVaultSize $oop $height $width

	set unknownChar {}

	# Set the vault features and icons from the D: lines
	set y 0
	foreach string $layout {
		set iconRow {}
		set featRow {}
		set letterRow {}
		set symbolRow {}
		foreach char [split $string ""] {

			# This is a known letter
			if {[info exists char2icon($char)]} {
				lappend iconRow $char2icon($char)
				lappend featRow $char2feat($char)
				lappend letterRow $char
				lappend symbolRow $char2symbol($char)

			# This is an unknown letter
			} else {
				set icon "vault [icon ascii index vault $char] \
					[Global vault,attr,monster]"
				lappend iconRow $icon
				lappend featRow 0
				lappend letterRow $char
				lappend symbolRow sym.blank

				# Keep a list of unknown letters
				if {[lsearch -exact $unknownChar $char] == -1} {
					lappend unknownChar $char
				}
			}
		}
		vault put $vaultEditId icon1 [list $iconRow] $y 0
		vault put $vaultEditId feature [list $featRow] $y 0
		vault put $vaultEditId letter [list $letterRow] $y 0
		vault put $vaultEditId symbol [list $symbolRow] $y 0
		incr y
	}

	if {[llength $unknownChar]} {
		set string [join $unknownChar ", "]
		tk_messageBox -parent $win -title "Unknown Character" \
			-message "These characters were not recognized:\n$string"
	}

	#
	# Now read in the corresponding .vlt file, it one exists
	#
	set name [file tail $path]
	set name [file rootname $name]-[Global config,prefix].vlt
	set path [PathTk config $name]
	if {[file exists $path]} {
		set doVault [tk_messageBox -title "Read Vault Icons" -parent $win \
			-type yesno -message "Read icons from $name?"]
		if {$doVault} {
			ReadVault $oop $path no
		}
	}

	ToUndo $oop
	Info $oop clip,bounds ""

	return
}

# NSVaultEditor::SetTitle --
#
#	Set the window title.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::SetTitle {oop string} {

	switch -- [Info $oop mode] {
		MODE_QUEST {set mode Quest}
		MODE_VAULT {set mode Vault}
		MODE_TOWN {set mode Town}
	}

	wm title [Info $oop win] "$mode \"$string\" - Vault Editor"

	return
}

proc tryunset {varName} {

	upvar $varName v
	if {[info exists v]} {
		unset v
	}
	return
}

# NSVaultEditor::ReadTPref --
#
#	Read lib/edit/t_pref.txt.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ReadTPref {} {

	variable TPref

	set path [Path lib edit t_pref.txt]
	set fileId [open $path]
	set lines [split [read $fileId] \n]
	close $fileId
	foreach string $lines {
		if {[string match {F:*} $string]} {
			set list [split $string :]
			set letter [lindex $list 1]
			set feat [lindex $list 2]

			set TPref(char2feat,$letter) $feat
			set TPref(char2F,$letter) [lrange $list 1 end]
			lappend TPref(list,letter) $letter
			lappend TPref(list,feat) $feat
		}
	}

	return
}

# NSVaultEditor::ReadQPref --
#
#	Read lib/edit/q_pref.txt.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::ReadQPref {} {

	variable QPref

	set path [Path lib edit q_pref.txt]
	set fileId [open $path]
	set lines [split [read $fileId] \n]
	close $fileId

	foreach string $lines {

		# Feature
		if {[string match {F:*} $string]} {
			set list [split $string :]
			set letter [lindex $list 1]
			set feat [lindex $list 2]

			set QPref(char2feat,$letter) $feat
			set QPref(char2F,$letter) [lrange $list 1 end]
			lappend QPref(list,letter) $letter
			lappend QPref(list,feat) $feat
		}
	}

	return
}

# NSVaultEditor::AskForSize --
#
#	Request new dimensions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::AskForSize {oop} {

	variable Result

	set win [Info $oop win]
	set vaultId [Info $oop display,vaultId]

	set w $win.askforsize
	if {![winfo exists $w]} {

		toplevel $w
		wm title $w "Vault Dimensions"
		wm resizable $w no no
		wm transient $w $win
		wm withdraw $w

		wm protocol $w WM_DELETE_WINDOW "set NSVaultEditor::Result 0"

		set f [frame $w.frameHeight -borderwidth 0]
		label $f.label -text Height: -width 7
		entry $f.entry -width 4
		pack $f.label $f.entry -side left
		pack $f -side top -anchor w -pady 10

		set f [frame $w.frameWidth -borderwidth 0]
		label $f.label -text Width: -width 7
		entry $f.entry -width 4
		pack $f.label $f.entry -side left
		pack $f -side top -anchor w

		set f [frame $w.frameBtn -borderwidth 0]
		button $f.btn1 -text OK -command "set NSVaultEditor::Result 1" \
			-width 9 -default active
		button $f.btn2 -text Cancel -command "set NSVaultEditor::Result 0" \
			-width 9
		pack $f.btn1 -side left -padx 10
		pack $f.btn2 -side left -padx {0 10} -pady 10
		pack $f -side top -anchor e

		NSUtils::SetDefaultButton $w $f.btn1

		# Return key selects default button
		bind $w <KeyPress-Return> \
			"NSUtils::InvokeDefaultButton $w"

		# Escape key selects "cancel" button
#	    bind $w.frame.text <KeyPress-Escape> \
#			"tkButtonInvoke $frame.button$n"
	}

	$w.frameHeight.entry delete 0 end
	$w.frameHeight.entry insert end [vault height $vaultId]
	$w.frameHeight.entry selection range 0 end
	$w.frameWidth.entry delete 0 end
	$w.frameWidth.entry insert end [vault width $vaultId]

	# Position window
	WindowPosition $w 2 3

	NSUtils::GrabSave $w
	focus $w.frameHeight.entry

	set Result 0
	tkwait variable NSVaultEditor::Result

	NSUtils::GrabRelease $w

	wm withdraw $w

	if {$Result} {
		set height [$w.frameHeight.entry get]
		set width [$w.frameWidth.entry get]
		SetVaultSize $oop $height $width
		return 1
	}

	return 0
}

namespace eval NSVaultEditor::Plugin {
}

# NSVaultEditor::Plugin::ReadIndex --
#
#	Read vault/plugins/pluginIndex.tcl.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Plugin::ReadIndex {} {

	variable ::NSVaultEditor::Plugin

	set Plugin(menuDepth) 0
	set Plugin(menuInfo) {}
	set Plugin(list) {}

	source [CPathTk vault plugins pluginIndex.tcl]

	return
}

# NSVaultEditor::Plugin::Menu --
#
#	Called to add a plugin menu from vault/plugins/pluginIndex.tcl.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Plugin::Menu {label} {

	variable ::NSVaultEditor::Plugin

	lappend Plugin(menuInfo) $Plugin(menuDepth) $label ""
	incr Plugin(menuDepth)

	return
}

# NSVaultEditor::Plugin::Stop --
#
#	End of a plugin menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Plugin::Stop {} {

	variable ::NSVaultEditor::Plugin

	incr Plugin(menuDepth) -1

	return
}

# NSVaultEditor::Plugin::Plugin --
#
#	Called to register a plugin from vault/plugins/pluginIndex.tcl.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::Plugin::Plugin {ident file label opt} {

	variable ::NSVaultEditor::Plugin

	set list [split $ident ,]
	set ident [join $list #]
	set Plugin($ident,args) [lrange $list 1 end]

	lappend Plugin(menuInfo) $Plugin(menuDepth) $label $ident

	set Plugin($ident,file) $file
	if {[string length $file]} {
		namespace eval ::NSVaultEditor [list source [CPathTk vault plugins $file]]
		set Plugin($ident,mtime) [file mtime [CPathTk vault plugins $file]]
	}
	foreach string [split $opt ,] {
		set list [split $string =]
		set Plugin($ident,[lindex $list 0]) [lindex $list 1]
	} 
	lappend Plugin(list) $ident

	return
}

proc NSVaultEditor::ListChoice_Validate {oop string} {

	variable ListChoice

	if {[string length $string]} {
		set index [lsearch -glob $ListChoice(list) *[join $string *]*]
		$ListChoice(listbox) selection clear 0 end
		if {$index != -1} {
			$ListChoice(listbox) selection set $index
			$ListChoice(listbox) see $index
		}
	}

	return 1
}

proc NSVaultEditor::ListChoice {oop title list initial} {

	variable ListChoice

	set parent [Info $oop win]

	set w $parent.choice$oop
	if {![winfo exists $w]} {
		toplevel $w
		wm withdraw $w

		#
		# Search entry
		#

		set frame $w.frameFind
		frame $frame -borderwidth 0
		label $frame.label -text Find:
		entry $frame.entry -validatecommand "NSVaultEditor::ListChoice_Validate $oop %P" \
			-validate key
		pack $frame.label -side left
		pack $frame.entry -side left -fill x
		pack $frame -side top -fill x

		#
		# List
		#

		listbox $w.list -bg White -width 35 -height 25 \
			-listvariable NSVaultEditor::ListChoice(list) \
			-yscrollcommand "$w.yscroll set"
		scrollbar $w.yscroll -command "$w.list yview"
		pack $w.list -side left -expand yes -fill both
		pack $w.yscroll -side left -fill y
		bind $w.list <Double-ButtonPress-1> {
			set NSVaultEditor::ListChoice(choice) [%W nearest %y]
		}
		bind $w <KeyPress-Return> \
			"set NSVaultEditor::ListChoice(choice) \[$w.list curselection\]"
		bind $w <KeyPress-Escape> {
			set NSVaultEditor::ListChoice(choice) -1
		}

		set ListChoice(listbox) $w.list
	}

	set ListChoice(list) $list
	if {$initial < 0 || $initial >= [llength $list]} {
		set initial 0
	}
	$w.list selection clear 0 end
	$w.list selection set $initial
	$w.list see $initial

	wm geometry $w +[expr [winfo x $parent] + 20]+[expr [winfo y $parent] + 20]
	wm transient $w $parent
	wm title $w $title
	wm deiconify $w

	grab $w
	focus $w.list
	set ListChoice(choice) -1
	tkwait variable NSVaultEditor::ListChoice(choice)

	grab release $w
	wm withdraw $w

	return $ListChoice(choice)
}

proc NSVaultEditor::OpenQuest {oop} {

	set glob [glob -nocomplain -directory [Path lib edit] {q[0-9]*.txt}]
	set glob [lsort -dictionary $glob]

	foreach path $glob {
		set name [file tail $path]
		set chan [open $path]
		set buf [read $chan]
		close $chan
		set questNum -1
		set name <Untitled>
		foreach string [split $buf \n] {
			if {[string match "Q:*:N:*" $string]} {
				set list [split $string :]
				set questNum [lindex $list 1]
				set name [lindex $list 3]
				break
			}
		}
		lappend listStr "$questNum: $name"
		lappend listNum $questNum
		lappend listPath $path
	}

	set choice [ListChoice $oop "Choose a quest" $listStr 0]
	if {$choice == -1} return

	set path [lindex $listPath $choice]
	Info $oop path,text [file tail $path]
	Info $oop path,vault ""
	Erase $oop
	ReadQuest $oop $path

	return
}

proc NSVaultEditor::Replace_Init {oop} {

	set parent [Info $oop win]

	set w $parent.replace
	if {[winfo exists $w]} {
		destroy $w
	}
	toplevel $w
	wm title $w Replace
	wm withdraw $w
	TransientToWin $w $parent

	wm protocol $w WM_DELETE_WINDOW "wm withdraw $w"

	message $w.message -text "Original icon layers on the left,\
		replaced icon layers on the right. Click a layer to make\
		it current, then select an icon in the icon browser or use\
		the eyedrop tool. Acts on entire vault or selected area." -width 200

	set iconW $NSIconBrowser::Priv(maxIconWidth)
	set iconH $NSIconBrowser::Priv(maxIconHeight)

	set canvas $w.canvas
	canvas $canvas -highlightthickness 0 -borderwidth 0 \
		-height [expr {($iconH + 6) * 4}] \
		-width [expr {($iconW + 6) * 2 + 60}] \
		-scrollregion {0 0 0 0}
	set x [expr {$iconW / 2 + 3}]
	foreach which {old new} {
		foreach layer {4 3 2 1} {
			set tag icon$which$layer
			$canvas create widget $x \
				[expr {3 + (4 - $layer) * ($iconH + 6)}] -anchor n \
				-borderwidth 1 -bordercolor black \
				-tags "$tag icon"
			$canvas bind $tag <ButtonPress-1> "
				%W itemconfigure icon -borderwidth 1
				%W itemconfigure $tag -borderwidth 2
				NSVaultEditor::Info $oop replace,current \{$which $layer\}
			"
		}
		set x [expr {($iconW + 6) * 2 + 60 - $iconW / 2 - 3}]
	}
	button $w.button -text Replace \
		-command "NSVaultEditor::Replace_Invoke $oop"
	pack $w.message -side top
	pack $canvas -side top -pady 5
	pack $w.button -side top -pady 5

	$canvas itemconfigure iconold1 -borderwidth 2
	Info $oop replace,current "old 1"

	Info $oop replace,win $w
	Info $oop replace,canvas $canvas

	wm geometry $w +[expr [winfo x $parent] + 20]+[expr [winfo y $parent] + 20]

	bind $w <KeyPress-Return> \
		"NSVaultEditor::Replace_Invoke $oop"

	return
}

proc NSVaultEditor::Replace_SetIcon {oop icon} {

	if {![info exists ::NSVaultEditor($oop,replace,current)]} return
	scan [Info $oop replace,current] "%s %d" which layer
	[Info $oop replace,canvas] itemconfigure icon$which$layer -assign "icon $icon"

	return
}

proc NSVaultEditor::Replace_Invoke {oop} {

	set widget [Info $oop widget]
	set sel [Info $oop widget,sel]
	set canvas [Info $oop replace,canvas]

	if {[Info $oop editMode] eq "icon"} {
		set vaultId [Info $oop display,vaultId]
	} else {
		set vaultId [Info $oop edit,vaultId]
	}

	set isSel [$widget itemcget $sel -visible]
	if {$isSel} {
		set bounds [Info $oop select,bounds]
	} else {
		set w [vault width $vaultId] ; incr w -1
		set h [vault height $vaultId] ; incr h -1
		set bounds [list 0 0 $h $w]
	}

	foreach layer {1 2 3 4} {
		scan [$canvas itemcget iconold$layer -assign] "%s %s %d" ignore type index
		set old($layer) "$type $index"
		scan [$canvas itemcget iconnew$layer -assign] "%s %s %d" ignore type index
		set new($layer) "$type $index"
	}

	# Save undo info before replace
	ToUndo $oop

	scan $bounds "%d %d %d %d" y1 x1 y2 x2

vault replace $vaultId \
	[list icon1 $old(1) icon2 $old(2) icon3 $old(3) icon4 $old(4)] \
	[list icon1 $new(1) icon2 $new(2) icon3 $new(3) icon4 $new(4)] \
	$y1 $x1 $y2 $x2

return

	for {set y $y1} {$y <= $y2} {incr y}  {
		for {set x $x1} {$x <= $x2} {incr x} {
			set match 1
			foreach layer {1 2 3 4} {
				set icon [vault get $vaultId icon$layer $y $x]
				if {![string equal $icon $old($layer)]} {
					set match 0
					break
				}
			}
			if {$match} {
				foreach layer {1 2 3 4} {
					vault put $vaultId icon$layer [list [list $new($layer)]] $y $x
				}
			}
		}
	}

	return
}

# NSVaultEditor::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSVaultEditor::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	NSVaultEditor::SetEditMode $oop [lindex {icon feature} $index]

	return
}

