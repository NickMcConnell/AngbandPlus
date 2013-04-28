# File: music.tcl

# Purpose: the Music Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMusic {

# namespace eval NSMusic
}

# NSMusic::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::InitModule {} {

	MsgCatInit music

	NSModule::LoadIfNeeded NSList

	foreach name {rewind play pause stop ff mute muted} {
		InitImageIfNeeded Image_Music_$name music-$name.gif
	}

	NSObject::New NSMusic

	return
}

# NSMusic::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::CloseModule {} {

	catch {
		destroy [Window music]
	}

	return
}

# NSMusic::NSMusic --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::NSMusic {oop} {

	InitWindow $oop

	set win [Info $oop win]

	Info $oop ignoreSel 0
	Info $oop song,current -1
	Info $oop song,match {}

	NSWindowManager::RegisterWindow music $win \
		"GetDefaultGeometry $win reqwidth main" "" \
		"NSMusic::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMusic $oop $win

	qebind NSMusic <Music-notify> {
		NSMusic::SynchControls [Global music,oop]
	}
	qebind NSMusic <Music-pause> {
		NSMusic::SynchControls [Global music,oop]
	}
	qebind NSMusic <Music-play> {
		NSMusic::SynchControls [Global music,oop]
	}
	qebind NSMusic <Music-resume> {
		NSMusic::SynchControls [Global music,oop]
	}
	qebind NSMusic <Music-stop> {
		NSMusic::SynchControls [Global music,oop]
	}

	#
	# Global list of application windows
	#

	Global music,oop $oop
	Window music [Info $oop win]

	PlaylistEditor_Init $oop

	return
}

# NSMusic::~NSMusic --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::~NSMusic {oop} {

	qeunbind NSMusic

	return
}

# NSMusic::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::Info {oop info args} {

	global NSMusic

	# Verify the object
	NSObject::CheckObject NSMusic $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMusic($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMusic($oop,$info)
			}
		}
	}

	return
}

# NSMusic::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::InitWindow {oop} {

	set win .music$oop
	toplevel $win
	wm title $win [mc Music]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMusic::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	MakeDivider $win.divider1 x

	set font [Value font,knowledge]

	#
	# Playlist List
	#

	set rowHgt [font metrics $font -linespace]
	if {[image height Image_Checked] > $rowHgt} {
		set rowHgt [image height Image_Checked]
	}
	incr rowHgt 8
	set width 350
	set height [expr {$rowHgt * 4}]

	set frame $win.framePlaylist
	NSList::New $frame -checkbutton yes
	set tree $frame.tree
	$tree configure -width $width -height $height -selectmode extended -showheader yes
	$tree column configure 0 -text [mc "Playlist"]

	NSList::OnSelection $tree \
		"NSMusic::SelectionChanged_Playlist $oop %T %c %S %D"

	NSList::OnInvoke $tree \
		"NSMusic::Invoke_Playlist $oop"

	NSList::OnToggle $tree \
		"NSMusic::Toggle $oop %T %I %r"

	Info $oop playlist,tree $tree
	Info $oop playlist,selection,rows {}
	Info $oop playlist,selection,id -1

	#
	# Song List
	#

	set rowHgt [font metrics $font -linespace]
	if {[image height Image_Checked] > $rowHgt} {
		set rowHgt [image height Image_Checked]
	}
	incr rowHgt 8
	set width 350
	set height 100

	set frame $win.frameSong
	NSList::New $frame
	set tree $frame.tree
	$tree configure -width $width -height $height -selectmode extended -showheader yes
	$tree column configure 0 -text [mc "Song"]

	NSList::OnSelection $tree \
		"NSMusic::SelectionChanged_Song $oop %T %c %S %D"

	# Do something when a selected song is clicked.
	NSList::OnClick $tree \
		"NSMusic::Click_Song $oop"

	NSList::OnInvoke $tree \
		"NSMusic::Invoke_Song $oop %r"

	Info $oop song,tree $tree
	Info $oop song,selection,rows {}

	#
	# Music controls
	#

	set frame $win.frameControl
	frame $frame \
		-borderwidth 1 -relief sunken

	tk::button $frame.buttonPlay \
		-image Image_Music_play -width 20 -height 20 \
		-command "NSMusic::MenuInvoke $oop ??? E_SONG_PLAY"
	tk::button $frame.buttonPause \
		-image Image_Music_pause -width 20 -height 20 \
		-command "NSMusic::MenuInvoke $oop ??? E_SONG_PAUSE"
	tk::button $frame.buttonStop \
		-image Image_Music_stop -width 20 -height 20 \
		-command "NSMusic::MenuInvoke $oop ??? E_SONG_STOP"
	tk::button $frame.buttonMute \
		-image Image_Music_mute -width 20 -height 20 \
		-command "NSMusic::MenuInvoke $oop ??? E_SONG_MUTE"

#	label $frame.label \
#		-text [mc Volume:]
	if {$::UseTile && 0} {
		ttk::scale $frame.volume \
			-orient horizontal \
			-length 100 -from 0 -to 100 \
			-command "NSMusic::SetVolume $oop"
	} else {
		scale $frame.volume \
			-orient horizontal \
			-width 10 -sliderlength 12 -length 100 -from 0 -to 100 \
			-showvalue no -command "NSMusic::SetVolume $oop"
	}
	$frame.volume set [music volume]

	pack $frame.buttonPlay $frame.buttonPause $frame.buttonStop \
		$frame.buttonMute -side left -padx {10 0} -pady 5
#	pack $frame.label \
#		-side left
	pack $frame.volume \
		-side left -padx {10 0} -pady 5

	Info $oop volume $frame.volume

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid columnconfigure $win 0 -weight 1
	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid rowconfigure $win 2 -weight 2
	grid rowconfigure $win 3 -weight 0
	grid rowconfigure $win 4 -weight 0

	grid $win.divider1 \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.framePlaylist \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameSong \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameControl \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.statusBar \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSMusic::Close $oop"
	bind $win <Control-KeyPress-w> "NSMusic::Close $oop"
if 0 {
	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus $canvas
		}
	"
}
	return
}

# NSMusic::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::InitMenus {oop} {

	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSMusic::MenuSetup $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSMusic::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSMusic::MenuInvoke $oop"

	#
	# Music Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_MUSIC
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_MUSIC -label [mc Music] -underline 0 -identifier M_MUSIC

	set entries {}
	lappend entries [list -type checkbutton -label [mc "Use Music"] \
		-variable Global(music,play) -identifier E_USE_MUSIC]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_MUSIC $entries

	#
	# Playlist Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_PLAYLIST
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_PLAYLIST -label [mc Playlist] -underline 0 -identifier M_PLAYLIST

	set entries {}
	lappend entries [list -type command -label [mc play-new] \
		-underline 0 -identifier E_PLAYLIST_NEW]
	lappend entries [list -type command -label [mc play-add] \
		-underline 0 -identifier E_PLAYLIST_ADD]
	lappend entries [list -type command -label [mc play-edit] \
		-underline 0 -identifier E_PLAYLIST_EDIT]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc play-remove] \
		-underline 0 -identifier E_PLAYLIST_REMOVE]

	NSMenu::MenuInsertEntries $mbar -end MENU_PLAYLIST $entries

	#
	# Song Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SONG
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_SONG -label [mc Song] -underline 0 -identifier M_SONG

	set entries {}
	lappend entries [list -type command -label [mc song-add-f] \
		-underline 0 -identifier E_SONG_ADD_FILES]
	lappend entries [list -type command -label [mc song-add-d] \
		-underline 0 -identifier E_SONG_ADD_FOLDER]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc song-remove-sel] \
		-underline 0 -identifier E_SONG_REMOVE_SELECTED]
	lappend entries [list -type command -label [mc song-remove-miss] \
		-underline 0 -identifier E_SONG_REMOVE_MISSING]
	lappend entries [list -type command -label [mc song-remove-all] \
		-underline 0 -identifier E_SONG_REMOVE_ALL]

	NSMenu::MenuInsertEntries $mbar -end MENU_SONG $entries

	set MenuString(M_MUSIC) \
		"Contains commands for using music."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSMusic::MenuSetup --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::MenuSetup {oop mbarId} {

	lappend identList E_USE_MUSIC E_RANDOMIZE E_DIRECTORY E_UPDATE E_CLOSE

	lappend identList E_PLAYLIST_NEW E_PLAYLIST_ADD
	set count [llength [Info $oop playlist,selection,rows]]
	if {$count == 1} {
		lappend identList E_PLAYLIST_EDIT E_SONG_ADD_FILES \
			E_SONG_ADD_FOLDER
		if {[llength [playlist songs [Info $oop playlist,selection,id]]]} {
			lappend identList E_SONG_REMOVE_MISSING E_SONG_REMOVE_ALL
		}
	}
	if {$count >= 1} {
		lappend identList E_PLAYLIST_REMOVE
	}

	set count [llength [Info $oop song,selection,rows]]
	if {$count} {
		lappend identList E_SONG_REMOVE_SELECTED
	}


	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSMusic::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		{} {
			set desc {}
		}

		E_USE_MUSIC {
			if {[Value music,play]} {
				set desc "Turns off music."
			} else {
				set desc "Turns on music."
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

# NSMusic::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::MenuInvoke {oop menuId ident} {

	switch -glob -- $ident {
		E_USE_MUSIC {NSMainWindow::ToggleMusic [Global main,oop]}
		E_RANDOMIZE {ToggleRandomize $oop}
		E_CLOSE {Close $oop}

		E_PLAYLIST_NEW {
			PlaylistEditor_New $oop
		}
		E_PLAYLIST_ADD {
			set types [list \
				[list [mc "Playlists"] {.mus}] \
				[list [mc "All files"] *] \
			]
			set files [tk_getOpenFile -parent [Info $oop win] \
				-filetypes $types -multiple yes]
			if {[llength $files]} {
				set playlistFiles {}
				foreach playlistId [playlist listof] {
					lappend playlistFiles [playlist cget $playlistId -file]
				}
				foreach file $files {
					# FIXME: make sure paths are normalized
					if {[lsearch -exact $playlistFiles $file] != -1} continue
					Config::Music::Playlist $file 1
				}
				SetList_Playlist $oop
			}
		}
		E_PLAYLIST_EDIT {
			set playlistId [Info $oop playlist,selection,id]
			PlaylistEditor_Edit $oop $playlistId
		}
		E_PLAYLIST_REMOVE {
			# Do you want to delete the playlist file "..." from your hard drive?
			set playlistIds {}
			foreach row [Info $oop playlist,selection,rows] {
				lappend playlistIds [lindex [playlist listof] $row]
			}
			foreach playlistId $playlistIds {
				playlist delete $playlistId
			}
			SetList_Playlist $oop
		}

		E_SONG_ADD_FILES {
			# TODO: Get file types from current music.dll
			set types [list \
				[list [mc "All supported types"] {.mo3 .it .xm .s3m .mtm .mod .umx .mp3 .mp2 .mp1 .ogg .wav}] \
				[list [mc "All files"] *] \
			]
			set files [tk_getOpenFile -parent [Info $oop win] \
				-filetypes $types -multiple yes]
			if {[llength $files]} {
				set playlistId [Info $oop playlist,selection,id]
				set songs [playlist songs $playlistId]
				foreach file $files {
					if {[lsearch -exact $songs $file] != -1} continue
					playlist append $playlistId [list $file]
				}
				SetList_Song $oop
			}
		}
		E_SONG_ADD_FOLDER {
			set dir [tk_chooseDirectory -parent [Info $oop win] \
				-mustexist yes]
			if {$dir ne ""} {
				# Add all files in dir of allowed type
				set files {}
				# TODO: Get file types from current music.dll
				set types [list .mo3 .it .xm .s3m .mtm .mod .umx .mp3 .mp2 .mp1 .ogg .wav]
				foreach path [glob -nocomplain -directory $dir *] {
					set extension [file extension $path]
					set extension [string tolower $extension]
					if {[lsearch -exact $types $extension] != -1} {
						lappend files $path
					}
				}
				if {[llength $files]} {
					set playlistId [Info $oop playlist,selection,id]
					set songs [playlist songs $playlistId]
					foreach file $files {
						if {[lsearch -exact $songs $file] != -1} continue
						playlist append $playlistId [list $file]
					}
					SetList_Song $oop
				}
			}
		}
		E_SONG_REMOVE_SELECTED {
			set indexList {}
			foreach row [Info $oop song,selection,rows] {
				lappend indexList [lindex [Info $oop song,rowToPlaylistIndex] $row]
			}
			set playlistId [Info $oop playlist,selection,id]
			playlist remove $playlistId $indexList
			SetList_Song $oop
if 0 {
			set count [llength $indexList]
			set title [playlist cget $playlistId -title]
			tk_messageBox -parent [Info $oop win] \
				-title [mc "Remove Selected Songs"] \
				-message [format [mc "Removed %d songs from the playlist \"%s\"."] $count $title]
}
		}
		E_SONG_REMOVE_MISSING {
			set playlistId [Info $oop playlist,selection,id]
			set index 0
			set missing {}
			foreach song [playlist songs $playlistId] {
				if {![file exists $song]} {
					lappend missing $index
				}
				incr index
			}
			set count [llength $missing]
			if {$count} {
				playlist remove $playlistId $missing
				SetList_Song $oop
if 0 {
				set title [playlist cget $playlistId -title]
				tk_messageBox -parent [Info $oop win] \
					-title [mc "Remove Missing Songs"] \
					-message [format [mc "Removed %d songs from the playlist \"%s\"."] $count $title]
}
			}
		}
		E_SONG_REMOVE_ALL {
			set playlistId [Info $oop playlist,selection,id]
			set title [playlist cget $playlistId -title]
			set result [tk_messageBox -parent [Info $oop win] -type yesno \
				-icon question -title [mc title-song-remove-all] \
				-message [format [mc msg-song-remove-all] $title]]
			if {$result eq "yes"} {
				playlist clear $playlistId
				SetList_Song $oop
			}
		}
		E_SONG_PLAY {
			if {[music state] eq "paused"} {
				music resume
				qegenerate <Music-resume>
			} else {
				set playlistId [Info $oop playlist,selection,id]
				set row [lindex [Info $oop song,selection,rows] 0]
				set index [lindex [Info $oop song,rowToPlaylistIndex] $row]
				set song [lindex [playlist songs $playlistId] $index]
				if {[music issong]} {
					music stop
					qegenerate <Music-stop>
				}
				PlaySong $song
			}
		}
		E_SONG_PAUSE {
			music pause
			qegenerate <Music-pause>
		}
		E_SONG_STOP {
			music stop
			qegenerate <Music-stop>
		}
		E_SONG_MUTE {
			music mute [expr ![music mute]]
			SynchControls $oop
		}
	}

	return
}

# NSMusic::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			SetList_Playlist $oop
			SynchControls $oop
		}
		postDisplay {
			if {$first} {
			}
		}
		postWithdraw {
		}
	}

	return
}

# NSMusic::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::Close {oop} {

	NSWindowManager::Undisplay music

	return
}

# NSMusic::SelectionChanged_Playlist --
#
#	Called when the module selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::SelectionChanged_Playlist {oop tree count select deselect} {

	set rows {}
	foreach item [$tree selection get] {
		lappend rows [NSList::Item2Row $tree $item]
	}

	Info $oop playlist,selection,rows $rows
	Info $oop playlist,selection,id -1

	if {$count == 1} {
		set row [lindex $rows 0]
		set playlistId [lindex [playlist listof] $row]
		Info $oop playlist,selection,id $playlistId
	}

	SetList_Song $oop

	return
}

# NSMusic::Invoke_Playlist --
#
#	Do something when a playlist is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::Invoke_Playlist {oop} {

	PlaylistEditor_Edit $oop [Info $oop playlist,selection,id]

	return
}

# NSMusic::Click_Song --
#
#	Do something when a selected module is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::Click_Song {oop} {

	return
}

# NSMusic::Invoke_Song --
#
#	Do something when the Song list is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::Invoke_Song {oop row} {

	set playlistId [Info $oop playlist,selection,id]
	set index [lindex [Info $oop song,rowToPlaylistIndex] $row]
	set song [lindex [playlist songs $playlistId] $index]
	PlaySong $song

	return
}

# NSMusic::SelectionChanged_Song --
#
#	Called when the module selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::SelectionChanged_Song {oop tree count select deselect} {

	set rows {}
	foreach item [$tree selection get] {
		lappend rows [NSList::Item2Row $tree $item]
	}

	Info $oop song,selection,rows $rows

	SynchControls $oop

	return
}

# NSMusic::SetList_Playlist --
#
#	Set the list of playlists.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::SetList_Playlist {oop} {

	set tree [Info $oop playlist,tree]

	# Clear the list
	NSList::Clear $tree

	foreach playlistId [playlist listof] {
		set title [playlist cget $playlistId -title]
		set item [$tree item create]
		NSList::SetText $tree $item $title
		if {[playlist cget $playlistId -active]} {
			$tree item state set $item checked
		}
		$tree item lastchild root $item
	}

	return
}

# NSMusic::SetList_Song --
#
#	Set the list of songs.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::SetList_Song {oop} {

	set tree [Info $oop song,tree]
	set playlistId [Info $oop playlist,selection,id]

	# Clear the list
	NSList::Clear $tree

	set rowToPlaylistIndex {}

	if {$playlistId != -1} {
		set index 0
		set fileList {}
		# Remember each file name and its index in the playlist
		foreach path [playlist songs $playlistId] {
			lappend fileList [list $index [file tail $path]]
			incr index
		}
		# Sort file names
		set fileList [lsort -dictionary -index 1 $fileList]
		foreach {index file} [eval concat $fileList] {
			set item [$tree item create]
			NSList::SetText $tree $item $file
			$tree item lastchild root $item
			lappend rowToPlaylistIndex $index
		}
	}

	# Remember the index of the song in the playlist
	Info $oop song,rowToPlaylistIndex $rowToPlaylistIndex

	return
}

# NSMusic::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSMusic::Toggle --
#
#	Toggle a checkbox in a list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::Toggle {oop tree item row} {

	set playlistId [lindex [playlist listof] $row]
	set active [playlist cget $playlistId -active]
	set active [expr {!$active}]

	$tree item state set $item ~checked
	playlist configure $playlistId -active $active

	return
}

# NSMusic::SynchControls --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::SynchControls {oop} {

	set win [Info $oop win]
	set frame $win.frameControl

	switch -- [music state] {
		none {
			if {[llength [Info $oop song,selection,rows]] == 1} {
				$frame.buttonPlay configure -state normal
			} else {
				$frame.buttonPlay configure -state disabled
			}
			$frame.buttonPause configure -state disabled
			$frame.buttonStop configure -state disabled
		}
		playing {
			$frame.buttonPlay configure -state disabled
			$frame.buttonPause configure -state normal
			$frame.buttonStop configure -state normal
		}
		paused {
			$frame.buttonPlay configure -state normal
			$frame.buttonPause configure -state disabled
			$frame.buttonStop configure -state normal
		}
	}

	if {[music mute]} {
		$frame.buttonMute configure -image Image_Music_muted
	} else {
		$frame.buttonMute configure -image Image_Music_mute
	}

	return
}

# NSMusic::SetVolume --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::SetVolume {oop volume} {

	music volume [expr int($volume)]
	return
}

# NSMusic::PlaylistEditor_Init --
#
#	Create the window used to edit playlist options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::PlaylistEditor_Init {oop} {

	set win [Info $oop win].playlist
	toplevel $win
	wm title $win [mc "Playlist Editor"]
	wm withdraw $win

	TransientToWin $win [Info $oop win]

	#
	# Title
	#

	set frame $win.frameTitle
	labelframe $frame \
		-text [mc Title]
#	label $frame.label \
#		-text [mc "The title of the playlist is displayed in the Music Window."]
	entry $frame.entry \
		-width 40

	pack $frame \
		-side top -anchor w -padx 10 -pady 10
#	pack $frame.label \
#		-side top -padx 2 -anchor w
	pack $frame.entry \
		-side left -padx 10 -pady 5

	#
	# Location
	#

	set frame $win.frameBoxes
	labelframe $frame \
		-text [mc "Location"]
	checkbutton $frame.town-day -text [mc "Town (Day)"] \
		-variable NSMusic($oop,playlist,editor,check,town-day)
	checkbutton $frame.town-night -text [mc "Town (Night)"] \
		-variable NSMusic($oop,playlist,editor,check,town-night)
	checkbutton $frame.dungeon -text [mc "Dungeon"] \
		-variable NSMusic($oop,playlist,editor,check,dungeon)
	checkbutton $frame.quest -text [mc "Quest"] \
		-variable NSMusic($oop,playlist,editor,check,quest)
	checkbutton $frame.wild-day -text [mc "Wild (Day)"] \
		-variable NSMusic($oop,playlist,editor,check,wild-day)
	checkbutton $frame.wild-night -text [mc "Wild (Night)"] \
		-variable NSMusic($oop,playlist,editor,check,wild-night)
	checkbutton $frame.arena -text [mc "Arena"] \
		-variable NSMusic($oop,playlist,editor,check,arena)

	pack $frame \
		-side top -padx 10 -pady 0 -anchor w
	grid $frame.town-day -column 0 -row 0 -sticky w
	grid $frame.town-night -column 0 -row 1 -sticky w
	grid $frame.dungeon -column 1 -row 0 -sticky w
	grid $frame.quest -column 1 -row 1 -sticky w
if {[variant ZANGBANDTK]} {
	grid $frame.wild-day -column 0 -row 2 -sticky w
	grid $frame.wild-night -column 0 -row 3 -sticky w
}
if {[variant KANGBANDTK ZANGBANDTK]} {
	grid $frame.arena -column 1 -row 2 -sticky w
}

	#
	# Level
	#

	set frame $win.frameLevel
	labelframe $frame \
		-text [mc "Dungeon Level"]
	label $frame.labelMin -text [mc "Min:"]
	spinbox $frame.min -width 4 -from 0 -to 127 \
		-textvariable NSMusic($oop,playlist,editor,level,min)
	label $frame.labelMax -text [mc "Max:"]
	spinbox $frame.max -width 4 -from 0 -to 127 \
		-textvariable NSMusic($oop,playlist,editor,level,max)

	pack $frame \
		-side top -padx 10 -pady 10 -anchor w
	pack $frame.labelMin \
		-side left -padx {10 0} -pady 5
	pack $frame.min \
		-side left -pady 5
	pack $frame.labelMax \
		-side left -padx {10 0} -pady 5
	pack $frame.max \
		-side left  -padx {0 10} -pady 5

	#
	# File
	#

	set frame $win.frameFile
	labelframe $frame \
		-text [mc File]
	message $frame.msg \
		-width 300 -borderwidth 0 -justify left -anchor w
	button $frame.button \
		-command "NSMusic::PlaylistEditor_SaveAs $oop" -text [mc "Save As..."]

	pack $frame \
		-side top -padx 10 -pady 0 -expand yes -fill x
	pack $frame.msg \
		-side top -padx 10 -anchor w -expand yes -fill both
	pack $frame.button \
		-side right -padx 10 -pady 10

	#
	# Divider
	#

	MakeDivider $win.divider x
	pack $win.divider \
		-side top -fill x -padx 10 -pady 5

	#
	# Buttons
	#

	set frame $win.frameButtons
	frame $frame \
		-borderwidth 0
	button $frame.ok \
		-text [mc OK] -width 9 -default active \
		-command "NSMusic::PlaylistEditor_OK $oop"
	button $frame.cancel \
		-text [mc Cancel] -width 9 \
		-command "NSMusic::PlaylistEditor_Cancel $oop"

	pack $frame \
		-side top -padx 5 -pady 0 -anchor e
	pack $frame.cancel \
		-side right -padx 5 -pady 5
	pack $frame.ok \
		-side right -padx 5 -pady 5

	NSUtils::SetDefaultButton $win $win.frameButtons.ok

	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $frame.cancel"

	Info $oop playlist,editor $win

	return
}

# NSMusic::PlaylistEditor_OK --
#
#	Handle OK button.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::PlaylistEditor_OK {oop} {

	set win [Info $oop playlist,editor]
	set title [$win.frameTitle.entry get]
	set file [$win.frameFile.msg cget -text]

	if {![Info $oop playlist,editor,saved]} {
		set file ""
	} else {
		set file [file normalize $file]
	}

	if {$title eq ""} {
		bell
		return
	}
	if {$file eq ""} {
		bell
		return
	}

	wm withdraw $win

	switch -- [Info $oop playlist,editor,action] {
		edit {
			set playlistId [Info $oop playlist,editor,id]

			# Handle "Save As..."
			set oldFile [playlist cget $playlistId -file]
			if {$oldFile ne $file} {
				file rename -force $oldFile $file
				if {[file exists $oldFile.bak]} {
					file rename -force $oldFile.bak $file.bak
				}
			}

			set locations {}
			foreach location {town-day town-night dungeon quest wild-day wild-night arena} {
				if {[Info $oop playlist,editor,check,$location]} {
					lappend locations $location
				}
			}
			lappend level [Info $oop playlist,editor,level,min]
			lappend level [Info $oop playlist,editor,level,max]
			playlist configure $playlistId -title $title -file $file \
				-level $level -location $locations
			SetList_Playlist $oop
		}
		new {
			set locations {}
			foreach location {town-day town-night dungeon quest wild-day wild-night arena} {
				if {[Info $oop playlist,editor,check,$location]} {
					lappend locations $location
				}
			}
			lappend level [Info $oop playlist,editor,level,min]
			lappend level [Info $oop playlist,editor,level,max]
			set playlistId [playlist new -title $title -file $file \
				-level $level -location $locations]
			Config::Music::WritePlaylist $playlistId
			SetList_Playlist $oop
		}
	}

	return
}

# NSMusic::PlaylistEditor_Cancel --
#
#	Handle Cancel button.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::PlaylistEditor_Cancel {oop} {

	set win [Info $oop playlist,editor]
	wm withdraw $win

	return
}

# NSMusic::PlaylistEditor_SaveAs --
#
#	Handle "Save As..." button.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::PlaylistEditor_SaveAs {oop} {

	set win [Info $oop playlist,editor]

	if {[Info $oop playlist,editor,action] eq "edit"} {
		set playlistId [Info $oop playlist,editor,id]
		set file [playlist cget $playlistId -file]
	} else {
		set file ""
	}

	if {$file ne ""} {
		set initialdir [file dirname $file]
		set initialfile [file tail $file]
	} else {
		set initialdir [CPathTk config]
		set initialfile [mc untitled.mus]
	}

	set types {
		{{Playlists} .mus}
		{{All files} *}
	}
	set path [tk_getSaveFile -parent $win -initialfile $initialfile \
		-initialdir $initialdir -filetypes $types]
	if {$path eq ""} return

	$win.frameFile.msg configure -text [file nativename $path]

	Info $oop playlist,editor,saved 1

	return
}

# NSMusic::PlaylistEditor_New --
#
#	Create a new playlist.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::PlaylistEditor_New {oop} {

	set win [Info $oop playlist,editor]

	$win.frameFile.msg configure -text [mc (unsaved)]

	$win.frameTitle.entry delete 0 end
	$win.frameTitle.entry insert end [mc "Untitled"]

	Info $oop playlist,editor,action new

	foreach location {town-day town-night dungeon quest wild-day wild-night arena} {
		Info $oop playlist,editor,check,$location 1
	}

	Info $oop playlist,editor,level,min 0
	Info $oop playlist,editor,level,max 127

	Info $oop playlist,editor,saved 0

	WindowPosition $win 2 3

	return
}

# NSMusic::PlaylistEditor_Edit --
#
#	Set options for an existing playlist.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMusic::PlaylistEditor_Edit {oop playlistId} {

	set win [Info $oop playlist,editor]

	$win.frameTitle.entry delete 0 end
	$win.frameTitle.entry insert end [playlist cget $playlistId -title]

	$win.frameFile.msg configure \
		-text [file nativename [playlist cget $playlistId -file]]

	Info $oop playlist,editor,action edit
	Info $oop playlist,editor,id $playlistId

	foreach location {town-day town-night dungeon quest wild-day wild-night arena} {
		Info $oop playlist,editor,check,$location 0
	}
	foreach location [playlist cget $playlistId -location] {
		Info $oop playlist,editor,check,$location 1
	}

	set level [playlist cget $playlistId -level]
	Info $oop playlist,editor,level,min [lindex $level 0]
	Info $oop playlist,editor,level,max [lindex $level 1]

	Info $oop playlist,editor,saved 1

	WindowPosition $win 2 3

	return
}

