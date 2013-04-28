# File: playlist.tcl

# Purpose: music playlists

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.

namespace eval NSPlaylist {

	namespace export playlist

	variable vNextId 0
	variable vListOf {}
	variable vPlaylist
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::playlist {args} {

	if {![llength $args]} {
		error "wrong # args: should be \"playlist option ?arg...?\""
	}

	set argV [lrange $args 1 end]
	set argC [llength $argV]

	switch -- [lindex $args 0] {
		new {
			return [new {*}$argV]
		}
		listof {
			if {$argC} {
				error "wrong # args: should be \"playlist listof\""
			}
			return [listof]
		}
		configure {
			if {!$argC} {
				error "wrong # args: should be \"playlist configure $playlistId ?args...?\""
			}
			return [configure {*}$argV]
		}
		append {
			if {$argC != 2} {
				error "wrong # args: should be \"playlist append $playlistId $songs\""
			}
			return [append {*}$argV]
		}
		cget {
			if {$argC != 2} {
				error "wrong # args: should be \"playlist cget $playlistId $option\""
			}
			return [cget {*}$argV]
		}
		clear {
			if {$argC != 1} {
				error "wrong # args: should be \"playlist clear $playlistId\""
			}
			return [clear {*}$argV]
		}
		delete {
			if {$argC != 1} {
				error "wrong # args: should be \"playlist delete $playlistId\""
			}
			return [delete {*}$argV]
		}
		songs {
			if {$argC != 1} {
				error "wrong # args: should be \"playlist songs $playlistId\""
			}
			return [songs {*}$argV]
		}
		remove {
			if {$argC != 2} {
				error "wrong # args: should be \"playlist remove $playlistId $indexList\""
			}
			return [remove {*}$argV]
		}
		default {
			error "unkonwn option \"$option\""
		}
	}

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::new {args} {

	variable vNextId
	variable vListOf
	variable vPlaylist

	set id [incr vNextId]
	lappend vListOf $id

	set vPlaylist($id,file) ""
	set vPlaylist($id,title) ""
	set vPlaylist($id,songs) {}
	set vPlaylist($id,active) 1
	set vPlaylist($id,location) {town-day town-night dungeon quest wild-day wild-night arena}
	set vPlaylist($id,level) {0 127}

	configure $id {*}$args

	return $id
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::listof {} {

	variable vListOf

	return $vListOf
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::configure {id args} {

	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	foreach {option value} $args {
		switch -- $option {
			-active {
				set vPlaylist($id,active) $value
			}
			-file {
				set vPlaylist($id,file) $value
			}
			-level {
				set vPlaylist($id,level) $value
			}
			-location {
				set vPlaylist($id,location) $value
			}
			-title {
				set vPlaylist($id,title) $value
			}
			default {
				error "unknown playlist option \"$option\""
			}
		}
	}

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::append {id songs} {

	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	# FIXME: filter out duplicates
	lappend vPlaylist($id,songs) {*}$songs

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::cget {id option} {

	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	switch -- $option {
		-active {
			return $vPlaylist($id,active)
		}
		-file {
			return $vPlaylist($id,file)
		}
		-level {
			return $vPlaylist($id,level)
		}
		-location {
			return $vPlaylist($id,location)
		}
		-title {
			return $vPlaylist($id,title)
		}
		default {
			error "unknown playlist option \"$option\""
		}
	}

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::delete {id} {

	variable vListOf
	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	array unset vPlaylist $id,*
	set index [lsearch -integer $vListOf $id]
	set vListOf [lreplace $vListOf $index $index]

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::songs {id} {

	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	return $vPlaylist($id,songs)

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::clear {id} {

	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	set vPlaylist($id,songs) {}

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::remove {id indexList} {

	variable vPlaylist

	if {![info exists vPlaylist($id,file)]} {
		error "invalid playlist \"$id\""
	}

	set songs $vPlaylist($id,songs)
	foreach index [lsort -decreasing -integer -unique $indexList] {
		set songs [lreplace $songs $index $index]
	}
	set vPlaylist($id,songs) $songs

	return
}

# NSPlaylist:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPlaylist::GetActivePlaylists {location dayOrNight level} {

	set result {}

	if {$location eq "town" || $location eq "wild"} {
		set location $location-$dayOrNight
	}
	foreach playlistId [listof] {
		if {![cget $playlistId -active]} continue
		if {[lsearch -exact [cget $playlistId -location] $location] == -1} continue
		if {$location eq "dungeon" || $location eq "quest"} {
			if {$level < [lindex [cget $playlistId -level] 0]} continue
			if {$level > [lindex [cget $playlistId -level] 1]} continue
		}
		lappend result $playlistId
	}

	return $result
}


