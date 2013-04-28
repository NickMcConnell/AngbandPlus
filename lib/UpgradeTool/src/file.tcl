# File: file.tcl

# Purpose: file and directory utils

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSFile {

# namespace eval NSFile
}

# NSFile::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFile::InitModule {} {

	return
}

# NSFile::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFile::CloseModule {} {

	return
}

# CopyFile --
#
#	Copy.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CopyFile {pathSrc pathDst} {

	LogMessage "copy\n  [SimplifyPath Src $pathSrc]\n  [SimplifyPath Dst $pathDst]"

	if {!$::DRY_RUN} {	
		if {[catch {
			file copy $pathSrc $pathDst
		} result]} {
			LogMessage ">>>>>\n$result\n<<<<<"
		}
	}

	return
}

# MkDir --
#
#	Create a directory if it does not exist. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MkDir {path} {

	set exists [file exists $path]
	if {!$exists} {
		if {!$::DRY_RUN} {
			file mkdir $path
		}
	}
	return $exists
}

# FileToString --
#
#	Return contents of a file as a string. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FileToString {path} {

	set chan [open $path]
	set buf [read -nonewline $chan]
	close $chan
	return $buf
}

# StringToFile --
#
#	Set contents of a file from a string. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc StringToFile {path buf} {

	if {$::TEST} {
		NSTest::TestFile $path $buf
	}
	if {[file exists $path]} {
		BackupFile $path
	}

	LogMessage "Writing [SimplifyPath Dst $path]"

	Status "Writing [file nativename [file join {*}[StripCommon $path [PathDst]]]]"

	if {$::DEBUG} {
		puts "### [Global Src,versionStr] -> [Global Dst,versionStr] [file tail $path]\n$buf"
	} else {
		LogMessage =====\n$buf=====
	}
	
	if {!$::DRY_RUN} {
		if {[catch {open $path [list CREAT WRONLY TRUNC]} chan]} {
			LogMessage ">>>>>\n$chan\n<<<<<"
			return
		}
		if {[catch {
			puts $chan $buf
		} err]} {
			LogMessage ">>>>>\n$err\n<<<<<"
		}
		close $chan
	}

	return
}

# SimplifyPath --
#
#	Strip off leading path elements. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc SimplifyPath {which path} {

	set len [llength [file split [Path$which]]]
	if {$len == 2} {
		return $path
	}
	set list [lreplace [file split $path] 1 [expr {$len - 2}] ...]
	return [eval file join $list]
}

# StripCommon --
#
#	Remove matching elements in path2 from path1.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc StripCommon {path1 path2} {

	if {[file exists $path1]} {
		set path1 [LongName $path1]
	}
	if {[file exists $path2]} {
		set path2 [LongName $path2]
	}

	set list1 [file split $path1]
	set list2 [file split $path2]
	set len [llength $list2]
	return [lrange $list1 $len end]
}

# IsFileInX --
#
#	Determine whether path1 is a child of path2.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsFileInX {path1 path2 {deep 1}} {

	if {![string length $path1]} {
		return 0
	}

	if {[file exists $path1]} {
		set path1 [LongName $path1]
	}
	if {[file exists $path2]} {
		set path2 [LongName $path2]
	}

	foreach elem1 [file split $path1] elem2 [file split $path2] {
		if {[Platform windows]} {
			set elem1 [string tolower $elem1]
			set elem2 [string tolower $elem2]
		}
		if {![string length $elem1]} {
			return 0
		}
		if {![string length $elem2]} {
			if {$deep} {
				return 1
			}
			if {[llength [file split $path1]] == [llength [file split $path2]] + 1} {
				return 1
			}
			return 0
		}
		if {[string compare $elem1 $elem2]} {
			return 0
		}
	}

	return 1
}

# IsFileInPathSrc --
#
#	Determine whether path1 is a child of [PathSrc].
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsFileInPathSrc {path} {

	if {![string length $path]} {
		return 0
	}
	return [IsFileInX $path [PathSrc]]
}

# IsFileInCPathSrc --
#
#	Determine whether path1 is a child of [CPathSrc].
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsFileInCPathSrc {path} {

	if {![string length [CPathSrc]]} {
		return 0
	}
	if {![string length $path]} {
		return 0
	}
	return [IsFileInX $path [CPathSrc]]
}

# UpdatePath --
#
#	Given an absolute path to a file or directory, determine if the path
#	is somewhere inside the Src directory. If so, return a new path which
#	points to the same object in the Dst directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc UpdatePath {path} {

	if {[IsFileInPathSrc $path]} {
		set pathOrig $path
		set path [PathDst {*}[StripCommon $path [PathSrc]]]
		LogMessage "updated path\n  [SimplifyPath Src $pathOrig]\n  [SimplifyPath Dst $path]"
	}
	return $path
}

# UpdateCPath --
#
#	Given an absolute path to a file or directory, determine if the path
#	is somewhere inside the Src CommonTk directory. If so, return a new
#	path which points to the same object in the Dst CommonTk directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc UpdateCPath {path} {

	if {[IsFileInCPathSrc $path]} {
		set pathOrig $path
		set path [CPathDst {*}[StripCommon $path [CPathSrc]]]
		LogMessage "updated path\n  [SimplifyPath Src $pathOrig]\n  [SimplifyPath Dst $path]"
	}
	return $path
}

