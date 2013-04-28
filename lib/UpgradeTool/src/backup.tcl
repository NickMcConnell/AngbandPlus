# File: backup.tcl

# Purpose: backup existing files

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBackup {

# namespace eval NSBackup
}

# NSBackup::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBackup::InitModule {} {

	return
}

# NSBackup::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBackup::CloseModule {} {

	return
}

# InitBackup --
#
#	When a Src file is copied or written to the Dst directory, and it
#	conflicts with one which is already there, the Dst file is backed
#	up.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc InitBackup {} {

	set path [PathDst UpgradeTool]

	if {![MkDir $path]} {
		if {!$::DRY_RUN} {
			set name "This folder is safe to delete.txt"
			file copy [Path $name] $path
		}
	}

	set attempt 0
	set list [glob -nocomplain -directory $path Attempt*]
	if {[llength $list]} {
		scan [file tail [lindex $list end]] Attempt%d attempt
	}
	incr attempt

	set name [format Attempt%03d $attempt]
	set path [PathDst UpgradeTool $name]
	Global backup,root $path

	# Create PathDst/UpgradeTool/Attempt001
	MkDir [Global backup,root]

	# We will create a log file in the Attempt001 directory
	set logFile [file join [Global backup,root] UpgradeTool.log]
	if {!$::DRY_RUN} {
		if {[catch {open $logFile w} fileId]} {
			MsgBox -icon error \
				-message [format [mc err-open-log] UpgradeTool.log $fileId]
			Exit
		}
		Global logFileId $fileId
	}
	Global backup,log $logFile

	return
}

# BackupFile --
#
#	Create a copy of the given file in the same directory. The original
#	file is then deleted.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc BackupFile {pathDst} {

	# PathSrc/foo/bar -> PathDst/UpgradeTool/Attempt001/foo/bar
	set pathBak [file join [Global backup,root] {*}[StripCommon $pathDst [PathDst]]]

	LogMessage "backup\n  [SimplifyPath Dst $pathDst]\n  [SimplifyPath Dst $pathBak]"
	if {!$::DRY_RUN} {
		if {[catch {
			MkDir [file dirname $pathBak]
			file rename -force $pathDst $pathBak
		} result]} {
			LogMessage ">>>>>\n$result\n<<<<<"
		}
	}

	return
}

