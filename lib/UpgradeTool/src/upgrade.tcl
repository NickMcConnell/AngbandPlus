# File: upgrade.tcl

# Purpose: perform the upgrade

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSUpgrade {

# namespace eval NSUpgrade
}

# NSUpgrade::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpgrade::InitModule {} {

	return
}

# NSUpgrade::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpgrade::CloseModule {} {

	return
}

# Upgrade --
#
#	Run the converters.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Upgrade {} {

	LoadVersionData Src
	LoadVersionData Dst

	FindCommonDir Src
	FindCommonDir Dst

	NSModule::LoadIfNeeded NSFile
	NSModule::LoadIfNeeded NSBackup
	InitBackup

	if {!$::TEST} {

	# Sanity: Check that required directories are there
	foreach pathList $NSVersionDataSrc::vDirName {
		set path [PathSrc {*}$pathList]
		if {![file exists $path] || ![file isdirectory $path]} {
			set path [file nativename $path]
			LogMessage "directory \"$path\" not found"
			MsgBox \
				-message [format [mc err-missing-dir] $path [Global Src,dirName]]
			Exit
		}
	}
	foreach pathList $NSVersionDataDst::vDirName {
		set path [PathDst {*}$pathList]
		if {![file exists $path] || ![file isdirectory $path]} {
			set path [file nativename $path]
			LogMessage "directory \"$path\" not found"
			MsgBox \
				-message [format [mc err-missing-dir] $path [Global Dst,dirName]]
			Exit
		}
	}

	if {$::DRY_RUN} {
		LogMessage "\nLog file is [SimplifyPath Dst [Global backup,log]]\n"
	}

	LogMessage "Upgrade begin @ [clock format [clock seconds]]\n"

	LogMessage "Source:      [PathSrc]"
	LogMessage "Destination: [PathDst]"
	LogMessage ""
	LogMessage "Old version: [Global Src,gameName] [Global Src,versionStr]"
	LogMessage "New version: [Global Dst,gameName] [Global Dst,versionStr]"
	LogMessage ""
	set path [PathSrc angband.dll]
	if {[file exists $path]} {
		LogMessage "Old angband.dll [file size $path] @ [clock format [file mtime $path]]"
	} else {
		LogMessage "Old angband.dll not found"
	}
	set path [PathDst angband.dll]
	if {[file exists $path]} {
		LogMessage "New angband.dll [file size $path] @ [clock format [file mtime $path]]"
	} else {
		LogMessage "New angband.dll not found"
	}
	LogMessage ""

	# !TEST
	}

	NSModule::LoadIfNeeded CvtGeom
	NSModule::LoadIfNeeded CvtSetting
	NSModule::LoadIfNeeded CvtSound
	NSModule::LoadIfNeeded CvtSoundCfg
	NSModule::LoadIfNeeded CvtValue

	SynchDirectory {lib user} $NSVersionDataSrc::vFile_user
	SynchDirectory {lib save} $NSVersionDataSrc::vFile_save

	set path [file join tk config birth]
	SynchFile $path 1

	set path [file join tk config char-icon]
	SynchFile $path 1

	set cvtList [list Value Setting Sound Geom]

	# Step 1: Read old files
	foreach cvt $cvtList {
		set cvtId($cvt) [NSObject::New Cvt$cvt]
		Cvt${cvt}::Read $cvtId($cvt)
	}

	# Step 2: Convert to new format
	foreach cvt $cvtList {
		Cvt${cvt}::Convert $cvtId($cvt)
	}

	# Step 3: Update values if needed
	foreach cvt $cvtList {
		Cvt${cvt}::Update $cvtId($cvt)
	}

	# Step 4: Write new files
	foreach cvt $cvtList {
		Cvt${cvt}::Write $cvtId($cvt)
	}

	# Step 5: Clean up
	foreach cvt $cvtList {
		NSObject::Delete Cvt$cvt $cvtId($cvt)
	}
	
	LogMessage "\nUpgrade complete @ [clock format [clock seconds]]" 
	Status ">>> Upgrade complete! Press Quit now."

	if {!$::TEST} {

	MsgBox -message [format [mc msg-complete] [Global Dst,gameName] [Global backup,log]]

	}
	
	return
}

# ReadVariantVersionFile --
#
#	Read the variant version.txt file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ReadVariantVersionFile {variantPath} {

	set path [file join $variantPath version.txt]
	if {[catch {open $path} chan]} {
		error "The variant version.txt file could not be found!\n$path"
	}
	set buf [gets $chan]
	close $chan
	if {[scan $buf "%d.%d.%dr%d" major minor patch release] != 4} {
		error "The variant version.txt file is bugged!\nContents: \"$buf\""
	}
	return [list $major $minor $patch $release]
}

# ExtractVersion --
#
#	Determine variant/version from directory name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ExtractVersion {which} {

	set dirName [file tail [Global dir,$which]]

	# Starting in OmnibandTk-1.5: read version.txt file
	set path [file join [Global dir,$which] version.txt]
	if {[file exists $path]} {
		if {[catch {ReadVariantVersionFile [Global dir,$which]} vers]} {
			MsgBox -message $message
			return 1
		}
		lassign $vers vMajor vMinor vPatch vRelease
		set versionNumber $vMajor$vMinor$vPatch
		set releaseNumber $vRelease
		set name $dirName
	} else {
		if {![regexp "(.*)-(\[0-9\]*)r(\[0-9\]*)" $dirName ignore name versionNumber releaseNumber] ||
			([lsearch -exact [list AngbandTk KAngbandTk OAngbandTk ZAngbandTk] $name] == -1)} {
			set message [format [mc err-weird-dir] $dirName]
			MsgBox -message $message
			return 1
		}
	}

	Global $which,gameName $name
	Global $which,versionNumber $versionNumber
	Global $which,releaseNumber $releaseNumber
	Global $which,versionStr ${versionNumber}r$releaseNumber
	Global $which,dirName $dirName

	return 0
}

# LoadVersionData --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc LoadVersionData {which} {

	set gameName [Global $which,gameName]
	set versionNumber [Global $which,versionNumber]
	set releaseNumber [Global $which,releaseNumber]
	set versionStr [Global $which,versionStr]

	set dataFile data$versionStr
	set path [Path data $gameName $dataFile]
	if {![file exists $path] || [file isdirectory $path]} {
		MsgBox -icon error \
			-message [format [mc err-unsupported] $versionStr $gameName]
		Exit
	}

	# Save directory
	set wd [pwd]

	catch {namespace delete ::NSVersionData$which}

	# Load data/AngbandTk/data123
	cd [Path data $gameName]
	namespace eval ::NSVersionData$which \
		"source $dataFile"

	# Get the version of the Common Files. Load data if any.
	set verCommon [set NSVersionData${which}::vCommonVersion]
	if {$verCommon} {
		cd [Path data CommonTk]
		set ver [string map {. ""} $verCommon]
		set dataFile [Path data CommonTk data$ver]
		namespace eval ::NSVersionData$which \
			"source $dataFile"
	}

	# Restore directory
	cd $wd

	return
}

# FindCommonDir --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FindCommonDir {which} {

	# Get the version of the Common Files
	set verCommon [set NSVersionData${which}::vCommonVersion]

	# Old versions don't have CommonTk directory
	if {!$verCommon} {
		Global $which,cpath ""
		return
	}

	# ../CommonTk-1.3
	if {$verCommon <= 1.3} {
		set path [file dirname [Path$which]]
		Global $which,cpath [file join $path CommonTk-$verCommon]
		return
	}

	# ../.. (OmnibandTk, 1.4)
	set path [file dirname [file dirname [Path$which]]]
	Global $which,cpath $path
	
	return
}

# CmpVer --
#
#	Compare versions. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CmpVer {which versionString} {

	if {![regexp "(\[0-9\]*)r(\[0-9\]*)" $versionString \
		ignore versionNumber releaseNumber]} {
		error "invalid version \"$versionString\""
	}

	set ver [Global $which,versionNumber]
	set rel [Global $which,releaseNumber]

	if {$ver < $versionNumber} {
		return -1
	}
	if {$ver > $versionNumber} {
		return +1
	}

	if {$rel < $releaseNumber} {
		return -1
	}
	if {$rel > $releaseNumber} {
		return +1
	}

	return 0
}

# InitResultWindow --
#
#	Debug window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc InitResultWindow {} {

	set win .results
	if {[winfo exists $win]} return

	toplevel $win
	wm title $win "Result"
	wm geometry $win +4+4

	Global result,win $win
	
	set text $win.text
	text $text \
		-font {Courier 9} -width 80 -height 24 \
		-yscrollcommand "$win.yscroll set"
	scrollbar $win.yscroll \
		-orient vertical -command "$text yview"

	pack $text \
		-side left -expand yes -fill both
	pack $win.yscroll \
		-side left -fill y

	return
}

# LogMessage --
#
#	Dump a message to the result window or log file
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc LogMessage {string} {

	if {$::DRY_RUN || $::BUILD} {
		[Global result,win].text insert end $string\n
	} elseif {[Global logFileId] != ""} {
		puts [Global logFileId] $string
	}

	return
}

# CPathSrc --
#
#	Build a path relative to Src,cpath
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CPathSrc {args} {

	return [file join [Global Src,cpath] {*}$args]
}

# CPathDst --
#
#	Build a path relative to Dst,cpath
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CPathDst {args} {

	return [file join [Global Dst,cpath] {*}$args]
}

# PathSrc --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc PathSrc {args} {

	return [file join [Global dir,Src] {*}$args]
}

# PathDst --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc PathDst {args} {

	return [file join [Global dir,Dst] {*}$args]
}

# Copy all files from the given directory that were not in the old
# release, and are not in the new release.
proc SynchDirectory {pathList {ignoreList {}} {globList *}} {

	LogMessage "==========\nsynch directory [file join {*}$pathList]..."

	Status "Copying files from [file nativename [file join {*}$pathList]]"
	
	# Get list of real files in the old directory
	set wd [pwd]
	cd [PathSrc {*}$pathList]
	set fileList [glob -nocomplain {*}$globList]
	cd $wd

	# Get list of files to ignore in old directory
#	set ignoreList [set NSVersionDataSrc::vFile_[lindex $args end]]

	foreach name $fileList {

		# Ignore this file
		if {[lsearch -exact $ignoreList $name] != -1} continue

		set srcPath [PathSrc {*}$pathList $name]
		set dstPath [PathDst {*}$pathList $name]

		# File exists in dest? Skip it
		if {[file exists $dstPath]} {
			BackupFile $dstPath
		}

		# Okay, copy it
		CopyFile $srcPath $dstPath
	}

	return
}

# SynchFile --
#
#	Copy a file from the source to the destination.
#	Do nothing if the source file does not exist. If the same file exists
#	in the destination back it up if "backup" is true, otherwise do nothing.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc SynchFile {file backup} {

	# Get the old file
	set pathSrc [PathSrc $file]

	LogMessage "==========\nsynch file\n  [SimplifyPath Src $pathSrc]"

	if {![file exists $pathSrc]} {
		LogMessage "  skipping (missing)"
		return
	}

	# Get the new file
	set pathDst [PathDst $file]

	if {[file exists $pathDst]} {
		if {$backup} {
			BackupFile $pathDst
		} else {
			LogMessage "  skipping (exists)"
			return
		}
	}

	Status "Copying [file nativename $file]"

	# Okay, copy it
	CopyFile $pathSrc $pathDst

	return
}

