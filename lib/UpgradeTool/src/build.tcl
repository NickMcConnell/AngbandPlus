# File: build.tcl

# Purpose: UpgradeBuilder

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBuild {

# namespace eval NSBuild
}

# NSBuild::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuild::InitModule {} {

	NSModule::LoadIfNeeded NSFile

	return
}

# NSBuild::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuild::CloseModule {} {

	return
}

proc NSBuild::Build {} {

	LoadVersionData Src
	LoadVersionData Dst

	FindCommonDir Src
	FindCommonDir Dst

	LogMessage "# data[Global Dst,versionStr] --"
	LogMessage "#\tContains version-specific variables and commands"
	LogMessage "\nsource data[Global Src,versionStr]\n"

	set verSrc $NSVersionDataSrc::vCommonVersion
	if {$verSrc} {
		scan $verSrc "%d.%d" major minor
		incr minor
		LogMessage "set vCommonVersion $major.$minor\n"
	}

	BuildDirList

	BuildFileList user

	Status "tk/config/config"
	BuildConfig

	Status "tk/config/setting"
	BuildSetting

	Status "tk/config/value"
	BuildValue

	LogMessage "lappend vRInfoCompat \\\n\t[Global Src,versionStr]\n"
	LogMessage "lappend vScoreCompat \\\n\t[Global Src,versionStr]\n"

	return
}

proc NSBuild::BuildDirList {} {

	set pathDst [PathDst]

	set count 0

	# Find source directories which no longer exist
	set dirDel {}
	foreach pathList $NSVersionDataSrc::vDirName {
		set path [file join $pathDst {*}$pathList]
		if {![file exists $path] || ![file isdirectory $path]} {
			lappend dirDel $pathList
		}
	}
	if {[set length [llength $dirDel]]} {
		set index 0
		LogMessage "ldelete vDirName \\"
		foreach name $dirDel {
			if {[incr index] < $length} {
				LogMessage "\t\{$name\} \\"
			} else {
				LogMessage "\t\{$name\}\n"
			}
		}
	}

	# Get vDirName for new version
	set wd [pwd]
	cd $pathDst
	set vDirName {}
	set nameList [glob -nocomplain *]
	while {[llength $nameList]} {
		set nameList2 {}
		foreach name $nameList {
			if {[file isdirectory $name]} {
				lappend vDirName [file split $name]
				foreach name2 [glob -nocomplain $name/*] {
					if {[file isdirectory $name]} {
						lappend nameList2 $name2
					}
				}
			}
		}
		set nameList $nameList2
	}
	cd $wd

	# Dump list of directories added in new version
	set dirAdd {}
	foreach name $vDirName {
		if {[lsearch -exact $NSVersionDataSrc::vDirName $name] == -1} {
			lappend dirAdd $name 
		}
	}
	if {[set length [llength $dirAdd]]} {
		set index 0
		LogMessage "lappend vDirName \\"
		foreach name $dirAdd {
			if {[incr index] < $length} {
				LogMessage "\t\{$name\} \\"
			} else {
				LogMessage "\t\{$name\}\n"
			}
		}
	}

	return
}

proc NSBuild::BuildFileList {dir} {

	# Find source files which no longer exist
	set fileDel {}
	foreach file [set NSVersionDataSrc::vFile_$dir] {
		set path [PathDst lib $dir $file]
		if {![file exists $path]} {
			lappend fileDel $file
		}
	}
	if {[set length [llength $fileDel]]} {
		set index 0
		LogMessage "ldelete vFile_$dir \\"
		foreach name $fileDel {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	# Get vFile for new version
	set wd [pwd]
	cd [PathDst lib $dir]
	set vFile [glob -nocomplain *]
	cd $wd

	# Dump list of files added in new version
	set fileAdd {}
	foreach name $vFile {
		if {[lsearch -exact [set NSVersionDataSrc::vFile_$dir] $name] == -1} {
			lappend fileAdd $name 
		}
	}
	if {[set length [llength $fileAdd]]} {
		set index 0
		LogMessage "lappend vFile_$dir \\"
		foreach name $fileAdd {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	return
}

proc NSBuild::BuildSetting {} {

	# Get the new tk/config/setting file
	set pathDst [PathDst tk config setting]

	# Verify the file exists
	if {![file exists $pathDst]} {
		LogMessage "skipping \"$pathDst\" (missing)"
		return
	}

	NSModule::LoadIfNeeded CvtSetting

	# Step 1: Read new file
	set cvtId [NSObject::New CvtSetting]
	CvtSetting::ReadDst $cvtId

	set settingSrc $NSVersionDataSrc::vSetting
	set settingDst $CvtSetting::DataDst($cvtId,list,name)
	
	set settingDel {}
	foreach name $settingSrc {

		# This setting is not in the new game
		if {[lsearch -exact $settingDst $name] == -1} {
			lappend settingDel $name
		}
	}

	set settingAdd {}
	foreach name $settingDst {

		# This setting is not in the old game
		if {[lsearch -exact $settingSrc $name] == -1} {
			lappend settingAdd $name
		}
	}

	if {[set length [llength $settingDel]]} {
		set index 0
		LogMessage "ldelete vSetting \\"
		foreach name $settingDel {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	if {[set length [llength $settingAdd]]} {
		set index 0
		LogMessage "lappend vSetting \\"
		foreach name $settingAdd {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	# Step : Clean up
	NSObject::Delete CvtSetting $cvtId

###
return
###

	# Open the tk/config/setting file
	if {[catch {open $pathDst} fileId]} {
		LogMessage ">>>>>\n$fileId\n<<<<<"
		return
	}

	# Get a list of old setting keywords
	set settingSrc $NSVersionDataSrc::vSetting

	# Build a list of new setting keywords
	set settingDst {}
	
	if {[catch {

		# Read a line from the new tk/config/setting file
		while {[gets $fileId lineBuf] != -1} {

			# Parse the setting
			set info [NSVersionDataDst::ParseSetting $lineBuf]
			if {![llength $info]} continue
			array set data $info

			# Build a list of new setting keywords
			lappend settingDst $data(name)
		}

	} result]} {
		LogMessage ">>>>>\n$result\n<<<<<" 
	}
	
	close $fileId

	set settingDel {}
	foreach name $settingSrc {

		# This setting is not in the new game
		if {[lsearch -exact $settingDst $name] == -1} {
			lappend settingDel $name
		}
	}

	set settingAdd {}
	foreach name $settingDst {

		# This setting is not in the old game
		if {[lsearch -exact $settingSrc $name] == -1} {
			lappend settingAdd $name
		}
	}

	if {[set length [llength $settingDel]]} {
		set index 0
		LogMessage "ldelete vSetting \\"
		foreach name $settingDel {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	if {[set length [llength $settingAdd]]} {
		set index 0
		LogMessage "lappend vSetting \\"
		foreach name $settingAdd {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	return
}

proc NSBuild::BuildValue {} {

	# Get the new tk/config/value file
	set pathDst [PathDst tk config value]

	if {![file exists $pathDst]} {
		LogMessage "skipping \"$pathDst\" (missing)"
		return
	}

	NSModule::LoadIfNeeded CvtValue

	# Step 1: Read new file
	set cvtId [NSObject::New CvtValue]
	CvtValue::ReadDst $cvtId

	set valueSrc $NSVersionDataSrc::vValue
	set valueDst $CvtValue::DataDst($cvtId,list,name)

	set valueDel {}
	foreach name $valueSrc {

		# This value is not in the new game
		if {[lsearch -exact $valueDst $name] == -1} {
			lappend valueDel $name
		}
	}

	set valueAdd {}
	foreach name $valueDst {

		# This value is not in the old game
		if {[lsearch -exact $valueSrc $name] == -1} {
			lappend valueAdd $name
		}
	}

	if {[set length [llength $valueDel]]} {
		set index 0
		LogMessage "ldelete vValue \\"
		foreach name $valueDel {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	if {[set length [llength $valueAdd]]} {
		set index 0
		LogMessage "lappend vValue \\"
		foreach name $valueAdd {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	# Step : Clean up
	NSObject::Delete CvtValue $cvtId

###
return
###

	if {[catch {open $pathDst} fileId]} {
		LogMessage ">>>>>\n$fileId\n<<<<<"
		return
	}

	# Get a list of old value keywords
	set valueSrc $NSVersionDataSrc::vValue

	# Build a list of new value keywords
	set ValueDst {}

	if {[catch {
	
		while {[gets $fileId lineBuf] != -1} {

			# Scan a line from the new tk/config/value file
			set info [NSVersionDataDst::ParseValue $lineBuf]
			if {![llength $info]} continue
			array set data $info

			# Build a list of new setting keywords
			lappend valueDst $data(name)
		}

	} result]} {
		LogMessage ">>>>>\n$result\n<<<<<" 
	}
	
	close $fileId

	set valueDel {}
	foreach name $valueSrc {

		# This value is not in the new game
		if {[lsearch -exact $valueDst $name] == -1} {
			lappend valueDel $name
		}
	}

	set valueAdd {}
	foreach name $valueDst {

		# This value is not in the old game
		if {[lsearch -exact $valueSrc $name] == -1} {
			lappend valueAdd $name
		}
	}

	if {[set length [llength $valueDel]]} {
		set index 0
		LogMessage "ldelete vValue \\"
		foreach name $valueDel {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	if {[set length [llength $valueAdd]]} {
		set index 0
		LogMessage "lappend vValue \\"
		foreach name $valueAdd {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	return
}

proc NSBuild::BuildConfig {} {

	# Get the new tk/config/config file
	set pathDst [PathDst tk config config]

	if {![file exists $pathDst]} {
		LogMessage "skipping (missing)"
		return
	}

	# Open the new file for reading
	if {[catch {open $pathDst} fileId]} {
		LogMessage ">>>>>\n$fileId\n<<<<<"
		return
	}

	# Get a list of old configurations
	set configSrc $NSVersionDataSrc::vConfig

if 1 {
	NSModule::LoadIfNeeded CvtConfig

	set cvtId [NSObject::New CvtConfig]
	CvtConfig::Read $cvtId Dst

	# Get a list of new configurations
	set configDst $CvtConfig::Data($cvtId,Dst,list,prefix)

	NSObject::Delete CvtConfig $cvtId

} else {

	# Build a list of new configurations
	set configDst {}
	
	if {[catch {
	
		set text ""
		while {[gets $fileId lineBuf] != -1} {
			if {[regexp {^Config: ([^ ]+)} $lineBuf ignore config]} {
				lappend configDst $config
			}
		}
		
	} result]} {
		LogMessage ">>>>>\n$result\n<<<<<"
	}

	close $fileId
}
	set configDel {}
	foreach name $configSrc {

		# This config is not in the new game
		if {$name ni $configDst} {
			lappend configDel $name
		}
	}

	set configAdd {}
	foreach name $configDst {

		# This config is not in the old game
		if {$name ni $configSrc} {
			lappend configAdd $name
		}
	}

	if {[set length [llength $configDel]]} {
		set index 0
		LogMessage "ldelete vConfig \\"
		foreach name $configDel {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	if {[set length [llength $configAdd]]} {
		set index 0
		LogMessage "lappend vConfig \\"
		foreach name $configAdd {
			if {[incr index] < $length} {
				LogMessage "\t$name \\"
			} else {
				LogMessage "\t$name\n"
			}
		}
	}

	return
}

