# File: value.tcl

# Purpose: converter for VARIANT/tk/config/value

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval CvtValue {

# namespace eval CvtValue
}

proc CvtValue::InitModule {} {

	return
}

proc CvtValue::CloseModule {} {

	return
}

proc CvtValue::CvtValue {oop} {

	variable OOP

	set OOP $oop

	return
}

proc CvtValue::Info {oop info args} {

	global CvtValue

	# Set info
	if {[llength $args]} {
		set CvtValue($oop,$info) [lindex $args 0]

	# Get info
	} else {
		return $CvtValue($oop,$info)
	}

	return
}

proc CvtValue::Read {oop} {

	variable DataSrc
	
	set path [PathSrc tk config value]
	Info $oop exists [file exists $path]
	if {![Info $oop exists]} {
		LogMessage "$path\n  skipping (missing)"
		return
	}

	set verSrc $NSVersionDataSrc::vValueVersion

	array unset DataSrc $oop,*

	namespace eval Version$verSrc "
		_Init $oop
		source {[PathSrc tk config value]}
	"

	return
}

proc CvtValue::ReadDst {oop} {

	variable DataSrc
	variable DataDst
	
# FIXME: Check for existence

	set verDst $NSVersionDataDst::vValueVersion

	array unset DataSrc $oop,*

	namespace eval Version$verDst "
		_Init $oop
		source {[PathDst tk config value]}
	"

	array set DataDst [array get DataSrc $oop,*]

	return
}

proc CvtValue::Convert {oop} {

	variable DataSrc
	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vValueVersion
	set verDst $NSVersionDataDst::vValueVersion

	array unset DataDst $oop,*

	if {$verSrc == $verDst} {
		array set DataDst [array get DataSrc $oop,*]
		return
	}

	for {set ver $verSrc} {$ver < $verDst} {incr ver} {
		Convert${ver}To[expr {$ver + 1}] $oop
	}

	return
}

proc CvtValue::Update {oop} {

	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vValueVersion
	set verDst $NSVersionDataDst::vValueVersion

	set upSrc $NSVersionDataSrc::vValueUpdate
	set upDst $NSVersionDataDst::vValueUpdate

	LogMessage "Value: Update version $verSrc.$upSrc to $verDst.$upDst"

	# Get a list of value keywords in the dest version
	set valueDst $NSVersionDataDst::vValue

	# Remove obsolete values
	set names {}
	foreach name $DataDst($oop,list,name) {
		if {[lsearch -exact $valueDst $name] == -1} {
			array unset DataDst $oop,value,$name
			LogMessage "skipping outdated value \"$name\""
			continue
		}
		lappend names $name
	}
	set DataDst($oop,list,name) $names

	Update$verDst $oop

	for {set up $upSrc} {$up < $upDst} {incr up} {
		UpdateMinor${up}To[expr {$up + 1}] $oop
	}

	return
}

# History of the sound directory
if 0 {
1: Src/lib/xtra/sound
2: Src/lib/xtra/sound
3: Src/lib/xtra/sound
4: CSrc/sound
5: CSrc/sound
6: CSrc/lib/sound
}

proc CvtValue::Update1 {oop} {

	variable DataDst

	set upSrc $NSVersionDataSrc::vValueUpdate
	set upDst $NSVersionDataDst::vValueUpdate

	if {$upSrc != $upDst} return
	if {$upDst >= 6} return

	set name sound,directory
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			if {$upDst >= 1 && $upDst <= 3} {
				if {[IsFileInX $path [PathSrc lib xtra sound] 0]} {
					set path [PathDst lib xtra sound]
				}
			}
			if {$upDst >= 4 && $upDst <= 5} {
				if {[IsFileInX $path [CPathSrc sound] 0]} {
					set path [CPathDst sound]
				}
			}
			set DataDst($oop,value,$name) [file split $path]
		}
	}

	set name music,directory
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			if {[IsFileInPathSrc $path]} {
				set path [UpdatePath $path]
			} elseif {[IsFileInCPathSrc $path]} {
				set path [UpdateCPath $path]
			}
			set DataDst($oop,value,$name) [file split $path]
		}
	}

	set name savefile
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			set path [UpdatePath $path]
			set DataDst($oop,value,$name) [file split $path]
		}
	}

###
return
###

	LogMessage "Value: Update1 update $upSrc to $upDst"

	# Update 6: The rest of this path-mangling is obsolete
	if {$upSrc >= 6} return

	# Update 1: sound,directory added (default is lib/xtra/sound)
	# 1,2 -> 2,3
	if {$upSrc >= 1 && $upDst < 4} {
		set name sound,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			set value [UpdatePath [eval file join $value]]
			set DataDst($oop,value,$name) [file split $value]
		}
	}

	# Update 2: music,directory added (default is "")
	if {$upSrc >= 2} {
		set name music,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[llength $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 3: savefile added
	if {$upSrc >= 3} {
		set name savefile
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[llength $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 4: sound,directory was lib/xtra/sound, is CommonTk/sound
	# 1,2,3 -> 4,5
	if {$upSrc < 4 && $upDst > 3 && $upDst < 6} {
		set name sound,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[llength $value]} {
				set value [UpdatePathHack [eval file join $value] sound]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 5: sound,directory CommonTk/sound -> CommonTk/sound
	# Update 6: sound,directory removed
	# 4,5 -> 5
	if {$upSrc >= 4 && $upSrc <= 5 && $upDst < 6} {
		set name sound,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[llength $value]} {
				set value [UpdateCPath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	return
}

proc CvtValue::UpdateMinor1To2 {oop} {

	variable DataDst

	set upDst $NSVersionDataDst::vValueUpdate

	# Update 1: sound,directory added (default is lib/xtra/sound)
	set name sound,directory
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			if {[IsFileInX $path [PathSrc lib xtra sound] 0]} {
				set value [UpdatePath $path]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	return
}

proc CvtValue::UpdateMinor2To3 {oop} {

	variable DataDst

	set upDst $NSVersionDataDst::vValueUpdate

	# Update 1: sound,directory added (default is lib/xtra/sound)
	set name sound,directory
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			if {[IsFileInX $path [PathSrc lib xtra sound] 0]} {
				set value [UpdatePath $path]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 2: music,directory added (default is "")
	if {$upDst == 3} {
		set name music,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[string length $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	return
}

proc CvtValue::UpdateMinor3To4 {oop} {

	variable DataDst

	set upDst $NSVersionDataDst::vValueUpdate

	# Update 2: music,directory added (default is "")
	if {$upDst == 4} {
		set name music,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[string length $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 3: savefile added
	if {$upDst == 4} {
		set name savefile
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[string length $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 4: sound,directory was lib/xtra/sound, is CommonTk/sound
	set name sound,directory
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			if {[IsFileInX $path [PathSrc lib xtra sound] 0]} {
				set value [CPathDst sound]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 4: color values were palette, are #RGB
	foreach name $DataDst($oop,list,name) {
		switch -glob -- $name {
			friend*2 -
			health*2 -
			status*2 -
			target*2 {
				# Opacity is unchanged
			}
			friend* -
			health* -
			list* -
			status* -
			target* -
			TERM_* -
			TV_* {
				set value $DataDst($oop,value,$name)
				set DataDst($oop,value,$name) [PaletteIndexToRGB $value]
				LogMessage "converted value\n  $value\n  $DataDst($oop,value,$name)"
			}
		}
	}

	return
}

proc CvtValue::UpdateMinor4To5 {oop} {

	variable DataDst

	set upDst $NSVersionDataDst::vValueUpdate

	# Update 2: music,directory added (default is "")
	if {$upDst == 5} {
		set name music,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[string length $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 3: savefile added
	if {$upDst == 5} {
		set name savefile
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[string length $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 5: sound,directory CommonTk/sound -> CommonTk/sound
	set name sound,directory
	if {[info exists DataDst($oop,value,$name)]} {
		set value $DataDst($oop,value,$name)
		if {[string length $value]} {
			set path [eval file join $value]
			if {[IsFileInX $path [CPathSrc sound] 0]} {
				set value [UpdateCPath $path]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	return
}

proc CvtValue::UpdateMinor5To6 {oop} {

	variable DataDst

	set upDst $NSVersionDataDst::vValueUpdate

	# Update 2: music,directory added (default is "")
	if {$upDst == 6} {
		set name music,directory
		if {[info exists DataDst($oop,value,$name)]} {
			set value $DataDst($oop,value,$name)
			if {[string length $value]} {
				set value [UpdatePath [eval file join $value]]
				set DataDst($oop,value,$name) [file split $value]
			}
		}
	}

	# Update 6: Convert {C:/ AngbandTk lib save SaveFile} to {Path lib save SaveFile}
	if {[info exists DataDst($oop,value,savefile)]} {
		set value $DataDst($oop,value,savefile)
		if {[string length $value]} {
			set path [eval file join $value]
			if {[IsFileInPathSrc $path]} {
				set list [StripCommon $path [PathSrc]]
				set DataDst($oop,value,savefile) [concat Path $list]
				LogMessage "updated path\n  [SimplifyPath Src $path]\n  $DataDst($oop,value,savefile)"
			} elseif {[IsFileInCPathSrc $path]} {
				set list [StripCommon $path [CPathSrc]]
				set DataDst($oop,value,savefile) [concat CPath $list]
				LogMessage "updated path\n  [SimplifyPath Src $path]\n  $DataDst($oop,value,savefile)"
			}
		}
	}

	# Update 6: set defaults for some new values
	if {$upDst == 6} {
		set nameList [list message,float misc,float misc,layout]
		set valueList [list 1 1 wide]
		foreach name $nameList value $valueList {
			lappend DataDst($oop,list,name) $name
			set DataDst($oop,value,$name) $value
			LogMessage "set default value: $name=$value"
		}
	}
	
	return
}

proc CvtValue::Write {oop} {

	if {![Info $oop exists]} return

	set verDst $NSVersionDataDst::vValueVersion

	set buf [Write$verDst $oop]
	StringToFile [PathDst tk config value] $buf

	return
}

proc CvtValue::Write1 {oop} {

	variable DataDst

	set upDst $NSVersionDataDst::vValueUpdate

	set buf ""
	foreach name $DataDst($oop,list,name) {
		append buf [list Manage $name $DataDst($oop,value,$name)] \n
	}

	return $buf
}

proc CvtValue::GetValueSrc {name} {

	variable DataSrc
	variable OOP

	if {[info exists DataSrc($OOP,value,$name)]} {
		return $DataSrc($OOP,value,$name)
	}
	return ""
}

#
# Version 1
#
namespace eval CvtValue::Version1 {
proc _Init {oop} {
	variable ::CvtValue::DataSrc
	set DataSrc(oop) $oop
	set DataSrc($oop,list,name) {}
	return
}
proc Manage {name value} {
	variable ::CvtValue::DataSrc
	set oop $DataSrc(oop)
	set DataSrc($oop,value,$name) $value
	lappend DataSrc($oop,list,name) $name
	return
}
# CvtValue::Version1
}
