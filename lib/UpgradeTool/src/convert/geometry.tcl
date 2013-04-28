# File: .tcl

# Purpose: converter

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval CvtGeom {

# namespace eval CvtGeom
}

proc CvtGeom::InitModule {} {

	return
}

proc CvtGeom::CloseModule {} {

	return
}

proc CvtGeom::CvtGeom {oop} {

	return
}

proc CvtGeom::Info {oop info args} {

	global CvtGeom

	# Set info
	if {[llength $args]} {
		set CvtGeom($oop,$info) [lindex $args 0]

	# Get info
	} else {
		return $CvtGeom($oop,$info)
	}

	return
}

proc CvtGeom::Read {oop} {

	variable DataSrc
	
	set path [PathSrc tk config geometry]
	Info $oop exists [file exists $path]
	if {![Info $oop exists]} {
		LogMessage "$path\n  skipping (missing)"
		return
	}

	set verSrc $NSVersionDataSrc::vGeometryVersion

	array unset DataSrc $oop,*
	set DataSrc($oop,list,name) {}

	set buf [FileToString [PathSrc tk config geometry]]
	foreach string [split $buf \n] {

		# Skip blank lines
		if {![string length $string]} continue

		# Skip comments
		if {[string equal [string index $string 0] #]} continue

		# Split line into window keyword and geometry
		if {[scan $string "%s %s" window geometry] != 2} continue

		set DataSrc($oop,geometry,$window) $geometry
		lappend DataSrc($oop,list,name) $window
	}

	return
}

proc CvtGeom::Convert {oop} {

	variable DataSrc
	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vGeometryVersion
	set verDst $NSVersionDataDst::vGeometryVersion

	array unset DataDst $oop,*

	if {$verSrc == $verDst} {
		array set DataDst [array get DataSrc $oop,*]
		return
	}

	for {set ver $verSrc} {$ver < $verDst} {incr ver} {
		Convert${ver}To[expr $ver + 1] $oop
	}
	return
}

proc CvtGeom::Update {oop} {

	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vGeometryVersion
	set verDst $NSVersionDataDst::vGeometryVersion

	LogMessage "Geometry: Update version $verSrc to $verDst"

	Update$verDst $oop

	return
}

proc CvtGeom::Update1 {oop} {

	variable DataDst

	set upSrc $NSVersionDataSrc::vGeometryUpdate
	set upDst $NSVersionDataDst::vGeometryUpdate

	# Update 1: forget some geometry
	if {$upSrc < 1} {
		foreach name {book mindcraft power} {
			if {[info exists DataDst($oop,geometry,$name)]} {
				LogMessage "skipping outdated window \"$name\""
				set DataDst($oop,geometry,$name) ""
			}
		}
	}

	return
}

proc CvtGeom::Write {oop} {

	if {![Info $oop exists]} return

	set verDst $NSVersionDataDst::vGeometryVersion

	set buf [Write$verDst $oop]
	StringToFile [PathDst tk config geometry] $buf

	return
}

proc CvtGeom::Write1 {oop} {

	variable DataDst

	set buf ""
	foreach name $DataDst($oop,list,name) {
		append buf [list $name $DataDst($oop,geometry,$name)] \n
	}
	return $buf
}

