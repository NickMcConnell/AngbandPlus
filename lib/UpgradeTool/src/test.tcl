# File: test.tcl

# Purpose: testsuite

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTest {

# namespace eval NSTest
}

# NSTest::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTest::InitModule {} {

	return
}

# NSTest::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTest::CloseModule {} {

	return
}

# NSTest:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTest::RunTests {} {

	variable Result

	set Result(pass) 0
	set Result(fail) 0
	set Result(skip) 0

	set tests {}
	lappend tests [Path test AngbandTk-291r1]
	lappend tests [Path test AngbandTk-291r2]
	lappend tests [Path test AngbandTk-292r1]
	lappend tests [Path test OmnibandTk-1.4 variant AngbandTk-292r2]
	RunTestsAux $tests

	set tests {}
	lappend tests [Path test OmnibandTk-1.4 variant ZAngbandTk-240r5]
	lappend tests [Path test OmnibandTk-1.5 variant ZAngbandTk]
	RunTestsAux $tests

	LogMessage "Test: skip $Result(skip), pass $Result(pass), fail $Result(fail)"

	return
}

# NSTest:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTest::RunTestsAux {tests} {

	set max [llength $tests]
	for {set i 1} {$i < $max} {incr i} {
		set test2 [lindex $tests $i]
		for {set j 0} {$j < $i} {incr j} {
			set test1 [lindex $tests $j]
			LogMessage "### Test [file tail $test1] -> [file tail $test2] ###\n"
			Global dir,Src $test1
			Global dir,Dst $test2
			ExtractVersion Src
			ExtractVersion Dst
			Upgrade
			LogMessage ""
			update
		}
	}

	return
}

# NSTest:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTest::TestFile {path buf} {

	variable Result

	set path2 $path.[Global Src,versionStr]
	if {![file exists $path2]} {
		LogMessage "Test: test skipped: [SimplifyPath Dst $path2] not found"
		incr Result(skip)
		return
	}
	set buf2 [FileToString $path2]
	if {[string compare $buf $buf2]} {
		LogMessage "***** Test: test failed:\n  $path2"
		set line 1
		foreach string [split $buf \n] string2 [split $buf2 \n] {
			if {[string compare $string $string2]} {
				LogMessage "  line $line:\n    $string\n    $string2"
			}
			incr line
		}
		incr Result(fail)
		return
	}
	incr Result(pass)

	return
}
