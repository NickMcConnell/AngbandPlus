# File: bgerror.tcl

# Purpose: bgerror implementation

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# bgerror --
#
#	Overrides Tk's bgerror command. Displays an error message, then
#	appends the stack trace to the errors.txt file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc bgerror {err} {

	global Angband
	global errorInfo
	global ErrorText

	set info $errorInfo

	set parent [focus]
	if {![winfo exists $parent] || ![winfo ismapped $parent]} {
		set parent .
		wm deiconify .
	}

	set message "The following error occurred:\n\n$err\n\n"
	append message "You might be able to continue playing.\n"
	append message "Please send the errors.txt file to the zangband mailing list"
	tk_messageBox -title "Error in ZAngband" \
		-message $message -icon info -parent $parent

	if {[catch {open [PathTk errors.txt] a} fileId]} {
		tk_messageBox -icon error -title Error \
			-message "Couldn't open [PathTk errors.txt]"
		return
	}

	catch {
		puts $fileId "***** ZAngband"
		puts $fileId $ErrorText
		puts $fileId "\n***** Last Stack:\n"
		puts $fileId $info
		puts $fileId ""
	}

	close $fileId

	return
}

# HandleError --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc HandleError {err {prompt ""}} {

	global Angband
	global errorInfo
	global ErrorText

	set stack $errorInfo

	if {[string length $prompt]} {
		set message "$prompt:\n\n$err\n\n"
	} else {
		set message "The following error occurred:\n\n$err\n\n"
	}
	append message "Please send the errors.txt file to zangband mailing list"
	if {$::DEBUG} {
		append message "\nQuit now?"
		set type yesno
	} else {
		set type ok
	}
	set answer [tk_messageBox -title "Error in ZAngband" \
		-message $message -type $type -icon error]

	if {[catch {open [PathTk errors.txt] a} fileId]} {
		if {$::DEBUG} {
			tk_messageBox -icon error -title Error \
				-message "Couldn't open [PathTk errors.txt]\n$fileId"
		}
	} else {
		catch {
			puts $fileId "***** ZAngband"
			puts $fileId $ErrorText
			puts $fileId "\n***** Last Stack:\n"
			puts $fileId $stack
			puts $fileId ""
		}
		close $fileId
	}

	if {[string equal $answer no]} return

	AbortGame

	return
}

