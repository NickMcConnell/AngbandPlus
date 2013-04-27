# text-list.tcl --
#
#	Turns a text widget into a simple list.
#	TO DO: KeyPress bindings
#

namespace eval NSTextList {

	variable LineLast ""
	variable LineNew ""
	variable Scan
	set Scan(afterId) ""

# namespace eval NSTextList
}

proc NSTextList::TextList {textBox} {

	global NSTextList

	bindtags $textBox [concat [bindtags $textBox] TextList_BindTag]

	set NSTextList($textBox,selectCmd) ""

	return
}

proc NSTextList::SelectLine {textBox index} {

	global NSTextList

    $textBox tag add sel $index "$index lineend + 1 chars"

	set command $NSTextList($textBox,selectCmd)
	if {[string length $command]} {
		set row [lindex [split [$textBox index $index] .] 0]
		uplevel #0 $command $row
	}

	return
}

proc NSTextList::SelectPoint {textBox x y} {

	variable LineLast
	variable LineNew

    set LineNew [$textBox index "@${x},$y linestart"]
    if {[string compare $LineNew $LineLast]} {
		$textBox tag remove sel 1.0 end
		set LineLast $LineNew
		SelectLine $textBox $LineLast
	}

	return
}

proc NSTextList::AutoScan {w} {

	variable Scan

	if {![winfo exists $w]} return
	if {$Scan(y) >= [winfo height $w]} {
		$w yview scroll 2 units
	} elseif {$Scan(y) < 0} {
		$w yview scroll -2 units
	} elseif {$Scan(x) >= [winfo width $w]} {
		$w xview scroll 2 units
	} elseif {$Scan(x) < 0} {
		$w xview scroll -2 units
	} else {
		return
	}
	SelectPoint $w $Scan(x) $Scan(y)
	set Scan(afterId) [after 50 NSTextList::AutoScan $w]

	return
}

bind TextList_BindTag <ButtonPress-1> {

	focus %W
	set NSTextList::LineLast ""
	NSTextList::SelectPoint %W %x %y
}

bind TextList_BindTag <Button1-Motion> {

    set NSTextList::Scan(x) %x
    set NSTextList::Scan(y) %y
	NSTextList::SelectPoint %W %x %y
}

bind TextList_BindTag <Button1-Leave> {
    set NSTextList::Scan(x) %x
    set NSTextList::Scan(y) %y
    NSTextList::AutoScan %W
}

bind TextList_BindTag <Button1-Enter> {
	after cancel $NSTextList::Scan(afterId)
}

bind TextList_BindTag <ButtonRelease-1> {
	after cancel $NSTextList::Scan(afterId)
}

