**Prerequisites**

* Make sure to install the `gdb` package
* Make sure to compile with `-g -gdb3` and `O0`


**Starting**

Load the binary with
`gdb ./hellband`

**Setting breakpoints**

`break filename:line`

**Querying variables**

* `info variables` to list "All global and static variable names".
* `info locals` to list "Local variables of current stack frame" (names and values)
* `info args` to list "Arguments of the current stack frame" (names and values).
* `p <variable>` to show a variable `variable`
* `explore <variable>` to inspect a variable `variable`

**Call stack**

* `bt` to show all frames aka the the call stack
*  `select-frame <n>` to select frame number `n` from the call stack

**Program flow**

* `continue` to run the program till the end or the next breakpoint
* `step` to execute the whole line at once
* `next` to execute the next atomic part of the line
