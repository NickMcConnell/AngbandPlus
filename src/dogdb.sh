#!/bin/sh
#
# this helps me debug angband/64 in GNU/Linux.
#
gdb /usr/bin/angband64-beta-7-3 `ps -a | grep [a]ngband64 | cut -c1-6`
