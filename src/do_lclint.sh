#!/bin/sh
rm -f make.err
#
# -booltype is to tell lclint what booleans should be like in Angband/64
# -warn_posix_headers stops warning about including headers with POSIX names
#                     in non-POSIX source
# -duplicatequals stops warning about long long, which is acceptable in gcc
echo "WARNING: this can drive you completely insane, besides making you doubt everything...."
echo "press any key to start insanity."
read k
/opt/lclint-2.4b/bin/lclint -booltype bool_angband_hack -warn_posix_headers -duplicatequals -mustfree $@ 2>&1 > lclint.err
echo "lclint.err about file "$@" created."
