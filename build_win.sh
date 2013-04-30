#!/bin/sh
#
# Simple UNIX packaging script; is a copy of build_win.bat, except written
# in sh.
#
# Invoke like:
#   $ ./build_win.sh unangband-063-win
#

DEST=$1

mkdir $DEST
mkdir $DEST/lib
mkdir $DEST/lib/apex
mkdir $DEST/lib/bone
mkdir $DEST/lib/data
mkdir $DEST/lib/edit
mkdir $DEST/lib/file
mkdir $DEST/lib/docs
mkdir $DEST/lib/help
mkdir $DEST/lib/info
mkdir $DEST/lib/pref
mkdir $DEST/lib/save
mkdir $DEST/lib/script
mkdir $DEST/lib/user
mkdir $DEST/lib/xtra
mkdir $DEST/lib/xtra/font
mkdir $DEST/lib/xtra/graf
mkdir $DEST/lib/xtra/music
mkdir $DEST/lib/xtra/sound
mkdir $DEST/lib/xtra/help

touch $DEST/lib/apex/delete.me $DEST/lib/data/delete.me \
      $DEST/lib/save/delete.me $DEST/lib/user/delete.me \
      $DEST/lib/xtra/music/delete.me

cp unangband.exe $DEST
cp readme.txt $DEST
cp changes.txt $DEST
cp faq.txt $DEST
cp bugs.txt $DEST
cp nocompile.txt $DEST/compile.txt

cp lib/docs/*.rtf $DEST/lib/docs
cp lib/edit/*.txt $DEST/lib/edit

cp lib/file/*.txt $DEST/lib/file
cp lib/info/*.txt $DEST/lib/info

cp lib/help/*.txt $DEST/lib/help
cp lib/help/*.hlp $DEST/lib/help

cp lib/pref/*.prf $DEST/lib/pref

cp lib/xtra/font/*.fon $DEST/lib/xtra/font

cp lib/xtra/graf/*.bmp $DEST/lib/xtra/graf

cp lib/xtra/sound/sound.cfg $DEST/lib/xtra/sound
cp lib/xtra/sound/*.wav $DEST/lib/xtra/sound

cp lib/xtra/help/angband.hlp $DEST/lib/xtra/help
cp lib/xtra/help/angband.cnt $DEST/lib/xtra/help

upx -9 $DEST/unangband.exe

zip -9 -r $DEST.zip $DEST

rm -rf $DEST
