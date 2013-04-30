mkdir unangband-063-src
mkdir unangband-063-src\lib
mkdir unangband-063-src\lib\apex
mkdir unangband-063-src\lib\bone
mkdir unangband-063-src\lib\data
mkdir unangband-063-src\lib\docs
mkdir unangband-063-src\lib\edit
mkdir unangband-063-src\lib\file
mkdir unangband-063-src\lib\help
mkdir unangband-063-src\lib\info
mkdir unangband-063-src\lib\pref
mkdir unangband-063-src\lib\save
mkdir unangband-063-src\lib\todo
mkdir unangband-063-src\lib\user
mkdir unangband-063-src\lib\xtra
mkdir unangband-063-src\lib\xtra\font
mkdir unangband-063-src\lib\xtra\graf
mkdir unangband-063-src\lib\xtra\music
mkdir unangband-063-src\lib\xtra\sound
mkdir unangband-063-src\lib\xtra\help
mkdir unangband-063-src\src
mkdir unangband-063-src\osx
mkdir unangband-063-src\osx\English.lproj
mkdir unangband-063-src\osx\English.lproj\main.nib

copy lib\apex\delete.me unangband-063-src\lib\apex
copy lib\bone\delete.me unangband-063-src\lib\bone
copy lib\data\delete.me unangband-063-src\lib\data
copy lib\save\delete.me unangband-063-src\lib\save
copy lib\user\delete.me unangband-063-src\lib\user
copy lib\xtra\font\delete.me unangband-063-src\lib\xtra\font
copy lib\xtra\graf\delete.me unangband-063-src\lib\xtra\graf
copy lib\xtra\music\delete.me unangband-063-src\lib\xtra\music

copy *.txt unangband-063-src
copy *.bat unangband-063-src
copy config*.* unangband-063-src
copy config*.* unangband-063-src

copy Makefile* unangband-063-src
copy *.m4 unangband-063-src

copy lib\Makefile* unangband-063-src\lib
copy lib\apex\Makefile* unangband-063-src\lib\apex
copy lib\bone\Makefile* unangband-063-src\lib\bone
copy lib\data\Makefile* unangband-063-src\lib\data
copy lib\docs\Makefile* unangband-063-src\lib\docs
copy lib\edit\Makefile* unangband-063-src\lib\edit
copy lib\file\Makefile* unangband-063-src\lib\file
copy lib\help\Makefile* unangband-063-src\lib\help
copy lib\info\Makefile* unangband-063-src\lib\info
copy lib\pref\Makefile* unangband-063-src\lib\pref
copy lib\save\Makefile* unangband-063-src\lib\save
copy lib\todo\Makefile* unangband-063-src\lib\todo
copy lib\user\Makefile* unangband-063-src\lib\user
copy lib\xtra\Makefile* unangband-063-src\lib\xtra
copy lib\xtra\font\Makefile* unangband-063-src\lib\xtra\font
copy lib\xtra\graf\Makefile* unangband-063-src\lib\xtra\graf
copy lib\xtra\music\Makefile* unangband-063-src\lib\xtra\music
copy lib\xtra\sound\Makefile* unangband-063-src\lib\xtra\sound
copy lib\xtra\help\Makefile* unangband-063-src\lib\xtra\help
copy src\Makefile* unangband-063-src\src

copy lib\docs\*.rtf unangband-063-src\lib\docs
copy lib\docs\*.txt unangband-063-src\lib\docs
copy lib\edit\*.txt unangband-063-src\lib\edit
copy lib\file\*.txt unangband-063-src\lib\file
copy lib\info\*.txt unangband-063-src\lib\info
copy lib\help\*.txt unangband-063-src\lib\help
copy lib\help\*.hlp unangband-063-src\lib\help
copy lib\pref\*.prf unangband-063-src\lib\pref
copy lib\todo\*.txt unangband-063-src\lib\todo

copy lib\xtra\font\*.fon unangband-063-src\lib\xtra\font
copy lib\xtra\graf\*.bmp unangband-063-src\lib\xtra\graf
copy lib\xtra\graf\*.png unangband-063-src\lib\xtra\graf
copy lib\xtra\sound\*.wav unangband-063-src\lib\xtra\sound

copy lib\xtra\sound\sound.cfg unangband-063-src\lib\xtra\sound
copy lib\xtra\help\angband.hlp unangband-063-src\lib\xtra\help
copy lib\xtra\help\angband.cnt unangband-063-src\lib\xtra\help

copy src\*.h unangband-063-src\src
copy src\*.c unangband-063-src\src
copy src\*.inc unangband-063-src\src
copy src\*.rc unangband-063-src\src
copy src\*.ico unangband-063-src\src
copy src\Makefile.* unangband-063-src\src

copy src\osx\*.icns unangband-063-src\src\osx
copy src\osx\*.xml unangband-063-src\src\osx
copy src\osx\*.h unangband-063-src\src\osx
copy src\osx\English.lproj\*.strings unangband-063-src\src\osx\English.lproj
copy src\osx\English.lproj\main.nib\*.?ib unangband-063-src\src\osx\English.lproj\main.nib

7z a -tzip -r unangband-063-src.zip unangband-063-src

rmdir /q /s unangband-063-src

