mkdir unangband-062-win
mkdir unangband-062-win\lib
mkdir unangband-062-win\lib\apex
mkdir unangband-062-win\lib\bone
mkdir unangband-062-win\lib\data
mkdir unangband-062-win\lib\edit
mkdir unangband-062-win\lib\file
mkdir unangband-062-win\lib\help
mkdir unangband-062-win\lib\info
mkdir unangband-062-win\lib\pref
mkdir unangband-062-win\lib\save
mkdir unangband-062-win\lib\script
mkdir unangband-062-win\lib\user
mkdir unangband-062-win\lib\xtra
mkdir unangband-062-win\lib\xtra\font
mkdir unangband-062-win\lib\xtra\graf
mkdir unangband-062-win\lib\xtra\music
mkdir unangband-062-win\lib\xtra\sound
mkdir unangband-062-win\lib\xtra\help

copy lib\apex\delete.me unangband-062-win\lib\apex
copy lib\bone\delete.me unangband-062-win\lib\bone
copy lib\data\delete.me unangband-062-win\lib\data
copy lib\save\delete.me unangband-062-win\lib\save
copy lib\user\delete.me unangband-062-win\lib\user
copy lib\xtra\music\delete.me unangband-062-win\lib\xtra\music

copy unangband.exe unangband-062-win
copy readme.txt unangband-062-win
copy changes.txt unangband-062-win
copy faq.txt unangband-062-win
copy bugs.txt unangband-062-win
copy nocompile.txt unangband-062-win\compile.txt

copy lib\edit\*.txt unangband-062-win\lib\edit

copy lib\file\*.txt unangband-062-win\lib\file
copy lib\info\*.txt unangband-062-win\lib\info

copy lib\help\*.txt unangband-062-win\lib\help
copy lib\help\*.hlp unangband-062-win\lib\help

copy lib\pref\*.prf unangband-062-win\lib\pref

copy lib\xtra\font\*.fon unangband-062-win\lib\xtra\font

copy lib\xtra\graf\*.bmp unangband-062-win\lib\xtra\graf

copy lib\xtra\sound\sound.cfg unangband-062-win\lib\xtra\sound
copy lib\xtra\sound\*.wav unangband-062-win\lib\xtra\sound

copy lib\xtra\help\angband.hlp unangband-062-win\lib\xtra\help
copy lib\xtra\help\angband.cnt unangband-062-win\lib\xtra\help

upx -9 unangband-062-win\unangband.exe

7z a -tzip -r unangband-062-win.zip unangband-062-win

rmdir /q /s unangband-062-win

