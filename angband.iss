; Angband for Windows setup script
; for use with Inno Setup < http://www.jrsoftware.org/isinfo.php >
;
; created by Robert Ruehlmann < rr9@thangorodrim.net >

[Setup]
AppName=GilAngband
AppVersion=0.1.0
AppVerName=GilAngband 0.1.0
OutputBaseFilename=GilAngband-010
AppPublisher=Gileba
AppPublisherURL=http://www.thangorodrim.net
AppSupportURL=http://www.thangorodrim.net
AppUpdatesURL=http://www.thangorodrim.net
DefaultDirName={pf}\Angband
DefaultGroupName=Angband
DisableStartupPrompt=yes
UninstallFilesDir={app}\uninst

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"

[Dirs]
Name: "{app}\lib\apex"
Name: "{app}\lib\bone"
Name: "{app}\lib\data"
Name: "{app}\lib\info"
Name: "{app}\lib\save"
Name: "{app}\lib\user"

[Files]
Source: "angband.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "Authors"; DestDir: "{app}"; DestName: "Authors.txt"; Flags: ignoreversion
Source: "changes.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "readme.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "Copying"; DestDir: "{app}"; DestName: "Copying.txt"; Flags: ignoreversion
Source: "Thanks"; DestDir: "{app}"; DestName: "Thanks.txt"; Flags: ignoreversion

Source: "lib\edit\*.txt"; DestDir: "{app}\lib\edit"; Flags: ignoreversion

Source: "lib\file\*.txt"; DestDir: "{app}\lib\file"; Flags: ignoreversion

Source: "lib\help\*.txt"; DestDir: "{app}\lib\help"; Flags: ignoreversion
Source: "lib\help\*.hlp"; DestDir: "{app}\lib\help"; Flags: ignoreversion

Source: "lib\pref\*.prf"; DestDir: "{app}\lib\pref"; Flags: ignoreversion

Source: "lib\xtra\font\*.fon"; DestDir: "{app}\lib\xtra\font"; Flags: ignoreversion

Source: "lib\xtra\graf\8x8.bmp"; DestDir: "{app}\lib\xtra\graf"; Flags: ignoreversion
Source: "lib\xtra\graf\16x16.bmp"; DestDir: "{app}\lib\xtra\graf"; Flags: ignoreversion
Source: "lib\xtra\graf\mask.bmp"; DestDir: "{app}\lib\xtra\graf"; Flags: ignoreversion
Source: "lib\xtra\graf\readme.txt"; DestDir: "{app}\lib\xtra\graf"; Flags: ignoreversion

Source: "lib\xtra\sound\sound.cfg"; DestDir: "{app}\lib\xtra\sound"; Flags: ignoreversion
Source: "lib\xtra\sound\*.wav"; DestDir: "{app}\lib\xtra\sound"; Flags: ignoreversion

Source: "lib\xtra\help\angband.hlp"; DestDir: "{app}\lib\xtra\help"; Flags: ignoreversion
Source: "lib\xtra\help\angband.cnt"; DestDir: "{app}\lib\xtra\help"; Flags: ignoreversion

[Icons]
Name: "{group}\Angband"; Filename: "{app}\gilangband.exe"
Name: "{userdesktop}\Angband"; Filename: "{app}\gilangband.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\gilangband.exe"; Description: "Launch GilAngband"; Flags: nowait postinstall skipifsilent

; remove the generated lib\data\*.raw files and the angband.ini
; leave savefiles, scores, and other user created files alone
[UninstallDelete]
Type: files; Name: "{app}\lib\data\*.raw"
Type: files; Name: "{app}\angband.ini"
