; lbinst.nsi, based on example2.nsi
;

;--------------------------------

!define LB_MAJOR_VERSION "0"
!define LB_MINOR_VERSION "1"
!define LB_MICRO_VERSION "6"

!define LB_SHORTVERSION "${LB_MAJOR_VERSION}${LB_MINOR_VERSION}${LB_MICRO_VERSION}"
!define LB_VERSION "${LB_MAJOR_VERSION}.${LB_MINOR_VERSION}.${LB_MICRO_VERSION}"

; The name of the installer
Name "Langband"

; The file to write
OutFile "lb-${LB_SHORTVERSION}.exe"

; The default installation directory
InstallDir "$PROGRAMFILES\Langband-${LB_VERSION}"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM SOFTWARE\Langband "Install_Dir"

; The text to prompt the user to enter a directory
ComponentText "This will install Langband on your computer. Select which optional things you want installed."

; The text to prompt the user to enter a directory
DirText "Choose a directory to install Langband in:"

;LicenseData "WinLicense.txt"

;--------------------------------

; The stuff to install
Section "Langband (required)"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  File "..\*.lisp"
  File "..\*.dll"
  File "..\ChangeLog"
  File "..\README"
  File "..\THANKS"
  File "..\COPYING"
  File "..\langband-engine.asd"

  File "vanilla.bat"
  File "ReadmeWin.txt"
  File "..\docs\web\favicon.ico"

  SetOutPath "$INSTDIR\variants"

  File /r "..\variants\vanilla"

  SetOutPath "$INSTDIR\ffi"
  File "..\ffi\*.lisp"

  SetOutPath "$INSTDIR\config"
  File "..\config\*.lisp"
  File "..\config\*.txt"

  SetOutPath "$INSTDIR\data"
  File /r "..\data\graphics"
  File /r "..\data\fonts"
  ;; skip audio

  SetOutPath "$INSTDIR\docs"
  FILE /r "..\docs\help"
  FILE /r "..\docs\binary-types"
;  FILE /r "..\docs\web"

  SetOutPath "$INSTDIR\zterm"
  File "..\zterm\dircall.dll"
  File "..\zterm\lbui.dll"

  SetOutPath "$INSTDIR\tools"
  File "..\tools\asdf.lisp"

  SetOutPath "$INSTDIR"

;;  Rename "$INSTDIR/README" "$INSTDIR/README.txt"

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Langband "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Langband" "DisplayName" "Langband (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Langband" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\Langband"
  CreateShortCut "$SMPROGRAMS\Langband\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\Langband\Langband.lnk" "$INSTDIR\vanilla.bat" "" "$INSTDIR\favicon.ico" 0
  CreateShortCut "$SMPROGRAMS\Langband\ReadMe.lnk" "$INSTDIR\ReadmeWin.txt" "" "$INSTDIR\ReadmeWin.txt" 0
  CreateShortCut "$SMPROGRAMS\Langband\Langband Homepage.lnk" "http://www.langband.org/"
  CreateShortCut "$SMPROGRAMS\Langband\Langband Bug Reporting.lnk" "http://sourceforge.net/tracker/?atid=106938&group_id=6938"
  CreateShortCut "$SMPROGRAMS\Langband\Download Lispworks Personal.lnk" "http://www.lispworks.com/downloads/lww-per-download.html"
SectionEnd

;--------------------------------

; Uninstaller

UninstallText "This will uninstall Langband. Hit 'uninstall' to continue."

; Uninstall section

Section "Uninstall"
  
  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Langband"
  DeleteRegKey HKLM SOFTWARE\Langband

  RMDir /r "$INSTDIR\*.*"
  RMDir /r "$INSTDIR"

  ; remove shortcuts, if any
  Delete "$SMPROGRAMS\Langband\*.*"
  RMDir "$SMPROGRAMS\Langband"


SectionEnd
