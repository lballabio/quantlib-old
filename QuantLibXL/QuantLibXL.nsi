
# to be used with NSIS 1.93 and up
#
# usage:
#       makensis /DLIGHT QuantLibXL.nsi
# OR
#       makensis QuantLibXL.nsi


# $Id$


!define VER_NUMBER "0.3.0b1-20020321"

# HEADER CONFIGURATION COMMANDS
!ifdef LIGHT
    Name "QuantLib XL Light"
    Caption "QuantLib XL Light - Setup"
    #do not change the name below
    OutFile "..\QuantLibXL-${VER_NUMBER}-light-inst.exe"
    ComponentText "This will install QuantLib XL ${VER_NUMBER} Light on your computer.$\n A more complete version including documentation, examples, source code, etc. can be downloaded from http://quantlib.org"
!else
    Name "QuantLib XL"
    Caption "QuantLib XL - Setup"
    #do not change the name below
    OutFile "..\QuantLibXL-${VER_NUMBER}-full-inst.exe"

    InstType "Full (w/ Source Code)"
    InstType Typical
    InstType Minimal

    ComponentText "This will install QuantLib XL ${VER_NUMBER} on your computer"
!endif

SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData License.txt
DirShow show
DirText "Please select a location to install QuantLib XL (or use the default):"
InstallDir "$PROGRAMFILES\QuantLib XL"
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLib XL" "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

# INSTALLATION EXECUTION COMMANDS



Section "-QuantLib XL"
SectionIn 1 2 3
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\QuantLib XL"
    SetOutPath $INSTDIR
    File "Authors.txt"
    File "Contributors.txt"
    File "History.txt"
    File "LICENSE.txt"
    File "News.txt"
    File "README.txt"
    File "TODO.txt"

    SetOutPath  $INSTDIR\qlxl
    File /r "qlxl\*.hpp"

    SetOutPath $INSTDIR\xll\Win32\VisualStudio
    File "xll\Win32\VisualStudio\qlxl.xll"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLib XL" \
                "DisplayName" "QuantLib XL (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLib XL" \
                "UninstallString" '"QuantLibXLUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE \
                "SOFTWARE\QuantLib XL" \
                "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER \
                "Environment" \
                "QLXL_DIR" "$INSTDIR"
    CreateShortCut "$SMPROGRAMS\QuantLib XL\Uninstall QuantLib XL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" \
                   "" "$INSTDIR\QuantLibXLUninstall.exe" 0
    CreateShortCut "$SMPROGRAMS\QuantLib XL\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\QuantLib XL\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\QuantLib XL\What's new.lnk" \
                   "$INSTDIR\News.txt"

    WriteUninstaller "QuantLibXLUninstall.exe"
SectionEnd



!ifndef LIGHT

#it doesn't work
#Function .onInstSuccess
#  MessageBox MB_YESNO|MB_ICONQUESTION \
#             "Setup has completed. View Readme.txt now?" \
#             IDNO NoReadme
#    ExecShell open '$INSTDIR\Readme.txt'
#  NoReadme:
#FunctionEnd

Section "Source Code"
SectionIn 1
  SetOutPath $INSTDIR
  File ChangeLog.txt
  File makefile.mak
  File QuantLibXL.dsp
  File QuantLibXL.dsw
  File QuantLibXL.mak
  File QuantLibXL.nsi

  SetOutPath  $INSTDIR\qlxl
  File /r "qlxl\*.cpp"
  File /r "qlxl\makefile.mak"

  CreateShortCut "$SMPROGRAMS\QuantLib XL\QuantLib XL project workspace.lnk" \
                 "$INSTDIR\QuantLibXL.dsw"

SectionEnd

Section "Workbooks"
SectionIn 1 2
    SetOutPath $INSTDIR\Workbooks
#    File /r "test\*.txt"
    File /r "test\quantlib.xls"

    CreateShortCut "$SMPROGRAMS\QuantLib XL\Test workbook.lnk" \
                   "$INSTDIR\Workbooks\quantlib.xls"

SectionEnd




SectionDivider

Section "Start Menu Group"
SectionIn 1 2 3
  SetOutPath "$SMPROGRAMS\QuantLib XL"

#it doesn't work
#  CreateShortCut "$SMPROGRAMS\QuantLib XL\QuantLib Home Page.lnk" \
#                 "http://quantlib.org/index.html"
#this works
  WriteINIStr "$SMPROGRAMS\QuantLib XL\QuantLib Home Page.url" \
              "InternetShortcut" "URL" "http://quantlib.org/"

  CreateShortCut "$SMPROGRAMS\QuantLib XL\QuantLib XL Directory.lnk" \
                 "$INSTDIR"
SectionEnd

!endif


Function .onInit

  SetOutPath $TEMP
  File /oname=spltmp.bmp "Docs\images\QL-largish.bmp"
  #the following line depends on NSIS being installed under D:\programs
  #sorry, but no better solution available yet
  IfFileExists "D:\programs\NSIS\splash.exe" 0 NoSplashExecutable
      File /oname=spltmp.exe "D:\programs\NSIS\splash.exe"
      ExecWait '"$TEMP\spltmp.exe" 4000 $HWNDPARENT $TEMP\spltmp'
      Delete $TEMP\spltmp.exe
      Delete $TEMP\spltmp.bmp
  NoSplashExecutable:
FunctionEnd

UninstallText "This will uninstall QuantLib XL. Hit next to continue."


Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLib XL"
    DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLib XL"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QLXL_DIR"
    Delete "$SMPROGRAMS\QuantLib XL\*.*"
    RMDir "$SMPROGRAMS\QuantLib XL"
    RMDir /r "$INSTDIR\Spreadsheets"
    RMDir /r "$INSTDIR\qlxl"
    RMDir /r "$INSTDIR\xll"
    RMDir /r "$INSTDIR"
SectionEnd
