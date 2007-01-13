
# to be used with NSIS 2.0 and up

#SetCompressor /SOLID lzma

!define VER_NUMBER "0.4.0"
!define VER_NUMBER_UNDERSCORE "0_4_0"
!define /date NOW "%Y%m%d-%H_%M"

# HEADER CONFIGURATION COMMANDS
Name "QuantLibXL"
Caption "QuantLibXL - Setup"

# for public release - exclude timestamp from filename
#OutFile "..\QuantLibXL-${VER_NUMBER}-${NOW}.exe"
OutFile "..\QuantLibXL-src-${VER_NUMBER}.exe"

ComponentText "This will install QuantLibXL ${VER_NUMBER} on your computer"

SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData "LICENSE.TXT"
DirText "Please select a location to install QuantLibXL (or use the default):"
# QLA relative paths break if version number is included in QLXL install dir
#InstallDir $PROGRAMFILES\QuantLibXL-${VER_NUMBER}
InstallDir $PROGRAMFILES\QuantLibXL
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL" "Install_Dir"
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

#!include "FileFunc.nsh"

InstType "Full (with WinHelp Documentation & Spreadsheets)"
InstType "Typical (with Spreadsheets)"
InstType "Minimal (Addin Only)"


Section "-QuantLibXL"
SectionIn 1 2 3
    # this directory must be created first, or the CreateShortCut will not work
    # exclude version number from directory as otherwise QuantLibAddin breaks
    #CreateDirectory "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"
    CreateDirectory "$SMPROGRAMS\QuantLibXL"
    SetOutPath "$INSTDIR"
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"
    File "QuantLibXL.sln"
    File "QuantLibXL_vc8.sln"

    File "*.txt"
    File "*.TXT"
    File "QuantLibXL*.nsi"
    File /r "*.vcproj"
    File /r "*.vcproj"
    File /r "*.hpp"
    File /r "*.cpp"

    #SetOutPath "$INSTDIR\framework"
    #File "framework\*.xla"

    #SetOutPath "$INSTDIR\xll"
    #File "xll\QuantLibXL-vc*-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
    File "Docs\*.doxy"
    File "Docs\*.html"
    File "Docs\Makefile.vc"
    File "Docs\style.css"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\*.ico"
    File "Docs\images\*.jpg"
    File "Docs\images\*.png"
    File "Docs\images\*.gif"

    SetOutPath "$INSTDIR\Docs\pages"
    File "Docs\pages\*.docs"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "DisplayName" "QuantLibXL ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "UninstallString" '"QuantLibXLUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE \
                "SOFTWARE\QuantLibXL" \
                "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER "Environment" "QUANTLIBXL_DIR" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" "" \
                   "$INSTDIR\QuantLibXLUninstall.exe" 0
    #CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL-${VER_NUMBER}.lnk" \
    #               "$INSTDIR\framework\QuantLibXL.xla"
    #CreateShortCut "$DESKTOP\QuantLibXL-${VER_NUMBER}.lnk" \
    #               "$INSTDIR\framework\QuantLibXL.xla"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\News.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL Directory.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLib Home Page.url" \
                "InternetShortcut" "URL" "http://www.quantlibxl.org/"

    WriteUninstaller "QuantLibXLUninstall.exe"
SectionEnd

Section "WinHelp documentation"
SectionIn 1
  SetOutPath "$INSTDIR\Docs"
  File /nonfatal "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
  IfFileExists "$INSTDIR\Docs\QuantLib-docs-${VER_NUMBER}.chm" 0 NoWinHelpDoc
      CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                     "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
  NoWinHelpDoc:
SectionEnd

Section "Spreadsheets"
SectionIn 1 2
    SetOutPath "$INSTDIR\Workbooks\DateCalendarsDayCounters"
    File "Workbooks\DateCalendarsDayCounters\*.xls"

    SetOutPath "$INSTDIR\Workbooks\Math"
    File "Workbooks\Math\*.xls"

    SetOutPath "$INSTDIR\Workbooks\OriginalExamples"
    File "Workbooks\OriginalExamples\*.xls"

    SetOutPath "$INSTDIR\Workbooks\Utilities"
    File "Workbooks\Utilities\*.xls"


    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Example workbooks.lnk" \
                   "$INSTDIR\Workbooks"

SectionEnd

Function .onInit
  SetOutPath $TEMP
  File /oname=spltmp.bmp "Docs\images\logo_ql.jpg"
  splash::show 2000 $TEMP\spltmp
  Pop $0 ; $0 has '1' if the user closed the splash screen early,
         ;        '0' if everything closed normal,
         ;        '-1' if some error occured.
 Delete $TEMP\spltmp.bmp
FunctionEnd

UninstallText "This will uninstall QuantLibXL. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL"
    DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QUANTLIBXL_DIR"

    Delete "$INSTDIR\xll\QuantLibXL-vc*-mt-s-${VER_NUMBER_UNDERSCORE}.xll"
    RMDir "$INSTDIR\xll"

    Delete "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
    RMDir "$INSTDIR\Docs"

    Delete "$INSTDIR\LICENSE.TXT"
    Delete "$INSTDIR\NEWS.txt"
    Delete "$INSTDIR\README.txt"
    Delete "$INSTDIR\QuantLibXLUninstall.exe"
    RMDir "$INSTDIR"

    RMDir /r "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"

SectionEnd

