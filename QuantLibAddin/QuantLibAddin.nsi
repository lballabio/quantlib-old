
# to be used with NSIS 2.0 and up

!define VER_NUMBER "0.3.12"
SetCompressor lzma
Name "QuantLibAddin"
Caption "QuantLibAddin - Setup"
OutFile "QuantLibAddin-${VER_NUMBER}-src.exe"
LicenseText "You must agree with the following license before installing:"
LicenseData LICENSE.TXT
DirText "Please select a location to install QuantLibAddin (or use the default):"
InstallDir $PROGRAMFILES\QuantLibAddin-${VER_NUMBER}
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"

Section
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}"
    SetOutPath $INSTDIR

    # these MUST be present
    File "README.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "QuantLibAddin.dsw"
    File "QuantLibAddin.sln"
    File "QuantLibAddin_vc8.sln"

    File "*.txt"
    File "*.TXT"
    File "QuantLibAddin.nsi"
    File /r "*.dsp"
    File /r "*.vcproj"
    File /r "*.vcproj"
    File /r "*.hpp"
    File /r "*.cpp"
    File /r "*.h"
    File /r "*.c"

    SetOutPath $INSTDIR\srcgen
    File /r "srcgen\Makefile.vc"
    File /r "srcgen\*.py"
    File /r "srcgen\*.xml"
    File /r "srcgen\stub.*"

    SetOutPath $INSTDIR\Addins\Calc
    File "Addins\Calc\Makefile.vc.debug.mtdll"
    File "Addins\Calc\QuantLibAddin.def"
    File "Addins\Calc\QuantLibAddin.idl"
    File "Addins\Calc\readme.txt"

    SetOutPath $INSTDIR\Clients\Calc
    File /r "Clients\Calc\*.sxc"

    SetOutPath $INSTDIR\Clients\Excel
    File /r "Clients\Excel\*.xls"

    SetOutPath $INSTDIR\Clients\Guile
    File /r "Clients\Guile\*.scm"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin-${VER_NUMBER}" \
                "DisplayName" "QuantLibAddin ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin-${VER_NUMBER}" \
                "UninstallString" '"QuantLibAddinUninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "QUANTLIBADDIN_DIR" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\Uninstall QuantLibAddin.lnk" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" "" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\News.txt"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin VC 6 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin.dsw"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin VC 7 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin VC 8 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin_vc8.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"

    WriteINIStr "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin Home Page.url" \
                "InternetShortcut" "URL" "http://quantlib.org/quantlibaddin"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin Directory.lnk" \
                   "$INSTDIR"

    WriteUninstaller "QuantLibAddinUninstall.exe"

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

UninstallText "This will uninstall QuantLibAddin. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin-${VER_NUMBER}"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QUANTLIBADDIN_DIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd

