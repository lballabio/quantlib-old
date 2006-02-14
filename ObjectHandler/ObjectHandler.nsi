
# to be used with NSIS 2.0 and up

!define VER_NUMBER "0.1.3"
SetCompressor lzma
Name "ObjectHandler"
Caption "ObjectHandler - Setup"
OutFile "ObjectHandler-${VER_NUMBER}-installer.exe"
LicenseText "ObjectHandler is released under the following license:"
LicenseData LICENSE.txt
DirText "Please select a location to install ObjectHandler (or use the default):"
InstallDir $PROGRAMFILES\ObjectHandler-${VER_NUMBER}
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"

Section
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}"
    SetOutPath $INSTDIR

    # these MUST be present
    File "README.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "ObjectHandler.dsw"
    File "ObjectHandler.sln"
    File "ObjectHandler_vc8.sln"

    File "*.txt"
    File "*.TXT"
    File "*.nsi"
    File /r "*.dsp"
    File /r "*.vcproj"
    File /r "*.hpp"
    File /r "*.cpp"

    SetOutPath $INSTDIR\xlsdk
    File "xlsdk\xlcall32.lib"
    File "xlsdk\xlcall.h"

    SetOutPath $INSTDIR\Examples\xl
    File "Examples\xl\*.xls"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\ObjectHandler-docs-${VER_NUMBER}.chm"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}" \
                "DisplayName" "ObjectHandler ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}" \
                "UninstallString" '"ObjectHandlerUninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "OBJECT_HANDLER_DIR" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\Uninstall ObjectHandler.lnk" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" "" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\News.txt"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 6 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler.dsw"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 7 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler.sln"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 8 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler_vc8.sln"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\ObjectHandler-docs-${VER_NUMBER}.chm"

    WriteINIStr "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Home Page.url" \
                "InternetShortcut" "URL" "http://quantlib.org/objecthandler"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Directory.lnk" \
                   "$INSTDIR"

    WriteUninstaller "ObjectHandlerUninstall.exe"

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

UninstallText "This will uninstall ObjectHandler. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "OBJECT_HANDLER_DIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS-${VER_NUMBER}\ObjectHandler"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd

