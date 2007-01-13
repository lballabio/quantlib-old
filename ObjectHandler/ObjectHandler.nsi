
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.2.0"
!define /date NOW "%Y%m%d-%H_%M"

# HEADER CONFIGURATION COMMANDS
Name "ObjectHandler"
Caption "ObjectHandler - Setup"
#do not change the name below
# suppress timestamp from filename for public release
#OutFile "..\ObjectHandler-${VER_NUMBER}-${NOW}.exe"
OutFile "..\ObjectHandler-${VER_NUMBER}.exe"

InstType "Full (w/ WinHelp Documentation)"
InstType Minimal

ComponentText "This will install ObjectHandler ${VER_NUMBER} on your computer"

SilentInstall normal
Icon Docs\images\favicon.ico
UninstallIcon Docs\images\favicon.ico
CRCCheck on
LicenseText "ObjectHandler is released under the following license:"
LicenseData LICENSE.txt
DirText "Please select a location to install ObjectHandler (or use the default):"
InstallDir $PROGRAMFILES\ObjectHandler-${VER_NUMBER}
InstallDirRegKey HKEY_LOCAL_MACHINE SOFTWARE\ObjectHandler-${VER_NUMBER} "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

# INSTALLATION EXECUTION COMMANDS

Section "-ObjectHandler"
SectionIn 1 2
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}"
    SetOutPath $INSTDIR

    # these MUST be present
    File "README.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "ObjectHandler.sln"
    File "ObjectHandler_vc8.sln"

    File "*.txt"
    File "*.TXT"
    File "*.nsi"
    File /r "*.vcproj"
    File /r "*.hpp"
    File /r "*.cpp"

    SetOutPath $INSTDIR\xlsdk
    File "xlsdk\xlcall32.lib"
    File "xlsdk\xlcall.h"

    SetOutPath $INSTDIR\Examples\xl
    File "Examples\xl\*.xls"

    SetOutPath "$INSTDIR\gensrc"
    File "gensrc\Makefile.vc"
    File "gensrc\gensrc.vcproj"
    File "gensrc\gensrc_vc8.vcproj"
    File /r "gensrc\*.xml"
    File /r "gensrc\*.py"
    File /r "gensrc\stub.*"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\ObjectHandler-docs-${VER_NUMBER}.chm"
    File "Docs\objecthandler.doxy"
    File "Docs\oh_footer.html"
    File "Docs\oh_header.html"
    File "Docs\oh_headeronline.html"
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
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}" \
                "DisplayName" "ObjectHandler ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}" \
                "UninstallString" '"ObjectHandlerUninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "OBJECT_HANDLER_DIR" "$INSTDIR"

    WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\ObjectHandler-${VER_NUMBER}" \
                "Install_Dir" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\Uninstall ObjectHandler.lnk" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" "" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\News.txt"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 7 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler.sln"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 8 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler_vc8.sln"

    WriteINIStr "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Home Page.url" \
                "InternetShortcut" "URL" "http://www.objecthandler.org"

    WriteUninstaller "ObjectHandlerUninstall.exe"

SectionEnd

Section "WinHelp documentation"
SectionIn 1
  SetOutPath "$INSTDIR\Docs"
  File /nonfatal "Docs\ObjectHandler-docs-${VER_NUMBER}.chm"
  IfFileExists "$INSTDIR\Docs\ObjectHandler-docs-${VER_NUMBER}.chm" 0 NoWinHelpDoc
    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\ObjectHandler-docs-${VER_NUMBER}.chm"
  NoWinHelpDoc:
SectionEnd

Section "Start Menu Group"
SectionIn 1 2
  SetOutPath $SMPROGRAMS\ObjectHandler-${VER_NUMBER}

  WriteINIStr "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Home Page.url" \
              "InternetShortcut" "URL" "http://www.objecthandler.org"

  CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Folder.lnk" \
                 "$INSTDIR"
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
    DeleteRegKey HKEY_LOCAL_MACHINE SOFTWARE\ObjectHandler-${VER_NUMBER}
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "OBJECT_HANDLER_DIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd

