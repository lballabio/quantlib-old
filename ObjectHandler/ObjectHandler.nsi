
# !defines

!define APP "ObjectHandler"
!define VER_NUMBER "0.9.0"
!define DEFAULT_PATH "C:\build_ql_0_9_0"

# Compiler Flags

SetCompressor lzma

# Installer Attributes

Caption "${APP} - Setup"
DirText "Please select a location to install ${APP} (or use the default):"
Icon "Docs\images\favicon.ico"
InstallDir "${DEFAULT_PATH}\${APP}"
LicenseData "LICENSE.TXT"
LicenseText "${APP} is released under the following license:"
Name "${APP}"
OutFile "..\${APP}-${VER_NUMBER}.exe"
UninstallIcon "Docs\images\favicon.ico"
UninstallText "This will uninstall ${APP}. Hit next to continue."

# Installer Instructions

Section
    SetOutPath $INSTDIR

    File "README.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "ObjectHandler_vc7.sln"
    File "ObjectHandler_vc8.sln"

    File "*.txt"
    File "*.TXT"
    File "ObjectHandler.nsi"
    File /r "*.vcproj"
    File /r "*.hpp"
    File /r "*.cpp"

    SetOutPath "$INSTDIR\xlsdk"
    File "xlsdk\xlcall32.lib"
    File "xlsdk\xlcall.h"

    SetOutPath "$INSTDIR\Examples\xl"
    File "Examples\xl\*.xls"

    SetOutPath "$INSTDIR\gensrc"
    File "gensrc\Makefile.vc"
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
    File "Docs\doxygen.css"
    File "Docs\ql.css"
    File "Docs\tabs.css"

    SetOutPath "$INSTDIR\dev_tools"
    File "dev_tools\preprocess_doxyfile.py"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\*.ico"
    File "Docs\images\*.jpg"
    File "Docs\images\*.png"
    File "Docs\images\*.php"

    SetOutPath "$INSTDIR\Docs\pages"
    File "Docs\pages\*.docs"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}" \
                "DisplayName" "ObjectHandler ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\ObjectHandlerUninstall.exe"

    CreateDirectory "$SMPROGRAMS\${APP}-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\ObjectHandler-docs-${VER_NUMBER}.chm"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\Uninstall ObjectHandler.lnk" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" "" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 7 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler_vc7.sln"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler VC 8 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler_vc8.sln"

    CreateShortCut "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Folder.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}\ObjectHandler Home Page.url" \
                "InternetShortcut" "URL" "http://www.objecthandler.org"

    WriteUninstaller "ObjectHandlerUninstall.exe"

SectionEnd

Section "Uninstall"

    RMDir /r /REBOOTOK "$INSTDIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\ObjectHandler-${VER_NUMBER}"

    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler-${VER_NUMBER}"

SectionEnd

