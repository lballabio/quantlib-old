
# !defines

!define APP "log4cxx"
!define VER_NUMBER "0.10.0d"
!define DEFAULT_PATH "C:\build_ql_1_1_0\${APP}"

# Compiler Flags

SetCompressor lzma

# Installer Attributes

Caption "${APP} - Setup"
DirText "Please select a location to install ${APP} (or use the default):"
InstallDir "${DEFAULT_PATH}"
LicenseData "LICENSE"
LicenseText "${APP} is released under the following license:"
Name "${APP}"
OutFile "..\${APP}-${VER_NUMBER}.exe"

# Installer Instructions

Section

    SetOutPath $INSTDIR
    File "INSTALL"
    File "KEYS"
    File "*NOTICE"
    File "LICENSE"
    File "log4cxx.nsi"
    File "*.xml"
    File "*.bat"
    File "*.in"
    File "*.rdf"
    File "*.m4"
    File "*.am"

    SetOutPath $INSTDIR\msvc
    File "msvc\*.vcproj"
    File "msvc\*.vcxproj"
    File "msvc\*.vcxproj.filters"
    File "msvc\*.sln"

    SetOutPath $INSTDIR\src
    File /r "src\*.h"
    File /r "src\*.hpp"
    File /r "src\*.c"
    File /r "src\*.cpp"
    File /r "src\*.am"
    File /r "src\*.xml"
    File /r "src\buildconf"
    File /r "src\*.in"
    File /r "src\*.m4"
    File /r "src\*.in"
    File /r "src\*.hw"
    File /r "src\*.hnw"
    File /r "src\*.html"
    File /r "src\*.win"
    File /r "src\*.dsw"
    File /r "src\*.dsp"
    File /r "src\*.layout"
    File /r "src\*.sh"
    File /r "src\*.pl"
    File /r "src\*.spec"
    File /r "src\NWGNUmakefile"
    File /r "src\LICENSE"
    File /r "src\NOTICE"
    File /r "src\*.conf"
    File /r "src\*.rc"
    File /r "src\*.dev"
    File /r "src\*.MySQL"
    File /r "src\renames_pending"
    File /r "src\*.bin"
    File /r "src\*.xslt"

    SetOutPath $INSTDIR\src\site\apt
    File   "src\site\apt\*.apt"

    SetOutPath $INSTDIR\src\site\apt\building
    File   "src\site\apt\building\*.apt"

    SetOutPath $INSTDIR\src\apr
    File  "src\apr\libaprnw.mcp.zip"
    File  "src\apr\CHANGES"

    SetOutPath $INSTDIR\src\apr\test
    File  "src\apr\test\README"

    SetOutPath $INSTDIR\src\apr\build\pkg
    File  "src\apr\build\pkg\README"

    SetOutPath $INSTDIR\src\apr-util
    File  "src\apr-util\CHANGES"

    SetOutPath $INSTDIR\src\apr-util\build\pkg
    File  "src\apr-util\build\pkg\README"

    SetOutPath $INSTDIR\src\apr-util\xml\expat
    File  "src\apr-util\xml\expat\README"
    File  "src\apr-util\xml\expat\COPYING"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-${VER_NUMBER}" \
                "DisplayName" "log4cxx-${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\log4cxxUninstall.exe"

    CreateDirectory "$SMPROGRAMS\log4cxx-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\Uninstall log4cxx.lnk" \
                   "$INSTDIR\log4cxxUninstall.exe" "" \
                   "$INSTDIR\log4cxxUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\README.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\License.lnk" \
                   "$INSTDIR\LICENSE.txt"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\log4cxx VC 7 project workspace.lnk" \
                   "$INSTDIR\msvc\log4cxx.sln"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\log4cxx VC 8 project workspace.lnk" \
                   "$INSTDIR\msvc\log4cxx_vc8.sln"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\log4cxx VC 9 project workspace.lnk" \
                   "$INSTDIR\msvc\log4cxx_vc9.sln"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\log4cxx VC 10 project workspace.lnk" \
                   "$INSTDIR\msvc\log4cxx_vc10.sln"

    CreateShortCut "$SMPROGRAMS\log4cxx-${VER_NUMBER}\log4cxx Folder.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\log4cxx-${VER_NUMBER}\log4cxx-ObjectHandler Home Page.url" \
                "InternetShortcut" "URL" "http://www.objecthandler.org"

    WriteUninstaller "log4cxxUninstall.exe"

SectionEnd


UninstallText "This will uninstall log4cxx. Hit next to continue."

Section "Uninstall"

    RMDir /r /REBOOTOK "$INSTDIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\log4cxx-${VER_NUMBER}"

    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-${VER_NUMBER}"

SectionEnd
