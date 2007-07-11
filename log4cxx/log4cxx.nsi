

# !defines

!define APP "log4cxx"
!define VER_NUMBER "0.9.7d"
!define DEFAULT_PATH "c:\build_ql_0_8_0\${APP}"

# Compiler Flags

SetCompressor lzma

# Installer Attributes

Caption "${APP} - Setup"
DirText "Please select a location to install ${APP} (or use the default):"
InstallDir "${DEFAULT_PATH}"
LicenseData "LICENSE.txt"
LicenseText "${APP} is released under the following license:"
Name "${APP}"
OutFile "..\${APP}-${VER_NUMBER}.exe"

# Installer Instructions

Section

    SetOutPath $INSTDIR

    File "AUTHORS"
    File "COPYING"
    File "LICENSE.txt"
    File "NEWS"
    File "README.txt"

    File "*.nsi"
    File /r "*.sln"
    File /r "*.vcproj"
    File /r "*.h"
    File /r "*.hpp"
    File /r "*.cpp"
    File /r "*.am"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-${VER_NUMBER}" \
                "DisplayName" "log4cxx-${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\log4cxxUninstall.exe"

    WriteRegStr HKEY_CURRENT_USER "Environment" "LOG4CXX_DIR" "$INSTDIR"

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
    DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\log4cxx-${VER_NUMBER}"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "LOG4CXX_DIR"

SectionEnd
