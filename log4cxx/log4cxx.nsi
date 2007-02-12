
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.9.7c"
!define /date NOW "%Y%m%d-%H_%M"

# HEADER CONFIGURATION COMMANDS
Name "log4cxx"
Caption "log4cxx - Setup"
#do not change the name below
# exclude timestamp from filename for public release
#OutFile "..\log4cxx-oh-${VER_NUMBER}-${NOW}.exe"
OutFile "..\log4cxx-${VER_NUMBER}.exe"

SilentInstall normal
CRCCheck on
LicenseText "log4cxx is released under the following license:"
LicenseData license.apl
DirText "Please select a location to install log4cxx (or use the default):"
InstallDir $PROGRAMFILES\log4cxx-${VER_NUMBER}
InstallDirRegKey HKEY_LOCAL_MACHINE SOFTWARE\log4cxx-${VER_NUMBER} "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

# INSTALLATION EXECUTION COMMANDS



Section "-log4cxx-oh"
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}"
    SetOutPath $INSTDIR

    # these MUST be present
    File "AUTHORS"
    File "COPYING"
    File "INSTALL"
    File "license.apl"
    File "NEWS"
    File "README"

    File "*.nsi"
    File /r "*.sln"
    File /r "*.vcproj"
    File /r "*.h"
    File /r "*.hpp"
    File /r "*.cpp"
    File /r "*.am"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-oh-${VER_NUMBER}" \
                "DisplayName" "log4cxx-oh ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-oh-${VER_NUMBER}" \
                "UninstallString" '"log4cxx-oh_Uninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "LOG4CXX_DIR" "$INSTDIR"

    WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\log4cxx-oh-${VER_NUMBER}" \
                "Install_Dir" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\Uninstall log4cxx-oh.lnk" \
                   "$INSTDIR\log4cxx-oh_Uninstall.exe" "" \
                   "$INSTDIR\log4cxx-oh_Uninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\AUTHORS.lnk" \
                   "$INSTDIR\AUTHORS"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\COPYING.lnk" \
                   "$INSTDIR\COPYING"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\INSTALL.lnk" \
                   "$INSTDIR\INSTALL"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\NEWS.lnk" \
                   "$INSTDIR\NEWS"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\README.lnk" \
                   "$INSTDIR\README"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\License.lnk" \
                   "$INSTDIR\license.apl"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\log4cxx-oh VC 7 project workspace.lnk" \
                   "$INSTDIR\msvc\log4cxx.sln"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\log4cxx-oh VC 8 project workspace.lnk" \
                   "$INSTDIR\msvc\log4cxx_vc8.sln"

    CreateShortCut "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\log4cxx-oh Folder.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}\log4cxx-ObjectHandler Home Page.url" \
                "InternetShortcut" "URL" "http://sourceforge.net/projects/objecthandler"

    WriteUninstaller "log4cxx-oh_Uninstall.exe"

SectionEnd


UninstallText "This will uninstall log4cxx-oh. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\log4cxx-oh-${VER_NUMBER}"
    DeleteRegKey HKEY_LOCAL_MACHINE SOFTWARE\log4cxx-oh-${VER_NUMBER}
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "LOG4CXX_DIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\log4cxx-oh-${VER_NUMBER}"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd
