
# !defines

!define APP "QuantLibAddin"
!define VER_NUMBER "0.9.8"
!define DEFAULT_PATH "c:\build_ql_0_9_8\${APP}"

# Compiler Flags

SetCompressor lzma

# Installer Attributes

Caption "${APP} - Setup"
DirText "Please select a location to install ${APP} (or use the default):"
Icon "Docs\images\favicon.ico"
InstallDir "${DEFAULT_PATH}"
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
    File "QuantLibAddin_vc7.sln"
    File "QuantLibAddin_vc8.sln"
    File "QuantLibAddin_vc9.sln"
    File "QuantLibAddinCalc_vc7.sln"

    File "*.txt"
    File "*.TXT"
    File "QuantLibAddin.nsi"
    File /r "*.vcproj"
    File /r "*.hpp"
    File /r "*.cpp"
    File /r "*.h"
    File /r "*.c"

    SetOutPath "$INSTDIR\qlo\serialization\create"

    SetOutPath "$INSTDIR\gensrc"
    File /r "gensrc\Makefile.vc"
    File /r "gensrc\*.py"
    File /r "gensrc\*.xml"

    SetOutPath "$INSTDIR\gensrc\stubs"
    File "gensrc\stubs\stub.*"

    SetOutPath "$INSTDIR\Addins\Calc"
    File "Addins\Calc\Makefile.vc"
    File "Addins\Calc\QuantLibAddinCalc.def"
    #File "Addins\Calc\QuantLibAddinCalc.idl"

    SetOutPath "$INSTDIR\Clients\Calc"
    File /r "Clients\Calc\*.ods"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"
    File "Docs\*.doxy"
    File "Docs\*.html"
    File "Docs\Makefile.vc"
    File "Docs\doxygen.css"
    File "Docs\ql.css"
    File "Docs\tabs.css"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\*.ico"
    File "Docs\images\*.jpg"
    File "Docs\images\*.png"

    SetOutPath "$INSTDIR\Docs\pages"
    File "Docs\pages\*.docs"

    SetOutPath "$INSTDIR\Docs\auto.pages"
    File "Docs\auto.pages\*.docs"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin-${VER_NUMBER}" \
                "DisplayName" "QuantLibAddin ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\QuantLibAddinUninstall.exe"

    CreateDirectory "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\Uninstall QuantLibAddin.lnk" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" "" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin VC 7 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin_vc7.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin VC 8 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin_vc8.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin VC 9 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin_vc9.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin Directory.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\QuantLibAddin Home Page.url" \
                "InternetShortcut" "URL" "http://www.quantlibaddin.org/"

    WriteUninstaller "QuantLibAddinUninstall.exe"

SectionEnd

Section "Uninstall"

    RMDir /r /REBOOTOK "$INSTDIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}"

    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin-${VER_NUMBER}"

SectionEnd

