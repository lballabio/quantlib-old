
# !defines

!define APP "gensrc"
!define VER_NUMBER "0.9.6"
!define DEFAULT_PATH "C:\build_ql_0_9_6"

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
    File "LICENSE.txt"
    File "NEWS.txt"
    File "Authors.txt"
    File "gensrc_vc7.sln"
    File "gensrc_vc8.sln"
    File "gensrc_vc9.sln"
    File "gensrc_vc7.vcproj"
    File "gensrc_vc8.vcproj"
    File "gensrc_vc9.vcproj"
    File "dependencies.vc"
    File "gensrc.py"
    File "gensrc.nsi"

    SetOutPath $INSTDIR\gensrc
    File /r "gensrc\*.py"
    File /r "gensrc\stub.*"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\gensrc-docs-${VER_NUMBER}.chm"
    File "Docs\docs_vc7.vcproj"
    File "Docs\docs_vc8.vcproj"
    File "Docs\gensrc.doxy"
    File "Docs\gs_footer.html"
    File "Docs\gs_header.html"
    File "Docs\gs_headeronline.html"
    File "Docs\Makefile.vc"
    File "Docs\doxygen.css"
    File "Docs\ql.css"
    File "Docs\tabs.css"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\*.ico"
    File "Docs\images\*.jpg"
    File "Docs\images\*.png"
    #File "Docs\images\*.php"

    SetOutPath "$INSTDIR\Docs\pages"
    File "Docs\pages\*.docs"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\gensrc-${VER_NUMBER}" \
                "DisplayName" "gensrc ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\gensrc-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\gensrcUninstall.exe"

    CreateDirectory "$SMPROGRAMS\${APP}-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\gensrc-docs-${VER_NUMBER}.chm"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Uninstall gensrc.lnk" \
                   "$INSTDIR\gensrcUninstall.exe" "" \
                   "$INSTDIR\gensrcUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc VC 7 project workspace.lnk" \
                   "$INSTDIR\gensrc_vc7.sln"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc VC 8 project workspace.lnk" \
                   "$INSTDIR\gensrc_vc8.sln"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc Folder.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc Home Page.url" \
                "InternetShortcut" "URL" "http://www.gensrc.org"

    WriteUninstaller "gensrcUninstall.exe"

SectionEnd

Section "Uninstall"

    RMDir /r /REBOOTOK "$INSTDIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\gensrc-${VER_NUMBER}"

    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\gensrc-${VER_NUMBER}"

SectionEnd

