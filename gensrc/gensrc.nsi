
# !defines

!define APP "gensrc"
!define VER_NUMBER "0.9.0"
!define DEFAULT_PATH "c:\build_ql_0_9_0\${APP}"

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

    File "Authors.txt"
    File "dependencies.vc"
    File "gensrc.sln"
    File "gensrc_vc8.sln"
    File "gensrc.vcproj"
    File "gensrc_vc8.vcproj"
    File "LICENSE.txt"
    File "NEWS.txt"
    File "README.txt"

    SetOutPath "$INSTDIR\import"
    File "import\*.py"

    SetOutPath "$INSTDIR\metadata"
    File "metadata\*.xml"

    SetOutPath "$INSTDIR\stubs"
    File "stubs\stub.*"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\gensrc-docs-${VER_NUMBER}.chm"
    File "Docs\docs.vcproj"
    File "Docs\docs_vc8.vcproj"
    File "Docs\gensrc.doxy"
    File "Docs\gs_footer.html"
    File "Docs\gs_header.html"
    File "Docs\gs_headeronline.html"
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
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\gensrc-${VER_NUMBER}" \
                "DisplayName" "gensrc ${VER_NUMBER} (remove only)"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\gensrc-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\gensrcUninstall.exe"

    WriteRegStr HKEY_CURRENT_USER "Environment" "GENSRC_DIR" "$INSTDIR"

    CreateDirectory "$SMPROGRAMS\gensrc-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Uninstall gensrc.lnk" \
                   "$INSTDIR\gensrcUninstall.exe" "" \
                   "$INSTDIR\gensrcUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\gensrc-docs-${VER_NUMBER}.chm"

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
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "GENSRC_DIR"

SectionEnd

