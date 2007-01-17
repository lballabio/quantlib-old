
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.9.0"
!define /date NOW "%Y%m%d-%H_%M"

# HEADER CONFIGURATION COMMANDS
Name "gensrc"
Caption "gensrc - Setup"
OutFile "..\gensrc-${VER_NUMBER}.exe"

SilentInstall normal
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"
CRCCheck on
LicenseText "gensrc is released under the following license:"
LicenseData LICENSE.txt
DirText "Please select a location to install gensrc (or use the default):"
InstallDir $PROGRAMFILES\gensrc-${VER_NUMBER}
InstallDirRegKey HKEY_LOCAL_MACHINE SOFTWARE\gensrc-${VER_NUMBER} "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

# INSTALLATION EXECUTION COMMANDS

Section "-gensrc"
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\gensrc-${VER_NUMBER}"
    SetOutPath $INSTDIR

    # these MUST be present
    File "Authors.txt"
    File "dependencies.vc"
    File "gensrc.sln"
    File "gensrc_vc8.sln"
    File "gensrc.vcproj"
    File "gensrc_vc8.vcproj"
    File "LICENSE.txt"
    File "NEWS.txt"
    File "README.txt"

    SetOutPath $INSTDIR\import
    File "import\*.py"

    SetOutPath $INSTDIR\metadata
    File "metadata\*.xml"

    SetOutPath $INSTDIR\stubs
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
                "UninstallString" '"gensrcUninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "GENSRC_DIR" "$INSTDIR"

    WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\gensrc-${VER_NUMBER}" \
                "Install_Dir" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Uninstall gensrc.lnk" \
                   "$INSTDIR\gensrcUninstall.exe" "" \
                   "$INSTDIR\gensrcUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Authors.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.TXT"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\NEWS.txt"

    CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\gensrc-docs-${VER_NUMBER}.chm"

    WriteINIStr "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc Home Page.url" \
                "InternetShortcut" "URL" "http://www.gensrc.org"

    WriteUninstaller "gensrcUninstall.exe"

SectionEnd

Section "Start Menu Group"
  SetOutPath $SMPROGRAMS\gensrc-${VER_NUMBER}

  WriteINIStr "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc Home Page.url" \
              "InternetShortcut" "URL" "http://www.gensrc.org/"

  CreateShortCut "$SMPROGRAMS\gensrc-${VER_NUMBER}\gensrc Folder.lnk" \
                 "$INSTDIR"
SectionEnd

UninstallText "This will uninstall gensrc. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\gensrc-${VER_NUMBER}"
    DeleteRegKey HKEY_LOCAL_MACHINE SOFTWARE\gensrc-${VER_NUMBER}
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "GENSRC_DIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\gensrc-${VER_NUMBER}"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd

