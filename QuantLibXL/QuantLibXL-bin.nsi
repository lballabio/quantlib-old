
# tested with NSIS version 2.22

# !defines

!define VER_NUMBER "0.4.0"
!define VER_NUMBER_UNDERSCORE "0_4_0"
!define /date NOW "%Y%m%d-%H_%M"

# Compiler flags

# Uncommenting the line below should result in a smaller, faster installer
# but the NSIS compilation is much slower.
#SetCompressor /SOLID lzma

# General attributes

Name "QuantLibXL"
Caption "QuantLibXL - Setup"

# for public release - exclude timestamp from filename
#OutFile "..\QuantLibXL-${VER_NUMBER}-${NOW}.exe"
OutFile "..\QuantLibXL-bin-${VER_NUMBER}.exe"

ComponentText "This will install QuantLibXL ${VER_NUMBER} on your computer"
SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData "LICENSE.TXT"
DirText "Please select a location to install QuantLibXL (or use the default):"
# VC relative paths break if version number is included in QLXL install dir
#InstallDir $PROGRAMFILES\QuantLibXL
InstallDir $PROGRAMFILES\QuantLibXL-${VER_NUMBER}
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL" "Install_Dir"
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on
UninstallText "This will uninstall QuantLibXL. Hit next to continue."

InstType "Full (Addin, Framework, Workbooks, and Documentation)"
InstType "No Workbooks (Addin, Framework, and Documentation)"

Section "-QuantLibXL"
SectionIn 1 2
    SetOutPath "$INSTDIR"
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "DisplayName" "QuantLibXL ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "UninstallString" '"$INSTDIR\QuantLibXLUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL" "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER "Environment" "QUANTLIBXL_DIR" "$INSTDIR"

    # this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" "" \
                   "$INSTDIR\QuantLibXLUninstall.exe" 0
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\News.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL Directory.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL Home Page.url" \
                "InternetShortcut" "URL" "http://www.quantlibxl.org/"

    WriteUninstaller "QuantLibXLUninstall.exe"

SectionEnd

Section "Addin"
SectionIn 1 2
    SetOutPath "$INSTDIR\xll"
    File "xll\QuantLibXL-vc*-mt-s-${VER_NUMBER_UNDERSCORE}.xll"
SectionEnd

Section "Framework"
SectionIn 1 2
    SetOutPath "$INSTDIR\framework"
    File "framework\*.xla"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL-${VER_NUMBER}.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"
    CreateShortCut "$DESKTOP\QuantLibXL-${VER_NUMBER}.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"
SectionEnd

Section "Documentation"
SectionIn 1 2
    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Documentation (WinHelp).lnk" \
        "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
SectionEnd

Section "Workbooks"
SectionIn 1
    SetOutPath "$INSTDIR"
    File /r "*.xls"
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Example workbooks.lnk" \
                   "$INSTDIR\Workbooks"
SectionEnd

Section "-promptReboot"
SectionIn 1 2
    MessageBox MB_YESNO|MB_ICONQUESTION \
        "You need to reboot your computer before using QuantLibXL.  Do you want to reboot now?" \
        IDNO +2
    Reboot
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

Section "Uninstall"

    RMDir /r "$INSTDIR"
    RMDir /r "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"

    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL"
    DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QUANTLIBXL_DIR"

SectionEnd

