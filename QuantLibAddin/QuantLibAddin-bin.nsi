
# to be used with NSIS 2.0 and up

!define VER_NUMBER "0.3.13"

Name "QuantLibXL"
Caption "QuantLibXL - Setup"
OutFile "QuantLibXL-${VER_NUMBER}-bin.exe"
LicenseText "You must agree with the following license before installing:"
LicenseData LICENSE.TXT
DirText "Please select a location to install QuantLibXL (or use the default):"
InstallDir $PROGRAMFILES\QuantLibXL-${VER_NUMBER}
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"

Section
    SetOutPath $INSTDIR
    File "*.txt"
    File "*.TXT"

    SetOutPath $INSTDIR\xll
    File "Addins\Excel\xll\QuantLibAddinStatic-vc80-mt-s-0_3_13.xll"

    SetOutPath $INSTDIR\Workbooks
    File /r "Clients\Excel\*.xls"

    SetOutPath $INSTDIR\framework
    File /r "framework\*"

    SetOutPath $INSTDIR\Docs
    File /nonfatal "Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"
    File "Docs\images\favicon.ico"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "DisplayName" "QuantLibXL ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "UninstallString" '"QuantLibXLUninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "QUANTLIBXL_DIR" "$INSTDIR"

    CreateDirectory "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL-${VER_NUMBER}.lnk" \
                   "$INSTDIR\framework\quantlibxl-${VER_NUMBER}.xla"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" "" \
                   "$INSTDIR\QuantLibXLUninstall.exe" 0

    WriteUninstaller "QuantLibXLUninstall.exe"
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

UninstallText "This will uninstall QuantLibXL. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL"
    RMDir /r /REBOOTOK "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd

