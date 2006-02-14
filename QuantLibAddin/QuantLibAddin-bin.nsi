
# to be used with NSIS 2.0 and up

!define VER_NUMBER "0.3.12"

Name "QuantLibAddin"
Caption "QuantLibAddin - Setup"
OutFile "QuantLibAddin-${VER_NUMBER}-bin.exe"
LicenseText "You must agree with the following license before installing:"
LicenseData LICENSE.TXT
DirText "Please select a location to install QuantLibAddin (or use the default):"
InstallDir $PROGRAMFILES\QuantLibAddin-${VER_NUMBER}
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"

Section
    SetOutPath $INSTDIR
    File "*.txt"
    File "*.TXT"

    SetOutPath $INSTDIR\Addins\Excel\xll
    File "Addins\Excel\xll\QuantLibAddinStatic-vc80-mt-s-0_3_12.xll"

    SetOutPath $INSTDIR\Clients\Excel
    File "Clients\Excel\*.xls"

    SetOutPath $INSTDIR\Docs
    File "Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin" \
                "DisplayName" "QuantLibAddin ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin" \
                "UninstallString" '"QuantLibAddinUninstall.exe"'

    CreateDirectory "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}\Uninstall QuantLibAddin.lnk" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" "" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" 0

    WriteUninstaller "QuantLibAddinUninstall.exe"
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

UninstallText "This will uninstall QuantLibAddin. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin"
    RMDir /r /REBOOTOK "$SMPROGRAMS\QuantLibAddin-${VER_NUMBER}"
    RMDir /r /REBOOTOK "$INSTDIR"
SectionEnd

