
!define VER_NUMBER "0.3.13"
!define VER_NUMBER_UNDERSCORE "0_3_13"

Name "QuantLibXL"
Caption "QuantLibXL - Setup"
OutFile "..\..\QuantLibXL-${VER_NUMBER}-bin.exe"
LicenseText "You must agree with the following license before installing:"
LicenseData "..\LICENSE.TXT"
DirText "Please select a location to install QuantLibXL (or use the default):"
InstallDir $PROGRAMFILES\QuantLibXL-${VER_NUMBER}
Icon "..\Docs\images\favicon.ico"
UninstallIcon "..\Docs\images\favicon.ico"

!include "FileFunc.nsh"

Section
    SetOutPath "$INSTDIR"
    File "..\LICENSE.TXT"
    File "..\NEWS.txt"
    File "..\README.txt"

    SetOutPath "$INSTDIR\Workbooks"
    File /r "Workbooks\*.xls"

    SetOutPath "$INSTDIR\framework"
    File "framework\*.xla"

    SetOutPath "$INSTDIR\xll"
    File "xll\QuantLibXL-vc80-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "DisplayName" "QuantLibXL ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "UninstallString" '"QuantLibXLUninstall.exe"'

    WriteRegStr HKEY_CURRENT_USER "Environment" "QUANTLIBXL_DIR" "$INSTDIR"

    CreateDirectory "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL-${VER_NUMBER}.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    SetOutPath "$INSTDIR\Docs"
    File /nonfatal "..\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    ; this should be conditional to the existance of the above file
    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" "" \
                   "$INSTDIR\QuantLibXLUninstall.exe" 0

    CreateShortCut "$DESKTOP\QuantLibXL-${VER_NUMBER}.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"

    WriteUninstaller "QuantLibXLUninstall.exe"
SectionEnd

Function .onInit
  SetOutPath $TEMP
  File /oname=spltmp.bmp "..\Docs\images\logo_ql.jpg"
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

    ;!execute 'unList.exe /DATE=1 /INSTDIR="Clients\Excel" /FILTER="*.xls" \
    ;    /LOG=qla1.log  /UNDIR_VAR="$INSTDIR\Workbooks" /MB=0'
    ;!include "qla1.log"
    ;RMDir "$INSTDIR\Workbooks"

    ;!execute 'unList.exe /DATE=1 /INSTDIR="framework" /FILTER="*.xla" \
    ;    /LOG=qla2.log  /UNDIR_VAR="$INSTDIR\framework" /MB=0'
    ;!include "qla2.log"
    ;RMDir "$INSTDIR\framework"

    Delete "$INSTDIR\xll\QuantLibXL-vc80-mt-s-${VER_NUMBER_UNDERSCORE}.xll"
    RMDir "$INSTDIR\xll"

    Delete "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
    RMDir "$INSTDIR\Docs"

    Delete "$INSTDIR\LICENSE.TXT"
    Delete "$INSTDIR\NEWS.txt"
    Delete "$INSTDIR\README.txt"
    Delete "$INSTDIR\QuantLibXLUninstall.exe"
    RMDir "$INSTDIR"

    RMDir /r "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}"

SectionEnd

