
; QuantLibXL full installer
# to be used with NSIS 2.0 and up

#SetCompressor /SOLID lzma

!define APP "QuantLibXL"
!define VER_NUMBER "1.6.0"
!define VER_NUMBER_UNDERSCORE "1_6_0"
!define COMPILER "vc90"

!define REV_NUMBER "-Rev17291"
!define /date NOW "%Y%m%d-%H_%M"

# HEADER CONFIGURATION COMMANDS
Name "${APP}"
Caption "${APP} - Setup"

#OutFile "..\${APP}-${VER_NUMBER}-${REV_NUMBER}-${NOW}.exe"
OutFile "..\${APP}-${VER_NUMBER}.exe"


ComponentText "This will install ${APP} ${VER_NUMBER} on your computer"

SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData "LICENSE.TXT"
DirText "Please select a location to install ${APP}-${VER_NUMBER} (or use the default):"
InstallDir $PROGRAMFILES\${APP}-${VER_NUMBER}
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\${APP}-${VER_NUMBER}" "Install_Dir"
Icon "Docs\images\favicon.ico"
UninstallIcon "Docs\images\favicon.ico"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

!include "FileFunc.nsh"

InstType "Full (with WinHelp Documentation & Spreadsheets)"
InstType "Typical (with Spreadsheets)"
InstType "Minimal (Addin Only)"


Section "-QuantLibXL"
SectionIn 1 2 3
    # this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\${APP}-${VER_NUMBER}"
    SetOutPath "$INSTDIR"
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"

    SetOutPath "$INSTDIR\framework"
    File "framework\QuantLibXL.xml"
    File "framework\QuantLibXL.xla"
    File "framework\QuantLibXLA.cer"
    File "framework\readme.txt"

    SetOutPath "$INSTDIR\Data"
    File /r "Data\*.xls"
    File /r "Data\*.xml"

    SetOutPath "$INSTDIR\metadata"
    File /r "..\QuantLibAddin\gensrc\metadata\*.xml"
    #File /r "metadata\*.xml"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\favicon.bmp"
    File "Docs\images\logo_ql.jpg"

    SetOutPath "$INSTDIR"
    File "QuantLibXL.nsi"
    File "QuantLibXL-bin.nsi"
    File "QuantLibXL-network.nsi"
    File "QuantLibXL-src.nsi"

    SetOutPath "$INSTDIR\xll"
    # runtime libraries needed
    #File "xll\QuantLibXLDynamic-${COMPILER}-mt-${VER_NUMBER_UNDERSCORE}.xll"
    #File "..\ObjectHandler\xll\ObjectHandler-xll-${COMPILER}-mt-${VER_NUMBER_UNDERSCORE}.xll"
    File "xll\QuantLibXL-${COMPILER}-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP}-${VER_NUMBER}" \
                "DisplayName" "${APP} ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP}-${VER_NUMBER}" \
                "UninstallString" '"QuantLibXLUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE \
                "SOFTWARE\${APP}-${VER_NUMBER}" \
                "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER "Environment" "QUANTLIBXL_DIR" "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" "" \
                   "$INSTDIR\QuantLibXLUninstall.exe" 0
    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\QuantLibXL.xla.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"
    CreateShortCut "$DESKTOP\QuantLibXL.xla.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"
    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\What's new.lnk" \
                   "$INSTDIR\News.txt"
    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\QuantLibXL Directory.lnk" \
                   "$INSTDIR"

    WriteINIStr "$SMPROGRAMS\${APP}-${VER_NUMBER}\QuantLib Home Page.url" \
                "InternetShortcut" "URL" "http://quantlib.org/"

    WriteINIStr "$SMPROGRAMS\${APP}-${VER_NUMBER}\QuantLibXL Home Page.url" \
                "InternetShortcut" "URL" "http://www.quantlibxl.org/"
                
    WriteUninstaller "QuantLibXLUninstall.exe"
SectionEnd

Section "WinHelp documentation"
SectionIn 1
  SetOutPath "$INSTDIR\Docs"
  File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
  IfFileExists "$INSTDIR\Docs\html\quantlibxl.chm" 0 NoWinHelpDoc
      CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                     "$INSTDIR\Docs\html\quantlibxl.chm"
  NoWinHelpDoc:
SectionEnd

Section "Spreadsheets"
SectionIn 1 2
    SetOutPath "$INSTDIR\Workbooks"
    File /r /x Drafts "Workbooks\*.xls"
    File /r /x Drafts "Workbooks\*.kof"

    CreateShortCut "$SMPROGRAMS\${APP}-${VER_NUMBER}\Example workbooks.lnk" \
                   "$INSTDIR\Workbooks"

SectionEnd

Function .onInit

;function that will detect whether your software is already installed and,
;if so, allows the user to uninstall it first.

    ReadRegStr $R0 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP}-${VER_NUMBER}" \
    "Uninstall"
    StrCmp $R0 "" done

    ClearErrors
    ExecWait '$R0 _?=$INSTDIR' ;Do not copy the uninstaller to a temp file

    IfErrors no_remove_uninstaller
        ;You can either use Delete /REBOOTOK in the uninstaller or add some code
        ;here to remove the uninstaller. Use a registry key to check
        ;whether the user has chosen to uninstall. If you are using an uninstaller
        ;components page, make sure all sections are uninstalled.
  no_remove_uninstaller:
done:

  SetOutPath $TEMP
  File /oname=spltmp.bmp "Docs\images\logo_ql.jpg"
  splash::show 2000 $TEMP\spltmp
  Pop $0 ; $0 has '1' if the user closed the splash screen early,
         ;        '0' if everything closed normal,
         ;        '-1' if some error occured.
 Delete $TEMP\spltmp.bmp
FunctionEnd

UninstallText "This will uninstall ${APP}-${VER_NUMBER}. Hit next to continue."

Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP}-${VER_NUMBER}"
    DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\${APP}-${VER_NUMBER}"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QUANTLIBXL_DIR"

    #!execute 'unList.exe /DATE=1 /INSTDIR="Clients\Excel" /FILTER="*.xls" \
    #    /LOG=qla1.log  /UNDIR_VAR="$INSTDIR\Workbooks" /MB=0'
    #!include "qla1.log"
    #RMDir "$INSTDIR\Workbooks"
    #
    #!execute 'unList.exe /DATE=1 /INSTDIR="framework" /FILTER="*.xla" \
    #    /LOG=qla2.log  /UNDIR_VAR="$INSTDIR\framework" /MB=0'
    #!include "qla2.log"
    #RMDir "$INSTDIR\framework"

    #Delete "$INSTDIR\xll\QuantLibXLDynamic-vc*-mt-${VER_NUMBER_UNDERSCORE}.xll"
    #Delete "$INSTDIR\xll\ObjectHandler-xll-vc*-mt-${VER_NUMBER_UNDERSCORE}.xll"
    Delete "$INSTDIR\xll\QuantLibXL-${COMPILER}-mt-s-${VER_NUMBER_UNDERSCORE}.xll"
    RMDir "$INSTDIR\xll"

    Delete "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"
    RMDir "$INSTDIR\Docs"

    Delete "$INSTDIR\LICENSE.TXT"
    Delete "$INSTDIR\NEWS.txt"
    Delete "$INSTDIR\README.txt"
    Delete "$INSTDIR\QuantLibXLUninstall.exe"
    RMDir "$INSTDIR"

    RMDir "$SMPROGRAMS\${APP}-${VER_NUMBER}"
    #RMDir /r "$SMPROGRAMS\${APP}-${VER_NUMBER}"

SectionEnd

