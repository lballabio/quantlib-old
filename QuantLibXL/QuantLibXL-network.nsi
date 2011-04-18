
; QuantLibXL Network Distribution

; Use this script to create an installer suited for writing to a network drive
; e.g. so that a shared QuantLibXL build can be invoked from mutiple workstations
; using the Launcher.

; Includes

!include "MUI.nsh"

; Constants

!define APP "QuantLibXL"
!define VER_NUMBER "1.1.0"
!define VER_NUMBER_UNDERSCORE "1_1_0"
!define COMPILER "vc90"

!define REV_NUMBER "Rev17718"
!define /date NOW "%Y%m%d-%H_%M"


# Compiler Flags

SetCompressor lzma

; General Attributes

Name "${APP} - Network Distribution"
OutFile "..\${APP}-${VER_NUMBER}-${REV_NUMBER}-${NOW}-network.exe"
InstallDir "X:\Apps\Appsscript\CabotoXL\${REV_NUMBER}"

; Interface Settings

!define MUI_ICON "Docs\images\favicon.ico"
!define MUI_UNICON "Docs\images\favicon.ico"
!define MUI_ABORTWARNING

!define MUI_DIRECTORYPAGE_TEXT_TOP \
"Specify the root folder for the ${APP} network installation:$\n\
ROOT_FOLDER\xll - the XLL addin$\n\
ROOT_FOLDER\framework - the VBA addin$\n\
ROOT_FOLDER\Workbooks - the workbooks$\n\
ROOT_FOLDER\metadata - the XML metadata$\n\
ROOT_FOLDER\Data - the xls/xml data serialization files$\n\
ROOT_FOLDER\Docs - the chm documentation file"

!define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Root folder for ${APP} Network installation"

; Pages

!insertmacro MUI_PAGE_LICENSE "LICENSE.TXT"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

; Languages

!insertmacro MUI_LANGUAGE "English"

; Functions

Function deletePrevious

    ; Uninstall the previous network installation of QLXL

    MessageBox MB_YESNO \
'You are about to overwrite the installation of ${APP}$\n\
(if any) which currently resides at$\n\
$INSTDIR$\n\
Are you certain that this is what you want to do?' IDYES +2
    Quit

    ; Delete the contents of the root installation directory:
    RMDir /r "$INSTDIR\*"

FunctionEnd

; Installer Sections

Section

    Call deletePrevious

    SetOutPath "$INSTDIR"
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"

    SetOutPath "$INSTDIR\framework"
    File "framework\QuantLibXL.xla"
    File "framework\QuantLibXLA.cer"

    SetOutPath "$INSTDIR\metadata"
    File /r "..\QuantLibAddin\gensrc\metadata\*.xml"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    SetOutPath "$INSTDIR\Data"
    File /r "Data\*.xls"
    File /r "Data\*.xml"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\favicon.bmp"

    SetOutPath "$INSTDIR"
    File "QuantLibXL.nsi"
    File "${APP}-bin.nsi"
    File "QuantLibXL-network.nsi"
    File "QuantLibXL-src.nsi"

    SetOutPath "$INSTDIR\xll"
    File "xll\QuantLibXLDynamic-${COMPILER}-mt-${VER_NUMBER_UNDERSCORE}.xll"
    File "..\ObjectHandler\xll\ObjectHandler-xll-${COMPILER}-mt-${VER_NUMBER_UNDERSCORE}.xll"

    SetOutPath "$INSTDIR\Workbooks"
    File /r /x Drafts "Workbooks\*.xls"
    File /r /x Drafts "Workbooks\*.kof"

SectionEnd
