; QuantLibXL Network Distribution

; Use this script to create an installer suited for writing to a network drive
; e.g. so that a shared QuantLibXL build can be invoked from mutiple workstations
; using the Launcher.

; Includes

!include "MUI.nsh"

; Constants

!define VER_NUMBER "0.8.0"
!define VER_NUMBER_UNDERSCORE "0_8_0"

; General Attributes

!define /date NOW "%Y%m%d-%H_%M"

Name "QuantLibXL Network Distribution"
OutFile "..\QuantLibXL-${VER_NUMBER}-${NOW}-network.exe"

; Interface Settings

!define MUI_ICON "Docs\images\favicon.ico"
!define MUI_ABORTWARNING

!define MUI_DIRECTORYPAGE_TEXT_TOP \
"Specify the root folder for the QuantLibXL network installation. \
The various QuantLibXL components will be installed to subdirectories \
of this root folder as follows:$\n\
ROOT_FOLDER\xll - the XLL addin$\n\
ROOT_FOLDER\framework - the VBA addin$\n\
ROOT_FOLDER\Workbooks - the workbooks$\n\
ROOT_FOLDER\metadata - the XML metadata$\n\
ROOT_FOLDER\Docs - the chm documentation file"

!define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Root folder for QuantLibXL Network installation"

; Pages

!insertmacro MUI_PAGE_LICENSE "LICENSE.TXT"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

; Languages

!insertmacro MUI_LANGUAGE "English"

; Installer Sections

Section "-QuantLibXL Network Distribution"

    SetOutPath "$INSTDIR"
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"

    SetOutPath "$INSTDIR\framework"
    File "framework\QuantLibXL.xla"
    File "framework\QuantLibXLDeveloperTeam.cer"

    SetOutPath "$INSTDIR\metadata"
    File /r "..\QuantLibAddin\gensrc\metadata\*.xml"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    ;eric 28-mar-2007 - i doubt whether these files are required?
    ;SetOutPath "$INSTDIR\Docs\images"
    ;File "Docs\images\favicon.bmp"
    ;File "Docs\images\favicon.ico"
    ;File "Docs\images\logo_ql.jpg"

    ;eric 28-mar-2007 - i doubt whether these files are required?
    ;SetOutPath "$INSTDIR"
    ;File "QuantLibXL-bin.nsi"
    ;File "QuantLibXL-network.nsi"
    ;File "QuantLibXL.nsi"

    SetOutPath "$INSTDIR\xll"
    File "xll\QuantLibXL-vc80-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    SetOutPath "$INSTDIR\Workbooks"
    File /r /x Drafts "Workbooks\*.xls"

SectionEnd

