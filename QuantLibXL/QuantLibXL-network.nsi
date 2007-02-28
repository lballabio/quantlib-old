; QuantLibXL Network Distribution

; Use this script to create an installer suited for writing
; to a network drive e.g. so that a shared QuantLibXL build
; can be invoked from mutiple workstations using the Launcher.

; NB This script previously contained functionality to implement a custom dialog
; allowing the user to specify multiple variables for use during installation.
; This functionality is no longer required but is left commented out in case
; needed again.

; Includes

!include "MUI.nsh"
;!include "FileFunc.nsh"

; Constants

!define VER_NUMBER "0.9.0"
!define VER_NUMBER_UNDERSCORE "0_9_0"
;!define INI_FILE QuantLibXL.network.ini

; Variables

;Var FRAMEWORK_PATH
;Var WORKBOOK_PATH
;Var ADDIN_PATH

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
$\n\
ROOT_FOLDER\xll - the XLL addin$\n\
ROOT_FOLDER\framework - the VBA addin$\n\
ROOT_FOLDER\Workbooks - the workbooks$\n\
ROOT_FOLDER\docs - the chm documentation file"

!define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Root folder for QuantLibXL Network installation"

; Pages

;!insertmacro GetFileAttributes
!insertmacro MUI_PAGE_LICENSE "LICENSE.TXT"
;Page custom ShowInstallPath ValidateInstallPath
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

; Languages

!insertmacro MUI_LANGUAGE "English"

; Reserve Files

;ReserveFile "${INI_FILE}"
;!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

; Installer Sections

Section "-QuantLibXL Network Distribution"

    SetOutPath "$INSTDIR"
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"

    ;SetOutPath "$FRAMEWORK_PATH\"
    SetOutPath "$INSTDIR\framework"
    File "framework\QuantLibXL.xla"
    File "framework\QuantLibXLDeveloperTeam.cer"

    SetOutPath "$INSTDIR\metadata"
    File /r "..\QuantLibAddin\gensrc\metadata\*.xml"
    #File /r "metadata\*.xml"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\favicon.bmp"
    File "Docs\images\favicon.ico"
    File "Docs\images\logo_ql.jpg"

    SetOutPath "$INSTDIR"
    File "QuantLibXL-bin.nsi"
    File "QuantLibXL-network.nsi"
    File "QuantLibXL.nsi"

    ;SetOutPath "$ADDIN_PATH\"
    SetOutPath "$INSTDIR\xll"
    File "xll\QuantLibXL-vc80-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    ;SetOutPath "$WORKBOOK_PATH\"
    SetOutPath "$INSTDIR\Workbooks"
    File /r /x Drafts "Workbooks\*.xls"

SectionEnd

; Installer Functions

;Function .onInit
;  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "${INI_FILE}"
;FunctionEnd

;Function ShowInstallPath
;  !insertmacro MUI_HEADER_TEXT "Distribution Paths" \
;    "Specify the network drives to which the QuantLibXL components should be installed."
;  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "${INI_FILE}"
;FunctionEnd

;Function ValidateInstallPath
;  ReadINIStr $FRAMEWORK_PATH "$PLUGINSDIR\${INI_FILE}" "Field 2" "State"
;  ReadINIStr $WORKBOOK_PATH "$PLUGINSDIR\${INI_FILE}" "Field 4" "State"
;  ReadINIStr $ADDIN_PATH "$PLUGINSDIR\${INI_FILE}" "Field 6" "State"
;  Push $FRAMEWORK_PATH
;  Call ValidatePath
;  Push $WORKBOOK_PATH
;  Call ValidatePath
;  Push $ADDIN_PATH
;  Call ValidatePath
;FunctionEnd

;Function ValidatePath
;  ClearErrors
;  Pop $0
;  ${GetFileAttributes} "$0" "NORMAL" $R0
;  IfErrors 0 +3
;    MessageBox MB_OK "The path$\n    $0$\nis invalid."
;    Abort
;FunctionEnd
