
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
InstallDir "X:\Offices1\CabotoXL"

; Interface Settings

!define MUI_ICON "Docs\images\favicon.ico"
!define MUI_UNICON "Docs\images\favicon.ico"
!define MUI_ABORTWARNING

!define MUI_DIRECTORYPAGE_TEXT_TOP \
"Specify the root folder for the QuantLibXL network installation. The various \
QuantLibXL components will be installed to subdirectories of this root folder \
as follows:$\n\
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

; Functions

Function deletePrevious

    ; Uninstall the previous network installation of QLXL

    MessageBox MB_YESNO \
'You are about to overwrite the installation of QuantLibXL$\n\
(if any) which currently resides at$\n\
$INSTDIR$\n\
Are you certain that this is what you want to do?' IDYES +2
    Quit

    ; For most directories we just do a recursive delete:
    RMDir /r "$INSTDIR\xll"
    RMDir /r "$INSTDIR\framework"
    RMDir /r "$INSTDIR\metadata"
    RMDir /r "$INSTDIR\Docs"

    ; For the Workbooks directory, we explicitly delete the contents of each subdirectory
    ; without deleting the directories themselves.  This is to preserve directory permissions
    ; for the installation of the next version of QLXL.
    Delete "$INSTDIR\Workbooks\*"
    Delete "$INSTDIR\Workbooks\Bonds\*"
    Delete "$INSTDIR\Workbooks\CmsCalibrations\*"
    Delete "$INSTDIR\Workbooks\CoveredWarrants\*"
    Delete "$INSTDIR\Workbooks\DateCalendarsDayCounters\*"
    Delete "$INSTDIR\Workbooks\FrozenMarketData\*"
    Delete "$INSTDIR\Workbooks\FrozenMarketData\ManualFeed\*"
    Delete "$INSTDIR\Workbooks\FrozenMarketData\ReutersFeed\*"
    Delete "$INSTDIR\Workbooks\InterestRateDerivatives\*"
    Delete "$INSTDIR\Workbooks\MarketData\*"
    Delete "$INSTDIR\Workbooks\MarketData\BloombergFeed\*"
    Delete "$INSTDIR\Workbooks\MarketData\ManualFeed\*"
    Delete "$INSTDIR\Workbooks\MarketData\ReutersFeed\*"
    Delete "$INSTDIR\Workbooks\MarketMetaData\*"
    Delete "$INSTDIR\Workbooks\MarketModels\*"
    Delete "$INSTDIR\Workbooks\Math\*"
    Delete "$INSTDIR\Workbooks\ohTests\*"
    Delete "$INSTDIR\Workbooks\OriginalExamples\*"
    Delete "$INSTDIR\Workbooks\Tests\*"
    Delete "$INSTDIR\Workbooks\TimeSeries\*"
    Delete "$INSTDIR\Workbooks\TimeSeries\BloombergFeed\*"
    Delete "$INSTDIR\Workbooks\TimeSeries\ReutersFeed\*"
    Delete "$INSTDIR\Workbooks\Utilities\*"

    ; Delete the contents of the root installation directory:
    Delete "$INSTDIR\*"

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
    File "framework\QuantLibXLDeveloperTeam.cer"

    SetOutPath "$INSTDIR\metadata"
    File /r "..\QuantLibAddin\gensrc\metadata\*.xml"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\favicon.bmp"
    ;eric 28-mar-2007 - i doubt whether these files are required?
    ;nando 29-mar-2007 - favicon.bmp is needed. not sure about the following 2
    ;File "Docs\images\favicon.ico"
    ;File "Docs\images\logo_ql.jpg"

    SetOutPath "$INSTDIR"
    File "QuantLibXL-bin.nsi"
    File "QuantLibXL-network.nsi"
    File "QuantLibXL.nsi"

    SetOutPath "$INSTDIR\xll"
    File "xll\QuantLibXL-vc80-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    SetOutPath "$INSTDIR\Workbooks"
    File /r /x Drafts "Workbooks\*.xls"

SectionEnd
