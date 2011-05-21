
# !defines

!define APP "QuantLibXL"
!define VER_NUMBER "1.1.0"
!define VER_NUMBER_UNDERSCORE "1_1_0"
!define COMPILER "vc90"
!define DEFAULT_PATH "c:\build_ql_1_1_0\${APP}"

# Compiler Flags

SetCompressor lzma

# Installer Attributes

Caption "${APP} - Setup"
DirText "Please select a location to install ${APP} (or use the default):"
Icon "Docs\images\favicon.ico"
InstallDir "${DEFAULT_PATH}"
#InstallDir $PROGRAMFILES\${APP}-${VER_NUMBER}
LicenseData "LICENSE.TXT"
LicenseText "${APP} is released under the following license:"
Name "${APP}"
OutFile "..\${APP}-src-${VER_NUMBER}.exe"
UninstallIcon "Docs\images\favicon.ico"
UninstallText "This will uninstall ${APP}. Hit next to continue."

ComponentText \
"By default the installer will install the QuantLibXL source code and basic example workbooks." \
"Optional components:" \
"The QuantLibXL Framework is a business application layer written in Excel VBA, \
including template workbooks for market data and interest rate derivates."

Section

    SetOutPath "$INSTDIR"
    
    File "Authors.txt"
    File "Contributors.txt"
    File "LICENSE.TXT"
    File "NEWS.txt"
    File "README.txt"

    File "*.sln"
    File "*.txt"
    File "*.TXT"
    File "QuantLibXL.nsi"
    File "${APP}-bin.nsi"
    #File "QuantLibXL-network.nsi"
    File "QuantLibXL-src.nsi"
    File /r "*.vcproj"
    File /r "*.vcxproj"
    File /r "*.vcxproj.filters"
    File /r "*.hpp"
    File /r "*.cpp"

    SetOutPath "$INSTDIR\Docs"
    File "Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    File "Docs\*.doxy"
    File "Docs\*.html"
    File "Docs\Makefile.vc"
    File "Docs\doxygen.css"
    File "Docs\ql.css"
    File "Docs\tabs.css"

    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\*.ico"
    File "Docs\images\*.jpg"
    File "Docs\images\*.png"

    SetOutPath "$INSTDIR\Docs\pages"
    File "Docs\pages\*.docs"

    SetOutPath "$INSTDIR\Workbooks\DateCalendarsDayCounters"
    File "Workbooks\DateCalendarsDayCounters\*.xls"

    SetOutPath "$INSTDIR\Workbooks\Math"
    File "Workbooks\Math\*.xls"

    SetOutPath "$INSTDIR\Workbooks\StandaloneExamples"
    File /r "Workbooks\StandaloneExamples\*.xls"

    SetOutPath "$INSTDIR\Workbooks\Utilities"
    File "Workbooks\Utilities\*.xls"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL-src-${VER_NUMBER}" \
                "DisplayName" "QuantLibXL-src ${VER_NUMBER} (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL-src-${VER_NUMBER}" \
                "UninstallString" "$INSTDIR\QuantLibXLUninstall.exe"

    CreateDirectory "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" "" \
                   "$INSTDIR\QuantLibXLUninstall.exe" 0

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL VC 7 project workspace.lnk" \
                   "$INSTDIR\QuantLibXL_basic_vc7.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL VC 8 project workspace.lnk" \
                   "$INSTDIR\QuantLibXL_basic_vc8.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL VC 9 project workspace.lnk" \
                   "$INSTDIR\QuantLibXL_basic_vc9.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL VC 10 project workspace.lnk" \
                   "$INSTDIR\QuantLibXL_basic_vc10.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL.xla.lnk" \
                   "$INSTDIR\framework\QuantLibXL.xla"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\README.txt.lnk" \
                   "$INSTDIR\README.txt"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\QuantLibXL Directory.lnk" \
                   "$INSTDIR"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\Documentation (WinHelp).lnk" \
                   "$INSTDIR\Docs\QuantLibXL-docs-${VER_NUMBER}.chm"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\Example workbooks.lnk" \
                   "$INSTDIR\Workbooks"

    WriteINIStr "$SMPROGRAMS\${APP}-bin-${VER_NUMBER}\QuantLib Home Page.url" \
                "InternetShortcut" "URL" "http://www.quantlib.org/"

    WriteINIStr "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}\QuantLibXL Home Page.url" \
                "InternetShortcut" "URL" "http://www.quantlibxl.org/"

    WriteUninstaller "QuantLibXLUninstall.exe"

SectionEnd

Section /o Framework

    SetOutPath "$INSTDIR\xll"
    #File "xll\QuantLibXLDynamic-${COMPILER}-mt-${VER_NUMBER_UNDERSCORE}.xll"
    #File "..\ObjectHandler\xll\ObjectHandler-xll-${COMPILER}-mt-${VER_NUMBER_UNDERSCORE}.xll"
    File "xll\QuantLibXL-${COMPILER}-mt-s-${VER_NUMBER_UNDERSCORE}.xll"

    SetOutPath "$INSTDIR\framework"
    File "framework\QuantLibXL.xla"
    File "framework\QuantLibXLA.cer"

    SetOutPath "$INSTDIR\Workbooks"
    File /r "Workbooks\*.xls"

    SetOutPath "$INSTDIR\Data"
    File /r "Data\*.xls"
    File /r "Data\*.xml"

    SetOutPath "$INSTDIR\framework2"
    File /r "framework2\*.txt"
    File /r "framework2\*.xla"
    File /r "framework2\*.xlam"
    File /r "framework2\*.xls"
    File /r "framework2\*.xml"

    # ObjectBuilder crashes if it can't find the icon
    SetOutPath "$INSTDIR\Docs\images"
    File "Docs\images\favicon.bmp"

SectionEnd

Section "Uninstall"

    RMDir /r /REBOOTOK "$INSTDIR"
    RMDir /r /REBOOTOK "$SMPROGRAMS\QuantLibXL-src-${VER_NUMBER}"

    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL-src-${VER_NUMBER}"

SectionEnd

