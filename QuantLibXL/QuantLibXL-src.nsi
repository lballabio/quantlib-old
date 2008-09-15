
# !defines

!define APP "QuantLibXL"
!define VER_NUMBER "0.9.6"
!define VER_NUMBER_UNDERSCORE "0_9_6"
!define COMPILER "vc80"
!define DEFAULT_PATH "c:\build_ql_0_9_6\${APP}"

# Compiler Flags

SetCompressor lzma

# Installer Attributes

Caption "${APP} - Setup"
DirText "Please select a location to install ${APP} (or use the default):"
Icon "Docs\images\favicon.ico"
InstallDir "${DEFAULT_PATH}"
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
    File "QuantLibXL-bin.nsi"
    File "QuantLibXL-network.nsi"
    File "QuantLibXL-src.nsi"
    File /r "*.vcproj"
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
    #File "Docs\images\*.php"

    SetOutPath "$INSTDIR\Docs\pages"
    File "Docs\pages\*.docs"

    SetOutPath "$INSTDIR\Workbooks\DateCalendarsDayCounters"
    File "Workbooks\DateCalendarsDayCounters\*.xls"

    SetOutPath "$INSTDIR\Workbooks\Math"
    File "Workbooks\Math\*.xls"

    SetOutPath "$INSTDIR\Workbooks\StandaloneExamples"
    File /r "Workbooks\StandaloneExamples\*.xls"
    File /r "Workbooks\StandaloneExamples\*.xla"
    
    SetOutPath "$INSTDIR\Workbooks\StandaloneExamples\SerializationDemo"
    File "Workbooks\StandaloneExamples\SerializationDemo\README.TXT"

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
                   "$INSTDIR\QuantLibAllStatic_vc7.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibXL-${VER_NUMBER}\QuantLibXL VC 8 project workspace.lnk" \
                   "$INSTDIR\QuantLibAllStatic_vc8.sln"

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
    File "framework\QuantLibXLDeveloperTeam.cer"

    SetOutPath "$INSTDIR\Workbooks"
    File /r "Workbooks\*.xls"

    SetOutPath "$INSTDIR\Data"
    File /r "Data\*.xls"
    File /r "Data\*.xml"

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

