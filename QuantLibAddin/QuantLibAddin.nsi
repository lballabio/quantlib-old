
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.3.10"

# HEADER CONFIGURATION COMMANDS
Name "QuantLibAddin"
Caption "QuantLibAddin - Setup"
#do not change the name below
OutFile "..\QuantLibAddin-${VER_NUMBER}-installer.exe"

InstType "Full (w/ WinHelp Documentation)"
InstType Minimal

ComponentText "This will install QuantLibAddin ${VER_NUMBER} on your computer"

SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData LICENSE.txt
DirText "Please select a location to install QuantLibAddin (or use the default):"
InstallDir $PROGRAMFILES\QuantLibAddin
InstallDirRegKey HKEY_LOCAL_MACHINE SOFTWARE\QuantLibAddin "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on



# INSTALLATION EXECUTION COMMANDS

Section "-QuantLibAddin"
SectionIn 1 2
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\QuantLibAddin"
    SetOutPath $INSTDIR

    # these MUST be present
    File "README.txt"
    File "LICENSE.txt"
    File "NEWS.txt"
    File "QuantLibAddin.dsw"
    File "QuantLibAddin.sln"
    File "QuantLibAddin.dev"

    File "*.txt"
    File "*.TXT"
    File "Makefile.am"
    File "QuantLibAddin.dsp"
    File "QuantLibAddin*.vcproj"
    File "QuantLibAddin.nsi"

    SetOutPath  $INSTDIR\qla
    File /r "qla\*.hpp"
    File /r "qla\*.cpp"
    File /r "qla\Makefile.am"

    SetOutPath $INSTDIR\Addins\C
    File /r "Addins\C\AddinC.dev"
    File /r "Addins\C\AddinC.dsp"
    File /r "Addins\C\AddinC*.vcproj"
    File /r "Addins\C\Makefile.am"
    File /r "Addins\C\*.cpp"
    File /r "Addins\C\*.h"

    SetOutPath $INSTDIR\Addins\Calc
    File /r "Addins\Calc\AddinCalc.dsp"
    File /r "Addins\Calc\AddinCalc*.vcproj"
    File /r "Addins\Calc\Makefile.am"
    File /r "Addins\Calc\Makefile.msdev*"
    File /r "Addins\Calc\QuantLibAddin.def"
    File /r "Addins\Calc\readme.txt"
    File /r "Addins\Calc\*.cpp"
    File /r "Addins\Calc\*.hpp"

    SetOutPath $INSTDIR\Addins\Excel
    File /r "Addins\Excel\AddinExcel.dev"
    File /r "Addins\Excel\AddinExcel.dsp"
    File /r "Addins\Excel\AddinExcel*.vcproj"
    File /r "Addins\Excel\Makefile.am"
    File /r "Addins\Excel\*.cpp"
    File /r "Addins\Excel\*.h"
    File /r "Addins\Excel\*.hpp"
    File /r "Addins\Excel\lib\XLCALL32.LIB"

    SetOutPath $INSTDIR\Addins\Guile
    File /r "Addins\Guile\Makefile.am"
    File /r "Addins\Guile\*.cpp"
    File /r "Addins\Guile\*.hpp"
    File /r "Addins\Guile\*.h"

    SetOutPath $INSTDIR\Autogen
    File /r "Autogen\Autogen.dsp"
    File /r "Autogen\Makefile.am"
    File /r "Autogen\Makefile.msdev"
    File /r "Autogen\*.py"
    File /r "Autogen\*.vcproj"
    File /r "Autogen\*.xml"
    File /r "Autogen\stub.*"

    SetOutPath $INSTDIR\Clients\C
    File /r "Clients\C\ClientCDemo.dsp"
    File /r "Clients\C\ClientCDemo*.vcproj"
    File /r "Clients\C\Makefile.am"
    File /r "Clients\C\*.c"

    SetOutPath $INSTDIR\Clients\C++
    File /r "Clients\C++\Makefile.am"
    File /r "Clients\C++\*.dsp"
    File /r "Clients\C++\*.vcproj"
    File /r "Clients\C++\*.cpp"

    SetOutPath $INSTDIR\Clients\Calc
    File /r "Clients\Calc\Makefile.am"
    File /r "Clients\Calc\*.sxc"

    SetOutPath $INSTDIR\Clients\Excel
    File /r "Clients\Excel\Makefile.am"
    File /r "Clients\Excel\*.xls"

    SetOutPath $INSTDIR\Clients\Guile
    File /r "Clients\Guile\Makefile.am"
    File /r "Clients\Guile\*.scm"

    SetOutPath $INSTDIR\Docs
    File /r "Docs\Makefile.am"
    File /r "Docs\*.bmp"
    File /r "Docs\*.css"
    File /r "Docs\*.docs"
    File /r "Docs\*.doxy"
    File /r "Docs\*.eps"
    File /r "Docs\*.html"
    File /r "Docs\*.jpg"
    File /r "Docs\*.pdf"
    File /r "Docs\*.png"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin" \
                "DisplayName" "QuantLibAddin (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibAddin" \
                "UninstallString" '"QuantLibAddinUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE \
                "SOFTWARE\QuantLibAddin" \
                "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER \
                "Environment" \
                "QUANTLIBADDIN_DIR" "$INSTDIR"
    CreateShortCut "$SMPROGRAMS\QuantLibAddin\Uninstall QuantLibAddin.lnk" \
                   "$INSTDIR\QuantLibAddinUninstall.exe" \
                   "" "$INSTDIR\QuantLibAddinUninstall.exe" 0
    CreateShortCut "$SMPROGRAMS\QuantLibAddin\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibAddin\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibAddin\What's new.lnk" \
                   "$INSTDIR\News.txt"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin\QuantLibAddin VC 6 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin.dsw"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin\QuantLibAddin VC 7 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin.sln"

    CreateShortCut "$SMPROGRAMS\QuantLibAddin\QuantLibAddin VC 8 project workspace.lnk" \
                   "$INSTDIR\QuantLibAddin_vc8.sln"

    WriteUninstaller "QuantLibAddinUninstall.exe"

SectionEnd

Section "WinHelp documentation"
SectionIn 1
  SetOutPath "$INSTDIR\Docs"
  File /nonfatal "Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"
  IfFileExists "$INSTDIR\Docs\QuantLibAddin-docs-${VER_NUMBER}.chm" 0 \
                                                                 NoWinHelpDoc
      CreateShortCut "$SMPROGRAMS\QuantLibAddin\Documentation (WinHelp).lnk" \
                 "$INSTDIR\Docs\QuantLibAddin-docs-${VER_NUMBER}.chm"
  NoWinHelpDoc:
SectionEnd

Section "Start Menu Group"
SectionIn 1 2
  SetOutPath $SMPROGRAMS\QuantLibAddin

  WriteINIStr "$SMPROGRAMS\QuantLibAddin\QuantLib Home Page.url" \
              "InternetShortcut" "URL" "http://quantlib.org/"

  CreateShortCut "$SMPROGRAMS\QuantLibAddin\QuantLibAddin Directory.lnk" \
                 "$INSTDIR"
SectionEnd

Function .onInit
  SetOutPath $TEMP
  File /oname=spltmp.bmp "Docs\images\QL.bmp"
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
    DeleteRegKey HKEY_LOCAL_MACHINE SOFTWARE\QuantLibAddin
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QUANTLIBADDIN_DIR"
    Delete "$SMPROGRAMS\QuantLibAddin\*.*"
    RMDir "$SMPROGRAMS\QuantLibAddin"
    RMDir /r "$INSTDIR"
SectionEnd
