
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.1.3"

# HEADER CONFIGURATION COMMANDS
Name "ObjectHandler"
Caption "ObjectHandler - Setup"
#do not change the name below
OutFile "..\ObjectHandler-${VER_NUMBER}-installer.exe"

InstType "Full (w/ WinHelp Documentation)"
InstType Minimal

ComponentText "This will install ObjectHandler ${VER_NUMBER} on your computer"

SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData LICENSE.txt
DirText "Please select a location to install ObjectHandler (or use the default):"
InstallDir $PROGRAMFILES\ObjectHandler
InstallDirRegKey HKEY_LOCAL_MACHINE SOFTWARE\ObjectHandler "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on


# INSTALLATION EXECUTION COMMANDS

Section "-ObjectHandler"
SectionIn 1 2
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\ObjectHandler"
    SetOutPath $INSTDIR

    # these MUST be present
    File "README.txt"
    File "LICENSE.txt"
    File "NEWS.txt"
    File "ObjectHandler.dsw"
    File "ObjectHandler.sln"
    File "ObjectHandler_vc8.sln"
    File ohlib.dsp
    File ohlib.vcproj
    File ohlib_vc8.vcproj

    File "*.txt"
    File "*.TXT"
    File "*.am"
    File "*.dsp"
    File "*.vcproj"
    File "*.nsi"

    SetOutPath  $INSTDIR\oh
    File /r "oh\*.hpp"
    File /r "oh\*.cpp"
    File /r "oh\*.am"

    SetOutPath  $INSTDIR\ohxl
    File /r "ohxl\*.hpp"
    File /r "ohxl\*.cpp"

    SetOutPath  $INSTDIR\ohxl\ohxll
    File /r "ohxl\ohxll\ohxll.dsp"
    File /r "ohxl\ohxll\ohxll.vcproj"
    File /r "ohxl\ohxll\ohxll_vc8.vcproj"
    File /r "ohxl\ohxll\*.cpp"

    SetOutPath  $INSTDIR\ohxl\ohxllib
    File /r "ohxl\ohxllib\ohxllib.dsp"
    File /r "ohxl\ohxllib\ohxllib.vcproj"
    File /r "ohxl\ohxllib\ohxllib_vc8.vcproj"

    SetOutPath  $INSTDIR\xlsdk
    File /r "xlsdk\xlsdk.dsp"
    File /r "xlsdk\xlsdk.vcproj"
    File /r "xlsdk\xlsdk_vc8.vcproj"
    File /r "xlsdk\xlcall32.lib"
    File /r "xlsdk\xlcall.h"
    File /r "xlsdk\*.hpp"
    File /r "xlsdk\*.cpp"

    SetOutPath $INSTDIR\Examples\C++
    File /r "Examples\C++\*.hpp"
    File /r "Examples\C++\*.cpp"
    File /r "Examples\C++\ExampleCpp.dsp"
    File /r "Examples\C++\ExampleCpp.vcproj"
    File /r "Examples\C++\ExampleCpp_vc8.vcproj"

    SetOutPath $INSTDIR\Examples\xl
    File /r "Examples\xl\*.hpp"
    File /r "Examples\xl\*.cpp"
    File /r "Examples\xl\*.dsp"
    File /r "Examples\xl\*.vcproj"
    File /r "Examples\xl\*.xls"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler" \
                "DisplayName" "ObjectHandler (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler" \
                "UninstallString" '"ObjectHandlerUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE \
                "SOFTWARE\ObjectHandler" \
                "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER \
                "Environment" \
                "OBJECT_HANDLER_DIR" "$INSTDIR"
    CreateShortCut "$SMPROGRAMS\ObjectHandler\Uninstall ObjectHandler.lnk" \
                   "$INSTDIR\ObjectHandlerUninstall.exe" \
                   "" "$INSTDIR\ObjectHandlerUninstall.exe" 0
    CreateShortCut "$SMPROGRAMS\ObjectHandler\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\ObjectHandler\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\ObjectHandler\What's new.lnk" \
                   "$INSTDIR\News.txt"

    CreateShortCut "$SMPROGRAMS\ObjectHandler\ObjectHandler VC 6 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler.dsw"

    CreateShortCut "$SMPROGRAMS\ObjectHandler\ObjectHandler VC 7 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler.sln"

    CreateShortCut "$SMPROGRAMS\ObjectHandler\ObjectHandler VC 8 project workspace.lnk" \
                   "$INSTDIR\ObjectHandler_vc8.sln"

    WriteUninstaller "ObjectHandlerUninstall.exe"

SectionEnd

Section "WinHelp documentation"
SectionIn 1
  SetOutPath "$INSTDIR\Docs"
  File /nonfatal "Docs\ObjectHandler-docs-${VER_NUMBER}.chm"
  IfFileExists "$INSTDIR\Docs\ObjectHandler-docs-${VER_NUMBER}.chm" 0 \
                                                                 NoWinHelpDoc
      CreateShortCut "$SMPROGRAMS\ObjectHandler\Documentation (WinHelp).lnk" \
                 "$INSTDIR\Docs\ObjectHandler-docs-${VER_NUMBER}.chm"
  NoWinHelpDoc:
SectionEnd

Section "Start Menu Group"
SectionIn 1 2
  SetOutPath $SMPROGRAMS\ObjectHandler

#it doesn't work
#  CreateShortCut "$SMPROGRAMS\ObjectHandler\QuantLib Home Page.lnk" \
#                 "http://quantlib.org/index.html"
#this works
  WriteINIStr "$SMPROGRAMS\ObjectHandler\QuantLib Home Page.url" \
              "InternetShortcut" "URL" "http://quantlib.org/"

  CreateShortCut "$SMPROGRAMS\ObjectHandler\ObjectHandler Directory.lnk" \
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

UninstallText "This will uninstall ObjectHandler. Hit next to continue."


Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\ObjectHandler"
    DeleteRegKey HKEY_LOCAL_MACHINE SOFTWARE\ObjectHandler
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "OBJECT_HANDLER_DIR"
    Delete "$SMPROGRAMS\ObjectHandler\*.*"
    RMDir "$SMPROGRAMS\ObjectHandler"
    RMDir /r "$INSTDIR\Examples\C++"
    RMDir /r "$INSTDIR\Examples\xl"
    RMDir /r "$INSTDIR\Examples"
    RMDir /r "$INSTDIR\ohxl\xllib"
    RMDir /r "$INSTDIR\ohxl\xll"
    RMDir /r "$INSTDIR\ohxl"
    RMDir /r "$INSTDIR\oh"
    RMDir /r "$INSTDIR\lib"
    RMDir /r "$INSTDIR"
SectionEnd
