
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.1.1"

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
    File "ObjectHandler.dev"
    File "makefile.mak"

    File "*.txt"
    File "*.TXT"
    File "*.am"
    File "*.dsp"
    File "*.vcproj"
    File "*.nsi"

    SetOutPath  $INSTDIR\oh
    File /r "oh\*.hpp"
    File /r "oh\*.cpp"
    File /r "oh\makefile.mak"
    File /r "oh\*.am"

    SetOutPath $INSTDIR\Example
    File /r "Example\*.hpp"
    File /r "Example\*.cpp"
    File /r "Example\*.dsp"
    File /r "Example\*.dev"
    File /r "Example\makefile.mak"
    File /r "Example\*.am"
    File /r "Example\*.vcproj"

    SetOutPath $INSTDIR\Docs
    File /r "Docs\*.am"
    File /r "Docs\*.bmp"
    File /r "Docs\*.css"
    File /r "Docs\*.docs"
    File /r "Docs\*.doxy"
    File /r "Docs\*.eps"
    File /r "Docs\*.html"
    File /r "Docs\*.jpg"
    File /r "Docs\*.pdf"
    File /r "Docs\*.png"
    File /r "Docs\makefile.mak"

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
    RMDir /r "$INSTDIR\Examples"
    RMDir /r "$INSTDIR\Docs"
    RMDir /r "$INSTDIR\oh"
    RMDir /r "$INSTDIR\lib"
    RMDir /r "$INSTDIR"
SectionEnd
