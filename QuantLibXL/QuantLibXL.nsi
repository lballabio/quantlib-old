
# to be used with NSIS 2.0 and up

SetCompressor lzma

!define VER_NUMBER "0.3.5"

# HEADER CONFIGURATION COMMANDS
Name "QuantLibXL"
Caption "QuantLibXL - Setup"
#do not change the name below
OutFile "..\QuantLibXL-${VER_NUMBER}.exe"

InstType "Full (w/ Source Code)"
InstType Typical
InstType Minimal

ComponentText "This will install QuantLibXL ${VER_NUMBER} on your computer"

SilentInstall normal
CRCCheck on
LicenseText "You must agree with the following license before installing:"
LicenseData License.txt
DirText "Please select a location to install QuantLibXL (or use the default):"
InstallDir "$PROGRAMFILES\QuantLibXL"
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL" "Install_Dir"
AutoCloseWindow false
ShowInstDetails hide
SetDateSave on

# INSTALLATION EXECUTION COMMANDS



Section "-QuantLibXL"
SectionIn 1 2 3
# this directory must be created first, or the CreateShortCut will not work
    CreateDirectory "$SMPROGRAMS\QuantLibXL"
    SetOutPath $INSTDIR
    File "Authors.txt"
    File "Contributors.txt"
    File "History.txt"
    File "LICENSE.txt"
    File "News.txt"
    File "README.txt"
    File "TODO.txt"

    SetOutPath $INSTDIR\xll\Win32\VisualStudio
    File "xll\Win32\VisualStudio\QuantLibXL.xll"
#    SetOutPath $INSTDIR\xll\Win32\Borland
#    File "xll\Win32\Borland\QuantLibXL.xll"

    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "DisplayName" "QuantLibXL (remove only)"
    WriteRegStr HKEY_LOCAL_MACHINE \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL" \
                "UninstallString" '"QuantLibXLUninstall.exe"'
    WriteRegStr HKEY_LOCAL_MACHINE \
                "SOFTWARE\QuantLibXL" \
                "Install_Dir" "$INSTDIR"
    WriteRegStr HKEY_CURRENT_USER \
                "Environment" \
                "QLXL_DIR" "$INSTDIR"
    CreateShortCut "$SMPROGRAMS\QuantLibXL\Uninstall QuantLibXL.lnk" \
                   "$INSTDIR\QuantLibXLUninstall.exe" \
                   "" "$INSTDIR\QuantLibXLUninstall.exe" 0
    CreateShortCut "$SMPROGRAMS\QuantLibXL\README.txt.lnk" \
                   "$INSTDIR\README.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL\LICENSE.txt.lnk" \
                   "$INSTDIR\LICENSE.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL\What's new.lnk" \
                   "$INSTDIR\News.txt"
    CreateShortCut "$SMPROGRAMS\QuantLibXL\History.txt.lnk" \
                   "$INSTDIR\History.txt"

    WriteUninstaller "QuantLibXLUninstall.exe"
SectionEnd



Section "Source Code"
SectionIn 1
  SetOutPath $INSTDIR
  File ChangeLog.txt
  File makefile.mak
  File QuantLibXL.dsp
  File QuantLibXL.dsw
  File QuantLibXL.mak
  File QuantLibXL.nsi

  SetOutPath  $INSTDIR\qlxl
  File /r "qlxl\*.hpp"
  File /r "qlxl\*.cpp"
  File /r "qlxl\makefile.mak"

  CreateShortCut "$SMPROGRAMS\QuantLibXL\QuantLibXL project workspace.lnk" \
                 "$INSTDIR\QuantLibXL.dsw"

SectionEnd

Section "Workbooks"
SectionIn 1 2
    SetOutPath $INSTDIR\Workbooks
#    File /r "test\*.txt"
#    File /r "Workbooks\*.xls"

     File /r "Workbooks\blackvol.xls"
     File /r "Workbooks\interp.xls"
     File /r "Workbooks\localvolatility.xls"
     File /r "Workbooks\Cholesky.xls"

     File /r "Workbooks\Binomial.xls"
     File /r "Workbooks\BlackScholes.xls"
     File /r "Workbooks\CorrelationMatrix.xls"
     File /r "Workbooks\Covariance.xls"
     File /r "Workbooks\europeangreeks.xls"
     File /r "Workbooks\FiniteDifferences.xls"
     File /r "Workbooks\FiniteDifferences.xls"
     File /r "Workbooks\interpolationderivative.xls"
     File /r "Workbooks\interpolationtest.xls"
     File /r "Workbooks\jumps.xls"
     File /r "Workbooks\normdist.xls"
     File /r "Workbooks\Paths.xls"
     File /r "Workbooks\Poisson.xls"
     File /r "Workbooks\primeNumbers.xls"
     File /r "Workbooks\QLPaths.xls"
     File /r "Workbooks\quantlib.xls"
     File /r "Workbooks\RandomNumbers.xls"
     File /r "Workbooks\RankReduction.xls"
     File /r "Workbooks\riskmeasures.xls"
     File /r "Workbooks\tsconstforward.xls"
     File /r "Workbooks\tsdiscount.xls"
     File /r "Workbooks\tspwcf.xls"

    CreateShortCut "$SMPROGRAMS\QuantLibXL\QuantLibXL Directory.lnk" \
                   "$INSTDIR"
    CreateShortCut "$SMPROGRAMS\QuantLibXL\Example workbooks.lnk" \
                   "$INSTDIR\Workbooks"

SectionEnd




Section "Start Menu Group"
SectionIn 1 2 3
  SetOutPath "$SMPROGRAMS\QuantLibXL"

  WriteINIStr "$SMPROGRAMS\QuantLibXL\QuantLib Home Page.url" \
              "InternetShortcut" "URL" "http://quantlib.org/"

  CreateShortCut "$SMPROGRAMS\QuantLibXL\QuantLibXL Directory.lnk" \
                 "$INSTDIR"
SectionEnd


Function .onInit

  SetOutPath $TEMP
  File /oname=spltmp.bmp "Docs\images\QL-largish.bmp"
  splash::show 2000 $TEMP\spltmp
  Pop $0 ; $0 has '1' if the user closed the splash screen early,
         ;        '0' if everything closed normal,
         ;        '-1' if some error occured.
  Delete $TEMP\spltmp.bmp
FunctionEnd

UninstallText "This will uninstall QuantLibXL. Hit next to continue."


Section "Uninstall"
    DeleteRegKey HKEY_LOCAL_MACHINE \
        "Software\Microsoft\Windows\CurrentVersion\Uninstall\QuantLibXL"
    DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\QuantLibXL"
    DeleteRegValue HKEY_CURRENT_USER  "Environment" "QLXL_DIR"
    Delete "$SMPROGRAMS\QuantLibXL\*.*"
    RMDir "$SMPROGRAMS\QuantLibXL"
    RMDir /r "$INSTDIR\Workbooks"
    RMDir /r "$INSTDIR\qlxl"
    RMDir /r "$INSTDIR\xll"
    RMDir /r "$INSTDIR"
SectionEnd
