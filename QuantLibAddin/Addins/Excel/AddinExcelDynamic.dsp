# Microsoft Developer Studio Project File - Name="AddinExcelDynamic" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=AddinExcelDynamic - Win32 Release CRTDLL
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE
!MESSAGE NMAKE /f "AddinExcelDynamic.mak".
!MESSAGE
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "AddinExcelDynamic.mak" CFG="AddinExcelDynamic - Win32 Release CRTDLL"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "AddinExcelDynamic - Win32 Release CRTDLL" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "AddinExcelDynamic - Win32 Debug CRTDLL" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "AddinExcelDynamic - Win32 Release CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "buildDynamic\vc6\ReleaseCRTDLL"
# PROP BASE Intermediate_Dir "buildDynamic\vc6\ReleaseCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "buildDynamic\vc6\ReleaseCRTDLL"
# PROP Intermediate_Dir "buildDynamic\vc6\ReleaseCRTDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ADDINEXCELDYNAMIC_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "XLL_IMPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:"xll\QuantLibAddinDynamic-vc6-mt-0_3_13.xll" /libpath:"$(OBJECT_HANDLER_DIR)\xlsdk\lib" /libpath:"$(OBJECT_HANDLER_DIR)\ohxl\ohxll\xll" /libpath:"$(QL_DIR)\lib"

!ELSEIF  "$(CFG)" == "AddinExcelDynamic - Win32 Debug CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "buildDynamic\vc6\DebugCRTDLL"
# PROP BASE Intermediate_Dir "buildDynamic\vc6\DebugCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "buildDynamic\vc6\DebugCRTDLL"
# PROP Intermediate_Dir "buildDynamic\vc6\DebugCRTDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ADDINEXCELDYNAMIC_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "XLL_IMPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"xll\QuantLibAddinDynamic-vc6-mt-gd-0_3_13.xll" /pdbtype:sept /libpath:"$(OBJECT_HANDLER_DIR)\xlsdk\lib" /libpath:"$(OBJECT_HANDLER_DIR)\ohxl\ohxll\xll" /libpath:"$(QL_DIR)\lib"

!ENDIF

# Begin Target

# Name "AddinExcelDynamic - Win32 Release CRTDLL"
# Name "AddinExcelDynamic - Win32 Debug CRTDLL"
# Begin Source File

SOURCE=.\basic.cpp
# End Source File
# Begin Source File

SOURCE=.\capfloor.cpp
# End Source File
# Begin Source File

SOURCE=.\couponvectors.cpp
# End Source File
# Begin Source File

SOURCE=.\exercise.cpp
# End Source File
# Begin Source File

SOURCE=.\instruments.cpp
# End Source File
# Begin Source File

SOURCE=.\interpolation.cpp
# End Source File
# Begin Source File

SOURCE=.\options.cpp
# End Source File
# Begin Source File

SOURCE=.\processes.cpp
# End Source File
# Begin Source File

SOURCE=.\randomsequencegenerator.cpp
# End Source File
# Begin Source File

SOURCE=.\qladdin.cpp
# End Source File
# Begin Source File

SOURCE=.\schedule.cpp
# End Source File
# Begin Source File

SOURCE=.\session.cpp
# End Source File
# Begin Source File

SOURCE=.\session.hpp
# End Source File
# Begin Source File

SOURCE=.\shortratemodels.cpp
# End Source File
# Begin Source File

SOURCE=.\simpleswap.cpp
# End Source File
# Begin Source File

SOURCE=.\swap.cpp
# End Source File
# Begin Source File

SOURCE=.\termstructures.cpp
# End Source File
# Begin Source File

SOURCE=.\utilities.cpp
# End Source File
# Begin Source File

SOURCE=.\volatilities.cpp
# End Source File
# Begin Source File

SOURCE=.\xibor.cpp
# End Source File
# End Target
# End Project
