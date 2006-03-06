# Microsoft Developer Studio Project File - Name="ohxll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=ohxll - Win32 Release CRTDLL
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ohxll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ohxll.mak" CFG="ohxll - Win32 Release CRTDLL"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ohxll - Win32 Release CRTDLL" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ohxll - Win32 Debug CRTDLL" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ohxll - Win32 Release CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\ReleaseCRTDLL"
# PROP BASE Intermediate_Dir "build\vc6\ReleaseCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "xll"
# PROP Intermediate_Dir "build\vc6\ReleaseCRTDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "OHXLL_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_EXPORTS" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /machine:I386 /out:"xll\ObjectHandler-vc6-mt-0_1_3.xll" /libpath:"..\xlsdk\lib" /libpath:"$(LOG4CXX_DIR)\msvc\Lib"

!ELSEIF  "$(CFG)" == "ohxll - Win32 Debug CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\DebugCRTDLL"
# PROP BASE Intermediate_Dir "build\vc6\DebugCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "xll"
# PROP Intermediate_Dir "build\vc6\DebugCRTDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "OHXLL_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_EXPORTS" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /debug /machine:I386 /out:"xll\ObjectHandler-vc6-mt-gd-0_1_3.xll" /pdbtype:sept /libpath:"..\xlsdk\lib" /libpath:"$(LOG4CXX_DIR)\msvc\Lib"

!ENDIF 

# Begin Target

# Name "ohxll - Win32 Release CRTDLL"
# Name "ohxll - Win32 Debug CRTDLL"
# Begin Group "oh"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\oh\autolink.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\exception.cpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\exception.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\logger.cpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\logger.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\object.cpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\object.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\objecthandler.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\objecthandlerbase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\objecthandlerbase.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\objhandler.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\objhandlerdefines.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\property.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\singleton.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\utilities.cpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\utilities.hpp
# End Source File
# End Group
# Begin Group "xl"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\conversions.cpp
# End Source File
# Begin Source File

SOURCE=..\conversions.hpp
# End Source File
# Begin Source File

SOURCE=..\export.hpp
# End Source File
# Begin Source File

SOURCE=..\functions.cpp
# End Source File
# Begin Source File

SOURCE=..\objecthandlerxl.cpp
# End Source File
# Begin Source File

SOURCE=..\objecthandlerxl.hpp
# End Source File
# Begin Source File

SOURCE=..\register.cpp
# End Source File
# Begin Source File

SOURCE=..\register.hpp
# End Source File
# Begin Source File

SOURCE=..\unregister.cpp
# End Source File
# Begin Source File

SOURCE=..\unregister.hpp
# End Source File
# End Group
# Begin Group "xll"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\addin.cpp
# End Source File
# End Group
# End Target
# End Project
