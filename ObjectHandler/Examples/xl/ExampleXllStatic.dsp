# Microsoft Developer Studio Project File - Name="ExampleXllStatic" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=ExampleXllStatic - Win32 Debug SingleThread
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ExampleXllStatic.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ExampleXllStatic.mak" CFG="ExampleXllStatic - Win32 Debug SingleThread"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ExampleXllStatic - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ExampleXllStatic - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ExampleXllStatic - Win32 Release CRTDLL" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ExampleXllStatic - Win32 Debug CRTDLL" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ExampleXllStatic - Win32 Release SingleThread" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ExampleXllStatic - Win32 Debug SingleThread" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ExampleXllStatic - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\Release"
# PROP BASE Intermediate_Dir "build\vc6\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\Release"
# PROP Intermediate_Dir "build\vc6\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EXAMPLEXLLSTATIC_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "." /I "..\.." /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /machine:I386 /out:"xll\ExampleXLLStatic-vc6-mt-s-0_1_4.xll" /libpath:"..\..\xlsdk\lib" /libpath:"..\..\ohxl\ohxllib\lib" /libpath:"$(LOG4CXX_DIR)/msvc/Lib"

!ELSEIF  "$(CFG)" == "ExampleXllStatic - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\Debug"
# PROP BASE Intermediate_Dir "build\vc6\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\Debug"
# PROP Intermediate_Dir "build\vc6\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EXAMPLEXLLSTATIC_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "." /I "..\.." /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /debug /machine:I386 /out:"xll\ExampleXLLStatic-vc6-mt-sgd-0_1_4.xll" /pdbtype:sept /libpath:"..\..\xlsdk\lib" /libpath:"..\..\ohxl\ohxllib\lib" /libpath:"$(LOG4CXX_DIR)/msvc/Lib"

!ELSEIF  "$(CFG)" == "ExampleXllStatic - Win32 Release CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\ReleaseCRTDLL"
# PROP BASE Intermediate_Dir "build\vc6\ReleaseCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseCRTDLL"
# PROP Intermediate_Dir "build\vc6\ReleaseCRTDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EXAMPLEXLLSTATIC_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "." /I "..\.." /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /machine:I386 /out:"xll\ExampleXLLStatic-vc6-mt-0_1_4.xll" /libpath:"..\..\xlsdk\lib" /libpath:"..\..\ohxl\ohxllib\lib" /libpath:"$(LOG4CXX_DIR)/msvc/Lib"

!ELSEIF  "$(CFG)" == "ExampleXllStatic - Win32 Debug CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\DebugCRTDLL"
# PROP BASE Intermediate_Dir "build\vc6\DebugCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugCRTDLL"
# PROP Intermediate_Dir "build\vc6\DebugCRTDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EXAMPLEXLLSTATIC_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "." /I "..\.." /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /debug /machine:I386 /out:"xll\ExampleXLLStatic-vc6-mt-gd-0_1_4.xll" /pdbtype:sept /libpath:"..\..\xlsdk\lib" /libpath:"..\..\ohxl\ohxllib\lib" /libpath:"$(LOG4CXX_DIR)/msvc/Lib"

!ELSEIF  "$(CFG)" == "ExampleXllStatic - Win32 Release SingleThread"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\ReleaseST"
# PROP BASE Intermediate_Dir "build\vc6\ReleaseST"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseST"
# PROP Intermediate_Dir "build\vc6\ReleaseST"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EXAMPLEXLLSTATIC_EXPORTS" /YX /FD /c
# ADD CPP /nologo /W3 /GR /GX /O2 /I "." /I "..\.." /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /machine:I386 /out:"xll\ExampleXLLStatic-vc6-s-0_1_4.xll" /libpath:"..\..\xlsdk\lib" /libpath:"..\..\ohxl\ohxllib\lib" /libpath:"$(LOG4CXX_DIR)/msvc/Lib"

!ELSEIF  "$(CFG)" == "ExampleXllStatic - Win32 Debug SingleThread"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\DebugST"
# PROP BASE Intermediate_Dir "build\vc6\DebugST"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugST"
# PROP Intermediate_Dir "build\vc6\DebugST"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EXAMPLEXLLSTATIC_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "." /I "..\.." /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /dll /debug /machine:I386 /out:"xll\ExampleXLLStatic-vc6-sgd-0_1_4.xll" /pdbtype:sept /libpath:"..\..\xlsdk\lib" /libpath:"..\..\ohxl\ohxllib\lib" /libpath:"$(LOG4CXX_DIR)/msvc/Lib"

!ENDIF 

# Begin Target

# Name "ExampleXllStatic - Win32 Release"
# Name "ExampleXllStatic - Win32 Debug"
# Name "ExampleXllStatic - Win32 Release CRTDLL"
# Name "ExampleXllStatic - Win32 Debug CRTDLL"
# Name "ExampleXllStatic - Win32 Release SingleThread"
# Name "ExampleXllStatic - Win32 Debug SingleThread"
# Begin Source File

SOURCE=.\addinstatic.cpp
# End Source File
# Begin Source File

SOURCE=.\car.cpp
# End Source File
# Begin Source File

SOURCE=.\car.hpp
# End Source File
# End Target
# End Project
