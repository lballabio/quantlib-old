# Microsoft Developer Studio Project File - Name="ohxllib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=ohxllib - Win32 Debug SingleThread
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE
!MESSAGE NMAKE /f "ohxllib.mak".
!MESSAGE
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "ohxllib.mak" CFG="ohxllib - Win32 Debug SingleThread"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "ohxllib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "ohxllib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "ohxllib - Win32 Release CRTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "ohxllib - Win32 Debug CRTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "ohxllib - Win32 Release SingleThread" (based on "Win32 (x86) Static Library")
!MESSAGE "ohxllib - Win32 Debug SingleThread" (based on "Win32 (x86) Static Library")
!MESSAGE

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ohxllib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\Release"
# PROP BASE Intermediate_Dir "build\vc6\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\Release"
# PROP Intermediate_Dir "build\vc6\Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "NDEBUG" /D "_LIB" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-mt-s-0_1_3.lib"

!ELSEIF  "$(CFG)" == "ohxllib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\Debug"
# PROP BASE Intermediate_Dir "build\vc6\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\Debug"
# PROP Intermediate_Dir "build\vc6\Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "_DEBUG" /D "_LIB" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-mt-sgd-0_1_3.lib"

!ELSEIF  "$(CFG)" == "ohxllib - Win32 Release CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\ReleaseCRTDLL"
# PROP BASE Intermediate_Dir "build\vc6\ReleaseCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseCRTDLL"
# PROP Intermediate_Dir "build\vc6\ReleaseCRTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "NDEBUG" /D "_LIB" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-mt-0_1_3.lib"

!ELSEIF  "$(CFG)" == "ohxllib - Win32 Debug CRTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\DebugCRTDLL"
# PROP BASE Intermediate_Dir "build\vc6\DebugCRTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugCRTDLL"
# PROP Intermediate_Dir "build\vc6\DebugCRTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "_DEBUG" /D "_LIB" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-mt-gd-0_1_3.lib"

!ELSEIF  "$(CFG)" == "ohxllib - Win32 Release SingleThread"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "build\vc6\ReleaseST"
# PROP BASE Intermediate_Dir "build\vc6\ReleaseST"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseST"
# PROP Intermediate_Dir "build\vc6\ReleaseST"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GR /GX /O2 /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "NDEBUG" /D "_LIB" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-s-0_1_3.lib"

!ELSEIF  "$(CFG)" == "ohxllib - Win32 Debug SingleThread"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "build\vc6\DebugST"
# PROP BASE Intermediate_Dir "build\vc6\DebugST"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugST"
# PROP Intermediate_Dir "build\vc6\DebugST"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(LOG4CXX_DIR)\include" /D "_DEBUG" /D "_LIB" /D "XLL_STATIC" /D "WIN32" /D "_MBCS" /D "LOG4CXX_STATIC" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-sgd-0_1_3.lib"

!ENDIF

# Begin Target

# Name "ohxllib - Win32 Release"
# Name "ohxllib - Win32 Debug"
# Name "ohxllib - Win32 Release CRTDLL"
# Name "ohxllib - Win32 Debug CRTDLL"
# Name "ohxllib - Win32 Release SingleThread"
# Name "ohxllib - Win32 Debug SingleThread"
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

SOURCE=..\..\oh\singleton.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\utilities.cpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\utilities.hpp
# End Source File
# Begin Source File

SOURCE=..\..\oh\valueobject.hpp
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
# End Target
# End Project
