# Microsoft Developer Studio Project File - Name="ohlib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=ohlib - Win32 Debug MTDLL
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ohlib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ohlib.mak" CFG="ohlib - Win32 Debug MTDLL"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ohlib - Win32 Debug MTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "ohlib - Win32 Release MTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ohlib - Win32 Debug MTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug MTDLL"
# PROP BASE Intermediate_Dir "Debug MTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugMTDLL"
# PROP Intermediate_Dir "build\vc6\DebugMTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "." /I "$(LOG4CXX_DIR)\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-mt-gd-0_1_1.lib"

!ELSEIF  "$(CFG)" == "ohlib - Win32 Release MTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release MTDLL"
# PROP BASE Intermediate_Dir "Release MTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseMTDLL"
# PROP Intermediate_Dir "build\vc6\ReleaseMTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "." /I "$(LOG4CXX_DIR)\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\ObjectHandler-vc6-mt-0_1_1.lib"

!ENDIF 

# Begin Target

# Name "ohlib - Win32 Debug MTDLL"
# Name "ohlib - Win32 Release MTDLL"
# Begin Source File

SOURCE=.\oh\autolink.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\exception.cpp
# End Source File
# Begin Source File

SOURCE=.\oh\exception.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\logger.cpp
# End Source File
# Begin Source File

SOURCE=.\oh\logger.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\object.cpp
# End Source File
# Begin Source File

SOURCE=.\oh\object.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\objecthandler.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\objecthandlerbase.cpp
# End Source File
# Begin Source File

SOURCE=.\oh\objecthandlerbase.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\objhandler.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\objhandlerdefines.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\property.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\singleton.hpp
# End Source File
# Begin Source File

SOURCE=.\oh\utilities.cpp
# End Source File
# Begin Source File

SOURCE=.\oh\utilities.hpp
# End Source File
# End Target
# End Project
