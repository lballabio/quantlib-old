# Microsoft Developer Studio Project File - Name="AddinC" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=AddinC - Win32 Debug SingleThread
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE
!MESSAGE NMAKE /f "AddinC.mak".
!MESSAGE
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "AddinC.mak" CFG="AddinC - Win32 Debug SingleThread"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "AddinC - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "AddinC - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "AddinC - Win32 Release CRTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "AddinC - Win32 Debug CRTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "AddinC - Win32 Release SingleThread" (based on "Win32 (x86) Static Library")
!MESSAGE "AddinC - Win32 Debug SingleThread" (based on "Win32 (x86) Static Library")
!MESSAGE

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "AddinC - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\AddinC-vc6-mt-s-0_3_12.lib"

!ELSEIF  "$(CFG)" == "AddinC - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\AddinC-vc6-mt-sgd-0_3_12.lib"

!ELSEIF  "$(CFG)" == "AddinC - Win32 Release CRTDLL"

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
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\AddinC-vc6-mt-0_3_12.lib"

!ELSEIF  "$(CFG)" == "AddinC - Win32 Debug CRTDLL"

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
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\AddinC-vc6-mt-gd-0_3_12.lib"

!ELSEIF  "$(CFG)" == "AddinC - Win32 Release SingleThread"

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
# ADD CPP /nologo /W3 /GR /GX /O2 /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\AddinC-vc6-s-0_3_12.lib"

!ELSEIF  "$(CFG)" == "AddinC - Win32 Debug SingleThread"

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
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "..\.." /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\AddinC-vc6-sgd-0_3_12.lib"

!ENDIF

# Begin Target

# Name "AddinC - Win32 Release"
# Name "AddinC - Win32 Debug"
# Name "AddinC - Win32 Release CRTDLL"
# Name "AddinC - Win32 Debug CRTDLL"
# Name "AddinC - Win32 Release SingleThread"
# Name "AddinC - Win32 Debug SingleThread"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\basic.cpp
# End Source File
# Begin Source File

SOURCE=capfloor.cpp
# End Source File
# Begin Source File

SOURCE=couponvectors.cpp
# End Source File
# Begin Source File

SOURCE=instruments.cpp
# End Source File
# Begin Source File

SOURCE=interpolation.cpp
# End Source File
# Begin Source File

SOURCE=ohfunctions.cpp
# End Source File
# Begin Source File

SOURCE=options.cpp
# End Source File
# Begin Source File

SOURCE=processes.cpp
# End Source File
# Begin Source File

SOURCE=schedule.cpp
# End Source File
# Begin Source File

SOURCE=session.cpp
# End Source File
# Begin Source File

SOURCE=shortratemodels.cpp
# End Source File
# Begin Source File

SOURCE=simpleswap.cpp
# End Source File
# Begin Source File

SOURCE=swap.cpp
# End Source File
# Begin Source File

SOURCE=termstructures.cpp
# End Source File
# Begin Source File

SOURCE=utilities.cpp
# End Source File
# Begin Source File

SOURCE=varies.cpp
# End Source File
# Begin Source File

SOURCE=volatilities.cpp
# End Source File
# Begin Source File

SOURCE=xibor.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\basic.h
# End Source File
# Begin Source File

SOURCE=capfloor.h
# End Source File
# Begin Source File

SOURCE=couponvectors.h
# End Source File
# Begin Source File

SOURCE=defines.h
# End Source File
# Begin Source File

SOURCE=instruments.h
# End Source File
# Begin Source File

SOURCE=interpolation.h
# End Source File
# Begin Source File

SOURCE=ohfunctions.h
# End Source File
# Begin Source File

SOURCE=options.h
# End Source File
# Begin Source File

SOURCE=processes.h
# End Source File
# Begin Source File

SOURCE=qladdin.h
# End Source File
# Begin Source File

SOURCE=schedule.h
# End Source File
# Begin Source File

SOURCE=shortratemodels.h
# End Source File
# Begin Source File

SOURCE=simpleswap.h
# End Source File
# Begin Source File

SOURCE=swap.h
# End Source File
# Begin Source File

SOURCE=termstructures.h
# End Source File
# Begin Source File

SOURCE=utilities.h
# End Source File
# Begin Source File

SOURCE=varies.h
# End Source File
# Begin Source File

SOURCE=varies.hpp
# End Source File
# Begin Source File

SOURCE=volatilities.h
# End Source File
# Begin Source File

SOURCE=xibor.h
# End Source File
# End Group
# End Target
# End Project
