# Microsoft Developer Studio Project File - Name="QuantLibAddin" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=QuantLibAddin - Win32 Debug SingleThread
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "QuantLibAddin.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "QuantLibAddin.mak" CFG="QuantLibAddin - Win32 Debug SingleThread"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "QuantLibAddin - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Release MTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Release SingleThread" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Debug MTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Debug SingleThread" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "QuantLibAddin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\Release"
# PROP Intermediate_Dir "build\vc6\Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /FR /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-s-0_3_9.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\Debug"
# PROP Intermediate_Dir "build\vc6\Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-sgd-0_3_9.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Release MTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "QuantLibAddin___Win32_Release_MTDLL"
# PROP BASE Intermediate_Dir "QuantLibAddin___Win32_Release_MTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseMTDLL"
# PROP Intermediate_Dir "build\vc6\ReleaseMTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GR /GX /O2 /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo /out:"lib\QuantLibAddin.lib"
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-0_3_9.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Release SingleThread"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "QuantLibAddin___Win32_Release_SingleThread"
# PROP BASE Intermediate_Dir "QuantLibAddin___Win32_Release_SingleThread"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\ReleaseST"
# PROP Intermediate_Dir "build\vc6\ReleaseST"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GR /GX /O2 /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo /out:"lib\QuantLibAddin.lib"
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-s-0_3_9.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Debug MTDLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "QuantLibAddin___Win32_Debug_MTDLL"
# PROP BASE Intermediate_Dir "QuantLibAddin___Win32_Debug_MTDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugMTDLL"
# PROP Intermediate_Dir "build\vc6\DebugMTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /ML /W3 /Gm /GR /GX /ZI /Od /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo /out:"lib\QuantLibAddin_d.lib"
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-gd-0_3_9.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Debug SingleThread"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "QuantLibAddin___Win32_Debug_SingleThread"
# PROP BASE Intermediate_Dir "QuantLibAddin___Win32_Debug_SingleThread"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "build\vc6\DebugST"
# PROP Intermediate_Dir "build\vc6\DebugST"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /ML /W3 /Gm /GR /GX /ZI /Od /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo /out:"lib\QuantLibAddin_d.lib"
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-sgd-0_3_9.lib"

!ENDIF 

# Begin Target

# Name "QuantLibAddin - Win32 Release"
# Name "QuantLibAddin - Win32 Debug"
# Name "QuantLibAddin - Win32 Release MTDLL"
# Name "QuantLibAddin - Win32 Release SingleThread"
# Name "QuantLibAddin - Win32 Debug MTDLL"
# Name "QuantLibAddin - Win32 Debug SingleThread"
# Begin Group "Source Files"

# PROP Default_Filter "*.cpp"
# Begin Source File

SOURCE=.\qla\asianoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\barrieroption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\basketoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\cliquetoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\dividendvanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\fixedcouponbond.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\forwardvanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\generalutils.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\instrumentutils.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\optionutils.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\stochasticprocess.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\utilities.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocouponbond.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocurve.cpp
# End Source File
# End Group
# Begin Group "Header files"

# PROP Default_Filter "*.hpp"
# Begin Source File

SOURCE=.\qla\asianoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\autolink.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\barrieroption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\basketoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\cliquetoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\dividendvanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\fixedcouponbond.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\forwardvanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\generalutils.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\instruments.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\instrumentutils.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\options.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\optionutils.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\qladdin.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\qladdindefines.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\stochasticprocess.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\utilities.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocouponbond.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocurve.hpp
# End Source File
# End Group
# Begin Source File

SOURCE=.\Announce.txt
# End Source File
# Begin Source File

SOURCE=.\Authors.txt
# End Source File
# Begin Source File

SOURCE=.\Contributors.txt
# End Source File
# Begin Source File

SOURCE=.\LICENSE.TXT
# End Source File
# Begin Source File

SOURCE=.\NEWS.txt
# End Source File
# Begin Source File

SOURCE=.\README.txt
# End Source File
# Begin Source File

SOURCE=.\TODO.txt
# End Source File
# End Target
# End Project
