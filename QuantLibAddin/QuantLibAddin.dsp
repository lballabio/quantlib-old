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
!MESSAGE "QuantLibAddin - Win32 Release CRTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Debug CRTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Release SingleThread" (based on "Win32 (x86) Static Library")
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
# PROP BASE Output_Dir "build\vc6\Release"
# PROP BASE Intermediate_Dir "build\vc6\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "build\vc6\Release"
# PROP Intermediate_Dir "build\vc6\Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /Zm200 /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-s-0_3_13.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /Zm200 /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-sgd-0_3_13.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Release CRTDLL"

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
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /Zm200 /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-0_3_13.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Debug CRTDLL"

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
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /Zm200 /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-gd-0_3_13.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Release SingleThread"

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
# ADD CPP /nologo /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /Zm200 /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-s-0_3_13.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Debug SingleThread"

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
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /Zm200 /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-sgd-0_3_13.lib"

!ENDIF

# Begin Target

# Name "QuantLibAddin - Win32 Release"
# Name "QuantLibAddin - Win32 Debug"
# Name "QuantLibAddin - Win32 Release CRTDLL"
# Name "QuantLibAddin - Win32 Debug CRTDLL"
# Name "QuantLibAddin - Win32 Release SingleThread"
# Name "QuantLibAddin - Win32 Debug SingleThread"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\qla\asianoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\barrieroption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\basic.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\capfloor.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\clientutils.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\cliquetoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\complextyperegistry.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\couponvectors.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\dividendvanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\enumregistry.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\europeanoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\exercise.cpp
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

SOURCE=.\qla\interpolation.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\processes.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\quantoforwardvanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\quantovanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\schedule.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\shortratemodels.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\simpleswap.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\swap.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\termstructures.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\utilities.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vanillaoption.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_basic.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_capfloor.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_couponvectors.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_exercise.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_instruments.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_interpolation.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_options.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_processes.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_schedule.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_shortratemodels.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_simpleswap.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_swap.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_termstructures.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_utilities.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_volatilities.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_xibor.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\volatilities.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\xibor.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocouponbond.cpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocurve.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
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

SOURCE=.\qla\baseinstruments.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\basic.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\capfloor.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\clientutils.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\cliquetoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\config.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\couponvectors.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\dividendvanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\europeanoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\exercise.hpp
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

SOURCE=.\qla\interpolation.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\options.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\processes.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\qladdin.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\qladdindefines.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\quantoforwardvanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\quantovanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\schedule.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\shortratemodels.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\simpleswap.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\swap.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\termstructures.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\typefactory.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\typeregistry.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\utilities.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vanillaoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_basic.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_capfloor.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_couponvectors.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_exercise.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_instruments.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_interpolation.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_options.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_processes.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_schedule.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_shortratemodels.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_simpleswap.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_swap.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_termstructures.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_utilities.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_volatilities.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\vo_xibor.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\volatilities.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\xibor.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocouponbond.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\zerocurve.hpp
# End Source File
# End Group
# End Target
# End Project
