# Microsoft Developer Studio Project File - Name="QuantLibAddin" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=QuantLibAddin - Win32 Debug MTDLL
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "QuantLibAddin.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "QuantLibAddin.mak" CFG="QuantLibAddin - Win32 Debug MTDLL"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "QuantLibAddin - Win32 Debug MTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE "QuantLibAddin - Win32 Release MTDLL" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "QuantLibAddin - Win32 Debug MTDLL"

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
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-gd-0_3_10.lib"

!ELSEIF  "$(CFG)" == "QuantLibAddin - Win32 Release MTDLL"

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
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "./" /I "$(OBJECT_HANDLER_DIR)" /I "$(QL_DIR)" /I "$(LOG4CXX_DIR)/include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"lib\QuantLibAddin-vc6-mt-0_3_10.lib"

!ENDIF 

# Begin Target

# Name "QuantLibAddin - Win32 Debug MTDLL"
# Name "QuantLibAddin - Win32 Release MTDLL"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
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

SOURCE=.\qla\capfloor.cpp
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

SOURCE=.\qla\basketoption.hpp
# End Source File
# Begin Source File

SOURCE=.\qla\capfloor.hpp
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
