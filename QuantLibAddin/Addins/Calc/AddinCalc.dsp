# Microsoft Developer Studio Project File - Name="AddinCalc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=AddinCalc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "AddinCalc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "AddinCalc.mak" CFG="AddinCalc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "AddinCalc - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "AddinCalc - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "AddinCalc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f AddinCalc.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "AddinCalc.exe"
# PROP BASE Bsc_Name "AddinCalc.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "nmake /f Makefile.win32"
# PROP Rebuild_Opt "/a"
# PROP Target_File "QuantLibAddin.dll"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "AddinCalc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f AddinCalc.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "AddinCalc.exe"
# PROP BASE Bsc_Name "AddinCalc.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "nmake /f Makefile.win32.debug"
# PROP Rebuild_Opt "/a"
# PROP Target_File "QuantLibAddin.dll"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "AddinCalc - Win32 Release"
# Name "AddinCalc - Win32 Debug"

!IF  "$(CFG)" == "AddinCalc - Win32 Release"

!ELSEIF  "$(CFG)" == "AddinCalc - Win32 Debug"

!ENDIF 

# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\funcdef.cpp
# End Source File
# Begin Source File

SOURCE=.\functions.cpp
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\options.cpp
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\qladdin.cpp
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\QuantLibAddin.def
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\utilities.cpp
# PROP Intermediate_Dir "."
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\autogen.hpp
# End Source File
# Begin Source File

SOURCE=.\functions.hpp
# End Source File
# Begin Source File

SOURCE=.\options.hpp
# End Source File
# Begin Source File

SOURCE=.\qladdin.hpp
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\qldefs.hpp
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\utilities.hpp
# PROP Intermediate_Dir "."
# End Source File
# End Group
# Begin Group "Make"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Makefile.win32
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\Makefile.win32.debug
# PROP Intermediate_Dir "."
# End Source File
# End Group
# Begin Source File

SOURCE=.\QuantLibAddin.idl
# PROP Intermediate_Dir "."
# End Source File
# Begin Source File

SOURCE=.\readme.txt
# PROP Intermediate_Dir "."
# End Source File
# End Target
# End Project
