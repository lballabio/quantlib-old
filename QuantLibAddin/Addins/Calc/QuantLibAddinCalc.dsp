# Microsoft Developer Studio Project File - Name="QuantLibAddinCalc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=QuantLibAddinCalc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE
!MESSAGE NMAKE /f "QuantLibAddinCalc.mak".
!MESSAGE
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "QuantLibAddinCalc.mak" CFG="QuantLibAddinCalc - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "QuantLibAddinCalc - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "QuantLibAddinCalc - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "QuantLibAddinCalc - Win32 Release"

# PROP BASE Use_MFC
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f QuantLibAddinCalc.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "QuantLibAddinCalc.exe"
# PROP BASE Bsc_Name "QuantLibAddinCalc.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "WINDOWS SUPPORT PENDING"
# PROP Rebuild_Opt "/a"
# PROP Target_File "QuantLibAddin.lib"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "QuantLibAddinCalc - Win32 Debug"

# PROP BASE Use_MFC
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f QuantLibAddinCalc.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "QuantLibAddinCalc.exe"
# PROP BASE Bsc_Name "QuantLibAddinCalc.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "WINDOWS SUPPORT PENDING"
# PROP Rebuild_Opt "/a"
# PROP Target_File "QuantLibAddin.lib"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF

# Begin Target

# Name "QuantLibAddinCalc - Win32 Release"
# Name "QuantLibAddinCalc - Win32 Debug"

!IF  "$(CFG)" == "QuantLibAddinCalc - Win32 Release"

!ELSEIF  "$(CFG)" == "QuantLibAddinCalc - Win32 Debug"

!ENDIF

# Begin Source File

SOURCE=.\QuantLibAddinCalc.dsp
# End Source File
# Begin Source File

SOURCE=.\functions.cpp
# End Source File
# Begin Source File

SOURCE=.\Makefile
# End Source File
# Begin Source File

SOURCE=.\options.cpp
# End Source File
# Begin Source File

SOURCE=.\qladdin.cpp
# End Source File
# Begin Source File

SOURCE=.\qladdin.hpp
# End Source File
# Begin Source File

SOURCE=.\qldefs.hpp
# End Source File
# Begin Source File

SOURCE=.\QuantLibAddin.idl
# End Source File
# Begin Source File

SOURCE=.\readme.txt
# End Source File
# Begin Source File

SOURCE=.\utilities.cpp
# End Source File
# Begin Source File

SOURCE=.\utilities.hpp
# End Source File
# End Target
# End Project
