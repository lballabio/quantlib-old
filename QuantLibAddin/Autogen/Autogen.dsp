# Microsoft Developer Studio Project File - Name="Autogen" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=Autogen - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Autogen.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Autogen.mak" CFG="Autogen - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Autogen - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "Autogen - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "Autogen - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f Autogen.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "Autogen.exe"
# PROP BASE Bsc_Name "Autogen.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "NMAKE /f Makefile.msdev"
# PROP Rebuild_Opt "/a"
# PROP Target_File "build\vc6\Autogen.flag1"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "Autogen - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f Autogen.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "Autogen.exe"
# PROP BASE Bsc_Name "Autogen.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "NMAKE /f Makefile.msdev"
# PROP Rebuild_Opt "/a"
# PROP Target_File "build\vc6\Autogen.flag1"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "Autogen - Win32 Release"
# Name "Autogen - Win32 Debug"

!IF  "$(CFG)" == "Autogen - Win32 Release"

!ELSEIF  "$(CFG)" == "Autogen - Win32 Debug"

!ENDIF 

# Begin Group "make"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Makefile.msdev
# End Source File
# End Group
# Begin Group "scripts"

# PROP Default_Filter "*.py"
# Begin Source File

SOURCE=.\autogen.py
# End Source File
# Begin Source File

SOURCE=.\c.py
# End Source File
# Begin Source File

SOURCE=.\calc.py
# End Source File
# Begin Source File

SOURCE=.\common.py
# End Source File
# Begin Source File

SOURCE=.\excel.py
# End Source File
# Begin Source File

SOURCE=.\parse.py
# End Source File
# Begin Source File

SOURCE=.\utils.py
# End Source File
# End Group
# Begin Group "stubs"

# PROP Default_Filter "stub.*"
# Begin Source File

SOURCE=.\stub.C.body
# End Source File
# Begin Source File

SOURCE=.\stub.C.includes
# End Source File
# Begin Source File

SOURCE=.\stub.Calc.body
# End Source File
# Begin Source File

SOURCE=.\stub.Calc.idlfoot
# End Source File
# Begin Source File

SOURCE=.\stub.Calc.idlfunc
# End Source File
# Begin Source File

SOURCE=.\stub.Calc.idlhead
# End Source File
# Begin Source File

SOURCE=.\stub.Calc.includes
# End Source File
# Begin Source File

SOURCE=.\stub.Calc.map
# End Source File
# Begin Source File

SOURCE=.\stub.copyright
# End Source File
# Begin Source File

SOURCE=.\stub.Excel.body
# End Source File
# Begin Source File

SOURCE=.\stub.Excel.includes
# End Source File
# Begin Source File

SOURCE=.\stub.Excel.regfooter
# End Source File
# Begin Source File

SOURCE=.\stub.Excel.regheader
# End Source File
# End Group
# Begin Group "metadata"

# PROP Default_Filter "*.xml"
# Begin Source File

SOURCE=.\instruments.xml
# End Source File
# Begin Source File

SOURCE=.\options.xml
# End Source File
# Begin Source File

SOURCE=.\utilities.xml
# End Source File
# End Group
# Begin Source File

SOURCE=.\README.txt
# End Source File
# End Target
# End Project
