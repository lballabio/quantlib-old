# Microsoft Developer Studio Project File - Name="testsuite" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=testsuite - Win32 Unicode Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "testsuite.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "testsuite.mak" CFG="testsuite - Win32 Unicode Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "testsuite - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "testsuite - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "testsuite - Win32 Unicode Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "testsuite - Win32 Unicode Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "testsuite - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /Zi /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x40c /d "NDEBUG"
# ADD RSC /l 0x40c /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 log4cxx.lib boost_regex_vc6_mdi.lib cppunit.lib /nologo /subsystem:console /debug /machine:I386 /out:"../Bin/Release/testsuite.exe"

!ELSEIF  "$(CFG)" == "testsuite - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x40c /d "_DEBUG"
# ADD RSC /l 0x40c /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 log4cxx.lib boost_regex_vc6_mdid.lib cppunitd.lib /nologo /subsystem:console /debug /machine:I386 /out:"../Bin/Debug/testsuite.exe" /pdbtype:sept

!ELSEIF  "$(CFG)" == "testsuite - Win32 Unicode Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "testsuite___Win32_Unicode_Debug"
# PROP BASE Intermediate_Dir "testsuite___Win32_Unicode_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Unicode_D"
# PROP Intermediate_Dir "Unicode_D"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D "UNICODE" /YX /FD /GZ /c
# ADD BASE RSC /l 0x40c /d "_DEBUG"
# ADD RSC /l 0x40c /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 log4cxx.lib boost_regex_vc6_mdid.lib cppunitd.lib /nologo /subsystem:console /debug /machine:I386 /out:"../Bin/Debug/testsuite.exe" /pdbtype:sept
# ADD LINK32 log4cxx.lib boost_regex_vc6_mdid.lib cppunitd.lib /nologo /subsystem:console /debug /machine:I386 /out:"../Bin/Unicode_D/testsuite.exe" /pdbtype:sept

!ELSEIF  "$(CFG)" == "testsuite - Win32 Unicode Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "testsuite___Win32_Unicode_Release"
# PROP BASE Intermediate_Dir "testsuite___Win32_Unicode_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Unicode_R"
# PROP Intermediate_Dir "Unicode_R"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /Zi /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "UNICODE" /YX /FD /c
# ADD BASE RSC /l 0x40c /d "NDEBUG"
# ADD RSC /l 0x40c /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 log4cxx.lib boost_regex_vc6_mdi.lib cppunit.lib /nologo /subsystem:console /debug /machine:I386 /out:"../Bin/Unicode_R/testsuite.exe"

!ENDIF 

# Begin Target

# Name "testsuite - Win32 Release"
# Name "testsuite - Win32 Debug"
# Name "testsuite - Win32 Unicode Debug"
# Name "testsuite - Win32 Unicode Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\tests\src\util\absolutedateandtimefilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\absolutetimefilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\asyncappendertestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\helpers\boundedfifotestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\compare.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\controlfilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\xml\customleveltestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\helpers\cyclicbuffertestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\xml\domtestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\drfatestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\filter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\hierarchythresholdtestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\iso8601filter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\l7dtestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\linenumberfilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\loggertestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\main.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\minimumtestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\helpers\optionconvertertestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\patternlayouttest.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\relativetimefilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\net\socketservertestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\defaultinit\testcase1.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\defaultinit\testcase2.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\defaultinit\testcase3.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\defaultinit\testcase4.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\threadfilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\transformer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\vectorappender.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\xml\xlevel.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\customlogger\xlogger.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\customlogger\xloggertestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\xml\xmllayouttestcase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\xmllineattributefilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\xmlthreadfilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\xmltimestampfilter.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\tests\src\util\absolutedateandtimefilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\absolutetimefilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\compare.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\controlfilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\filter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\iso8601filter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\linenumberfilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\relativetimefilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\net\socketservertestcase.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\threadfilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\transformer.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\vectorappender.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\xml\xlevel.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\customlogger\xlogger.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\xmllineattributefilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\xmlthreadfilter.h
# End Source File
# Begin Source File

SOURCE=..\..\tests\src\util\xmltimestampfilter.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
