# Microsoft Developer Studio Project File - Name="QuantLibPython" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=QuantLibPython - Win32 OnTheEdgeDebug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE
!MESSAGE NMAKE /f "QuantLibPython.mak".
!MESSAGE
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "QuantLibPython.mak" CFG="QuantLibPython - Win32 OnTheEdgeDebug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "QuantLibPython - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "QuantLibPython - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "QuantLibPython - Win32 OnTheEdgeRelease" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "QuantLibPython - Win32 OnTheEdgeDebug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\build\temp.win32-2.1\Release"
# PROP Intermediate_Dir ".\build\temp.win32-2.1\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /Gi /GR /GX /Od /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "NOMINMAX" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\build\temp.win32-2.1\Debug"
# PROP Intermediate_Dir ".\build\temp.win32-2.1\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /Gi /GR /GX /ZI /Od /I "$(QL_DIR)" /D "WIN32" /D "NOMINMAX" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "QL_DEBUG" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "OnTheEdgeRelease"
# PROP BASE Intermediate_Dir "OnTheEdgeRelease"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\build\temp.win32-2.1\OnTheEdgeRelease"
# PROP Intermediate_Dir ".\build\temp.win32-2.1\OnTheEdgeRelease"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /Od /I "..\QuantLib\Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /Gi /GR /GX /Od /I "..\QuantLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "NOMINMAX" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:"Release/QuantLibc.pyd" /libpath:"..\QuantLib\lib\Win32\VisualStudio"
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" /libpath:"..\QuantLib\lib\Win32\VisualStudio"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "OnTheEdgeDebug"
# PROP BASE Intermediate_Dir "OnTheEdgeDebug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\build\temp.win32-2.1\OnTheEdgeDebug"
# PROP Intermediate_Dir ".\build\temp.win32-2.1\OnTheEdgeDebug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\QuantLib\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /Gi /GR /GX /ZI /Od /I "..\QuantLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "NOMINMAX" /D "QL_DEBUG" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"Debug/QuantLibc_d.pyd" /pdbtype:sept /libpath:"..\QuantLib\lib\Win32\VisualStudio"
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" /libpath:"..\QuantLib\lib\Win32\VisualStudio"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
# End Special Build Tool

!ENDIF

# Begin Target

# Name "QuantLibPython - Win32 Release"
# Name "QuantLibPython - Win32 Debug"
# Name "QuantLibPython - Win32 OnTheEdgeRelease"
# Name "QuantLibPython - Win32 OnTheEdgeDebug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=".\QuantLib\QuantLib.py"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\quantlib_wrap.cpp"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=".\QuantLib\SWIG\Barrier.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\BoundaryConditions.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Calendars.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\CashFlows.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Currencies.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Date.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\DayCounters.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Distributions.i"
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\FdPricers.i
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\Functions.i
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\History.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Indexes.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Instruments.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Interpolation.i"
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\MarketElements.i
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Matrix.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\MontecarloPricers.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\MontecarloTools.i"
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\MultiPath.i
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Null.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Observer.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Operators.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Options.i"
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\Path.i
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\PiecewiseFlatForward.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Pricers.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\ql.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\QLArray.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\QuantLib.i"

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

# PROP Ignore_Default_Tool 1
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"
# Begin Custom Build - Compiling QuantLib.i ...
InputPath=".\QuantLib\SWIG\QuantLib.i"

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python makewrappers.py

# End Custom Build

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

# PROP Ignore_Default_Tool 1
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"
# Begin Custom Build - Compiling QuantLib.i ...
InputPath=".\QuantLib\SWIG\QuantLib.i"

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python makewrappers.py

# End Custom Build

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

# PROP Ignore_Default_Tool 1
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"
# Begin Custom Build - Compiling QuantLib.i ...
InputPath=".\QuantLib\SWIG\QuantLib.i"

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python makewrappers.py

# End Custom Build

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

# PROP Ignore_Default_Tool 1
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"
# Begin Custom Build - Compiling QuantLib.i ...
InputPath=".\QuantLib\SWIG\QuantLib.i"

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python makewrappers.py

# End Custom Build

!ENDIF

# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\RandomNumbers.i
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\RateHelpers.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\RiskStatistics.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Scheduler.i"
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\SegmentIntegrals.i
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Solvers1D.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Statistics.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\String.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Swap.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\TermStructures.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Types.i"
# End Source File
# Begin Source File

SOURCE=".\QuantLib\SWIG\Vectors.i"
# End Source File
# Begin Source File

SOURCE=.\QuantLib\SWIG\Volatility.i
# End Source File
# End Group
# End Target
# End Project
