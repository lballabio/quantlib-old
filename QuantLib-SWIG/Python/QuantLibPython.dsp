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
# PROP Output_Dir ".\build\temp.win32-2.2\Release"
# PROP Intermediate_Dir ".\build\temp.win32-2.2\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /Gi /GR /GX /Od /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "NOMINMAX" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" /libpath:"$(PPMT_DIR)\lib\Win32\VisualStudio\\"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files into .\build\lib.win32-2.2
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.2\QuantLib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\build\temp.win32-2.2\Debug"
# PROP Intermediate_Dir ".\build\temp.win32-2.2\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /Gi /GR /GX /ZI /Od /I "$(QL_DIR)" /D "WIN32" /D "NOMINMAX" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" /libpath:"$(PPMT_DIR)\lib\Win32\VisualStudio\\"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files into .\build\lib.win32-2.2
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.2\QuantLib
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
# PROP Output_Dir ".\build\temp.win32-2.2\OnTheEdgeRelease"
# PROP Intermediate_Dir ".\build\temp.win32-2.2\OnTheEdgeRelease"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /Od /I "..\QuantLib\Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /Gi /GR /GX /Od /I "..\..\QuantLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "NOMINMAX" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:"Release/QuantLibc.pyd" /libpath:"..\QuantLib\lib\Win32\VisualStudio"
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" /libpath:"..\..\QuantLib\lib\Win32\VisualStudio"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files into .\build\lib.win32-2.2
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.2\QuantLib
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
# PROP Output_Dir ".\build\temp.win32-2.2\OnTheEdgeDebug"
# PROP Intermediate_Dir ".\build\temp.win32-2.2\OnTheEdgeDebug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\QuantLib\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /Gi /GR /GX /ZI /Od /I "..\..\QuantLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "NOMINMAX" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"Debug/QuantLibc_d.pyd" /pdbtype:sept /libpath:"..\QuantLib\lib\Win32\VisualStudio"
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" /libpath:"..\..\QuantLib\lib\Win32\VisualStudio"
# SUBTRACT LINK32 /verbose /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files into .\build\lib.win32-2.2
PostBuild_Cmds=copy .\QuantLib\*.py .\build\lib.win32-2.2\QuantLib
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

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

# ADD CPP /W3

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\SWIG\blackmodel.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\calendars.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\capfloor.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\cashflows.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\common.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\currencies.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\date.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\daycounters.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\distributions.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\functions.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\history.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\indexes.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\instruments.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\interpolation.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\linearalgebra.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\marketelements.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\montecarlo.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\null.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\observer.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\old_pricers.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\old_volatility.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\operators.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\optimizers.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\options.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\piecewiseflatforward.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\ql.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\quantlib.i

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	
# Begin Custom Build
InputPath=..\SWIG\quantlib.i

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python setup.py wrap

# End Custom Build

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	
# Begin Custom Build
InputPath=..\SWIG\quantlib.i

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python setup.py wrap

# End Custom Build

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	
# Begin Custom Build
InputPath=..\SWIG\quantlib.i

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	which swig 
	swig -version 
	which python 
	python -V 
	python setup.py wrap 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	
# Begin Custom Build
InputPath=..\SWIG\quantlib.i

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	python setup.py wrap

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\SWIG\randomnumbers.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\riskstatistics.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\scheduler.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\segmentintegral.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\statistics.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\swap.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\swaption.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\termstructures.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\types.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\vectors.i
# End Source File
# Begin Source File

SOURCE=..\SWIG\volatilities.i
# End Source File
# End Group
# End Target
# End Project
