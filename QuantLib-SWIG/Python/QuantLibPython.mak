# Microsoft Developer Studio Generated NMAKE File, Based on QuantLibPython.dsp
!IF "$(CFG)" == ""
CFG=QuantLibPython - Win32 OnTheEdgeDebug
!MESSAGE No configuration specified. Defaulting to QuantLibPython - Win32 OnTheEdgeDebug.
!ENDIF 

!IF "$(CFG)" != "QuantLibPython - Win32 Release" && "$(CFG)" != "QuantLibPython - Win32 Debug" && "$(CFG)" != "QuantLibPython - Win32 OnTheEdgeRelease" && "$(CFG)" != "QuantLibPython - Win32 OnTheEdgeDebug"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

OUTDIR=.\build\temp.win32-2.1\Release
INTDIR=.\build\temp.win32-2.1\Release
# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\Release
# End Custom Macros

ALL : ".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\QuantLibc.exp"
	-@erase "$(OUTDIR)\QuantLibc.lib"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gi /GR /GX /Od /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "NOMINMAX" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\QuantLibc.pdb" /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" /implib:"$(OUTDIR)\QuantLibc.lib" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

OUTDIR=.\build\temp.win32-2.1\Debug
INTDIR=.\build\temp.win32-2.1\Debug
# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\Debug
# End Custom Macros

ALL : ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\QuantLibc_d.exp"
	-@erase "$(OUTDIR)\QuantLibc_d.lib"
	-@erase "$(OUTDIR)\QuantLibc_d.pdb"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.ilk"
	-@erase ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /Gi /GR /GX /ZI /Od /I "$(QL_DIR)" /D "WIN32" /D "NOMINMAX" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "QL_DEBUG" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\QuantLibc_d.pdb" /debug /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" /implib:"$(OUTDIR)\QuantLibc_d.lib" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

OUTDIR=.\build\temp.win32-2.1\OnTheEdgeRelease
INTDIR=.\build\temp.win32-2.1\OnTheEdgeRelease
# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\OnTheEdgeRelease
# End Custom Macros

ALL : ".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\QuantLibc.exp"
	-@erase "$(OUTDIR)\QuantLibc.lib"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gi /GR /GX /Od /I "..\QuantLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "NOMINMAX" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\QuantLibc.pdb" /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" /implib:"$(OUTDIR)\QuantLibc.lib" /libpath:"..\QuantLib\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\OnTheEdgeRelease
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.1\QuantLib\QuantLibc.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

OUTDIR=.\build\temp.win32-2.1\OnTheEdgeDebug
INTDIR=.\build\temp.win32-2.1\OnTheEdgeDebug
# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\OnTheEdgeDebug
# End Custom Macros

ALL : ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\QuantLibc_d.exp"
	-@erase "$(OUTDIR)\QuantLibc_d.lib"
	-@erase "$(OUTDIR)\QuantLibc_d.pdb"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.ilk"
	-@erase ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /Gi /GR /GX /ZI /Od /I "..\QuantLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "NOMINMAX" /D "QL_DEBUG" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\QuantLibc_d.pdb" /debug /machine:I386 /out:".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" /implib:"$(OUTDIR)\QuantLibc_d.lib" /libpath:"..\QuantLib\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.1\OnTheEdgeDebug
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.1\QuantLib\QuantLibc_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("QuantLibPython.dep")
!INCLUDE "QuantLibPython.dep"
!ELSE 
!MESSAGE Warning: cannot find "QuantLibPython.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "QuantLibPython - Win32 Release" || "$(CFG)" == "QuantLibPython - Win32 Debug" || "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease" || "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"
SOURCE=".\QuantLib\quantlib_wrap.cpp"

"$(INTDIR)\quantlib_wrap.obj"	"$(INTDIR)\quantlib_wrap.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=".\QuantLib\SWIG\QuantLib.i"

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

InputPath=".\QuantLib\SWIG\QuantLib.i"
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python makewrappers.py
<< 
	

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

InputPath=".\QuantLib\SWIG\QuantLib.i"
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python makewrappers.py
<< 
	

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

InputPath=".\QuantLib\SWIG\QuantLib.i"
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python makewrappers.py
<< 
	

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

InputPath=".\QuantLib\SWIG\QuantLib.i"
USERDEP__QUANT=".\QuantLib\SWIG\Barrier.i"	".\QuantLib\SWIG\BoundaryConditions.i"	".\QuantLib\SWIG\Calendars.i"	".\QuantLib\SWIG\CashFlows.i"	".\QuantLib\SWIG\Currencies.i"	".\QuantLib\SWIG\Date.i"	".\QuantLib\SWIG\DayCounters.i"	".\QuantLib\SWIG\Distributions.i"	".\QuantLib\SWIG\FdPricers.i"	".\QuantLib\SWIG\Functions.i"	".\QuantLib\SWIG\History.i"	".\QuantLib\SWIG\Indexes.i"	".\QuantLib\SWIG\Instruments.i"	".\QuantLib\SWIG\Interpolation.i"	".\QuantLib\SWIG\MarketElements.i"	".\QuantLib\SWIG\Matrix.i"	".\QuantLib\SWIG\MontecarloPricers.i"	".\QuantLib\SWIG\MontecarloTools.i"	".\QuantLib\SWIG\MultiPath.i"	".\QuantLib\SWIG\Null.i"	".\QuantLib\SWIG\Observer.i"	".\QuantLib\SWIG\Operators.i"	".\QuantLib\SWIG\Options.i"	".\QuantLib\SWIG\Path.i"	".\QuantLib\SWIG\PiecewiseFlatForward.i"	".\QuantLib\SWIG\Pricers.i"	".\QuantLib\SWIG\QLArray.i"	".\QuantLib\SWIG\RandomNumbers.i"	".\QuantLib\SWIG\RateHelpers.i"	".\QuantLib\SWIG\RiskStatistics.i"	".\QuantLib\SWIG\Scheduler.i"	".\QuantLib\SWIG\SegmentIntegrals.i"	".\QuantLib\SWIG\Solvers1D.i"	".\QuantLib\SWIG\Statistics.i"	".\QuantLib\SWIG\String.i"	".\QuantLib\SWIG\Swap.i"	".\QuantLib\SWIG\TermStructures.i"	".\QuantLib\SWIG\Types.i"\
	".\QuantLib\SWIG\Vectors.i"	".\QuantLib\SWIG\Volatility.i"	".\QuantLib\SWIG\ql.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python makewrappers.py
<< 
	

!ENDIF 


!ENDIF 

