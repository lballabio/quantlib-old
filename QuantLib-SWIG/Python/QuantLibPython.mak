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

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

OUTDIR=.\build\temp.win32-2.2\Release
INTDIR=.\build\temp.win32-2.2\Release
# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\Release
# End Custom Macros

ALL : ".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\_QuantLib.exp"
	-@erase "$(OUTDIR)\_QuantLib.lib"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /Gi /GR /GX /Od /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "NOMINMAX" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\_QuantLib.pdb" /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" /implib:"$(OUTDIR)\_QuantLib.lib" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" /libpath:"$(PPMT_DIR)\lib\Win32\VisualStudio\\" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.2\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

OUTDIR=.\build\temp.win32-2.2\Debug
INTDIR=.\build\temp.win32-2.2\Debug
# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\Debug
# End Custom Macros

ALL : ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\_QuantLib_d.exp"
	-@erase "$(OUTDIR)\_QuantLib_d.lib"
	-@erase "$(OUTDIR)\_QuantLib_d.pdb"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.ilk"
	-@erase ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /Gi /GR /GX /ZI /Od /I "$(QL_DIR)" /D "WIN32" /D "NOMINMAX" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\_QuantLib_d.pdb" /debug /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" /implib:"$(OUTDIR)\_QuantLib_d.lib" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" /libpath:"$(PPMT_DIR)\lib\Win32\VisualStudio\\" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

OUTDIR=.\build\temp.win32-2.2\OnTheEdgeRelease
INTDIR=.\build\temp.win32-2.2\OnTheEdgeRelease
# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\OnTheEdgeRelease
# End Custom Macros

ALL : ".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\_QuantLib.exp"
	-@erase "$(OUTDIR)\_QuantLib.lib"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /Gi /GR /GX /Od /I "..\..\QuantLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "NOMINMAX" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\_QuantLib.pdb" /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" /implib:"$(OUTDIR)\_QuantLib.lib" /libpath:"..\..\QuantLib\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\OnTheEdgeRelease
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.2\QuantLib\_QuantLib.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

OUTDIR=.\build\temp.win32-2.2\OnTheEdgeDebug
INTDIR=.\build\temp.win32-2.2\OnTheEdgeDebug
# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\OnTheEdgeDebug
# End Custom Macros

ALL : ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"


CLEAN :
	-@erase "$(INTDIR)\quantlib_wrap.obj"
	-@erase "$(INTDIR)\quantlib_wrap.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\_QuantLib_d.exp"
	-@erase "$(OUTDIR)\_QuantLib_d.lib"
	-@erase "$(OUTDIR)\_QuantLib_d.pdb"
	-@erase "$(OUTDIR)\QuantLibPython.bsc"
	-@erase ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.ilk"
	-@erase ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /Gi /GR /GX /ZI /Od /I "..\..\QuantLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "NOMINMAX" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibPython.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\quantlib_wrap.sbr"

"$(OUTDIR)\QuantLibPython.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\_QuantLib_d.pdb" /debug /machine:I386 /out:".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" /implib:"$(OUTDIR)\_QuantLib_d.lib" /libpath:"..\..\QuantLib\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\quantlib_wrap.obj"

".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
PostBuild_Desc=copying QuantLib-Python files
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\build\temp.win32-2.2\OnTheEdgeDebug
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\build\lib.win32-2.2\QuantLib\_QuantLib_d.pyd" "$(OUTDIR)\QuantLibPython.bsc"
   copy .\QuantLib\*.py .\build\lib.win32-2.1\QuantLib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 

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


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("QuantLibPython.dep")
!INCLUDE "QuantLibPython.dep"
!ELSE 
!MESSAGE Warning: cannot find "QuantLibPython.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "QuantLibPython - Win32 Release" || "$(CFG)" == "QuantLibPython - Win32 Debug" || "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease" || "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"
SOURCE=".\QuantLib\quantlib_wrap.cpp"

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /Gi /GR /GX /Od /I "$(QL_DIR)" /D "WIN32" /D "NDEBUG" /D "NOMINMAX" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\quantlib_wrap.obj"	"$(INTDIR)\quantlib_wrap.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /Gi /GR /GX /ZI /Od /I "$(QL_DIR)" /D "WIN32" /D "NOMINMAX" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

"$(INTDIR)\quantlib_wrap.obj"	"$(INTDIR)\quantlib_wrap.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

CPP_SWITCHES=/nologo /MT /W3 /Gi /GR /GX /Od /I "..\..\QuantLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "NOMINMAX" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\quantlib_wrap.obj"	"$(INTDIR)\quantlib_wrap.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /Gi /GR /GX /ZI /Od /I "..\..\QuantLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "QUANTLIBPYTHON_EXPORTS" /D "NOMINMAX" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibPython.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

"$(INTDIR)\quantlib_wrap.obj"	"$(INTDIR)\quantlib_wrap.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\SWIG\quantlib.i

!IF  "$(CFG)" == "QuantLibPython - Win32 Release"

InputPath=..\SWIG\quantlib.i
USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python setup.py wrap
<< 
	

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 Debug"

InputPath=..\SWIG\quantlib.i
USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python setup.py wrap
<< 
	

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeRelease"

InputPath=..\SWIG\quantlib.i
USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	which swig 
	swig -version 
	which python 
	python -V 
	python setup.py wrap 
<< 
	

!ELSEIF  "$(CFG)" == "QuantLibPython - Win32 OnTheEdgeDebug"

InputPath=..\SWIG\quantlib.i
USERDEP__QUANT="..\SWIG\blackmodel.i"	"..\SWIG\calendars.i"	"..\SWIG\capfloor.i"	"..\SWIG\cashflows.i"	"..\SWIG\common.i"	"..\SWIG\currencies.i"	"..\SWIG\date.i"	"..\SWIG\daycounters.i"	"..\SWIG\distributions.i"	"..\SWIG\functions.i"	"..\SWIG\history.i"	"..\SWIG\indexes.i"	"..\SWIG\instruments.i"	"..\SWIG\interpolation.i"	"..\SWIG\linearalgebra.i"	"..\SWIG\marketelements.i"	"..\SWIG\montecarlo.i"	"..\SWIG\null.i"	"..\SWIG\observer.i"	"..\SWIG\old_pricers.i"	"..\SWIG\old_volatility.i"	"..\SWIG\operators.i"	"..\SWIG\optimizers.i"	"..\SWIG\options.i"	"..\SWIG\piecewiseflatforward.i"	"..\SWIG\ql.i"	"..\SWIG\randomnumbers.i"	"..\SWIG\riskstatistics.i"	"..\SWIG\scheduler.i"	"..\SWIG\segmentintegral.i"	"..\SWIG\statistics.i"	"..\SWIG\swap.i"	"..\SWIG\swaption.i"	"..\SWIG\termstructures.i"	"..\SWIG\types.i"	"..\SWIG\vectors.i"	"..\SWIG\volatilities.i"	

".\QuantLib\quantlib_wrap.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)" $(USERDEP__QUANT)
	<<tempfile.bat 
	@echo off 
	python setup.py wrap
<< 
	

!ENDIF 


!ENDIF 

