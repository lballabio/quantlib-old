# Microsoft Developer Studio Generated NMAKE File, Based on QuantLibXL.dsp
!IF "$(CFG)" == ""
CFG=QuantLibXL - Win32 OnTheEdgeDebug
!MESSAGE No configuration specified. Defaulting to QuantLibXL - Win32 OnTheEdgeDebug.
!ENDIF 

!IF "$(CFG)" != "QuantLibXL - Win32 Release" && "$(CFG)" != "QuantLibXL - Win32 Debug" && "$(CFG)" != "QuantLibXL - Win32 OnTheEdgeRelease" && "$(CFG)" != "QuantLibXL - Win32 OnTheEdgeDebug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "QuantLibXL.mak" CFG="QuantLibXL - Win32 OnTheEdgeDebug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "QuantLibXL - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "QuantLibXL - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "QuantLibXL - Win32 OnTheEdgeRelease" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "QuantLibXL - Win32 OnTheEdgeDebug" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "QuantLibXL - Win32 Release"

OUTDIR=.\build\Release
INTDIR=.\build\Release
# Begin Custom Macros
OutDir=.\build\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : ".\xll\Win32\VisualStudio\QuantLibXL.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ELSE 

ALL : "xlw - Win32 Release" "QuantLib - Win32 Release" ".\xll\Win32\VisualStudio\QuantLibXL.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"QuantLib - Win32 ReleaseCLEAN" "xlw - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\datef.obj"
	-@erase "$(INTDIR)\datef.sbr"
	-@erase "$(INTDIR)\mathf.obj"
	-@erase "$(INTDIR)\mathf.sbr"
	-@erase "$(INTDIR)\montecarlo.obj"
	-@erase "$(INTDIR)\montecarlo.sbr"
	-@erase "$(INTDIR)\pricers.obj"
	-@erase "$(INTDIR)\pricers.sbr"
	-@erase "$(INTDIR)\utilities.obj"
	-@erase "$(INTDIR)\utilities.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vols.obj"
	-@erase "$(INTDIR)\vols.sbr"
	-@erase "$(INTDIR)\xlAutoOpen.obj"
	-@erase "$(INTDIR)\xlAutoOpen.sbr"
	-@erase "$(OUTDIR)\QuantLibXL.bsc"
	-@erase "$(OUTDIR)\QuantLibXL.exp"
	-@erase "$(OUTDIR)\QuantLibXL.lib"
	-@erase ".\xll\Win32\VisualStudio\QuantLibXL.xll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GR /GX /O2 /I ".\\" /I "$(XLW_DIR)" /I "$(QL_DIR)" /D "NDEBUG" /D "NOMINMAX" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibXL.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibXL.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\datef.sbr" \
	"$(INTDIR)\mathf.sbr" \
	"$(INTDIR)\montecarlo.sbr" \
	"$(INTDIR)\pricers.sbr" \
	"$(INTDIR)\utilities.sbr" \
	"$(INTDIR)\xlAutoOpen.sbr" \
	"$(INTDIR)\vols.sbr"

"$(OUTDIR)\QuantLibXL.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\QuantLibXL.pdb" /machine:I386 /out:"xll\Win32\VisualStudio\QuantLibXL.xll" /implib:"$(OUTDIR)\QuantLibXL.lib" /libpath:"$(XLW_DIR)\lib\Win32\VisualStudio" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\datef.obj" \
	"$(INTDIR)\mathf.obj" \
	"$(INTDIR)\montecarlo.obj" \
	"$(INTDIR)\pricers.obj" \
	"$(INTDIR)\utilities.obj" \
	"$(INTDIR)\xlAutoOpen.obj" \
	"$(INTDIR)\vols.obj" \
	"..\QuantLib\lib\Win32\VisualStudio\QuantLib.lib" \
	"..\XLW\lib\Win32\VisualStudio\xlw.lib"

".\xll\Win32\VisualStudio\QuantLibXL.xll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 Debug"

OUTDIR=.\build\Debug
INTDIR=.\build\Debug
# Begin Custom Macros
OutDir=.\build\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : ".\xll\Win32\VisualStudio\QuantLibXL_d.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ELSE 

ALL : "xlw - Win32 Debug" "QuantLib - Win32 Debug" ".\xll\Win32\VisualStudio\QuantLibXL_d.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"QuantLib - Win32 DebugCLEAN" "xlw - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\datef.obj"
	-@erase "$(INTDIR)\datef.sbr"
	-@erase "$(INTDIR)\mathf.obj"
	-@erase "$(INTDIR)\mathf.sbr"
	-@erase "$(INTDIR)\montecarlo.obj"
	-@erase "$(INTDIR)\montecarlo.sbr"
	-@erase "$(INTDIR)\pricers.obj"
	-@erase "$(INTDIR)\pricers.sbr"
	-@erase "$(INTDIR)\utilities.obj"
	-@erase "$(INTDIR)\utilities.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\vols.obj"
	-@erase "$(INTDIR)\vols.sbr"
	-@erase "$(INTDIR)\xlAutoOpen.obj"
	-@erase "$(INTDIR)\xlAutoOpen.sbr"
	-@erase "$(OUTDIR)\QuantLibXL.bsc"
	-@erase "$(OUTDIR)\QuantLibXL_d.exp"
	-@erase "$(OUTDIR)\QuantLibXL_d.lib"
	-@erase "$(OUTDIR)\QuantLibXL_d.pdb"
	-@erase ".\xll\Win32\VisualStudio\QuantLibXL_d.ilk"
	-@erase ".\xll\Win32\VisualStudio\QuantLibXL_d.xll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I ".\\" /I "$(XLW_DIR)" /I "$(QL_DIR)" /D "QL_DEBUG" /D "_DEBUG" /D "XLW_DEBUG" /D "NOMINMAX" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibXL.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibXL.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\datef.sbr" \
	"$(INTDIR)\mathf.sbr" \
	"$(INTDIR)\montecarlo.sbr" \
	"$(INTDIR)\pricers.sbr" \
	"$(INTDIR)\utilities.sbr" \
	"$(INTDIR)\xlAutoOpen.sbr" \
	"$(INTDIR)\vols.sbr"

"$(OUTDIR)\QuantLibXL.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\QuantLibXL_d.pdb" /debug /machine:I386 /out:"xll\Win32\VisualStudio\QuantLibXL_d.xll" /implib:"$(OUTDIR)\QuantLibXL_d.lib" /pdbtype:sept /libpath:"$(XLW_DIR)\lib\Win32\VisualStudio" /libpath:"$(QL_DIR)\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\datef.obj" \
	"$(INTDIR)\mathf.obj" \
	"$(INTDIR)\montecarlo.obj" \
	"$(INTDIR)\pricers.obj" \
	"$(INTDIR)\utilities.obj" \
	"$(INTDIR)\xlAutoOpen.obj" \
	"$(INTDIR)\vols.obj" \
	"..\QuantLib\lib\Win32\VisualStudio\QuantLib_d.lib" \
	"..\XLW\lib\Win32\VisualStudio\xlwd.lib"

".\xll\Win32\VisualStudio\QuantLibXL_d.xll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeRelease"

OUTDIR=.\build\OnTheEdgeRelease
INTDIR=.\build\OnTheEdgeRelease
# Begin Custom Macros
OutDir=.\build\OnTheEdgeRelease
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : ".\xll\Win32\VisualStudio\QuantLibXL.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ELSE 

ALL : "xlw - Win32 OnTheEdgeRelease" "QuantLib - Win32 OnTheEdgeRelease" ".\xll\Win32\VisualStudio\QuantLibXL.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"QuantLib - Win32 OnTheEdgeReleaseCLEAN" "xlw - Win32 OnTheEdgeReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\datef.obj"
	-@erase "$(INTDIR)\datef.sbr"
	-@erase "$(INTDIR)\mathf.obj"
	-@erase "$(INTDIR)\mathf.sbr"
	-@erase "$(INTDIR)\montecarlo.obj"
	-@erase "$(INTDIR)\montecarlo.sbr"
	-@erase "$(INTDIR)\pricers.obj"
	-@erase "$(INTDIR)\pricers.sbr"
	-@erase "$(INTDIR)\utilities.obj"
	-@erase "$(INTDIR)\utilities.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vols.obj"
	-@erase "$(INTDIR)\vols.sbr"
	-@erase "$(INTDIR)\xlAutoOpen.obj"
	-@erase "$(INTDIR)\xlAutoOpen.sbr"
	-@erase "$(OUTDIR)\QuantLibXL.bsc"
	-@erase "$(OUTDIR)\QuantLibXL.exp"
	-@erase "$(OUTDIR)\QuantLibXL.lib"
	-@erase ".\xll\Win32\VisualStudio\QuantLibXL.xll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GR /GX /O2 /I ".\\" /I "..\XLW" /I "..\QuantLib" /D "NDEBUG" /D "NOMINMAX" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibXL.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibXL.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\datef.sbr" \
	"$(INTDIR)\mathf.sbr" \
	"$(INTDIR)\montecarlo.sbr" \
	"$(INTDIR)\pricers.sbr" \
	"$(INTDIR)\utilities.sbr" \
	"$(INTDIR)\xlAutoOpen.sbr" \
	"$(INTDIR)\vols.sbr"

"$(OUTDIR)\QuantLibXL.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\QuantLibXL.pdb" /machine:I386 /out:"xll\Win32\VisualStudio\QuantLibXL.xll" /implib:"$(OUTDIR)\QuantLibXL.lib" /libpath:"..\XLW\lib\Win32\VisualStudio" /libpath:"..\QuantLib\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\datef.obj" \
	"$(INTDIR)\mathf.obj" \
	"$(INTDIR)\montecarlo.obj" \
	"$(INTDIR)\pricers.obj" \
	"$(INTDIR)\utilities.obj" \
	"$(INTDIR)\xlAutoOpen.obj" \
	"$(INTDIR)\vols.obj" \
	"..\QuantLib\lib\Win32\VisualStudio\QuantLib.lib" \
	"..\XLW\lib\Win32\VisualStudio\xlw.lib"

".\xll\Win32\VisualStudio\QuantLibXL.xll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeDebug"

OUTDIR=.\build\OnTheEdgeDebug
INTDIR=.\build\OnTheEdgeDebug
# Begin Custom Macros
OutDir=.\build\OnTheEdgeDebug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : ".\xll\Win32\VisualStudio\QuantLibXL_d.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ELSE 

ALL : "xlw - Win32 OnTheEdgeDebug" "QuantLib - Win32 OnTheEdgeDebug" ".\xll\Win32\VisualStudio\QuantLibXL_d.xll" "$(OUTDIR)\QuantLibXL.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"QuantLib - Win32 OnTheEdgeDebugCLEAN" "xlw - Win32 OnTheEdgeDebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\datef.obj"
	-@erase "$(INTDIR)\datef.sbr"
	-@erase "$(INTDIR)\mathf.obj"
	-@erase "$(INTDIR)\mathf.sbr"
	-@erase "$(INTDIR)\montecarlo.obj"
	-@erase "$(INTDIR)\montecarlo.sbr"
	-@erase "$(INTDIR)\pricers.obj"
	-@erase "$(INTDIR)\pricers.sbr"
	-@erase "$(INTDIR)\utilities.obj"
	-@erase "$(INTDIR)\utilities.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\vols.obj"
	-@erase "$(INTDIR)\vols.sbr"
	-@erase "$(INTDIR)\xlAutoOpen.obj"
	-@erase "$(INTDIR)\xlAutoOpen.sbr"
	-@erase "$(OUTDIR)\QuantLibXL.bsc"
	-@erase "$(OUTDIR)\QuantLibXL_d.exp"
	-@erase "$(OUTDIR)\QuantLibXL_d.lib"
	-@erase "$(OUTDIR)\QuantLibXL_d.pdb"
	-@erase ".\xll\Win32\VisualStudio\QuantLibXL_d.ilk"
	-@erase ".\xll\Win32\VisualStudio\QuantLibXL_d.xll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GR /GX /ZI /Od /I ".\\" /I "..\XLW" /I "..\QuantLib" /D "QL_DEBUG" /D "_DEBUG" /D "XLW_DEBUG" /D "NOMINMAX" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\QuantLibXL.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\QuantLibXL.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\datef.sbr" \
	"$(INTDIR)\mathf.sbr" \
	"$(INTDIR)\montecarlo.sbr" \
	"$(INTDIR)\pricers.sbr" \
	"$(INTDIR)\utilities.sbr" \
	"$(INTDIR)\xlAutoOpen.sbr" \
	"$(INTDIR)\vols.sbr"

"$(OUTDIR)\QuantLibXL.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\QuantLibXL_d.pdb" /debug /machine:I386 /out:"xll\Win32\VisualStudio\QuantLibXL_d.xll" /implib:"$(OUTDIR)\QuantLibXL_d.lib" /pdbtype:sept /libpath:"..\XLW\lib\Win32\VisualStudio" /libpath:"..\QuantLib\lib\Win32\VisualStudio" 
LINK32_OBJS= \
	"$(INTDIR)\datef.obj" \
	"$(INTDIR)\mathf.obj" \
	"$(INTDIR)\montecarlo.obj" \
	"$(INTDIR)\pricers.obj" \
	"$(INTDIR)\utilities.obj" \
	"$(INTDIR)\xlAutoOpen.obj" \
	"$(INTDIR)\vols.obj" \
	"..\QuantLib\lib\Win32\VisualStudio\QuantLib_d.lib" \
	"..\XLW\lib\Win32\VisualStudio\xlwd.lib"

".\xll\Win32\VisualStudio\QuantLibXL_d.xll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

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
!IF EXISTS("QuantLibXL.dep")
!INCLUDE "QuantLibXL.dep"
!ELSE 
!MESSAGE Warning: cannot find "QuantLibXL.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "QuantLibXL - Win32 Release" || "$(CFG)" == "QuantLibXL - Win32 Debug" || "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeRelease" || "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeDebug"
SOURCE=.\qlxl\datef.cpp

"$(INTDIR)\datef.obj"	"$(INTDIR)\datef.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\qlxl\mathf.cpp

"$(INTDIR)\mathf.obj"	"$(INTDIR)\mathf.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\qlxl\montecarlo.cpp

"$(INTDIR)\montecarlo.obj"	"$(INTDIR)\montecarlo.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\qlxl\pricers.cpp

"$(INTDIR)\pricers.obj"	"$(INTDIR)\pricers.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\qlxl\utilities.cpp

"$(INTDIR)\utilities.obj"	"$(INTDIR)\utilities.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\qlxl\vols.cpp

"$(INTDIR)\vols.obj"	"$(INTDIR)\vols.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\qlxl\xlAutoOpen.cpp

"$(INTDIR)\xlAutoOpen.obj"	"$(INTDIR)\xlAutoOpen.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "QuantLibXL - Win32 Release"

"QuantLib - Win32 Release" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 Release" 
   cd "..\QuantLibXL"

"QuantLib - Win32 ReleaseCLEAN" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 Release" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 Debug"

"QuantLib - Win32 Debug" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 Debug" 
   cd "..\QuantLibXL"

"QuantLib - Win32 DebugCLEAN" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeRelease"

"QuantLib - Win32 OnTheEdgeRelease" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 OnTheEdgeRelease" 
   cd "..\QuantLibXL"

"QuantLib - Win32 OnTheEdgeReleaseCLEAN" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 OnTheEdgeRelease" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeDebug"

"QuantLib - Win32 OnTheEdgeDebug" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 OnTheEdgeDebug" 
   cd "..\QuantLibXL"

"QuantLib - Win32 OnTheEdgeDebugCLEAN" : 
   cd "\Projects\QuantLib"
   $(MAKE) /$(MAKEFLAGS) /F .\QuantLib.mak CFG="QuantLib - Win32 OnTheEdgeDebug" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ENDIF 

!IF  "$(CFG)" == "QuantLibXL - Win32 Release"

"xlw - Win32 Release" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 Release" 
   cd "..\QuantLibXL"

"xlw - Win32 ReleaseCLEAN" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 Release" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 Debug"

"xlw - Win32 Debug" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 Debug" 
   cd "..\QuantLibXL"

"xlw - Win32 DebugCLEAN" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeRelease"

"xlw - Win32 OnTheEdgeRelease" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 OnTheEdgeRelease" 
   cd "..\QuantLibXL"

"xlw - Win32 OnTheEdgeReleaseCLEAN" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 OnTheEdgeRelease" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ELSEIF  "$(CFG)" == "QuantLibXL - Win32 OnTheEdgeDebug"

"xlw - Win32 OnTheEdgeDebug" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 OnTheEdgeDebug" 
   cd "..\QuantLibXL"

"xlw - Win32 OnTheEdgeDebugCLEAN" : 
   cd "\Projects\XLW"
   $(MAKE) /$(MAKEFLAGS) /F .\xlw.mak CFG="xlw - Win32 OnTheEdgeDebug" RECURSE=1 CLEAN 
   cd "..\QuantLibXL"

!ENDIF 


!ENDIF 

