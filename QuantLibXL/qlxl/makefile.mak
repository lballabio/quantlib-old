
.autodepend
.silent

# Debug version
!ifdef DEBUG
    _D = _d
    _D2 = d
!endif

# Directories
INCLUDE_DIR     = ..
XLW_INCLUDE_DIR = "$(XLW_DIR)"
XLW_LIB_DIR     = "$(XLW_DIR)\lib\Win32\Borland"
QL_INCLUDE_DIR  = "$(QL_DIR)"
QL_LIB_DIR      = "$(QL_DIR)\lib\Win32\Borland"
BCC_INCLUDE     = $(MAKEDIR)\..\include
BCC_LIBS        = $(MAKEDIR)\..\lib
OUTPUT_DIR      = ..\xll\Win32\Borland

# Object files
CORE_OBJS = \
    mathf.obj$(_D) \
    datef.obj$(_D) \
    engines.obj$(_D) \
    montecarlo.obj$(_D) \
    pricers.obj$(_D) \
    qlxlfoper.obj$(_D) \
    termstructures.obj$(_D) \
    utilities.obj$(_D) \
    vols.obj$(_D) \
    xlAutoOpen.obj$(_D)

# Tools to be used
CC        = bcc32
TLIB      = tlib
!ifdef DEBUG
    MAKE = $(MAKE) -DDEBUG
!endif
!ifdef SAFE
    MAKE = $(MAKE) -DSAFE
!endif


# Options
CC_OPTS = -vi- \
    -I$(INCLUDE_DIR) \
    -I$(XLW_INCLUDE_DIR) \
    -I$(QL_INCLUDE_DIR) \
    -I$(BCC_INCLUDE)
!ifdef DEBUG
CC_OPTS = $(CC_OPTS) -v -DXLW_DEBUG -DQL_DEBUG
!endif
!ifdef SAFE
CC_OPTS = $(CC_OPTS) -DQL_EXTRA_SAFETY_CHECKS
!endif

TLIB_OPTS    = /P32
!ifdef DEBUG
TLIB_OPTS    = /P64
!endif

# Generic rules
.cpp.obj:
    $(CC) $(CC_OPTS) $<
.cpp.obj$(_D):
    $(CC) $(CC_OPTS) -o$@ $<

# Primary target:
$(OUTPUT_DIR)\QuantLibXL$(_D).xll:: $(OUTPUT_DIR) $(CORE_OBJS)
    if exist $(OUTPUT_DIR)\QuantLibXL$(_D).xll del $(OUTPUT_DIR)\QuantLibXL$(_D).xll
    $(TLIB) $(TLIB_OPTS) $(OUTPUT_DIR)\QuantLibXL$(_D).xll /a $(CORE_OBJS)
#    bcc32 $(CC_OPTS) -L$(XLW_LIB_DIR) -L$(QL_LIB_DIR) -L$(BCC_LIBS) -tWD -oqlxl$(_D).obj -e"$(OUTPUT_DIR)\QuantLibXL$(_D).xll" qlxl.cpp QuantLib$(_D).lib xlw$(_D2).lib

$(OUTPUT_DIR):
    if not exist ..\xll md ..\xll
    if not exist ..\xll\Win32 md ..\xll\Win32
    if not exist ..\xll\Win32\Borland md ..\xll\Win32\Borland

# Clean up
clean::
    if exist *.obj                del /q *.obj
    if exist *.tds                del /q *.tds
    if exist $(OUTPUT_DIR)\*.tds  del $(OUTPUT_DIR)\*.tds
    if exist *.xll                del /q *.xll
    if exist $(OUTPUT_DIR)\*.xll  del $(OUTPUT_DIR)\*.xll
    