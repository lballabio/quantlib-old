
.autodepend
.silent

!ifdef _DEBUG
!ifndef _RTLDLL
    _D = -sd
!else
    _D = -d
!endif
!else
!ifndef _RTLDLL
    _D = -s
!endif
!endif

!ifdef __MT__
    _mt = -mt
!endif

# Directories
INCLUDE_DIR    = ..
OUTPUT_DIR     = ..\lib

# Object files
CORE_OBJS = \
    "exception.obj$(_mt)$(_D)" \
    "object.obj$(_mt)$(_D)" \
    "objecthandler.obj$(_mt)$(_D)" \
    "utilities.obj$(_mt)$(_D)"

# Tools to be used
CC        = bcc32
TLIB      = tlib

# MAKE Options
!ifdef __MT__
    MAKE = $(MAKE) -D__MT__
!endif
!ifdef _RTLDLL
    MAKE = $(MAKE) -D_RTLDLL
!endif
!ifdef _DEBUG
    MAKE = $(MAKE) -D_DEBUG
!endif
!ifdef SAFE
    MAKE = $(MAKE) -DSAFE
!endif


# Options
CC_OPTS = -vi- -q -c -I$(INCLUDE_DIR)

!ifdef _DEBUG
    CC_OPTS = $(CC_OPTS) -v -D_DEBUG
!else
    CC_OPTS = $(CC_OPTS) -O2 -DNDEBUG
!endif

!ifdef _RTLDLL
    CC_OPTS = $(CC_OPTS) -D_RTLDLL
!endif

!ifdef __MT__
    CC_OPTS = $(CC_OPTS) -tWM
!endif

!ifdef SAFE
    CC_OPTS = $(CC_OPTS) -DQL_EXTRA_SAFETY_CHECKS
!endif


TLIB_OPTS    = /P16
!ifdef _DEBUG
TLIB_OPTS    = /P32
!endif

# Generic rules
.cpp.obj:
    $(CC) $(CC_OPTS) $<
.cpp.obj$(_mt)$(_D):
    $(CC) $(CC_OPTS) -o$@ $<

# Primary target:
# ObjectHandler library
$(OUTPUT_DIR)\ObjectHandler-bcb$(_mt)$(_D)-0_0_1.lib:: $(OUTPUT_DIR) $(CORE_OBJS)
    if exist $(OUTPUT_DIR)\ObjectHandler-bcb$(_mt)$(_D)-0_0_1.lib \
         del $(OUTPUT_DIR)\ObjectHandler-bcb$(_mt)$(_D)-0_0_1.lib
    $(TLIB) $(TLIB_OPTS) \
        "$(OUTPUT_DIR)\ObjectHandler-bcb$(_mt)$(_D)-0_0_1.lib" /a $(CORE_OBJS)

$(OUTPUT_DIR):
    if not exist ..\lib md ..\lib


# Clean up
clean::
    if exist *.obj*                    del /q *.obj*
    if exist *.lib                     del /q *.lib
    if exist $(OUTPUT_DIR)\*-bcb-*.lib del /q $(OUTPUT_DIR)\*-bcb-*.lib
