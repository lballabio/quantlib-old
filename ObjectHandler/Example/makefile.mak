
# makefile for ObjectHandler example under Borland C++

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
OBJECT_HANDLER_INCLUDE_DIR = "$(OBJECT_HANDLER_DIR)"
OBJECT_HANDLER_LIB_DIR     = "$(OBJECT_HANDLER_DIR)\lib"
EXE_DIR                    = .\bin

# Object files
OBJHANDLER_EXAMPLE_FILES = \
    "example.obj$(_mt)$(_D)" \
    "interface.obj$(_mt)$(_D)" \
    "objectfoo.obj$(_mt)$(_D)"

# Tools to be used
CC        = bcc32
TLIB      = tlib

# Options
CC_OPTS = -vi- -I$(OBJECT_HANDLER_INCLUDE_DIR) -I.

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

TLIB_OPTS    = /P256

OBJHANDLER_EXAMPLE_LIB = Example-bcb$(_mt)$(_D).lib

# Generic rules
.cpp.obj:
    $(CC) -c -q $(CC_OPTS) $<
.cpp.obj$(_mt)$(_D):
    $(CC) -c -q $(CC_OPTS) -o$@ $<


# Primary target:
$(EXE_DIR)\Example-bcb$(_mt)$(_D).exe:: $(EXE_DIR) $(OBJHANDLER_EXAMPLE_LIB)
    if exist $(EXE_DIR)\Example-bcb$(_mt)$(_D).exe \
         del $(EXE_DIR)\Example-bcb$(_mt)$(_D).exe
    $(CC) $(CC_OPTS) -L$(OBJECT_HANDLER_LIB_DIR) \
          -e"$(EXE_DIR)\Example-bcb$(_mt)$(_D).exe" \
          $(OBJHANDLER_EXAMPLE_LIB) ObjectHandler-bcb$(_mt)$(_D)-$(VERSION_STRING).lib

$(EXE_DIR):
    if not exist .\bin md .\bin

$(OBJHANDLER_EXAMPLE_LIB): $(OBJHANDLER_EXAMPLE_FILES)
    if exist $(OBJHANDLER_EXAMPLE_LIB)     del $(OBJHANDLER_EXAMPLE_LIB)
    $(TLIB) $(TLIB_OPTS) "$(OBJHANDLER_EXAMPLE_LIB)" /a \
                                      $(OBJHANDLER_EXAMPLE_FILES)


check: $(EXE_DIR)\Example-bcb$(_mt)$(_D).exe
    $(EXE_DIR)\Example-bcb$(_mt)$(_D).exe
    cd ..


# Clean up
clean::
    if exist *.obj*                   del /q *.obj*
    if exist $(EXE_DIR)\*-bcb*.tds   del /q $(EXE_DIR)\*-bcb*.tds
    if exist $(EXE_DIR)\*-bcb*.exe   del /q $(EXE_DIR)\*-bcb*.exe
