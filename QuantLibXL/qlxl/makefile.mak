
#
# makefile for main.cpp under Borland C++
#
# $Id$

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
OUTPUT_DIR     = ..\xll\Win32\Borland

#Warning W8026 :
#Warning W8027 :
#Warning W8012 :
#Warning W8057 : Parameter 'argc' is never used in function main(int,char * *)

# Options
#    -w-8026 -w-8027 -w-8012 -w-8057 \
CC_OPTS = -vi- \
    -I$(INCLUDE_DIR) \
    -I$(XLW_INCLUDE_DIR) \
    -I$(QL_INCLUDE_DIR) \
    -I$(BCC_INCLUDE)
!ifdef DEBUG
CC_OPTS = $(CC_OPTS) -v -DXLW_DEBUG -DQL_DEBUG
!endif

# Primary target:
$(OUTPUT_DIR)\qlxl$(_D).xll:: qlxl.cpp
    if exist $(OUTPUT_DIR)\qlxl$(_D).xll del $(OUTPUT_DIR)\qlxl$(_D).xll
    bcc32 $(CC_OPTS) -L$(XLW_LIB_DIR) -L$(QL_LIB_DIR) -L$(BCC_LIBS) -tWD -oqlxl$(_D).obj -e"$(OUTPUT_DIR)\qlxl$(_D).xll" qlxl.cpp QuantLib$(_D).lib xlw$(_D2).lib

# Clean up
clean::
    if exist *.obj   del /q *.obj
    if exist *.tds   del /q *.tds
    if exist *.xll   del /q *.xll
