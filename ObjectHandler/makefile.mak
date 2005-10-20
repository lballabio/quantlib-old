
# main makefile for ObjectHandler under Borland C++

.autodepend
#.silent

MAKE = $(MAKE) -DVERSION=0.1.2 -DVERSION_STRING=0_1_2

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

# Primary target:
# ObjectHandler library
objecthandler::
    cd oh
    $(MAKE)
    cd ..

# Example
example:: objecthandler
    cd Example
    $(MAKE) -DOBJECT_HANDLER_DIR=".."
    cd ..

# check
check: example
    cd Example
    $(MAKE) -DOBJECT_HANDLER_DIR=".." check
    cd ..

# the installation directive requires the OBJECT_HANDLER_DIR environment
# variable to point to the installed version of ObjectHandler
install : inst
inst:: objecthandler
    if exist "$(OBJECT_HANDLER_DIR)\oh" \
            rmdir /S /Q "$(OBJECT_HANDLER_DIR)\oh"
    xcopy oh\*.hpp "$(OBJECT_HANDLER_DIR)\oh" /S /I

    if exist "$(OBJECT_HANDLER_DIR)\lib" rmdir /S /Q "$(OBJECT_HANDLER_DIR)\lib"
    xcopy lib\*.lib "$(OBJECT_HANDLER_DIR)\lib" /S /I
    xcopy lib\*.pdb "$(OBJECT_HANDLER_DIR)\lib" /S /I


# Documentation
docs-all:
    cd Docs
    $(MAKE) all
    cd ..

docs-html:
    cd Docs
    $(MAKE) html
    cd ..

docs-htmlhelp:
    cd Docs
    $(MAKE) htmlhelp
    cd ..

docs-html-online:
    cd Docs
    $(MAKE) html-online
    cd ..

docs-pdf:
    cd Docs
    $(MAKE) pdf
    cd ..

docs-ps:
    cd Docs
    $(MAKE) ps
    cd ..


# Clean up
clean::
    cd oh
    $(MAKE) clean
    cd ..\Example
    $(MAKE) clean
    cd ..\Docs
    $(MAKE) clean
    cd ..
