
# main makefile for XLW under Borland C++
#
# $Id $


.autodepend
#.silent

# Options
!ifdef DEBUG
    MAKE = $(MAKE) -DDEBUG
!endif

# Primary target:
# qlxl library
qlxl::
    cd qlxl
    $(MAKE)
    cd ..

# the installation directive requires the QLXL_DIR environment variable to
# point to the installed version of QLXL
inst::
    if exist "$(QLXL_DIR)\qlxl" rmdir /S /Q "$(QLXL_DIR)\qlxl"
    xcopy xlw\*.h* "$(QLXL_DIR)\qlxl" /S /I

    if exist "$(QLXL_DIR)\xll\Win32\VisualStudio" rmdir /S /Q "$(QLXL_DIR)\xll\Win32\VisualStudio"
    xcopy lib\Win32\VisualStudio\*.xll "$(QLXL_DIR)\xll\Win32\VisualStudio" /S /I
    xcopy lib\Win32\VisualStudio\*.pdb "$(QLXL_DIR)\xll\Win32\VisualStudio" /S /I

    if exist "$(QLXL_DIR)\xll\Win32\Borland" rmdir /S /Q "$(QLXL_DIR)\xll\Win32\Borland"
    xcopy lib\Win32\Borland\*.xll "$(QLXL_DIR)\xll\Win32\Borland" /S /I


# Documentation
docs-all:
    cd Docs
    $(MAKE) all
    cd ..

docs-html:
    cd Docs
    $(MAKE) html
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
    cd qlxl
    $(MAKE) clean
    cd ..\Docs
    $(MAKE) clean
    cd ..
