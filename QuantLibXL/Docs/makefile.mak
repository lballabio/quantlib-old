
# makefile for QuantLibXL documentation under Borland C++

.autodepend
.silent

# Tools to be used
SED       = sed
DOXYGEN   = doxygen
LATEX     = latex
PDFLATEX  = pdflatex
MAKEINDEX = makeindex
DVIPS     = dvips

# Options
TEX_OPTS = --quiet --pool-size=1000000

# Primary target:
# all docs
all:: html
    cd latex
    $(PDFLATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(PDFLATEX) $(TEX_OPTS) refman
    $(LATEX) $(TEX_OPTS) refman
    $(DVIPS) refman
    cd ..

# HTML documentation only
html: html-offline

html-offline::
    $(SED) -e "s|qlxl_basepath|$(QLXL_DIR)\\\\|" \
           quantlibxl.doxy > quantlibxl.doxy.temp
    $(DOXYGEN) quantlibxl.doxy.temp
    del /Q quantlibxl.doxy.temp
    copy images\*.bmp html
    copy images\*.bmp latex
    copy images\*.eps html
    copy images\*.eps latex
    copy images\*.jpg html
    copy images\*.jpg latex
    copy images\*.pdf html
    copy images\*.pdf latex
    copy images\*.png html
    copy images\*.png latex


# PDF documentation
pdf:: html
    cd latex
    $(PDFLATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(PDFLATEX) $(TEX_OPTS) refman
    cd ..

# PostScript documentation
ps:: html
    cd latex
    $(LATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(LATEX) $(TEX_OPTS) refman
    $(DVIPS) refman
    cd ..

# Clean up
clean::
    if exist html  rmdir /S /Q html
    if exist latex rmdir /S /Q latex
