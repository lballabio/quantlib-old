
# makefile for ObjectHandler documentation under Borland C++

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
all:: tex-files
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
    if not exist .\html md .\html
    copy images\QL-small.jpg html
    copy images\sfnetlogo.png html
    $(SED) -e "s|oh_basepath|D:/Projects/ObjectHandler|" \
           -e "s|oh_version|0.0.1|" \
           -e "s|GENERATE_HTMLHELP      = NO|GENERATE_HTMLHELP      = YES|" \
           objecthandler.doxy > objecthandler.doxy.temp
    $(DOXYGEN) objecthandler.doxy.temp
    del /Q objecthandler.doxy.temp

html-online::
    if not exist .\html md .\html
    copy images\QL-small.jpg html
    copy images\sfnetlogo.png html
    $(SED) -e "s|oh_basepath|D:/Projects/ObjectHandler|" \
           -e "s|oh_version|0.0.1|" \
           -e "s/objecthandlerfooter.html/objecthandlerfooteronline.html/" \
           objecthandler.doxy > objecthandler.doxy.temp
    $(DOXYGEN) objecthandler.doxy.temp
    del /Q objecthandler.doxy.temp

# PDF documentation
pdf:: tex-files
    cd latex
    $(PDFLATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(PDFLATEX) $(TEX_OPTS) refman
    cd ..

# PostScript documentation
ps:: tex-files
    cd latex
    $(LATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(LATEX) $(TEX_OPTS) refman
    $(DVIPS) refman
    cd ..

# Correct LaTeX files to get the right layout
tex-files:: html
    copy userman.tex latex
    cd latex
    ren refman.tex oldrefman.tex
    $(SED) -e "/Page Index/d" \
           -e "/input{pages}/d" \
           -e "/Page Documentation/d" \
           -e "/input{faq}/d" \
           -e "/include{group}/d" \
           -e "/include{history}/d" \
           -e "/include{index}/d" \
           -e "/include{install}/d" \
           -e "/include{license}/d" \
           -e "/include{overview}/d" \
           -e "/include{resources}/d" \
           -e "/include{usage}/d" \
           -e "/include{where}/d" \
           -e "s/ple Documentation}/ple Documentation}\\\\label{exchap}/" \
           oldrefman.tex > refman.tex
    del oldrefman.tex
	ren deprecated.tex olddeprecated.tex
	$(SED) -e "s/section/chapter/" olddeprecated.tex > deprecated.tex
	del olddeprecated.tex
	ren bug.tex oldbug.tex
	$(SED) -e "s/section/chapter/" oldbug.tex > bug.tex
	rm -f oldbug.tex
    cd ..

# Clean up
clean::
    if exist html  rmdir /S /Q html
    if exist latex rmdir /S /Q latex
