
# makefile for ObjectHandler documentation under Borland C++

.autodepend
#.silent

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
all:: html-config htmlhelp-config tex-config tex-files
    cd latex
    $(PDFLATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(PDFLATEX) $(TEX_OPTS) refman
    $(LATEX) $(TEX_OPTS) refman
    $(DVIPS) refman
    cd ..
    copy latex\refman.pdf ObjectHandler-docs-$(VERSION).pdf
    copy latex\refman.ps  ObjectHandler-docs-$(VERSION).ps

generic-config::
    $(SED) -e "s|oh_basepath|D:/Projects/ObjectHandler|" \
           -e "s|oh_version|$(VERSION)|" \
           objecthandler.doxy > objecthandler.doxy.temp

html-config:: generic-config
    $(SED) -e "s|GENERATE_HTML          = NO|GENERATE_HTML          = YES|" \
           objecthandler.doxy.temp > objecthandler.doxy.temp2
    del /Q objecthandler.doxy.temp
    ren objecthandler.doxy.temp2 objecthandler.doxy.temp
    if not exist .\html md .\html
    copy images\*.jpg html
    copy images\*.png html

htmlhelp-config:: generic-config
    $(SED) -e "s|GENERATE_HTML          = NO|GENERATE_HTML          = YES|" \
           -e "s|GENERATE_HTMLHELP      = NO|GENERATE_HTMLHELP      = YES|" \
           objecthandler.doxy.temp > objecthandler.doxy.temp2
    del /Q objecthandler.doxy.temp
    ren objecthandler.doxy.temp2 objecthandler.doxy.temp
    if not exist .\html md .\html
    copy images\*.jpg html
    copy images\*.png html

html-online-config:: generic-config
    $(SED) -e "s/_OUTPUT            = html/_OUTPUT            = html-online/" \
           -e "s/quantlibfooter.html/quantlibfooteronline.html/" \
           -e "s|GENERATE_HTML          = NO|GENERATE_HTML          = YES|" \
           objecthandler.doxy.temp > objecthandler.doxy.temp2
    del /Q objecthandler.doxy.temp
    ren objecthandler.doxy.temp2 objecthandler.doxy.temp
    if not exist .\html-online md .\html-online
    copy images\*.jpg html-online
    copy images\*.png html-online

tex-config:: generic-config
    $(SED) -e "s|GENERATE_LATEX         = NO|GENERATE_LATEX         = YES|" \
           objecthandler.doxy.temp > objecthandler.doxy.temp2
    del /Q objecthandler.doxy.temp
    ren objecthandler.doxy.temp2 objecthandler.doxy.temp
    if not exist .\latex md .\latex
    copy images\*.pdf latex
    copy images\*.eps latex

html:: html-config
    $(DOXYGEN) objecthandler.doxy.temp
    del /Q objecthandler.doxy.temp

htmlhelp:: htmlhelp-config
    $(DOXYGEN) objecthandler.doxy.temp
    del /Q objecthandler.doxy.temp

html-online:: html-online-config
    $(DOXYGEN) objecthandler.doxy.temp
    del /Q objecthandler.doxy.temp

# LaTeX files for ps and pdf
tex-files:: tex-config
    $(DOXYGEN) objecthandler.doxy.temp
    del /Q objecthandler.doxy.temp


























# PDF documentation
pdf:: tex-files
    cd latex
    $(PDFLATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(PDFLATEX) $(TEX_OPTS) refman
    cd ..
    copy latex\refman.pdf ObjectHandler-docs-$(VERSION).pdf

# PostScript documentation
ps:: tex-files
    cd latex
    $(LATEX) $(TEX_OPTS) refman
    $(MAKEINDEX) refman.idx
    $(LATEX) $(TEX_OPTS) refman
    $(DVIPS) refman
    cd ..
    copy latex\refman.ps ObjectHandler-docs-$(VERSION).ps


# Clean up
clean::
    if exist html                 rmdir /S /Q html
    if exist html-online          rmdir /S /Q html-online
    if exist latex                rmdir /S /Q latex
    if exist ObjectHandler-docs-* rm -f ObjectHandler-docs-*
    if exist *.temp               rm -f *.temp
    if exist *.temp2              rm -f *.temp2
