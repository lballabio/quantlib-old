EXTRA_DIST = Doxyfile mainpage.dox introduction.dox

# DOC is defined if installer requests doc generation.


if DOC

htmldir = $(pkgdatadir)/html
#html_DATA = html/index.html

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(htmldir)
	cp -dpR html/* $(DESTDIR)$(htmldir)

# Automake's "distcheck" is sensitive to having files left over
# after "make uninstall", so we have to clean up the install hook.
uninstall-local:
	rm -rf $(DESTDIR)$(htmldir)

dox: html/index.html

if LATEX_DOC

pdf: @PACKAGE@.pdf
@PACKAGE@.pdf:
	$(MAKE) -C ./latex pdf
	ln -s ./latex/refman.ps	 @PACKAGE@.ps
	ln -s ./latex/refman.pdf @PACKAGE@.pdf
	
endif

else

# We repeat the three targets in both the "if" and "else" clauses
# of the conditional, because the generated makefile will contain
# references to the targets (target "install" depends on target
# "install-datahook", for example), and some make programs get upset
# if no target exists.

install-data-hook:
uninstall-local:
dox:

endif


all-local: dox


html/index.html: Doxyfile introduction.dox mainpage.dox
	"@DOXYGEN@"

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)

# Make tarfile to distribute the HTML documentation.
doc-dist: dox
	rm -rf $(distdir)
	mkdir $(distdir)
	mkdir $(distdir)/docs
	mkdir $(distdir)/docs/html
	cp html/* $(distdir)/docs/html
	tar -czf $(top_builddir)/$(PACKAGE)-docs-$(VERSION).tar.gz -C $(distdir) docs
	rm -rf $(distdir)


clean-local:
	$(RM) -r latex
	$(RM) -r html man @PACKAGE@.ps @PACKAGE@.pdf

