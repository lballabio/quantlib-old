
SUBDIRS = CSharp Guile Java MzScheme Perl Python Ruby

EXTRA_DIST = \
	ChangeLog.txt \
	LICENSE.TXT \
	News.txt \
	Readme.txt \
	autogen.sh


dist-hook:
	mkdir -p $(distdir)/SWIG
	cp -p ./SWIG/*.i $(distdir)/SWIG

