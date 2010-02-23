
SUBDIRS = CSharp Guile Java MzScheme OCaml Perl Python R Ruby

EXTRA_DIST = \
	ChangeLog.txt \
	LICENSE.TXT \
	News.txt \
	Readme.txt \
	autogen.sh


dist-hook:
	mkdir -p $(distdir)/SWIG
	cp -p ./SWIG/*.i $(distdir)/SWIG

