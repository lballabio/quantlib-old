#SUBDIRS = docs src msvc include tests performance examples
SUBDIRS = src msvc include tests performance examples
EXTRA_DIST = autogen.sh configure include license.apl LICENSE.txt README.txt
dist-hook:
	rm -rf \
	`find $(distdir) -name Makefile` \
	`find $(distdir) -name Makefile.in` \
	`find $(distdir) -name CVS` \
	$(distdir)/aclocal.m4 \
	$(distdir)/config.guess \
	$(distdir)/config.sub \
	$(distdir)/configure \
	$(distdir)/depcomp \
	$(distdir)/install-sh \
	$(distdir)/ltmain.sh \
	$(distdir)/missing \
	$(distdir)/mkinstalldirs \
	$(distdir)/include/log4cxx/stamp*

