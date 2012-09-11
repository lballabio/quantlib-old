
SUBDIRS = ibor swap

AM_CPPFLAGS = -I${top_srcdir}

this_includedir=${includedir}/${subdir}
this_include_HEADERS = \
	bmaindex.hpp \
	iborindex.hpp \
	interestrateindex.hpp \
	swapindex.hpp

libIndexes_la_SOURCES = \
	bmaindex.cpp \
	iborindex.cpp \
	swapindex.cpp

libIndexes_la_LIBADD = \
    ibor/libIborIndexes.la \
    swap/libSwapIndexes.la

noinst_LTLIBRARIES = libIndexes.la

