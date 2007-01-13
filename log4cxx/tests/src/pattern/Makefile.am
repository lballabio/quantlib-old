EXTRA_DIST = $(top_srcdir)/tests/src/pattern/*.cpp
noinst_HEADERS= $(top_srcdir)/tests/src/pattern/*.h

if TESTS

noinst_LIBRARIES = libpattern.a

INCLUDES = -I$(top_srcdir)/include

libpattern_a_SOURCES = \
	num343patternconverter.cpp \
	patternparsertestcase.cpp
	
check: libpattern.a

endif


