EXTRA_DIST = $(top_srcdir)/tests/src/xml/*.cpp
noinst_HEADERS= $(top_srcdir)/tests/src/xml/*.h

if TESTS

noinst_LIBRARIES = libxml.a

INCLUDES = -I$(top_srcdir)/include

libxml_a_SOURCES = \
	customleveltestcase.cpp \
	domtestcase.cpp \
	xlevel.cpp \
	xmllayouttestcase.cpp
	
check: libxml.a

endif
