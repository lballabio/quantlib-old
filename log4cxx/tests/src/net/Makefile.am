EXTRA_DIST = $(top_srcdir)/tests/src/net/*.cpp
noinst_HEADERS= $(top_srcdir)/tests/src/net/*.h

if TESTS

noinst_LIBRARIES = libnet.a
INCLUDES = -I$(top_srcdir)/include

libnet_a_SOURCES = \
	socketservertestcase.cpp

check: libnet.a 

endif


