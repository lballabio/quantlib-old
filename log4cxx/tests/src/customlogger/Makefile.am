EXTRA_DIST = $(top_srcdir)/tests/src/customlogger/*.cpp
noinst_HEADERS= $(top_srcdir)/tests/src/customlogger/*.h

if TESTS

noinst_LIBRARIES = libcustomlogger.a

INCLUDES = -I$(top_srcdir)/include

libcustomlogger_a_SOURCES = \
	xlogger.cpp\
	xloggertestcase.cpp
	
check: libcustomlogger.a

endif
