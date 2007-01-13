EXTRA_DIST = *.cpp

if TESTS

noinst_LIBRARIES = libdefaultinit.a

INCLUDES = -I$(top_srcdir)/include

libdefaultinit_a_SOURCES = \
	testcase1.cpp\
	testcase2.cpp\
	testcase3.cpp\
	testcase4.cpp
	
check: libdefaultinit.a

endif
