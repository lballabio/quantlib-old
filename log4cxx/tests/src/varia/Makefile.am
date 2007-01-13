EXTRA_DIST = *.cpp

if TESTS

noinst_LIBRARIES = libvaria.a

INCLUDES = -I$(top_srcdir)/include

libvaria_a_SOURCES = \
	errorhandlertestcase.cpp \
	levelmatchfiltertestcase.cpp \
	levelrangefiltertestcase.cpp
	
check: libvaria.a

endif


