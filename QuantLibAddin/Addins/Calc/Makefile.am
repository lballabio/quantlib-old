
#includedir = $(prefix)/include/qla
AM_CPPFLAGS = -I${top_srcdir} -I$(OFFICE_SDK_PATH)/include \
-DUNX -DGCC -DLINUX -DCPPU_ENV=gcc3
AM_LDFLAGS = -L$(OFFICE_PROGRAM_PATH) \
    -lcppuhelpergcc3 -lcppu -lsal \
    -lQuantLib -lObjectHandler \
    -release $(PACKAGE_VERSION)

COMPONENT_NAME=QuantLibAddin
IDL_DIR=$(OFFICE_SDK_PATH)/idl
DKREGISTRYNAME=$(OFFICE_PROGRAM_PATH)/types.rdb

TYPES = com.sun.star.sheet.XAddIn \
	com.sun.star.lang.XServiceName \
	com.sun.star.lang.XServiceInfo \
	com.sun.star.uno.XWeak \
	com.sun.star.lang.XSingleServiceFactory \
	com.sun.star.lang.XMultiServiceFactory \
	com.sun.star.uno.XAggregation \
	com.sun.star.lang.XTypeProvider \
	com.sun.star.uno.XComponentContext \
	com.sun.star.lang.XSingleComponentFactory \
	com.sun.star.registry.XRegistryKey \
	com.sun.star.sheet.addin.XQL
TYPESLIST = $(foreach t,$(TYPES),-T$(t))

$(COMPONENT_NAME).urd : $(COMPONENT_NAME).idl
	idlc -I$(IDL_DIR) $(COMPONENT_NAME).idl

$(COMPONENT_NAME).flag1 : $(COMPONENT_NAME).urd
	regmerge $(COMPONENT_NAME).rdb /UCR $(COMPONENT_NAME).urd
	echo flagged > $@

$(COMPONENT_NAME).flag2 : $(COMPONENT_NAME).flag1
	cppumaker -BUCR $(TYPESLIST) $(DKREGISTRYNAME) $(COMPONENT_NAME).rdb
	echo flagged > $@

#FIXME execute as hook after build
#$(COMPONENT_NAME).flag3 : $(COMPONENT_NAME).flag1 $(COMPONENT_NAME).so
#	regcomp -register -r $(COMPONENT_NAME).rdb -c $(COMPONENT_NAME).so
#	echo flagged > $@

EXTRA_DIST = \
    AddinCalc.dsp \
    AddinCalc.vcproj \
    Makefile.win32 \
    Makefile.win32.debug \
    QuantLibAddin.def \
    QuantLibAddin.idl \
    readme.txt

# for now we do not install the Calc addin
noinst_LTLIBRARIES = libQuantLibAddinCalc.la

# FIXME need a better way to force this dependency
funcdef.cpp : $(COMPONENT_NAME).flag2
#libQuantLibAddinCalc_la_DEPENDENCIES = $(COMPONENT_NAME).flag2

libQuantLibAddinCalc_la_LIBADD = ../../qla/libQuantLibAddin.la

noinst_HEADERS = \
    autogen.hpp \
    options.hpp \
    functions.hpp \
    qladdin.hpp \
    qldefs.hpp \
    utilities.hpp

libQuantLibAddinCalc_la_SOURCES = \
    funcdef.cpp \
    functions.cpp \
    options.cpp \
    qladdin.cpp \
    utilities.cpp

CLEANFILES = \
	com \
	$(COMPONENT_NAME).urd \
	$(COMPONENT_NAME).rdb \
	$(COMPONENT_NAME).flag*

