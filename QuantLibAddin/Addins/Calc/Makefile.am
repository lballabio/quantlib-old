
AM_CPPFLAGS = -I${top_srcdir} -I$(OFFICE_SDK_PATH)/include \
-DUNX -DGCC -DLINUX -DCPPU_ENV=gcc3

COMPONENT_NAME=QuantLibAddinCalc
IDL_DIR=$(OFFICE_SDK_PATH)/idl
DKREGISTRYNAME=$(OFFICE_PROGRAM_PATH)/types.rdb

if BUILD_CALC
lib_LTLIBRARIES = libQuantLibAddinCalc.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinCalc.la
endif

libQuantLibAddinCalc_la_LIBADD = ../../qlo/libQuantLibAddin.la

libQuantLibAddinCalc_la_LDFLAGS = \
    -Wl,--rpath,$(OFFICE_PROGRAM_PATH) \
    -L$(OFFICE_PROGRAM_PATH) \
    -lcppuhelpergcc3 -lcppu -lsal \
    -lQuantLib -lObjectHandler \
    -release $(PACKAGE_VERSION)

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

if BUILD_CALC
BUILT_SOURCES = $(COMPONENT_NAME).flag2
endif

EXTRA_DIST = \
    AddinCalc.vcproj \
    AddinCalc_vc8.vcproj \
    Makefile.vc \
    QuantLibAddinCalc.def \
    QuantLibAddinCalc.idl \
    readme.txt

if BUILD_CALC
noinst_HEADERS = \
    calcutils.hpp \
    conversions.hpp \
    exercise.hpp \
    instruments.hpp \
    loop.hpp \
    ohfunctions.hpp \
    options.hpp \
    payoffs.hpp \
    pricingengines.hpp \
    processes.hpp \
    qla_all.hpp \
    qladdin.hpp \
    qldefs.hpp \
    ratehelpers.hpp \
    settings.hpp \
    termstructures.hpp \
    utilities.hpp \
    volatilities.hpp
endif

libQuantLibAddinCalc_la_SOURCES = \
    calcutils.cpp \
    conversions.cpp \
    exercise.cpp \
    funcdef.cpp \
    instruments.cpp \
    manual.cpp \
    ohfunctions.cpp \
    options.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    qladdin.cpp \
    ratehelpers.cpp \
    session.cpp \
    settings.cpp \
    termstructures.cpp \
    utilities.cpp \
    volatilities.cpp

if BUILD_CALC

noinst_DATA = $(COMPONENT_NAME).flag3
$(COMPONENT_NAME).flag3 : libQuantLibAddinCalc.la
	cp -f $(COMPONENT_NAME).rdb .libs
	cd .libs && \
	regcomp -register -r $(COMPONENT_NAME).rdb -c libQuantLibAddinCalc.so
	echo flagged > $@

install-exec-hook:
	cp -f .libs/$(COMPONENT_NAME).rdb $(OFFICE_PROGRAM_PATH)
	cp -f .libs/libQuantLibAddinCalc.so $(OFFICE_PROGRAM_PATH)

endif

CLEANFILES = \
	$(COMPONENT_NAME).urd \
	$(COMPONENT_NAME).rdb \
	$(COMPONENT_NAME).flag*

clean-local :
	rm -rf com

