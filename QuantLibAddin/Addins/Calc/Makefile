COMPONENT_NAME=QuantLibAddin
IDL_DIR=$(OFFICE_SDK_PATH)/idl
DKREGISTRYNAME=$(OFFICE_PROGRAM_PATH)/types.rdb
#CC=g++
CC=/usr/local/gcc/3.3.3/bin/g++
CC_FLAGS=-c -fPIC
CC_INCLUDES=-I. -I../.. -I/usr/include -I$(OFFICE_SDK_PATH)/include \
	-I../../../ObjectHandler
CC_DEFINES=-DHAVE_CONFIG_H -DUNX -DGCC -DLINUX -DCPPU_ENV=gcc3
LINK=g++
LIBRARY_LINK_FLAGS=-shared
LINK_LIBS=-L$(OFFICE_PROGRAM_PATH) -L/usr/local/lib -L../../QuantLibAddin
LIBRARY_LIBS := -lcppuhelpergcc3 -lcppu -lsal -lstlport_gcc -lvcl645li \
				-lvos3gcc3 -lQuantLib -lQuantLibAddin -lboost_regex
CXXFILES := $(wildcard *.cpp)
OBJFILES := $(patsubst %.cpp,%.o,$(CXXFILES))

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

ALL : $(COMPONENT_NAME).flag3

$(COMPONENT_NAME).urd : $(COMPONENT_NAME).idl
	idlc -I$(IDL_DIR) $(COMPONENT_NAME).idl

$(COMPONENT_NAME).flag1 : $(COMPONENT_NAME).urd
	regmerge $(COMPONENT_NAME).rdb /UCR $(COMPONENT_NAME).urd
	echo flagged > $@

$(COMPONENT_NAME).flag2 : $(COMPONENT_NAME).flag1
	cppumaker -BUCR $(TYPESLIST) $(DKREGISTRYNAME) $(COMPONENT_NAME).rdb
	echo flagged > $@

%.o : %.cpp $(COMPONENT_NAME).flag2
	$(CC) $(CC_FLAGS) $(CC_INCLUDES) $(CC_DEFINES) -o $@ $<

$(COMPONENT_NAME).so : $(OBJFILES)
	$(LINK) $(LIBRARY_LINK_FLAGS) $(LINK_LIBS) -o $(COMPONENT_NAME).so $(OBJFILES) $(LIBRARY_LIBS)

$(COMPONENT_NAME).flag3 : $(COMPONENT_NAME).rdb $(COMPONENT_NAME).so
	regcomp -register -r $(COMPONENT_NAME).rdb -c $(COMPONENT_NAME).so
	echo flagged > $@

DELRECURSIVE=rm -rf
DEL=rm -f
clean :
	-$(DELRECURSIVE) com
	-$(DEL)  $(COMPONENT_NAME).urd
	-$(DEL)  $(COMPONENT_NAME).rdb
	-$(DEL)  $(COMPONENT_NAME).flag*
	-$(DEL)  *.o
	-$(DEL)  $(COMPONENT_NAME).so
