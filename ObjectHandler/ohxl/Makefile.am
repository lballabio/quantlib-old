
EXTRA_DIST = \
    callingrange.cpp \
    callingrange.hpp \
    configuration.cpp \
    configuration.hpp \
    convert_oper.cpp \
    convert_oper.hpp \
    functioncall.cpp \
    functioncall.hpp \
    loop.hpp \
    objecthandlerxl.hpp \
    objectxl.cpp \
    objectxl.hpp \
    ohxldefines.hpp \
    rangereference.cpp \
    rangereference.hpp \
    repositoryxl.cpp \
    repositoryxl.hpp \
    xloper.hpp
 
dist-hook:
	mkdir -p $(distdir)/Conversions  $(distdir)/Functions $(distdir)/ohxll $(distdir)/ohxllib $(distdir)/Register $(distdir)/Utilities
	cp -p $(srcdir)/Conversions/*.?pp $(distdir)/Conversions
	cp -p $(srcdir)/Functions/*.?pp $(distdir)/Functions
	cp -p $(srcdir)/ohxll/*.vcproj $(distdir)/ohxll
	cp -p $(srcdir)/ohxllib/*.vcproj $(distdir)/ohxllib
	cp -p $(srcdir)/Register/*.?pp $(distdir)/Register
	cp -p $(srcdir)/Utilities/*.?pp $(distdir)/Utilities

