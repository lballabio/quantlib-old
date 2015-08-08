
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
    objectwrapperxl.hpp \
    objectwrapperxl.cpp \
    ohxldefines.hpp \
    rangereference.cpp \
    rangereference.hpp \
    repositoryxl.cpp \
    repositoryxl.hpp \
    xloper.hpp
 
dist-hook:
	mkdir -p $(distdir)/conversions  $(distdir)/functions $(distdir)/ohxll $(distdir)/ohxllib $(distdir)/register $(distdir)/utilities
	cp -p $(srcdir)/conversions/*.?pp $(distdir)/conversions
	cp -p $(srcdir)/functions/*.?pp $(distdir)/functions
	cp -p $(srcdir)/ohxll/*.vcproj $(distdir)/ohxll
	cp -p $(srcdir)/ohxll/*.vcxproj $(distdir)/ohxll
	cp -p $(srcdir)/ohxll/*.vcxproj.filters $(distdir)/ohxll
	cp -p $(srcdir)/ohxllib/*.vcproj $(distdir)/ohxllib
	cp -p $(srcdir)/ohxllib/*.vcxproj $(distdir)/ohxllib
	cp -p $(srcdir)/ohxllib/*.vcxproj.filters $(distdir)/ohxllib
	cp -p $(srcdir)/register/*.?pp $(distdir)/register
	cp -p $(srcdir)/utilities/*.?pp $(distdir)/utilities

