
EXTRA_DIST = \
    callingrange.cpp \
    callingrange.hpp \
    functioncall.cpp \
    functioncall.hpp \
    loop.hpp \
    objecthandlerxl.cpp \
    objecthandlerxl.hpp \
    objhandlerxl.hpp \
    rangereference.cpp \
    rangereference.hpp
 
dist-hook:
	mkdir -p $(distdir)/Functions $(distdir)/Register $(distdir)/ohxll $(distdir)/ohxllib
	cp -p $(srcdir)/Functions/*.?pp $(distdir)/Functions
	cp -p $(srcdir)/Register/*.?pp $(distdir)/Register
	cp -p $(srcdir)/ohxll/*.vcproj $(distdir)/ohxll
	cp -p $(srcdir)/ohxllib/*.vcproj $(distdir)/ohxllib

