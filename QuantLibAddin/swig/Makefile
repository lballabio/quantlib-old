
SWIG = ../../../swig/preinst-swig

all: SWIG ValueObjects AddinObjects AddinCpp Main

.PHONY: SWIG
SWIG: quantlib.i ../../../swig/swig
	$(SWIG) -prefix ql -c++ -objecthandler quantlib.i

.PHONY: ValueObjects
ValueObjects:
	$(MAKE) -C ValueObjects

.PHONY: AddinObjects
AddinObjects:
	$(MAKE) -C AddinObjects

.PHONY: AddinCpp
AddinCpp:
	$(MAKE) -C AddinCpp

.PHONY: Main
Main:
	$(MAKE) -C Main

clean :
	$(MAKE) -C ValueObjects clean
	$(MAKE) -C AddinObjects clean
	$(MAKE) -C AddinCpp clean
	$(MAKE) -C Main clean
