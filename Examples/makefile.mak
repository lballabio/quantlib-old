
#
# makefile for QuantLib Examples under Borland C++
#
# $Id$

.autodepend
.silent

# Options
!ifdef DEBUG
    MAKE = $(MAKE) -DDEBUG
!endif

!ifdef QL_DIR
    MAKE = $(MAKE) -DQL_DIR="$(QL_DIR)"
!endif

# Primary target:
# QuantLib Examples
examples::
    cd DiscreteHedging
    $(MAKE)
    cd ..\EuropeanOption
    $(MAKE)
    cd ..\Swap
    $(MAKE)
    cd ..\BermudanSwaption
    $(MAKE)
    cd ..

# Check
check::
    cd EuropeanOption
    $(MAKE) check
    cd ..\BermudanSwaption
    $(MAKE) check
    cd ..\DiscreteHedging
    $(MAKE) check
    cd ..\Swap
    $(MAKE) check
    cd ..

# Clean up
clean::
    cd BermudanSwaption
    $(MAKE) clean
    cd ..\DiscreteHedging
    $(MAKE) clean
    cd ..\EuropeanOption
    $(MAKE) clean
    cd ..\Swap
    $(MAKE) clean
    cd ..
