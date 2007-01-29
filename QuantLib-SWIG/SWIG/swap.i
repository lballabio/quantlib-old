
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef quantlib_swap_i
#define quantlib_swap_i

%include instruments.i
%include termstructures.i
%include cashflows.i
%include timebasket.i

%{
using QuantLib::Swap;
using QuantLib::VanillaSwap;
typedef boost::shared_ptr<Instrument> SwapPtr;
typedef boost::shared_ptr<Instrument> VanillaSwapPtr;
typedef VanillaSwap::Type VanillaSwapType;

//copy vanilla swap types into global name space 
static const VanillaSwapType Receiver = VanillaSwap::Receiver;
static const VanillaSwapType Payer = VanillaSwap::Payer;
%}

%rename(Swap) SwapPtr;
class SwapPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        SwapPtr(const Handle<YieldTermStructure>& termStructure,
                const std::vector<boost::shared_ptr<CashFlow> >& firstLeg,
                const std::vector<boost::shared_ptr<CashFlow> >& secondLeg) {
            return new SwapPtr(new Swap(termStructure, firstLeg, secondLeg));
        }
        Date startDate() {
            return boost::dynamic_pointer_cast<Swap>(*self)->startDate();
        }
    }
};

%rename(VanillaSwap) VanillaSwapPtr;

enum VanillaSwapType { Receiver = -1, Payer = 1};


class VanillaSwapPtr : public SwapPtr {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fair-rate")        fairRate;
    %rename("fair-spread")      fairSpread;
    %rename("fixed-leg-BPS")    fixedLegBPS;
    %rename("floating-leg-BPS") floatingLegBPS;
    #endif
  public:
    %extend {
        VanillaSwapPtr(VanillaSwapType type, Real nominal,
                      const Schedule& fixedSchedule, Rate fixedRate,
                      const DayCounter& fixedDayCount,
                      const Schedule& floatSchedule,
                      const IborIndexPtr& index,
                      Spread spread,
                      const DayCounter& floatingDayCount,
                      const Handle<YieldTermStructure>& termStructure) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new VanillaSwapPtr(
                new VanillaSwap(type,
				nominal,fixedSchedule,fixedRate,
                               fixedDayCount,floatSchedule,libor,
                               spread,
                   floatingDayCount,
                   termStructure));
        }
        Rate fairRate() {
            return boost::dynamic_pointer_cast<VanillaSwap>(*self)->fairRate();
        }
        Spread fairSpread() {
            return boost::dynamic_pointer_cast<VanillaSwap>(*self)
                 ->fairSpread();
        }
        Real fixedLegBPS() {
            return boost::dynamic_pointer_cast<VanillaSwap>(*self)
                 ->fixedLegBPS();
        }
        Real floatingLegBPS() {
            return boost::dynamic_pointer_cast<VanillaSwap>(*self)
                 ->floatingLegBPS();
        }
    }
};


#endif
