
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2007 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

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
using QuantLib::DiscountingSwapEngine;

typedef boost::shared_ptr<Instrument> SwapPtr;
typedef boost::shared_ptr<Instrument> VanillaSwapPtr;
typedef boost::shared_ptr<PricingEngine> DiscountingSwapEnginePtr;
%}

%rename(Swap) SwapPtr;
class SwapPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        SwapPtr(const std::vector<boost::shared_ptr<CashFlow> >& firstLeg,
                const std::vector<boost::shared_ptr<CashFlow> >& secondLeg) {
            return new SwapPtr(new Swap(firstLeg, secondLeg));
        }
        Date startDate() {
            return boost::dynamic_pointer_cast<Swap>(*self)->startDate();
        }
        Date maturityDate() {
            return boost::dynamic_pointer_cast<Swap>(*self)->maturityDate();
        }
    }
};


#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_VanillaSwap) VanillaSwap;
#else
%ignore VanillaSwap;
#endif
class VanillaSwap {
  public:
    enum Type { Receiver = -1, Payer = 1 };
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
  private:
    VanillaSwap();
#endif
};

%rename(VanillaSwap) VanillaSwapPtr;
class VanillaSwapPtr : public SwapPtr {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fair-rate")        fairRate;
    %rename("fair-spread")      fairSpread;
    %rename("fixed-leg-BPS")    fixedLegBPS;
    %rename("floating-leg-BPS") floatingLegBPS;
    #endif
  public:
    %extend {
        static const VanillaSwap::Type Receiver = VanillaSwap::Receiver;
        static const VanillaSwap::Type Payer = VanillaSwap::Payer;
        VanillaSwapPtr(VanillaSwap::Type type, Real nominal,
                       const Schedule& fixedSchedule, Rate fixedRate,
                       const DayCounter& fixedDayCount,
                       const Schedule& floatSchedule,
                       const IborIndexPtr& index,
                       Spread spread,
                       const DayCounter& floatingDayCount) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new VanillaSwapPtr(
                    new VanillaSwap(type, nominal,fixedSchedule,fixedRate,
                                    fixedDayCount,floatSchedule,libor,
                                    spread, floatingDayCount));
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


%rename(DiscountingSwapEngine) DiscountingSwapEnginePtr;
class DiscountingSwapEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        DiscountingSwapEnginePtr(
                            const Handle<YieldTermStructure>& discountCurve) {
            return new DiscountingSwapEnginePtr(
                                    new DiscountingSwapEngine(discountCurve));
        }
    }
};



#endif
