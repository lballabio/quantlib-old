
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
using QuantLib::SimpleSwap;
typedef boost::shared_ptr<Instrument> SwapPtr;
typedef boost::shared_ptr<Instrument> SimpleSwapPtr;
%}

%rename(Swap) SwapPtr;
class SwapPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        SwapPtr(const std::vector<boost::shared_ptr<CashFlow> >& firstLeg,
                const std::vector<boost::shared_ptr<CashFlow> >& secondLeg,
                const Handle<YieldTermStructure>& termStructure) {
            return new SwapPtr(new Swap(firstLeg, secondLeg,
                                        termStructure));
        }
        Date startDate() {
            return boost::dynamic_pointer_cast<Swap>(*self)->startDate();
        }
        Date maturity() {
            return boost::dynamic_pointer_cast<Swap>(*self)->maturity();
        }
        Real firstLegBPS() {
            return boost::dynamic_pointer_cast<Swap>(*self)->firstLegBPS();
        }
        Real secondLegBPS() {
            return boost::dynamic_pointer_cast<Swap>(*self)->secondLegBPS();
        }
        TimeBasket sensitivity() {
            return boost::dynamic_pointer_cast<Swap>(*self)->sensitivity();
        }
    }
};


%rename(SimpleSwap) SimpleSwapPtr;
class SimpleSwapPtr : public SwapPtr {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fair-rate")        fairRate;
    %rename("fair-spread")      fairSpread;
    %rename("fixed-leg-BPS")    fixedLegBPS;
    %rename("floating-leg-BPS") floatingLegBPS;
    #endif
  public:
    %extend {
        SimpleSwapPtr(bool payFixedRate, Real nominal,
                      const Schedule& fixedSchedule, Rate fixedRate,
                      const DayCounter& fixedDayCount,
                      const Schedule& floatSchedule,
                      const XiborPtr& index,
                      Integer indexFixingDays, Spread spread,
                      const Handle<YieldTermStructure>& termStructure) {
            boost::shared_ptr<Xibor> libor =
                boost::dynamic_pointer_cast<Xibor>(index);
            return new SimpleSwapPtr(
                new SimpleSwap(payFixedRate,nominal,fixedSchedule,fixedRate,
                               fixedDayCount,floatSchedule,libor,
                               indexFixingDays,spread,termStructure));
        }
        Rate fairRate() {
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)->fairRate();
        }
        Spread fairSpread() {
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)
                 ->fairSpread();
        }
        Real fixedLegBPS() {
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)
                 ->fixedLegBPS();
        }
        Real floatingLegBPS() {
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)
                 ->floatingLegBPS();
        }
    }
};


#endif
