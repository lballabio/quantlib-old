
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
typedef Handle<Instrument> SwapHandle;
typedef Handle<Instrument> SimpleSwapHandle;
%}

%rename(Swap) SwapHandle;
class SwapHandle : public Handle<Instrument> {
  public:
    %extend {
        SwapHandle(const std::vector<Handle<CashFlow> >& firstLeg,
                   const std::vector<Handle<CashFlow> >& secondLeg,
                   const RelinkableHandle<TermStructure>& termStructure) {
            return new SwapHandle(new Swap(firstLeg, secondLeg, 
                                           termStructure));
        }
        Date startDate() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<Swap>(*self)->startDate();
            %#else
            return Handle<Swap>(*self)->startDate();
            %#endif
        }
        Date maturity() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<Swap>(*self)->maturity();
            %#else
            return Handle<Swap>(*self)->maturity();
            %#endif
        }
        double firstLegBPS() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<Swap>(*self)->firstLegBPS();
            %#else
            return Handle<Swap>(*self)->firstLegBPS();
            %#endif
        }
        double secondLegBPS() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<Swap>(*self)->secondLegBPS();
            %#else
            return Handle<Swap>(*self)->secondLegBPS();
            %#endif
        }
        TimeBasket sensitivity() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<Swap>(*self)->sensitivity();
            %#else
            return Handle<Swap>(*self)->sensitivity();
            %#endif
        }
    }
};

#if defined(SWIGRUBY)
// too many parameters for a native function.
// we'll have to group some
%inline %{
class FixedSwapLeg {
  public:
    FixedSwapLeg(int fixedFrequency, Rate fixedRate,
                 bool fixedIsAdjusted, const DayCounter& fixedDayCount)
    : fixedFrequency(fixedFrequency), fixedRate(fixedRate),
      fixedIsAdjusted(fixedIsAdjusted), fixedDayCount(fixedDayCount) {}
    int fixedFrequency;
    Rate fixedRate;
    bool fixedIsAdjusted;
    DayCounter fixedDayCount;
};
class FloatingSwapLeg {
  public:
    FloatingSwapLeg(int floatingFrequency, XiborHandle index, 
                    int indexFixingDays, Spread spread)
    : floatingFrequency(floatingFrequency), index(index),
      indexFixingDays(indexFixingDays), spread(spread) {}
    int floatingFrequency;
    XiborHandle index;
    int indexFixingDays;
    Spread spread;
};
%}
#endif


%rename(SimpleSwap) SimpleSwapHandle;
class SimpleSwapHandle : public SwapHandle {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fair-rate")        fairRate;
    %rename("fair-spread")      fairSpread;
    %rename("fixed-leg-BPS")    fixedLegBPS;
    %rename("floating-leg-BPS") floatingLegBPS;
    #endif
  public:
    %extend {
    #if defined(SWIGRUBY)
        SimpleSwapHandle(bool payFixedRate, const Date& startDate, 
                         int n, TimeUnit unit, const Calendar& calendar, 
                         RollingConvention rollingConvention, double nominal,
                         const FixedSwapLeg& fixedLeg,
                         const FloatingSwapLeg& floatingLeg,
                         const RelinkableHandle<TermStructure>& 
                                                             termStructure) {
            %#if defined(HAVE_BOOST)
            Handle<Xibor> libor = 
                 boost::dynamic_pointer_cast<Xibor>(floatingLeg.index);
            %#else
            Handle<Xibor> libor = floatingLeg.index;
            %#endif
            return new SimpleSwapHandle(
                new SimpleSwap(
                    payFixedRate, startDate, n, unit, calendar,
                    rollingConvention, nominal, 
                    fixedLeg.fixedFrequency, fixedLeg.fixedRate, 
                    fixedLeg.fixedIsAdjusted, fixedLeg.fixedDayCount, 
                    floatingLeg.floatingFrequency, libor, 
                    floatingLeg.indexFixingDays, floatingLeg.spread, 
                    termStructure));
        }
    #else
        SimpleSwapHandle(bool payFixedRate, const Date& startDate, 
                         int n, TimeUnit unit, const Calendar& calendar, 
                         RollingConvention rollingConvention, double nominal, 
                         int fixedFrequency, Rate fixedRate,
                         bool fixedIsAdjusted, const DayCounter& fixedDayCount,
                         int floatingFrequency, const XiborHandle& index, 
                         int indexFixingDays, Spread spread, 
                         const RelinkableHandle<TermStructure>& 
                                                            termStructure) {
            %#if defined(HAVE_BOOST)
            Handle<Xibor> libor = boost::dynamic_pointer_cast<Xibor>(index);
            %#else
            Handle<Xibor> libor = index;
            %#endif
            return new SimpleSwapHandle(
                new SimpleSwap(payFixedRate, startDate, n, unit, calendar,
                               rollingConvention, nominal, fixedFrequency, 
                               fixedRate, fixedIsAdjusted, fixedDayCount, 
                               floatingFrequency, libor, indexFixingDays, 
                               spread, termStructure));
        }
        SimpleSwapHandle(bool payFixedRate, double nominal, 
                         const Schedule& fixedSchedule, Rate fixedRate,
                         const DayCounter& fixedDayCount,
                         const Schedule& floatSchedule,
                         const XiborHandle& index,
                         int indexFixingDays, Spread spread,
                         const RelinkableHandle<TermStructure>& 
                                                          termStructure) {
            %#if defined(HAVE_BOOST)
            Handle<Xibor> libor = boost::dynamic_pointer_cast<Xibor>(index);
            %#else
            Handle<Xibor> libor = index;
            %#endif
            return new SimpleSwapHandle(
                new SimpleSwap(payFixedRate,nominal,fixedSchedule,fixedRate,
                               fixedDayCount,floatSchedule,libor,
                               indexFixingDays,spread,termStructure));
        }
    #endif
        Rate fairRate() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)->fairRate();
            %#else
            return Handle<SimpleSwap>(*self)->fairRate();
            %#endif
        }
        Spread fairSpread() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)
                 ->fairSpread();
            %#else
            return Handle<SimpleSwap>(*self)->fairSpread();
            %#endif
        }
        double fixedLegBPS() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)
                 ->fixedLegBPS();
            %#else
            return Handle<SimpleSwap>(*self)->fixedLegBPS();
            %#endif
        }
        double floatingLegBPS() {
            %#if defined(HAVE_BOOST)
            return boost::dynamic_pointer_cast<SimpleSwap>(*self)
                 ->floatingLegBPS();
            %#else
            return Handle<SimpleSwap>(*self)->floatingLegBPS();
            %#endif
        }
    }
};


#endif
