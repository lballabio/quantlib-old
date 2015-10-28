/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2007 StatPro Italia srl
 Copyright (C) 2011 Lluis Pujol Bajador
 Copyright (C) 2015 Gouthaman Balaraman

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
        const Leg & leg(Size i){
            return boost::dynamic_pointer_cast<Swap>(*self)->leg(i);
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
	%rename ("fixed-leg-NPV") fixedLegNPV;
	%rename ("floating-leg-NPV") floatingLegNPV;
	%rename ("floating-leg") floatingLeg;
	%rename ("fixed-leg") fixedLeg;
	%rename ("fixed-schedule") fixedSchedule;
	%rename ("floating-schedule") floatingSchedule;
	%rename ("fixed-rate") fixedRate;
	%rename ("fixed-day-count") fixedDayCount;
	%rename ("floating-day-count") floatingDayCount;
	
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
        Real fixedLegNPV() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->fixedLegNPV();
        }
        Real floatingLegNPV() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->floatingLegNPV();
        }
        // Inspectors 
        const Leg& fixedLeg() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->fixedLeg();
        }
        const Leg& floatingLeg() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->floatingLeg();
        }
        Real nominal() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->nominal();
        }
        const Schedule& fixedSchedule() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->fixedSchedule();
        }
        const Schedule& floatingSchedule() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->floatingSchedule();
        }
        Rate fixedRate() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->fixedRate();
        }
        Spread spread() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->spread();
        }
        const DayCounter& floatingDayCount() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->floatingDayCount();
        }
        const DayCounter& fixedDayCount() {
	        return boost::dynamic_pointer_cast<VanillaSwap> (*self)
		        ->fixedDayCount();
        }
    }
};


%rename(DiscountingSwapEngine) DiscountingSwapEnginePtr;
class DiscountingSwapEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        DiscountingSwapEnginePtr(
                            const Handle<YieldTermStructure>& discountCurve,
                            const Date& settlementDate = Date(),
                            const Date& npvDate = Date()) {
            return new DiscountingSwapEnginePtr(
                                    new DiscountingSwapEngine(discountCurve,
                                                              boost::none,
                                                              settlementDate,
                                                              npvDate));
        }
        DiscountingSwapEnginePtr(
                            const Handle<YieldTermStructure>& discountCurve,
                            bool includeSettlementDateFlows,
                            const Date& settlementDate = Date(),
                            const Date& npvDate = Date()) {
            return new DiscountingSwapEnginePtr(
                         new DiscountingSwapEngine(discountCurve,
                                                   includeSettlementDateFlows,
                                                   settlementDate,
                                                   npvDate));
        }
    }
};


%{
using QuantLib::AssetSwap;
typedef boost::shared_ptr<Instrument> AssetSwapPtr;
%}

%rename(AssetSwap) AssetSwapPtr;
class AssetSwapPtr : public SwapPtr {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") AssetSwapPtr;
    #endif
  public:
    %extend {
        AssetSwapPtr(bool payFixedRate,
                     const BondPtr& bond,
                     Real bondCleanPrice,
                     const InterestRateIndexPtr& index,
                     Spread spread,
                     const Schedule& floatSchedule = Schedule(),
                     const DayCounter& floatingDayCount = DayCounter(),
                     bool parAssetSwap = true) {
            const boost::shared_ptr<Bond> b =
                boost::dynamic_pointer_cast<Bond>(bond);
            const boost::shared_ptr<IborIndex> i =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new AssetSwapPtr(
                new AssetSwap(payFixedRate,b,bondCleanPrice,i,spread,
                              floatSchedule,floatingDayCount,parAssetSwap));
        }
        Real fairCleanPrice() {
            return boost::dynamic_pointer_cast<AssetSwap>(*self)
                ->fairCleanPrice();
        }
        Spread fairSpread() {
            return boost::dynamic_pointer_cast<AssetSwap>(*self)
                ->fairSpread();
        }
    }
};


#endif
