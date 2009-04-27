
/*
 Copyright (C) 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2009 Joseph Malicki

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

#ifndef quantlib_bonds_i
#define quantlib_bonds_i

%include instruments.i
%include calendars.i
%include daycounters.i
%include cashflows.i
%include interestrate.i
%include indexes.i

%{
using QuantLib::Bond;
using QuantLib::ZeroCouponBond;
using QuantLib::FixedRateBond;
using QuantLib::FloatingRateBond;
using QuantLib::DiscountingBondEngine;

typedef boost::shared_ptr<Instrument> BondPtr;
typedef boost::shared_ptr<Instrument> ZeroCouponBondPtr;
typedef boost::shared_ptr<Instrument> FixedRateBondPtr;
typedef boost::shared_ptr<Instrument> FloatingRateBondPtr;
typedef boost::shared_ptr<PricingEngine> DiscountingBondEnginePtr;
%}

%rename(Bond) BondPtr;
class BondPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGPYTHON) || defined (SWIGRUBY)
    %rename(bondYield) yield;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("next-coupon-rate")      nextCouponRate;
    %rename("previous-coupon-rate")  previousCouponRate;
    %rename("settlement-days")       settlementDays;
    %rename("settlement-date")       settlementDate;
    %rename("maturity-date")         maturityDate;
    %rename("issue-date")            issueDate;
    %rename("clean-price")           cleanPrice;
    %rename("dirty-price")           dirtyPrice;
    %rename("settlement-value")      settlementValue;
    %rename("accrued-amount")        accruedAmount;
    #endif
  protected:
    BondPtr();
  public:
    %extend {
        // public functions
        Rate nextCouponRate(const Date& d = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->nextCouponRate();
        }
        Rate previousCouponRate(const Date& d = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->previousCouponRate();
        }
        // inspectors
        Natural settlementDays() const {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->settlementDays();
        }
        Date settlementDate() {
            return boost::dynamic_pointer_cast<Bond>(*self)->settlementDate();
        }
        Date maturityDate() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->maturityDate();
        }
        Date issueDate() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->issueDate();
        }
        std::vector<boost::shared_ptr<CashFlow> > cashflows() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->cashflows();
        }
        std::vector<boost::shared_ptr<CashFlow> > redemptions() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->redemptions();
        }
        boost::shared_ptr<CashFlow> redemption() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->redemption();
        }
        Calendar calendar() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->calendar();
        }
        std::vector<Real> notionals() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->notionals();
        }
        Real notional(Date d = Date()) const {
            return boost::dynamic_pointer_cast<Bond>(*self)->notional(d);
        }
        // calculations
        Real cleanPrice() {
            return boost::dynamic_pointer_cast<Bond>(*self)->cleanPrice();
        }
        Real cleanPrice(Rate yield,
                        const DayCounter &dc,
                        Compounding compounding,
                        Frequency frequency,
                        const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->cleanPrice(yield,dc, compounding, frequency, settlement);
        }
        Real dirtyPrice() {
            return boost::dynamic_pointer_cast<Bond>(*self)->dirtyPrice();
        }
        Real dirtyPrice(Rate yield,
                        const DayCounter &dc,
                        Compounding compounding,
                        Frequency frequency,
                        const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->dirtyPrice(yield,dc, compounding,
                             frequency, settlement);
        }
        Real yield(const DayCounter& dc,
                   Compounding compounding,
                   Frequency freq,
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->yield(dc,compounding,freq,accuracy,maxEvaluations);
        }
        Real yield(Real cleanPrice,
                   const DayCounter& dc,
                   Compounding compounding,
                   Frequency freq,
                   const Date& settlement = Date(),
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->yield(cleanPrice,dc,compounding,freq,
                        settlement,accuracy,maxEvaluations);
        }
        Real accruedAmount(const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->accruedAmount(settlement);
        }
        Real settlementValue() const {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->settlementValue();
        }
        Real settlementValue(Real cleanPrice) const {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->settlementValue(cleanPrice);
        }
    }
};


#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("clean-price-from-z-spread") cleanPriceFromZSpread;
%rename("dirty-price-from-z-spread") dirtyPriceFromZSpread;
#endif

%inline %{

    Real cleanPriceFromZSpread(
                   const BondPtr& bond,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   Spread zSpread,
                   const DayCounter& dc,
                   Compounding compounding,
                   Frequency freq,
                   const Date& settlementDate = Date()) {
        return QuantLib::BondFunctions::cleanPrice(
                                  *(boost::dynamic_pointer_cast<Bond>(bond)),
                                  discountCurve,
                                  zSpread, dc, compounding,
                                  freq, settlementDate);
    }

%}



%rename(ZeroCouponBond) ZeroCouponBondPtr;
class ZeroCouponBondPtr : public BondPtr {
    %feature("kwargs") ZeroCouponBondPtr;
  public:
    %extend {
        ZeroCouponBondPtr(
                Natural settlementDays,
                const Calendar &calendar,
                Real faceAmount,
                const Date & maturityDate,
                BusinessDayConvention paymentConvention = QuantLib::Following,
                Real redemption = 100.0,
                const Date& issueDate = Date()) {
            return new ZeroCouponBondPtr(
                new ZeroCouponBond(settlementDays, calendar, faceAmount,
                                   maturityDate,
                                   paymentConvention, redemption,
                                   issueDate));
        }
    }
};

%rename(FixedRateBond) FixedRateBondPtr;
class FixedRateBondPtr : public BondPtr {
    %feature("kwargs") FixedRateBondPtr;
  public:
    %extend {
        FixedRateBondPtr(
                Integer settlementDays,
                Real faceAmount,
                const Schedule &schedule,
                const std::vector<Rate>& coupons,
                const DayCounter& paymentDayCounter,
                BusinessDayConvention paymentConvention = QuantLib::Following,
                Real redemption = 100.0,
                Date issueDate = Date()) {
            return new FixedRateBondPtr(
                new FixedRateBond(settlementDays, faceAmount,
                                  schedule, coupons, paymentDayCounter,
                                  paymentConvention, redemption,
                                  issueDate));
        }
        Frequency frequency() const {
            return boost::dynamic_pointer_cast<FixedRateBond>(*self)
                ->frequency();
        }
        DayCounter dayCounter() const {
            return boost::dynamic_pointer_cast<FixedRateBond>(*self)
                ->dayCounter();
        }
    }
};

%rename(FloatingRateBond) FloatingRateBondPtr;
class FloatingRateBondPtr : public BondPtr {
    %feature("kwargs") FloatingRateBondPtr;
  public:
    %extend {
        FloatingRateBondPtr(Size settlementDays,
                            Real faceAmount,
                            const Schedule& schedule,
                            const IborIndexPtr& index,
                            const DayCounter& paymentDayCounter,
                            BusinessDayConvention paymentConvention,
                            Size fixingDays,
                            const std::vector<Real>& gearings,
                            const std::vector<Spread>& spreads,
                            const std::vector<Rate>& caps,
                            const std::vector<Rate>& floors,
                            bool inArrears,
                            Real redemption,
                            const Date& issueDate) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new FloatingRateBondPtr(
                new FloatingRateBond(settlementDays,
                                     faceAmount,
                                     schedule,
                                     libor,
                                     paymentDayCounter,
                                     paymentConvention,
                                     fixingDays,
                                     gearings,
                                     spreads,
                                     caps,
                                     floors,
                                     inArrears,
                                     redemption,
                                     issueDate));
        }
    }
};


%rename(DiscountingBondEngine) DiscountingBondEnginePtr;
class DiscountingBondEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        DiscountingBondEnginePtr(
                            const Handle<YieldTermStructure>& discountCurve) {
            return new DiscountingBondEnginePtr(
                                    new DiscountingBondEngine(discountCurve));
        }
    }
};


#endif
