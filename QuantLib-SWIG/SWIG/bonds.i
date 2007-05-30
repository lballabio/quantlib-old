
/*
 Copyright (C) 2004, 2005, 2006, 2007 StatPro Italia srl

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

typedef boost::shared_ptr<Instrument> BondPtr;
typedef boost::shared_ptr<Instrument> ZeroCouponBondPtr;
typedef boost::shared_ptr<Instrument> FixedRateBondPtr;
typedef boost::shared_ptr<Instrument> FloatingRateBondPtr;
%}

%rename(Bond) BondPtr;
class BondPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGPYTHON) || defined (SWIGRUBY)
    %rename(bondYield) yield;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("settlement-date") settlementDate;
    %rename("clean-price")     cleanPrice;
    %rename("dirty-price")     dirtyPrice;
    %rename("accrued-amount")  accruedAmount;
    #endif
  protected:
    BondPtr();
  public:
    %extend {
        // inspectors
        Date settlementDate() {
            return boost::dynamic_pointer_cast<Bond>(*self)->settlementDate();
        }
        std::vector<boost::shared_ptr<CashFlow> > cashflows() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->cashflows();
        }
        boost::shared_ptr<CashFlow> redemption() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->redemption();
        }
        Calendar calendar() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->calendar();
        }
        BusinessDayConvention paymentConvention() const {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->paymentConvention();
        }
        DayCounter dayCounter() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->dayCounter();
        }
        Frequency frequency() const {
            return boost::dynamic_pointer_cast<Bond>(*self)->frequency();
        }
        // calculations
        Real cleanPrice() {
            return boost::dynamic_pointer_cast<Bond>(*self)->cleanPrice();
        }
        Real cleanPrice(Rate yield, Compounding compounding,
                        const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->cleanPrice(yield,compounding,settlement);
        }
        Real dirtyPrice() {
            return boost::dynamic_pointer_cast<Bond>(*self)->dirtyPrice();
        }
        Real dirtyPrice(Rate yield, Compounding compounding,
                        const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->dirtyPrice(yield,compounding,settlement);
        }
        Real yield(Compounding compounding,
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->yield(compounding,accuracy,maxEvaluations);
        }
        Real yield(Real cleanPrice,
                   Compounding compounding,
                   const Date& settlement = Date(),
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->yield(cleanPrice,compounding,settlement,
                        accuracy,maxEvaluations);
        }
        Real accruedAmount(const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->accruedAmount(settlement);
        }
    }
};


%rename(ZeroCouponBond) ZeroCouponBondPtr;
class ZeroCouponBondPtr : public BondPtr {
    %feature("kwargs") ZeroCouponBondPtr;
  public:
    %extend {
        ZeroCouponBondPtr(
                Integer settlementDays,
                Real faceAmount,
                const Calendar &calendar,
                const Date & maturityDate,
                const DayCounter& dayCounter,
                BusinessDayConvention paymentConvention = QuantLib::Following,
                Real redemption = 100.0,
                const Date& issueDate = Date(),
                const Handle<YieldTermStructure>& discountCurve
                                              = Handle<YieldTermStructure>()) {
            return new ZeroCouponBondPtr(
                new ZeroCouponBond(settlementDays, faceAmount,
                                   calendar, maturityDate, dayCounter,
                                   paymentConvention, redemption,
                                   issueDate, discountCurve));
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
                Date issueDate = Date(),
                const Handle<YieldTermStructure>& discountCurve
                                              = Handle<YieldTermStructure>()) {
            return new FixedRateBondPtr(
                new FixedRateBond(settlementDays, faceAmount,
                                  schedule, coupons, paymentDayCounter,
                                  paymentConvention, redemption,
                                  issueDate, discountCurve));
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
                            const boost::shared_ptr<IborIndex>& index,
                            const DayCounter& paymentDayCounter,
                            BusinessDayConvention paymentConvention,
                            Size fixingDays,
                            const std::vector<Real>& gearings,
                            const std::vector<Spread>& spreads,
                            const std::vector<Rate>& caps,
                            const std::vector<Rate>& floors,
                            bool inArrears,
                            Real redemption,
                            const Date& issueDate,
                            const Handle<YieldTermStructure>& discountCurve
                                              = Handle<YieldTermStructure>()) {
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
                                     issueDate,
                                     discountCurve));
        }
    }
};


#endif
