
/*
 Copyright (C) 2004, 2005 StatPro Italia srl

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
using QuantLib::FixedCouponBond;
using QuantLib::FloatingRateBond;
typedef boost::shared_ptr<Instrument> BondPtr;
typedef boost::shared_ptr<Instrument> ZeroCouponBondPtr;
typedef boost::shared_ptr<Instrument> FixedCouponBondPtr;
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
        BusinessDayConvention accrualConvention() const {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->accrualConvention();
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
  public:
    %extend {
        ZeroCouponBondPtr(const Date& issueDate,
                          const Date& maturityDate,
                          Integer settlementDays,
                          const DayCounter& dayCounter,
                          const Calendar& calendar,
                          BusinessDayConvention convention
                                                       = QuantLib::Following,
                          Real redemption = 100.0,
                          const Handle<YieldTermStructure>& discountCurve
                                              = Handle<YieldTermStructure>()) {
            return new ZeroCouponBondPtr(
                new ZeroCouponBond(issueDate, maturityDate, settlementDays,
                                   dayCounter, calendar, convention,
                                   redemption, discountCurve));
        }
    }
};

%rename(FixedCouponBond) FixedCouponBondPtr;
class FixedCouponBondPtr : public BondPtr {
  public:
    %extend {
        FixedCouponBondPtr(const Date& issueDate,
                           const Date& datedDate,
                           const Date& maturityDate,
                           Integer settlementDays,
                           const std::vector<Rate>& coupons,
                           Frequency couponFrequency,
                           const Calendar& calendar,
                           const DayCounter& dayCounter,
                           BusinessDayConvention accrualConvention
                                                       = QuantLib::Following,
                           BusinessDayConvention paymentConvention
                                                       = QuantLib::Following,
                           Real redemption = 100.0,
                           const Handle<YieldTermStructure>& discountCurve
                                              = Handle<YieldTermStructure>(),
                           const Date& stub = Date(),
                           bool fromEnd = true) {
            return new FixedCouponBondPtr(
                new FixedCouponBond(issueDate, datedDate, maturityDate,
                                    settlementDays, coupons, couponFrequency,
                                    calendar, 
				    dayCounter,  
				    accrualConvention,
				    paymentConvention,
                                    redemption, discountCurve, stub, fromEnd));
        }
    }
};

%rename(FloatingRateBond) FloatingRateBondPtr;
class FloatingRateBondPtr : public BondPtr {
  public:
    %extend {
        FloatingRateBondPtr(const Date& issueDate,
                            const Date& datedDate,
                            const Date& maturityDate,
                            Integer settlementDays,
                            const XiborPtr& index,
                            Integer fixingDays,
                            const std::vector<Spread>& spreads,
                            Frequency couponFrequency,
                            const Calendar& calendar,
                            const DayCounter& dayCounter,
			    BusinessDayConvention accrualConvention = 
			    Following,
		            BusinessDayConvention paymentConvention = 
			    Following,
                            Real redemption = 100.0,
                            const Handle<YieldTermStructure>& discountCurve
                                              = Handle<YieldTermStructure>(),
                            const Date& stub = Date(),
                            bool fromEnd = true) {
            boost::shared_ptr<Xibor> libor =
                boost::dynamic_pointer_cast<Xibor>(index);
            return new FloatingRateBondPtr(
                new FloatingRateBond(issueDate, datedDate, maturityDate,
                                     settlementDays, libor, fixingDays,
                                     spreads, couponFrequency,
                                     calendar, dayCounter, 
				     accrualConvention,
				     paymentConvention, 
                                     redemption, discountCurve,
                                     stub, fromEnd));
        }
    }
};


#endif
