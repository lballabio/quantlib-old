
/*
 Copyright (C) 2004 StatPro Italia srl

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

%{
using QuantLib::Bond;
using QuantLib::FixedCouponBond;
typedef boost::shared_ptr<Instrument> BondPtr;
typedef boost::shared_ptr<Instrument> FixedCouponBondPtr;
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
        Date settlementDate() {
            return boost::dynamic_pointer_cast<Bond>(*self)->settlementDate();
        }
        Real cleanPrice(Rate yield, const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->cleanPrice(yield,settlement);
        }
        Real dirtyPrice(Rate yield, const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->dirtyPrice(yield,settlement);
        }
        Real yield(Real cleanPrice,
                   const Date& settlement = Date(),
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->yield(cleanPrice,settlement,accuracy,maxEvaluations);
        }
        Real accruedAmount(const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->accruedAmount(settlement);
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
                           Rate coupon,
                           Frequency couponFrequency,
                           const DayCounter& dayCounter,
                           const Calendar& calendar,
                           BusinessDayConvention convention
                                                       = QuantLib::Following,
                           Real redemption = 100.0) {
            return new FixedCouponBondPtr(
                new FixedCouponBond(issueDate, datedDate, maturityDate,
                                    settlementDays, coupon, couponFrequency,
                                    dayCounter, calendar, convention,
                                    redemption));
        }
    }
};


#endif
