
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
        // calculations
        Real cleanPrice() {
            return boost::dynamic_pointer_cast<Bond>(*self)->cleanPrice();
        }
        Real cleanPrice(Rate yield, const DayCounter &dc,
	     		Compounding compounding,
			Frequency frequency, 
                        const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->cleanPrice(yield,dc, compounding, frequency, settlement);
        }
        Real dirtyPrice() {
            return boost::dynamic_pointer_cast<Bond>(*self)->dirtyPrice();
        }
        Real dirtyPrice(Rate yield, const DayCounter &dc,
                        Compounding compounding,
			Frequency frequency, 
                        const Date& settlement = Date()) {
            return boost::dynamic_pointer_cast<Bond>(*self)
                ->dirtyPrice(yield,dc, compounding,
		frequency,
		settlement);
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
			settlement,
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


#endif
