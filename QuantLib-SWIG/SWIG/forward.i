/*
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


#ifndef quantlib_forward_i
#define quantlib_forward_i

%include instruments.i
%include termstructures.i
%include interestrate.i
%include fra.i

// Forward
%{
using QuantLib::Forward;
typedef boost::shared_ptr<Instrument> ForwardPtr;
%}

%rename(Forward) ForwardPtr;
class ForwardPtr : public boost::shared_ptr<Instrument>{
    public:
        %extend {
            // calculations
            Real forwardValue() {
                return boost::dynamic_pointer_cast<Forward>(*self)
                    -> forwardValue();
            }
            InterestRate impliedYield(
                            Real underlyingSpotValue,
                            Real forwardValue,
                            Date settlementDate,
                            Compounding compoundingConvention,
                            DayCounter dayCounter) {
                    return boost::dynamic_pointer_cast<Forward>(*self)
                        -> impliedYield(underlyingSpotValue, forwardValue,
                                        settlementDate, compoundingConvention, 
                                        dayCounter);
                }
        }
};


// FixedRateBondForward
%{
using QuantLib::FixedRateBondForward;
using QuantLib::FixedRateBond;
using QuantLib::BusinessDayConvention;
using QuantLib::Position;
typedef boost::shared_ptr<Instrument> FixedRateBondForwardPtr;
%}

%rename(FixedRateBondForward) FixedRateBondForwardPtr;
class FixedRateBondForwardPtr : public ForwardPtr {
    public:
        %extend {
            FixedRateBondForwardPtr(
                    const Date& valueDate,
                    const Date& maturityDate,
                    Position::Type type,
                    Real strike,
                    Natural settlementDays,
                    const DayCounter& dayCounter,
                    const Calendar& calendar,
                    BusinessDayConvention businessDayConvention,
                    const FixedRateBondPtr& fixedBond,
                    const Handle<YieldTermStructure>& discountCurve =
                                                Handle<YieldTermStructure>(),
                    const Handle<YieldTermStructure>& incomeDiscountCurve =
                                                Handle<YieldTermStructure>()) {
                const boost::shared_ptr<FixedRateBond>& fixedCouponBond = 
                    boost::dynamic_pointer_cast<FixedRateBond>(fixedBond);
                return new FixedRateBondForwardPtr(
                       new FixedRateBondForward(
                            valueDate, maturityDate, type, strike,
                            settlementDays, dayCounter, calendar,
                            businessDayConvention, fixedCouponBond,
                            discountCurve,  incomeDiscountCurve));
            }
            
            Real forwardPrice() {
                return boost::dynamic_pointer_cast<FixedRateBondForward>(*self)
                    -> forwardPrice();
            }

            Real cleanForwardPrice() {
                return boost::dynamic_pointer_cast<FixedRateBondForward>(*self)
                    -> cleanForwardPrice();
            }

            Real spotIncome(const Handle<YieldTermStructure>& incomeDiscountCurve) {
                return boost::dynamic_pointer_cast<FixedRateBondForward>(*self)
                    -> spotIncome(incomeDiscountCurve);
            }

            Real spotValue() {
                return boost::dynamic_pointer_cast<FixedRateBondForward>(*self)
                    -> spotValue();
            }
        }
};

#endif
