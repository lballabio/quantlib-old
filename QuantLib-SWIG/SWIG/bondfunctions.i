
/*
 Copyright (C) 2013 Simon Shakeshaft

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

#ifndef quantlib_bond_functions_i
#define quantlib_bond_functions_i

%include bonds.i
%include common.i
%include types.i
%include daycounters.i


%{
using QuantLib::BondFunctions;
%}

class BondFunctions {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("start-date")                 startDate;
    %rename("maturity-date")              maturityDate;
    %rename("is-tradeable")               isTradable;
    %rename("previous-cash-flow-date")    previousCashFlowDate;
    %rename("next-cash-flow-date")        nextCashFlowDate;
    %rename("previous-cash-flow-amount")  previousCashFlowAmount;
    %rename("next-cash-flow-amount")      nextCashFlowAmount;
    %rename("previous-coupon-rate")       previousCouponRate;
    %rename("next-coupon-rate")           nextCouponRate;
    %rename("accrual-start-date")         accrualStartDate;
    %rename("accrual-end-date")           accrualEndDate;
    %rename("accrual-period")             accrualPeriod;
    %rename("accrual-days")               accrualDays;
    %rename("clean-price")                cleanPrice;
    %rename("atm-rate")                   atmRate;
    %rename("basis-point-value")          basisPointValue;
    %rename("yield-value-basis-point")    yieldValueBasisPoint;
    #endif
  public:
    %extend {
        static Date startDate(const BondPtr& bond) {
            return QuantLib::BondFunctions::startDate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)));
        }
        static Date maturityDate(const BondPtr& bond) {
            return QuantLib::BondFunctions::maturityDate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)));
        }
        static bool isTradable(const BondPtr& bond,
                               Date settlementDate = Date()) {
            return QuantLib::BondFunctions::isTradable(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static Date previousCashFlowDate(const BondPtr& bond,
                                         Date refDate = Date()) {
            return QuantLib::BondFunctions::previousCashFlowDate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    refDate);
        }
        static Date nextCashFlowDate(const BondPtr& bond,
                                     Date refDate = Date()) {
            return QuantLib::BondFunctions::nextCashFlowDate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    refDate);
        }
        static Real previousCashFlowAmount(const BondPtr& bond,
                                           Date refDate = Date()) {
            return QuantLib::BondFunctions::previousCashFlowAmount(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    refDate);
        }
        static Real nextCashFlowAmount(const BondPtr& bond,
                                       Date refDate = Date()) {
            return QuantLib::BondFunctions::nextCashFlowAmount(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    refDate);
        }
        static Rate previousCouponRate(const BondPtr& bond,
                                       Date settlementDate = Date()) {
            return QuantLib::BondFunctions::previousCouponRate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static Rate nextCouponRate(const BondPtr& bond,
                                   Date settlementDate = Date()) {
            return QuantLib::BondFunctions::nextCouponRate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static Date accrualStartDate(const BondPtr& bond,
                                     Date settlementDate = Date()) {
            return QuantLib::BondFunctions::accrualStartDate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static Date accrualEndDate(const BondPtr& bond,
                                   Date settlementDate = Date()) {
            return QuantLib::BondFunctions::accrualEndDate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static Time accrualPeriod(const BondPtr& bond,
                                  Date settlementDate = Date()) {
            return QuantLib::BondFunctions::accrualPeriod(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static BigInteger accrualDays(const BondPtr& bond,
                                      Date settlementDate = Date()) {
            return QuantLib::BondFunctions::accrualDays(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    settlementDate);
        }
        static Real cleanPrice(
                   const BondPtr& bond,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   Date settlementDate = Date()) {
            return QuantLib::BondFunctions::cleanPrice(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    *discountCurve,
                    settlementDate);
        }
        static Real bps(
                   const BondPtr& bond,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   Date settlementDate = Date()) {
            return QuantLib::BondFunctions::bps(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    *discountCurve,
                    settlementDate);
        }
        static Rate atmRate(
                   const BondPtr& bond,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   Date settlementDate = Date(),
                   Real cleanPrice = Null<Real>()) {
            return QuantLib::BondFunctions::atmRate(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    *discountCurve,
                    settlementDate,
                    cleanPrice);
        }
        static Real cleanPrice(const BondPtr& bond,
                               const InterestRate& yield,
                               Date settlementDate = Date()) {
            return QuantLib::BondFunctions::cleanPrice(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    yield,
                    settlementDate);
        }
        static Real cleanPrice(const BondPtr& bond,
                               Rate yield,
                               const DayCounter& dayCounter,
                               Compounding compounding,
                               Frequency frequency,
                               Date settlementDate = Date()) {
            return QuantLib::BondFunctions::cleanPrice(
                    *(boost::dynamic_pointer_cast<Bond>(bond)),
                    yield,
                    dayCounter,
                    compounding,
                    frequency,
                    settlementDate);
        }
        static Real bps(const BondPtr& bond,
                        const InterestRate& yield,
                        Date settlementDate = Date()) {
            return QuantLib::BondFunctions::bps(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        settlementDate);
        }
        static Real bps(const BondPtr& bond,
                        Rate yield,
                        const DayCounter& dayCounter,
                        Compounding compounding,
                        Frequency frequency,
                        Date settlementDate = Date()) {
            return QuantLib::BondFunctions::bps(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        dayCounter,
                        compounding,
                        frequency,
                        settlementDate);
        }
        static Rate yield(const BondPtr& bond,
                          Real cleanPrice,
                          const DayCounter& dayCounter,
                          Compounding compounding,
                          Frequency frequency,
                          Date settlementDate = Date(),
                          Real accuracy = 1.0e-10,
                          Size maxIterations = 100,
                          Rate guess = 0.05) {
            return QuantLib::BondFunctions::yield(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        cleanPrice,
                        dayCounter,
                        compounding,
                        frequency,
                        settlementDate,
                        accuracy,
                        maxIterations,
                        guess);
        }
        static Time duration(const BondPtr& bond,
                             const InterestRate& yield,
                             Duration::Type type = Duration::Modified,
                             Date settlementDate = Date() ) {
            return QuantLib::BondFunctions::duration(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        type,
                        settlementDate);
        }
        static Time duration(const BondPtr& bond,
                        Rate yield,
                        const DayCounter& dayCounter,
                        Compounding compounding,
                        Frequency frequency,
                        Duration::Type type = Duration::Modified,
                        Date settlementDate = Date()) {
            return QuantLib::BondFunctions::yield(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        dayCounter,
                        compounding,
                        frequency,
                        settlementDate);
        }
        static Real convexity(const BondPtr& bond,
                              const InterestRate& yield,
                              Date settlementDate = Date()) {
            return QuantLib::BondFunctions::convexity(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        settlementDate);
        }
        static Real convexity(const BondPtr& bond,
                              Rate yield,
                              const DayCounter& dayCounter,
                              Compounding compounding,
                              Frequency frequency,
                              Date settlementDate = Date()) {
            return QuantLib::BondFunctions::convexity(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        dayCounter,
                        compounding,
                        frequency,
                        settlementDate);
        }
        static Real basisPointValue(const BondPtr& bond,
                                    const InterestRate& yield,
                                    Date settlementDate = Date()) {
            return QuantLib::BondFunctions::basisPointValue(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        settlementDate);
        }
        static Real basisPointValue(const BondPtr& bond,
                                    Rate yield,
                                    const DayCounter& dayCounter,
                                    Compounding compounding,
                                    Frequency frequency,
                                    Date settlementDate = Date()) {
            return QuantLib::BondFunctions::basisPointValue(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        dayCounter,
                        compounding,
                        frequency,
                        settlementDate);
        }
        static Real yieldValueBasisPoint(const BondPtr& bond,
                                         const InterestRate& yield,
                                         Date settlementDate = Date()) {
            return QuantLib::BondFunctions::yieldValueBasisPoint(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        settlementDate);
        }
        static Real yieldValueBasisPoint(const BondPtr& bond,
                                         Rate yield,
                                         const DayCounter& dayCounter,
                                         Compounding compounding,
                                         Frequency frequency,
                                         Date settlementDate = Date()) {
            return QuantLib::BondFunctions::yieldValueBasisPoint(
                        *(boost::dynamic_pointer_cast<Bond>(bond)),
                        yield,
                        dayCounter,
                        compounding,
                        frequency,
                        settlementDate);
        }
    }
};


#endif
