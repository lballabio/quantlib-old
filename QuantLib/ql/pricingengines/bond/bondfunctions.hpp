/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2008, 2009, 2010 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2009 StatPro Italia srl
 Copyright (C) 2009 Nathan Abbott

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

/*! \file bondfunctions.hpp
    \brief bond functions
*/

#ifndef quantlib_bond_functions_hpp
#define quantlib_bond_functions_hpp

#include <ql/cashflows/duration.hpp>
#include <ql/cashflow.hpp>
#include <ql/interestrate.hpp>
#include <boost/shared_ptr.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/pricingengines/bond/bondfunctionsimpl.hpp>


namespace QuantLib {

    // forward declarations
	template<class>
	class Bond_t;
	typedef Bond_t<double> Bond;
    class DayCounter;

    //! Bond adapters of CashFlows functions
    /*! See CashFlows for functions' documentation.

        These adapters calls into CashFlows functions passing as input the
        Bond cashflows, the dirty price (i.e. npv) calculated from clean
        price, the bond settlement date (unless another date is given), zero
        ex-dividend days, and excluding any cashflow on the settlement date.

        Prices are always clean, as per market convention.
    */
    struct BondFunctions {
        //! \name Date inspectors
        //@{
        inline static Date startDate(const Bond& bond) {
			return impl::startDate<Real>(bond);
		}
        inline static Date maturityDate(const Bond& bond) {
			return impl::maturityDate<Real>(bond);
		}
        inline static bool isTradable(const Bond& bond,
			Date settlementDate = Date()) {
			return impl::isTradable<Real>(bond,settlementDate);
		}
        //@}

        //! \name CashFlow inspectors
        //@{
        inline static Leg::const_reverse_iterator
        previousCashFlow(const Bond& bond,
                         Date refDate = Date()) {
			return impl::previousCashFlow<Real>(bond,refDate);
		}
        inline static Leg::const_iterator nextCashFlow(const Bond& bond,
                                                Date refDate = Date()) {
			return impl::nextCashFlow<Real>(bond,refDate);
		}
        inline static Date previousCashFlowDate(const Bond& bond,
                                         Date refDate = Date()) {
			return impl::previousCashFlowDate<Real>(bond,refDate);
		}
        inline static Date nextCashFlowDate(const Bond& bond,
                                     Date refDate = Date()) {
			return impl::nextCashFlowDate<Real>(bond,refDate);
		}
        inline static Real previousCashFlowAmount(const Bond& bond,
                                           Date refDate = Date()) {
			return impl::previousCashFlowAmount<Real>(bond,refDate);
		}
        inline static Real nextCashFlowAmount(const Bond& bond,
                                       Date refDate = Date()) {
			return impl::nextCashFlowAmount<Real>(bond,refDate);
		}
        //@}

        //! \name Coupon inspectors
        //@{
        inline static Rate previousCouponRate(const Bond& bond,
                                       Date settlementDate = Date()) {
			return impl::previousCouponRate<Real>(bond,settlementDate);
		}
        inline static Rate nextCouponRate(const Bond& bond,
                                   Date settlementDate = Date()) {
			return impl::nextCouponRate<Real>(bond,settlementDate);
		}
        inline static Date accrualStartDate(const Bond& bond,
                                     Date settlementDate = Date()) {
			return impl::accrualStartDate<Real>(bond,settlementDate);
		}
        inline static Date accrualEndDate(const Bond& bond,
                                   Date settlementDate = Date()) {
			return impl::accrualEndDate<Real>(bond,settlementDate);
		}
        inline static Date referencePeriodStart(const Bond& bond,
                                         Date settlementDate = Date()) {
			return impl::referencePeriodStart<Real>(bond,settlementDate);
		}
        inline static Date referencePeriodEnd(const Bond& bond,
                                       Date settlementDate = Date()) {
			return impl::referencePeriodEnd<Real>(bond,settlementDate);
		}
        inline static Time accrualPeriod(const Bond& bond,
                                  Date settlementDate = Date()) {
			return impl::accrualPeriod<Real>(bond,settlementDate);
		}
        inline static BigInteger accrualDays(const Bond& bond,
                                      Date settlementDate = Date()) {
			return impl::accrualDays<Real>(bond,settlementDate);
		}
        inline static Time accruedPeriod(const Bond& bond,
                                  Date settlementDate = Date()) {
			return impl::accruedPeriod<Real>(bond,settlementDate);
		}
        inline static BigInteger accruedDays(const Bond& bond,
                                      Date settlementDate = Date()) {
			return impl::accruedDays<Real>(bond,settlementDate);
		}
        inline static Real accruedAmount(const Bond& bond,
                                  Date settlementDate = Date()) {
			return impl::accruedAmount<Real>(bond,settlementDate);
		}
        //@}

        //! \name YieldTermStructure functions
        //@{
        inline static Real cleanPrice(const Bond& bond,
                               const YieldTermStructure& discountCurve,
                               Date settlementDate = Date()) {
			return impl::cleanPrice<Real>(bond,discountCurve,settlementDate);
		}
        inline static Real bps(const Bond& bond,
                        const YieldTermStructure& discountCurve,
                        Date settlementDate = Date()) {
			return impl::bps<Real>(bond,discountCurve,settlementDate);
		}
        inline static Rate atmRate(const Bond& bond,
                            const YieldTermStructure& discountCurve,
                            Date settlementDate = Date(),
                            Real cleanPrice = Null<Real>()) {
			return impl::atmRate<Real>(bond,discountCurve,settlementDate,cleanPrice);
		}

        //@}

        //! \name Yield (a.k.a. Internal Rate of Return, i.e. IRR) functions
        //@{
        inline static Real cleanPrice(const Bond& bond,
                               const InterestRate& yield,
                               Date settlementDate = Date()) {
			return impl::cleanPrice<Real>(bond,yield,settlementDate);
		}
        inline static Real cleanPrice(const Bond& bond,
                               Rate yield,
                               const DayCounter& dayCounter,
                               Compounding compounding,
                               Frequency frequency,
							   Date settlementDate = Date()) {
			return impl::cleanPrice<Real>(bond,yield,dayCounter,compounding,frequency,settlementDate);
		}
		inline static Real dirtyPrice(const Bond& bond,
							   const InterestRate& yield,
			                   Date settlementDate = Date()) {
			return impl::dirtyPrice<Real>(bond,yield,settlementDate);
		}
		inline static Real dirtyPrice(const Bond& bond,
			                   Rate yield,
			                   const DayCounter& dayCounter,
			                   Compounding compounding,
			                   Frequency frequency,
			                   Date settlementDate = Date()) {
			return impl::dirtyPrice<Real>(bond,yield,dayCounter,compounding,frequency,settlementDate);
		}
        inline static Real bps(const Bond& bond,
                        const InterestRate& yield,
                        Date settlementDate = Date()) {
			return impl::bps<Real>(bond,yield,settlementDate);
		}
        inline static Real bps(const Bond& bond,
                        Rate yield,
                        const DayCounter& dayCounter,
                        Compounding compounding,
                        Frequency frequency,
                        Date settlementDate = Date()) {
			return impl::bps<Real>(bond,yield,dayCounter,compounding,frequency,settlementDate);
		}
        inline static Rate yield(const Bond& bond,
                          Real cleanPrice,
                          const DayCounter& dayCounter,
                          Compounding compounding,
                          Frequency frequency,
                          Date settlementDate = Date(),
                          Real accuracy = 1.0e-10,
                          Size maxIterations = 100,
                          Rate guess = 0.05) {
			return impl::yield<Real>(bond,cleanPrice,dayCounter,compounding,frequency,settlementDate,accuracy,maxIterations,guess);
		}
        inline static Time duration(const Bond& bond,
                             const InterestRate& yield,
                             Duration::Type type = Duration::Modified,
                             Date settlementDate = Date() ) {
			return impl::duration<Real>(bond,yield,type,settlementDate);
		}
        inline static Time duration(const Bond& bond,
                             Rate yield,
                             const DayCounter& dayCounter,
                             Compounding compounding,
                             Frequency frequency,
                             Duration::Type type = Duration::Modified,
                             Date settlementDate = Date() ) {
			return impl::duration<Real>(bond,yield,dayCounter,compounding,frequency,type,settlementDate);
		}
        inline static Real convexity(const Bond& bond,
                              const InterestRate& yield,
                              Date settlementDate = Date()) {
			return impl::convexity<Real>(bond,yield,settlementDate);
		}
        inline static Real convexity(const Bond& bond,
                              Rate yield,
                              const DayCounter& dayCounter,
                              Compounding compounding,
                              Frequency frequency,
                              Date settlementDate = Date()) {
			return impl::convexity<Real>(bond,yield,dayCounter,compounding,frequency,settlementDate);
		}
        inline static Real basisPointValue(const Bond& bond,
                                    const InterestRate& yield,
                                    Date settlementDate = Date()) {
			return impl::basisPointValue<Real>(bond,yield,settlementDate);
		}
        inline static Real basisPointValue(const Bond& bond,
                                    Rate yield,
                                    const DayCounter& dayCounter,
                                    Compounding compounding,
                                    Frequency frequency,
                                    Date settlementDate = Date()) {
			return impl::basisPointValue<Real>(bond,yield,dayCounter,compounding,frequency,settlementDate);
		}
        inline static Real yieldValueBasisPoint(const Bond& bond,
                                         const InterestRate& yield,
                                         Date settlementDate = Date()) {
			return impl::yieldValueBasisPoint<Real>(bond,yield,settlementDate);
		}
        inline static Real yieldValueBasisPoint(const Bond& bond,
                                         Rate yield,
                                         const DayCounter& dayCounter,
                                         Compounding compounding,
                                         Frequency frequency,
                                         Date settlementDate = Date()) {
			return impl::yieldValueBasisPoint<Real>(bond,yield,dayCounter,compounding,frequency,settlementDate);
		}
        //@}

        //! \name Z-spread functions
        //@{
        inline static Real cleanPrice(const Bond& bond,
                               const boost::shared_ptr<YieldTermStructure>& discount,
                               Spread zSpread,
                               const DayCounter& dayCounter,
                               Compounding compounding,
                               Frequency frequency,
                               Date settlementDate = Date()) {
			return impl::cleanPrice<Real>(bond,discount,zSpread,dayCounter,compounding,frequency,settlementDate);
		}
		inline static Real yieldTermStructurePV01(const Bond& bond,
			                               const boost::shared_ptr<YieldTermStructure>& discount,
										   const DayCounter& dayCounter,
										   Compounding compounding,
										   Frequency frequency,
										   Date settlementDate = Date()) {
			return impl::yieldTermStructurePV01<Real>(bond,discount,dayCounter,compounding,frequency,settlementDate);
		}
        inline static Spread zSpread(const Bond& bond,
                              Real cleanPrice,
                              const boost::shared_ptr<YieldTermStructure>& discount,
                              const DayCounter& dayCounter,
                              Compounding compounding,
                              Frequency frequency,
                              Date settlementDate = Date(),
                              Real accuracy = 1.0e-10,
                              Size maxIterations = 100,
                              Rate guess = 0.0) {
			return impl::zSpread<Real>(bond,cleanPrice,discount,dayCounter,compounding, frequency, settlementDate, accuracy, maxIterations,guess);
		}
        //@}


    };

}

#endif
