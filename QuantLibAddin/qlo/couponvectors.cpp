
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2005 Aurelien Chanudet

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/couponvectors.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/typefactory.hpp>

#include <ql/CashFlows/cashflowvectors.hpp>
#include <ql/CashFlows/simplecashflow.hpp>
#include <ql/CashFlows/cmscoupon.hpp>
#include <ql/CashFlows/analysis.hpp>

namespace QuantLibAddin {

    QuantLib::Date CashFlowStream::startDate() const {
        return QuantLib::Cashflows::startDate(cashFlowVector_);
    }

    QuantLib::Date CashFlowStream::maturityDate() const {
        return QuantLib::Cashflows::maturityDate(cashFlowVector_);
    }

    QuantLib::Real CashFlowStream::npv(const QuantLib::Handle< QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::Cashflows::bps(cashFlowVector_, hYTS);
    }

    QuantLib::Real CashFlowStream::bps(const QuantLib::Handle< QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::Cashflows::bps(cashFlowVector_, hYTS);
    }

    QuantLib::Rate CashFlowStream::atmRate(const QuantLib::Handle< QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::Cashflows::atmRate(cashFlowVector_, hYTS);
    }

    std::vector<std::vector<boost::any> > CashFlowStream::analysis() const {
        return flowAnalysis(cashFlowVector_);
    }

    const Leg& CashFlowStream::getVector() {
        return cashFlowVector_;
    }

    SimpleCashFlow::SimpleCashFlow(double amount, const QuantLib::Date& date) {
        cashFlowVector_.push_back(boost::shared_ptr<QuantLib::CashFlow>(new
            QuantLib::SimpleCashFlow(amount,date)));
    }

    SimpleCashFlowVector::SimpleCashFlowVector(const std::vector<double> amounts,
                                               const std::vector<QuantLib::Date>& dates)
    {
        for (QuantLib::Size i=0; i < amounts.size(); ++i) {
            cashFlowVector_.push_back(boost::shared_ptr<QuantLib::CashFlow>(
                new QuantLib::SimpleCashFlow(amounts[i],dates[i])));
        }
    }

    FixedRateCouponVector::FixedRateCouponVector(
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    const QuantLib::BusinessDayConvention& convention,
                    const std::vector<double>& nominals,
                    const std::vector<double>& couponRates,
                    const QuantLib::DayCounter& dayCounter) {
        cashFlowVector_ = QuantLib::FixedRateCouponVector(*schedule,
                                                          nominals,
                                                          couponRates,
                                                          dayCounter,
                                                          convention,
                                                          dayCounter);
    }

    FloatingRateCouponVector::FloatingRateCouponVector(
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    const std::vector<double>& nominals,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::IborIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    QuantLib::Integer fixingDays,
                    const QuantLib::DayCounter& dayCounter,
                    const QuantLib::BusinessDayConvention& paymentAdjustment) {
                                 
        cashFlowVector_ = 
            QuantLib::FloatingRateCouponVector(*schedule,
                                               nominals,
                                               index,
                                               dayCounter/*index->dayCounter()*/,
                                               fixingDays /*index->fixingDays()*/,
                                               paymentAdjustment/*QuantLib::Following*/,
                                               gearings,
                                               spreads);
    }

    CappedFlooredFloatingRateCouponVector::CappedFlooredFloatingRateCouponVector(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        const std::vector<QuantLib::Real>& nominals,
        const std::vector<QuantLib::Real>& gearings,
        const boost::shared_ptr<QuantLib::IborIndex>& index,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Real>& caps,
        const std::vector<QuantLib::Real>& floors,  
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& dayCounter,
        const QuantLib::BusinessDayConvention& paymentAdjustment,
        const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& volatility) {
            cashFlowVector_ = QuantLib::CappedFlooredFloatingRateCouponVector(*schedule,
                                               nominals,
                                               index,
                                               dayCounter/*index->dayCounter()*/,
                                               fixingDays /*index->fixingDays()*/,
                                               paymentAdjustment/*QuantLib::Following*/,
                                               gearings, spreads,
                                               caps, floors,
                                               volatility);
    }

    VanillaCMSCouponPricer::VanillaCMSCouponPricer(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& v,
            const std::string& typeOfVanillaCMSCouponPricer,
            QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            QuantLib::Real meanReversion) {
        libraryObject_ = Create<boost::shared_ptr<QuantLib::VanillaCMSCouponPricer> >()
            (typeOfVanillaCMSCouponPricer, v, modelOfYieldCurve, meanReversion);
    }

    CMSCouponVector::CMSCouponVector(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const std::vector<QuantLib::Real>& nominals,
        const boost::shared_ptr<QuantLib::SwapIndex>& index,
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& paymentDayCounter,
        const std::vector<QuantLib::Real>& gearings,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Rate>& caps,
        const std::vector<QuantLib::Rate>& floors,
        const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer)
    {
        cashFlowVector_ = QuantLib::CMSCouponVector(*schedule,
                                                    nominals,
                                                    index,
                                                    pricer,
                                                    paymentDayCounter,
                                                    fixingDays,
                                                    paymentAdjustment,
                                                    gearings,
                                                    spreads,
                                                    caps,
                                                    floors);
    }

    CMSZeroCouponVector::CMSZeroCouponVector(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const std::vector<QuantLib::Real>& nominals,
        const boost::shared_ptr<QuantLib::SwapIndex>& index,
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& paymentDayCounter,
        const std::vector<QuantLib::Real>& gearings,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Rate>& caps,
        const std::vector<QuantLib::Rate>& floors,
        const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer)
    {
        cashFlowVector_ = QuantLib::CMSZeroCouponVector(*schedule,
                                                    nominals,
                                                    index,
                                                    pricer,
                                                    paymentDayCounter,
                                                    fixingDays,
                                                    paymentAdjustment,
                                                    gearings,
                                                    spreads,
                                                    caps,
                                                    floors);
    }


    CMSInArrearsCouponVector::CMSInArrearsCouponVector(
        const boost::shared_ptr<QuantLib::Schedule>& schedule,
        QuantLib::BusinessDayConvention paymentAdjustment,
        const std::vector<QuantLib::Real>& nominals,
        const boost::shared_ptr<QuantLib::SwapIndex>& index,
        QuantLib::Integer fixingDays,
        const QuantLib::DayCounter& paymentDayCounter,
        const std::vector<QuantLib::Real>& gearings,
        const std::vector<QuantLib::Real>& spreads,
        const std::vector<QuantLib::Rate>& caps,
        const std::vector<QuantLib::Rate>& floors,
        const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer)
    {
        cashFlowVector_ = QuantLib::CMSInArrearsCouponVector(*schedule,
                                                    nominals,
                                                    index,
                                                    pricer,
                                                    paymentDayCounter,
                                                    fixingDays,
                                                    paymentAdjustment,
                                                    gearings,
                                                    spreads,
                                                    caps,
                                                    floors);
    }

}
