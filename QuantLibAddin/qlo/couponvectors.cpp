
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti

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
#include <qlo/Enumerations/Factories/iborcouponpricersfactory.hpp>

#include <ql/cashflow.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/cashflows/analysis.hpp>

using QuantLib::earlier_than;
using QuantLib::CashFlow;

namespace QuantLibAddin {

    QuantLib::Rate Leg::lastCouponRate(const QuantLib::Date& refDate) const {
        return QuantLib::CashFlows::lastCouponRate(leg_, refDate);
    }

    QuantLib::Rate Leg::currentCouponRate(const QuantLib::Date& refDate) const {
        return QuantLib::CashFlows::currentCouponRate(leg_, refDate);
    }

    QuantLib::Date Leg::startDate() const {
        return QuantLib::CashFlows::startDate(leg_);
    }

    QuantLib::Date Leg::maturityDate() const {
        return QuantLib::CashFlows::maturityDate(leg_);
    }

    QuantLib::Real Leg::npv(const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::CashFlows::npv(leg_, hYTS);
    }

    QuantLib::Real Leg::bps(const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::CashFlows::bps(leg_, hYTS);
    }

    QuantLib::Rate Leg::atmRate(const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS) const {
        return QuantLib::CashFlows::atmRate(leg_, hYTS);
    }

    std::vector<std::vector<boost::any> > Leg::analysis() const {
        return flowAnalysis(leg_);
    }

    void Leg::setPricer(
            const boost::shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer) {
        return QuantLib::CashFlows::setPricer(leg_,pricer);
    }

    void Leg::setPricers(
            const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers) {
        return QuantLib::CashFlows::setPricers(leg_,pricers);
    }

    const QuantLib::Leg& Leg::getQuantLibLeg() {
        return leg_;
    }

    MultiPhaseLeg::MultiPhaseLeg(
                    const std::vector<boost::shared_ptr<Leg> >& streams,
                    bool toBeSorted) {
        for (QuantLib::Size i=0; i<streams.size(); ++i) {
            const QuantLib::Leg& leg = streams[i]->getQuantLibLeg();
            leg_.insert(leg_.end(), leg.begin(), leg.end());
        }
        if (toBeSorted)
            std::sort(leg_.begin(), leg_.end(),
                      earlier_than<boost::shared_ptr<CashFlow> >());
    };

    SimpleCashFlowVector::SimpleCashFlowVector(const std::vector<QuantLib::Real>& amounts,
                                               const std::vector<QuantLib::Date>& dates)
    {
        for (QuantLib::Size i=0; i < amounts.size(); ++i) {
            leg_.push_back(boost::shared_ptr<CashFlow>(
                new QuantLib::SimpleCashFlow(amounts[i],dates[i])));
        }
    }

    FixedRateLeg::FixedRateLeg(
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    const std::vector<QuantLib::Rate>& couponRates,
                    const QuantLib::DayCounter& paymentDayCounter) {
        leg_ = QuantLib::FixedRateLeg(nominals,
                                      *schedule,
                                      couponRates,
                                      paymentDayCounter,
                                      paymentConvention);
    }

    IborLeg::IborLeg(
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    QuantLib::Natural fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Rate>& floors,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::IborIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& caps) {

            leg_ = QuantLib::IborLeg(nominals,
                                 *schedule,
                                 index,
                                 paymentDayCounter,
                                 paymentConvention,
                                 fixingDays,
                                 gearings, spreads,
                                 caps, floors,
                                 isInArrears);
    }

    DigitalIborLeg::DigitalIborLeg(
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    QuantLib::Natural fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::IborIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& callRates,
                    bool isCallOptionAdded,
                    const std::vector<QuantLib::Rate>& putRates,
                    bool isPutOptionAdded,
                    const std::vector<QuantLib::Rate>& digitalPayoffs,
                    QuantLib::Replication::Type replicationType,
                    QuantLib::Real eps) {

                        leg_ = QuantLib::DigitalIborLeg(nominals,
                                                        *schedule,
                                                        index,
                                                        paymentDayCounter,
                                                        paymentConvention,
                                                        fixingDays,
                                                        gearings,
                                                        spreads,
                                                        isInArrears,
                                                        callRates,
                                                        isCallOptionAdded,
                                                        putRates,
                                                        isPutOptionAdded,
                                                        digitalPayoffs,
                                                        replicationType,
                                                        eps);
    }

    
    IborCouponPricer::IborCouponPricer(
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& v,
            const std::string& typeOfIborCouponPricer) {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<QuantLib::IborCouponPricer> >()
            (typeOfIborCouponPricer, v);
    }

    CmsLeg::CmsLeg(
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    QuantLib::Natural fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Rate>& floors,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::SwapIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& caps) {
        leg_ = QuantLib::CmsLeg(nominals,
                                *schedule,
                                index,
                                paymentDayCounter,
                                paymentConvention,
                                fixingDays,
                                gearings, spreads,
                                caps, floors,
                                isInArrears);
    }

    DigitalCmsLeg::DigitalCmsLeg(
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    QuantLib::Natural fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::SwapIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& callRates,
                    bool isCallOptionAdded,
                    const std::vector<QuantLib::Rate>& putRates,
                    bool isPutOptionAdded,
                    const std::vector<QuantLib::Rate>& digitalPayoffs,
                    QuantLib::Replication::Type replicationType,
                    QuantLib::Real eps) {

                        leg_ = QuantLib::DigitalCmsLeg(nominals,
                                                        *schedule,
                                                        index,
                                                        paymentDayCounter,
                                                        paymentConvention,
                                                        fixingDays,
                                                        gearings,
                                                        spreads,
                                                        isInArrears,
                                                        callRates,
                                                        isCallOptionAdded,
                                                        putRates,
                                                        isPutOptionAdded,
                                                        digitalPayoffs,
                                                        replicationType,
                                                        eps);
 
    }

    RangeAccrualLeg::RangeAccrualLeg(
           QuantLib::BusinessDayConvention paymentConvention,
           const std::vector<QuantLib::Real>& nominals,
           const boost::shared_ptr<QuantLib::Schedule>& schedule,
           QuantLib::Natural fixingDays,
           const QuantLib::DayCounter& paymentDayCounter,
           const std::vector<QuantLib::Rate>& lowerTriggers,
           const std::vector<QuantLib::Real>& gearings,
           const boost::shared_ptr<QuantLib::IborIndex>& index,
           const std::vector<QuantLib::Spread>& spreads,
           const std::vector<QuantLib::Rate>& upperTriggers,
           const QuantLib::Period& observationTenor,
           QuantLib::BusinessDayConvention observationConvention) {
        leg_ = QuantLib::RangeAccrualLeg(nominals,
                                *schedule,
                                index,
                                paymentDayCounter,
                                paymentConvention,
                                fixingDays,
                                gearings,
                                spreads,
                                lowerTriggers,
                                upperTriggers, 
                                observationTenor,
                                observationConvention);
    }

    CmsZeroLeg::CmsZeroLeg(
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const boost::shared_ptr<QuantLib::Schedule>& schedule,
                    QuantLib::Natural fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Rate>& floors,
                    const std::vector<QuantLib::Real>& gearings,
                    const boost::shared_ptr<QuantLib::SwapIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& caps) {
        leg_ = QuantLib::CmsZeroLeg(nominals,
                                    *schedule,
                                    index,
                                    paymentDayCounter,
                                    paymentConvention,
                                    fixingDays,
                                    gearings, spreads,
                                    caps, floors);
    }
}
