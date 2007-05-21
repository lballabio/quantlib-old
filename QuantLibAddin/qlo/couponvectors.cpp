
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
#include <ql/cashflows/cashflows.hpp>

using QuantLib::earlier_than;
using QuantLib::CashFlow;

namespace QuantLibAddin {

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
