/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/couponvectors.hpp>

#include <qlo/enumerations/factories/iborcouponpricersfactory.hpp>
#include <ql/cashflows/digitalcmscoupon.hpp>
#include <ql/cashflows/digitaliborcoupon.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/cashflows/rangeaccrual.hpp>
#include <ql/cashflows/cashflows.hpp>

using ObjectHandler::LibraryObject;
using ObjectHandler::Create;
using ObjectHandler::ValueObject;
using QuantLib::earlier_than;
using QuantLib::CashFlow;
using boost::shared_ptr;

namespace QuantLibAddin {

    DigitalReplication::DigitalReplication(const shared_ptr<ValueObject>& p,
                                           QuantLib::Replication::Type r,
                                           QuantLib::Real eps,
                                           bool perm)
    : LibraryObject<QuantLib::DigitalReplication>(p, perm)
    {
            libraryObject_ = shared_ptr<QuantLib::DigitalReplication> (new
                QuantLib::DigitalReplication(r, eps));
    }

    FixedRateLeg::FixedRateLeg(
                    const shared_ptr<ValueObject>& p,
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const shared_ptr<QuantLib::Schedule>& schedule,
                    const std::vector<QuantLib::Rate>& couponRates,
                    const QuantLib::DayCounter& paymentDayCounter,
                    bool perm)
    : Leg(p, perm)
    {
        leg_ = QuantLib::FixedRateLeg(*schedule, paymentDayCounter)
            .withNotionals(nominals)
            .withCouponRates(couponRates)
            .withPaymentAdjustment(paymentConvention);
    }

    IborLeg::IborLeg(const shared_ptr<ValueObject>& p,
                     QuantLib::BusinessDayConvention paymentConvention,
                     const std::vector<QuantLib::Real>& nominals,
                     const shared_ptr<QuantLib::Schedule>& schedule,
                     const std::vector<QuantLib::Natural>& fixingDays,
                     bool isInArrears,
                     const QuantLib::DayCounter& paymentDayCounter,
                     const std::vector<QuantLib::Rate>& floors,
                     const std::vector<QuantLib::Real>& gearings,
                     const shared_ptr<QuantLib::IborIndex>& index,
                     const std::vector<QuantLib::Spread>& spreads,
                     const std::vector<QuantLib::Rate>& caps,
                     bool permanent)
    : Leg(p, permanent)
    {
            leg_ = QuantLib::IborLeg(*schedule, index)
                .withNotionals(nominals)
                .withPaymentDayCounter(paymentDayCounter)
                .withPaymentAdjustment(paymentConvention)
                .withFixingDays(fixingDays)
                .withGearings(gearings)
                .withSpreads(spreads)
                .withCaps(caps)
                .withFloors(floors)
                .inArrears(isInArrears);
    }

    DigitalIborLeg::DigitalIborLeg(
            const shared_ptr<ValueObject>& p,
            QuantLib::BusinessDayConvention paymentConvention,
            const std::vector<QuantLib::Real>& nominals,
            const shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Natural>& fixingDays,
            bool isInArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Real>& gearings,
            const shared_ptr<QuantLib::IborIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& callStrikes,
            std::string callPositionAndATMInclusion,
            const std::vector<QuantLib::Rate>& callDigitalPayoffs,
            const std::vector<QuantLib::Rate>& putStrikes,
            std::string putPositionAndATMInclusion,
            const std::vector<QuantLib::Rate>& putDigitalPayoffs,
            const shared_ptr<QuantLib::DigitalReplication>& replication,
            bool permanent)
    : Leg(p, permanent)
    {
        QuantLib::Position::Type callPosition;
        QuantLib::Position::Type putPosition;
        bool isCallATMIncluded;
        bool isPutATMIncluded;

        std::string legalLabels[] = { "Short - ATM included",
                                      "Short - ATM excluded",
                                      "Long - ATM included",
                                      "Long - ATM excluded" };

        int foundLabel = -1;
        int counter = 0;
        while (foundLabel==-1 && counter<4) {
            if (callPositionAndATMInclusion == legalLabels[counter])
                foundLabel = counter;
            counter++;
        }
        switch(foundLabel) {
            case 0:
                callPosition = QuantLib::Position::Short;
                isCallATMIncluded = true;
                break;
            case 1:
                callPosition = QuantLib::Position::Short;
                isCallATMIncluded = false;
                break;
            case 2:
                callPosition = QuantLib::Position::Long;
                isCallATMIncluded = true;
                break;
            case 3:
                callPosition = QuantLib::Position::Long;
                isCallATMIncluded = false;
                break;
            default:
                OH_FAIL("DigitalIborLeg::DigitalIborLeg: invalid string for call option: "
                        << callPositionAndATMInclusion);
                break;
        }
        foundLabel = -1;
        counter = 0;
        while (foundLabel==-1 && counter<4) {
            if (putPositionAndATMInclusion == legalLabels[counter])
                foundLabel = counter;
            counter++;
        }
        switch(foundLabel) {
            case 0:
                putPosition = QuantLib::Position::Short;
                isPutATMIncluded = true;
                break;
            case 1:
                putPosition = QuantLib::Position::Short;
                isPutATMIncluded = false;
                break;
            case 2:
                putPosition = QuantLib::Position::Long;
                isPutATMIncluded = true;
                break;
            case 3:
                putPosition = QuantLib::Position::Long;
                isPutATMIncluded = false;
                break;
            default:
                OH_FAIL("DigitalIborLeg::DigitalIborLeg: invalid string for put option: "
                        << callPositionAndATMInclusion);
                break;
        }

        leg_ = QuantLib::DigitalIborLeg(*schedule, index)
            .withNotionals(nominals)
            .withPaymentDayCounter(paymentDayCounter)
            .withPaymentAdjustment(paymentConvention)
            .withFixingDays(fixingDays)
            .withGearings(gearings)
            .withSpreads(spreads)
            .inArrears(isInArrears)
            .withCallStrikes(callStrikes)
            .withLongCallOption(callPosition)
            .withCallATM(isCallATMIncluded)
            .withCallPayoffs(callDigitalPayoffs)
            .withPutStrikes(putStrikes)
            .withLongPutOption(putPosition)
            .withPutATM(isPutATMIncluded)
            .withPutPayoffs(putDigitalPayoffs)
            .withReplication(replication);
    }

    IborCouponPricer::IborCouponPricer(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::OptionletVolatilityStructure>& v,
            const std::string& t,
            bool permanent)
    : FloatingRateCouponPricer(properties, permanent)
    {
        libraryObject_ =
            Create<shared_ptr<QuantLib::IborCouponPricer> >()(t, v);
    }

    CmsLeg::CmsLeg(const shared_ptr<ValueObject>& properties,
                   QuantLib::BusinessDayConvention paymentConvention,
                   const std::vector<QuantLib::Real>& nominals,
                   const shared_ptr<QuantLib::Schedule>& schedule,
                   const std::vector<QuantLib::Natural>& fixingDays,
                   bool isInArrears,
                   const QuantLib::DayCounter& paymentDayCounter,
                   const std::vector<QuantLib::Rate>& floors,
                   const std::vector<QuantLib::Real>& gearings,
                   const shared_ptr<QuantLib::SwapIndex>& index,
                   const std::vector<QuantLib::Spread>& spreads,
                   const std::vector<QuantLib::Rate>& caps,
                   bool permanent)
    : Leg(properties, permanent)
    {
        leg_ = QuantLib::CmsLeg(*schedule, index)
            .withNotionals(nominals)
            .withPaymentDayCounter(paymentDayCounter)
            .withPaymentAdjustment(paymentConvention)
            .withFixingDays(fixingDays)
            .withGearings(gearings)
            .withSpreads(spreads)
            .withCaps(caps)
            .withFloors(floors)
            .inArrears(isInArrears);
    }

    DigitalCmsLeg::DigitalCmsLeg(
                    const shared_ptr<ValueObject>& properties,
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const shared_ptr<QuantLib::Schedule>& schedule,
                    const std::vector<QuantLib::Natural>& fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Real>& gearings,
                    const shared_ptr<QuantLib::SwapIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& callStrikes,
                    std::string callPositionAndATMInclusion,
                    const std::vector<QuantLib::Rate>& callDigitalPayoffs,
                    const std::vector<QuantLib::Rate>& putStrikes,
                    std::string putPositionAndATMInclusion,
                    const std::vector<QuantLib::Rate>& putDigitalPayoffs,
                    const shared_ptr<QuantLib::DigitalReplication>& replication,
                    bool permanent)
    : Leg(properties, permanent)
    {
        QuantLib::Position::Type callPosition;
        QuantLib::Position::Type putPosition;
        bool isCallATMIncluded;
        bool isPutATMIncluded;

        std::string legalLabels[] = { "Short - ATM included",
                                      "Short - ATM excluded",
                                      "Long - ATM included",
                                      "Long - ATM excluded" };

        int foundLabel = -1;
        int counter = 0;
        while (foundLabel==-1) {
            if (callPositionAndATMInclusion == legalLabels[counter])
                foundLabel = counter;
            counter++;
        }
        switch(foundLabel) {
            case 0:
                callPosition = QuantLib::Position::Short;
                isCallATMIncluded = true;
                break;
            case 1:
                callPosition = QuantLib::Position::Short;
                isCallATMIncluded = false;
                break;
            case 2:
                callPosition = QuantLib::Position::Long;
                isCallATMIncluded = true;
                break;
            case 3:
                callPosition = QuantLib::Position::Long;
                isCallATMIncluded = false;
                break;
            default:
                OH_FAIL("DigitalIborLeg::DigitalIborLeg: invalid string for call option: "
                        << callPositionAndATMInclusion);
                break;
        }
        foundLabel = -1;
        counter = 0;
        while (foundLabel==-1) {
            if (putPositionAndATMInclusion == legalLabels[counter])
                foundLabel = counter;
            counter++;
        }
        switch(foundLabel) {
            case 0:
                putPosition = QuantLib::Position::Short;
                isPutATMIncluded = true;
                break;
            case 1:
                putPosition = QuantLib::Position::Short;
                isPutATMIncluded = false;
                break;
            case 2:
                putPosition = QuantLib::Position::Long;
                isPutATMIncluded = true;
                break;
            case 3:
                putPosition = QuantLib::Position::Long;
                isPutATMIncluded = false;
                break;
            default:
                OH_FAIL("DigitalIborLeg::DigitalIborLeg: invalid string for put option: "
                        << callPositionAndATMInclusion);
                break;
        }

        leg_ = QuantLib::DigitalCmsLeg(*schedule, index)
            .withNotionals(nominals)
            .withPaymentDayCounter(paymentDayCounter)
            .withPaymentAdjustment(paymentConvention)
            .withFixingDays(fixingDays)
            .withGearings(gearings)
            .withSpreads(spreads)
            .inArrears(isInArrears)
            .withCallStrikes(callStrikes)
            .withLongCallOption(callPosition)
            .withCallATM(isCallATMIncluded)
            .withCallPayoffs(callDigitalPayoffs)
            .withPutStrikes(putStrikes)
            .withLongPutOption(putPosition)
            .withPutATM(isPutATMIncluded)
            .withPutPayoffs(putDigitalPayoffs)
            .withReplication(replication);
    }

    RangeAccrualLeg::RangeAccrualLeg(
           const shared_ptr<ValueObject>& properties,
           QuantLib::BusinessDayConvention paymentConvention,
           const std::vector<QuantLib::Real>& nominals,
           const shared_ptr<QuantLib::Schedule>& schedule,
           const std::vector<QuantLib::Natural>& fixingDays,
           const QuantLib::DayCounter& paymentDayCounter,
           const std::vector<QuantLib::Rate>& lowerTriggers,
           const std::vector<QuantLib::Real>& gearings,
           const shared_ptr<QuantLib::IborIndex>& index,
           const std::vector<QuantLib::Spread>& spreads,
           const std::vector<QuantLib::Rate>& upperTriggers,
           const QuantLib::Period& observationTenor,
           QuantLib::BusinessDayConvention observationConvention,
           bool permanent)
    : Leg(properties, permanent)
    {
        leg_ = QuantLib::RangeAccrualLeg(*schedule, index)
            .withNotionals(nominals)
            .withPaymentDayCounter(paymentDayCounter)
            .withPaymentAdjustment(paymentConvention)
            .withFixingDays(fixingDays)
            .withGearings(gearings)
            .withSpreads(spreads)
            .withLowerTriggers(lowerTriggers)
            .withUpperTriggers(upperTriggers)
            .withObservationTenor(observationTenor)
            .withObservationConvention(observationConvention);
    }

    CmsZeroLeg::CmsZeroLeg(
                    const shared_ptr<ValueObject>& properties,
                    QuantLib::BusinessDayConvention paymentConvention,
                    const std::vector<QuantLib::Real>& nominals,
                    const shared_ptr<QuantLib::Schedule>& schedule,
                    const std::vector<QuantLib::Natural>& fixingDays,
                    bool isInArrears,
                    const QuantLib::DayCounter& paymentDayCounter,
                    const std::vector<QuantLib::Rate>& floors,
                    const std::vector<QuantLib::Real>& gearings,
                    const shared_ptr<QuantLib::SwapIndex>& index,
                    const std::vector<QuantLib::Spread>& spreads,
                    const std::vector<QuantLib::Rate>& caps,
                    bool permanent)
    : Leg(properties, permanent)
    {
        leg_ = QuantLib::CmsLeg(*schedule, index)
            .withNotionals(nominals)
            .withPaymentDayCounter(paymentDayCounter)
            .withPaymentAdjustment(paymentConvention)
            .withFixingDays(fixingDays)
            .withGearings(gearings)
            .withSpreads(spreads)
            .withCaps(caps)
            .withFloors(floors)
            .withZeroPayments();
    }

}
