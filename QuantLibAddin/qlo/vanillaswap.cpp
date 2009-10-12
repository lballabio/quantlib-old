/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Katiuscia Manzoni

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/vanillaswap.hpp>
#include <ql/instruments/makevanillaswap.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/yield/ratehelpers.hpp>
#include <ql/time/imm.hpp>

using QuantLib::MakeVanillaSwap;
using boost::shared_ptr;

namespace QuantLibAddin {

    VanillaSwap::VanillaSwap(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::VanillaSwap::Type type,
            QuantLib::Real nominal,
            const shared_ptr<QuantLib::Schedule>& fixedSchedule,
            QuantLib::Rate fixRate,
            const QuantLib::DayCounter& fixDayCounter,
            const shared_ptr<QuantLib::Schedule>& floatSchedule,
            const shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Spread spread,
            const QuantLib::DayCounter& floatDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::VanillaSwap(type,
                                  nominal,
                                  *fixedSchedule,
                                  fixRate,
                                  fixDayCounter,
                                  *floatSchedule,
                                  index,
                                  spread,
                                  floatDayCounter,
                                  paymentConvention));
    }

    VanillaSwap::VanillaSwap(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Period& swapTenor, 
            const shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Rate fixedRate,
            const QuantLib::Period& fwdStart,
            const QuantLib::DayCounter& fixDayCounter,
            QuantLib::Spread floatingLegSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = MakeVanillaSwap(swapTenor, index,
                                         fixedRate, fwdStart)
            .withFixedLegDayCount(fixDayCounter)
            .withFloatingLegSpread(floatingLegSpread)
            .operator shared_ptr<QuantLib::VanillaSwap>();
    }

    VanillaSwap::VanillaSwap(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Period& swapTenor, 
            const shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Rate fixedRate,
            const QuantLib::Date& immDate,
            const QuantLib::DayCounter& fixDayCounter,
            QuantLib::Spread floatingLegSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        QuantLib::Date effectiveDate = immDate;
        if (effectiveDate==QuantLib::Date())
            effectiveDate = QuantLib::IMM::nextDate();

        QuantLib::Date terminationDate = effectiveDate+swapTenor;
        terminationDate = QuantLib::Date::nthWeekday(3,
                                                     QuantLib::Wednesday,
                                                     terminationDate.month(),
                                                     terminationDate.year());

        libraryObject_ = MakeVanillaSwap(swapTenor, index,
                                                   fixedRate)
                         .withEffectiveDate(effectiveDate)
                         .withTerminationDate(terminationDate)
                         .withRule(QuantLib::DateGeneration::ThirdWednesday)
                         .withFixedLegDayCount(fixDayCounter)
                         .withFloatingLegSpread(floatingLegSpread)
                         .operator shared_ptr<QuantLib::VanillaSwap>();
    }

    VanillaSwap::VanillaSwap(
        const shared_ptr<ObjectHandler::ValueObject>& properties,
        const shared_ptr<QuantLib::SwapIndex>& swapIndex,
        const QuantLib::Date& fixingDate,
        bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = swapIndex->underlyingSwap(fixingDate);
    }

    VanillaSwap::VanillaSwap(
        const shared_ptr<ObjectHandler::ValueObject>& properties,
        const shared_ptr<QuantLib::SwapRateHelper>& swapRH,
        bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = swapRH->swap();
    }


    std::vector<std::vector<ObjectHandler::property_t> > VanillaSwap::fixedLegAnalysis() {
        return Swap::legAnalysis(0);
    }

    std::vector<std::vector<ObjectHandler::property_t> > VanillaSwap::floatingLegAnalysis() {
        return Swap::legAnalysis(1);
    }

}
