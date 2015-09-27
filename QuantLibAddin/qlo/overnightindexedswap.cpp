/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2009, 2011, 2015 Ferdinando Ametrano

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

#include <qlo/overnightindexedswap.hpp>
#include <ql/instruments/makeois.hpp>
#include <ql/termstructures/yield/oisratehelper.hpp>
#include <ql/time/ecb.hpp>

using std::vector;
using ObjectHandler::property_t;
using QuantLib::Period;
using QuantLib::MakeOIS;
using boost::shared_ptr;

namespace QuantLibAddin {

    OvernightIndexedSwap::OvernightIndexedSwap(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::OvernightIndexedSwap::Type type,
            std::vector<QuantLib::Real> nominals,
            const shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Rate fixedRate,
            const QuantLib::DayCounter& fixedDC,
            const shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
            QuantLib::Spread overnightSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::OvernightIndexedSwap(type, nominals,
                                           *schedule,
                                           fixedRate, fixedDC,
                                           overnightIndex, overnightSpread));
    }

    // MakeOIS
    OvernightIndexedSwap::OvernightIndexedSwap(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural settlDays,
            const QuantLib::Period& swapTenor,
            const shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
            QuantLib::Rate fixRate,
            const QuantLib::Period& fwdStart,
            const QuantLib::DayCounter& fixDayCounter,
            QuantLib::Spread overnightSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = MakeOIS(swapTenor, overnightIndex, fixRate, fwdStart)
                        .withSettlementDays(settlDays)
                        .withFixedLegDayCount(fixDayCounter)
                        .withOvernightLegSpread(overnightSpread)
                        .operator shared_ptr<QuantLib::OvernightIndexedSwap>();
    }

    // MakeDatedOIS
    OvernightIndexedSwap::OvernightIndexedSwap(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& startDate,
            const QuantLib::Date& enddate,
            const shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
            QuantLib::Rate fixedRate,
            const QuantLib::DayCounter& fixedDC,
            QuantLib::Spread overnightSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        QuantLib::Date effectiveDate = startDate;
        if (effectiveDate==QuantLib::Date())
            effectiveDate = QuantLib::ECB::nextDate();

        QuantLib::Date terminationDate = enddate;
        if (terminationDate==QuantLib::Date())
            terminationDate = QuantLib::ECB::nextDate(effectiveDate);

        libraryObject_= MakeOIS(Period(), overnightIndex,
                                fixedRate)
            .withEffectiveDate(effectiveDate)
            .withTerminationDate(terminationDate)
            .withFixedLegDayCount(fixedDC)
            .withOvernightLegSpread(overnightSpread)
            .operator shared_ptr<QuantLib::OvernightIndexedSwap>();
    }

    //VanillaSwap::VanillaSwap(
    //    const shared_ptr<ObjectHandler::ValueObject>& properties,
    //    const shared_ptr<QuantLib::OISIndex>& oisIndex,
    //    const QuantLib::Date& fixingDate,
    //    bool permanent)
    //: Swap(properties, permanent)
    //{
    //    libraryObject_ = oisIndex->underlyingSwap(fixingDate);
    //}

    OvernightIndexedSwap::OvernightIndexedSwap(
        const shared_ptr<ObjectHandler::ValueObject>& properties,
        const shared_ptr<QuantLib::OISRateHelper>& swapRH,
        bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = swapRH->swap();
    }

    vector<vector<property_t> > OvernightIndexedSwap::fixedLegAnalysis(
                                                    const QuantLib::Date& d) {
        return Swap::legAnalysis(0, d);
    }

    vector<vector<property_t> > OvernightIndexedSwap::overnightLegAnalysis(
                                                    const QuantLib::Date& d) {
        return Swap::legAnalysis(1, d);
    }

}
