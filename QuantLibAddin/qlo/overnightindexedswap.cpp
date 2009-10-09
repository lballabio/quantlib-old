/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2009 Ferdinando Ametrano

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
#include <ql/experimental/overnightswap/makeois.hpp>
//#include <ql/indexes/swapindex.hpp>
#include <ql/experimental/overnightswap/oisratehelper.hpp>

using std::vector;
using ObjectHandler::property_t;
using QuantLib::Period;
using QuantLib::MakeOIS;

namespace QuantLibAddin {

    OvernightIndexedSwap::OvernightIndexedSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::OvernightIndexedSwap::Type type,
            QuantLib::Real nominal,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Rate fixedRate,
            const QuantLib::DayCounter& fixedDC,
            const boost::shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
            QuantLib::Spread overnightSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::OvernightIndexedSwap(type, nominal,
                                           *schedule,
                                           fixedRate, fixedDC,
                                           overnightIndex, overnightSpread));
    }

    OvernightIndexedSwap::OvernightIndexedSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::OvernightIndexedSwap::Type type,
            QuantLib::Real nominal,
            const QuantLib::Date& startDate,
            const QuantLib::Period& tenor,
            QuantLib::Frequency paymentFrequency,
            QuantLib::Rate fixedRate,
            const QuantLib::DayCounter& fixedDC,
            const boost::shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
            QuantLib::Spread overnightSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = MakeOIS(tenor, overnightIndex, fixedRate, startDate)
            .withType(type)
            .withNominal(nominal)
            .withPaymentFrequency(paymentFrequency)
            .withFixedLegDayCount(fixedDC)
            .withOvernightLegSpread(overnightSpread)
            .operator boost::shared_ptr<QuantLib::OvernightIndexedSwap>();
    }

    OvernightIndexedSwap::OvernightIndexedSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::OvernightIndexedSwap::Type type,
            QuantLib::Real nominal,
            const QuantLib::Date& startDate,
            const QuantLib::Date& enddate,
            QuantLib::Frequency paymentFrequency,
            QuantLib::Rate fixedRate,
            const QuantLib::DayCounter& fixedDC,
            const boost::shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
            QuantLib::Spread overnightSpread,
            bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_= MakeOIS(Period(), overnightIndex, fixedRate, startDate)
            .withType(type)
            .withNominal(nominal)
            .withTerminationDate(enddate)
            .withPaymentFrequency(paymentFrequency)
            .withFixedLegDayCount(fixedDC)
            .withOvernightLegSpread(overnightSpread)
            .operator boost::shared_ptr<QuantLib::OvernightIndexedSwap>();
    }

    OvernightIndexedSwap::OvernightIndexedSwap(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::OISRateHelper>& swapRH,
        bool permanent)
    : Swap(properties, permanent)
    {
        libraryObject_ = swapRH->swap();
    }

    vector<vector<property_t> > OvernightIndexedSwap::fixedLegAnalysis() {
        return Swap::legAnalysis(0);
    }

    vector<vector<property_t> > OvernightIndexedSwap::overnightLegAnalysis() {
        return Swap::legAnalysis(1);
    }

}
