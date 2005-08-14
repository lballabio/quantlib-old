
/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
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

#ifdef HAVE_CONFIG_H
#include <qla/config.hpp>
#endif
#include <qla/capfloor.hpp>
#include <qla/shortratemodels.hpp>
#include <qla/termstructures.hpp>
#include <qla/typefactory.hpp>

#include <ql/date.hpp>
#include <ql/index.hpp>
#include <ql/schedule.hpp>
#include <ql/CashFlows/cashflowvectors.hpp>
#include <ql/Indexes/all.hpp>
#include <ql/Utilities/strings.hpp>
// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_NPV                       "NPV"
#define FIELD_ENGINE                    "ENGINE"
#define IDX_NPV                         0
#define IDX_ENGINE                      1

namespace QuantLibAddin {

    CapFloor::CapFloor(
            const long &start,
            const long &length,
            const std::string &timeUnitsID,
            const std::string &conventionID,
            const std::string &frequencyID,
            const long &fixingDays,
            const std::string &handleTermStructure,
            const double &nominal,
            const double &capStrike,
            const double &floorStrike,
            const std::string &handleEngine,
            const std::string &optionID,
            const long &boolAmortization) {

        QuantLib::BusinessDayConvention convention =
            Create<QuantLib::BusinessDayConvention>()(conventionID);
        QuantLib::Frequency frequency =
            Create<QuantLib::Frequency>()(frequencyID);
        QuantLib::TimeUnit timeUnits =
            Create<QuantLib::TimeUnit>()(timeUnitsID);

        boost::shared_ptr<AnalyticCapFloorEngine> engine =
            OH_GET_OBJECT(AnalyticCapFloorEngine, handleEngine);
        if (!engine)
            QL_FAIL("CapFloor: error retrieving object " + handleEngine);

        boost::shared_ptr<QuantLib::PricingEngine> engineQL =
            OH_GET_REFERENCE(QuantLib::PricingEngine, engine);

        boost::shared_ptr<YieldTermStructure> termStructure =
            OH_GET_OBJECT(YieldTermStructure, handleTermStructure);
        if (!termStructure)
            QL_FAIL("CapFloor: error retrieving object " + handleTermStructure);

        boost::shared_ptr<QuantLib::YieldTermStructure> termStructureP =
            OH_GET_REFERENCE(QuantLib::YieldTermStructure, termStructure);
        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructureP);

        boost::shared_ptr<QuantLib::Xibor> index(
            new QuantLib::Euribor(12/frequency, QuantLib::Months, termStructureH));
        QuantLib::Calendar calendar = index->calendar();

        QuantLib::Date startDate(start);
        QuantLib::Date endDate =
            calendar.advance(startDate, length, timeUnits, convention);
        QuantLib::Schedule schedule(calendar, startDate, endDate, frequency, convention);

        std::size_t numCashFlows = schedule.size();

        double amortization = (boolAmortization == 1) ? nominal/numCashFlows : 0.0;

        std::vector<double> nominals;
        for (std::size_t i=0 ; i < numCashFlows ; i++)
            nominals.push_back(nominal-i*amortization);

        std::vector<double> capStrikes(numCashFlows, capStrike);
        std::vector<double> floorStrikes(numCashFlows, floorStrike);

        std::vector<boost::shared_ptr<QuantLib::CashFlow> > leg =
            QuantLib::FloatingRateCouponVector(schedule,
                                               convention,
                                               nominals,
                                               index,
                                               fixingDays,
                                               std::vector<QuantLib::Spread>(),
                                               index->dayCounter());

        std::string idUpper = QuantLib::uppercase(optionID);
        if (idUpper.compare("CAP") == 0) {
            capfloor_ = boost::shared_ptr<QuantLib::CapFloor>(
                new QuantLib::Cap(leg, capStrikes, termStructureH, engineQL));
        } else if (idUpper.compare("FLOOR") == 0) {
            capfloor_ = boost::shared_ptr<QuantLib::CapFloor>(
                new QuantLib::Floor(leg, floorStrikes, termStructureH, engineQL));
        } else if (idUpper.compare("COLLAR") == 0) {
            capfloor_ = boost::shared_ptr<QuantLib::CapFloor>(
                new QuantLib::Collar(leg, capStrikes, floorStrikes, termStructureH, engineQL));
        } else {
            QL_FAIL("CapFloor: unknown option type " + optionID);
        }

        ObjHandler::any_ptr any_npv(new boost::any(capfloor_->NPV()));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        properties_.push_back(prop_npv);
    }

    AnalyticCapFloorEngine::AnalyticCapFloorEngine(
            const std::string &handleModel) {

        boost::shared_ptr<AffineModel> model = 
            OH_GET_OBJECT(AffineModel, handleModel);
        if (!model)
            QL_FAIL("AnalyticCapFloorEngine: error retrieving object " + handleModel);

        const boost::shared_ptr<QuantLib::AffineModel> modelQL =
            OH_GET_REFERENCE(QuantLib::AffineModel, model);

        engine_ = boost::shared_ptr<QuantLib::AnalyticCapFloorEngine>(
            new QuantLib::AnalyticCapFloorEngine(modelQL));
    }

}

