
/*
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
#include <qla/optionutils.hpp>
#include <qla/shortratemodels.hpp>
#include <qla/termstructures.hpp>

#include <ql/date.hpp>
#include <ql/index.hpp>
#include <ql/schedule.hpp>
#include <ql/CashFlows/cashflowvectors.hpp>
#include <ql/Indexes/all.hpp>

namespace QuantLibAddin {
    
    CapFloor::CapFloor(ObjHandler::ArgStack& args) {
        std::string optionID = ObjHandler::Args<std::string>::popArg(args);
        std::string handleEngine = ObjHandler::Args<std::string>::popArg(args);
        double strike = ObjHandler::Args<double>::popArg(args);
        double nominal = ObjHandler::Args<double>::popArg(args);
        std::string handleTermStructure = ObjHandler::Args<std::string>::popArg(args);
        long fixingDays = ObjHandler::Args<long>::popArg(args);
        std::string frequencyID = ObjHandler::Args<std::string>::popArg(args);
        std::string conventionID = ObjHandler::Args<std::string>::popArg(args);
        std::string timeUnitsID = ObjHandler::Args<std::string>::popArg(args);
        long length = ObjHandler::Args<long>::popArg(args);
        long start = ObjHandler::Args<long>::popArg(args);
        
        QuantLib::BusinessDayConvention convention = IDtoConvention(conventionID);
        QuantLib::Frequency frequency = IDtoFrequency(frequencyID);
        QuantLib::TimeUnit timeUnits = IDtoTimeUnit(timeUnitsID);
        
        boost::shared_ptr<AnalyticCapFloorEngine> engine =
            boost::dynamic_pointer_cast<AnalyticCapFloorEngine>
            (QL_OBJECT_GET(handleEngine));
        if (!engine)
            QL_FAIL("CapFloor: error retrieving object " + handleEngine);
        
        boost::shared_ptr<QuantLib::PricingEngine> engineQL =
            boost::static_pointer_cast<QuantLib::PricingEngine>
            (engine->getReference());
        
        boost::shared_ptr<YieldTermStructure> termStructure =
            boost::dynamic_pointer_cast<YieldTermStructure>
            (QL_OBJECT_GET(handleTermStructure));
        if (!termStructure)
            QL_FAIL("CapFloor: error retrieving object " + handleTermStructure);
        
        boost::shared_ptr<QuantLib::YieldTermStructure> termStructureP =
            boost::static_pointer_cast<QuantLib::YieldTermStructure>
            (termStructure->getReference());
        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructureP);
        
        boost::shared_ptr<QuantLib::Xibor> index(
            new QuantLib::Euribor(12/frequency, QuantLib::Months, termStructureH));
        QuantLib::Calendar calendar = index->calendar();
        
        QuantLib::Date startDate(start);
        QuantLib::Date endDate =
            calendar.advance(startDate, length, timeUnits, convention);
        QuantLib::Schedule schedule(calendar, startDate, endDate, frequency, convention);
        
        std::size_t numCashFlows = schedule.size();
        std::vector<double> nominals(numCashFlows, nominal);
        std::vector<double> strikes(numCashFlows, strike);
        
        std::vector<boost::shared_ptr<QuantLib::CashFlow> > leg =
            QuantLib::FloatingRateCouponVector(schedule,
                                               convention,
                                               nominals,
                                               index,
                                               fixingDays,
                                               std::vector<QuantLib::Spread>(),
                                               index->dayCounter());
        
        std::string idUpper = QuantLib::StringFormatter::toUppercase(optionID);
        if (idUpper.compare("CAP") == 0) {
            capfloor_ = boost::shared_ptr<QuantLib::CapFloor>(
                new QuantLib::Cap(leg, strikes, termStructureH, engineQL));
        } else if (idUpper.compare("FLOOR") == 0) {
            capfloor_ = boost::shared_ptr<QuantLib::CapFloor>(
                new QuantLib::Floor(leg, strikes, termStructureH, engineQL));
     // } else if (idUpper.compare("COLLAR") == 0) {
     //     capfloor_ = boost::shared_ptr<QuantLib::CapFloor>(
     //         new QuantLib::Collar(leg, strikes, termStructureH, engineQL));
        } else {
            QL_FAIL("CapFloor: unknown option type " + optionID);
        }
        
        ObjHandler::any_ptr any_npv(new boost::any(capfloor_->NPV()));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        properties_.push_back(prop_npv);
    }
    
    AnalyticCapFloorEngine::AnalyticCapFloorEngine(ObjHandler::ArgStack& args) {
        std::string handleModel = ObjHandler::Args<std::string>::popArg(args);
        
        boost::shared_ptr<AffineModel> model = 
            boost::dynamic_pointer_cast<AffineModel>
            (QL_OBJECT_GET(handleModel));
        if (!model)
            QL_FAIL("AnalyticCapFloorEngine: error retrieving object " + handleModel);
        
        const boost::shared_ptr<QuantLib::AffineModel> modelQL =
            boost::static_pointer_cast<QuantLib::AffineModel>(model->getReference());
        
        engine_ = boost::shared_ptr<QuantLib::AnalyticCapFloorEngine>(
            new QuantLib::AnalyticCapFloorEngine(modelQL));
    }

}