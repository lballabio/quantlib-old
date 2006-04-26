
/*
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
#include <qla/couponvectors.hpp>
#include <qla/shortratemodels.hpp>
#include <qla/termstructures.hpp>
#include <qla/typefactory.hpp>

namespace QuantLibAddin {

    CapFloor::CapFloor(
            const boost::shared_ptr < InstanceName > &instanceName,
            const std::string&         couponVectorID,
            const std::string&         termStructureID,
            const std::vector<double>& capStrikes,
            const std::vector<double>& floorStrikes,
            const std::string&         engineID,
            const std::string&         optionID)  : Instrument(instanceName) {

        OH_GET_REFERENCE(engine, engineID, 
            AnalyticCapFloorEngine, QuantLib::PricingEngine)

        OH_GET_REFERENCE(termStructureP, termStructureID, 
            YieldTermStructure, QuantLib::YieldTermStructure)
        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructureP);

        OH_GET_OBJECT(couponVectorWrapper, couponVectorID, CouponVector)
        const CashFlowVector& couponVector = couponVectorWrapper->getObject();

        QuantLib::CapFloor::Type option =
            Create<QuantLib::CapFloor::Type>()(optionID);

        mInstrument = boost::shared_ptr<QuantLib::CapFloor>(
            new QuantLib::CapFloor(option,
                                   couponVector,
                                   capStrikes,
                                   floorStrikes,
                                   termStructureH,
                                   engine));
    }

    AnalyticCapFloorEngine::AnalyticCapFloorEngine(
            const boost::shared_ptr < InstanceName > &instanceName,
            const std::string& handleModel)  : ObjHandler::Object(instanceName) {

        OH_GET_REFERENCE(model, handleModel, 
            AffineModel, QuantLib::AffineModel)

        engine_ = boost::shared_ptr<QuantLib::AnalyticCapFloorEngine>(
            new QuantLib::AnalyticCapFloorEngine(model));
    }

}

