
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
#include <qla/shortratemodels.hpp>
#include <qla/termstructures.hpp>
#include <qla/typefactory.hpp>
#include <qla/xibor.hpp>

#include <ql/date.hpp>
#include <ql/index.hpp>
#include <ql/schedule.hpp>
#include <ql/CashFlows/cashflowvectors.hpp>
#include <ql/CashFlows/floatingratecoupon.hpp>

namespace QuantLibAddin {

    CapFloor::CapFloor(
            const long& start,
            const long& maturity,
            const std::string& handleTermStructure,
            const std::string& handleIndex,
            const std::vector<double>& nominals,
            const std::vector<double>& capStrikes,
            const std::vector<double>& floorStrikes,
            const std::string& handleEngine,
            const std::string& optionID) {

        boost::shared_ptr<AnalyticCapFloorEngine> engine =
            OH_GET_OBJECT(AnalyticCapFloorEngine, handleEngine);
        if (!engine)
            QL_FAIL("CapFloor: error retrieving pricing engine " + handleEngine);

        boost::shared_ptr<QuantLib::PricingEngine> engineQL =
            OH_GET_REFERENCE(QuantLib::PricingEngine, engine);

        boost::shared_ptr<YieldTermStructure> termStructure =
            OH_GET_OBJECT(YieldTermStructure, handleTermStructure);
        if (!termStructure)
            QL_FAIL("CapFloor: error retrieving term structure " + handleTermStructure);

        boost::shared_ptr<QuantLib::YieldTermStructure> termStructureP =
            OH_GET_REFERENCE(QuantLib::YieldTermStructure, termStructure);

        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructureP);

        boost::shared_ptr<QuantLibAddin::Xibor> indexWrapper =
            OH_GET_OBJECT(QuantLibAddin::Xibor, handleIndex);
        if (!indexWrapper)
            QL_FAIL("CapFloor: error retrieving index  " + handleIndex);
        boost::shared_ptr<QuantLib::Xibor> index =
            OH_GET_REFERENCE(QuantLib::Xibor, indexWrapper);

        QuantLib::Calendar              calendar    = index->calendar();
        QuantLib::Frequency             frequency   = index->frequency();
        QuantLib::BusinessDayConvention convention  = index->businessDayConvention();

        QuantLib::Date startDate(start);
        QuantLib::Date maturityDate(maturity);

        QuantLib::Schedule schedule(calendar,
                                    startDate,
                                    maturityDate,
                                    frequency,
                                    convention);

        std::vector<boost::shared_ptr<QuantLib::CashFlow> > leg =
            QuantLib::FloatingRateCouponVector(schedule,
                                               convention,
                                               nominals,
                                               index,
                                               index->settlementDays(),
                                               std::vector<QuantLib::Spread>(),
                                               index->dayCounter());

        QuantLib::CapFloor::Type option =
            Create<QuantLib::CapFloor::Type>()(optionID);

        mInstrument = boost::shared_ptr<QuantLib::CapFloor>(
            new QuantLib::CapFloor(option,
                                   leg,
                                   capStrikes,
                                   floorStrikes,
                                   termStructureH,
                                   engineQL));
    }

    const std::vector<std::vector<double> >& CapFloor::cashFlows() {
        const std::vector<boost::shared_ptr<QuantLib::CashFlow> >& leg = getObject().leg();
        cashFlows_.clear();
        for (std::size_t i=0 ; i < leg.size() ; i++) {
            std::vector<double> cf;
            QuantLib::FloatingRateCoupon& c = (QuantLib::FloatingRateCoupon&) *(leg[i]);
            cf.push_back(c.accrualStartDate().serialNumber());
            cf.push_back(c.accrualEndDate().serialNumber());
            cf.push_back(c.date().serialNumber());
            cf.push_back(c.fixingDate().serialNumber());
            cf.push_back(c.accrualPeriod());
            cf.push_back(c.accrualDays());
            cf.push_back(c.amount());
            cf.push_back(c.indexFixing());
            cashFlows_.push_back(cf);
        }
        return cashFlows_;
    }

    AnalyticCapFloorEngine::AnalyticCapFloorEngine(
            const std::string& handleModel) {

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

