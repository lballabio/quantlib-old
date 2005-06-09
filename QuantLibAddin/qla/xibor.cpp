
/*
 Copyright (C) 2005 Plamen Neykov

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

#if defined(HAVE_CONFIG_H)
    #include <qla/config.hpp>
#endif

#include "ql/Indexes/indexmanager.hpp"
#include <qla/xibor.hpp>
#include <qla/generalutils.hpp>
#include <qla/termstructures.hpp>
#include <qla/enumfactory.hpp>

namespace QuantLibAddin {
	QuantLib::Date make_date(long d) { return QuantLib::Date(d); }

	Xibor::Xibor(ObjHandler::ArgumentStack& arguments) {
        std::vector<double> fixings = OH_POP_ARGUMENT(std::vector<double>, arguments);
        std::vector<long> lDates = OH_POP_ARGUMENT(std::vector<long>, arguments);
		std::string fwdCurveId = OH_POP_ARGUMENT(std::string, arguments);
		long fixingDays = OH_POP_ARGUMENT(long, arguments);
		std::string fltDayCounterID = OH_POP_ARGUMENT(std::string, arguments);
		std::string fltBDCID = OH_POP_ARGUMENT(std::string, arguments);
		std::string calendarID = OH_POP_ARGUMENT(std::string, arguments);
        std::string timeUnitsID = OH_POP_ARGUMENT(std::string, arguments);
        long tenor = OH_POP_ARGUMENT(long, arguments);
		std::string crrID = OH_POP_ARGUMENT(std::string, arguments);
		std::string indexName = OH_POP_ARGUMENT(std::string, arguments);

		QuantLib::DayCounter fltDayCounter = CreateEnum<QuantLib::DayCounter>::create(fltDayCounterID);
		QuantLib::BusinessDayConvention fltBDC = 
			CreateEnum<QuantLib::BusinessDayConvention>::create(fltBDCID);
		QuantLib::Calendar calendar = CreateEnum<QuantLib::Calendar>::create(calendarID);
		QuantLib::TimeUnit timeUnits = CreateEnum<QuantLib::TimeUnit>::create(timeUnitsID);
		QuantLib::Currency crr = CreateEnum<QuantLib::Currency>::create(crrID);

		boost::shared_ptr<QuantLibAddin::YieldTermStructure> tmpFwdYC =
			OH_GET_OBJECT(YieldTermStructure, fwdCurveId);
		if (!tmpFwdYC)
			QL_FAIL("Xibor::Xibor Forecasting Curve not found: " + fwdCurveId);
		boost::shared_ptr<QuantLib::YieldTermStructure> fwdYC = 
			OH_GET_REFERENCE(QuantLib::YieldTermStructure, tmpFwdYC);
		QuantLib::Handle<QuantLib::YieldTermStructure> forecastingTermStructure; 
		forecastingTermStructure.linkTo(fwdYC);

		index_ = boost::shared_ptr<QuantLib::Xibor>(new QuantLib::Xibor(indexName, 
			tenor, timeUnits,
			fixingDays, crr, calendar, 
			fltBDC, fltDayCounter, forecastingTermStructure));
		QL_REQUIRE(fixings.size() == lDates.size(), "Xibor::Xibor the nuber of given dates does not match the number of fixings!");
		if(lDates.size() > 0 && !(lDates.size() == 1 && lDates[0] == 0)) {
			std::vector<QuantLib::Date> dates(lDates.size());
			std::transform(lDates.begin(), lDates.end(), dates.begin(), make_date);
			QuantLib::History history(dates, fixings);
			QuantLib::IndexManager::instance().setHistory(index_->name(), history);
		}
	}
}


