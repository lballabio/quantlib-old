
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

#if defined(HAVE_CONFIG_H)
    #include <qla/config.hpp>
#endif

#include <qla/simpleswap.hpp>
#include <qla/generalutils.hpp>
#include <qla/termstructures.hpp>
#include <qla/xibor.hpp>

namespace QuantLibAddin {
	SimpleSwap::SimpleSwap(ObjHandler::ArgumentStack& arguments) {
		std::string discCurveId     = OH_POP_ARGUMENT(std::string, arguments);
		QuantLib::Rate floatSpread  = OH_POP_ARGUMENT(QuantLib::Rate, arguments);
		bool floatLongFinal         = OH_POP_ARGUMENT(bool, arguments);
		bool floatStartFromEnd      = OH_POP_ARGUMENT(bool, arguments);
		std::string indexHandle     = OH_POP_ARGUMENT(std::string, arguments);
 		std::string fltFrqID        = OH_POP_ARGUMENT(std::string, arguments);
		bool fixLongFinal           = OH_POP_ARGUMENT(bool, arguments);
		bool fixStartFromEnd        = OH_POP_ARGUMENT(bool, arguments);
		std::string fixDayCounterID = OH_POP_ARGUMENT(std::string, arguments);
		std::string fixBDCID        = OH_POP_ARGUMENT(std::string, arguments);
 		std::string fixFrqID        = OH_POP_ARGUMENT(std::string, arguments);
		std::string calendarID      = OH_POP_ARGUMENT(std::string, arguments);
		QuantLib::Rate fixRate      = OH_POP_ARGUMENT(QuantLib::Rate, arguments);
		bool payFixed               = OH_POP_ARGUMENT(bool, arguments);
		QuantLib::Real nominal      = OH_POP_ARGUMENT(QuantLib::Real, arguments);
		long lMaturity              = OH_POP_ARGUMENT(long, arguments);
		long lStartDate             = OH_POP_ARGUMENT(long, arguments);

//		QuantLib::Frequency fltFrq = IDtoFrequency(fltFrqID);
		QuantLib::DayCounter fixDayCounter = IDtoDayCounter(fixDayCounterID);
		QuantLib::BusinessDayConvention fixBDC = IDtoConvention(fixBDCID);
		QuantLib::Frequency fixFrq = IDtoFrequency(fixFrqID);
		QuantLib::Calendar calendar = IDtoCalendar(calendarID);
		QuantLib::Date maturity = QuantLib::Date(lMaturity);
		QuantLib::Date startDate = QuantLib::Date(lStartDate);

		boost::shared_ptr<QuantLibAddin::YieldTermStructure> tmpDiscYC =
			OH_GET_OBJECT(QuantLibAddin::YieldTermStructure, discCurveId);
		if (!tmpDiscYC)
			QL_FAIL("SimpleSwap::SimpleSwap Discounting Curve not found: " + discCurveId);
		boost::shared_ptr<QuantLib::YieldTermStructure> discYC = 
			OH_GET_REFERENCE(QuantLib::YieldTermStructure, tmpDiscYC);
		QuantLib::Handle<QuantLib::YieldTermStructure> discountingTermStructure;
		discountingTermStructure.linkTo(discYC);

		boost::shared_ptr<QuantLibAddin::Xibor> tmpIndex =
			OH_GET_OBJECT(QuantLibAddin::Xibor, indexHandle);
		if (!tmpIndex)
			QL_FAIL("SimpleSwap::SimpleSwap Index not found: " + indexHandle);
		boost::shared_ptr<QuantLib::Xibor> index = OH_GET_REFERENCE(QuantLib::Xibor, tmpIndex);

		QuantLib::Schedule fixedSchedule(calendar, startDate, maturity, fixFrq, fixBDC, 
			QuantLib::Date(), fixStartFromEnd, fixLongFinal);

		QuantLib::Schedule floatSchedule(calendar, startDate, maturity,	
			index->frequency(), index->businessDayConvention(), 
			QuantLib::Date(), floatStartFromEnd, floatLongFinal);

		swap_ = boost::shared_ptr<QuantLib::SimpleSwap>(new QuantLib::SimpleSwap(
			payFixed, nominal,
			fixedSchedule, fixRate, fixDayCounter, floatSchedule, 
			index, index->settlementDays(), floatSpread,
			discountingTermStructure));

		properties_.push_back(ObjHandler::ObjectProperty("NPV", 
			ObjHandler::any_ptr(new boost::any(swap_->NPV()))));
		properties_.push_back(ObjHandler::ObjectProperty("FAIR_RATE", 
			ObjHandler::any_ptr(new boost::any(swap_->fairRate()))));
		properties_.push_back(ObjHandler::ObjectProperty("FAIR_SPREAD", 
			ObjHandler::any_ptr(new boost::any(swap_->fairSpread()))));
		properties_.push_back(ObjHandler::ObjectProperty("FIXED_LEG_BPS", 
			ObjHandler::any_ptr(new boost::any(swap_->fixedLegBPS()))));
		properties_.push_back(ObjHandler::ObjectProperty("FLOATING_LEG_BPS", 
			ObjHandler::any_ptr(new boost::any(swap_->floatingLegBPS()))));
	}
}

