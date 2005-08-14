
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

#if defined(HAVE_CONFIG_H)
    #include <qla/config.hpp>
#endif

#include "qla/simpleswap.hpp"
#include "qla/generalutils.hpp"
#include "qla/termstructures.hpp"
#include "qla/typefactory.hpp"
#include "qla/xibor.hpp"
#include <ql/CashFlows/fixedratecoupon.hpp>
#include <ql/CashFlows/parcoupon.hpp>
#include <vector>

namespace QuantLibAddin {
	SimpleSwap::SimpleSwap(
            const long &lStartDate,
		    const long &lMaturity,
		    const QuantLib::Real &nominal,
		    const bool &payFixed,
		    const QuantLib::Rate &fixRate,
		    const std::string &calendarID,
 		    const std::string &fixFrqID,
		    const std::string &fixBDCID,
		    const std::string &fixDayCounterID,
		    const bool &fixStartFromEnd,
		    const bool &fixLongFinal,
 		    const std::string &fltFrqID,
		    const std::string &indexHandle,
		    const bool &floatStartFromEnd,
		    const bool &floatLongFinal,
		    const QuantLib::Rate &floatSpread,
		    const std::string &discCurveId) {

		QuantLib::DayCounter fixDayCounter =
            Create<QuantLib::DayCounter>()(fixDayCounterID);
		QuantLib::BusinessDayConvention fixBDC = 
            Create<QuantLib::BusinessDayConvention>()(fixBDCID);
		QuantLib::Frequency fixFrq =
            Create<QuantLib::Frequency>()(fixFrqID);
		QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
		QuantLib::Date maturity = QuantLib::Date(lMaturity);
		QuantLib::Date startDate = QuantLib::Date(lStartDate);

		boost::shared_ptr<QuantLibAddin::YieldTermStructure> tmpDiscYC =
			OH_GET_OBJECT(QuantLibAddin::YieldTermStructure, discCurveId);
		QL_REQUIRE(tmpDiscYC, "SimpleSwap::SimpleSwap Discounting Curve not found: " + discCurveId);
		boost::shared_ptr<QuantLib::YieldTermStructure> discYC = 
			OH_GET_REFERENCE(QuantLib::YieldTermStructure, tmpDiscYC);
		QuantLib::Handle<QuantLib::YieldTermStructure> discountingTermStructure;
		discountingTermStructure.linkTo(discYC);

		boost::shared_ptr<QuantLibAddin::Xibor> tmpIndex =
			OH_GET_OBJECT(QuantLibAddin::Xibor, indexHandle);
		QL_REQUIRE(tmpIndex, "SimpleSwap::SimpleSwap Index not found: " + indexHandle);
		boost::shared_ptr<QuantLib::Xibor> index = OH_GET_REFERENCE(QuantLib::Xibor, tmpIndex);

		QuantLib::Schedule fixedSchedule(calendar, startDate, maturity, fixFrq, fixBDC, 
			QuantLib::Date(), fixStartFromEnd, fixLongFinal);

		QuantLib::Schedule floatSchedule(calendar, startDate, maturity,	
			index->frequency(), index->businessDayConvention(), 
			QuantLib::Date(), floatStartFromEnd, floatLongFinal);

		mInstrument = boost::shared_ptr<QuantLib::Instrument>(new QuantLib::SimpleSwap(
			payFixed, nominal,
			fixedSchedule, fixRate, fixDayCounter, floatSchedule, 
			index, index->settlementDays(), floatSpread,
			discountingTermStructure));
	}

	const std::vector<std::vector<double> >&
	SimpleSwap::getFixLeg() {
		const std::vector<boost::shared_ptr<QuantLib::CashFlow> >& flows = getObject().fixedLeg();
		fixLeg.clear();
		for(size_t i = 0; i < flows.size(); i++) {
			std::vector<double> cf;
			QuantLib::FixedRateCoupon& c = (QuantLib::FixedRateCoupon&) *(flows[i]);
			cf.push_back(c.accrualStartDate().serialNumber());
			cf.push_back(c.accrualEndDate().serialNumber());
			cf.push_back(c.date().serialNumber());
			cf.push_back(c.accrualPeriod());
			cf.push_back(c.accrualDays());
			cf.push_back(c.amount());
			fixLeg.push_back(cf);
		}
		return fixLeg;
	}

	const std::vector<std::vector<double> >&
	SimpleSwap::getFloatLeg() {
		const std::vector<boost::shared_ptr<QuantLib::CashFlow> >& flows = getObject().floatingLeg();
		floatLeg.clear();
		for(size_t i = 0; i < flows.size(); i++) {
			std::vector<double> cf;
			QuantLib::ParCoupon& c = (QuantLib::ParCoupon&)*(flows[i]);
			cf.push_back(c.accrualStartDate().serialNumber());
			cf.push_back(c.accrualEndDate().serialNumber());
			cf.push_back(c.date().serialNumber());
			cf.push_back(c.fixingDate().serialNumber());
			cf.push_back(c.accrualPeriod());
			cf.push_back(c.accrualDays());
			cf.push_back(c.amount());
			cf.push_back(c.indexFixing());
			floatLeg.push_back(cf);
		}
		return floatLeg;
	}
}

