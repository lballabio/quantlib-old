
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

#ifndef qla_simpleswap_hpp
#define qla_simpleswap_hpp

#include <qla/baseinstruments.hpp>
#include <ql/Instruments/simpleswap.hpp>

namespace QuantLibAddin {

    class SimpleSwap : public Instrument {
    public:
        SimpleSwap(
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
		    const std::string &discCurveId);

		EXPORT_QL_OBJECT(QuantLib::SimpleSwap)

		const std::vector<std::vector<double> >& getFixLeg();
		const std::vector<std::vector<double> >& getFloatLeg();

    private:
		std::vector<std::vector<double> > fixLeg;
		std::vector<std::vector<double> > floatLeg;
    };
}

#endif

