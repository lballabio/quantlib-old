
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <oh/objhandler.hpp>
#include <qla/generalutils.hpp>
#include <ql/Calendars/all.hpp>
#include <boost/lexical_cast.hpp>
#include <ql/Currencies/all.hpp>

namespace QuantLibAddin {
    std::vector<QuantLib::Date> longVectorToDateVector(
            const std::vector < long > &v) {
        std::vector<QuantLib::Date> ret;
        std::vector<long>::const_iterator i;
        for (i=v.begin(); i!=v.end(); i++)
            ret.push_back(QuantLib::Date(*i));
        return ret;
    }

    QuantLib::Matrix vectorVectorToMatrix(
        const std::vector < std::vector < double > > &vv) {
        if (vv.size() == 0) {
            QuantLib::Matrix m(0, 0);
            return m;
        } else {
            const std::vector < double > v = vv[0];
            if (v.size() == 0) {
                QuantLib::Matrix m(0, 0);
                return m;
            }
            QuantLib::Matrix m(vv.size(), v.size());
            for (unsigned int i = 0; i < vv.size(); i++) {
                const std::vector < double > v = vv[i];
                for (unsigned int j = 0; j < v.size(); j++)
                    m[i][j] = v[j];
            }
            return m;
        }
    }

	QuantLib::Date FutIDtoExpiryDate(
		const std::string& immID,
		const QuantLib::Calendar& calendar, 
		QuantLib::BusinessDayConvention bdc,
		QuantLib::Integer decade) {
		if(decade % 10 != 0)
			QL_FAIL("FutIDtoExpiryDate: wrong decade: " + boost::lexical_cast<std::string>(decade));
		QuantLib::Month m;
		switch(immID[0]) {
			case 'H':
			case 'h': m = QuantLib::March; break;
			case 'M':
			case 'm': m = QuantLib::June; break;
			case 'U':
			case 'u': m = QuantLib::September; break;
			case 'Z':
			case 'z': m = QuantLib::December; break;
			default: QL_FAIL("FutIDtoExpiryDate: Unknown IMM Id: " + immID); break;
		}
		QuantLib::Year year = boost::lexical_cast<QuantLib::Year>(immID.substr(1));
		if(year < 0 || year > 9)
			QL_FAIL("FutIDtoExpiryDate: wrong year: " + immID);
		QuantLib::Date immDate = QuantLib::Date(1, m, year + decade);
		immDate = QuantLib::Date::nextIMMdate(immDate);
		return calendar.adjust(immDate, bdc);
	}
}


