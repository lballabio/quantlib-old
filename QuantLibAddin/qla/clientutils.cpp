
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005 Eric Ehlers

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
#include <qla/clientutils.hpp>

namespace QuantLibAddin {

    QuantLib::Date createQLDate(const long &date) {
        if (date) 
            return QuantLib::Date(date);
        else 
            return QuantLib::Date();
    }

    std::vector < QuantLib::Date > createQLDate(const std::vector < long > &dates) {
        std::vector < QuantLib::Date > ret;
        for (std::vector < long >::const_iterator i = dates.begin();
            i != dates.end(); i++)
            ret.push_back(QuantLib::Date(*i));
        return ret;
    }

    std::vector < long > dateToLongVec(const std::vector < QuantLib::Date > &v) {
        std::vector < long > ret;
        for (std::vector < QuantLib::Date >::const_iterator i = v.begin();
            i != v.end(); i++)
            ret.push_back(i->serialNumber());
        return ret;
    }

}

