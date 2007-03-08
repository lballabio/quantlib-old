
/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#ifndef qla_date_hpp
#define qla_date_hpp

#include <ql/frequency.hpp>
#include <vector>

namespace QuantLib {
    class Period;
    class Date;
}

namespace QuantLibAddin {

    // to be removed using coercion
    QuantLib::Period periodFromFrequency(QuantLib::Frequency f);
    QuantLib::Frequency frequencyFromPeriod(const QuantLib::Period& p);

    std::vector<QuantLib::Date> qlNextIMMdates(const QuantLib::Date& d,
                                               const std::vector<bool>& mainCycle);

    std::vector<std::string> qlNextIMMcodes(const QuantLib::Date& d,
                                            const std::vector<bool>& mainCycle);

}

#endif
