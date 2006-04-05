
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef qla_cliquetoption_hpp
#define qla_cliquetoption_hpp

#include <qla/baseinstruments.hpp>
#include <qla/processes.hpp>
#include <ql/Instruments/cliquetoption.hpp>

namespace QuantLibAddin {

    class CliquetOption : public OneAssetOption {
    public:
        CliquetOption::CliquetOption(
            const std::string &handleBlackScholes,
            const std::string &optionTypeID,
            const double &strike,
            const long &expiryDate,
            const std::vector < long > &resetDates,
            const std::string &engineID,
            const long &timeSteps);

        EXPORT_QL_OBJECT(QuantLib::CliquetOption);
    };

}

#endif

