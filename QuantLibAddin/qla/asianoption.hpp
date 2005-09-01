
/*
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

#ifndef qla_asianoption_hpp
#define qla_asianoption_hpp

#include <qla/baseinstruments.hpp>
#include <qla/processes.hpp>
#include <ql/Instruments/asianoption.hpp>

namespace QuantLibAddin {

    class ContinuousAveragingAsianOption : public Instrument {
    public:
        ContinuousAveragingAsianOption(
            const std::string &handleBlackScholes,
            const std::string &averageID,
            const std::string &optionTypeID,
            const std::string &payoffID,
            const double &strike,
            const std::string &exerciseID,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engineID,
            const long &timeSteps);

        EXPORT_QL_OBJECT(QuantLib::ContinuousAveragingAsianOption);
    };

    class DiscreteAveragingAsianOption : public Instrument {
    public:
        DiscreteAveragingAsianOption(
            const std::string &handleBlackScholes,
            const std::string &averageID,
            const double &runningAccumulator,
            const long &pastFixings,
            const std::vector < long > &fixingDates,
            const std::string &optionTypeID,
            const std::string &payoffID,
            const double &strike,
            const std::string &exerciseID,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engineID,
            const long &timeSteps);

        EXPORT_QL_OBJECT(QuantLib::DiscreteAveragingAsianOption);
    };

}

#endif

