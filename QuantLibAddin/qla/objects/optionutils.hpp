
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

#ifndef qla_optionutils_hpp
#define qla_optionutils_hpp

#include <ql/option.hpp>
#include <ql/Instruments/payoffs.hpp>
#include <ql/Math/matrix.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_NPV                       "NPV"
#define FIELD_ENGINE                    "ENGINE"
#define IDX_NPV                         0
#define IDX_ENGINE                      1

namespace QuantLibAddin {

    QuantLib::Option::Type IDtoOptionType(
        const std::string &optionTypeID);

    boost::shared_ptr<QuantLib::StrikedTypePayoff> IDtoPayoff(
        const std::string &optionTypeID,
        const std::string &payoffID,
        const QuantLib::Real &input1,
        const QuantLib::Real &input2 = 0.);

    boost::shared_ptr<QuantLib::Exercise> IDtoExercise(
        const std::string &exerciseID,
        const QuantLib::Date &exerciseDate,
        const QuantLib::Date &settlementDate);

    boost::shared_ptr<QuantLib::PricingEngine> IDtoEngine(
        const std::string &engineID,
        const QuantLib::Size &timeSteps);

    QuantLib::Matrix vectorVectorToMatrix(
        const std::vector < const std::vector < double > > &vv);

    std::vector<QuantLib::Date> longVectorToDateVector(
        const std::vector < long > &v);

}

#endif

