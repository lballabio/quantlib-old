
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

/*! \file
    \brief Implementations of the functions defined in settings.xml
    for use within the Addins.

*/

#ifndef qla_settings_hpp
#define qla_settings_hpp
#include <ql/date.hpp>
#include <ql/settings.hpp>

namespace QuantLibAddin {

/*! \group utilities
    diagnostic and information functions for QuantLibAddin
*/

    /*! set the evaluation date */
    inline QuantLib::Date setEvaluationDate(const QuantLib::Date &evalDate) {
        QuantLib::Settings::instance().evaluationDate() = evalDate;
        return evalDate;
    }

}

#endif
