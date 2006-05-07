
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

/*! \file
    \brief Miscellaneous utility functions available to clients 
    of %QuantLibAddin i.e. the Addins
*/

#ifndef qla_clientutils_hpp
#define qla_clientutils_hpp

#include <ql/date.hpp>
#include <vector>

namespace QuantLibAddin {

/*! \group clientutils
    Miscellaneous utility functions for clients of %QuantLibAddin
*/

    /*! convert a long to a QuantLib date
    */
    QuantLib::Date createQLDate(const long &date);
    std::vector < QuantLib::Date > createQLDate(const std::vector < long > &dates);

    std::vector < long > dateToLongVec(const std::vector < QuantLib::Date > &v);
}

#endif

