/*
 Copyright (C) 2002 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/
/*! \file datef.hpp
    \brief QuantLib Excel date functions

    \fullpath
    qlxl/%datef.hpp
*/

// $Id$

#ifndef qlxl_datef_h
#define qlxl_datef_h

#include <qlxl/qlxl.hpp>

extern "C"
{

    LPXLOPER EXCEL_EXPORT xlaccrualDays(XlfOper xlDate1,
                                        XlfOper xlDate2,
                                        XlfOper xlDayCountType);

    LPXLOPER EXCEL_EXPORT xlaccrualFactor(XlfOper xlDate1,
                                          XlfOper xlDate2,
                                          XlfOper xlDayCountType,
                                          XlfOper xlDate3,
                                          XlfOper xlDate4);
}

#endif
