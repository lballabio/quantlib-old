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
/*! \file vols.hpp
    \brief QuantLib Excel volatility functions

    \fullpath
    qlxl/%vols.hpp
*/

// $Id$

#ifndef qlxl_vols_h
#define qlxl_vols_h

#include <qlxl/qlxl.hpp>

extern "C"
{

    /* \todo make it array functions,
             and check if extrapolation is allowed
    */
    LPXLOPER EXCEL_EXPORT xlBlackVol(XlfOper xlrefDate,
                                     XlfOper xldc,
                                     XlfOper xldates,
                                     XlfOper xlstrikes,
                                     XlfOper xlblackVolSurface,
                                     XlfOper xldate1,
                                     XlfOper xldate2,
                                     XlfOper xlstrike,
                                     XlfOper xlinterpolation2DType,
                                     XlfOper xlallowExtrapolation);
}

#endif
