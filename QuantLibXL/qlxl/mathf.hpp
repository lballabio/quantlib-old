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
/*! \file mathf.hpp
    \brief QuantLib Excel math functions

    \fullpath
    qlxl/%mathf.hpp
*/

// $Id$

#ifndef qlxl_mathf_h
#define qlxl_mathf_h

#include <qlxl/qlxl.hpp>

extern "C"
{

    /* \todo make it array functions, 
             and check if extrapolation is allowed
    */
    LPXLOPER EXCEL_EXPORT xlinterpolate(XlfOper xlx_array,
                                        XlfOper xly_array,
                                        XlfOper xlx,
                                        XlfOper xlinterpolationType,
                                        XlfOper xlallowExtrapolation);

    LPXLOPER EXCEL_EXPORT xlinterpolate2D(XlfOper xlx_array,
                                          XlfOper xly_array,
                                          XlfOper xlz_matrix,
                                          XlfOper xlx,
                                          XlfOper xly,
                                          XlfOper xlinterpolation2DType,
                                          XlfOper xlallowExtrapolation);

    LPXLOPER EXCEL_EXPORT xlnormDist(XlfOper xlx,
                                     XlfOper xlmean,
                                     XlfOper xlstd_dev,
                                     XlfOper xlcumulative);
    LPXLOPER EXCEL_EXPORT xlnormSDist(XlfOper xlx);
    LPXLOPER EXCEL_EXPORT xlnormInv(XlfOper xlprobability,
                                    XlfOper xlmean,
                                    XlfOper xlstd_dev);
    LPXLOPER EXCEL_EXPORT xlnormSInv(XlfOper xlprobability);


    
    LPXLOPER EXCEL_EXPORT xlpotentialUpside(XlfOper xlpercentile,
                                            XlfOper xlmean,
                                            XlfOper xlstd_dev);
    LPXLOPER EXCEL_EXPORT xlvalueAtRisk(XlfOper xlpercentile,
                                        XlfOper xlmean,
                                        XlfOper xlstd_dev);
    LPXLOPER EXCEL_EXPORT xlexpectedShortfall(XlfOper xlpercentile,
                                              XlfOper xlmean,
                                              XlfOper xlstd_dev);
    LPXLOPER EXCEL_EXPORT xlshortfall(XlfOper xltarget,
                                      XlfOper xlmean,
                                      XlfOper xlstd_dev);
    LPXLOPER EXCEL_EXPORT xlaverageShortfall(XlfOper xltarget,
                                             XlfOper xlmean,
                                             XlfOper xlstd_dev);
}

#endif
