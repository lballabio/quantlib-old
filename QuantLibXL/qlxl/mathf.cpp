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
/*! \file mathf.cpp
    \brief QuantLib Excel math functions

    \fullpath
    qlxl/%mathf.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>
#include <qlxl/mathf.hpp>

extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlinterpolate(XlfOper xlx_array,
                                        XlfOper xly_array,
                                        XlfOper xlx,
                                        XlfOper xlinterpolationType,
                                        XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;

        std::vector<double> x_value = xlx_array.AsDoubleVector();
        std::vector<double> y_value = xly_array.AsDoubleVector();
        QL_REQUIRE(x_value.size()==y_value.size(),
            "interpolate: array mismatch");
        
        double result = Functions::interpolate(x_value, y_value, xlx.AsDouble(),
            xlinterpolationType.AsInt(), xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlinterpolate2D(XlfOper xlx_array,
                                          XlfOper xly_array,
                                          XlfOper xlz_matrix,
                                          XlfOper xlx,
                                          XlfOper xly,
                                          XlfOper xlinterpolation2DType,
                                          XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;
        std::vector<double> x_value = xlx_array.AsDoubleVector();
        std::vector<double> y_value = xly_array.AsDoubleVector();


        // Initialization of the counter variables.
        size_t i, j, rowNo = y_value.size(), colNo = x_value.size();

        XlfRef matrix_range = xlz_matrix.AsRef();
        QL_REQUIRE(matrix_range.GetNbCols()==colNo,
            "the matrix range must be NxM");
        QL_REQUIRE(matrix_range.GetNbRows()==rowNo,
            "the matrix range must be NxM");


        // XlfExcel::Coerce method (internally called) will return to Excel
        // if one of the cell was invalidated and need to be recalculated.
        // Excel will calculate this cell and call again our function.
        // Thus we copy first all the data to avoid to partially compute the
        // average for nothing since one of the cell might be uncalculated.

        // Allocate the vectors where to copy the values.
        Math::Matrix data_matrix(rowNo, colNo);
        for (i = 0; i < rowNo; ++i) {
            for (j = 0; j < colNo; ++j) {
                data_matrix[i][j] = matrix_range(i,j).AsDouble();
            }
        }
        
        double result = Functions::interpolate2D(x_value, y_value, data_matrix,
            xlx.AsDouble(), xly.AsDouble(), xlinterpolation2DType.AsInt(),
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }





    LPXLOPER EXCEL_EXPORT xlnormDist(XlfOper xlx,
                                     XlfOper xlmean,
                                     XlfOper xlstd_dev,
                                     XlfOper xlcumulative) {
        EXCEL_BEGIN;
        double result = Functions::normDist(xlx.AsDouble(), xlmean.AsDouble(),
            xlstd_dev.AsDouble(), xlcumulative.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlnormSDist(XlfOper xlx) {
        EXCEL_BEGIN;
        double result = Functions::normDist(xlx.AsDouble(), 0.0,
            1.0, true);
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlnormInv(XlfOper xlprobability,
                                    XlfOper xlmean,
                                    XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result = Functions::normInv(xlprobability.AsDouble(), xlmean.AsDouble(),
            xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlnormSInv(XlfOper xlprobability) {
        EXCEL_BEGIN;
        double result = Functions::normInv(xlprobability.AsDouble(), 0.0, 1.0);
        return XlfOper(result);
        EXCEL_END;
    }



    LPXLOPER EXCEL_EXPORT xlpotentialUpside(XlfOper xlpercentile,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result = Math::RiskMeasures().potentialUpside(xlpercentile.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlvalueAtRisk(XlfOper xlpercentile,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result = Math::RiskMeasures().valueAtRisk(xlpercentile.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlexpectedShortfall(XlfOper xlpercentile,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result = Math::RiskMeasures().expectedShortfall(xlpercentile.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlshortfall(XlfOper xltarget,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result = Math::RiskMeasures().shortfall(xltarget.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlaverageShortfall(XlfOper xltarget,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result = Math::RiskMeasures().averageShortfall(xltarget.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }



}
