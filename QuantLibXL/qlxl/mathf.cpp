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

        size_t n, i;
        std::vector<double> x_value;
        XlfRef x_range = xlx_array.AsRef();
        if (x_range.GetNbCols()==1) {
            n = x_range.GetNbRows();
            x_value = std::vector<double>(n);
            for (i = 0; i < n; ++i) {
                x_value[i] = x_range(i,0).AsDouble();
            }
        } else if (x_range.GetNbRows()==1) {
            n = x_range.GetNbCols();
            x_value = std::vector<double>(n);
            for (i = 0; i < n; ++i) {
                x_value[i] = x_range(0,i).AsDouble();
            }
        } else
            throw Error("x-range must be an array");

        
        std::vector<double> y_value(n);
        XlfRef y_range = xly_array.AsRef();
        if (y_range.GetNbCols()==1) {
            QL_REQUIRE(y_range.GetNbRows()==n,
                "y-range does not match x-range");
            for (i = 0; i < n; ++i) {
                y_value[i] = y_range(i,0).AsDouble();
            }
        } else if (y_range.GetNbRows()==1) {
            QL_REQUIRE(y_range.GetNbCols()==n,
                "y-range does not match x-range");
            for (i = 0; i < n; ++i) {
                y_value[i] = y_range(0,i).AsDouble();
            }
        } else
            throw Error("y-range must be an array");

        
        
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
        XlfRef x_range = xlx_array.AsRef();
        QL_REQUIRE(x_range.GetNbRows()==1,
            "x-range must be one raw");
        
        XlfRef y_range = xly_array.AsRef();
        QL_REQUIRE(y_range.GetNbCols()==1,
            "y-range must be one column");

        // Initialization of the counter variables.
        size_t i, j, rowNo = y_range.GetNbRows(), colNo = x_range.GetNbCols();

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
        std::vector<double> x_value(colNo);
        std::vector<double> y_value(rowNo);
        Math::Matrix data_matrix(rowNo, colNo);
        for (i = 0; i < rowNo; ++i) {
            for (j = 0; j < colNo; ++j) {
                data_matrix[i][j] = matrix_range(i,j).AsDouble();
            }
        }
        for (j = 0; j < colNo; ++j) {
            x_value[j] = x_range(0, j).AsDouble();
        }
        for (i = 0; i < rowNo; ++i) {
            y_value[i] = y_range(i, 0).AsDouble();
        }
        
        double result = Functions::interpolate2D(x_value, y_value, data_matrix,
            xlx.AsDouble(), xly.AsDouble(), xlinterpolation2DType.AsInt(),
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }
}
