/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano

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
/*! \file mathf.cpp
    \brief QuantLib Excel math functions

    \fullpath
    qlxl/%mathf.cpp
*/

// $Id$

#include <qlxl/qlxlfoper.hpp>

extern "C"
{


    using namespace QuantLib;
    using namespace QuantLib::Math;

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

        double result = Functions::interpolate(x_value, y_value,
            xlx.AsDouble(),
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
        Matrix data_matrix = QlXlfOper(xlz_matrix).AsMatrix();
        QL_REQUIRE(data_matrix.columns()==x_value.size(),
            "the matrix range must be NxM");
        QL_REQUIRE(data_matrix.rows()==y_value.size(),
            "the matrix range must be NxM");

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
        double result = Functions::normInv(xlprobability.AsDouble(),
            xlmean.AsDouble(),
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
        double result =
            RiskMeasures().gaussianPotentialUpside(xlpercentile.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlvalueAtRisk(XlfOper xlpercentile,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result =
            RiskMeasures().gaussianValueAtRisk(xlpercentile.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlexpectedShortfall(XlfOper xlpercentile,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result =
            RiskMeasures().gaussianExpectedShortfall(
            xlpercentile.AsDouble(), xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlshortfall(XlfOper xltarget,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result =
            RiskMeasures().gaussianShortfall(xltarget.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlaverageShortfall(XlfOper xltarget,
        XlfOper xlmean, XlfOper xlstd_dev) {
        EXCEL_BEGIN;
        double result =
            RiskMeasures().gaussianAverageShortfall(xltarget.AsDouble(),
            xlmean.AsDouble(), xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlprimeNumbers(XlfOper xlabsoluteIndex) {
        EXCEL_BEGIN;
        return XlfOper(short(Functions::primeNumbers(xlabsoluteIndex.AsInt())));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xleigenVectors(XlfOper xlmatrix) {
        EXCEL_BEGIN;
        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        Matrix result = SymmetricSchurDecomposition(data_matrix).eigenvectors();
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xleigenValues(XlfOper xlmatrix) {
        EXCEL_BEGIN;
        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        Array result = SymmetricSchurDecomposition(data_matrix).eigenvalues();
        return XlfOper(result.size(), 1, result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlpseudoSQRT(XlfOper xlmatrix,
                                       XlfOper xlsalvagingAlgorithm) {
        EXCEL_BEGIN;
        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        SalvagingAlgorithm sa = SalvagingAlgorithm(xlsalvagingAlgorithm.AsInt());
        Matrix result = pseudoSqrt(data_matrix, sa);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlmatrixProduct(XlfOper xlmatrix,
                                          XlfOper xlmatrix2) {
        EXCEL_BEGIN;
        Matrix data_matrix  = QlXlfOper(xlmatrix ).AsMatrix();
        Matrix data_matrix2 = QlXlfOper(xlmatrix2).AsMatrix();
        Matrix result = data_matrix * data_matrix2;
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlmatrixTranspose(XlfOper xlmatrix) {
        EXCEL_BEGIN;
        Matrix data_matrix  = QlXlfOper(xlmatrix ).AsMatrix();
        Matrix result = transpose(data_matrix);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlrandomize(XlfOper xlseed) {
        EXCEL_BEGIN;
        Functions::randomize(xlseed.AsInt());
        return XlfOper(std::string("done").c_str());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlrand() {
        EXCEL_BEGIN;
        return XlfOper(Functions::rand());
        EXCEL_END;
    }

}
