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
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/functions/mathf.hpp>
#include <ql/Math/statistics.hpp>
#include <ql/Math/riskstatistics.hpp>
#include <ql/Math/symmetricschurdecomposition.hpp>
#include <ql/Math/pseudosqrt.hpp>
#include <ql/Math/choleskydecomposition.hpp>
#include <ql/Math/poissondistribution.hpp>
#include <ql/Math/binomialdistribution.hpp>


extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlinterpolate(XlfOper xlx_array,
                                        XlfOper xly_array,
                                        XlfOper xlx,
                                        XlfOper xlinterpolationType,
                                        XlfOper xlallowExtrapolation,
                                        XlfOper xlleftConditionType,
                                        XlfOper xlleftConditionValue,
                                        XlfOper xlrightConditionType,
                                        XlfOper xlrightConditionValue,
                                        XlfOper xlmonotonicityConstraint,
                                        XlfOper xlderivativeOrder) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        std::vector<double> x_value = xlx_array.AsDoubleVector();
        std::vector<double> y_value = xly_array.AsDoubleVector();
        QL_REQUIRE(x_value.size()==y_value.size(),
            "interpolate: array mismatch");

        std::vector<double> x = xlx.AsDoubleVector();
        XlfRef range = xlx.AsRef();
        Size rowNo = range.GetNbRows();
        Size colNo = range.GetNbCols();

        int interpolationType = xlinterpolationType.AsInt();
        bool allowExtrapolation = xlallowExtrapolation.AsBool();

        CubicSpline::BoundaryCondition leftConditionType =
            CubicSpline::BoundaryCondition(xlleftConditionType.AsInt());
        CubicSpline::BoundaryCondition rightConditionType =
            CubicSpline::BoundaryCondition(xlrightConditionType.AsInt());

        bool monotonicityConstraint = xlmonotonicityConstraint.AsBool();

        int derivativeOrder = xlderivativeOrder.AsInt();

        std::vector<double> result = interpolate(x_value.begin(), x_value.end(),
            y_value.begin(), x.begin(), x.end(),
            interpolationType,
            allowExtrapolation,
            leftConditionType, xlleftConditionValue.AsDouble(),
            rightConditionType, xlrightConditionValue.AsDouble(),
            monotonicityConstraint,
            derivativeOrder);
        return XlfOper(rowNo, colNo, result.begin());
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

        WIZARD_NO_CALC;

        std::vector<double> x_value = xlx_array.AsDoubleVector();
        std::vector<double> y_value = xly_array.AsDoubleVector();
        Matrix data_matrix = QlXlfOper(xlz_matrix).AsMatrix();
        QL_REQUIRE(data_matrix.columns()==x_value.size(),
            "the matrix range must be NxM");
        QL_REQUIRE(data_matrix.rows()==y_value.size(),
            "the matrix range must be NxM");

        double result = interpolate2D(x_value, y_value, data_matrix,
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

        WIZARD_NO_CALC;

        double result = normDist(xlx.AsDouble(), xlmean.AsDouble(),
            xlstd_dev.AsDouble(), xlcumulative.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlnormSDist(XlfOper xlx) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        double result = normDist(xlx.AsDouble(), 0.0,
            1.0, true);
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlnormInv(XlfOper xlprobability,
                                    XlfOper xlmean,
                                    XlfOper xlstd_dev) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        double result = normInv(xlprobability.AsDouble(),
            xlmean.AsDouble(),
            xlstd_dev.AsDouble());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlnormSInv(XlfOper xlprobability) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        double result = normInv(xlprobability.AsDouble(), 0.0, 1.0);
        return XlfOper(result);
        EXCEL_END;
    }



    LPXLOPER EXCEL_EXPORT xlPoisson(XlfOper xlx,
                                    XlfOper xlmean,
                                    XlfOper xlcumulative) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        bool cumulative = xlcumulative.AsBool();
        double result;
        if (cumulative) {
            result=CumulativePoissonDistribution(
                xlmean.AsDouble())(xlx.AsInt());
        } else {
            result=PoissonDistribution(xlmean.AsDouble())(xlx.AsInt());
        }
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlCombin(XlfOper number,
                                   XlfOper number_chosen) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        double result=binomialCoefficient(number.AsInt(),
                number_chosen.AsInt());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlBinomDist(XlfOper xlnumber_s,
                                      XlfOper xltrials,
                                      XlfOper xlprobability_s,
                                      XlfOper xlcumulative) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        bool cumulative = xlcumulative.AsBool();
        double result;
        if (cumulative) {
            result=CumulativeBinomialDistribution(xlprobability_s.AsDouble(),
                xltrials.AsInt())(xlnumber_s.AsInt());
        } else {
            result=BinomialDistribution(xlprobability_s.AsDouble(),
                xltrials.AsInt())(xlnumber_s.AsInt());
        }
        return XlfOper(result);
        EXCEL_END;
    }


    LPXLOPER EXCEL_EXPORT xlPeizerPratt(XlfOper xltrials,
                                        XlfOper xlpercentile) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        double result = PeizerPrattMethod2Inversion(xlpercentile.AsDouble(),
            xltrials.AsInt());
        return XlfOper(result);
        EXCEL_END;
    }











    #define TYPICAL_ARRAY_STAT_FUNCTION(METHOD) \
    LPXLOPER EXCEL_EXPORT METHOD( \
        XlfOper xldata_array) { \
        EXCEL_BEGIN; \
        WIZARD_NO_CALC; \
        std::vector<double> data_value = xldata_array.AsDoubleVector(); \
        GenericRiskStatistics<Statistics> s; \
        s.addSequence(data_value.begin(), data_value.end()); \
        double result = s.METHOD(); \
        return XlfOper(result); \
        EXCEL_END; \
    }
    TYPICAL_ARRAY_STAT_FUNCTION(mean)
    TYPICAL_ARRAY_STAT_FUNCTION(variance)
    TYPICAL_ARRAY_STAT_FUNCTION(standardDeviation)
    TYPICAL_ARRAY_STAT_FUNCTION(skewness)
    TYPICAL_ARRAY_STAT_FUNCTION(kurtosis)
    TYPICAL_ARRAY_STAT_FUNCTION(min)
    TYPICAL_ARRAY_STAT_FUNCTION(max)
    TYPICAL_ARRAY_STAT_FUNCTION(semiDeviation)
    TYPICAL_ARRAY_STAT_FUNCTION(semiVariance)
    TYPICAL_ARRAY_STAT_FUNCTION(downsideDeviation)
    TYPICAL_ARRAY_STAT_FUNCTION(downsideVariance)

    #define TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(METHOD) \
    LPXLOPER EXCEL_EXPORT METHOD( \
        XlfOper xldouble, XlfOper xldata_array) { \
        EXCEL_BEGIN; \
        WIZARD_NO_CALC; \
        std::vector<double> data_value = xldata_array.AsDoubleVector(); \
        GenericRiskStatistics<Statistics> s; \
        s.addSequence(data_value.begin(), data_value.end()); \
        double result = s.METHOD(xldouble.AsDouble()); \
        return XlfOper(result); \
        EXCEL_END; \
    }
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(percentile)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(valueAtRisk)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(topPercentile)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(potentialUpside)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(expectedShortfall)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(shortfall)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(averageShortfall)
    TYPICAL_DOUBLE_ARRAY_STAT_FUNCTION(regret)


    #define TYPICAL_2DOUBLE_STAT_FUNCTION(METHOD) \
    LPXLOPER EXCEL_EXPORT METHOD( \
        XlfOper xlmean, XlfOper xlstd_dev) { \
        EXCEL_BEGIN; \
        WIZARD_NO_CALC; \
        StatsHolder h(xlmean.AsDouble(), xlstd_dev.AsDouble()); \
        GaussianStatistics<StatsHolder> s(h); \
        double result = s.METHOD(); \
        return XlfOper(result); \
        EXCEL_END; \
    }
    TYPICAL_2DOUBLE_STAT_FUNCTION(gaussianDownsideDeviation)
    TYPICAL_2DOUBLE_STAT_FUNCTION(gaussianDownsideVariance)

    #define TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(METHOD) \
    LPXLOPER EXCEL_EXPORT METHOD( \
        XlfOper xldouble, XlfOper xlmean, XlfOper xlstd_dev) { \
        EXCEL_BEGIN; \
        WIZARD_NO_CALC; \
        StatsHolder h(xlmean.AsDouble(), xlstd_dev.AsDouble()); \
        GaussianStatistics<StatsHolder> s(h); \
        double result = s.METHOD(xldouble.AsDouble()); \
        return XlfOper(result); \
        EXCEL_END; \
    }
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianPercentile)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianValueAtRisk)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianTopPercentile)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianPotentialUpside)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianExpectedShortfall)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianShortfall)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianAverageShortfall)
    TYPICAL_DOUBLE_2DOUBLE_STAT_FUNCTION(gaussianRegret)









    LPXLOPER EXCEL_EXPORT xlprimeNumbers(XlfOper xlabsoluteIndex) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        return XlfOper(short(primeNumbers(xlabsoluteIndex.AsInt())));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xleigenVectors(XlfOper xlmatrix) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        Matrix result = SymmetricSchurDecomposition(data_matrix).eigenvectors();
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xleigenValues(XlfOper xlmatrix) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        Array result = SymmetricSchurDecomposition(data_matrix).eigenvalues();
        return XlfOper(result.size(), 1, result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlCholesky(XlfOper xlmatrix,
                                     XlfOper xlcholeskyFlexible) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        bool flexible = (xlcholeskyFlexible.IsMissing() ?
            false : xlcholeskyFlexible.AsBool());
        Matrix result = CholeskyDecomposition(data_matrix,
            flexible);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlpseudoSQRT(XlfOper xlmatrix,
                                       XlfOper xlsalvagingAlgorithm) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        SalvagingAlgorithm::Type sa =
            SalvagingAlgorithm::Type(xlsalvagingAlgorithm.AsInt());
        Matrix result = pseudoSqrt(data_matrix, sa);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlrankReducedSQRT(XlfOper xlmatrix,
                                            XlfOper xlmaxRank,
                                            XlfOper xlcomponentsRetainedPercentage,
                                            XlfOper xlsalvagingAlgorithm) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        int maxRank = xlmaxRank.AsInt();
        double componentsRetainedPercentage =
            xlcomponentsRetainedPercentage.AsDouble();
        SalvagingAlgorithm::Type sa = 
            SalvagingAlgorithm::Type(xlsalvagingAlgorithm.AsInt());
        Matrix result = rankReducedSqrt(data_matrix, maxRank, 
            componentsRetainedPercentage, sa);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }


    LPXLOPER EXCEL_EXPORT xlmatrixProduct(XlfOper xlmatrix,
                                          XlfOper xlmatrix2) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix  = QlXlfOper(xlmatrix ).AsMatrix();
        Matrix data_matrix2 = QlXlfOper(xlmatrix2).AsMatrix();
        Matrix result = data_matrix * data_matrix2;
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlmatrixTranspose(XlfOper xlmatrix) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix  = QlXlfOper(xlmatrix ).AsMatrix();
        Matrix result = transpose(data_matrix);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }


    LPXLOPER EXCEL_EXPORT xlrandomize(XlfOper xlseed) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        randomize(xlseed.AsInt());
        return XlfOper(std::string("done with " + 
            IntegerFormatter::toString(xlseed.AsInt())).c_str());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlrand() {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        return XlfOper(QuantLib::rand());
        EXCEL_END;
    }

}
