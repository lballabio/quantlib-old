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
/*! \file qlxl.cpp
    \brief QuantLib Excel add-in

    \fullpath
    qlxl/%qlxl.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>

extern "C"
{


    using namespace QuantLib;


    long EXCEL_EXPORT xlAutoOpen()
    {
//        EXCEL_BEGIN;

        // Displays a message in the status bar.
        XlfExcel::Instance().SendMessage(
            "Registering QuantLibXL v"
            QLXL_VERSION
            " library...");


        // description of input variables
        XlfArgDesc           d01(        "date1", "first date");
        XlfArgDesc           d02(        "date2", "second date");
        XlfArgDesc           d03(        "date3", "reference period first date");
        XlfArgDesc           d04(        "date4", "reference period last date");

        XlfArgDesc          accr(     "dayCount", "accrual convention");

        XlfArgDesc    optionType(         "type", "option type");
        XlfArgDesc    underlying(   "underlying", "spot value of the underlying");
        XlfArgDesc        strike(       "strike", "option's strike");
        XlfArgDesc dividendYield("dividendYield", "dividend yield");
        XlfArgDesc  riskFreeRate( "riskFreeRate", "risk free rate");
        XlfArgDesc      maturity(     "maturity", "option's maturity");
        XlfArgDesc    volatility(   "volatility", "underlying's volatility");

        XlfArgDesc x_array("x_array", "x data array");
        XlfArgDesc y_array("y_array", "y data array");
        XlfArgDesc z_matrix("z_matrix", "z data matrix");
        XlfArgDesc x_value("x_value", "x value to be interpolated");
        XlfArgDesc y_value("y_value", "y value to be interpolated");
        XlfArgDesc interpolationType("interpolation_type",
            "interpolation type");
        XlfArgDesc interpolation2DType("interpolation2D_type",
            "2D interpolation type");
        XlfArgDesc allowExtrapolation("allow_extrapolation",
            "allow extrapolation boolean");

        // Registers qlversion
        XlfFuncDesc QLversion("xlQLversion","QLversion",
            "QuantLib version string","qlUtilities");
        QLversion.Register();

        // Registers qlhexversion
        XlfFuncDesc QLhexversion("xlQLhexversion","QLhexversion",
            "QuantLib version number","qlUtilities");
        QLhexversion.Register();

        // Registers xlwversion
        XlfFuncDesc XLWversion("xlXLWversion","XLWversion",
            "QuantLib version string","qlUtilities");
        XLWversion.Register();

        // Registers xlwhexversion
        XlfFuncDesc XLWhexversion("xlXLWhexversion","XLWhexversion",
            "QuantLib version number","qlUtilities");
        XLWhexversion.Register();

        // Registers qlxlversion
        XlfFuncDesc QLXLversion("xlQLXLversion","QLXLversion",
            "QuantLib version string","qlUtilities");
        QLXLversion.Register();

        // Registers qlxlhexversion
        XlfFuncDesc QLXLhexversion("xlQLXLhexversion","QLXLhexversion",
            "QuantLib version number","qlUtilities");
        QLXLhexversion.Register();


        // Registers accrual_days
        XlfFuncDesc accrualDaysDesc("xlaccrualDays","qlAccrual_days",
            "Accrual days","qlDates");
        accrualDaysDesc.SetArguments(d01+d02+accr);
        accrualDaysDesc.Register();

        XlfFuncDesc accrualFactorDesc("xlaccrualFactor","qlAccrual_factor",
            "Accrual factor","qlDates");
        accrualFactorDesc.SetArguments(d01+d02+accr+d03+d04);
        accrualFactorDesc.Register();

        // Registers Black-Scholes
        XlfFuncDesc blackscholes("xlBlackScholes","BlackScholes",
            "Black Scholes formula","qlPricers");
        blackscholes.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+maturity+volatility);
        blackscholes.Register();


        // Registers interpolation
        XlfFuncDesc interpolateDesc("xlinterpolate","qlinterpolate",
            "1 dimensional interpolation","qlMath");
        interpolateDesc.SetArguments(x_array+y_array+x_value+interpolationType+allowExtrapolation);
        interpolateDesc.Register();

        XlfFuncDesc interpolate2DDesc("xlinterpolate2D","qlinterpolate2D",
            "2 dimensional interpolation","qlMath");
        interpolate2DDesc.SetArguments(x_array+y_array+z_matrix+x_value+y_value+interpolation2DType+allowExtrapolation);
        interpolate2DDesc.Register();


        // Registers Normal distribution
        XlfArgDesc x_dist_value("x",
            "is the value for which you want the distribution");
        XlfArgDesc mean("mean", "is the arithmetic mean of the distribution");
        XlfArgDesc std_dev("std_dev", "is the standard deviation of "
            "the distribution");
        XlfArgDesc cumulative("cumulative", "is a logical value that determines "
            "the form of the function. If cumulative is TRUE, NORMDIST returns "
            "the cumulative distribution function; if FALSE, it returns "
            "the probability mass function");
        XlfArgDesc probability("probability", "is a probability corresponding"
            " to the normal distribution");
        
        XlfFuncDesc normDistDesc("xlnormDist","qlnormDist",
            "Return the normal cumulative distribution for the specified "
            "mean and standard deviation","qlMath");
        normDistDesc.SetArguments(x_dist_value+mean+std_dev+cumulative);
        normDistDesc.Register();

        XlfFuncDesc normSDistDesc("xlnormSDist","qlnormSDist",
            "Return the standard normal cumulative distribution (has a mean of zero "
            "and a standard deviation of one","qlMath");
        normSDistDesc.SetArguments(x_dist_value);
        normSDistDesc.Register();

        XlfFuncDesc normInvDesc("xlnormInv","qlnormInv",
            "Return the inverse of the normal cumulative distribution for the specified "
            "mean and standard deviation","qlMath");
        normInvDesc.SetArguments(probability+mean+std_dev);
        normInvDesc.Register();

        XlfFuncDesc normSInvDesc("xlnormSInv","qlnormSInv",
            "Return the inverse of the standard normal cumulative distribution "
            "(has a mean of zero "
            "and a standard deviation of one","qlMath");
        normSInvDesc.SetArguments(probability);
        normSInvDesc.Register();


        // Registers Risk Measures
        XlfArgDesc percentile("percentile",
            "is the confidence level");
        XlfArgDesc targetReturn("target return",
            "is the target return at which you want to perform the analysis");

        XlfFuncDesc potentialUpsideDesc("xlpotentialUpside","qlpotentialUpside",
            "Return the potential upside with a percentile confidence "
            "for a normal distribution specified "
            "by mean and standard deviation","qlMath");
        potentialUpsideDesc.SetArguments(percentile+mean+std_dev);
        potentialUpsideDesc.Register();

        XlfFuncDesc valueAtRiskDesc("xlvalueAtRisk","qlvalueAtRisk",
            "Return the value at risk with a percentile confidence "
            "for a normal distribution specified "
            "by mean and standard deviation","qlMath");
        valueAtRiskDesc.SetArguments(percentile+mean+std_dev);
        valueAtRiskDesc.Register();
        
        XlfFuncDesc expectedShortfallDesc("xlexpectedShortfall","qlexpectedShortfall",
            "Return the expected shortfall with a percentile confidence "
            "for a normal distribution specified "
            "by mean and standard deviation","qlMath");
        expectedShortfallDesc.SetArguments(percentile+mean+std_dev);
        expectedShortfallDesc.Register();
        
        XlfFuncDesc shortfallDesc("xlshortfall","qlshortfall",
            "Return the shortfall at the chosen target "
            "for a normal distribution specified "
            "by mean and standard deviation","qlMath");
        shortfallDesc.SetArguments(targetReturn+mean+std_dev);
        shortfallDesc.Register();
        
        XlfFuncDesc averageShortfallDesc("xlaverageShortfall","qlaverageShortfall",
            "Return the average shortfall at the chosen target "
            "for a normal distribution specified "
            "by mean and standard deviation","qlMath");
        averageShortfallDesc.SetArguments(targetReturn+mean+std_dev);
        averageShortfallDesc.Register();
        
        
        
        
        // Clears the status bar.
        XlfExcel::Instance().SendMessage();
        return 1;

//        EXCEL_END;
    }

}
