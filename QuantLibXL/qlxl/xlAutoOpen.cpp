
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
#include <qlxl/qlxlfoper.hpp>

extern "C" {
    using namespace QuantLib;

    long EXCEL_EXPORT xlAutoOpen() {
        XlfExcel::Instance().FreeMemory();
        try {

            // Displays a message in the status bar.
            XlfExcel::Instance().SendMessage("Registering QuantLibXL v"
                QLXL_VERSION " library...");


            // description of input variables
            XlfArgDesc percentile("percentile", "is the distribution percentile");
            XlfArgDesc mean("mean", "is the arithmetic mean of the distribution");
            XlfArgDesc std_dev("standard deviation", "is the standard deviation of "
                "the distribution");
            XlfArgDesc targetReturn("target return",
                "is the target return at which you want to perform the analysis");
            XlfArgDesc x_dist_value("x",
                "is the value for which you want the distribution");
            XlfArgDesc cumulative("cumulative", "is a logical value that determines "
                "the form of the function. If cumulative is TRUE, NORMDIST returns "
                "the cumulative distribution function; if FALSE, it returns "
                "the probability mass function");
            XlfArgDesc probability("probability", "is a probability corresponding"
                " to the normal distribution");


            XlfArgDesc d01("date1", "first date");
            XlfArgDesc d02("date2", "second date");
            XlfArgDesc rpd01("date3", "reference period first date");
            XlfArgDesc rpd02("date4", "reference period last date");
            XlfArgDesc refDate("refDate", "reference date");
            XlfArgDesc evalDate("evalDate", "evaluation date");
            XlfArgDesc dates("dates", "dates");


            XlfArgDesc dayCount("dayCount", "day count convention");

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

            XlfArgDesc optionType("type", "is the option type");
            XlfArgDesc underlying("underlying", "is the value of the underlying");
            XlfArgDesc moneyness("moneyness", "is the moneyness measured as percentage of the ATM");
            XlfArgDesc strike("strike", "is the strike");
            XlfArgDesc strikes("strikes", "strikes");
            XlfArgDesc dividendYield("dividend yield", "is the dividend yield");
            XlfArgDesc riskFreeRate("risk-free rate", "is the risk free rate");
            XlfArgDesc foreignRiskFreeRate("foreign risk-free rate", "is the risk free rate in the foreign currency");
            XlfArgDesc maturity("maturity", "is the option's maturity measured in years");
            XlfArgDesc resetTime("reset time", "is the strike reset time in years");
            XlfArgDesc blackVolSurface("blackVolSurface", "Black (market) volatility surface");
            XlfArgDesc volatility("volatility", "is the underlying's volatility");
            XlfArgDesc exchangeVolatility("exchangeVolatility", "is the volatility of the FX rate");
            XlfArgDesc correlation("correlation", "is the correlation");
            XlfArgDesc timeSteps("time steps", "is the number of time steps");
            XlfArgDesc gridPoints("grid points", "is the number of grid points");
            XlfArgDesc samples("samples", "is the number of simulated samples");
            XlfArgDesc anthiteticVariance("anthitetic variance", "is the anthitetic variance boolean");

            XlfArgDesc drift("drift rate", "is the drift rate of the asset");
            XlfArgDesc times("times", "is the vector of times measured in years");
            XlfArgDesc paths("paths", "is the number of simulated paths");



            // Registers Risk Measures
            XlfFuncDesc valueAtRiskDesc("xlvalueAtRisk","qlValueAtRisk",
                "Return the value at risk with a percentile confidence "
                "for a normal distribution specified "
                "by mean and standard deviation","QuantLibXL Math");
            valueAtRiskDesc.SetArguments(percentile+mean+std_dev);
            valueAtRiskDesc.Register();

            XlfFuncDesc potentialUpsideDesc("xlpotentialUpside","qlPotentialUpside",
                "Return the potential upside with a percentile confidence "
                "for a normal distribution specified "
                "by mean and standard deviation","QuantLibXL Math");
            potentialUpsideDesc.SetArguments(percentile+mean+std_dev);
            potentialUpsideDesc.Register();

            XlfFuncDesc expectedShortfallDesc("xlexpectedShortfall","qlExpectedShortfall",
                "Return the expected shortfall with a percentile confidence "
                "for a normal distribution specified "
                "by mean and standard deviation","QuantLibXL Math");
            expectedShortfallDesc.SetArguments(percentile+mean+std_dev);
            expectedShortfallDesc.Register();


            XlfFuncDesc shortfallDesc("xlshortfall","qlShortfall",
                "Return the shortfall at the chosen target "
                "for a normal distribution specified "
                "by mean and standard deviation","QuantLibXL Math");
            shortfallDesc.SetArguments(targetReturn+mean+std_dev);
            shortfallDesc.Register();

            XlfFuncDesc averageShortfallDesc("xlaverageShortfall","qlAverageShortfall",
                "Return the average shortfall at the chosen target "
                "for a normal distribution specified "
                "by mean and standard deviation","QuantLibXL Math");
            averageShortfallDesc.SetArguments(targetReturn+mean+std_dev);
            averageShortfallDesc.Register();


            // Registers accrual_days
            XlfFuncDesc accrualDaysDesc("xlaccrualDays","qlAccrual_days",
                "Accrual days","QuantLibXL Date Functions");
            accrualDaysDesc.SetArguments(d01+d02+dayCount);
            accrualDaysDesc.Register();

            XlfFuncDesc accrualFactorDesc("xlaccrualFactor","qlAccrual_factor",
                "Accrual factor","QuantLibXL Date Functions");
            accrualFactorDesc.SetArguments(d01+d02+dayCount+rpd01+rpd02);
            accrualFactorDesc.Register();



            // Registers Black-Scholes
            XlfFuncDesc europeanOption("xlEuropeanOption","qlEuropeanOption",
                "Black Scholes formula for european option","QuantLibXL Finance");
            europeanOption.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+maturity+volatility);
            europeanOption.Register();

            XlfFuncDesc europeanOption_fd("xlEuropeanOption_FD","qlEuropeanOption_FD",
                "european option computed with finite differences","QuantLibXL Finance");
            europeanOption_fd.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+maturity+volatility+timeSteps+gridPoints);
            europeanOption_fd.Register();

            XlfFuncDesc europeanOption_mc("xlEuropeanOption_MC","qlEuropeanOption_MC",
                "european option computed with Monte Carlo simulation","QuantLibXL Finance");
            europeanOption_mc.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+maturity+volatility+anthiteticVariance+samples);
            europeanOption_mc.Register();

            XlfFuncDesc cliquetOption("xlCliquetOption","qlCliquetOption",
                "european cliquet option","QuantLibXL Finance");
            cliquetOption.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+maturity+volatility);
            cliquetOption.Register();

            XlfFuncDesc cliquetOption_mc("xlCliquetOption_MC","qlCliquetOption_MC",
                "european cliquet option computed with Monte Carlo simulation","QuantLibXL Finance");
            cliquetOption_mc.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+maturity+volatility+anthiteticVariance+samples);
            cliquetOption_mc.Register();

            XlfFuncDesc performanceOption("xlPerformanceOption","qlPerformanceOption",
                "european performance option","QuantLibXL Finance");
            performanceOption.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+maturity+volatility);
            performanceOption.Register();

            XlfFuncDesc performanceOption_mc("xlPerformanceOption_MC","qlPerformanceOption_MC",
                "european performance option computed with Monte Carlo simulation","QuantLibXL Finance");
            performanceOption_mc.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+maturity+volatility+anthiteticVariance+samples);
            performanceOption_mc.Register();

            XlfFuncDesc americanOption_fd("xlAmericanOption_FD","qlAmericanOption_FD",
                "american option computed with finite differences","QuantLibXL Finance");
            americanOption_fd.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+maturity+volatility+timeSteps+gridPoints);
            americanOption_fd.Register();

            XlfFuncDesc quantoEuropeanOption("xlQuantoEuropeanOption","qlQuantoEuropeanOption",
                "Quanto european option","QuantLibXL Finance");
            quantoEuropeanOption.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+maturity+volatility+foreignRiskFreeRate+exchangeVolatility+correlation);
            quantoEuropeanOption.Register();


            // Registers PathGenerator
            XlfFuncDesc pathGenerator("xlPathGenerator","qlPathGenerator",
                "Geometric Brownian motion path","QuantLibXL Finance");
            pathGenerator.SetArguments(underlying+drift+volatility+times+paths);
            pathGenerator.Register();


            // Registers interpolation
            XlfFuncDesc interpolateDesc("xlinterpolate","qlInterpolate",
                "1 dimensional interpolation","QuantLibXL Math");
            interpolateDesc.SetArguments(x_array+y_array+x_value+interpolationType+allowExtrapolation);
            interpolateDesc.Register();

            XlfFuncDesc interpolate2DDesc("xlinterpolate2D","qlInterpolate2D",
                "2 dimensional interpolation","QuantLibXL Math");
            interpolate2DDesc.SetArguments(x_array+y_array+z_matrix+x_value+y_value+interpolation2DType+allowExtrapolation);
            interpolate2DDesc.Register();


            // Registers Normal distribution


            XlfFuncDesc normInvDesc("xlnormInv","qlNormInv",
                "Return the inverse of the normal cumulative distribution for the specified "
                "mean and standard deviation","QuantLibXL Math");
            normInvDesc.SetArguments(probability+mean+std_dev);
            normInvDesc.Register();

            XlfFuncDesc normSInvDesc("xlnormSInv","qlNormSInv",
                "Return the inverse of the standard normal cumulative distribution "
                "(has a mean of zero "
                "and a standard deviation of one","QuantLibXL Math");
            normSInvDesc.SetArguments(probability);
            normSInvDesc.Register();


            XlfFuncDesc normDistDesc("xlnormDist","qlNormDist",
                "Return the normal cumulative distribution for the specified "
                "mean and standard deviation","QuantLibXL Math");
            normDistDesc.SetArguments(x_dist_value+mean+std_dev+cumulative);
            normDistDesc.Register();

            XlfFuncDesc normSDistDesc("xlnormSDist","qlNormSDist",
                "Return the standard normal cumulative distribution (has a mean of zero "
                "and a standard deviation of one","QuantLibXL Math");
            normSDistDesc.SetArguments(x_dist_value);
            normSDistDesc.Register();


            // vol functions
            XlfFuncDesc blackVol("xlBlackVol","qlBlackVol",
                "Return the interpolated Black forward volatility for "
                "a fixed strike and maturity "
                "given a Black volatility surface as input","QuantLibXL Finance");
            blackVol.SetArguments(refDate+dayCount+dates+strikes+blackVolSurface+d01+d02+strike+interpolation2DType+allowExtrapolation);
            blackVol.Register();


            // Registers qlversion
            XlfFuncDesc QLversion("xlQLversion","qlQLVersion",
                "QuantLib version string","QuantLibXL Utilities");
            QLversion.Register();

            // Registers qlhexversion
            XlfFuncDesc QLhexversion("xlQLhexversion","qlQLHexVersion",
                "QuantLib version number","QuantLibXL Utilities");
            QLhexversion.Register();

            // Registers xlwversion
            XlfFuncDesc XLWversion("xlXLWversion","qlXLWVersion",
                "QuantLib version string","QuantLibXL Utilities");
            XLWversion.Register();

            // Registers xlwhexversion
            XlfFuncDesc XLWhexversion("xlXLWhexversion","qlXLWHexVersion",
                "QuantLib version number","QuantLibXL Utilities");
            XLWhexversion.Register();

            // Registers qlxlversion
            XlfFuncDesc QLXLversion("xlQLXLversion","qlQLXLVersion",
                "QuantLib version string","QuantLibXL Utilities");
            QLXLversion.Register();

            // Registers qlxlhexversion
            XlfFuncDesc QLXLhexversion("xlQLXLhexversion","qlQLXLHexVersion",
                "QuantLib version number","QuantLibXL Utilities");
            QLXLhexversion.Register();



            // Clears the status bar.
            XlfExcel::Instance().SendMessage();
            return 1;
        } catch (...) {
            XlfExcel::Instance().SendMessage("QuantLibXL v"
                QLXL_VERSION ": registration failed");
	        return 0;
        }
    }
}
