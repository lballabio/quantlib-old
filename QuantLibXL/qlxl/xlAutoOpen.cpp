
/*
 Copyright (C) 2002, 2003, 2004 Ferdinando Ametrano

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

/*! \file qlxl.cpp
    \brief QuantLib Excel add-in

*/

#include <qlxl/qlxl.hpp>
#include <qlxl/qlxlfoper.hpp>

#define BOOST_LIB_DIAGNOSTIC
#include <ql/Functions/qlfunctions.hpp>
#undef BOOST_LIB_DIAGNOSTIC


extern "C" {
    using namespace QuantLib;

    long EXCEL_EXPORT xlAutoOpen() {
        XlfExcel::Instance().FreeMemory();
        try {

            // Displays a message in the status bar.
            XlfExcel::Instance().SendMessage("Registering QuantLibXL v"
                QLXL_VERSION " library...");


            // description of input variables
            XlfArgDesc percentile("percentile",
                "is the distribution percentile");
            XlfArgDesc mean("mean",
                "is the arithmetic mean of the distribution");
            XlfArgDesc std_dev("standard deviation", "is the standard "
                "deviation of the distribution");
            XlfArgDesc targetReturn("target return", "is the target return "
                "at which you want to perform the analysis");
            XlfArgDesc x_dist_value("x",
                "is the value for which you want the distribution");
            XlfArgDesc cumulative("cumulative",
                "is a logical value that determines the form of the function: "
                "if TRUE the cumulative "
                "distribution function is returned, else the probability "
                "mass function is returned.");
            XlfArgDesc probability("probability", "is a probability "
                "corresponding to the normal distribution");
            XlfArgDesc number_s("number_s",
                "is the number of successes in trials");
            XlfArgDesc trials("trials",
                "is the number of independent trials");
            XlfArgDesc trials_odd("(odd) trials",
                "is the number of independent trials. It must be an odd number.");
            XlfArgDesc probability_s("probability_s",
                "is the probability of success on each trial.");
            XlfArgDesc number("number",
                "is the number of items");
            XlfArgDesc number_chosen("number_chosen",
                "is the number of items in each combination.");


            XlfArgDesc d01("date1", "first date");
            XlfArgDesc d02("date2", "second date");
            XlfArgDesc rpd01("date3", "reference period first date");
            XlfArgDesc rpd02("date4", "reference period last date");
            XlfArgDesc refDate("referenceDate",
                "reference (settlement) date (t=0)");
            XlfArgDesc evalDate("evalDate", "evaluation date");
            XlfArgDesc expiryDate("expiryDate", "expiry date");
            XlfArgDesc resetDate("resetDate", "strike-setting date");
            XlfArgDesc fixingDates("fixingDates", "list of fixing dates");
            XlfArgDesc dates("dates", "list of dates");


            XlfArgDesc dayCount("dayCount", "day count convention");

            XlfArgDesc calendar("calendar", "holiday calendar");
            XlfArgDesc includeWeekEnds("includeWeekEnds", "true: consider week-ends as holidays");

            XlfArgDesc data_array("data_array", "data array");
            XlfArgDesc x_array("x_array", "x data array");
            XlfArgDesc y_array("y_array", "y data array");
            XlfArgDesc z_matrix("z_matrix", "z data matrix");
            XlfArgDesc x_value("x_value", "x value to be interpolated");
            XlfArgDesc y_value("y_value", "y value to be interpolated");
            XlfArgDesc leftEndCondition("left end condition",
                "(spline-only) 0=not-a-knot; 1=first derivative value; 2=second derivative value");
            XlfArgDesc leftEndValue("left end value",
                "(spline-only) derivative value (ignored for not-a-knot end condition)");
            XlfArgDesc rightEndCondition("right end condition",
                "(spline-only) 0=not-a-knot; 1=first derivative value; 2=second derivative value");
            XlfArgDesc rightEndValue("right end value",
                "(spline-only) derivative value (ignored for not-a-knot end condition)");
            XlfArgDesc interpolationType("interpolationType",
                "1:linear; 2:spline; 3:log-linear");
            XlfArgDesc allowExtrapolation("extrapolation",
                "allow extrapolation boolean");
            XlfArgDesc monotonicityConstraint("monotonicity_",
                "enforce monotonicity constraint");
            XlfArgDesc derivativeOrder("derivativeOrder",
                ": -1 (primitive), 0 (function), 1 (1st derivative), "
                "or 2 (2nd derivative)");
            XlfArgDesc absoluteIndex("absoluteIndex", "zero based index");

            XlfArgDesc matrix("matrix", "input matrix");
            XlfArgDesc componentsRetainedPercentage("components percentage",
                "retained from the spectral "
                "(a.k.a Principal Component) analysis");
            XlfArgDesc maxRank("max rank",
                "required for the output matrix");
            XlfArgDesc salvagingAlgorithm("salvagingAlgorithm", "salvaging "
                "algorithm to be used "
                "when the input matrix in not positive semi definite");
            XlfArgDesc flexible("flexible", "if TRUE "
                "it doesn't fail with a positive semi definite input matrix");

            XlfArgDesc optionType("type", "is the option type");
            XlfArgDesc underlying("underlying",
                "is the current value of the underlying");
            XlfArgDesc assetLevel("assetLevel",
                "is the reference level of the asset");
            XlfArgDesc moneyness("moneyness",
                "is the moneyness measured as percentage of the ATM level");
            XlfArgDesc strike("strike", "is the strike");
            XlfArgDesc strikes("strikes", "strikes");
            XlfArgDesc dividendYield("dividend yield",
                "is the dividend yield");
            XlfArgDesc riskFreeRate("risk-free rate",
                "is the risk free rate");
            XlfArgDesc foreignRiskFreeRate("foreign risk-free rate",
                "is the risk free rate in the foreign currency");
            XlfArgDesc blackVolSurface("blackVolSurface",
                "Black (market) volatility surface");
            XlfArgDesc volatility("volatility",
                "is the underlying's volatility");
            XlfArgDesc volatilities("volatilities",
                "vector");

            XlfArgDesc accruedCoupon("accrued coupon",
                "is coupon accrued so far");
            XlfArgDesc lastFixing("last fixing",
                "is the underlying's last fixing");
            XlfArgDesc localCap("local cap", "is a local cap");
            XlfArgDesc localFloor("local floor", "is a local floor");
            XlfArgDesc globalCap("global cap", "is a global cap");
            XlfArgDesc globalFloor("global floor", "is a global floor");
            XlfArgDesc redemptionOnly("redemption only", "if true all the "
                "payoffs are paid at redemption instead of being paid "
                "immediatly");

            XlfArgDesc exchangeVolatility("exchangeVolatility",
                "is the volatility of the FX rate");
            XlfArgDesc correlation("correlation", "is the correlation");
            XlfArgDesc timeSteps("time steps", "is the number of time steps");
            XlfArgDesc gridPoints("grid points",
                "is the number of grid points");
            XlfArgDesc samples("samples",
                "is the number of samples");
            XlfArgDesc anthiteticVariance("anthitetic variance",
                "is the anthitetic variance boolean");

            XlfArgDesc drift("drift rate", "is the drift rate of the asset");
            XlfArgDesc times("times",
                "is the vector of times measured in years");
            XlfArgDesc paths("paths", "is the number of simulated paths");

            XlfArgDesc termStructure("termStructure",
                "is the yield term structure. It can be a single number, "
                "a date/discount grid, or a date/forward grid");

            XlfArgDesc seed("seed",
                "is the seed used to randomize the pseudo-random sequence");
            XlfArgDesc dimension("dimension",
                "is the dimensionality of the Monte Carlo simulation");
            XlfArgDesc generatorType("random number generator type",
                "1 Mersenne Twister, 2 Jäckel-Sobol, 3 Halton, 4 Faure, "
                "5 unit-Sobol, 6 SobolLevitan-Sobol");







            // Registers Statistics
            XlfFuncDesc meanDesc(
                "mean",
                "qlMean",
                "Return the mean of the observations",
                "QuantLibXL Statistics");
            meanDesc.SetArguments(data_array);
            meanDesc.Register();

            XlfFuncDesc varianceDesc(
                "variance",
                "qlVariance",
                "Return the variance of the observations",
                "QuantLibXL Statistics");
            varianceDesc.SetArguments(data_array);
            varianceDesc.Register();

            XlfFuncDesc standardDeviationDesc(
                "standardDeviation",
                "qlStandardDeviation",
                "Return the standard deviation of the observations",
                "QuantLibXL Statistics");
            standardDeviationDesc.SetArguments(data_array);
            standardDeviationDesc.Register();

            XlfFuncDesc skewnessDesc(
                "skewness",
                "qlSkewness",
                "Return the skewness of the observations",
                "QuantLibXL Statistics");
            skewnessDesc.SetArguments(data_array);
            skewnessDesc.Register();

            XlfFuncDesc kurtosisDesc(
                "kurtosis",
                "qlKurtosis",
                "Return the excess kurtosis of the observations",
                "QuantLibXL Statistics");
            kurtosisDesc.SetArguments(data_array);
            kurtosisDesc.Register();

            XlfFuncDesc minDesc(
                "min",
                "qlMin",
                "Return the minimun of the observations",
                "QuantLibXL Statistics");
            minDesc.SetArguments(data_array);
            minDesc.Register();

            XlfFuncDesc maxDesc(
                "max",
                "qlMax",
                "Return the maximum of the observations",
                "QuantLibXL Statistics");
            maxDesc.SetArguments(data_array);
            maxDesc.Register();

            XlfFuncDesc semiDeviationDesc(
                "semiDeviation",
                "qlSemiDeviation",
                "Return the standard deviation of the observations below the mean",
                "QuantLibXL Statistics");
            semiDeviationDesc.SetArguments(data_array);
            semiDeviationDesc.Register();

            XlfFuncDesc semiVarianceDesc(
                "semiVariance",
                "qlSemiVariance",
                "Return the variance of the observations below the mean",
                "QuantLibXL Statistics");
            semiVarianceDesc.SetArguments(data_array);
            semiVarianceDesc.Register();

            XlfFuncDesc downsideDeviationDesc(
                "downsideDeviation",
                "qlDownsideDeviation",
                "Return the standard deviation of the negative observations",
                "QuantLibXL Statistics");
            downsideDeviationDesc.SetArguments(data_array);
            downsideDeviationDesc.Register();

            XlfFuncDesc downsideVarianceDesc(
                "downsideVariance",
                "qlDownsideVariance",
                "Return the variance of the negative observations",
                "QuantLibXL Statistics");
            downsideVarianceDesc.SetArguments(data_array);
            downsideVarianceDesc.Register();

            XlfFuncDesc percentileDesc(
                "percentile",
                "qlPercentile",
                "Return the percentile of the distribution",
                "QuantLibXL Statistics");
            percentileDesc.SetArguments(percentile+data_array);
            percentileDesc.Register();

            XlfFuncDesc valueAtRiskDesc(
                "valueAtRisk",
                "qlValueAtRisk",
                "Return the value at risk with a percentile confidence",
                "QuantLibXL Statistics");
            valueAtRiskDesc.SetArguments(percentile+data_array);
            valueAtRiskDesc.Register();

            XlfFuncDesc topPercentileDesc(
                "topPercentile",
                "qlTopPercentile",
                "Return the top percentile of the distribution",
                "QuantLibXL Statistics");
            topPercentileDesc.SetArguments(percentile+data_array);
            topPercentileDesc.Register();

            XlfFuncDesc potentialUpsideDesc(
                "potentialUpside",
                "qlPotentialUpside",
                "Return the potential upside of the distribution with a percentile confidence",
                "QuantLibXL Statistics");
            potentialUpsideDesc.SetArguments(percentile+data_array);
            potentialUpsideDesc.Register();

            XlfFuncDesc expectedShortfallDesc(
                "expectedShortfall",
                "qlExpectedShortfall",
                "Return the expected shortfall with a percentile confidence ",
                "QuantLibXL Statistics");
            expectedShortfallDesc.SetArguments(percentile+data_array);
            expectedShortfallDesc.Register();

            XlfFuncDesc shortfallDesc(
                "shortfall",
                "qlShortfall",
                "Return the shortfall at the chosen target",
                "QuantLibXL Statistics");
            shortfallDesc.SetArguments(targetReturn+data_array);
            shortfallDesc.Register();

            XlfFuncDesc averageShortfallDesc(
                "averageShortfall",
                "qlAverageShortfall",
                "Return the average shortfall at the chosen target",
                "QuantLibXL Statistics");
            averageShortfallDesc.SetArguments(targetReturn+data_array);
            averageShortfallDesc.Register();

            XlfFuncDesc regretDesc(
                "regret",
                "qlRegret",
                "Return the regret at the chosen target",
                "QuantLibXL Statistics");
            regretDesc.SetArguments(targetReturn+data_array);
            regretDesc.Register();

            XlfFuncDesc gaussianDownsideDeviationDesc(
                "gaussianDownsideDeviation",
                "qlGaussianDownsideDeviation",
                "Return the standard deviation of the negative part "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianDownsideDeviationDesc.SetArguments(mean+std_dev);
            gaussianDownsideDeviationDesc.Register();

            XlfFuncDesc gaussianDownsideVarianceDesc(
                "gaussianDownsideVariance",
                "qlGaussianDownsideVariance",
                "Return the variance of the negative part "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianDownsideVarianceDesc.SetArguments(mean+std_dev);
            gaussianDownsideVarianceDesc.Register();

            XlfFuncDesc gaussianPercentileDesc(
                "gaussianPercentile",
                "qlGaussianPercentile",
                "Return the percentile "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianPercentileDesc.SetArguments(percentile+mean+std_dev);
            gaussianPercentileDesc.Register();

            XlfFuncDesc gaussianValueAtRiskDesc(
                "gaussianValueAtRisk",
                "qlGaussianValueAtRisk",
                "Return the value at risk with a percentile confidence"
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianValueAtRiskDesc.SetArguments(percentile+mean+std_dev);
            gaussianValueAtRiskDesc.Register();

            XlfFuncDesc gaussianTopPercentileDesc(
                "gaussianTopPercentile",
                "qlGaussianTopPercentile",
                "Return the top percentile "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianTopPercentileDesc.SetArguments(percentile+mean+std_dev);
            gaussianTopPercentileDesc.Register();

            XlfFuncDesc gaussianPotentialUpsideDesc(
                "gaussianPotentialUpside",
                "qlGaussianPotentialUpside",
                "Return the potential upside with a percentile confidence "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianPotentialUpsideDesc.SetArguments(percentile+mean+std_dev);
            gaussianPotentialUpsideDesc.Register();

            XlfFuncDesc gaussianExpectedShortfallDesc(
                "gaussianExpectedShortfall",
                "qlGaussianExpectedShortfall",
                "Return the expected shortfall with a percentile confidence "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianExpectedShortfallDesc.SetArguments(percentile+mean+std_dev);
            gaussianExpectedShortfallDesc.Register();

            XlfFuncDesc gaussianShortfallDesc(
                "gaussianShortfall",
                "qlGaussianShortfall",
                "Return the shortfall at the chosen target for "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianShortfallDesc.SetArguments(targetReturn+mean+std_dev);
            gaussianShortfallDesc.Register();

            XlfFuncDesc gaussianAverageShortfallDesc(
                "gaussianAverageShortfall",
                "qlGaussianAverageShortfall",
                "Return the average shortfall at the chosen target "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianAverageShortfallDesc.SetArguments(targetReturn+mean+std_dev);
            gaussianAverageShortfallDesc.Register();

            XlfFuncDesc gaussianRegretDesc(
                "gaussianRegret",
                "qlGaussianRegret",
                "Return the regret at the chosen target "
                "of a normal distribution N(mean, standard deviation)",
                "QuantLibXL Statistics");
            gaussianRegretDesc.SetArguments(targetReturn+mean+std_dev);
            gaussianRegretDesc.Register();





            // Registers calendars
            XlfFuncDesc holidayList("xlholidayList",
                "qlHolidayList",
                "holiday list for a given calendar",
                "QuantLibXL Date Functions");
            holidayList.SetArguments(calendar+d01+d02+includeWeekEnds);
            holidayList.Register();






            // Registers accrual_days
            XlfFuncDesc accrualDaysDesc("xlaccrualDays",
                "qlAccrual_days",
                "Accrual days",
                "QuantLibXL Date Functions");
            accrualDaysDesc.SetArguments(d01+d02+dayCount);
            accrualDaysDesc.Register();

            XlfFuncDesc accrualFactorDesc("xlaccrualFactor",
                "qlAccrual_factor",
                "Accrual factor",
                "QuantLibXL Date Functions");
            accrualFactorDesc.SetArguments(d01+d02+dayCount+rpd01+rpd02);
            accrualFactorDesc.Register();



            // Registers Black-Scholes
            XlfFuncDesc europeanOption("xlEuropeanOption",
                "qlEuropeanOption",
                "Black Scholes formula for european option",
                "QuantLibXL Finance");
            europeanOption.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+refDate+expiryDate+volatility+interpolationType);
            europeanOption.Register();

            XlfFuncDesc europeanOption_fd("xlEuropeanOption_FD",
                "qlEuropeanOption_FD",
                "european option computed with finite differences",
                "QuantLibXL Finance");
            europeanOption_fd.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+refDate+expiryDate+volatility+timeSteps+gridPoints);
            europeanOption_fd.Register();

            XlfFuncDesc europeanOption_mc("xlEuropeanOption_MC",
                "qlEuropeanOption_MC",
                "european option computed with Monte Carlo simulation",
                "QuantLibXL Finance");
            europeanOption_mc.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+refDate+expiryDate+volatility+anthiteticVariance+samples);
            europeanOption_mc.Register();

            XlfFuncDesc cliquetOption("xlCliquetOption",
                "qlCliquetOption",
                "european cliquet option","QuantLibXL Finance");
            cliquetOption.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+refDate+fixingDates+blackVolSurface+interpolationType);
            cliquetOption.Register();


            XlfFuncDesc cliquetOption_mc("xlCliquetOption_MC",
                "qlCliquetOption_MC",
                "european cliquet option computed with Monte Carlo simulation",
                "QuantLibXL Finance");
            cliquetOption_mc.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+refDate+fixingDates+blackVolSurface+interpolationType+accruedCoupon+lastFixing+localCap+localFloor+globalCap+globalFloor+redemptionOnly+anthiteticVariance+samples);
            cliquetOption_mc.Register();

            XlfFuncDesc performanceOption("xlPerformanceOption",
                "qlPerformanceOption",
                "european performance option",
                "QuantLibXL Finance");
            performanceOption.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+refDate+fixingDates+blackVolSurface+interpolationType);
            performanceOption.Register();

            XlfFuncDesc performanceOption_mc("xlPerformanceOption_MC",
                "qlPerformanceOption_MC",
                "european performance option computed with "
                "Monte Carlo simulation",
                "QuantLibXL Finance");
            performanceOption_mc.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+refDate+expiryDate+volatility+anthiteticVariance+samples);
            performanceOption_mc.Register();

            XlfFuncDesc americanOption_fd("xlAmericanOption_FD",
                "qlAmericanOption_FD",
                "american option computed with finite differences",
                "QuantLibXL Finance");
            americanOption_fd.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+refDate+expiryDate+volatility+timeSteps+gridPoints);
            americanOption_fd.Register();

            // new engine framework
            XlfFuncDesc quantoEuropeanOption("xlQuantoEuropeanOption",
                "qlQuantoEuropeanOption",
                "Quanto european option","QuantLibXL Finance");
            quantoEuropeanOption.SetArguments(optionType+underlying+strike+dividendYield+riskFreeRate+refDate+expiryDate+volatility+interpolationType+foreignRiskFreeRate+exchangeVolatility+correlation);
            quantoEuropeanOption.Register();

            XlfFuncDesc forwardEuropeanOption("xlForwardEuropeanOption",
                "qlForwardEuropeanOption",
                "Forward european option","QuantLibXL Finance");
            forwardEuropeanOption.SetArguments(optionType+underlying+moneyness+dividendYield+riskFreeRate+refDate+resetDate+expiryDate+volatility+interpolationType);
            forwardEuropeanOption.Register();

            XlfFuncDesc performanceEuropeanOption(
                "xlPerformanceEuropeanOption",
                "qlPerformanceEuropeanOption",
                "Performance european option","QuantLibXL Finance");
            performanceEuropeanOption.SetArguments(optionType+underlying+moneyness+dividendYield+refDate+riskFreeRate+resetDate+expiryDate+volatility+interpolationType);
            performanceEuropeanOption.Register();


            // Registers interpolation
            XlfFuncDesc interpolateDesc("xlinterpolate","qlInterpolate",
                "1 dimensional interpolation","QuantLibXL Math");
            interpolateDesc.SetArguments(x_array+y_array+x_value+interpolationType+allowExtrapolation+leftEndCondition+leftEndValue+rightEndCondition+rightEndValue+monotonicityConstraint+derivativeOrder);
            interpolateDesc.Register();

            XlfFuncDesc interpolate2DDesc("xlinterpolate2D","qlInterpolate2D",
                "2 dimensional interpolation","QuantLibXL Math");
            interpolate2DDesc.SetArguments(x_array+y_array+z_matrix+x_value+y_value+interpolationType+allowExtrapolation);
            interpolate2DDesc.Register();

            XlfFuncDesc primeNumbersDesc("xlprimeNumbers","qlprimeNumbers",
                "return the (absoluteIndex+1)-th prime number",
                "QuantLibXL Math");
            primeNumbersDesc.SetArguments(absoluteIndex);
            primeNumbersDesc.Register();

            XlfFuncDesc eigenVectorsDesc("xleigenVectors","qlEigenVectors",
                "return the eigenvectors of the input matrix, one for each "
                "column of the result matrix",
                "QuantLibXL Math");
            eigenVectorsDesc.SetArguments(matrix);
            eigenVectorsDesc.Register();

            XlfFuncDesc eigenValuesDesc("xleigenValues","qlEigenValues",
                "return the eigenvalues of the input matrix",
                "QuantLibXL Math");
            eigenValuesDesc.SetArguments(matrix);
            eigenValuesDesc.Register();

            XlfFuncDesc pseudoSQRTDesc("xlpseudoSQRT","qlpseudoSQRT",
                "return the pseudo square root of the input matrix, such "
                "that InputMatrix = ResultMatrix * transpose(ResultMatrix)",
                "QuantLibXL Math");
            pseudoSQRTDesc.SetArguments(matrix+salvagingAlgorithm);
            pseudoSQRTDesc.Register();

            XlfFuncDesc rankReducedSQRTDesc("xlrankReducedSQRT","qlrankReducedSQRT",
                "return the pseudo square root of the rank reduced input "
                "matrix, such that rank(InputMatrix)<=maxRank. If maxRank "
                "allows, then the required percentage of eigenvalues is "
                "retained.",
                "QuantLibXL Math");
            rankReducedSQRTDesc.SetArguments(matrix+maxRank+componentsRetainedPercentage+salvagingAlgorithm);
            rankReducedSQRTDesc.Register();

            XlfFuncDesc choleskyDesc("xlCholesky","qlCholesky",
                "return the Cholesky decomposition of the input matrix",
                "QuantLibXL Math");
            choleskyDesc.SetArguments(matrix+flexible);
            choleskyDesc.Register();

            XlfFuncDesc matrixProductDesc("xlmatrixProduct","qlmatrixProduct",
                "return the product of the input matrices","QuantLibXL Math");
            matrixProductDesc.SetArguments(matrix+matrix);
            matrixProductDesc.Register();

            XlfFuncDesc matrixTransposeDesc("xlmatrixTranspose",
                "qlmatrixTranspose",
                "return the transoped of the input matrix","QuantLibXL Math");
            matrixTransposeDesc.SetArguments(matrix);
            matrixTransposeDesc.Register();



            // Registers Random Number functions

            XlfFuncDesc rand(
                "xlrand",
                "qlRand",
                "Drop-in replacement for Excel's rand() function. "
                "It uses a Mersenne Twister random number generator, which can be "
                "re-initialize at any time using qlRandomize(seed).",
                "QuantLibXL Random Numbers",
                XlfFuncDesc::Volatile);
            rand.Register();

            XlfFuncDesc randomize("xlrandomize","qlRandomize",
                "Re-initialize the Mersenne Twister uniform random number "
                "generator invoked by qlRand().",
                "QuantLibXL Random Numbers");
            randomize.SetArguments(seed);
            randomize.Register();

            XlfFuncDesc randomNumberGenerator(
                "xlRandomNumberGenerator",
                "qlRandomNumberGenerator",
                "Generates uniform variates in the n-dimensional unit hypercube [0,1]^n",
                "QuantLibXL Random Numbers");
            randomNumberGenerator.SetArguments(dimension+samples+generatorType+seed);
            randomNumberGenerator.Register();

            XlfFuncDesc gaussianRandomNumberGenerator(
                "xlGaussianRandomNumberGenerator",
                "qlGaussianRandomNumberGenerator",
                "Generates gaussian variates in the n-dimensional space",
                "QuantLibXL Random Numbers");
            gaussianRandomNumberGenerator.SetArguments(dimension+samples+generatorType+seed);
            gaussianRandomNumberGenerator.Register();

            // Registers PathGenerator
            XlfFuncDesc pathGenerator("xlPathGenerator",
                "qlPathGenerator",
                "Geometric Brownian motion path",
                "QuantLibXL Monte Carlo");
            pathGenerator.SetArguments(underlying+dividendYield+riskFreeRate+refDate+times+blackVolSurface+interpolationType+paths+generatorType+seed);
            pathGenerator.Register();

            XlfFuncDesc brownianBridge("xlBrownianBridge",
                "qlBrownianBridge",
                "Brownian Bridge",
                "QuantLibXL Monte Carlo");
            brownianBridge.SetArguments(underlying+dividendYield+riskFreeRate+refDate+times+blackVolSurface+interpolationType+paths+generatorType+seed);
            brownianBridge.Register();

            // Registers CovFromCorr
            XlfFuncDesc CovFromCorr("xlCovFromCorr",
                "qlCovFromCorr",
                "Covariance matrix from correlations and vols",
                "QuantLibXL Finance");
            CovFromCorr.SetArguments(matrix+volatilities);
            CovFromCorr.Register();



            // Registers distributions

            XlfFuncDesc CombinDesc("xlCombin","qlCombin",
                "Returns the number of combinations for a given number of items.",
                "QuantLibXL Math");
            CombinDesc.SetArguments(number+number_chosen);
            CombinDesc.Register();

            XlfFuncDesc BinomDistDesc("xlBinomDist","qlBinomDist",
                "Returns the Binomial distribution.",
                "QuantLibXL Math");
            BinomDistDesc.SetArguments(number_s+trials+probability_s+cumulative);
            BinomDistDesc.Register();

            XlfFuncDesc PeizerPrattDesc("xlPeizerPratt","qlPeizerPratt",
                "Returns the probability p such that: "
                "1 - CumulativeBinomialDistribution((trials-1)/2, trials, p) "
                "= CumulativeNormalDistribution(x)",
                "QuantLibXL Math");
            PeizerPrattDesc.SetArguments(trials_odd+x_dist_value);
            PeizerPrattDesc.Register();

            XlfFuncDesc PoissonDesc("xlPoisson","qlPoisson",
                "Returns the Poisson distribution.",
                "QuantLibXL Math");
            PoissonDesc.SetArguments(x_dist_value+mean+cumulative);
            PoissonDesc.Register();

            XlfFuncDesc normInvDesc("xlnormInv","qlNormInv",
                "Return the inverse of the normal cumulative distribution "
                "for the specified mean and standard deviation",
                "QuantLibXL Math");
            normInvDesc.SetArguments(probability+mean+std_dev);
            normInvDesc.Register();

            XlfFuncDesc normSInvDesc("xlnormSInv","qlNormSInv",
                "Return the inverse of the standard normal cumulative "
                "distribution (has a mean of zero and a standard deviation "
                "of one",
                "QuantLibXL Math");
            normSInvDesc.SetArguments(probability);
            normSInvDesc.Register();


            XlfFuncDesc normDistDesc("xlnormDist","qlNormDist",
                "Return the normal cumulative distribution for the specified "
                "mean and standard deviation","QuantLibXL Math");
            normDistDesc.SetArguments(x_dist_value+mean+std_dev+cumulative);
            normDistDesc.Register();

            XlfFuncDesc normSDistDesc("xlnormSDist","qlNormSDist",
                "Return the standard normal cumulative distribution "
                "(has a mean of zero and a standard deviation of one)",
                "QuantLibXL Math");
            normSDistDesc.SetArguments(x_dist_value);
            normSDistDesc.Register();


            // vol functions
            XlfFuncDesc blackVol("xlBlackVol","qlBlackVol",
                "Return the interpolated Black forward volatility for "
                "a forward period at a fixed strike "
                "given a Black volatility surface as input",
                "QuantLibXL Finance");
            blackVol.SetArguments(refDate+d01+d02+strike+blackVolSurface+interpolationType+allowExtrapolation);
            blackVol.Register();

            XlfFuncDesc localVol("xlLocalVol","qlLocalVol",
                "Return the local volatility at a fixed date/time "
                "and level of the asset "
                "given the asset current value, the dividend curve, "
                "the yield curve, and "
                "the Black volatility surface as input","QuantLibXL Finance");
            localVol.SetArguments(refDate+underlying+evalDate+assetLevel+dividendYield+riskFreeRate+blackVolSurface+interpolationType+allowExtrapolation);
            localVol.Register();

            // yield functions
            XlfFuncDesc discount("xlDiscount","qlDiscount",
                "Return the discount calculated at the evaluation date "
                "on the yield term structure given as input",
                "QuantLibXL Finance");
            discount.SetArguments(refDate+termStructure+evalDate+allowExtrapolation);
            discount.Register();

            XlfFuncDesc zero("xlZero","qlZero",
                "Return the zero yield (continuos compounding act/365) "
                "calculated at the evaluation date "
                "on the yield term structure given as input",
                "QuantLibXL Finance");
            zero.SetArguments(refDate+termStructure+evalDate+allowExtrapolation);
            zero.Register();

            XlfFuncDesc forward("xlForward","qlForward",
                "Return the forward yield (continuos compounding act/365) "
                "calculated between 2 dates "
                "on the yield term structure given as input",
                "QuantLibXL Finance");
            forward.SetArguments(refDate+termStructure+d01+d02+allowExtrapolation);
            forward.Register();






            // Registers qlversion
            XlfFuncDesc QLversion(
                "xlQLversion",
                "qlQLVersion",
                "QuantLib version string",
                "QuantLibXL Utilities",
                XlfFuncDesc::Volatile);
            QLversion.Register();

            // Registers qlhexversion
            XlfFuncDesc QLhexversion(
                "xlQLhexversion",
                "qlQLHexVersion",
                "QuantLib version number",
                "QuantLibXL Utilities",
                XlfFuncDesc::Volatile);
            QLhexversion.Register();

            // Registers xlwversion
            XlfFuncDesc XLWversion(
                "xlXLWversion",
                "qlXLWVersion",
                "XLW version string",
                "QuantLibXL Utilities",
                XlfFuncDesc::Volatile);
            XLWversion.Register();

            // Registers xlwhexversion
            XlfFuncDesc XLWhexversion(
                "xlXLWhexversion",
                "qlXLWHexVersion",
                "XLW version number",
                "QuantLibXL Utilities",
                XlfFuncDesc::Volatile);
            XLWhexversion.Register();

            // Registers qlxlversion
            XlfFuncDesc QLXLversion(
                "xlQLXLversion",
                "qlQLXLVersion",
                "QuantLibXL version string",
                "QuantLibXL Utilities",
                XlfFuncDesc::Volatile);
            QLXLversion.Register();

            // Registers qlxlhexversion
            XlfFuncDesc QLXLhexversion(
                "xlQLXLhexversion",
                "qlQLXLHexVersion",
                "QuantLibXL version number",
                "QuantLibXL Utilities",
                XlfFuncDesc::Volatile);
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
