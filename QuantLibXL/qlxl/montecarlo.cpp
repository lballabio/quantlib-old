/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano

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
/*! \file montecarlo.cpp
    \brief QuantLib Excel Monte Carlo functions

    \fullpath
    qlxl/%montecarlo.cpp
*/

// $Id$

#include <qlxl/qlxlfoper.hpp>

extern "C"
{

    using namespace QuantLib;
    using namespace QuantLib::Math;
    using namespace QuantLib::MonteCarlo;
    using namespace QuantLib::RandomNumbers;

    LPXLOPER EXCEL_EXPORT xlPathGenerator(XlfOper xlunderlying,
                                          XlfOper xldrift,
                                          XlfOper xlvolatility,
                                          XlfOper xltimes,
                                          XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        double underlying = xlunderlying.AsDouble();
        Size samples = xlsamples.AsInt();

        std::vector<Time> times = xltimes.AsDoubleVector();
        TimeGrid timeGrid(times.begin(), times.end());
        Size timeSteps = times.size();

        std::vector<double> volatility = xlvolatility.AsDoubleVector();
        std::vector<double> inputDrift = xldrift.AsDoubleVector();
        QL_REQUIRE(volatility.size()<= timeSteps,
            "volatility and time arrays mismatched");
        QL_REQUIRE(inputDrift.size()<= timeSteps,
            "drift and time arrays mismatched");

        Size i;
        std::vector<double> variance(timeSteps);
        for (i=0; i< volatility.size(); i++) {
            variance[i] = volatility[i]*volatility[i];
        }
        for (i=volatility.size(); i<timeSteps ; i++) {
            variance[i] = variance[volatility.size()-1];
        }

        std::vector<double> drift(timeSteps);
        for (i=0; i< inputDrift.size(); i++) {
            drift[i] = inputDrift[i] - 0.5*variance[i];
        }
        for (i=inputDrift.size(); i<timeSteps ; i++) {
            drift[i] = inputDrift[inputDrift.size()-1] - 0.5*variance[i];
        }
        

        unsigned long mcSeed = 0;
//        UniformRandomSequenceGenerator rsg(timeSteps, mcSeed);
//        GaussianRandomSequenceGenerator grsg(rsg);

//        UniformLowDiscrepancySequenceGenerator ldsg(timeSteps);
//        GaussianLowDiscrepancySequenceGenerator gldsg(ldsg);

//        DiffusionProcess bs();
//        PathGenerator<GaussianRandomSequenceGenerator>
//            myPathGenerator(bs, times, grsg);

         PathGenerator_old<GaussianRandomGenerator> myPathGenerator(
             drift, variance, timeGrid, mcSeed);

        Size j;
        Path myPath(timeGrid);
        Matrix result(timeSteps, samples);
        for (j = 0; j < samples; j++) {
            myPath = myPathGenerator.next().value;

            result[0][j] = underlying * QL_EXP(myPath[0]);
            for (i = 1; i < timeSteps; i++) {
                result[i][j] = result[i-1][j] * QL_EXP(myPath[i]);
            }
        }

        return XlfOper(timeSteps, samples, result.begin());

        EXCEL_END;
    }

}
