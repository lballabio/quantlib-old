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
/*! \file montecarlo.cpp
    \brief QuantLib Excel Monte Carlo functions

    \fullpath
    qlxl/%montecarlo.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>
#include <qlxl/montecarlo.hpp>

extern "C"
{

    using namespace QuantLib;
    using QuantLib::MonteCarlo::Path;
    using QuantLib::MonteCarlo::PathGenerator;
    using QuantLib::RandomNumbers::InvCumulativeKnuthGaussianRng;
    using QuantLib::Math::Matrix;

    LPXLOPER EXCEL_EXPORT xlPathGenerator(XlfOper xlunderlying,
                                          XlfOper xldrift,
                                          XlfOper xlvolatility,
                                          XlfOper xltimes,
                                          XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        double underlying = xlunderlying.AsDouble();
        double variance = xlvolatility.AsDouble()*xlvolatility.AsDouble();
        double drift = xldrift.AsDouble() - 0.5*variance;
        std::vector<double> times = xltimes.AsDoubleVector();
        Size n = times.size();
        Size samples = xlsamples.AsInt();


        PathGenerator<InvCumulativeKnuthGaussianRng> myPathGenerator =
            PathGenerator<InvCumulativeKnuthGaussianRng>(
            drift, variance, times);

        Size i, j;
        Path myPath(n);
        Matrix result(n, samples);
        for (j = 0; j < samples; j++) {
            myPath = myPathGenerator.next().value;

            result[0][j] = underlying * QL_EXP(myPath.drift()[0] +
                                               myPath.diffusion()[0]);
            for (i = 1; i < n; i++) {
                result[i][j] = result[i-1][j] * QL_EXP(myPath.drift()[i] +
                                              myPath.diffusion()[i]);
            }
        }

        return XlfOper(n, samples, result.begin());

        EXCEL_END;
    }

}
