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
/*! \file montecarlo.cpp
    \brief QuantLib Excel Monte Carlo functions

    \fullpath
    qlxl/%montecarlo.cpp
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/MonteCarlo/mctypedefs.hpp>
#include <ql/MonteCarlo/getcovariance.hpp>

extern "C"
{

    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlRandomNumberGenerator(XlfOper xldimension,
                                                  XlfOper xlsamples,
                                                  XlfOper xlgeneratorType,
                                                  XlfOper xlseed)
    {
        EXCEL_BEGIN;

        Size dimension = xldimension.AsInt();
        Size samples = xlsamples.AsInt();

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();
        UniformRandomSequenceGenerator rsg(dimension, mcSeed);
        UniformLowDiscrepancySequenceGenerator ldsg(dimension);

        Matrix result(dimension, samples);
        Array sample;
        for (Size j=0; j<samples; j++) {
            switch (generatorType) {
            case 1:
                sample = rsg.nextSequence().value;
                break;
            case 2:
                sample = ldsg.nextSequence().value;
                break;
            default:
                throw Error("Unknown generator");
            }

            for (Size i=0; i<dimension; i++) {
                result[i][j] = sample[i];
            }
        }

        return XlfOper(dimension, samples, result.begin());

        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlGaussianRandomNumberGenerator(
        XlfOper xldimension,
        XlfOper xlsamples,
        XlfOper xlgeneratorType,
        XlfOper xlseed) {

        EXCEL_BEGIN;

        Size dimension = xldimension.AsInt();
        Size samples = xlsamples.AsInt();

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();
        UniformRandomSequenceGenerator rsg(dimension, mcSeed);
        GaussianRandomSequenceGenerator grsg(rsg);
        UniformLowDiscrepancySequenceGenerator ldsg(dimension);
        GaussianLowDiscrepancySequenceGenerator gldsg(ldsg);

        Matrix result(dimension, samples);
        Array sample;
        for (Size j=0; j<samples; j++) {
            switch (generatorType) {
            case 1:
                sample = grsg.nextSequence().value;
                break;
            case 2:
                sample = gldsg.nextSequence().value;
                break;
            default:
                throw Error("Unknown generator");
            }

            for (Size i=0; i<dimension; i++) {
                result[i][j] = sample[i];
            }
        }

        return XlfOper(dimension, samples, result.begin());

        EXCEL_END;
    }
    
    
    LPXLOPER EXCEL_EXPORT xlPathGenerator(XlfOper xlunderlying,
                                          XlfOper xldividendYield,
                                          XlfOper xlriskFree,
                                          XlfOper xlrefDate,
                                          XlfOper xltimes,
                                          XlfOper xlvolatility,
                                          XlfOper xlinterpolationType,
                                          XlfOper xlsamples,
                                          XlfOper xlgeneratorType,
                                          XlfOper xlseed)
    {
        EXCEL_BEGIN;

        double underlying = xlunderlying.AsDouble();
        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Size samples = xlsamples.AsInt();

        std::vector<Time> times = xltimes.AsDoubleVector();
        TimeGrid timeGrid(times.begin(), times.end());
        Size timeSteps = times.size();


        RelinkableHandle<TermStructure> riskFreeTS =
            QlXlfOper(xlriskFree).AsTermStructure(refDate);
        RelinkableHandle<TermStructure> dividendTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        RelinkableHandle<BlackVolTermStructure> blackVolTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                              xlinterpolationType.AsInt());

        Handle<BlackScholesProcess> bs(new
            BlackScholesProcess(riskFreeTS, dividendTS, blackVolTS,
                                                        underlying));

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();
        UniformRandomSequenceGenerator rsg(timeSteps, mcSeed);
        GaussianRandomSequenceGenerator grsg(rsg);
        UniformLowDiscrepancySequenceGenerator ldsg(timeSteps);
        GaussianLowDiscrepancySequenceGenerator gldsg(ldsg);

        PathGenerator<GaussianRandomSequenceGenerator>
            PseudoRandomPathGenerator(bs, timeGrid, grsg, false);

        PathGenerator<GaussianLowDiscrepancySequenceGenerator>
            QuasiRandomPathGenerator(bs, timeGrid, gldsg, true);

        Size j;
        Path myPath(timeGrid);
        Matrix result(timeSteps, samples);
        for (j = 0; j < samples; j++) {
            switch (generatorType) {
            case 1:
                myPath = PseudoRandomPathGenerator.next().value;
                break;
            case 2:
                myPath = QuasiRandomPathGenerator.next().value;
                break;
            default:
                throw Error("Unknown generator");
            }

            result[0][j] = underlying * QL_EXP(myPath[0]);
            for (Size i = 1; i < timeSteps; i++) {
                result[i][j] = result[i-1][j] * QL_EXP(myPath[i]);
            }
        }

        return XlfOper(timeSteps, samples, result.begin());

        EXCEL_END;
    }

/*
    LPXLOPER EXCEL_EXPORT xlBrownianBridge(XlfOper xlunderlying,
                                          XlfOper xldividendYield,
                                          XlfOper xlriskFree,
                                          XlfOper xlrefDate,
                                          XlfOper xltimes,
                                          XlfOper xlvolatility,
                                          XlfOper xlinterpolationType,
                                          XlfOper xlsamples,
                                          XlfOper xlgeneratorType,
                                          XlfOper xlseed)
    {
        EXCEL_BEGIN;

        double underlying = xlunderlying.AsDouble();
        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Size samples = xlsamples.AsInt();

        std::vector<Time> times = xltimes.AsDoubleVector();
        TimeGrid timeGrid(times.begin(), times.end());
        Size timeSteps = times.size();


        RelinkableHandle<TermStructure> riskFreeTS =
            QlXlfOper(xlriskFree).AsTermStructure(refDate);
        RelinkableHandle<TermStructure> dividendTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        RelinkableHandle<BlackVolTermStructure> blackVolTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                              xlinterpolationType.AsInt());

        Handle<BlackScholesProcess> bs(new
            BlackScholesProcess(riskFreeTS, dividendTS, blackVolTS,
                                                        underlying));

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();
        switch (generatorType) {
            case 1:
                UniformRandomSequenceGenerator rsg(timeSteps, mcSeed);
                GaussianRandomSequenceGenerator grsg(rsg);
                BrownianBridge<GaussianRandomSequenceGenerator>
                    PseudoRandomBrownianBridge(bs, timeGrid, grsg);
                BrownianBridge<GaussianRandomSequenceGenerator>::sample_type myPath;
                Matrix result(timeSteps, samples);
                for (Size j = 0; j < samples; j++) {
                    myPath = PseudoRandomBrownianBridge.next().value;
                    result[0][j] = underlying * QL_EXP(myPath[0]);
                    for (Size i = 1; i < timeSteps; i++) {
                        result[i][j] = result[i-1][j] * QL_EXP(myPath[i]);
                    }
                }
                return XlfOper(timeSteps, samples, result.begin());
                break;
            case 2:
                UniformLowDiscrepancySequenceGenerator ldsg(timeSteps);
                GaussianLowDiscrepancySequenceGenerator gldsg(ldsg);
                BrownianBridge<GaussianLowDiscrepancySequenceGenerator>
                    QuasiRandomBrownianBridge(bs, timeGrid, gldsg);
                BrownianBridge<GaussianLowDiscrepancySequenceGenerator>::sample_type myPath;
                for (Size j = 0; j < samples; j++) {
                    myPath = QuasiRandomBrownianBridge.next().value;
                    result[0][j] = underlying * QL_EXP(myPath[0]);
                    for (Size i = 1; i < timeSteps; i++) {
                        result[i][j] = result[i-1][j] * QL_EXP(myPath[i]);
                    }
                return XlfOper(timeSteps, samples, result.begin());
                break;
            default:
                throw Error("Unknown generator");
            }

        EXCEL_END;
    }
*/
    LPXLOPER EXCEL_EXPORT xlCovFromCorr(XlfOper xlmatrix,
                                        XlfOper xlvolatilities) {
        EXCEL_BEGIN;
        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        std::vector<double> vols = xlvolatilities.AsDoubleVector();
        Matrix result = getCovariance(vols.begin(), vols.end(), data_matrix);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

}
