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
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/MonteCarlo/montecarlomodel.hpp>
#include <ql/MonteCarlo/getcovariance.hpp>
#include <ql/RandomNumbers/haltonrsg.hpp>
#include <ql/RandomNumbers/faurersg.hpp>
#include <ql/RandomNumbers/randomizedlds.hpp>
#include <boost/shared_ptr.hpp>

extern "C"
{

    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlRandomNumberGenerator(XlfOper xldimension,
                                                  XlfOper xlsamples,
                                                  XlfOper xlgeneratorType,
                                                  XlfOper xlseed)
    {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;


        Size dimension = xldimension.AsInt();
        Size samples = xlsamples.AsInt();

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();

        // PseudoRandom::ursg_type rsg(dimension, mcSeed);
        GenericPseudoRandom<MersenneTwisterUniformRng,
            InverseCumulativeNormal>::ursg_type rsg(dimension, mcSeed);
        // LowDiscrepancy::ursg_type ldsg(dimension, mcSeed);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::ursg_type  so1ldsg(dimension, mcSeed,
            SobolRsg::Jaeckel);
        GenericLowDiscrepancy<HaltonRsg,
            InverseCumulativeNormal>::ursg_type halldsg(dimension, mcSeed);
        GenericLowDiscrepancy<FaureRsg,
            InverseCumulativeNormal>::ursg_type  fauldsg(dimension);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::ursg_type  so2ldsg(dimension, mcSeed,
            SobolRsg::Unit);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::ursg_type  so3ldsg(dimension, mcSeed,
            SobolRsg::SobolLevitan);

        RamdomizedLDS<SobolRsg,
            RandomSequenceGenerator<MersenneTwisterUniformRng> >
                rldsg(so1ldsg);


        Matrix result(samples, dimension);
        Array sample;
        for (Size j=0; j<samples; j++) {
            switch (generatorType) {
            case 1:
                sample = rsg.nextSequence().value;
                break;
            case 2:
                sample = so1ldsg.nextSequence().value;
                break;
            case 3:
                sample = halldsg.nextSequence().value;
                break;
            case 4:
                sample = fauldsg.nextSequence().value;
                break;
            case 5:
                sample = so2ldsg.nextSequence().value;
                break;
            case 6:
                sample = so3ldsg.nextSequence().value;
                break;
            case 7:
                sample = rldsg.nextSequence().value;
                break;
            default:
                QL_FAIL("Unknown generator");
            }

            for (Size i=0; i<dimension; i++) {
                result[j][i] = sample[i];
            }
        }

        return XlfOper(samples, dimension, result.begin());

        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlGaussianRandomNumberGenerator(
        XlfOper xldimension,
        XlfOper xlsamples,
        XlfOper xlgeneratorType,
        XlfOper xlseed) {

        EXCEL_BEGIN;

        WIZARD_NO_CALC;


        Size dimension = xldimension.AsInt();
        Size samples = xlsamples.AsInt();

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();

        // PseudoRandom::ursg_type rsg(dimension, mcSeed);
        // PseudoRandom::rsg_type grsg(rsg);
        GenericPseudoRandom<MersenneTwisterUniformRng,
            InverseCumulativeNormal>::ursg_type rsg(dimension, mcSeed);
        GenericPseudoRandom<MersenneTwisterUniformRng,
            InverseCumulativeNormal>::rsg_type grsg(rsg);
        // LowDiscrepancy::ursg_type ldsg(dimension, mcSeed);
        // LowDiscrepancy::rsg_type gldsg(ldsg);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::ursg_type so1ldsg(dimension, mcSeed,
            SobolRsg::Jaeckel);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::rsg_type gso1ldsg(so1ldsg);

        GenericLowDiscrepancy<HaltonRsg,
            InverseCumulativeNormal>::ursg_type halldsg(dimension, mcSeed);
        GenericLowDiscrepancy<HaltonRsg,
            InverseCumulativeNormal>::rsg_type ghalldsg(halldsg);

        GenericLowDiscrepancy<FaureRsg,
            InverseCumulativeNormal>::ursg_type  fauldsg(dimension);
        GenericLowDiscrepancy<FaureRsg,
            InverseCumulativeNormal>::rsg_type gfauldsg(fauldsg);

        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::ursg_type so2ldsg(dimension, mcSeed,
            SobolRsg::Unit);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::rsg_type gso2ldsg(so2ldsg);

        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::ursg_type so3ldsg(dimension, mcSeed,
            SobolRsg::SobolLevitan);
        GenericLowDiscrepancy<SobolRsg,
            InverseCumulativeNormal>::rsg_type gso3ldsg(so3ldsg);


        Matrix result(samples, dimension);
        Array sample;
        for (Size j=0; j<samples; j++) {
            switch (generatorType) {
            case 1:
                sample = grsg.nextSequence().value;
                break;
            case 2:
                sample = gso1ldsg.nextSequence().value;
                break;
            case 3:
                sample = ghalldsg.nextSequence().value;
                break;
            case 4:
                sample = gfauldsg.nextSequence().value;
                break;
            case 5:
                sample = gso2ldsg.nextSequence().value;
                break;
            case 6:
                sample = gso3ldsg.nextSequence().value;
                break;
            default:
                QL_FAIL("Unknown generator");
            }

            for (Size i=0; i<dimension; i++) {
                result[j][i] = sample[i];
            }
        }

        return XlfOper(samples, dimension, result.begin());

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

        WIZARD_NO_CALC;

        double underlying = xlunderlying.AsDouble();
        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Size samples = xlsamples.AsInt();

        std::vector<Time> times = xltimes.AsDoubleVector();
        TimeGrid timeGrid(times.begin(), times.end());
        Size timeSteps = times.size();


        Handle<YieldTermStructure> riskFreeTS =
            QlXlfOper(xlriskFree).AsTermStructure(refDate);
        Handle<YieldTermStructure> dividendTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        Handle<BlackVolTermStructure> blackVolTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                              xlinterpolationType.AsInt());

        boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(underlying));
        boost::shared_ptr<BlackScholesProcess> bs(new
            BlackScholesProcess(Handle<Quote>(spot),
                riskFreeTS, dividendTS, blackVolTS));

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();

        PseudoRandom::ursg_type rsg(timeSteps, mcSeed);
        LowDiscrepancy::ursg_type ldsg(timeSteps);

        PseudoRandom::rsg_type grsg(rsg);
        LowDiscrepancy::rsg_type gldsg(ldsg);


        PathGenerator<PseudoRandom::rsg_type>
            PseudoRandomPathGenerator(bs, timeGrid, grsg, false);

        // BB here
        PathGenerator<LowDiscrepancy::rsg_type>
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
                QL_FAIL("Unknown generator");
            }

            result[0][j] = underlying * QL_EXP(myPath[0]);
            for (Size i = 1; i < timeSteps; i++) {
                result[i][j] = result[i-1][j] * QL_EXP(myPath[i]);
            }
        }

        return XlfOper(timeSteps, samples, result.begin());

        EXCEL_END;
    }


    LPXLOPER EXCEL_EXPORT xlBrownianBridge(XlfOper xlunderlying,
                                          XlfOper xldividendYield,
                                          XlfOper xlriskFree,
                                          XlfOper xlrefDate,
                                          XlfOper xltimes,
                                          XlfOper xlvolatility,
                                          XlfOper xlinterpolationType,
                                          XlfOper xlsamples,
                                          XlfOper xlgeneratorType,
                                          XlfOper xlseed) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        double underlying = xlunderlying.AsDouble();
        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Size samples = xlsamples.AsInt();

        std::vector<Time> times = xltimes.AsDoubleVector();
        TimeGrid timeGrid(times.begin(), times.end());
        Size timeSteps = times.size();


        Handle<YieldTermStructure> riskFreeTS =
            QlXlfOper(xlriskFree).AsTermStructure(refDate);
        Handle<YieldTermStructure> dividendTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        Handle<BlackVolTermStructure> blackVolTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                              xlinterpolationType.AsInt());

        boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(underlying));
        boost::shared_ptr<StochasticProcess> bs(new
            BlackScholesProcess(Handle<Quote>(spot),
                riskFreeTS, dividendTS, blackVolTS));

        int generatorType = xlgeneratorType.AsInt();
        unsigned long mcSeed = xlseed.AsInt();
        Matrix result(timeSteps, samples);
        std::vector<double> myPath;
        Size j, i;
        if (generatorType==1) {
			PseudoRandom::ursg_type rsg(timeSteps, mcSeed);
			PseudoRandom::rsg_type grsg(rsg);

            BrownianBridge<PseudoRandom::rsg_type>
                PseudoRandomBrownianBridge(bs, timeGrid, grsg);
            for (j=0; j<samples; j++) {
                myPath = PseudoRandomBrownianBridge.next().value;
                for (i=0; i<timeSteps; i++) {
                    result[i][j] = myPath[i];
                }
            }
            return XlfOper(timeSteps, samples, result.begin());
        } else {
			LowDiscrepancy::ursg_type ldsg(timeSteps);
			LowDiscrepancy::rsg_type gldsg(ldsg);

            BrownianBridge<LowDiscrepancy::rsg_type>
                QuasiRandomBrownianBridge(bs, timeGrid, gldsg);
            for (j=0; j<samples; j++) {
                myPath = QuasiRandomBrownianBridge.next().value;
                for (i=0; i<timeSteps; i++) {
                    result[i][j] = myPath[i];
                }
            }
            return XlfOper(timeSteps, samples, result.begin());
        }


        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlCovFromCorr(XlfOper xlmatrix,
                                        XlfOper xlvolatilities) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        std::vector<double> vols = xlvolatilities.AsDoubleVector();
        Matrix result = getCovariance(vols.begin(), vols.end(), data_matrix);
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlCorrFromCov(XlfOper xlmatrix) {

        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        Matrix result = CovarianceDecomposition(data_matrix).correlationMatrix();
        return XlfOper(result.rows(), result.columns(), result.begin());
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlVolsFromCov(XlfOper xlmatrix) {

        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Matrix data_matrix = QlXlfOper(xlmatrix).AsMatrix();
        Array result = CovarianceDecomposition(data_matrix).standardDeviations();
        return XlfOper(result.size(), 1, result.begin());
        EXCEL_END;
    }
}
