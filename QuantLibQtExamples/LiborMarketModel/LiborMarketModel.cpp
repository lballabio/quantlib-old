/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C)

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <../customutilities.hpp>
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <iostream>
#include <iomanip>

using namespace std;
using namespace QuantLib;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

    Integer sessionId() { return 0; }

}
#endif

//----------------------------------------------------------------------------------------

namespace NamespaceLiborMarketModelTest {

    class LiborMarketModelTest {
      public:
        static void testSimpleCovarianceModels();
        static void testCapletPricing();
        static void testSwaptionPricing();
        static void testCalibration();
        static void mylmmtest();
        static void testIshiyamaLMM();
    };

    boost::shared_ptr<IborIndex> makeIndex(std::vector<Date> dates,
                                           std::vector<Rate> rates) {
        DayCounter dayCounter = Actual360();

        RelinkableHandle<YieldTermStructure> termStructure;

        boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));

        Date todaysDate =
            index->fixingCalendar().adjust(Date(4,September,2005));
        Settings::instance().evaluationDate() = todaysDate;

        dates[0] = index->fixingCalendar().advance(todaysDate,
                                                   index->fixingDays(), Days);

        termStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
                                    new ZeroCurve(dates, rates, dayCounter)));

        return index;
    }


    boost::shared_ptr<IborIndex> makeIndex() {
        std::vector<Date> dates;
        std::vector<Rate> rates;
        dates.push_back(Date(4,September,2005));
        dates.push_back(Date(4,September,2018));
        rates.push_back(0.039);
        rates.push_back(0.041);

        return makeIndex(dates, rates);
    }


    boost::shared_ptr<OptionletVolatilityStructure>
    makeCapVolCurve(const Date& todaysDate) {
        Volatility vols[] = {14.40, 17.15, 16.81, 16.64, 16.17,
                             15.78, 15.40, 15.21, 14.86};

        std::vector<Date> dates;
        std::vector<Volatility> capletVols;
        boost::shared_ptr<LiborForwardModelProcess> process(
                               new LiborForwardModelProcess(10, makeIndex()));

        for (Size i=0; i < 9; ++i) {
            capletVols.push_back(vols[i]/100);
            dates.push_back(process->fixingDates()[i+1]);
        }

        return boost::shared_ptr<CapletVarianceCurve>(
                         new CapletVarianceCurve(todaysDate, dates,
                                                 capletVols, Actual360()));
    }


    void LiborMarketModelTest::testSimpleCovarianceModels() {
        BOOST_MESSAGE("Testing simple covariance models...");

        SavedSettings backup;

        const Size size = 10;
        const Real tolerance = 1e-14;
        Size i;

        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmExponentialCorrelationModel(size, 0.1));

        Matrix recon = corrModel->correlation(0.0)
            - corrModel->pseudoSqrt(0.0)*transpose(corrModel->pseudoSqrt(0.0));

        for (i=0; i<size; ++i) {
            for (Size j=0; j<size; ++j) {
                if (std::fabs(recon[i][j]) > tolerance)
                    BOOST_ERROR("Failed to reproduce correlation matrix"
                    << "\n    calculated: " << recon[i][j]
                    << "\n    expected:   " << 0);
            }
        }

        std::vector<Time> fixingTimes(size);
        for (i=0; i<size; ++i) {
            fixingTimes[i] = 0.5*i;
        }

        const Real a=0.2;
        const Real b=0.1;
        const Real c=2.1;
        const Real d=0.3;

        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmLinearExponentialVolatilityModel(fixingTimes, a, b, c, d));

        boost::shared_ptr<LfmCovarianceProxy> covarProxy(
            new LfmCovarianceProxy(volaModel, corrModel));

        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, makeIndex()));

        boost::shared_ptr<LiborForwardModel> liborModel(
            new LiborForwardModel(process, volaModel, corrModel));

        std::cout.precision(3);
        for (Real t=0; t<4.6; t+=0.31) { //14*0.31=4.34<4.60
            recon = covarProxy->covariance(t)
                - covarProxy->diffusion(t)*transpose(covarProxy->diffusion(t));
            std::cout << "Covariance Matrix" << std::endl;
            std::cout << covarProxy->covariance(t);
            for (Size i=0; i<size; ++i) {
                for (Size j=0; j<size; ++j) {
                    if (std::fabs(recon[i][j]) > tolerance)
                        BOOST_ERROR("Failed to reproduce correlation matrix"
                        << "\n    calculated: " << recon[i][j]
                    << "\n    expected:   " << 0);
                }
            }

            Array volatility = volaModel->volatility(t);
            std::cout << "Volatility" << std::endl;
            std::cout << volatility << std::endl;
            for (Size k=0; k<size; ++k) {
                Real expected = 0;
                if (k>2*t) {
                    const Real T = fixingTimes[k];
                    expected=(a*(T-t)+d)*std::exp(-b*(T-t)) + c;
                }

                if (std::fabs(expected - volatility[k]) > tolerance)
                    BOOST_ERROR("Failed to reproduce volatilities"
                    << "\n    calculated: " << volatility[k]
                << "\n    expected:   " << expected);
            }
        }
    }


    void LiborMarketModelTest::testCapletPricing() {
        BOOST_MESSAGE("Testing caplet pricing...");

        SavedSettings backup;

        const Size size = 10;
    #if defined(QL_USE_INDEXED_COUPON)
        const Real tolerance = 1e-5;
    #else
        const Real tolerance = 1e-12;
    #endif

        boost::shared_ptr<IborIndex> index = makeIndex();
        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, index));

        // set-up pricing engine
        const boost::shared_ptr<OptionletVolatilityStructure> capVolCurve =
            makeCapVolCurve(Settings::instance().evaluationDate()); //input caplet volatility curve

        Array variances = LfmHullWhiteParameterization(process, capVolCurve)
            .covariance(0.0).diagonal();
        std::cout << Sqrt(variances) << std::endl;
        // caplet volatility = Sqrt(variances) = [ 0.000000; 0.144000; 0.196030; 0.159045; 0.162679; 0.139841; 0.137742; 0.128101; 0.138934; 0.116066 ]

        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmFixedVolatilityModel(Sqrt(variances),
            process->fixingTimes()));

        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmExponentialCorrelationModel(size, 0.3)); //rho=0.3

        boost::shared_ptr<AffineModel> model(
            new LiborForwardModel(process, volaModel, corrModel));

        const Handle<YieldTermStructure> termStructure =
            process->index()->forwardingTermStructure();

        boost::shared_ptr<AnalyticCapFloorEngine> engine1(
            new AnalyticCapFloorEngine(model, termStructure));

        boost::shared_ptr<Cap> cap1(
            new Cap(process->cashFlows(),
            std::vector<Rate>(size, 0.04))); //exercise rate=0.04
        cap1->setPricingEngine(engine1);

        const Real expected = 0.015853935178;
        const Real calculated = cap1->NPV();

        std::cout << "Cap NPV: " << cap1->NPV() << std::endl;

        if (std::fabs(expected - calculated) > tolerance)
            BOOST_ERROR("Failed to reproduce npv"
            << "\n    calculated: " << calculated
            << "\n    expected:   " << expected);
    }

    void LiborMarketModelTest::testCalibration() {
        BOOST_MESSAGE("Testing calibration of a Libor forward model...");

        SavedSettings backup;

        const Size size = 14;
        const Real tolerance = 8e-3;

        /* In reality, one has to strip caplet volatilities back from cap quotes
            cap market prices (cap quotes) -> implied caplet volatilities
        */
        Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
            0.169007,0.167956,0.166261,0.164239,
            0.162082,0.159923,0.157781,0.155745,
            0.153776,0.151950,0.150189,0.148582,
            0.147034,0.145598,0.144248}; //19

        Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
            0.136930, 0.126833, 0.118135, 0.175963,
            0.166359, 0.155203, 0.143712, 0.132769,
            0.122947, 0.114310, 0.174455, 0.162265,
            0.150539, 0.138734, 0.128215, 0.118470,
            0.110540, 0.169780, 0.156860, 0.144821,
            0.133537, 0.123167, 0.114363, 0.106500,
            0.164521, 0.151223, 0.139670, 0.128632,
            0.119123, 0.110330, 0.103114, 0.158956,
            0.146036, 0.134555, 0.124393, 0.115038,
            0.106996, 0.100064}; //42

        boost::shared_ptr<IborIndex> index = makeIndex();
        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, index));
        Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();

        // set-up the model
        // sigma_n(t)=sigma(T_n-t)=(a(T_n-t)+b)*exp(-c(T_n-t))+d)
        Real aguess = 0.5;
        Real bguess = 0.6;
        Real cguess = 0.1;
        Real dguess = 0.1;
        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmExtLinearExponentialVolModel(process->fixingTimes(), aguess, bguess, cguess, dguess));

        // rho(i,j;rho,beta)=rho+(1-rho)*exp(-beta*|i-j|)
        Real rhoguess = 0.5;
        Real betaguess = 0.8;
        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmLinearExponentialCorrelationModel(size, rhoguess, betaguess));

        boost::shared_ptr<LiborForwardModel> model(
            new LiborForwardModel(process, volaModel, corrModel));

        Size swapVolIndex = 0;
        DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

        // set-up calibration helper
        std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

        Size i;
        for (i=2; i < size; ++i) { // 2,3,...,13
            const Period maturity = i*index->tenor();
            Handle<Quote> capVol(
                boost::shared_ptr<Quote>(new SimpleQuote(capVols[i-2])));

            boost::shared_ptr<CalibrationHelper> caphelper(
                new CapHelper(maturity, capVol, index, Annual,
                index->dayCounter(), true, termStructure,
                CalibrationHelper::ImpliedVolError));

            caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new AnalyticCapFloorEngine(model, termStructure)));

            calibrationHelper.push_back(caphelper);

            if (i<= size/2) { // 2,3,4,5,6,7
                // add a few swaptions to test swaption calibration as well
                for (Size j=1; j <= size/2; ++j) {
                    const Period len = j*index->tenor();
                    Handle<Quote> swaptionVol(
                        boost::shared_ptr<Quote>(
                        new SimpleQuote(swaptionVols[swapVolIndex++])));

                    boost::shared_ptr<CalibrationHelper> swaptionHelper(
                        new SwaptionHelper(maturity, len, swaptionVol, index,
                        index->tenor(), dayCounter,
                        index->dayCounter(),
                        termStructure,
                        CalibrationHelper::ImpliedVolError));

                    swaptionHelper->setPricingEngine(
                        boost::shared_ptr<PricingEngine>(
                        new LfmSwaptionEngine(model,termStructure)));

                    calibrationHelper.push_back(swaptionHelper);
                }
            }
        }
        std::cout << "calibration helper size: " << calibrationHelper.size() << std::endl; // 12+7*(12/2)=54

        /*
        boost::shared_ptr<LiborForwardModel>(
            boost::shared_ptr<LiborForwardModelProcess>, //boost::shared_ptr<LiborForwardModelProcess> process(new LiborForwardModelProcess(size, index));
            boost::shared_ptr<LmVolatilityModel>, //LmExtLinearExponentialVolModel(process->fixingTimes(),0.5,0.6,0.1,0.1));
            boost::shared_ptr<LmCorrelationModel> //LmLinearExponentialCorrelationModel(size, 0.5, 0.8));
            ))->calibrate(
                std::vector<boost::shared_ptr<CalibrationHelper> >,
                LevenbergMarquardt(1e-6, 1e-6, 1e-6),
                EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
        */

        /*
            Calibrated parameters
            1. correlation of forward LIBOR rates L_n(t)=L(t,Tn,Tn+1): mu(j,k)=Corr(dL_k(t),dL_j(t))=q(T_k-t,T_j-t)
            2. volatilities of forward LIBOR rates L_n(t)=L(t,Tn,Tn+1): sigma_n(t), t=0,...,T_M-1, n=t,...,M
               sigma_n(t)=lambda_n(t)*phi(L_n(t))
                         =(a(T_n-t)+b)*exp(-c(T_n-t))+d)*k_n
                         =(a(T_n-t)+b)*exp(-c(T_n-t))+d)*(L_n(t))^p
                         =(a(T_n-t)+b)*exp(-c(T_n-t))+d)*(b*L_n(t)+a)
               sigma_n(t)=v(n)*sigma_n(t)
                         =v(n)*sigma(T_n-t)
                         =v(n)*(a(T_n-t)+b)*exp(-c(T_n-t))+d)
        */

        LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
        model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));

        std::cout.precision(10);

        // measure the calibration error
        Real calculated = 0.0;
        for (i=0; i<calibrationHelper.size(); ++i) {
            Real diff = calibrationHelper[i]->calibrationError();
            calculated += diff*diff;
            std::cout << i << ": " << calibrationHelper[i]->marketValue() << std::endl;
        }
        if (std::sqrt(calculated) > tolerance)
            BOOST_ERROR("Failed to calibrate libor forward model"
            << "\n    calculated diff: " << std::sqrt(calculated)
            << "\n    expected : smaller than  " << tolerance);
    }

    void LiborMarketModelTest::testSwaptionPricing() {
        BOOST_MESSAGE("Testing forward swap and swaption pricing...");

        SavedSettings backup;

        const Size size  = 10;
        const Size steps = 8*size;
    #if defined(QL_USE_INDEXED_COUPON)
        const Real tolerance = 1e-6;
    #else
        const Real tolerance = 1e-12;
    #endif

        std::vector<Date> dates;
        std::vector<Rate> rates;
        dates.push_back(Date(4,September,2005));
        dates.push_back(Date(4,September,2011));
        rates.push_back(0.04);
        rates.push_back(0.08);

        boost::shared_ptr<IborIndex> index = makeIndex(dates, rates);

        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, index));

        Real rhoguess = 0.5;
        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmExponentialCorrelationModel(size, rhoguess));

        Real aguess = 0.291;
        Real bguess = 1.483;
        Real cguess = 0.116;
        Real dguess = 0.00001;
        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmLinearExponentialVolatilityModel(process->fixingTimes(),
            aguess, bguess, cguess, dguess));

        // set-up pricing engine
        process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
            new LfmCovarianceProxy(volaModel, corrModel)));

        // set-up a small Monte-Carlo simulation to price swaptions
        typedef PseudoRandom::rsg_type rsg_type;
        typedef MultiPathGenerator<rsg_type>::sample_type sample_type;

        std::vector<Time> tmp = process->fixingTimes();
        TimeGrid grid(tmp.begin(), tmp.end(), steps);

        Size i;
        std::vector<Size> location;
        for (i=0; i < tmp.size(); ++i) {
            location.push_back(
                std::find(grid.begin(),grid.end(),tmp[i])-grid.begin());
        }

        rsg_type rsg = PseudoRandom::make_sequence_generator(
            process->factors()*(grid.size()-1),
            BigNatural(42));

        const Size nrTrails = 5000;
        MultiPathGenerator<rsg_type> generator(process, grid, rsg, false);

        boost::shared_ptr<LiborForwardModel>
            liborModel(new LiborForwardModel(process, volaModel, corrModel));

        Calendar calendar = index->fixingCalendar();
        DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
        BusinessDayConvention convention = index->businessDayConvention();

        Date settlement  = index->forwardingTermStructure()->referenceDate();

        boost::shared_ptr<SwaptionVolatilityMatrix> m =
            liborModel->getSwaptionVolatilityMatrix();


        std::cout << "Swaption NPV" << std::endl;
        for (i=1; i < size; ++i) { //i=1,2,,,9
            for (Size j=1; j <= size-i; ++j) { //j=1-9,1-8,...1-2,1
                Date fwdStart    = settlement + Period(6*i, Months);
                Date fwdMaturity = fwdStart + Period(6*j, Months);

                Schedule schedule(fwdStart, fwdMaturity, index->tenor(), calendar,
                    convention, convention, DateGeneration::Forward, false);

                Rate swapRate  = 0.0404;
                boost::shared_ptr<VanillaSwap> forwardSwap( //Swap
                    new VanillaSwap(VanillaSwap::Receiver, 1.0,
                    schedule, swapRate, dayCounter,
                    schedule, index, 0.0, index->dayCounter()));
                forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
                    new DiscountingSwapEngine(index->forwardingTermStructure())));

                // check forward pricing first
                const Real expected = forwardSwap->fairRate();
                const Real calculated = liborModel->S_0(i-1,i+j-1);

                if (std::fabs(expected - calculated) > tolerance)
                    BOOST_ERROR("Failed to reproduce fair forward swap rate"
                    << "\n    calculated: " << calculated
                    << "\n    expected:   " << expected);

                swapRate = forwardSwap->fairRate();
                forwardSwap = boost::shared_ptr<VanillaSwap>(
                    new VanillaSwap(VanillaSwap::Receiver, 1.0,
                    schedule, swapRate, dayCounter,
                    schedule, index, 0.0, index->dayCounter()));
                forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
                    new DiscountingSwapEngine(index->forwardingTermStructure())));

                if (i == j && i<=size/2) { //i=j=1,2,3,4,5
                    boost::shared_ptr<PricingEngine> engine(
                        new LfmSwaptionEngine(liborModel,
                        index->forwardingTermStructure()));
                    boost::shared_ptr<Exercise> exercise(
                        new EuropeanExercise(process->fixingDates()[i]));

                    boost::shared_ptr<Swaption> swaption( //Swaption
                        new Swaption(forwardSwap, exercise));
                    swaption->setPricingEngine(engine);

                    GeneralStatistics stat;

                    for (Size n=0; n<nrTrails; ++n) {
                        sample_type path = (n%2) ? generator.antithetic()
                            : generator.next();

                        std::vector<Rate> rates(size);
                        for (Size k=0; k<process->size(); ++k) {
                            rates[k] = path.value[k][location[i]];
                        }
                        std::vector<DiscountFactor> dis =
                            process->discountBond(rates);

                        Real npv=0.0;
                        for (Size m=i; m < i+j; ++m) {
                            npv += (swapRate - rates[m])
                                * (  process->accrualEndTimes()[m]
                            - process->accrualStartTimes()[m])*dis[m];
                        }
                        stat.add(std::max(npv, 0.0));
                    }

                    if (std::fabs(swaption->NPV() - stat.mean())
                        > stat.errorEstimate()*2.35)
                        BOOST_ERROR("Failed to reproduce swaption npv"
                        << "\n    calculated: " << stat.mean()
                        << "\n    expected:   " << swaption->NPV());

                    std::cout << swaption->NPV() << ", " << stat.mean() << std::endl;
                }
            }
        }
    }

    void LiborMarketModelTest::mylmmtest(){
        BOOST_MESSAGE(" Testing calibration of a Libor forward model ");

        SavedSettings backup;

        const Size size = 14;
        const Real tolerance = 8e-3;

        /*
            1. Time dependent volatilities of forward LIBOR rates: lambdak(t)
            2. Correlation of forward LIBOR rates
        */

        Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
            0.169007,0.167956,0.166261,0.164239,
            0.162082,0.159923,0.157781,0.155745,
            0.153776,0.151950,0.150189,0.148582,
            0.147034,0.145598,0.144248}; //19

        Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
            0.136930, 0.126833, 0.118135, 0.175963,
            0.166359, 0.155203, 0.143712, 0.132769,
            0.122947, 0.114310, 0.174455, 0.162265,
            0.150539, 0.138734, 0.128215, 0.118470,
            0.110540, 0.169780, 0.156860, 0.144821,
            0.133537, 0.123167, 0.114363, 0.106500,
            0.164521, 0.151223, 0.139670, 0.128632,
            0.119123, 0.110330, 0.103114, 0.158956,
            0.146036, 0.134555, 0.124393, 0.115038,
            0.106996, 0.100064}; //42

        std::cout << "\ncapVols" << std::endl;
        for (Size i=0; i<LENGTH(capVols); ++i) {
            std::cout << capVols[i] << std::endl;
        }
        std::cout << "\nswaptionVols" << std::endl;
        for (Size i=0; i<LENGTH(swaptionVols); ++i) {
            std::cout << swaptionVols[i] << std::endl;
        }


        boost::shared_ptr<IborIndex> index = makeIndex();
        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, index));
        Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();

        // set-up the model
        // Rebonato ||lambdak(t)||=g(t,x)=g(x)=(a+bx)exp(-cx)+d
        // sigma_i(t)=k_i*((a*(T_{i}-t)+d)*e^{-b(T_{i}-t)}+c)
        const Real a=0.5; //0.2
        const Real b=0.6; //0.1
        const Real c=0.1; //2.1
        const Real d=0.1; //0.3
        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmExtLinearExponentialVolModel(process->fixingTimes(), a, b, c, d));
        std::cout << "\nCaplet Volatility" << std::endl;
        for (Size t=0; t<size/2; ++t) {
            std::cout << t << ": " << volaModel->volatility(t) << std::endl;
        }

        // rho_{i,j}=rho + (1-rho)*e^{(-\beta \|i-j\|)}
        const Real rho=0.5;
        const Real beta=0.5;
        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmLinearExponentialCorrelationModel(size, rho, beta));
        // lmexpcorrmodel.cpp
        std::cout << "\nForward Libor Correlation" << std::endl;
        std::cout << corrModel->correlation(0) << std::endl;
        /*
        std::cout << "sqrt(correlation)" << std::endl;
        std::cout << corrModel->pseudoSqrt(0) << std::endl;
        std::cout << "sqrt(correlation)^T*sqrt(correlation)" << std::endl;
        std::cout << (corrModel->pseudoSqrt(0))*transpose(corrModel->pseudoSqrt(0)) << std::endl;
        */

        boost::shared_ptr<LiborForwardModel> model(
            new LiborForwardModel(process, volaModel, corrModel));
        const Array initparams = model->params();

        Size swapVolIndex = 0;
        DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

        // set-up calibration helper
        std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

        Size i;
        for (i=2; i < size; ++i) { // 2,3,...,17
            const Period maturity = i*index->tenor();
            Handle<Quote> capVol(
                boost::shared_ptr<Quote>(new SimpleQuote(capVols[i-2])));

            boost::shared_ptr<CalibrationHelper> caphelper(
                new CapHelper(maturity, capVol, index, Annual,
                index->dayCounter(), true, termStructure,
                CalibrationHelper::ImpliedVolError));

            caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new AnalyticCapFloorEngine(model, termStructure)));

            calibrationHelper.push_back(caphelper);

            if (i<= size/2) { // 2,3,4,5,6,7,8,9
                // add a few swaptions to test swaption calibration as well
                for (Size j=1; j <= size/2; ++j) {
                    const Period len = j*index->tenor();
                    Handle<Quote> swaptionVol(
                        boost::shared_ptr<Quote>(
                        new SimpleQuote(swaptionVols[swapVolIndex++])));

                    boost::shared_ptr<CalibrationHelper> swaptionHelper(
                        new SwaptionHelper(maturity, len, swaptionVol, index,
                        index->tenor(), dayCounter,
                        index->dayCounter(),
                        termStructure,
                        CalibrationHelper::ImpliedVolError));

                    swaptionHelper->setPricingEngine(
                        boost::shared_ptr<PricingEngine>(
                        new LfmSwaptionEngine(model,termStructure)));

                    calibrationHelper.push_back(swaptionHelper);
                }
            }
        }

        /*
        boost::shared_ptr<LiborForwardModel>(
            boost::shared_ptr<LiborForwardModelProcess>, //boost::shared_ptr<LiborForwardModelProcess> process(new LiborForwardModelProcess(size, index));
            boost::shared_ptr<LmVolatilityModel>, //LmExtLinearExponentialVolModel(process->fixingTimes(),0.5,0.6,0.1,0.1));
            boost::shared_ptr<LmCorrelationModel> //LmLinearExponentialCorrelationModel(size, 0.5, 0.8));
            ))->calibrate(
                std::vector<boost::shared_ptr<CalibrationHelper> >,
                LevenbergMarquardt(1e-6, 1e-6, 1e-6),
                EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
        */

        LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
        model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
        const Array calibparams = model->params();

        std::cout.precision(10);

        // measure the calibration error
        std::cout << "\nCalibration Helper Parameters (After Calibration)" << std::endl;
        std::cout << "Market Value\tCalibration Error" << std::endl;
        Real calculated = 0.0;
        for (i=0; i<calibrationHelper.size(); ++i) {
            Real diff = calibrationHelper[i]->calibrationError();
            calculated += diff*diff;

            //std::cout << calibrationHelper[i]->marketValue() << std::endl; //i << ": " <<
            std::cout << i << ": " << calibrationHelper[i]->marketValue() << "\ts" << calibrationHelper[i]->calibrationError() << std::endl;
        }
        if (std::sqrt(calculated) > tolerance)
            BOOST_ERROR("Failed to calibrate libor forward model"
            << "\n    calculated diff: " << std::sqrt(calculated)
            << "\n    expected : smaller than  " << tolerance);

        const char paramnames[20][64] = {"a","b","c","d",
                             "sigma1","sigma2","sigma3","sigma4","sigma5",
                             "sigma6","sigma7","sigma8","sigma9","sigma10",
                             "sigma11","sigma12","sigma13","sigma14", //forward libor volatilities: sigma
                                         "rho","beta"};  //4+14+2=20

        std::cout << "\nParameters" << std::endl;
        std::cout << std::setw(3) << "i"
                  << std::setw(10) << "Parameter"
                  << std::setw(12) << "Initial"
                  << std::setw(25) << "Calibrated" << std::endl;
        for (Size i=0; i<20/*calibparams.size()*/; ++i) {
            std::cout << std::setw(3) << i
                      << std::setw(10) << paramnames[i]
                      << std::setw(12) << initparams[i]
                      << std::setw(25) << calibparams[i] << std::endl;
        }
        std::cout << "\nEstimated Forward LIBOR Volatilities" << std::endl;
        for (Size i=4; i<size+4; ++i) { //calibparams.size()=20=4+14+2
            std::cout << std::setw(3) << ((i-4.0)/2)
                      << std::setw(10) << paramnames[i]
                      << std::setw(18) << calibparams[i] << std::endl;
        }


        //******************************************************************************
        // Repricing caps and swaptions using the calibrated parameters
        //******************************************************************************
        std::cout << "\n\nRepricing caps and swaptions using the calibrated parameters" << std::endl;
        boost::shared_ptr<LmVolatilityModel> volaModelOpt(
                new LmExtLinearExponentialVolModel(process->fixingTimes(), calibparams[0], calibparams[1], calibparams[2], calibparams[3]));
        boost::shared_ptr<LmCorrelationModel> corrModelOpt(
            new LmLinearExponentialCorrelationModel(size, calibparams[19], calibparams[20]));
        /*boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, makeIndex()));*/
        process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
            new LfmCovarianceProxy(volaModelOpt, corrModelOpt)));
        boost::shared_ptr<LiborForwardModel> liborModelOpt(
            new LiborForwardModel(process, volaModelOpt, corrModelOpt));

        Calendar calendar = index->fixingCalendar();
        //DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
        BusinessDayConvention convention = index->businessDayConvention();
        Date settlement  = index->forwardingTermStructure()->referenceDate();

        //boost::shared_ptr<SwaptionVolatilityMatrix> volaMatOpt =
        //liborModelOpt->getSwaptionVolatilityMatrix();
        //std::cout << volaModelOpt->volatility(0.0) << std::endl;
        //std::cout << corrModelOpt->correlation(0.0) << std::endl;
        std::cout.precision(4);
        std::cout << "\nForward LIBOR volatility matrix recovered from cap/swaption volatilities" << std::endl;
        boost::shared_ptr<LfmCovarianceProxy> covarProxyOpt(
            new LfmCovarianceProxy(volaModelOpt, corrModelOpt));
        std::cout << "Forward LIBOR proxy volatility at time 0" << std::endl;
        std::cout << volaModelOpt->volatility(0) << std::endl << std::endl;
        std::cout << "Forward LIBOR proxy covariance matrix at time 0" << std::endl;
        std::cout << covarProxyOpt->covariance(0) << std::endl << std::endl;
        for (Real t=0; t<4.6; t+=0.31) { //14*0.31=4.34<4.60
            std::cout << "Forward LIBOR proxy volatility at time " << t << std::endl;
            std::cout << covarProxyOpt->covariance(t) << std::endl;
            std::cout << "Forward LIBOR proxy volatility matrix at time " << t << std::endl;
            std::cout << volaModelOpt->volatility(t) << std::endl;
        }

        std::cout << "\nSwaption NPV using calibrated forward LIBOR vols/corrs parameters" << std::endl;
        for (i=1; i < size; ++i) { //i=1,2,,,14
            for (Size j=1; j <= size-i; ++j) { //j=1~14,1~13,1-9,1-8,...1-2,1
                Date fwdStart    = settlement + Period(6*i, Months);
                Date fwdMaturity = fwdStart + Period(6*j, Months);

                Schedule schedule(fwdStart, fwdMaturity, index->tenor(), calendar,
                    convention, convention, DateGeneration::Forward, false);

                Rate swapRate  = 0.0404;
                boost::shared_ptr<VanillaSwap> forwardSwap(
                    new VanillaSwap(VanillaSwap::Receiver, 1.0,
                    schedule, swapRate, dayCounter,
                    schedule, index, 0.0, index->dayCounter()));
                forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
                    new DiscountingSwapEngine(index->forwardingTermStructure())));

                swapRate = forwardSwap->fairRate();
                forwardSwap = boost::shared_ptr<VanillaSwap>(
                    new VanillaSwap(VanillaSwap::Receiver, 1.0,
                    schedule, swapRate, dayCounter,
                    schedule, index, 0.0, index->dayCounter()));
                forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
                    new DiscountingSwapEngine(index->forwardingTermStructure())));

                if (i == j && i<=size) { //i=j=1,2,3,4,5,6,7,...,14
                    boost::shared_ptr<PricingEngine> engine(
                        new LfmSwaptionEngine(liborModelOpt,
                        index->forwardingTermStructure()));
                    boost::shared_ptr<Exercise> exercise(
                        new EuropeanExercise(process->fixingDates()[i]));

                    boost::shared_ptr<Swaption> swaption( //Swaption
                        new Swaption(forwardSwap, exercise));
                    swaption->setPricingEngine(engine);

                    Real swaptionNPV = swaption->NPV();
                    //std::cout << swaption->NPV() << std::endl;
                    std::cout << swaption->impliedVolatility(swaptionNPV,index->forwardingTermStructure(),0.15) << std::endl;
                }
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////

    namespace {

        boost::shared_ptr<IborIndex> makeIndex_(std::vector<Date> dates,
            std::vector<Rate> rates) {
                DayCounter dayCounter = Actual360();

                RelinkableHandle<YieldTermStructure> termStructure;

                boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));

                Date todaysDate = index->fixingCalendar().adjust(Date(31,October,2001));// Input
                Settings::instance().evaluationDate() = todaysDate;

                dates[0] = index->fixingCalendar().advance(todaysDate,
                    index->fixingDays(), Days);

                termStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
                    new ZeroCurve(dates, rates, dayCounter)));

                return index;
        }

        // Interpolate forward LIBOR rates
        boost::shared_ptr<IborIndex> makeIndex_() {
            std::vector<Date> dates;
            std::vector<Rate> rates;
            dates.push_back(Date(31,October,2001));// Input
            dates.push_back(Date(31,October,2011));
            rates.push_back(0.0009);
            rates.push_back(0.0275);

            return makeIndex_(dates, rates);
        }
    }


    void LiborMarketModelTest::testIshiyamaLMM(){

        BOOST_MESSAGE("ΎRKY Forward LIBOR Market Model Calibration Test");

        //******************************************************************************
        // Calibrate LMM vol/corr parameters using cap/swaption volatilities
        //******************************************************************************
        SavedSettings backup;

        const Size size = 20;
        const Real tolerance = 8e-3;

        // caplet volatility curve
        Volatility capVols[] = {153.50, 140.00, 126.50, 113.00, 100.50, 88.00, 83.75, 79.50, 73.00, 66.50,
            61.75, 57.00, 52.75, 48.50, 48.00, 47.50, 45.75, 44.00, 42.00, 40.00};//20
        /*Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
            0.169007,0.167956,0.166261,0.164239,
            0.162082,0.159923,0.157781,0.155745,
            0.153776,0.151950,0.150189,0.148582,
            0.147034,0.145598,0.144248}; //19*/

        Volatility swaptionVols[] = {82.40, 56.60, 53.10, 49.00, 44.90, 42.25, 39.60, 36.33, 33.07, 29.80,
                                     52.20, 47.70, 45.20, 41.10, 37.60, 33.85, 30.10, 28.63, 27.17, 25.70,
                                     44.80,	41.50, 36.90, 33.55, 31.50, 28.75, 26.00, 24.85, 23.70, 22.55,
                                     42.10, 36.20, 32.00, 33.30, 30.70, 26.88, 22.65, 21.75, 20.85, 19.95,
                                     35.20, 29.00, 26.30, 23.60, 21.40, 20.50, 19.60, 19.20, 18.80, 18.40,
                                     29.80, 25.13, 23.50, 21.58, 19.85, 19.25, 18.65, 18.34, 18.03, 17.73,
                                     24.40, 21.25, 20.70, 19.55, 18.30, 18.00, 17.70, 17.48, 17.27, 17.05,
                                     23.13, 20.23, 20.00, 18.88, 17.53, 17.23, 16.93, 16.75, 16.57, 16.38,
                                     21.87, 19.22, 19.30, 18.22, 16.77, 16.47, 16.17, 16.02, 15.87, 15.72,
                                     20.60, 18.20, 18.60, 17.55, 16.00, 15.70, 15.40, 15.28, 15.17, 15.05};//10x10=100
        /*Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
            0.136930, 0.126833, 0.118135, 0.175963,
            0.166359, 0.155203, 0.143712, 0.132769,
            0.122947, 0.114310, 0.174455, 0.162265,
            0.150539, 0.138734, 0.128215, 0.118470,
            0.110540, 0.169780, 0.156860, 0.144821,
            0.133537, 0.123167, 0.114363, 0.106500,
            0.164521, 0.151223, 0.139670, 0.128632,
            0.119123, 0.110330, 0.103114, 0.158956,
            0.146036, 0.134555, 0.124393, 0.115038,
            0.106996, 0.100064}; //42*/

        boost::shared_ptr<IborIndex> index = makeIndex_();
        index->forwardingTermStructure()->enableExtrapolation(true);

        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, index));

        Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();

        // set-up the model
        Real aguess = 0.5;
        Real bguess = 0.6;
        Real cguess = 0.1;
        Real dguess = 0.1;
        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmExtLinearExponentialVolModel(process->fixingTimes(), aguess, bguess, cguess, dguess));

        Real rhoguess = 0.5;
        Real betaguess = 0.8;
        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmLinearExponentialCorrelationModel(size, rhoguess, betaguess));

        boost::shared_ptr<LiborForwardModel> model(
            new LiborForwardModel(process, volaModel, corrModel));
        const Array initparams = model->params();

        Size swapVolIndex = 0;
        DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

        // set-up calibration helper
        std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

        int caphelpersize =0;
        int swaptionhelpersize =0;

        Size i;
        for (i=1; i <= size; ++i) { // 1,2,...,20
            const Period maturity = i*index->tenor(); //0.5y,1y,1.5y,...,10y
            Handle<Quote> capVol(
                boost::shared_ptr<Quote>(new SimpleQuote(capVols[i-1]/100.0))); //i-1=0,1,...,19

            boost::shared_ptr<CalibrationHelper> caphelper(
                new CapHelper(maturity, capVol, index, Annual,
                index->dayCounter(), true, termStructure,
                CalibrationHelper::ImpliedVolError));

            caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new AnalyticCapFloorEngine(model, termStructure)));

            calibrationHelper.push_back(caphelper);
            caphelpersize++; //caphelpersize=1,2,...,20

            // add a few swaptions to test swaption calibration as well
            if (i < size/2) { //i=1,2,...,9
                for (Size j=1; j <= (size/2-i); ++j) { //j=1~9, 1~8,...,1~2, 1
                    const Period len = 2*j*index->tenor(); //1y~9y, 1y~8y,..., 1y
                    swapVolIndex = (j-1) + (i-1)*10;
                    Handle<Quote> swaptionVol(
                        boost::shared_ptr<Quote>(
                        new SimpleQuote(swaptionVols[swapVolIndex]/100.0)));
                    //std::cout << swaptionVols[swapVolIndex] << ", ";

                    boost::shared_ptr<CalibrationHelper> swaptionHelper(
                        new SwaptionHelper(maturity, len, swaptionVol, index,
                        index->tenor(), dayCounter,
                        index->dayCounter(),
                        termStructure,
                        CalibrationHelper::ImpliedVolError));

                    swaptionHelper->setPricingEngine(
                        boost::shared_ptr<PricingEngine>(
                        new LfmSwaptionEngine(model,termStructure)));

                    calibrationHelper.push_back(swaptionHelper);
                    swaptionhelpersize++;
                    std::cout << "(" << i << ", " << j << "; " << swapVolIndex << "), ";
                }
                std::cout << std::endl;
            }

        }

        std::cout << "cap helper size: " << caphelpersize << std::endl; //20
        std::cout << "swaption helper size: " << swaptionhelpersize << std::endl; // 9(9+1)/2=45
        std::cout << "calibration helper size: " << calibrationHelper.size() << std::endl; // 65

        LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
        model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
        const Array calibparams = model->params();

        std::cout.precision(10);

        // measure the calibration error
        std::cout << "\nCalibration Helper Parameters (After Calibration)" << std::endl;
        std::cout << "Market Value\tCalibration Error" << std::endl;
        Real calculated = 0.0;
        for (i=0; i<calibrationHelper.size(); ++i) {
            Real diff = calibrationHelper[i]->calibrationError();
            calculated += diff*diff;
            std::cout << i << ": " << calibrationHelper[i]->marketValue() << "\ts" << calibrationHelper[i]->calibrationError() << std::endl;
        }
        if (std::sqrt(calculated) > tolerance)
            BOOST_ERROR("Failed to calibrate libor forward model"
            << "\n    calculated diff: " << std::sqrt(calculated)
            << "\n    expected : smaller than  " << tolerance);


        // print parameters
        const char paramnames[26][64] = {"a","b","c","d",
            "sigma1","sigma2","sigma3","sigma4","sigma5",
            "sigma6","sigma7","sigma8","sigma9","sigma10",
            "sigma11","sigma12","sigma13","sigma14",
            "sigma15","sigma16","sigma17","sigma18","sigma19","sigma20", //forward libor volatilities: sigma
            "rho","beta"};  //4+20+2=26

        std::cout << "\nParameters" << std::endl;
        std::cout << std::setw(3) << "i" << std::setw(10) << "Parameter" << std::setw(15) << "Initial" << std::setw(15) << "Calibrated" << std::endl;
        for (Size i=0; i<calibparams.size(); ++i) {
            std::cout << std::setw(3) << i << std::setw(10) << paramnames[i]
            << std::setw(15) << initparams[i] << std::setw(15) << calibparams[i] << std::endl;
        }
        std::cout << "\nEstimated Forward LIBOR Local Volatilities" << std::endl;
        for (Size i=4; i<24; ++i) { //calibparams.size()=26=4+20+2
            std::cout << std::setw(3) << ((i-4.0)/2) << std::setw(10) << paramnames[i] << std::setw(15) << calibparams[i] << std::endl;
        }


        //******************************************************************************
        // Repricing caps and swaptions using the calibrated parameters
        //******************************************************************************
        std::cout << "\n\nRepricing caps and swaptions using the calibrated parameters" << std::endl;
        boost::shared_ptr<LmVolatilityModel> volaModelOpt(
                new LmExtLinearExponentialVolModel(process->fixingTimes(), calibparams[0], calibparams[1], calibparams[2], calibparams[3]));
        boost::shared_ptr<LmCorrelationModel> corrModelOpt(
            new LmLinearExponentialCorrelationModel(size, calibparams[24], calibparams[25]));
        /*boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, makeIndex()));*/
        process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
            new LfmCovarianceProxy(volaModelOpt, corrModelOpt)));
        boost::shared_ptr<LiborForwardModel> liborModelOpt(
            new LiborForwardModel(process, volaModelOpt, corrModelOpt));

        Calendar calendar = index->fixingCalendar();
        //DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
        BusinessDayConvention convention = index->businessDayConvention();
        Date settlement  = index->forwardingTermStructure()->referenceDate();

        std::cout.precision(3);
        std::cout << "\nForward LIBOR volatility matrix recovered from cap/swaption volatilities" << std::endl;
        boost::shared_ptr<LfmCovarianceProxy> covarProxyOpt(
            new LfmCovarianceProxy(volaModelOpt, corrModelOpt));
        std::cout << "Forward LIBOR proxy volatility at time 0" << std::endl;
        std::cout << volaModelOpt->volatility(0)*100.0 << std::endl;
        std::cout << "Forward LIBOR proxy covariance matrix at time 0" << std::endl;
        std::cout << covarProxyOpt->covariance(0)*100.0 << std::endl;
        std::cout << "Forward LIBOR proxy correlation matrix at time 0" << std::endl;
        std::cout << corrModelOpt->correlation(0) << std::endl;
        for (Real t=0; t<10.0; t+=0.5) {
            std::cout << "Forward LIBOR proxy volatility at time " << t << std::endl;
            std::cout << volaModelOpt->volatility(t)*100.0 << std::endl;
            std::cout << "Forward LIBOR proxy covariance at time " << t << std::endl;
            std::cout << covarProxyOpt->covariance(t)*100.0 << std::endl;
            /*std::cout << "Forward LIBOR proxy correlation matrix at time " << t << std::endl;
            std::cout << corrModelOpt->correlation(t) << std::endl;*/
        }
    }
}

//----------------------------------------------------------------------------------------

namespace NamespaceLiborMarketModelProcessTest {

    class LiborMarketModelProcessTest {
      public:
        static void testInitialisation();
        static void testLambdaBootstrapping();
        static void testMonteCarloCapletPricing();
    };

    /* Input Parameters
        boost::shared_ptr<IborIndex>
        boost::shared_ptr<CapletVarianceCurve>
        boost::shared_ptr<LiborForwardModelProcess>
    */

    Size len = 10;

    boost::shared_ptr<IborIndex> makeIndex() {
        DayCounter dayCounter = Actual360();
        std::vector<Date> dates;
        std::vector<Rate> rates;
        dates.push_back(Date(4,September,2005));
        dates.push_back(Date(4,September,2018));
        rates.push_back(0.01);
        rates.push_back(0.08);

        RelinkableHandle<YieldTermStructure> termStructure(
                      boost::shared_ptr<YieldTermStructure>(
                                      new ZeroCurve(dates,rates,dayCounter)));

        boost::shared_ptr<IborIndex> index(new Euribor1Y(termStructure));

        Date todaysDate =
            index->fixingCalendar().adjust(Date(4,September,2005));
        Settings::instance().evaluationDate() = todaysDate;

        dates[0] = index->fixingCalendar().advance(todaysDate,
                                                   index->fixingDays(), Days);

        termStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
                                    new ZeroCurve(dates, rates, dayCounter)));

        return index;
    }

    boost::shared_ptr<CapletVarianceCurve>
    makeCapVolCurve(const Date& todaysDate) {
        Volatility vols[] = {14.40, 17.15, 16.81, 16.64, 16.17,
                             15.78, 15.40, 15.21, 14.86, 14.54};

        std::vector<Date> dates;
        std::vector<Volatility> capletVols;
        boost::shared_ptr<LiborForwardModelProcess> process(
                            new LiborForwardModelProcess(len+1, makeIndex()));

        for (Size i=0; i < len; ++i) {
            capletVols.push_back(vols[i]/100);
            dates.push_back(process->fixingDates()[i+1]);
        }

        return boost::shared_ptr<CapletVarianceCurve>(
                         new CapletVarianceCurve(todaysDate, dates,
                                                 capletVols, ActualActual()));
    }

    boost::shared_ptr<LiborForwardModelProcess>
    makeProcess(const Matrix& volaComp = Matrix()) {
        Size factors = (volaComp.empty() ? 1 : volaComp.columns());

        boost::shared_ptr<IborIndex> index = makeIndex();
        boost::shared_ptr<LiborForwardModelProcess> process(
                                    new LiborForwardModelProcess(len, index));

        boost::shared_ptr<LfmCovarianceParameterization> fct(
                new LfmHullWhiteParameterization(
                    process,
                    makeCapVolCurve(Settings::instance().evaluationDate()),
                    volaComp * transpose(volaComp), factors));

        process->setCovarParam(fct);

        return process;
    }


    void LiborMarketModelProcessTest::testInitialisation() {
        BOOST_MESSAGE("Testing caplet LMM process initialisation...");

        SavedSettings backup;

        DayCounter dayCounter = Actual360();
        RelinkableHandle<YieldTermStructure> termStructure(
            flatRate(Date::todaysDate(), 0.04, dayCounter));

        boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));
        boost::shared_ptr<OptionletVolatilityStructure> capletVol(new
            ConstantOptionletVolatility(termStructure->referenceDate(),
                                        termStructure->calendar(),
                                        Following,
                                        0.2, //volatility
                                        termStructure->dayCounter()));

        Calendar calendar = index->fixingCalendar();

        for (Integer daysOffset=0; daysOffset < 1825 /* 5 year*/; daysOffset+=8) {
            Date todaysDate = calendar.adjust(Date::todaysDate()+daysOffset);
            Settings::instance().evaluationDate() = todaysDate;
            Date settlementDate =
                calendar.advance(todaysDate, index->fixingDays(), Days);

            termStructure.linkTo(flatRate(settlementDate, 0.04, dayCounter)); //forward rate=0.04

            LiborForwardModelProcess process(60, index);

            std::vector<Time> fixings = process.fixingTimes();
            for (Size i=1; i < fixings.size()-1; ++i) {
                Size ileft  = process.nextIndexReset(fixings[i]-0.000001);
                Size iright = process.nextIndexReset(fixings[i]+0.000001);
                Size ii     = process.nextIndexReset(fixings[i]);
                if ((ileft != i) || (iright != i+1) || (ii != i+1)) {
                    BOOST_ERROR("Failed to next index resets");
                }
            }
        }
    }

    void LiborMarketModelProcessTest::testLambdaBootstrapping() {
        BOOST_MESSAGE("Testing caplet LMM lambda bootstrapping...");

        SavedSettings backup;

        Real tolerance = 1e-10;
        Volatility lambdaExpected[]= {14.3010297550, 19.3821411939, 15.9816590141,
                                      15.9953118303, 14.0570815635, 13.5687599894,
                                      12.7477197786, 13.7056638165, 11.6191989567};

        boost::shared_ptr<LiborForwardModelProcess> process = makeProcess();

        Matrix covar = process->covariance(0.0, Null<Array>(), 1.0);

        for (Size i=0; i<9; ++i) {
            const Real calculated = std::sqrt(covar[i+1][i+1]);
            const Real expected   = lambdaExpected[i]/100;

            if (std::fabs(calculated - expected) > tolerance)
                BOOST_ERROR("Failed to reproduce expected lambda values"
                            << "\n    calculated: " << calculated
                            << "\n    expected:   " << expected);
        }

        boost::shared_ptr<LfmCovarianceParameterization> param =
            process->covarParam();

        std::vector<Time> tmp = process->fixingTimes();
        TimeGrid grid(tmp.begin(), tmp.end(), 14);

        for (Size t=0; t<grid.size(); ++t) {
            Matrix diff = (param->integratedCovariance(grid[t])
            - param->LfmCovarianceParameterization::integratedCovariance(grid[t]));
            std::cout << "\nLambda at time " << t << std::endl;
            for (Size i=0; i<diff.rows(); ++i) {
                for (Size j=0; j<diff.columns(); ++j) {
                    if (std::fabs(diff[i][j]) > tolerance) {
                         BOOST_FAIL("Failed to reproduce integrated covariance" <<
                            "\n    i: " << i <<
                            "\n    j: " << j <<
                            "\nerror: " << diff[i][j]);
                    }
                    std::cout << "\t" << param->integratedCovariance(grid[t])[i][j];
                }
                std::cout << std::endl;
            }
        }
    }

    void LiborMarketModelProcessTest::testMonteCarloCapletPricing() {
        BOOST_MESSAGE("Testing caplet LMM Monte-Carlo caplet pricing...");

        SavedSettings backup;

        /* factor loadings are taken from Hull & White article
           plus extra normalisation to get orthogonal eigenvectors
           http://www.rotman.utoronto.ca/~amackay/fin/libormktmodel2.pdf */
        Real compValues[] = {0.85549771, 0.46707264, 0.22353259,
                             0.91915359, 0.37716089, 0.11360610,
                             0.96438280, 0.26413316,-0.01412414,
                             0.97939148, 0.13492952,-0.15028753,
                             0.95970595,-0.00000000,-0.28100621,
                             0.97939148,-0.13492952,-0.15028753,
                             0.96438280,-0.26413316,-0.01412414,
                             0.91915359,-0.37716089, 0.11360610,
                             0.85549771,-0.46707264, 0.22353259};

        Matrix volaComp(9,3);
        std::copy(compValues, compValues+9*3, volaComp.begin());

        //
        std::cout << volaComp << std::endl;
        //

        boost::shared_ptr<LiborForwardModelProcess> process1 = makeProcess();
        boost::shared_ptr<LiborForwardModelProcess> process2 = makeProcess(
                                                                        volaComp);
        std::vector<Time> tmp = process1->fixingTimes();
        TimeGrid grid(tmp.begin(), tmp.end(),12);

        Size i;
        std::vector<Size> location;
        for (i=0; i < tmp.size(); ++i) {
            location.push_back(
                          std::find(grid.begin(),grid.end(),tmp[i])-grid.begin());
        }

        // set-up a small Monte-Carlo simulation to price caplets
        // and ratchet caps using a one- and a three factor libor market model
        typedef LowDiscrepancy::rsg_type rsg_type;
        typedef MultiPathGenerator<rsg_type>::sample_type sample_type;

        BigNatural seed = 42;
        rsg_type rsg1 = LowDiscrepancy::make_sequence_generator(
                                process1->factors()*(grid.size()-1), seed);
        rsg_type rsg2 = LowDiscrepancy::make_sequence_generator(
                                process2->factors()*(grid.size()-1), seed);
        MultiPathGenerator<rsg_type> generator1(process1, grid, rsg1, false);
        MultiPathGenerator<rsg_type> generator2(process2, grid, rsg2, false);

        const Size nrTrails = 250000;
        std::vector<GeneralStatistics> stat1(process1->size());
        std::vector<GeneralStatistics> stat2(process2->size());
        std::vector<GeneralStatistics> stat3(process2->size()-1);
        for (i=0; i<nrTrails; ++i) {
            sample_type path1 = generator1.next();
            sample_type path2 = generator2.next();

            std::vector<Rate> rates1(len);
            std::vector<Rate> rates2(len);
            for (Size j=0; j<process1->size(); ++j) {
                rates1[j] = path1.value[j][location[j]];
                rates2[j] = path2.value[j][location[j]];
            }

            std::vector<DiscountFactor> dis1 = process1->discountBond(rates1);
            std::vector<DiscountFactor> dis2 = process2->discountBond(rates2);

            for (Size k=0; k<process1->size(); ++k) {
                Real accrualPeriod =  process1->accrualEndTimes()[k]
                                    - process1->accrualStartTimes()[k];
                // caplet payoff function, cap rate at 4%
                Real payoff1 = std::max(rates1[k] - 0.04, 0.0) * accrualPeriod;

                Real payoff2 = std::max(rates2[k] - 0.04, 0.0) * accrualPeriod;
                stat1[k].add(dis1[k] * payoff1);
                stat2[k].add(dis2[k] * payoff2);

                if (k != 0) {
                    // ratchet cap payoff function
                    Real payoff3 =  std::max(rates2[k] - (rates2[k-1]+0.0025), 0.0)
                                  * accrualPeriod;
                    stat3[k-1].add(dis2[k] * payoff3);
                }
            }

        }

        Real capletNpv[] = {0.000000000000, 0.000002841629, 0.002533279333,
                            0.009577143571, 0.017746502618, 0.025216116835,
                            0.031608230268, 0.036645683881, 0.039792254012,
                            0.041829864365};

        Real ratchetNpv[] = {0.0082644895, 0.0082754754, 0.0082159966,
                             0.0082982822, 0.0083803357, 0.0084366961,
                             0.0084173270, 0.0081803406, 0.0079533814};


        std::cout << "process1->size(): " << process1->size() << std::endl;
        std::cout << "expected\tcalc1\tcalc2\tcalc3" << std::endl;
        for (Size k=0; k < process1->size(); ++k) {

            Real calculated1 = stat1[k].mean();
            Real tolerance1  = stat1[k].errorEstimate();
            Real expected    = capletNpv[k];

            std::cout << expected << "\t";
            std::cout << calculated1 << "\t";
            if (std::fabs(calculated1 - expected) > tolerance1) {
                BOOST_ERROR("Failed to reproduce expected caplet NPV"
                            << "\n    calculated: " << calculated1
                            << "\n    error int:  " << tolerance1
                            << "\n    expected:   " << expected);
            }

            Real calculated2 = stat2[k].mean();
            Real tolerance2  = stat2[k].errorEstimate();

            std::cout << calculated2 << "\t";
            if (std::fabs(calculated2 - expected) > tolerance2) {
                BOOST_ERROR("Failed to reproduce expected caplet NPV"
                            << "\n    calculated: " << calculated2
                            << "\n    error int:  " << tolerance2
                            << "\n    expected:   " << expected);
            }

            if (k != 0) {
                Real calculated3 = stat3[k-1].mean();
                Real tolerance3  = stat3[k-1].errorEstimate();
                expected    = ratchetNpv[k-1];

                Real refError = 1e-5; // 1e-5. error bars of the reference values

                std::cout << calculated3 << std::endl;
                if (std::fabs(calculated3 - expected) > tolerance3 + refError) {
                    BOOST_ERROR("Failed to reproduce expected caplet NPV"
                                << "\n    calculated: " << calculated3
                                << "\n    error int:  " << tolerance3 + refError
                                << "\n    expected:   " << expected);
                }
            } else{
                std::cout << std::endl;
            }
        }
    }
}


//----------------------------------------------------------------------------------------
namespace MyLMMTest {

    boost::shared_ptr<IborIndex> makeIndex(std::vector<Date> dates,
                                           std::vector<Rate> rates) {
        DayCounter dayCounter = Actual360();

        RelinkableHandle<YieldTermStructure> termStructure;

        boost::shared_ptr<IborIndex> index(new Euribor6M(termStructure));

        //Date todaysDate = index->fixingCalendar().adjust(Date(4,September,2005));
        Date todaysDate = index->fixingCalendar().adjust(Date(31,October,2001));
        Settings::instance().evaluationDate() = todaysDate;

        dates[0] = index->fixingCalendar().advance(todaysDate,
                                                   index->fixingDays(), Days);

        termStructure.linkTo(boost::shared_ptr<YieldTermStructure>(
                                    //new ZeroCurve(dates, rates, dayCounter)));
            new InterpolatedForwardCurve<Cubic>(dates, rates, dayCounter)));

        return index;
    }

    /*
    boost::shared_ptr<IborIndex> makeIndex() {
        std::vector<Date> dates;
        std::vector<Rate> rates;
        //dates.push_back(Date(4,September,2005));
        //dates.push_back(Date(4,September,2018));
        //rates.push_back(0.039);
        //rates.push_back(0.041);
        dates.push_back(Date(31,October,2001));
        dates.push_back(Date(31,October,2011));
        rates.push_back(0.00075);
        rates.push_back(0.02750);

        return makeIndex(dates, rates);
    }
    */

    /*
    boost::shared_ptr<OptionletVolatilityStructure>
    makeCapVolCurve(const Date& todaysDate) {
        Volatility vols[] = {14.40, 17.15, 16.81, 16.64, 16.17,
                             15.78, 15.40, 15.21, 14.86};

        std::vector<Date> dates;
        std::vector<Volatility> capletVols;
        boost::shared_ptr<LiborForwardModelProcess> process(
                               new LiborForwardModelProcess(10, makeIndex()));

        for (Size i=0; i < 9; ++i) {
            capletVols.push_back(vols[i]/100);
            dates.push_back(process->fixingDates()[i+1]);
        }

        return boost::shared_ptr<CapletVarianceCurve>(
                         new CapletVarianceCurve(todaysDate, dates,
                                                 capletVols, Actual360()));
    }
    */

    void IshiyamaLMMCalibration(){
        LARGE_TITLE(" Testing calibration of a Libor forward model ");

        SavedSettings backup;

        const Size size = 20;
        const Real tolerance = 8e-3;

        /*
            1. Time dependent volatilities of forward LIBOR rates: lambdak(t)
            2. Correlation of forward LIBOR rates
        */

        /*
        Volatility capVols[] = {0.145708,0.158465,0.166248,0.168672,
            0.169007,0.167956,0.166261,0.164239,
            0.162082,0.159923,0.157781,0.155745,
            0.153776,0.151950,0.150189,0.148582,
            0.147034,0.145598,0.144248}; //19
        */
        // Ishiyama (2001) Table 5
        Volatility capVols[] = {
                                1.530,
                                1.400,
                                1.265,
                                1.130,
                                1.005,
                                0.880,
                                0.838,
                                0.795,
                                0.730,
                                0.665,
                                0.618,
                                0.570,
                                0.528,
                                0.485,
                                0.480,
                                0.475,
                                0.458,
                                0.440,
                                0.420,
                                0.400}; //20


        /*
        Volatility swaptionVols[] = {0.170595, 0.166844, 0.158306, 0.147444,
            0.136930, 0.126833, 0.118135, 0.175963,
            0.166359, 0.155203, 0.143712, 0.132769,
            0.122947, 0.114310, 0.174455, 0.162265,
            0.150539, 0.138734, 0.128215, 0.118470,
            0.110540, 0.169780, 0.156860, 0.144821,
            0.133537, 0.123167, 0.114363, 0.106500,
            0.164521, 0.151223, 0.139670, 0.128632,
            0.119123, 0.110330, 0.103114, 0.158956,
            0.146036, 0.134555, 0.124393, 0.115038,
            0.106996, 0.100064}; //42
            */

        // Ishiyama (2001) Table 10
        Volatility swaptionVols[] = {
            0.8240,	0.5660,	0.5310,	0.4900,	0.4490,	0.4225,	0.3960,	0.3633,	0.3307,	0.2980,
            0.5220,	0.4770,	0.4520,	0.4110,	0.3760,	0.3385,	0.3010,	0.2863,	0.2717,	0.2570,
            0.4480,	0.4150,	0.3690,	0.3350,	0.3150,	0.2875,	0.2600,	0.2485,	0.2370,	0.2250,
            0.4210,	0.3620,	0.3200,	0.3330,	0.3070,	0.2668,	0.2265,	0.2175,	0.2075,	0.1995,
            0.3520,	0.2900,	0.2630,	0.2360,	0.2140,	0.2050,	0.1960,	0.1920,	0.1880,	0.1840,
            0.2980,	0.2513,	0.2350,	0.2158,	0.1985,	0.1925,	0.1865,	0.1834,	0.1803,	0.1773,
            0.2440,	0.2125,	0.2070,	0.1955,	0.1830,	0.1800,	0.1770,	0.1748,	0.1727,	0.1705,
            0.2313,	0.2023,	0.2000,	0.1888,	0.1753,	0.1723,	0.1693,	0.1675,	0.1657,	0.1638,
            0.2187,	0.1922,	0.1930,	0.1822,	0.1677,	0.1647,	0.1617,	0.1602,	0.1587,	0.1572,
            0.2060,	0.1820,	0.1860,	0.1755,	0.1600,	0.1570,	0.1540,	0.1528,	0.1517,	0.1505}; //10x10

        std::cout << "\ncapVols" << std::endl;
        for (Size i=0; i<LENGTH(capVols); ++i) {
            //std::cout << capVols[i] << std::endl;
        }
        std::cout << "\nswaptionVols" << std::endl;
        for (Size i=0; i<LENGTH(swaptionVols); ++i) {
            //std::cout << swaptionVols[i] << std::endl;
        }

        Date today(Date(31,October,2001));


        std::cout << "\nForward libor term structure" << std::endl;
        Real fwdLIBOR[] = {0.09, 0.12, 0.17, 0.22, 0.35, 0.44, 0.63, 0.74, 0.98, 1.12,
                           1.37, 1.53, 1.80, 1.98, 2.18, 2.36, 2.45, 2.62, 2.60, 2.75,
                           2.90};//20 21
        //boost::shared_ptr<IborIndex> index = makeIndex();
        boost::shared_ptr<IborIndex> index(new Euribor6M());
        std::vector<Date> dates;
        std::vector<Rate> rates;
        for (Size i=0; i<LENGTH(fwdLIBOR); ++i) {
            dates.push_back(index->fixingCalendar().advance(today, 6*(i+1), Months, ModifiedFollowing));
            rates.push_back((fwdLIBOR[i]/100.0));
            std::cout << std::setw(3) << ((i+1)*0.5) << ": "
                      << std::setw(15) << dates[i]
                      << std::setw(12) << io::rate(rates[i])
                      << std::endl;
        }
        index = makeIndex(dates, rates);

        /*
        Date dd = today;
        for(Size i=0; i<LENGTH(fwdLIBOR)-1; ++i) {
            dd = index->fixingCalendar().advance(today, 6*(i+1), Months, ModifiedFollowing);
            std::cout << std::setw(3) << ((i+1)*0.5) << ": "
                      << std::setw(15) << dd
                      << std::setw(12) << io::rate(index->fixing(dd, true))
                      << std::endl;
        }
        */


        boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, index));
        Handle<YieldTermStructure> termStructure = index->forwardingTermStructure();




        // set-up the model
        // Rebonato ||lambdak(t)||=g(t,x)=g(x)=(a+bx)exp(-cx)+d
        // sigma_i(t)=k_i*((a*(T_{i}-t)+d)*e^{-b(T_{i}-t)}+c)
        const Real a=0.1; //0.2 0.5
        const Real b=0.1; //0.1 0.6
        const Real c=0.1; //2.1 0.1
        const Real d=0.1; //0.3 0.3
        boost::shared_ptr<LmVolatilityModel> volaModel(
            new LmExtLinearExponentialVolModel(process->fixingTimes(), a, b, c, d));
        std::cout << "\nCaplet Volatility" << std::endl;
        for (Size t=0; t<size/2; ++t) {
            std::cout << t << ": " << volaModel->volatility(t) << std::endl;
        }

        // rho_{i,j}=rho + (1-rho)*e^{(-\beta \|i-j\|)}
        const Real rho = 0.01; //0.5
        const Real beta = 0.5; //0.5
        boost::shared_ptr<LmCorrelationModel> corrModel(
            new LmLinearExponentialCorrelationModel(size, rho, beta));
        // lmexpcorrmodel.cpp
        std::cout << "\nForward Libor Correlation" << std::endl;
        std::cout << corrModel->correlation(0) << std::endl;
        /*
        std::cout << "sqrt(correlation)" << std::endl;
        std::cout << corrModel->pseudoSqrt(0) << std::endl;
        std::cout << "sqrt(correlation)^T*sqrt(correlation)" << std::endl;
        std::cout << (corrModel->pseudoSqrt(0))*transpose(corrModel->pseudoSqrt(0)) << std::endl;
        */

        boost::shared_ptr<LiborForwardModel> model(
            new LiborForwardModel(process, volaModel, corrModel));
        const Array initparams = model->params();

        Size swapVolIndex = 0;
        DayCounter dayCounter=index->forwardingTermStructure()->dayCounter();

        // set-up calibration helper
        std::vector<boost::shared_ptr<CalibrationHelper> > calibrationHelper;

        Size i;
        for (i=0; i < size; ++i) { // 0,1,...,19 n=20 (i=2; i < size; ++i)
            const Period maturity = (i+1)*index->tenor(); // i*6M
            Handle<Quote> capVol(
                boost::shared_ptr<Quote>(new SimpleQuote(capVols[i])));// 0,1,...,19 n=20 (capVols[i-2])

            boost::shared_ptr<CalibrationHelper> caphelper(
                new CapHelper(maturity, capVol, index, Annual,
                index->dayCounter(), true, termStructure,
                CalibrationHelper::ImpliedVolError));

            caphelper->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new AnalyticCapFloorEngine(model, termStructure)));

            calibrationHelper.push_back(caphelper);

            if (i < size/2-1) { // 0,1,...,8 (i<= size/2)
                // add a few swaptions to test swaption calibration as well
                for (Size j=0; j < (size/2-i-1); ++j) { // 0,1,...,8
                    //const Period len = (j+1)*index->tenor(); // j*6M
                    const Period leni(i+1, Years); // 1,2,...,9 years swap tenor
                    const Period lenj(j+1, Years); // 1,2,...,9 years swaption tenor
                    Handle<Quote> swaptionVol(
                        boost::shared_ptr<Quote>(
                        new SimpleQuote(swaptionVols[i+10*j/*swapVolIndex++*/])));
                    std::cout << "swaptionVols[" << j << ", " << i << "] = " << swaptionVols[i+10*j] << std::endl;

                    boost::shared_ptr<CalibrationHelper> swaptionHelper(
                        new SwaptionHelper(maturity, lenj, swaptionVol, index,
                        leni/*index->tenor()*/, dayCounter,
                        index->dayCounter(),
                        termStructure,
                        CalibrationHelper::ImpliedVolError));

                    swaptionHelper->setPricingEngine(
                        boost::shared_ptr<PricingEngine>(
                        new LfmSwaptionEngine(model,termStructure)));

                    calibrationHelper.push_back(swaptionHelper);
                }
            }
        }

        /*
        boost::shared_ptr<LiborForwardModel>(
            boost::shared_ptr<LiborForwardModelProcess>, //boost::shared_ptr<LiborForwardModelProcess> process(new LiborForwardModelProcess(size, index));
            boost::shared_ptr<LmVolatilityModel>, //LmExtLinearExponentialVolModel(process->fixingTimes(),0.5,0.6,0.1,0.1));
            boost::shared_ptr<LmCorrelationModel> //LmLinearExponentialCorrelationModel(size, 0.5, 0.8));
            ))->calibrate(
                std::vector<boost::shared_ptr<CalibrationHelper> >,
                LevenbergMarquardt(1e-6, 1e-6, 1e-6),
                EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
        */
std::cout << "stopped here" << std::endl;
        LevenbergMarquardt om(1e-6, 1e-6, 1e-6);
        model->calibrate(calibrationHelper, om, EndCriteria(2000, 100, 1e-6, 1e-6, 1e-6));
        const Array calibparams = model->params();
std::cout << "stopped here2" << std::endl;
        std::cout.precision(10);

        // measure the calibration error
        std::cout << "\nCalibration Helper Parameters (After Calibration)" << std::endl;
        std::cout << "Market Value\tCalibration Error" << std::endl;
        Real calculated = 0.0;
        for (i=0; i<calibrationHelper.size(); ++i) {
            Real diff = calibrationHelper[i]->calibrationError();
            calculated += diff*diff;

            //std::cout << calibrationHelper[i]->marketValue() << std::endl; //i << ": " <<
            std::cout << i << ": " << calibrationHelper[i]->marketValue() << "\ts" << calibrationHelper[i]->calibrationError() << std::endl;
        }
        if (std::sqrt(calculated) > tolerance)
            BOOST_ERROR("Failed to calibrate libor forward model"
            << "\n    calculated diff: " << std::sqrt(calculated)
            << "\n    expected : smaller than  " << tolerance);

        const char paramnames[24][64] = {"a","b","c","d",
                             "sigma1","sigma2","sigma3","sigma4","sigma5",
                             "sigma6","sigma7","sigma8","sigma9","sigma10",
                             "sigma11","sigma12","sigma13","sigma14",
                             "sigma15","sigma16","sigma17","sigma18", //forward libor volatilities: sigma
                             "rho","beta"};  //4+18+2=24

        std::cout << "\nParameters" << std::endl;
        std::cout << std::setw(3) << "i" << std::setw(10) << "Parameter" << std::setw(12) << "Initial" << std::setw(25) << "Calibrated" << std::endl;
        for (Size i=0; i<20/*calibparams.size()*/; ++i) {
            std::cout << std::setw(3) << i << std::setw(10) << paramnames[i]
            << std::setw(12) << initparams[i] << std::setw(25) << calibparams[i] << std::endl;
        }
        std::cout << "\nEstimated Forward LIBOR Volatilities" << std::endl;
        for (Size i=4; i<size+4; ++i) { //calibparams.size()=4+18+2=24
            std::cout << std::setw(3) << ((i-4.0)/2) << std::setw(10) << paramnames[i] << std::setw(18) << calibparams[i] << std::endl;
        }


        //******************************************************************************
        // Repricing caps and swaptions using the calibrated parameters
        //******************************************************************************

        std::cout << "\n\nRepricing caps and swaptions using the calibrated parameters" << std::endl;
        boost::shared_ptr<LmVolatilityModel> volaModelOpt(
                new LmExtLinearExponentialVolModel(process->fixingTimes(), calibparams[0], calibparams[1], calibparams[2], calibparams[3]));
        boost::shared_ptr<LmCorrelationModel> corrModelOpt(
            //new LmLinearExponentialCorrelationModel(size, calibparams[19], calibparams[20]));
            new LmLinearExponentialCorrelationModel(size, calibparams[22], calibparams[23]));

        std::cout << "rho =  calibparams[22] = " << calibparams[22] << std::endl;
        std::cout << "beta = calibparams[23] = " << calibparams[23] << std::endl;

        /*boost::shared_ptr<LiborForwardModelProcess> process(
            new LiborForwardModelProcess(size, makeIndex()));*/
        process->setCovarParam(boost::shared_ptr<LfmCovarianceParameterization>(
            new LfmCovarianceProxy(volaModelOpt, corrModelOpt)));
        boost::shared_ptr<LiborForwardModel> liborModelOpt(
            new LiborForwardModel(process, volaModelOpt, corrModelOpt));

        Calendar calendar = index->fixingCalendar();
        //DayCounter dayCounter = index->forwardingTermStructure()->dayCounter();
        BusinessDayConvention convention = index->businessDayConvention();
        Date settlement  = index->forwardingTermStructure()->referenceDate();

        //boost::shared_ptr<SwaptionVolatilityMatrix> volaMatOpt =
        //liborModelOpt->getSwaptionVolatilityMatrix();
        //std::cout << volaModelOpt->volatility(0.0) << std::endl;
        //std::cout << corrModelOpt->correlation(0.0) << std::endl;
        std::cout.precision(4);
        std::cout << "\nForward LIBOR volatility matrix recovered from cap/swaption volatilities" << std::endl;
        boost::shared_ptr<LfmCovarianceProxy> covarProxyOpt(
            new LfmCovarianceProxy(volaModelOpt, corrModelOpt));
        std::cout << "Forward LIBOR proxy volatility at time 0" << std::endl;
        std::cout << volaModelOpt->volatility(0) << std::endl << std::endl;
        std::cout << "Forward LIBOR proxy covariance matrix at time 0" << std::endl;
        std::cout << covarProxyOpt->covariance(0) << std::endl << std::endl;
        for (Real t=0; t<4.6; t+=0.31) { //14*0.31=4.34<4.60
            ////std::cout << "Forward LIBOR proxy volatility at time " << t << std::endl;
            ////std::cout << covarProxyOpt->covariance(t) << std::endl;
            ////std::cout << "Forward LIBOR proxy volatility matrix at time " << t << std::endl;
            ////std::cout << volaModelOpt->volatility(t) << std::endl;
        }

        std::cout << "\nSwaption NPV using calibrated forward LIBOR vols/corrs parameters" << std::endl;
        for (i=1; i < size; ++i) { //i=1,2,,,18
            for (Size j=1; j <= size-i; ++j) { //j=1~18,1~17,1-9,1-8,...1-2,1
                Date fwdStart    = settlement + Period(6*i, Months);
                Date fwdMaturity = fwdStart + Period(6*j, Months);

                Schedule schedule(fwdStart, fwdMaturity, index->tenor(), calendar,
                    convention, convention, DateGeneration::Forward, false);

                Rate swapRate  = 0.0404;
                boost::shared_ptr<VanillaSwap> forwardSwap(
                    new VanillaSwap(VanillaSwap::Receiver, 1.0,
                    schedule, swapRate, dayCounter,
                    schedule, index, 0.0, index->dayCounter()));
                forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
                    new DiscountingSwapEngine(index->forwardingTermStructure())));

                swapRate = forwardSwap->fairRate();
                forwardSwap = boost::shared_ptr<VanillaSwap>(
                    new VanillaSwap(VanillaSwap::Receiver, 1.0,
                    schedule, swapRate, dayCounter,
                    schedule, index, 0.0, index->dayCounter()));
                forwardSwap->setPricingEngine(boost::shared_ptr<PricingEngine>(
                    new DiscountingSwapEngine(index->forwardingTermStructure())));

                if (i == j && i<=size) { //i=j=1,2,3,4,5,6,7,...,14
                    boost::shared_ptr<PricingEngine> engine(
                        new LfmSwaptionEngine(liborModelOpt,
                        index->forwardingTermStructure()));
                    boost::shared_ptr<Exercise> exercise(
                        new EuropeanExercise(process->fixingDates()[i]));

                    boost::shared_ptr<Swaption> swaption( //Swaption
                        new Swaption(forwardSwap, exercise));
                    swaption->setPricingEngine(engine);

                    Real swaptionNPV = swaption->NPV();
                    //std::cout << swaption->NPV() << std::endl;
                    std::cout << swaption->impliedVolatility(swaptionNPV,index->forwardingTermStructure(),0.15) << std::endl;
                }
            }
        }
    }


    void LMMTest() {

        /*
        // rebate that does nothing, need it because some rebate is expected
        // when you break a swap nothing happens.
        NothingExerciseValue nullRebate(rateTimes);
        CallSpecifiedMultiProduct dummyProduct =
            CallSpecifiedMultiProduct(receiverSwap, naifStrategy,
            ExerciseAdapter(nullRebate));
        EvolutionDescription evolution = dummyProduct.evolution();

        Size numberRates =20;
        Real volLevel = 0.11;

        std::vector<Volatility> volatilities(numberRates, volLevel);
        boost::shared_ptr<PiecewiseConstantCorrelation> fwdcorr(
                    new  ExponentialForwardCorrelation(correlations));

        FlatVol calibration(
                    volatilities,
                    fwdcorr,
                    evolution,
                    numberOfFactors,
                    initialRates,
                    displacements);

        boost::shared_ptr<MarketModel> marketModel(new FlatVol(calibration));

        // we use a factory since there is data that will only be known later
        SobolBrownianGeneratorFactory generatorFactory(
            SobolBrownianGenerator::Diagonal, seed);

        std::vector<Size> numeraires( moneyMarketMeasure(evolution));

        // the evolver will actually evolve the rates
        LogNormalFwdRatePc evolver(marketModel,
                                    generatorFactory,
                                    numeraires);  // numeraires for each step

        boost::shared_ptr<MarketModelEvolver> evolverPtr(
                    new LogNormalFwdRatePc(evolver));

        boost::shared_ptr<MarketModelEvolver> evolverEuler(
                    new LogNormalFwdRateEuler(marketModel,
                                            generatorFactory,
                                            numeraires));
        */



    }

}

//----------------------------------------------------------------------------------------


int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;


        /*********************
         ***     TESTS     ***
         *********************/


        {
            //LARGE_TITLE("Libor Market Model");
            //using namespace NamespaceLiborMarketModelTest;
            //LiborMarketModelTest::testSimpleCovarianceModels();
            //LiborMarketModelTest::testCapletPricing();
            //LiborMarketModelTest::testCalibration();
            //LiborMarketModelTest::testSwaptionPricing();
            //LiborMarketModelTest::mylmmtest();
            //LiborMarketModelTest::testIshiyamaLMM();
        }

        /*

        {
            LARGE_TITLE("Libor Market Model Process");
            using namespace NamespaceLiborMarketModelProcessTest;
            LiborMarketModelProcessTest::testInitialisation();
            LiborMarketModelProcessTest::testLambdaBootstrapping();
            LiborMarketModelProcessTest::testMonteCarloCapletPricing();
        }
        */


        {
            LARGE_TITLE("My Libor Market Model Test");
            using namespace MyLMMTest;
            IshiyamaLMMCalibration();
            //LMMTest();

        }





        cout << endl << endl;

        Real seconds  = timer.elapsed();
        Integer hours = Integer(seconds/3600);
        seconds -= hours * 3600;
        Integer minutes = Integer(seconds/60);
        seconds -= minutes * 60;
        cout << "Run completed in ";
        if (hours > 0)
            cout << hours << " h ";
        if (hours > 0 || minutes > 0)
            cout << minutes << " m ";
        cout << fixed << setprecision(0)
             << seconds << " s" << endl;

        return 0;
    } catch (exception& e) {
        cerr << e.what() << endl;
        return 1;
    } catch (...) {
        cerr << "unknown error" << endl;
        return 1;
    }
}

