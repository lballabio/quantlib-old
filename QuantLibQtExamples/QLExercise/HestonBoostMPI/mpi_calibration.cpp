/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Klaus Spanderen

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.

 Parts are derived from quantlib test-suite/hestonmodel.cpp

 compile:
g++ -I. -I/usr/lib/mpich/include -I/home/spanderen/workspace/QuantLib -O3 -o mpi_calibration mpi_calibration.cpp mpihestonmmodelhelper.o -L/home/spanderen/workspace/QuantLib/ql/.libs -lQuantLib -lboost_thread -lboost_mpi
*/

#include <ql/time/period.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/processes/hestonprocess.hpp>
#include <ql/models/equity/hestonmodel.hpp>
#include <ql/models/equity/hestonmodelhelper.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/termstructures/yield/zerocurve.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/pricingengines/vanilla/analytichestonengine.hpp>
#include <ql/pricingengines/vanilla/fdhestonvanillaengine.hpp>

#include <boost/mpi.hpp>
#include <iostream>

#include "mpicalibrationhelper.hpp"

using namespace QuantLib;

namespace {

    struct CalibrationMarketData {
        Handle<Quote> s0;
        Handle<YieldTermStructure> riskFreeTS, dividendYield;
        std::vector<boost::shared_ptr<CalibrationHelper> > options;
    };

    CalibrationMarketData getDAXCalibrationMarketData() {
        /* this example is taken from A. Sepp
           Pricing European-Style Options under Jump Diffusion Processes
           with Stochstic Volatility: Applications of Fourier Transform
           http://math.ut.ee/~spartak/papers/stochjumpvols.pdf
        */

        Date settlementDate(Settings::instance().evaluationDate());
        
        DayCounter dayCounter = Actual365Fixed();
        Calendar calendar = TARGET();
        
        Integer t[] = { 13, 41, 75, 165, 256, 345, 524, 703 };
        Rate r[] = { 0.0357,0.0349,0.0341,0.0355,0.0359,0.0368,0.0386,0.0401 };
        
        std::vector<Date> dates;
        std::vector<Rate> rates;
        dates.push_back(settlementDate);
        rates.push_back(0.0357);
        Size i;
        for (i = 0; i < 8; ++i) {
            dates.push_back(settlementDate + t[i]);
            rates.push_back(r[i]);
        }

        Handle<YieldTermStructure> riskFreeTS(
            boost::shared_ptr<YieldTermStructure>(
                new ZeroCurve(dates, rates, dayCounter)));
        
        Handle<YieldTermStructure> dividendYield(
			new FlatForward(settlementDate, 0.0, dayCounter));
        
        Volatility v[] =
          { 0.6625,0.4875,0.4204,0.3667,0.3431,0.3267,0.3121,0.3121,
            0.6007,0.4543,0.3967,0.3511,0.3279,0.3154,0.2984,0.2921,
            0.5084,0.4221,0.3718,0.3327,0.3155,0.3027,0.2919,0.2889,
            0.4541,0.3869,0.3492,0.3149,0.2963,0.2926,0.2819,0.2800,
            0.4060,0.3607,0.3330,0.2999,0.2887,0.2811,0.2751,0.2775,
            0.3726,0.3396,0.3108,0.2781,0.2788,0.2722,0.2661,0.2686,
            0.3550,0.3277,0.3012,0.2781,0.2781,0.2661,0.2661,0.2681,
            0.3428,0.3209,0.2958,0.2740,0.2688,0.2627,0.2580,0.2620,
            0.3302,0.3062,0.2799,0.2631,0.2573,0.2533,0.2504,0.2544,
            0.3343,0.2959,0.2705,0.2540,0.2504,0.2464,0.2448,0.2462,
            0.3460,0.2845,0.2624,0.2463,0.2425,0.2385,0.2373,0.2422,
            0.3857,0.2860,0.2578,0.2399,0.2357,0.2327,0.2312,0.2351,
            0.3976,0.2860,0.2607,0.2356,0.2297,0.2268,0.2241,0.2320 };
        
        Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(4468.17)));
        Real strike[] = { 3400,3600,3800,4000,4200,4400,
                          4500,4600,4800,5000,5200,5400,5600 };
        
        std::vector<boost::shared_ptr<CalibrationHelper> > options;
        
        for (Size s = 0; s < 13; ++s) {
            for (Size m = 0; m < 8; ++m) {
                const Integer id = s*8+m;
                const Handle<Quote> vol(boost::shared_ptr<Quote>(
                    new SimpleQuote(v[id])));
        
                const Period maturity((int)((t[m]+3)/7.), Weeks); 

                const boost::shared_ptr<CalibrationHelper> hestonHelper(
                    new HestonModelHelper(maturity, calendar,
                                          s0->value(), strike[s], vol,
                                          riskFreeTS, dividendYield));
                                                                  
                options.push_back(boost::shared_ptr<CalibrationHelper>(
                     new MPICalibrationHelper(
                         id, vol, riskFreeTS, hestonHelper,
                         CalibrationHelper::ImpliedVolError)));
            }
        }
        
        CalibrationMarketData marketData
		    = { s0, riskFreeTS, dividendYield, options };
        
        return marketData;
    }        
}

int main(int argc, char* argv[])
{
    boost::mpi::environment env(argc, argv);

    Date settlementDate(5, July, 2002);
    Settings::instance().evaluationDate() = settlementDate;

    CalibrationMarketData marketData = getDAXCalibrationMarketData();
    
    const Handle<YieldTermStructure> riskFreeTS = marketData.riskFreeTS;
    const Handle<YieldTermStructure> dividendTS = marketData.dividendYield;
    const Handle<Quote> s0 = marketData.s0;

    const std::vector<boost::shared_ptr<CalibrationHelper> > options
        = marketData.options;

    const Real v0   = 0.1;
    const Real kappa= 1.0;
    const Real theta= 0.1;
    const Real sigma= 0.5;
    const Real rho  =-0.5;

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
        riskFreeTS, dividendTS, s0, v0, kappa, theta, sigma, rho));

    boost::shared_ptr<HestonModel> model(new HestonModel(process));

    boost::shared_ptr<PricingEngine> engine(
        new FdHestonVanillaEngine(model, 20, 100, 50));

    for (Size i = 0; i < options.size(); ++i) {
        // explicit cast because setPricingEngine is non virtual
        boost::dynamic_pointer_cast<MPICalibrationHelper>(options[i])
            ->setPricingEngine(engine);
    }

    LevenbergMarquardt om(1e-8, 1e-8, 1e-8);
    model->calibrate(options, om, 
					 EndCriteria(400, 40, 1.0e-8, 1.0e-8, 1.0e-8));

    Real sse = 0;
    for (Size i = 0; i < 13*8; ++i) {
        const Real diff = options[i]->calibrationError()*100.0;
        sse += diff*diff;
    }

    std::cout << "sse: " << sse << std::endl;

	return 0;
}
