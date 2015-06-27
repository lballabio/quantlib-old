/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

#include <ql/quantlib.hpp>
#include <boost/timer.hpp>

using namespace QuantLib;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {
Integer sessionId() { return 0; }
}
#endif

class Timer {
    boost::timer timer_;
    double elapsed_;

  public:
    void start() { timer_ = boost::timer(); }
    void stop() { elapsed_ = timer_.elapsed(); }
    double elapsed() const { return elapsed_; }
};

// reusable code snipped to perform one pricing step
#define PROXY_PRICING \
        npvRef = swaptionRef2->NPV(); \
        timer.start(); \
        npvProxy = swaption2->NPV(); \
        /*underlyingProxy = 0.0; swaption2->result<Real>("exerciseValue");*/ \
        timer.stop(); \
        npvProxyTiming = timer.elapsed(); \
        std::clog << "\nPricing results on " \
                  << Settings::instance().evaluationDate() \
                  << ", reference rate " << rateLevelRefQuote->value() \
                  << " with maturity " << maturityRefQuote->value() << "\n"; \
        std::clog << "Integral engine npv = " << npvRef << "\n"; \
        std::clog << "Proxy    engine npv = " << npvProxy \
                  << " (timing: " << npvProxyTiming*1000000.0 \
        << "mus)" /*<< ", underlying npv = " << underlyingProxy*/ << "\n";
// here the main part of the code starts

int main(int argc, char *argv[]) {

    try {

        std::clog << "Bermudan swaption proxy pricing example\n";

        // Timer seutp

        Timer timer;

        // Original evaluation date and rate level

        Real rateLevelOrig = 0.02;
        Date refDateOrig(12, January, 2015);

        Settings::instance().evaluationDate() = refDateOrig;

        // the yield term structure for the original pricing
        // this must _not_ be floating, see the warning in
        // the proxy engine's header.

        Handle<YieldTermStructure> ytsOrig(boost::make_shared<FlatForward>(
            refDateOrig, rateLevelOrig, Actual365Fixed()));

        // the yield term structure for reference pricings
        // (integral engine) on future dates, this _floating_

        boost::shared_ptr<SimpleQuote> rateLevelRefQuote =
            boost::make_shared<SimpleQuote>(0.02);
        Handle<Quote> rateLevelRef(rateLevelRefQuote);
        Handle<YieldTermStructure> ytsRef(boost::make_shared<FlatForward>(
            0, TARGET(), rateLevelRef, Actual365Fixed()));

        // the euribor index for the swaption's underlying.
        // one should again use the fixed yts ...
        boost::shared_ptr<IborIndex> euribor6m =
            boost::make_shared<Euribor>(6 * Months, ytsOrig);
        // but for the reference pricings in the integral engine
        // we need a floating version as well.
        boost::shared_ptr<IborIndex> euribor6mRef =
            boost::make_shared<Euribor>(6 * Months, ytsRef);

        // the length of the bermudan swaption in years

        Size length = 10;

        // instrument setup

        Real strike = 0.02; // near atm option
        Date effectiveDate = TARGET().advance(refDateOrig, 2 * Days);
        Date startDate = TARGET().advance(effectiveDate, 1 * Years);
        Date maturityDate = TARGET().advance(startDate, length * Years);

        Schedule fixedSchedule(startDate, maturityDate, 1 * Years, TARGET(),
                               ModifiedFollowing, ModifiedFollowing,
                               DateGeneration::Forward, false);
        Schedule floatingSchedule(startDate, maturityDate, 6 * Months, TARGET(),
                                  ModifiedFollowing, ModifiedFollowing,
                                  DateGeneration::Forward, false);

        boost::shared_ptr<VanillaSwap> underlying =
            boost::make_shared<VanillaSwap>(VanillaSwap(
                VanillaSwap::Payer, 1.0, fixedSchedule, strike, Thirty360(),
                floatingSchedule, euribor6m, 0.0, Actual360()));
        boost::shared_ptr<VanillaSwap> underlyingRef =
            boost::make_shared<VanillaSwap>(VanillaSwap(
                VanillaSwap::Payer, 1.0, fixedSchedule, strike, Thirty360(),
                floatingSchedule, euribor6mRef, 0.0, Actual360()));

        std::vector<Date> exerciseDates;
        for (Size i = 0; i < length; ++i) {
            exerciseDates.push_back(
                TARGET().advance(fixedSchedule[i], -2 * Days));
        }

        boost::shared_ptr<Exercise> exercise =
            boost::make_shared<BermudanExercise>(exerciseDates, false);

        boost::shared_ptr<Swaption> swaption =
            boost::make_shared<Swaption>(underlying, exercise);
        boost::shared_ptr<Swaption> swaptionRef =
            boost::make_shared<Swaption>(underlyingRef, exercise);

        // our instrument is a swaption, but the engine is for non standard
        // swaptions
        // so we just convert it

        boost::shared_ptr<NonstandardSwaption> swaption2 =
            boost::make_shared<NonstandardSwaption>(*swaption);
        boost::shared_ptr<NonstandardSwaption> swaptionRef2 =
            boost::make_shared<NonstandardSwaption>(*swaptionRef);

        // just take any model volatility and reversion, we do not calibrate
        // them here. Also they are flat, so no steps needed really.

        std::vector<Date> stepDates;
        std::vector<Real> sigmas(1, 0.0070);
        Real reversion = 0.0030;

        // the gsr model in T-forward measure, T=50 chosen arbitrary here
        // the first model uses the fixed yts, used for the mc pricing
        // generating the proxy

        boost::shared_ptr<Gsr> gsrFixed = boost::make_shared<Gsr>(
            ytsOrig, stepDates, sigmas, reversion, 50.0);

        // the second model is used for the reference pricing, therefore
        // using the floating yts

        boost::shared_ptr<Gsr> gsrFloating =
            boost::make_shared<Gsr>(ytsRef, stepDates, sigmas, reversion, 50.0);

        // the integral engine for reference pricings

        boost::shared_ptr<PricingEngine> integralEngine =
            boost::make_shared<Gaussian1dNonstandardSwaptionEngine>(
                gsrFloating, 64, 7.0, true, false, Handle<Quote>(), ytsRef);

        // compute a reference price for the inital pricing

        timer.start();
        swaption2->setPricingEngine(integralEngine);
        Real npvOrigIntegral = swaption2->NPV();
        timer.stop();
        Real npvOrigIntegralTiming = timer.elapsed();

        // the mc engine, note that we use the fixed model here

        boost::shared_ptr<PricingEngine> mcEngine =
            MakeMcGaussian1dNonstandardSwaptionEngine<>(gsrFixed)
                .withSteps(1) // the gsr model allows for large steps
                .withSamples(10000)
                .withSeed(42)
                .withCalibrationSamples(10000)
                .withProxy(true);

        // compute the mc price

        timer.start();
        swaption2->setPricingEngine(mcEngine);
        Real npvOrigMc = swaption2->NPV();
        Real errorOrigMc = swaption2->errorEstimate();
        timer.stop();
        Real npvOrigMcTiming = timer.elapsed();

        // output the results

        std::clog << "Pricing results on the original reference date ("
                  << refDateOrig << "):\n";
        std::clog << "Integral engine npv = " << npvOrigIntegral
                  << " (timing: " << npvOrigIntegralTiming*1000000.0 << "mus)\n";
        std::clog << "MC       engine npv = " << npvOrigMc << " error estimate "
                  << errorOrigMc << " (timing: " << npvOrigMcTiming*1000000.0 << "mus)\n";

        // proxy pricing, that is what this example is really about

        // reference maturity for the scenario rate

        boost::shared_ptr<SimpleQuote> maturityRefQuote =
            boost::make_shared<SimpleQuote>();
        Handle<Quote> maturityRef(maturityRefQuote);

        boost::shared_ptr<PricingEngine> proxyEngine =
            boost::make_shared<ProxyNonstandardSwaptionEngine>(
                swaption2->proxy(), rateLevelRef, maturityRef, 64, 7.0, false);

        Real npvRef, npvProxy, npvProxyTiming;

        swaptionRef2->setPricingEngine(integralEngine);
        swaption2->setPricingEngine(proxyEngine);

        // move forward by 6 months, to the middle of the first period

        Settings::instance().evaluationDate() = Date(12, June, 2015);
        rateLevelRefQuote->setValue(0.02); // no change
        maturityRefQuote->setValue(10.5);  // maturity of the underlying
        PROXY_PRICING;

        // move somewhere to the middle and check itm, otm, atm 

        Settings::instance().evaluationDate() = Date(11, June, 2019);
        rateLevelRefQuote->setValue(0.025); // in the money
        maturityRefQuote->setValue(6.5);  
        PROXY_PRICING;
        rateLevelRefQuote->setValue(0.02); // at the money
        PROXY_PRICING;
        rateLevelRefQuote->setValue(0.015); // out of the money
        PROXY_PRICING;

        // move to the beginning of a period

        Settings::instance().evaluationDate() = Date(11, January, 2020);
        rateLevelRefQuote->setValue(0.02); 
        maturityRefQuote->setValue(6.0);
        PROXY_PRICING;

        // move to the end of a period

        Settings::instance().evaluationDate() = Date(11, January, 2021);
        rateLevelRefQuote->setValue(0.02); 
        maturityRefQuote->setValue(5.0);
        PROXY_PRICING;

        // move to the last period

        Settings::instance().evaluationDate() = Date(11, June, 2024);
        rateLevelRefQuote->setValue(0.02); 
        maturityRefQuote->setValue(1.5);
        PROXY_PRICING;

        // check exercise

        // Settings::instance().evaluationDate() = Date(10, January, 2020);
        // rateLevelRefQuote->setValue(0.005);
        // maturityRefQuote->setValue(6.0);
        // npvProxy = swaption2->NPV();
        // underlyingProxy = swaption2->result<Real>("exerciseValue");
        // std::clog << "\nExercise check (" << Settings::instance().evaluationDate() << "):\n";
        // std::clog << "otm option: exercise value=" << underlyingProxy << " npv=" << npvProxy << std::endl;

        // rateLevelRefQuote->setValue(0.04);
        // npvProxy = swaption2->NPV();
        // underlyingProxy = swaption2->result<Real>("exerciseValue");
        // std::clog << "itm option: exercise value=" << underlyingProxy << " npv=" << npvProxy << std::endl;

        return 0;

    } catch (QuantLib::Error e) {
        std::clog << "terminated with a ql exception: " << e.what()
                  << std::endl;
        return 1;
    } catch (std::exception e) {
        std::clog << "terminated with a general exception: " << e.what()
                  << std::endl;
        return 1;
    }
}
