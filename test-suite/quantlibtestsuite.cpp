
/*
 Copyright (C) 2003 RiskMap srl

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
// $Id$

#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/TestResult.h>
#include <iostream>
#include <string>
#include "qltestlistener.hpp"
#include "capfloor.hpp"
#include "covariance.hpp"
#include "dates.hpp"
#include "daycounters.hpp"
#include "distributions.hpp"
#include "europeanoption.hpp"
#include "instruments.hpp"
#include "integrals.hpp"
#include "marketelements.hpp"
#include "operators.hpp"
#include "piecewiseflatforward.hpp"
#include "riskstats.hpp"
#include "solvers.hpp"
#include "stats.hpp"
#include "swap.hpp"

int main() {
    CppUnit::TextUi::TestRunner runner;
    QLTestListener qlListener;
    runner.eventManager().addListener(&qlListener);

    runner.addTest(CapFloorTest::suite());
    runner.addTest(new CovarianceTest);
    runner.addTest(new DateTest);
    runner.addTest(new DayCounterTest);
    runner.addTest(new DistributionTest);
    runner.addTest(EuropeanOptionTest::suite());
    runner.addTest(InstrumentTest::suite());
    runner.addTest(new IntegralTest);
    runner.addTest(MarketElementTest::suite());
    runner.addTest(new OperatorTest);
    runner.addTest(new PiecewiseFlatForwardTest);
    runner.addTest(new RiskStatisticsTest);
    runner.addTest(new Solver1DTest);
    runner.addTest(new StatisticsTest);
    runner.addTest(SimpleSwapTest::suite());

    std::string header = "Testing QuantLib " QL_VERSION ;
    std::cerr << std::string(header.length(),'=') << std::endl;
    std::cerr << header << std::endl;
    std::cerr << std::string(header.length(),'=');
    std::cerr.flush();
    bool succeeded = runner.run();
    return succeeded ? 0 : 1;
}
