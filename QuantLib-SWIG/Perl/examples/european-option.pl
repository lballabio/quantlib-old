#!/usr/bin/perl

use QuantLib;
use strict;

my($todaysDate) = new QuantLib::Date(15, $QuantLib::May, 1998);
QuantLib::Settings::instance()->setEvaluationDate($todaysDate);
my($settlementDate) = new QuantLib::Date(17, $QuantLib::May, 1998);
my($riskFreeRate) = new QuantLib::FlatForward($settlementDate,
                                              0.05,
                                              new QuantLib::Actual365Fixed);

my($exercise) = new QuantLib::EuropeanExercise(
                                 new QuantLib::Date(17,$QuantLib::May, 1999));
my($payoff) = new QuantLib::PlainVanillaPayoff($QuantLib::Option::Put, 8.0);

my($underlying) = new QuantLib::SimpleQuote(7.0);
my($volatility) = new QuantLib::BlackConstantVol($todaysDate,
                                                 0.10,
                                                 new QuantLib::Actual365Fixed);
my($dividendYield) = new QuantLib::FlatForward($settlementDate,
                                               0.05,
                                               new QuantLib::Actual365Fixed);

my($process) = new QuantLib::BlackScholesProcess(
                      new QuantLib::QuoteHandle($underlying),
                      new QuantLib::YieldTermStructureHandle($dividendYield),
                      new QuantLib::YieldTermStructureHandle($riskFreeRate),
                      new QuantLib::BlackVolTermStructureHandle($volatility));

my($option) = new QuantLib::VanillaOption($process, $payoff, $exercise);

$option->setPricingEngine(new QuantLib::AnalyticEuropeanEngine);
print "analytic: ", $option->NPV(), "\n";

$option->setPricingEngine(new QuantLib::IntegralEngine);
print "integral: ", $option->NPV(), "\n";

