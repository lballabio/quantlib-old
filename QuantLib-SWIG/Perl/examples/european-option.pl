#!/usr/bin/perl

use QuantLib;
use strict;

my($todaysDate) = new QuantLib::Date(15, $QuantLib::May, 1998);
QuantLib::Settings::instance()->setEvaluationDate($todaysDate);
my($settlementDate) = new QuantLib::Date(17, $QuantLib::May, 1998);

my($dayCount) = new QuantLib::Actual365Fixed;

my($underlying) = new QuantLib::SimpleQuote(7.0);

my($riskFreeRate) = new QuantLib::FlatForward($settlementDate,
                                              0.05,
                                              $dayCount);

my($dividendYield) = new QuantLib::FlatForward($settlementDate,
                                               0.05,
                                               $dayCount);

my($calendar) = new QuantLib::TARGET;
my($volatility) = new QuantLib::BlackConstantVol($todaysDate,
                                                 $calendar,
                                                 0.10,
                                                 $dayCount);

my($u) = new QuantLib::QuoteHandle($underlying);
my($r) = new QuantLib::YieldTermStructureHandle($riskFreeRate);
my($q) = new QuantLib::YieldTermStructureHandle($dividendYield);
my($sigma) = new QuantLib::BlackVolTermStructureHandle($volatility);
my($process) = new QuantLib::BlackScholesMertonProcess($u, $q, $r, $sigma);

my($exerciseDate) = new QuantLib::Date(17,$QuantLib::May, 1999);
my($exercise) = new QuantLib::EuropeanExercise($exerciseDate);
my($payoff) = new QuantLib::PlainVanillaPayoff($QuantLib::Option::Put, 8.0);

my($option) = new QuantLib::VanillaOption($payoff, $exercise);

my($engine1) = new QuantLib::AnalyticEuropeanEngine($process);
$option->setPricingEngine($engine1);
print "analytic: ", $option->NPV(), "\n";

my($engine2) = new QuantLib::IntegralEngine($process);
$option->setPricingEngine($engine2);
print "integral: ", $option->NPV(), "\n";

