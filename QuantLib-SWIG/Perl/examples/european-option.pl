#!/usr/bin/perl

use QuantLib;
package QuantLibc;
use strict;

my($todaysDate) = new_Date(15, $QuantLibc::May, 1998);
Settings_setEvaluationDate(Settings_instance(), $todaysDate);
my($settlementDate) = new_Date(17, $QuantLibc::May, 1998);
my($riskFreeRate) = new_FlatForward($settlementDate,
				    0.05,
				    new_Actual365Fixed);

my($exercise) = new_EuropeanExercise(new_Date(17,$QuantLibc::May, 1999));
my($payoff) = new_PlainVanillaPayoff($QuantLibc::Option_Put, 8.0);

my($underlying) = new_SimpleQuote(7.0);
my($volatility) = new_BlackConstantVol($todaysDate, 
				       0.10,
				       new_Actual365Fixed());
my($dividendYield) = new_FlatForward($settlementDate,
				     0.05, new_Actual365Fixed());

my($process) = 
    new_BlackScholesProcess(new_QuoteHandle($underlying),
			    new_YieldTermStructureHandle($dividendYield),
			    new_YieldTermStructureHandle($riskFreeRate),
			    new_BlackVolTermStructureHandle($volatility));

my($option) =
    new_VanillaOption($process, $payoff, $exercise);

$option->setPricingEngine(new_AnalyticEuropeanEngine());
print "analytic ", $option->NPV(), "\n";

$option->setPricingEngine(new_IntegralEngine());
print "integral ", $option->NPV(), "\n";
