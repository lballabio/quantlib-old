#!/usr/bin/perl

use QuantLib;
package QuantLibc;

my($todaysDate) = new_Date(15, 5, 1998);
my($settlementDate) = new_Date(17, 5, 1998);
my($riskFreeRate) = new_FlatForward($settlementDate,
				    0.05,
				    new_Actual365Fixed);

my($exercise) = new_EuropeanExercise(new_Date(17,5,1999));
my($payoff) = new_PlainVanillaPayoff(Option_Call, 8.0);

my($underlying) = new_SimpleQuote(7.0);
my($volatility) = new_BlackConstantVol($todaysDate, 
				       0.10,
				       new_Actual365Fixed());
my($dividendYield) = new_FlatForward($settlementDate,
				     0.05, new_Actual365Fixed());
