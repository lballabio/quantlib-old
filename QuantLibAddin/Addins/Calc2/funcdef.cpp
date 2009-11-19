
/*  
 Copyright (C) 2004, 2005, 2006, 2007, 2008 Eric Ehlers
 Copyright (C) 2009 Roland Lichters
 
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

// This file was generated automatically by gensrc.py.
// Editing this file manually is not recommended.

#ifdef WIN32
#pragma warning(disable: 4503)
#pragma warning(disable: 4786)
#pragma warning(disable: 4800)
#endif

#include <calcaddins.hpp>
#include <init.hpp>

CalcAddins_impl::CalcAddins_impl() throw () {

    QuantLibAddin::initializeAddin();

    // Basic examples

    funcMap[ STRFROMANSI( "methodOne" ) ]
        =  STRFROMANSI( "method1" );
    funcDesc[ STRFROMANSI( "methodOne" ) ]
        =  STRFROMANSI( "return 1" );
    argName[ STRFROMANSI( "qlVersion" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVersion" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    funcMap[ STRFROMANSI( "methodTwo" ) ]
        =  STRFROMANSI( "method2" );
    funcDesc[ STRFROMANSI( "methodTwo" ) ]
        =  STRFROMANSI( "return a + 2" );
    argName[ STRFROMANSI( "methodTwo" ) ].push_back( STRFROMANSI( "a" ) );
    argDesc[ STRFROMANSI( "methodTwo" ) ].push_back( STRFROMANSI( "long" ) );

    funcMap[ STRFROMANSI( "methodThree" ) ]
        =  STRFROMANSI( "method3" );
    funcDesc[ STRFROMANSI( "methodThree" ) ]
        =  STRFROMANSI( "return (a + 2) * b" );
    argName[ STRFROMANSI( "methodThree" ) ].push_back( STRFROMANSI( "a" ) );
    argDesc[ STRFROMANSI( "methodThree" ) ].push_back( STRFROMANSI( "long" ) );
    argName[ STRFROMANSI( "methodThree" ) ].push_back( STRFROMANSI( "b" ) );
    argDesc[ STRFROMANSI( "methodThree" ) ].push_back( STRFROMANSI( "multiplier of type double" ) );

    funcMap[ STRFROMANSI( "methodFour" ) ]
        =  STRFROMANSI( "method4" );
    funcDesc[ STRFROMANSI( "methodFour" ) ]
        =  STRFROMANSI( "return an exponentially distributed random number with mean m" );
    argName[ STRFROMANSI( "methodFour" ) ].push_back( STRFROMANSI( "m" ) );
    argDesc[ STRFROMANSI( "methodFour" ) ].push_back( STRFROMANSI( "double" ) );

    funcMap[ STRFROMANSI( "methodFive" ) ]
        =  STRFROMANSI( "method5" );
    funcDesc[ STRFROMANSI( "methodFive" ) ]
        =  STRFROMANSI( "return input array plus 4" );
    argName[ STRFROMANSI( "methodFive" ) ].push_back( STRFROMANSI( "a" ) );
    argDesc[ STRFROMANSI( "methodFive" ) ].push_back( STRFROMANSI( "cell range" ) );

    // Logging Functions

    // ohSetLogFile

    funcMap[ STRFROMANSI( "ohSetLogFile" ) ]
        =  STRFROMANSI( "ohSetLogFile" );
    funcDesc[ STRFROMANSI( "ohSetLogFile" ) ]
        =  STRFROMANSI( "begin logging to named file" );
    argName[ STRFROMANSI( "ohSetLogFile" ) ].push_back( STRFROMANSI( "LogFileName" ) );
    argDesc[ STRFROMANSI( "ohSetLogFile" ) ].push_back( STRFROMANSI( "path and name of log file" ) );
    argName[ STRFROMANSI( "ohSetLogFile" ) ].push_back( STRFROMANSI( "LogLevel" ) );
    argDesc[ STRFROMANSI( "ohSetLogFile" ) ].push_back( STRFROMANSI( "threshold for log messages" ) );
    argName[ STRFROMANSI( "ohSetLogFile" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "ohSetLogFile" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Utilities

    // qlVersion

    funcMap[ STRFROMANSI( "qlVersion" ) ]
        =  STRFROMANSI( "qlVersion" );
    funcDesc[ STRFROMANSI( "qlVersion" ) ]
        =  STRFROMANSI( "returns the version number of QuantLib" );
    argName[ STRFROMANSI( "qlVersion" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVersion" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAddinVersion

    funcMap[ STRFROMANSI( "qlAddinVersion" ) ]
        =  STRFROMANSI( "qlAddinVersion" );
    funcDesc[ STRFROMANSI( "qlAddinVersion" ) ]
        =  STRFROMANSI( "returns the version number of QuantLibAddin" );
    argName[ STRFROMANSI( "qlAddinVersion" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAddinVersion" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // ohVersion

    funcMap[ STRFROMANSI( "ohVersion" ) ]
        =  STRFROMANSI( "ohVersion" );
    funcDesc[ STRFROMANSI( "ohVersion" ) ]
        =  STRFROMANSI( "returns the version number of ObjectHandler" );
    argName[ STRFROMANSI( "ohVersion" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "ohVersion" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Exercise

    // qlEuropeanExercise

    funcMap[ STRFROMANSI( "qlEuropeanExercise" ) ]
        =  STRFROMANSI( "qlEuropeanExercise" );
    funcDesc[ STRFROMANSI( "qlEuropeanExercise" ) ]
        =  STRFROMANSI( "Construct an object of class EuropeanExercise and return its id" );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "ExpiryDate" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "expiry date" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Exercise

    // qlEuropeanExercise

    funcMap[ STRFROMANSI( "qlEuropeanExercise" ) ]
        =  STRFROMANSI( "qlEuropeanExercise" );
    funcDesc[ STRFROMANSI( "qlEuropeanExercise" ) ]
        =  STRFROMANSI( "Construct an object of class EuropeanExercise and return its id" );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "ExpiryDate" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "expiry date" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "overwrite flag" ) );
    
    // Instruments

    // qlInstrumentNPV

    funcMap[ STRFROMANSI( "qlInstrumentNPV" ) ]
        =  STRFROMANSI( "qlInstrumentNPV" );
    funcDesc[ STRFROMANSI( "qlInstrumentNPV" ) ]
        =  STRFROMANSI( "Returns the NPV for the given Instrument object" );
    argName[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Instrument object" ) );
    argName[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInstrumentSetPricingEngine

    funcMap[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ]
        =  STRFROMANSI( "qlInstrumentSetPricingEngine" );
    funcDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ]
        =  STRFROMANSI( "Sets a new pricing engine to the given Instrument pbject" );
    argName[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Instrument object" ) );
    argName[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "PricingEngine" ) );
    argDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "pricing engine object ID" ) );
    argName[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Options

    // qlVanillaOption

    funcMap[ STRFROMANSI( "qlVanillaOption" ) ]
        =  STRFROMANSI( "qlVanillaOption" );
    funcDesc[ STRFROMANSI( "qlVanillaOption" ) ]
        =  STRFROMANSI( "Construct an object of class VanillaOption and return its id" );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Payoff" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "StrikedTypePayoff object ID" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Exercise" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Exercise object ID" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Payoffs

    // qlStrikedTypePayoff

    funcMap[ STRFROMANSI( "qlStrikedTypePayoff" ) ]
        =  STRFROMANSI( "qlStrikedTypePayoff" );
    funcDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ]
        =  STRFROMANSI( "Construct an object of class StrikedTypePayoff and return its id" );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "PayoffID" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "payoff ID (e.g. Vanilla, PercentageStrike, AssetOrNothing, CashOrNothing, Gap, SuperShare)." ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "OptionType" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "option type" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Strike" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "strike" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "ThirdParameter" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "the 3rd paramenter for the payoff definition of CashOrNothing (cash), Gap (determines the size of the payoff), SuperFund (second strike)." ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Pricing Engines

    // qlPricingEngine

    funcMap[ STRFROMANSI( "qlPricingEngine" ) ]
        =  STRFROMANSI( "qlPricingEngine" );
    funcDesc[ STRFROMANSI( "qlPricingEngine" ) ]
        =  STRFROMANSI( "Construct an object of class PricingEngine and return its id" );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "EngineID" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "engine type" ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "ProcessID" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "GeneralizedBlackScholesProcess object ID" ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Processes

    // qlGeneralizedBlackScholesProcess

    funcMap[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ]
        =  STRFROMANSI( "qlGeneralizedBlackScholesProcess" );
    funcDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ]
        =  STRFROMANSI( "Construct an object of class GeneralizedBlackScholesProcess and return its id" );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "BlackVolID" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Black Vol Term Structure" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Underlying" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "underlying" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "DayCounter ID" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "settlement date" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "RiskFreeRate" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "risk free rate" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "DividendYield" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "dividend yield" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Settings

    // qlSettingsEvaluationDate

    funcMap[ STRFROMANSI( "qlSettingsEvaluationDate" ) ]
        =  STRFROMANSI( "qlSettingsEvaluationDate" );
    funcDesc[ STRFROMANSI( "qlSettingsEvaluationDate" ) ]
        =  STRFROMANSI( "returns the current value of the Evaluation Date" );
    argName[ STRFROMANSI( "qlSettingsEvaluationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsEvaluationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSettingsSetEvaluationDate

    funcMap[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ]
        =  STRFROMANSI( "qlSettingsSetEvaluationDate" );
    funcDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ]
        =  STRFROMANSI( "sets the value of the Evaluation Date" );
    argName[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "EvalDate" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "new value for the evaluation date" ) );
    argName[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );


    // Volatilities

    // qlBlackConstantVol

    funcMap[ STRFROMANSI( "qlBlackConstantVol" ) ]
        =  STRFROMANSI( "qlBlackConstantVol" );
    funcDesc[ STRFROMANSI( "qlBlackConstantVol" ) ]
        =  STRFROMANSI( "Construct an object of class BlackConstantVol and return its id" );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "settlement date" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Volatility" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "volatility" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "DayCounter ID" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

}

