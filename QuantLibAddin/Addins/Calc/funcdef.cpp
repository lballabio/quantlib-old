
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

// This file was generated automatically by gensrc.py.  If you edit this file
// manually then your changes will be lost the next time gensrc runs.

// This source code file was generated from the following stub:
//      gensrc/gensrc/stubs/stub.calc.map

#ifdef WIN32
#pragma warning(disable: 4503)
#pragma warning(disable: 4786)
#pragma warning(disable: 4800)
#endif

//#include <Addins/Calc/qladdin.hpp>
#include <calcaddins.hpp>
#include <init.hpp>

CalcAddins_impl::CalcAddins_impl() throw () {

    QuantLibAddin::initializeAddin();

    // Logging Functions

    // ohLogSetFile

    funcMap[ STRFROMANSI( "ohLogSetFile" ) ]
        =  STRFROMANSI( "ohLogSetFile" );
    funcDesc[ STRFROMANSI( "ohLogSetFile" ) ]
        =  STRFROMANSI( "begin logging to named file." );
    argName[ STRFROMANSI( "ohLogSetFile" ) ].push_back( STRFROMANSI( "LogFileName" ) );
    argDesc[ STRFROMANSI( "ohLogSetFile" ) ].push_back( STRFROMANSI( "path and name of log file." ) );
    argName[ STRFROMANSI( "ohLogSetFile" ) ].push_back( STRFROMANSI( "LogLevel" ) );
    argDesc[ STRFROMANSI( "ohLogSetFile" ) ].push_back( STRFROMANSI( "threshold for log messages. Default value = 4." ) );
    argName[ STRFROMANSI( "ohLogSetFile" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "ohLogSetFile" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Calendar

    // qlCalendarAddHoliday

    funcMap[ STRFROMANSI( "qlCalendarAddHoliday" ) ]
        =  STRFROMANSI( "qlCalendarAddHoliday" );
    funcDesc[ STRFROMANSI( "qlCalendarAddHoliday" ) ]
        =  STRFROMANSI( "adds an holiday to the given calendar." );
    argName[ STRFROMANSI( "qlCalendarAddHoliday" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarAddHoliday" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarAddHoliday" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarAddHoliday" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlCalendarAddHoliday" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarAddHoliday" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarName

    funcMap[ STRFROMANSI( "qlCalendarName" ) ]
        =  STRFROMANSI( "qlCalendarName" );
    funcDesc[ STRFROMANSI( "qlCalendarName" ) ]
        =  STRFROMANSI( "Returns the name of the given calendar." );
    argName[ STRFROMANSI( "qlCalendarName" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarName" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarName" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarName" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarRemoveHoliday

    funcMap[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ]
        =  STRFROMANSI( "qlCalendarRemoveHoliday" );
    funcDesc[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ]
        =  STRFROMANSI( "removes an holiday from the given calendar." );
    argName[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarRemoveHoliday" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Caps/Floors

    // qlCapFloor

    funcMap[ STRFROMANSI( "qlCapFloor" ) ]
        =  STRFROMANSI( "qlCapFloor" );
    funcDesc[ STRFROMANSI( "qlCapFloor" ) ]
        =  STRFROMANSI( "Construct an object of class CapFloor and return its id" );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "OptionType" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "option type (cap or floor)." ) );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "LegID" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "coupon vector." ) );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "Strikes" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "strikes." ) );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCapFloor" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlCapFloorMaturityDate

    funcMap[ STRFROMANSI( "qlCapFloorMaturityDate" ) ]
        =  STRFROMANSI( "qlCapFloorMaturityDate" );
    funcDesc[ STRFROMANSI( "qlCapFloorMaturityDate" ) ]
        =  STRFROMANSI( "Returns the maturity (i.e. last payment) date for the given CapFloor object." );
    argName[ STRFROMANSI( "qlCapFloorMaturityDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCapFloorMaturityDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::CapFloor object" ) );
    argName[ STRFROMANSI( "qlCapFloorMaturityDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCapFloorMaturityDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCapFloorStartDate

    funcMap[ STRFROMANSI( "qlCapFloorStartDate" ) ]
        =  STRFROMANSI( "qlCapFloorStartDate" );
    funcDesc[ STRFROMANSI( "qlCapFloorStartDate" ) ]
        =  STRFROMANSI( "Returns the start (i.e. first accrual) date for the given CapFloor object." );
    argName[ STRFROMANSI( "qlCapFloorStartDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCapFloorStartDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::CapFloor object" ) );
    argName[ STRFROMANSI( "qlCapFloorStartDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCapFloorStartDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMakeCapFloor

    funcMap[ STRFROMANSI( "qlMakeCapFloor" ) ]
        =  STRFROMANSI( "qlMakeCapFloor" );
    funcDesc[ STRFROMANSI( "qlMakeCapFloor" ) ]
        =  STRFROMANSI( "Construct an object of class CapFloor and return its id" );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "OptionType" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "option type (Cap or Floor)." ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "Length" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "as period (e.g. 10Y)." ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "floating IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "Strike" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "strike. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "as period (if zero days the first caplet is removed)." ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "PricingEngineID" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "CapFloor PricingEngine object ID." ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeCapFloor" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Caplet Volatility Term Structures

    // qlConstantOptionletVolatility

    funcMap[ STRFROMANSI( "qlConstantOptionletVolatility" ) ]
        =  STRFROMANSI( "qlConstantOptionletVolatility" );
    funcDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ]
        =  STRFROMANSI( "Construct an object of class ConstantOptionletVolatility and return its id" );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "NDays" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "number of days to advance from EvaluationDate: it fixes the date at which the variance = 0.0. Default value = 0." ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET) used for calculating the exercise dates from the expiries." ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "BusinessDayConvention" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "Business day convention used for calculating the exercise dates from the expiries." ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "Volatility" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "cap/floor constant volatility Quote." ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlConstantOptionletVolatility" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRelinkableHandleOptionletVolatilityStructure

    funcMap[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ]
        =  STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" );
    funcDesc[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ]
        =  STRFROMANSI( "Construct an object of class RelinkableHandleImpl<QuantLibAddin::OptionletVolatilityStructure, QuantLib::OptionletVolatilityStructure> and return its id" );
    argName[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "CurrentLink" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "OptionletVolatilityStructure object ID. If omitted, nothing is linked by the RelinkableHandle. Default value = ." ) );
    argName[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleOptionletVolatilityStructure" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Default Probability Term Structures

    // qlFlatHazardRate

    funcMap[ STRFROMANSI( "qlFlatHazardRate" ) ]
        =  STRFROMANSI( "qlFlatHazardRate" );
    funcDesc[ STRFROMANSI( "qlFlatHazardRate" ) ]
        =  STRFROMANSI( "Construct an object of class FlatHazardRate and return its id" );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "NDays" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "number of days to advance from EvaluationDate (usually zero or two): it fixes the date at which the discount factor = 1.0. Default value = 0." ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET) to advance from global EvaluationDate. Default value = NullCalendar." ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "the curve level." ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFlatHazardRate" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Exercise

    // qlEuropeanExercise

    funcMap[ STRFROMANSI( "qlEuropeanExercise" ) ]
        =  STRFROMANSI( "qlEuropeanExercise" );
    funcDesc[ STRFROMANSI( "qlEuropeanExercise" ) ]
        =  STRFROMANSI( "Construct an object of class EuropeanExercise and return its id" );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "ExpiryDate" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "expiry date." ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuropeanExercise" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Handles

    // qlHandleCurrentLink

    funcMap[ STRFROMANSI( "qlHandleCurrentLink" ) ]
        =  STRFROMANSI( "qlHandleCurrentLink" );
    funcDesc[ STRFROMANSI( "qlHandleCurrentLink" ) ]
        =  STRFROMANSI( "ID of object to which this handle is linked - empty string if none." );
    argName[ STRFROMANSI( "qlHandleCurrentLink" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlHandleCurrentLink" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Handle object" ) );
    argName[ STRFROMANSI( "qlHandleCurrentLink" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlHandleCurrentLink" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlHandleEmpty

    funcMap[ STRFROMANSI( "qlHandleEmpty" ) ]
        =  STRFROMANSI( "qlHandleEmpty" );
    funcDesc[ STRFROMANSI( "qlHandleEmpty" ) ]
        =  STRFROMANSI( "True if handle is empty, False if not." );
    argName[ STRFROMANSI( "qlHandleEmpty" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlHandleEmpty" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Handle object" ) );
    argName[ STRFROMANSI( "qlHandleEmpty" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlHandleEmpty" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRelinkableHandleLinkTo

    funcMap[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ]
        =  STRFROMANSI( "qlRelinkableHandleLinkTo" );
    funcDesc[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ]
        =  STRFROMANSI( "Relink the RelinkableHandle to the given object." );
    argName[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::RelinkableHandle object" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ].push_back( STRFROMANSI( "CurrentLink" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ].push_back( STRFROMANSI( "ID of Object to which the RelinkableHandle should be relinked, or empty string for null object. Default value = ." ) );
    argName[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleLinkTo" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Indices

    // qlBMAIndex

    funcMap[ STRFROMANSI( "qlBMAIndex" ) ]
        =  STRFROMANSI( "qlBMAIndex" );
    funcDesc[ STRFROMANSI( "qlBMAIndex" ) ]
        =  STRFROMANSI( "Construct an object of class BMAIndex and return its id" );
    argName[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBMAIndex" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlEonia

    funcMap[ STRFROMANSI( "qlEonia" ) ]
        =  STRFROMANSI( "qlEonia" );
    funcDesc[ STRFROMANSI( "qlEonia" ) ]
        =  STRFROMANSI( "Construct an object of class Eonia and return its id" );
    argName[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEonia" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlEuribor

    funcMap[ STRFROMANSI( "qlEuribor" ) ]
        =  STRFROMANSI( "qlEuribor" );
    funcDesc[ STRFROMANSI( "qlEuribor" ) ]
        =  STRFROMANSI( "Construct an object of class Euribor and return its id" );
    argName[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "index tenor: SW (1W), 2W, 3W, 1M, 2M, 3M, 4M, 5M, 6M, 7M, 8M, 9M, 10M, 11M, 12M (1Y)." ) );
    argName[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuribor" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlEuribor365

    funcMap[ STRFROMANSI( "qlEuribor365" ) ]
        =  STRFROMANSI( "qlEuribor365" );
    funcDesc[ STRFROMANSI( "qlEuribor365" ) ]
        =  STRFROMANSI( "Construct an object of class Euribor365 and return its id" );
    argName[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "index tenor: SW (1W), 2W, 3W, 1M, 2M, 3M, 4M, 5M, 6M, 7M, 8M, 9M, 10M, 11M, 12M (1Y)." ) );
    argName[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuribor365" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlIborIndex

    funcMap[ STRFROMANSI( "qlIborIndex" ) ]
        =  STRFROMANSI( "qlIborIndex" );
    funcDesc[ STRFROMANSI( "qlIborIndex" ) ]
        =  STRFROMANSI( "Construct an object of class IborIndex and return its id" );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "FamilyName" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "index family name." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "index tenor (e.g. 2D for two days , 3W for three weeks, 6M for six months, 1Y for one year)." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2)." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Index Currency." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "BDayConvention" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "business day convention (e.g. Modified Following)." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "EndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "TRUE if the index follow the 'end of month' convention." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "DayCounter ID." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "FwdCurve" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "forwarding YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlIborIndex" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlLibor

    funcMap[ STRFROMANSI( "qlLibor" ) ]
        =  STRFROMANSI( "qlLibor" );
    funcDesc[ STRFROMANSI( "qlLibor" ) ]
        =  STRFROMANSI( "Construct an object of class Libor and return its id" );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "Libor index currency." ) );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "index tenor: ON (1D), SW (1W), 2W, 3W, 1M, 2M, 3M, 4M, 5M, 6M, 7M, 8M, 9M, 10M, 11M, 12M (1Y)." ) );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlLibor" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlOvernightIndex

    funcMap[ STRFROMANSI( "qlOvernightIndex" ) ]
        =  STRFROMANSI( "qlOvernightIndex" );
    funcDesc[ STRFROMANSI( "qlOvernightIndex" ) ]
        =  STRFROMANSI( "Construct an object of class OvernightIndex and return its id" );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "FamilyName" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "index family name." ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "fixing days (e.g. 0)." ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "Index Currency." ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "DayCounter ID." ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndex" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Instruments

    // qlInstrumentNPV

    funcMap[ STRFROMANSI( "qlInstrumentNPV" ) ]
        =  STRFROMANSI( "qlInstrumentNPV" );
    funcDesc[ STRFROMANSI( "qlInstrumentNPV" ) ]
        =  STRFROMANSI( "Returns the NPV for the given Instrument object." );
    argName[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Instrument object" ) );
    argName[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInstrumentNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInstrumentSetPricingEngine

    funcMap[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ]
        =  STRFROMANSI( "qlInstrumentSetPricingEngine" );
    funcDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ]
        =  STRFROMANSI( "Sets a new pricing engine to the given Instrument pbject." );
    argName[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Instrument object" ) );
    argName[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "PricingEngine" ) );
    argDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "PricingEngine object ID." ) );
    argName[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInstrumentSetPricingEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInstrumentValuationDate

    funcMap[ STRFROMANSI( "qlInstrumentValuationDate" ) ]
        =  STRFROMANSI( "qlInstrumentValuationDate" );
    funcDesc[ STRFROMANSI( "qlInstrumentValuationDate" ) ]
        =  STRFROMANSI( "Returns the NPV for the given Instrument object." );
    argName[ STRFROMANSI( "qlInstrumentValuationDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInstrumentValuationDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Instrument object" ) );
    argName[ STRFROMANSI( "qlInstrumentValuationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInstrumentValuationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Options

    // qlVanillaOption

    funcMap[ STRFROMANSI( "qlVanillaOption" ) ]
        =  STRFROMANSI( "qlVanillaOption" );
    funcDesc[ STRFROMANSI( "qlVanillaOption" ) ]
        =  STRFROMANSI( "Construct an object of class VanillaOption and return its id" );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Payoff" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "StrikedTypePayoff object ID." ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Exercise" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Exercise object ID." ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlVanillaOption" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Overnight Indexed Swap

    // qlMakeDatedOIS

    funcMap[ STRFROMANSI( "qlMakeDatedOIS" ) ]
        =  STRFROMANSI( "qlMakeDatedOIS" );
    funcDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ]
        =  STRFROMANSI( "Construct an object of class OvernightIndexedSwap and return its id" );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "ois start date. If missing first ECB date is used. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "ois end date. If missing first ECB date after start date is used. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "OvernightIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "OvernightIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "the fixed leg rate. If missing atm rate is used. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "fixed leg day counter. Default value = Actual/360." ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "the spread in the overnight leg rate. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeDatedOIS" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMakeOIS

    funcMap[ STRFROMANSI( "qlMakeOIS" ) ]
        =  STRFROMANSI( "qlMakeOIS" );
    funcDesc[ STRFROMANSI( "qlMakeOIS" ) ]
        =  STRFROMANSI( "Construct an object of class OvernightIndexedSwap and return its id" );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "SwapTenor" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "swap tenor period (e.g. 5Y)." ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "OvernightIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "OvernightIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "the fixed leg rate. If missing atm rate is used. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "forward start period." ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "fixed leg day counter. Default value = Actual/360." ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "the spread in the overnight leg rate. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeOIS" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlOvernightIndexedSwap

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwap" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwap" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ]
        =  STRFROMANSI( "Construct an object of class OvernightIndexedSwap and return its id" );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "PayerReceiver" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "PAYER to pay the fixed rate, RECEIVER to receive it. Default value = Payer." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Nominal" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Notional Amount. Default value = 100." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Schedule" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "the fixed leg rate. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "fixed leg day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "OvernightIndex" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "overnight leg OvernightIndex object ID." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "overnight leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlOvernightIndexedSwapFairRate

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapFairRate" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapFairRate" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapFairRate" ) ]
        =  STRFROMANSI( "returns the fair fixed leg rate which would zero the swap NPV for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFairRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFairRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFairRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFairRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapFairSpread

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapFairSpread" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapFairSpread" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapFairSpread" ) ]
        =  STRFROMANSI( "returns the fair spread over the overnight rate which would zero the swap NPV for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFairSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFairSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFairSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFairSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapFixedLegNPV

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" ) ]
        =  STRFROMANSI( "returns the NPV of the fixed rate leg for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapFixedRate

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapFixedRate" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapFixedRate" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapFixedRate" ) ]
        =  STRFROMANSI( "returns the fixed rate for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFixedRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFixedRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapFixedRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapFixedRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapNominal

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapNominal" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapNominal" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapNominal" ) ]
        =  STRFROMANSI( "returns the swap nominal for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapNominal" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapNominal" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapNominal" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapNominal" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapOvernightLegBPS

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" ) ]
        =  STRFROMANSI( "returns the BPS of the overnight rate leg for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegBPS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapOvernightLegNPV

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" ) ]
        =  STRFROMANSI( "returns the NPV of the overnight rate leg for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapOvernightLegNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlOvernightIndexedSwapSpread

    funcMap[ STRFROMANSI( "qlOvernightIndexedSwapSpread" ) ]
        =  STRFROMANSI( "qlOvernightIndexedSwapSpread" );
    funcDesc[ STRFROMANSI( "qlOvernightIndexedSwapSpread" ) ]
        =  STRFROMANSI( "returns the spread over overnight rate for the given OvernightIndexedSwap object." );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::OvernightIndexedSwap object" ) );
    argName[ STRFROMANSI( "qlOvernightIndexedSwapSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOvernightIndexedSwapSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "option type." ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Strike" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "strike." ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "ThirdParameter" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "the 3rd paramenter for the payoff definition of CashOrNothing (cash), Gap (determines the size of the payoff), SuperFund (second strike). Default value = QuantLib::Null<QuantLib::Real>()." ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlStrikedTypePayoff" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Piecewise Yield Curves

    // qlPiecewiseYieldCurve

    funcMap[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ]
        =  STRFROMANSI( "qlPiecewiseYieldCurve" );
    funcDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ]
        =  STRFROMANSI( "Construct an object of class PiecewiseYieldCurve and return its id" );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "NDays" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "number of days to advance from EvaluationDate (usually zero or two): it fixes the date at which the discount factor = 1.0. Default value = 0." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET) to advance from global EvaluationDate." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "RateHelpers" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "vector of rate-helpers." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Jumps" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Jump quotes vector." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "JumpDates" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Jump dates vector." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Accuracy" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Bootstrapping accuracy. Default value = 1.0e-12." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "TraitsID" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Discount, ZeroRate, or ForwardRate. Default value = Discount." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "InterpolatorID" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "BackwardFlat, ForwardFlat, Linear, LogLinear, CubicSpline, or LogCubic. Default value = LogLinear." ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseYieldCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Pricing Engines

    // qlAnalyticCapFloorEngine

    funcMap[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ]
        =  STRFROMANSI( "qlAnalyticCapFloorEngine" );
    funcDesc[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ]
        =  STRFROMANSI( "Construct an object of class AnalyticCapFloorEngine and return its id" );
    argName[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "HandleModel" ) );
    argDesc[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "affine model (providing a discount bond option pricing formula) object ID." ) );
    argName[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlAnalyticCapFloorEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBinomialPricingEngine

    funcMap[ STRFROMANSI( "qlBinomialPricingEngine" ) ]
        =  STRFROMANSI( "qlBinomialPricingEngine" );
    funcDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ]
        =  STRFROMANSI( "Construct an object of class PricingEngine and return its id" );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "EngineID" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "engine type." ) );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "ProcessID" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "GeneralizedBlackScholesProcess object ID." ) );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "TimeSteps" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "#/time steps." ) );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBinomialPricingEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBlackCapFloorEngine

    funcMap[ STRFROMANSI( "qlBlackCapFloorEngine" ) ]
        =  STRFROMANSI( "qlBlackCapFloorEngine" );
    funcDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ]
        =  STRFROMANSI( "Construct an object of class BlackCapFloorEngine and return its id" );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "VolTS" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "OptionletVolatilityStructure object ID." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBlackSwaptionEngine

    funcMap[ STRFROMANSI( "qlBlackSwaptionEngine" ) ]
        =  STRFROMANSI( "qlBlackSwaptionEngine" );
    funcDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ]
        =  STRFROMANSI( "Construct an object of class BlackSwaptionEngine and return its id" );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "VolTS" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "SwaptionVolatilityStructure object ID." ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBlackSwaptionEngine2

    funcMap[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ]
        =  STRFROMANSI( "qlBlackSwaptionEngine2" );
    funcDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ]
        =  STRFROMANSI( "Construct an object of class BlackSwaptionEngine and return its id" );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "Vol" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "cap/floor term volatility." ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBondEngine

    funcMap[ STRFROMANSI( "qlBondEngine" ) ]
        =  STRFROMANSI( "qlBondEngine" );
    funcDesc[ STRFROMANSI( "qlBondEngine" ) ]
        =  STRFROMANSI( "Construct an object of class BondEngine and return its id" );
    argName[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBondEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlDiscountingSwapEngine

    funcMap[ STRFROMANSI( "qlDiscountingSwapEngine" ) ]
        =  STRFROMANSI( "qlDiscountingSwapEngine" );
    funcDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ]
        =  STRFROMANSI( "Construct an object of class DiscountingSwapEngine and return its id" );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "IncludeSettlDate" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "TRUE if cashflows paid at the settlement date must be taken into account. Default value = true." ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "cashflows before this date are not taken into account. If missing it is assumed equal to the discounting yield term structure's reference date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "NpvDate" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "all cashflows are discounted to this date. If missing it is assumed equal to the discounting yield term structure's reference date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDiscountingSwapEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlPricingEngine

    funcMap[ STRFROMANSI( "qlPricingEngine" ) ]
        =  STRFROMANSI( "qlPricingEngine" );
    funcDesc[ STRFROMANSI( "qlPricingEngine" ) ]
        =  STRFROMANSI( "Construct an object of class PricingEngine and return its id" );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "EngineID" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "engine type." ) );
    argName[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "ProcessID" ) );
    argDesc[ STRFROMANSI( "qlPricingEngine" ) ].push_back( STRFROMANSI( "GeneralizedBlackScholesProcess object ID." ) );
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
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Black Vol Term Structure." ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Underlying" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "underlying." ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "settlement date." ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "RiskFreeRate" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "risk free rate." ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "DividendYield" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "dividend yield." ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlGeneralizedBlackScholesProcess" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Quotes

    // qlQuoteValue

    funcMap[ STRFROMANSI( "qlQuoteValue" ) ]
        =  STRFROMANSI( "qlQuoteValue" );
    funcDesc[ STRFROMANSI( "qlQuoteValue" ) ]
        =  STRFROMANSI( "Returns the current value of the given Quote object." );
    argName[ STRFROMANSI( "qlQuoteValue" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlQuoteValue" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Quote object" ) );
    argName[ STRFROMANSI( "qlQuoteValue" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlQuoteValue" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRelinkableHandleQuote

    funcMap[ STRFROMANSI( "qlRelinkableHandleQuote" ) ]
        =  STRFROMANSI( "qlRelinkableHandleQuote" );
    funcDesc[ STRFROMANSI( "qlRelinkableHandleQuote" ) ]
        =  STRFROMANSI( "Construct an object of class RelinkableHandleImpl<QuantLibAddin::Quote, QuantLib::Quote> and return its id" );
    argName[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "CurrentLink" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "Quote object ID. If omitted, nothing is linked by the RelinkableHandle. Default value = ." ) );
    argName[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleQuote" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSimpleQuote

    funcMap[ STRFROMANSI( "qlSimpleQuote" ) ]
        =  STRFROMANSI( "qlSimpleQuote" );
    funcDesc[ STRFROMANSI( "qlSimpleQuote" ) ]
        =  STRFROMANSI( "Construct an object of class SimpleQuote and return its id" );
    argName[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "Value" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "quote. Default value = QuantLib::Null<QuantLib::Real>()." ) );
    argName[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "TickValue" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "tick value used for sensitivity analysis." ) );
    argName[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuote" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSimpleQuoteReset

    funcMap[ STRFROMANSI( "qlSimpleQuoteReset" ) ]
        =  STRFROMANSI( "qlSimpleQuoteReset" );
    funcDesc[ STRFROMANSI( "qlSimpleQuoteReset" ) ]
        =  STRFROMANSI( "resets the given SimpleQuote object to the uninitialized state." );
    argName[ STRFROMANSI( "qlSimpleQuoteReset" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuoteReset" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SimpleQuote object" ) );
    argName[ STRFROMANSI( "qlSimpleQuoteReset" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuoteReset" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSimpleQuoteSetValue

    funcMap[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ]
        =  STRFROMANSI( "qlSimpleQuoteSetValue" );
    funcDesc[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ]
        =  STRFROMANSI( "sets a new value to the given SimpleQuote object and returns the difference with the previous value." );
    argName[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::SimpleQuote object" ) );
    argName[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ].push_back( STRFROMANSI( "Value" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ].push_back( STRFROMANSI( "the new value. Default value = QuantLib::Null<QuantLib::Real>()." ) );
    argName[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSimpleQuoteSetValue" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // RateHelper

    // qlBondHelper

    funcMap[ STRFROMANSI( "qlBondHelper" ) ]
        =  STRFROMANSI( "qlBondHelper" );
    funcDesc[ STRFROMANSI( "qlBondHelper" ) ]
        =  STRFROMANSI( "Construct an object of class BondHelper and return its id" );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "cleanPrice." ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Bond" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Bond object ID." ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlDatedOISRateHelper

    funcMap[ STRFROMANSI( "qlDatedOISRateHelper" ) ]
        =  STRFROMANSI( "qlDatedOISRateHelper" );
    funcDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ]
        =  STRFROMANSI( "Construct an object of class DatedOISRateHelper and return its id" );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "swap start date." ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "swap end date." ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "ONIndex" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "floating leg OvernightIndex object ID." ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlDepositRateHelper

    funcMap[ STRFROMANSI( "qlDepositRateHelper" ) ]
        =  STRFROMANSI( "qlDepositRateHelper" );
    funcDesc[ STRFROMANSI( "qlDepositRateHelper" ) ]
        =  STRFROMANSI( "Construct an object of class DepositRateHelper and return its id" );
    argName[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "deposit quote." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlDepositRateHelper2

    funcMap[ STRFROMANSI( "qlDepositRateHelper2" ) ]
        =  STRFROMANSI( "qlDepositRateHelper2" );
    funcDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ]
        =  STRFROMANSI( "Construct an object of class DepositRateHelper and return its id" );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "deposit quote." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "deposit length (e.g. 3M for three months)." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2)." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Convention" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "business day convention (e.g. Modified Following)." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "EndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "End of Month rule (TRUE for end of month to end of month termination date, FALSE otherwise)." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "DayCounter ID." ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDepositRateHelper2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFixedRateBondHelper

    funcMap[ STRFROMANSI( "qlFixedRateBondHelper" ) ]
        =  STRFROMANSI( "qlFixedRateBondHelper" );
    funcDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ]
        =  STRFROMANSI( "Construct an object of class FixedRateBondHelper and return its id" );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "cleanPrice." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "settlement days." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Coupons" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "coupon fixed rates." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Redemption" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "redemption value. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFraRateHelper

    funcMap[ STRFROMANSI( "qlFraRateHelper" ) ]
        =  STRFROMANSI( "qlFraRateHelper" );
    funcDesc[ STRFROMANSI( "qlFraRateHelper" ) ]
        =  STRFROMANSI( "Construct an object of class FraRateHelper and return its id" );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "PeriodToStart" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "Period to start date." ) );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFraRateHelper2

    funcMap[ STRFROMANSI( "qlFraRateHelper2" ) ]
        =  STRFROMANSI( "qlFraRateHelper2" );
    funcDesc[ STRFROMANSI( "qlFraRateHelper2" ) ]
        =  STRFROMANSI( "Construct an object of class FraRateHelper and return its id" );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "PeriodToStart" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Period to start date." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "LengthInMonths" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "months to end." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2)." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Convention" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "business day convention (e.g. Modified Following)." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "EndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "End of Month rule (TRUE for end of month to end of month termination date, FALSE otherwise)." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "DayCounter ID." ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFraRateHelper2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlOISRateHelper

    funcMap[ STRFROMANSI( "qlOISRateHelper" ) ]
        =  STRFROMANSI( "qlOISRateHelper" );
    funcDesc[ STRFROMANSI( "qlOISRateHelper" ) ]
        =  STRFROMANSI( "Construct an object of class OISRateHelper and return its id" );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "SettlDays" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "swap settlement days." ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "swap length (e.g. 5Y for five years)." ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "ONIndex" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "floating leg OvernightIndex object ID." ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSwapRateHelper

    funcMap[ STRFROMANSI( "qlSwapRateHelper" ) ]
        =  STRFROMANSI( "qlSwapRateHelper" );
    funcDesc[ STRFROMANSI( "qlSwapRateHelper" ) ]
        =  STRFROMANSI( "Construct an object of class SwapRateHelper and return its id" );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "SwapIndex object ID." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "floating leg spread." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "forward start period." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "DiscountingCurve" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSwapRateHelper2

    funcMap[ STRFROMANSI( "qlSwapRateHelper2" ) ]
        =  STRFROMANSI( "qlSwapRateHelper2" );
    funcDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ]
        =  STRFROMANSI( "Construct an object of class SwapRateHelper and return its id" );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "swap length (e.g. 5Y for five years)." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "FixedLegFrequency" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "fixed leg frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly)." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "FixedLegConvention" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "fixed leg convention (e.g. Unadjusted)." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "FixedLegDayCounter" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "floating leg IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "floating leg spread." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "forward start period." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "DiscountingCurve" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSwapRateHelperSpread

    funcMap[ STRFROMANSI( "qlSwapRateHelperSpread" ) ]
        =  STRFROMANSI( "qlSwapRateHelperSpread" );
    funcDesc[ STRFROMANSI( "qlSwapRateHelperSpread" ) ]
        =  STRFROMANSI( "returns the spread for the given SwapRateHelper object." );
    argName[ STRFROMANSI( "qlSwapRateHelperSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelperSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SwapRateHelper object" ) );
    argName[ STRFROMANSI( "qlSwapRateHelperSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelperSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Schedules

    // qlSchedule

    funcMap[ STRFROMANSI( "qlSchedule" ) ]
        =  STRFROMANSI( "qlSchedule" );
    funcDesc[ STRFROMANSI( "qlSchedule" ) ]
        =  STRFROMANSI( "Construct an object of class Schedule and return its id" );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "EffectiveDate" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "effective date." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "TerminationDate" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "termination date." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "tenor (e.g. 2D for two days , 3W for three weeks, 6M for six months, 1Y for one year)." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Convention" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "accrual dates business day convention." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "TermDateConv" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "termination date business day convention." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "GenRule" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Date generation rule (Backward, Forward, ThirdWednesday, Twentieth, TwentiethIMM, Zero)." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "EndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "end of month convention. Default value = false." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "FirstDate" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "stub date, if there is an irregular starting period. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "NextToLastDate" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "stub date, if there an irregular final period. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlScheduleSize

    funcMap[ STRFROMANSI( "qlScheduleSize" ) ]
        =  STRFROMANSI( "qlScheduleSize" );
    funcDesc[ STRFROMANSI( "qlScheduleSize" ) ]
        =  STRFROMANSI( "returns the number of dates in the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Settings

    // qlSettingsEvaluationDate

    funcMap[ STRFROMANSI( "qlSettingsEvaluationDate" ) ]
        =  STRFROMANSI( "qlSettingsEvaluationDate" );
    funcDesc[ STRFROMANSI( "qlSettingsEvaluationDate" ) ]
        =  STRFROMANSI( "returns the current value of the Evaluation Date." );
    argName[ STRFROMANSI( "qlSettingsEvaluationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsEvaluationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSettingsSetEvaluationDate

    funcMap[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ]
        =  STRFROMANSI( "qlSettingsSetEvaluationDate" );
    funcDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ]
        =  STRFROMANSI( "sets the value of the Evaluation Date." );
    argName[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "EvalDate" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "new value for the evaluation date." ) );
    argName[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Swaption

    // qlMakeSwaption

    funcMap[ STRFROMANSI( "qlMakeSwaption" ) ]
        =  STRFROMANSI( "qlMakeSwaption" );
    funcDesc[ STRFROMANSI( "qlMakeSwaption" ) ]
        =  STRFROMANSI( "Construct an object of class Swaption and return its id" );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "SwapIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "OptionTenor" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "option tenor as Period (e.g. '5Y')." ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "Strike" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "strike. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "PricingEngineID" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "Swaption PricingEngine object ID." ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeSwaption" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSwaption

    funcMap[ STRFROMANSI( "qlSwaption" ) ]
        =  STRFROMANSI( "qlSwaption" );
    funcDesc[ STRFROMANSI( "qlSwaption" ) ]
        =  STRFROMANSI( "Construct an object of class Swaption and return its id" );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "VanillaSwap" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "underlying (vanilla) swap object ID." ) );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "Exercise" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "Exercise object ID." ) );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "SettlementType" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "settlement type (PhysicalSettlement, CashSettlement)." ) );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSwaption" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Swaption Volatility Term Structures

    // qlConstantSwaptionVolatility

    funcMap[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ]
        =  STRFROMANSI( "qlConstantSwaptionVolatility" );
    funcDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ]
        =  STRFROMANSI( "Construct an object of class ConstantSwaptionVolatility and return its id" );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "NDays" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "number of days to advance from EvaluationDate: it fixes the date at which the variance = 0.0. Default value = 0." ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET) used for calculating the exercise dates from the expiries." ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "BusinessDayConvention" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "Business day convention used for calculating the exercise dates from the expiries." ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "Volatility" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "cap/floor constant volatility Quote." ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlConstantSwaptionVolatility" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Yield Term Structures

    // qlDiscountCurve

    funcMap[ STRFROMANSI( "qlDiscountCurve" ) ]
        =  STRFROMANSI( "qlDiscountCurve" );
    funcDesc[ STRFROMANSI( "qlDiscountCurve" ) ]
        =  STRFROMANSI( "Construct an object of class DiscountCurve and return its id" );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "CurveDates" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "dates of the curve." ) );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "CurveDiscounts" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "discount factors for the above dates." ) );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDiscountCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFlatForward

    funcMap[ STRFROMANSI( "qlFlatForward" ) ]
        =  STRFROMANSI( "qlFlatForward" );
    funcDesc[ STRFROMANSI( "qlFlatForward" ) ]
        =  STRFROMANSI( "Construct an object of class FlatForward and return its id" );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "NDays" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "number of days to advance from EvaluationDate (usually zero or two): it fixes the date at which the discount factor = 1.0. Default value = 0." ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET) to advance from global EvaluationDate. Default value = NullCalendar." ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "the curve level." ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Continuous." ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFlatForward" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlForwardCurve

    funcMap[ STRFROMANSI( "qlForwardCurve" ) ]
        =  STRFROMANSI( "qlForwardCurve" );
    funcDesc[ STRFROMANSI( "qlForwardCurve" ) ]
        =  STRFROMANSI( "Construct an object of class ForwardCurve and return its id" );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "CurveDates" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "dates of the curve." ) );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "ForwardYields" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "forwards rates for the above dates." ) );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlForwardCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlForwardSpreadedTermStructure

    funcMap[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ]
        =  STRFROMANSI( "qlForwardSpreadedTermStructure" );
    funcDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ]
        =  STRFROMANSI( "Construct an object of class ForwardSpreadedTermStructure and return its id" );
    argName[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "BaseYieldCurve" ) );
    argDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "Base YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "spread." ) );
    argName[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlForwardSpreadedTermStructure" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlImpliedTermStructure

    funcMap[ STRFROMANSI( "qlImpliedTermStructure" ) ]
        =  STRFROMANSI( "qlImpliedTermStructure" );
    funcDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ]
        =  STRFROMANSI( "Construct an object of class ImpliedTermStructure and return its id" );
    argName[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "BaseYieldCurve" ) );
    argDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "Base YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "ReferenceDate" ) );
    argDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "the reference date the base YieldTermStructure should be shifted to." ) );
    argName[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlImpliedTermStructure" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRelinkableHandleYieldTermStructure

    funcMap[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ]
        =  STRFROMANSI( "qlRelinkableHandleYieldTermStructure" );
    funcDesc[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ]
        =  STRFROMANSI( "Construct an object of class RelinkableHandleImpl<QuantLibAddin::YieldTermStructure, QuantLib::YieldTermStructure> and return its id" );
    argName[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "CurrentLink" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "YieldTermStructure object ID. If omitted, nothing is linked by the RelinkableHandle. Default value = ." ) );
    argName[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleYieldTermStructure" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlTermStructureCalendar

    funcMap[ STRFROMANSI( "qlTermStructureCalendar" ) ]
        =  STRFROMANSI( "qlTermStructureCalendar" );
    funcDesc[ STRFROMANSI( "qlTermStructureCalendar" ) ]
        =  STRFROMANSI( "Returns the calendar used by the given TermStructure object." );
    argName[ STRFROMANSI( "qlTermStructureCalendar" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlTermStructureCalendar" ) ].push_back( STRFROMANSI( "id of existing QuantLib::TermStructure object" ) );
    argName[ STRFROMANSI( "qlTermStructureCalendar" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlTermStructureCalendar" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlTermStructureMaxDate

    funcMap[ STRFROMANSI( "qlTermStructureMaxDate" ) ]
        =  STRFROMANSI( "qlTermStructureMaxDate" );
    funcDesc[ STRFROMANSI( "qlTermStructureMaxDate" ) ]
        =  STRFROMANSI( "Returns the max date for the given TermStructure object." );
    argName[ STRFROMANSI( "qlTermStructureMaxDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlTermStructureMaxDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::TermStructure object" ) );
    argName[ STRFROMANSI( "qlTermStructureMaxDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlTermStructureMaxDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlTermStructureReferenceDate

    funcMap[ STRFROMANSI( "qlTermStructureReferenceDate" ) ]
        =  STRFROMANSI( "qlTermStructureReferenceDate" );
    funcDesc[ STRFROMANSI( "qlTermStructureReferenceDate" ) ]
        =  STRFROMANSI( "Returns the reference date for the given TermStructure object." );
    argName[ STRFROMANSI( "qlTermStructureReferenceDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlTermStructureReferenceDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::TermStructure object" ) );
    argName[ STRFROMANSI( "qlTermStructureReferenceDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlTermStructureReferenceDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlTermStructureSettlementDays

    funcMap[ STRFROMANSI( "qlTermStructureSettlementDays" ) ]
        =  STRFROMANSI( "qlTermStructureSettlementDays" );
    funcDesc[ STRFROMANSI( "qlTermStructureSettlementDays" ) ]
        =  STRFROMANSI( "Returns the number of settlement days for the given TermStructure object." );
    argName[ STRFROMANSI( "qlTermStructureSettlementDays" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlTermStructureSettlementDays" ) ].push_back( STRFROMANSI( "id of existing QuantLib::TermStructure object" ) );
    argName[ STRFROMANSI( "qlTermStructureSettlementDays" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlTermStructureSettlementDays" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlYieldTSParRate

    funcMap[ STRFROMANSI( "qlYieldTSParRate" ) ]
        =  STRFROMANSI( "qlYieldTSParRate" );
    funcDesc[ STRFROMANSI( "qlYieldTSParRate" ) ]
        =  STRFROMANSI( "Returns the par interest rate from the given YieldTermStructure object." );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::YieldTermStructure object" ) );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "tenor in years." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "start date." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "ResultDayCounter" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "resultDayCounter." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "AllowExtrapolation" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "TRUE allows extrapolation. Default value = false." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlYieldTSParRate2

    funcMap[ STRFROMANSI( "qlYieldTSParRate2" ) ]
        =  STRFROMANSI( "qlYieldTSParRate2" );
    funcDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ]
        =  STRFROMANSI( "Returns the par interest rate from the given YieldTermStructure object." );
    argName[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "id of existing QuantLib::YieldTermStructure object" ) );
    argName[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "Dates" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "par rate payment dates." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "ResultDayCounter" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "resultDayCounter." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "AllowExtrapolation" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "TRUE allows extrapolation. Default value = false." ) );
    argName[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlYieldTSParRate2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlZeroCurve

    funcMap[ STRFROMANSI( "qlZeroCurve" ) ]
        =  STRFROMANSI( "qlZeroCurve" );
    funcDesc[ STRFROMANSI( "qlZeroCurve" ) ]
        =  STRFROMANSI( "Construct an object of class ZeroCurve and return its id" );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "CurveDates" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "dates of the curve." ) );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "CurveYields" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "zero rates for the above dates." ) );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlZeroCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Utilities

    // qlAddinVersion

    funcMap[ STRFROMANSI( "qlAddinVersion" ) ]
        =  STRFROMANSI( "qlAddinVersion" );
    funcDesc[ STRFROMANSI( "qlAddinVersion" ) ]
        =  STRFROMANSI( "returns the version number of QuantLibAddin (a.k.a QuantLibObject)." );
    argName[ STRFROMANSI( "qlAddinVersion" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAddinVersion" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVersion

    funcMap[ STRFROMANSI( "qlVersion" ) ]
        =  STRFROMANSI( "qlVersion" );
    funcDesc[ STRFROMANSI( "qlVersion" ) ]
        =  STRFROMANSI( "returns the version number of QuantLib." );
    argName[ STRFROMANSI( "qlVersion" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVersion" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Vanilla Swap

    // qlMakeVanillaSwap

    funcMap[ STRFROMANSI( "qlMakeVanillaSwap" ) ]
        =  STRFROMANSI( "qlMakeVanillaSwap" );
    funcDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ]
        =  STRFROMANSI( "Construct an object of class VanillaSwap and return its id" );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "SwapTenor" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "swap tenor period (e.g. 5Y)." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "floating IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "the fixed leg rate. If missing atm rate is used. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "forward start period." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "fixed leg day counter. Default value = 30/360 (Bond Basis)." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "floating leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlVanillaSwap

    funcMap[ STRFROMANSI( "qlVanillaSwap" ) ]
        =  STRFROMANSI( "qlVanillaSwap" );
    funcDesc[ STRFROMANSI( "qlVanillaSwap" ) ]
        =  STRFROMANSI( "Construct an object of class VanillaSwap and return its id" );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "PayerReceiver" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "PAYER to pay the fixed rate, RECEIVER to receive it. Default value = Payer." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Nominal" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Notional Amount. Default value = 100." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "FixSchedule" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "fixed leg Schedule object ID." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "the fixed leg rate. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "fixed leg day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "FloatingLegSchedule" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "floating leg Schedule object ID." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "floating leg IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "floating leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "FloatingLegDayCounter" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "floating day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "PaymentConvention" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Payment dates' business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlVanillaSwapFairRate

    funcMap[ STRFROMANSI( "qlVanillaSwapFairRate" ) ]
        =  STRFROMANSI( "qlVanillaSwapFairRate" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFairRate" ) ]
        =  STRFROMANSI( "returns the fair fixed leg rate which would zero the swap NPV for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFairRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFairRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFairRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFairRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapFairSpread

    funcMap[ STRFROMANSI( "qlVanillaSwapFairSpread" ) ]
        =  STRFROMANSI( "qlVanillaSwapFairSpread" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFairSpread" ) ]
        =  STRFROMANSI( "returns the fair spread over the floating rate which would zero the swap NPV for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFairSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFairSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFairSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFairSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapFixedLegBPS

    funcMap[ STRFROMANSI( "qlVanillaSwapFixedLegBPS" ) ]
        =  STRFROMANSI( "qlVanillaSwapFixedLegBPS" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFixedLegBPS" ) ]
        =  STRFROMANSI( "returns the BPS of the fixed rate leg for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFixedLegBPS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedLegBPS" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFixedLegBPS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedLegBPS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapFixedLegNPV

    funcMap[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ]
        =  STRFROMANSI( "qlVanillaSwapFixedLegNPV" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ]
        =  STRFROMANSI( "returns the NPV of the fixed rate leg for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapFloatingLegBPS

    funcMap[ STRFROMANSI( "qlVanillaSwapFloatingLegBPS" ) ]
        =  STRFROMANSI( "qlVanillaSwapFloatingLegBPS" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFloatingLegBPS" ) ]
        =  STRFROMANSI( "returns the BPS of the floating rate leg for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapFloatingLegNPV

    funcMap[ STRFROMANSI( "qlVanillaSwapFloatingLegNPV" ) ]
        =  STRFROMANSI( "qlVanillaSwapFloatingLegNPV" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFloatingLegNPV" ) ]
        =  STRFROMANSI( "returns the NPV of the floating rate leg for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFloatingLegNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFloatingLegNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFloatingLegNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFloatingLegNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Volatilities

    // qlBlackConstantVol

    funcMap[ STRFROMANSI( "qlBlackConstantVol" ) ]
        =  STRFROMANSI( "qlBlackConstantVol" );
    funcDesc[ STRFROMANSI( "qlBlackConstantVol" ) ]
        =  STRFROMANSI( "Construct an object of class BlackConstantVol and return its id" );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "settlement date." ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Volatility" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "volatility." ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackConstantVol" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

}

