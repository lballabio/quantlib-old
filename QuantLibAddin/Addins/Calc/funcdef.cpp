
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
//      C:/Users/erik/Documents/repos/quantlib/gensrc/gensrc/stubs/stub.calc.map

#ifdef WIN32
#pragma warning(disable: 4503)
#pragma warning(disable: 4786)
#pragma warning(disable: 4800)
#endif

#include <qladdin.hpp>
#include <init.hpp>

CalcAddins_impl::CalcAddins_impl() throw ()  {

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

    // ohLogSetLevel

    funcMap[ STRFROMANSI( "ohLogSetLevel" ) ]
        =  STRFROMANSI( "ohLogSetLevel" );
    funcDesc[ STRFROMANSI( "ohLogSetLevel" ) ]
        =  STRFROMANSI( "set threshold for log messages." );
    argName[ STRFROMANSI( "ohLogSetLevel" ) ].push_back( STRFROMANSI( "LogLevel" ) );
    argDesc[ STRFROMANSI( "ohLogSetLevel" ) ].push_back( STRFROMANSI( "threshold for log messages." ) );
    argName[ STRFROMANSI( "ohLogSetLevel" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "ohLogSetLevel" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Serialization

    // ohObjectLoad

    funcMap[ STRFROMANSI( "ohObjectLoad" ) ]
        =  STRFROMANSI( "ohObjectLoad" );
    funcDesc[ STRFROMANSI( "ohObjectLoad" ) ]
        =  STRFROMANSI( "Deserialize list of objects from given file, return IDs of deserialized objects." );
    argName[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Directory" ) );
    argDesc[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Directory from which objects are to be deserialized." ) );
    argName[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Pattern" ) );
    argDesc[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Name of XML file from which objects are to be deserialized, or a pattern in UNIX format (wildcard is .*). Default value = .*\\.xml." ) );
    argName[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Recurse" ) );
    argDesc[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Recurse subdirectories of Directory when searching for filenames matching Pattern. Default value = false." ) );
    argName[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Overwrite any existing Object that has the same ID as one being loaded. Default value = false." ) );
    argName[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "ohObjectLoad" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // ohObjectSave

    funcMap[ STRFROMANSI( "ohObjectSave" ) ]
        =  STRFROMANSI( "ohObjectSave" );
    funcDesc[ STRFROMANSI( "ohObjectSave" ) ]
        =  STRFROMANSI( "Serialize list of objects to given path, return count of objects serialized." );
    argName[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "ObjectList" ) );
    argDesc[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "list of IDs of objects to be serialized." ) );
    argName[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "Filename" ) );
    argDesc[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "file name to which objects are to be serialized." ) );
    argName[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "overwrite the output file if it exists. Default value = false." ) );
    argName[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "IncludeGroups" ) );
    argDesc[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "include Groups in the serialisation. Default value = true." ) );
    argName[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "ohObjectSave" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Accounting Engines

    // qlAccountingEngine

    funcMap[ STRFROMANSI( "qlAccountingEngine" ) ]
        =  STRFROMANSI( "qlAccountingEngine" );
    funcDesc[ STRFROMANSI( "qlAccountingEngine" ) ]
        =  STRFROMANSI( "Construct an object of class AccountingEngine and return its id" );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "MarketModelEvolver" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "MarketModelEvolver object ID." ) );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "Product" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "MarketModelMultiProduct object ID." ) );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "InitialNumeraireValue" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "initial numeraire value." ) );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlAccountingEngineMultiplePathValues

    funcMap[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ]
        =  STRFROMANSI( "qlAccountingEngineMultiplePathValues" );
    funcDesc[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ]
        =  STRFROMANSI( "return multiple path values." );
    argName[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AccountingEngine object" ) );
    argName[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "SequenceStats" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "Sequence Statistics object ID." ) );
    argName[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "Paths" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "number of paths." ) );
    argName[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAccountingEngineMultiplePathValues" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Asset Swap

    // qlAssetSwap

    funcMap[ STRFROMANSI( "qlAssetSwap" ) ]
        =  STRFROMANSI( "qlAssetSwap" );
    funcDesc[ STRFROMANSI( "qlAssetSwap" ) ]
        =  STRFROMANSI( "Construct an object of class AssetSwap and return its id" );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "PayBondCoupon" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "TRUE to pay the bond's coupons and receive floating. Default value = false." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "Bond" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "underlying bond object ID." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "market price of the underlying bond." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "floating leg IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "Floating leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "FloatingLegSchedule" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "floating leg schedule object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "FloatingLegDayCounter" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "floating day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "ParAssetSwap" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "TRUE for par asset swap, FALSE for market asset swap. Default value = true." ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlAssetSwap2

    funcMap[ STRFROMANSI( "qlAssetSwap2" ) ]
        =  STRFROMANSI( "qlAssetSwap2" );
    funcDesc[ STRFROMANSI( "qlAssetSwap2" ) ]
        =  STRFROMANSI( "Construct an object of class AssetSwap and return its id" );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "ParAssetSwap" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "TRUE for par asset swap, FALSE for market asset swap. Default value = true." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Bond" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "underlying bond object ID." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "market price of the underlying bond." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "NonParRepayment" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "non par repayment on deal maturity date." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Gearing" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "gearing. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "floating leg IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Floating leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "FloatingLegDayCounter" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "floating day counter (e.g. Actual/360). Default value = QuantLib::DayCounter()." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "DealMaturity" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "deal maturity (bond maturity if missing). Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "PayBondCoupon" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "TRUE to pay the bond's coupons and receive floating. Default value = false." ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlAssetSwap2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlAssetSwapFairCleanPrice

    funcMap[ STRFROMANSI( "qlAssetSwapFairCleanPrice" ) ]
        =  STRFROMANSI( "qlAssetSwapFairCleanPrice" );
    funcDesc[ STRFROMANSI( "qlAssetSwapFairCleanPrice" ) ]
        =  STRFROMANSI( "the fair price of the bond in the asset swap." );
    argName[ STRFROMANSI( "qlAssetSwapFairCleanPrice" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFairCleanPrice" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapFairCleanPrice" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFairCleanPrice" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAssetSwapFairNonParRepayment

    funcMap[ STRFROMANSI( "qlAssetSwapFairNonParRepayment" ) ]
        =  STRFROMANSI( "qlAssetSwapFairNonParRepayment" );
    funcDesc[ STRFROMANSI( "qlAssetSwapFairNonParRepayment" ) ]
        =  STRFROMANSI( "the fair non par repayment of the bond in the asset swap." );
    argName[ STRFROMANSI( "qlAssetSwapFairNonParRepayment" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFairNonParRepayment" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapFairNonParRepayment" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFairNonParRepayment" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAssetSwapFairSpread

    funcMap[ STRFROMANSI( "qlAssetSwapFairSpread" ) ]
        =  STRFROMANSI( "qlAssetSwapFairSpread" );
    funcDesc[ STRFROMANSI( "qlAssetSwapFairSpread" ) ]
        =  STRFROMANSI( "the fair rate of the asset swap, i.e. the asset swap spread." );
    argName[ STRFROMANSI( "qlAssetSwapFairSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFairSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapFairSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFairSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAssetSwapFloatingLegAnalysis

    funcMap[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ]
        =  STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" );
    funcDesc[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ]
        =  STRFROMANSI( "The floating leg cash flow analysis." );
    argName[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ].push_back( STRFROMANSI( "AfterDate" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ].push_back( STRFROMANSI( "Shows only cashflows after given date Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFloatingLegAnalysis" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAssetSwapFloatingLegBPS

    funcMap[ STRFROMANSI( "qlAssetSwapFloatingLegBPS" ) ]
        =  STRFROMANSI( "qlAssetSwapFloatingLegBPS" );
    funcDesc[ STRFROMANSI( "qlAssetSwapFloatingLegBPS" ) ]
        =  STRFROMANSI( "the BPS of the floating leg." );
    argName[ STRFROMANSI( "qlAssetSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapFloatingLegBPS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAssetSwapParSwap

    funcMap[ STRFROMANSI( "qlAssetSwapParSwap" ) ]
        =  STRFROMANSI( "qlAssetSwapParSwap" );
    funcDesc[ STRFROMANSI( "qlAssetSwapParSwap" ) ]
        =  STRFROMANSI( "Returns TRUE if par swap" );
    argName[ STRFROMANSI( "qlAssetSwapParSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapParSwap" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapParSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapParSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlAssetSwapPayBondCoupon

    funcMap[ STRFROMANSI( "qlAssetSwapPayBondCoupon" ) ]
        =  STRFROMANSI( "qlAssetSwapPayBondCoupon" );
    funcDesc[ STRFROMANSI( "qlAssetSwapPayBondCoupon" ) ]
        =  STRFROMANSI( "Returns TRUE if it is a bond coupon payer swap" );
    argName[ STRFROMANSI( "qlAssetSwapPayBondCoupon" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapPayBondCoupon" ) ].push_back( STRFROMANSI( "id of existing QuantLib::AssetSwap object" ) );
    argName[ STRFROMANSI( "qlAssetSwapPayBondCoupon" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAssetSwapPayBondCoupon" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Bonds

    // qlBond

    funcMap[ STRFROMANSI( "qlBond" ) ]
        =  STRFROMANSI( "qlBond" );
    funcDesc[ STRFROMANSI( "qlBond" ) ]
        =  STRFROMANSI( "Construct an object of class Bond and return its id" );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Description" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Bond description string. Autogenerated if null Default value = std::string()." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "bond Currency. Default value = NullCurrency." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "settlement days. Default value = 3." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET). Default value = NullCalendar." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "MaturityDate" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "maturity date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "LegID" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "coupon vector Leg object ID." ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBond" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBondAccrualDays

    funcMap[ STRFROMANSI( "qlBondAccrualDays" ) ]
        =  STRFROMANSI( "qlBondAccrualDays" );
    funcDesc[ STRFROMANSI( "qlBondAccrualDays" ) ]
        =  STRFROMANSI( "Returns the total number of accrual days for the current coupon of the given bond. The current bond settlement is used if no date is given." );
    argName[ STRFROMANSI( "qlBondAccrualDays" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualDays" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccrualDays" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualDays" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccrualDays" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualDays" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAccrualEndDate

    funcMap[ STRFROMANSI( "qlBondAccrualEndDate" ) ]
        =  STRFROMANSI( "qlBondAccrualEndDate" );
    funcDesc[ STRFROMANSI( "qlBondAccrualEndDate" ) ]
        =  STRFROMANSI( "Returns the accrual end date for the current coupon of the given bond." );
    argName[ STRFROMANSI( "qlBondAccrualEndDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualEndDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccrualEndDate" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualEndDate" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccrualEndDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualEndDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAccrualPeriod

    funcMap[ STRFROMANSI( "qlBondAccrualPeriod" ) ]
        =  STRFROMANSI( "qlBondAccrualPeriod" );
    funcDesc[ STRFROMANSI( "qlBondAccrualPeriod" ) ]
        =  STRFROMANSI( "Returns the total accrual period for the current coupon of the given bond. The current bond settlement is used if no date is given." );
    argName[ STRFROMANSI( "qlBondAccrualPeriod" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualPeriod" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccrualPeriod" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualPeriod" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccrualPeriod" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualPeriod" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAccrualStartDate

    funcMap[ STRFROMANSI( "qlBondAccrualStartDate" ) ]
        =  STRFROMANSI( "qlBondAccrualStartDate" );
    funcDesc[ STRFROMANSI( "qlBondAccrualStartDate" ) ]
        =  STRFROMANSI( "Returns the accrual start date for the current coupon of the given bond." );
    argName[ STRFROMANSI( "qlBondAccrualStartDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualStartDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccrualStartDate" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualStartDate" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccrualStartDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccrualStartDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAccruedAmount

    funcMap[ STRFROMANSI( "qlBondAccruedAmount" ) ]
        =  STRFROMANSI( "qlBondAccruedAmount" );
    funcDesc[ STRFROMANSI( "qlBondAccruedAmount" ) ]
        =  STRFROMANSI( "Returns the accrued amount for the given bond. The current bond settlement is used if no date is given." );
    argName[ STRFROMANSI( "qlBondAccruedAmount" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedAmount" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccruedAmount" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedAmount" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccruedAmount" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedAmount" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAccruedDays

    funcMap[ STRFROMANSI( "qlBondAccruedDays" ) ]
        =  STRFROMANSI( "qlBondAccruedDays" );
    funcDesc[ STRFROMANSI( "qlBondAccruedDays" ) ]
        =  STRFROMANSI( "Returns the accrued days for the current coupon of the given bond. The current bond settlement is used if no date is given." );
    argName[ STRFROMANSI( "qlBondAccruedDays" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedDays" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccruedDays" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedDays" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccruedDays" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedDays" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAccruedPeriod

    funcMap[ STRFROMANSI( "qlBondAccruedPeriod" ) ]
        =  STRFROMANSI( "qlBondAccruedPeriod" );
    funcDesc[ STRFROMANSI( "qlBondAccruedPeriod" ) ]
        =  STRFROMANSI( "Returns the accrued period for the current coupon of the given bond. The current bond settlement is used if no date is given." );
    argName[ STRFROMANSI( "qlBondAccruedPeriod" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedPeriod" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAccruedPeriod" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedPeriod" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAccruedPeriod" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAccruedPeriod" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondAtmRateFromYieldTermStructure

    funcMap[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ]
        =  STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" );
    funcDesc[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ]
        =  STRFROMANSI( "Returns the ATM rate implied by the given YieldTermStructure, settlement date, and clean price." );
    argName[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "clean price." ) );
    argName[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondAtmRateFromYieldTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondBpsFromYield

    funcMap[ STRFROMANSI( "qlBondBpsFromYield" ) ]
        =  STRFROMANSI( "qlBondBpsFromYield" );
    funcDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ]
        =  STRFROMANSI( "Returns the basis point sensitivity implied by the given yield and settlement date." );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "Yield" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "bond yield." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYield" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondBpsFromYieldTermStructure

    funcMap[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ]
        =  STRFROMANSI( "qlBondBpsFromYieldTermStructure" );
    funcDesc[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ]
        =  STRFROMANSI( "Returns the basis point sensitivity implied by the given YieldTermStructure and settlement date." );
    argName[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondBpsFromYieldTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondCalendar

    funcMap[ STRFROMANSI( "qlBondCalendar" ) ]
        =  STRFROMANSI( "qlBondCalendar" );
    funcDesc[ STRFROMANSI( "qlBondCalendar" ) ]
        =  STRFROMANSI( "Returns the calendar of the bond, e.g. TARGET." );
    argName[ STRFROMANSI( "qlBondCalendar" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondCalendar" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondCalendar" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondCalendar" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondCleanPrice

    funcMap[ STRFROMANSI( "qlBondCleanPrice" ) ]
        =  STRFROMANSI( "qlBondCleanPrice" );
    funcDesc[ STRFROMANSI( "qlBondCleanPrice" ) ]
        =  STRFROMANSI( "Returns the clean price for the given bond." );
    argName[ STRFROMANSI( "qlBondCleanPrice" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPrice" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondCleanPrice" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPrice" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondCleanPriceFromYield

    funcMap[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ]
        =  STRFROMANSI( "qlBondCleanPriceFromYield" );
    funcDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ]
        =  STRFROMANSI( "Returns the bond clean price implied by the given yield and settlement date." );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "Yield" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "bond yield." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYield" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondCleanPriceFromYieldTermStructure

    funcMap[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ]
        =  STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" );
    funcDesc[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ]
        =  STRFROMANSI( "Returns the bond clean price implied by the given YieldTermStructure and settlement date." );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromYieldTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondCleanPriceFromZSpread

    funcMap[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ]
        =  STRFROMANSI( "qlBondCleanPriceFromZSpread" );
    funcDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ]
        =  STRFROMANSI( "Returns the bond clean price implied by the given Z-Spread, discount curve, and settlement date." );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "ZSpread" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "the Z-spread." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondCleanPriceFromZSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondConvexityFromYield

    funcMap[ STRFROMANSI( "qlBondConvexityFromYield" ) ]
        =  STRFROMANSI( "qlBondConvexityFromYield" );
    funcDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ]
        =  STRFROMANSI( "Returns the convexity implied by the given yield and settlement date." );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "Yield" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "bond yield." ) );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondConvexityFromYield" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondCurrency

    funcMap[ STRFROMANSI( "qlBondCurrency" ) ]
        =  STRFROMANSI( "qlBondCurrency" );
    funcDesc[ STRFROMANSI( "qlBondCurrency" ) ]
        =  STRFROMANSI( "Returns the bond currency." );
    argName[ STRFROMANSI( "qlBondCurrency" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondCurrency" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Bond object" ) );
    argName[ STRFROMANSI( "qlBondCurrency" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondCurrency" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondDescription

    funcMap[ STRFROMANSI( "qlBondDescription" ) ]
        =  STRFROMANSI( "qlBondDescription" );
    funcDesc[ STRFROMANSI( "qlBondDescription" ) ]
        =  STRFROMANSI( "Returns the bond description string." );
    argName[ STRFROMANSI( "qlBondDescription" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondDescription" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Bond object" ) );
    argName[ STRFROMANSI( "qlBondDescription" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondDescription" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondDirtyPriceFromYield

    funcMap[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ]
        =  STRFROMANSI( "qlBondDirtyPriceFromYield" );
    funcDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ]
        =  STRFROMANSI( "Returns the bond dirty price implied by the given yield and settlement date." );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "Yield" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "bond yield." ) );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondDirtyPriceFromYield" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondDurationFromYield

    funcMap[ STRFROMANSI( "qlBondDurationFromYield" ) ]
        =  STRFROMANSI( "qlBondDurationFromYield" );
    funcDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ]
        =  STRFROMANSI( "Returns the duration implied by the given yield and settlement date." );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Yield" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "bond yield." ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "DurationType" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Duration type (Simple, Macaulay, or Modified). Default value = Modified." ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondDurationFromYield" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondFlowAnalysis

    funcMap[ STRFROMANSI( "qlBondFlowAnalysis" ) ]
        =  STRFROMANSI( "qlBondFlowAnalysis" );
    funcDesc[ STRFROMANSI( "qlBondFlowAnalysis" ) ]
        =  STRFROMANSI( "Returns the bond cash flow analysis." );
    argName[ STRFROMANSI( "qlBondFlowAnalysis" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondFlowAnalysis" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Bond object" ) );
    argName[ STRFROMANSI( "qlBondFlowAnalysis" ) ].push_back( STRFROMANSI( "AfterDate" ) );
    argDesc[ STRFROMANSI( "qlBondFlowAnalysis" ) ].push_back( STRFROMANSI( "Shows only cashflows after given date Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondFlowAnalysis" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondFlowAnalysis" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondIsTradable

    funcMap[ STRFROMANSI( "qlBondIsTradable" ) ]
        =  STRFROMANSI( "qlBondIsTradable" );
    funcDesc[ STRFROMANSI( "qlBondIsTradable" ) ]
        =  STRFROMANSI( "Returns TRUE if the given Bond is tradable at the given settlement date. The current bond settlement is used if no date is given." );
    argName[ STRFROMANSI( "qlBondIsTradable" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondIsTradable" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondIsTradable" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondIsTradable" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondIsTradable" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondIsTradable" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondIssueDate

    funcMap[ STRFROMANSI( "qlBondIssueDate" ) ]
        =  STRFROMANSI( "qlBondIssueDate" );
    funcDesc[ STRFROMANSI( "qlBondIssueDate" ) ]
        =  STRFROMANSI( "Returns the issue date of the bond." );
    argName[ STRFROMANSI( "qlBondIssueDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondIssueDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondIssueDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondIssueDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondMaturityDate

    funcMap[ STRFROMANSI( "qlBondMaturityDate" ) ]
        =  STRFROMANSI( "qlBondMaturityDate" );
    funcDesc[ STRFROMANSI( "qlBondMaturityDate" ) ]
        =  STRFROMANSI( "Returns the maturity date of the bond." );
    argName[ STRFROMANSI( "qlBondMaturityDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondMaturityDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondMaturityDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondMaturityDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondNextCashFlowAmount

    funcMap[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ]
        =  STRFROMANSI( "qlBondNextCashFlowAmount" );
    funcDesc[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ]
        =  STRFROMANSI( "Returns the next cash flow date." );
    argName[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondNextCashFlowAmount" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondNextCashFlowDate

    funcMap[ STRFROMANSI( "qlBondNextCashFlowDate" ) ]
        =  STRFROMANSI( "qlBondNextCashFlowDate" );
    funcDesc[ STRFROMANSI( "qlBondNextCashFlowDate" ) ]
        =  STRFROMANSI( "Returns the next cash flow amount." );
    argName[ STRFROMANSI( "qlBondNextCashFlowDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondNextCashFlowDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondNextCashFlowDate" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondNextCashFlowDate" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondNextCashFlowDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondNextCashFlowDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondNextCouponRate

    funcMap[ STRFROMANSI( "qlBondNextCouponRate" ) ]
        =  STRFROMANSI( "qlBondNextCouponRate" );
    funcDesc[ STRFROMANSI( "qlBondNextCouponRate" ) ]
        =  STRFROMANSI( "Returns the next coupon rate. Depending on (the Bond and) the given date it can be historic, deterministic or expected in a stochastic sense. When the bond settlement date is used the coupon is the already-fixed not-yet-paid one." );
    argName[ STRFROMANSI( "qlBondNextCouponRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondNextCouponRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondNextCouponRate" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondNextCouponRate" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondNextCouponRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondNextCouponRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondNotional

    funcMap[ STRFROMANSI( "qlBondNotional" ) ]
        =  STRFROMANSI( "qlBondNotional" );
    funcDesc[ STRFROMANSI( "qlBondNotional" ) ]
        =  STRFROMANSI( "Returns the notional of the bond at a given date." );
    argName[ STRFROMANSI( "qlBondNotional" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondNotional" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondNotional" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondNotional" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondNotional" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondNotional" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondNotionals

    funcMap[ STRFROMANSI( "qlBondNotionals" ) ]
        =  STRFROMANSI( "qlBondNotionals" );
    funcDesc[ STRFROMANSI( "qlBondNotionals" ) ]
        =  STRFROMANSI( "Returns the notionals of the bond." );
    argName[ STRFROMANSI( "qlBondNotionals" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondNotionals" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondNotionals" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondNotionals" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondPreviousCashFlowAmount

    funcMap[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ]
        =  STRFROMANSI( "qlBondPreviousCashFlowAmount" );
    funcDesc[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ]
        =  STRFROMANSI( "Returns the previous cash flow amount." );
    argName[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCashFlowAmount" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondPreviousCashFlowDate

    funcMap[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ]
        =  STRFROMANSI( "qlBondPreviousCashFlowDate" );
    funcDesc[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ]
        =  STRFROMANSI( "Returns the previous cash flow date." );
    argName[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCashFlowDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondPreviousCouponRate

    funcMap[ STRFROMANSI( "qlBondPreviousCouponRate" ) ]
        =  STRFROMANSI( "qlBondPreviousCouponRate" );
    funcDesc[ STRFROMANSI( "qlBondPreviousCouponRate" ) ]
        =  STRFROMANSI( "Returns the previous coupon rate. Depending on (the Bond and) the given date it can be historic, deterministic or expected in a stochastic sense. When the bond settlement date is used the coupon is the last paid one." );
    argName[ STRFROMANSI( "qlBondPreviousCouponRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCouponRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondPreviousCouponRate" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCouponRate" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondPreviousCouponRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondPreviousCouponRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondReferencePeriodEnd

    funcMap[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ]
        =  STRFROMANSI( "qlBondReferencePeriodEnd" );
    funcDesc[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ]
        =  STRFROMANSI( "Returns the reference period end date for the current coupon of the given bond." );
    argName[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondReferencePeriodEnd" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondReferencePeriodStart

    funcMap[ STRFROMANSI( "qlBondReferencePeriodStart" ) ]
        =  STRFROMANSI( "qlBondReferencePeriodStart" );
    funcDesc[ STRFROMANSI( "qlBondReferencePeriodStart" ) ]
        =  STRFROMANSI( "Returns the reference period start date for the current coupon of the given bond." );
    argName[ STRFROMANSI( "qlBondReferencePeriodStart" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondReferencePeriodStart" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondReferencePeriodStart" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondReferencePeriodStart" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondReferencePeriodStart" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondReferencePeriodStart" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondSetCouponPricer

    funcMap[ STRFROMANSI( "qlBondSetCouponPricer" ) ]
        =  STRFROMANSI( "qlBondSetCouponPricer" );
    funcDesc[ STRFROMANSI( "qlBondSetCouponPricer" ) ]
        =  STRFROMANSI( "Set the coupon pricer at the given Bond object." );
    argName[ STRFROMANSI( "qlBondSetCouponPricer" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondSetCouponPricer" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Bond object" ) );
    argName[ STRFROMANSI( "qlBondSetCouponPricer" ) ].push_back( STRFROMANSI( "FloatingRateCouponPricer" ) );
    argDesc[ STRFROMANSI( "qlBondSetCouponPricer" ) ].push_back( STRFROMANSI( "FloatingRate coupon pricer object ID." ) );
    argName[ STRFROMANSI( "qlBondSetCouponPricer" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondSetCouponPricer" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondSetCouponPricers

    funcMap[ STRFROMANSI( "qlBondSetCouponPricers" ) ]
        =  STRFROMANSI( "qlBondSetCouponPricers" );
    funcDesc[ STRFROMANSI( "qlBondSetCouponPricers" ) ]
        =  STRFROMANSI( "Set the coupon pricer at the given Bond object." );
    argName[ STRFROMANSI( "qlBondSetCouponPricers" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondSetCouponPricers" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Bond object" ) );
    argName[ STRFROMANSI( "qlBondSetCouponPricers" ) ].push_back( STRFROMANSI( "FloatingRateCouponPricer" ) );
    argDesc[ STRFROMANSI( "qlBondSetCouponPricers" ) ].push_back( STRFROMANSI( "FloatingRate coupon pricer object ID." ) );
    argName[ STRFROMANSI( "qlBondSetCouponPricers" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondSetCouponPricers" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondSettlementDate

    funcMap[ STRFROMANSI( "qlBondSettlementDate" ) ]
        =  STRFROMANSI( "qlBondSettlementDate" );
    funcDesc[ STRFROMANSI( "qlBondSettlementDate" ) ]
        =  STRFROMANSI( "Returns the settlement date of the bond." );
    argName[ STRFROMANSI( "qlBondSettlementDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondSettlementDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondSettlementDate" ) ].push_back( STRFROMANSI( "EvaluationDate" ) );
    argDesc[ STRFROMANSI( "qlBondSettlementDate" ) ].push_back( STRFROMANSI( "The current global Settings::EvaluationDate is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondSettlementDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondSettlementDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondSettlementDays

    funcMap[ STRFROMANSI( "qlBondSettlementDays" ) ]
        =  STRFROMANSI( "qlBondSettlementDays" );
    funcDesc[ STRFROMANSI( "qlBondSettlementDays" ) ]
        =  STRFROMANSI( "Returns the number of settlement days of the bond." );
    argName[ STRFROMANSI( "qlBondSettlementDays" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondSettlementDays" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondSettlementDays" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondSettlementDays" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondStartDate

    funcMap[ STRFROMANSI( "qlBondStartDate" ) ]
        =  STRFROMANSI( "qlBondStartDate" );
    funcDesc[ STRFROMANSI( "qlBondStartDate" ) ]
        =  STRFROMANSI( "Returns the start (i.e. first accrual) date for the given Bond object." );
    argName[ STRFROMANSI( "qlBondStartDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondStartDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondStartDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondStartDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondYieldFromCleanPrice

    funcMap[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ]
        =  STRFROMANSI( "qlBondYieldFromCleanPrice" );
    funcDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ]
        =  STRFROMANSI( "Returns the yield for the given bond corresponding to the given clean price and settlement date." );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "clean price." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Accuracy" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "tolerance. Default value = 1.0e-10." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "MaxIterations" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "max number of iterations. Default value = 100." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Guess" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "guess. Default value = 0.05." ) );
    argName[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondYieldFromCleanPrice" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlBondZSpreadFromCleanPrice

    funcMap[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ]
        =  STRFROMANSI( "qlBondZSpreadFromCleanPrice" );
    funcDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ]
        =  STRFROMANSI( "Returns the z-spread for the given bond corresponding to the given clean price and settlement date." );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Bond object" ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "CleanPrice" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "clean price." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Yield DayCounter ID. Default value = Actual/Actual (ISDA)." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Compounded." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "SettlementDate" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "The current bond settlement date is used if no specific date is given. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Accuracy" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "tolerance. Default value = 1.0e-10." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "MaxIterations" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "max number of iterations. Default value = 100." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Guess" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "guess. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBondZSpreadFromCleanPrice" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCmsRateBond

    funcMap[ STRFROMANSI( "qlCmsRateBond" ) ]
        =  STRFROMANSI( "qlCmsRateBond" );
    funcDesc[ STRFROMANSI( "qlCmsRateBond" ) ]
        =  STRFROMANSI( "Construct an object of class CmsRateBond and return its id" );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Description" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Bond description string. Autogenerated if null Default value = std::string()." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "bond Currency." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "settlement days." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2). Default value = QuantLib::Null<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Floors" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "floor strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "floating swap rate index." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Caps" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "cap strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Redemption" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Redemption value. Default value = 100." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCmsRateBond" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFixedRateBond

    funcMap[ STRFROMANSI( "qlFixedRateBond" ) ]
        =  STRFROMANSI( "qlFixedRateBond" );
    funcDesc[ STRFROMANSI( "qlFixedRateBond" ) ]
        =  STRFROMANSI( "Construct an object of class FixedRateBond and return its id" );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Description" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Bond description string. Autogenerated if null Default value = std::string()." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "bond Currency." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "settlement days." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Coupons" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "simple annual compounding coupon rates." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Redemption" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Redemption value. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "PaymentCalendar" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "payment holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFixedRateBond2

    funcMap[ STRFROMANSI( "qlFixedRateBond2" ) ]
        =  STRFROMANSI( "qlFixedRateBond2" );
    funcDesc[ STRFROMANSI( "qlFixedRateBond2" ) ]
        =  STRFROMANSI( "Construct an object of class FixedRateBond and return its id" );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Description" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Bond description string. Autogenerated if null Default value = std::string()." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "bond Currency." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "settlement days." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Coupons" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "coupon InterestRate IDs." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Redemption" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Redemption value. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "PaymentCalendar" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "payment holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBond2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFloatingRateBond

    funcMap[ STRFROMANSI( "qlFloatingRateBond" ) ]
        =  STRFROMANSI( "qlFloatingRateBond" );
    funcDesc[ STRFROMANSI( "qlFloatingRateBond" ) ]
        =  STRFROMANSI( "Construct an object of class FloatingRateBond and return its id" );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Description" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Bond description string. Autogenerated if null Default value = std::string()." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "bond Currency." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "settlement days." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2). Default value = QuantLib::Null<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Floors" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "floor strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "floating rate index." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Caps" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "cap strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Redemption" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Redemption value. Default value = 100." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFloatingRateBond" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlZeroCouponBond

    funcMap[ STRFROMANSI( "qlZeroCouponBond" ) ]
        =  STRFROMANSI( "qlZeroCouponBond" );
    funcDesc[ STRFROMANSI( "qlZeroCouponBond" ) ]
        =  STRFROMANSI( "Construct an object of class ZeroCouponBond and return its id" );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Description" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Bond description string. Autogenerated if null Default value = std::string()." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "bond Currency." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "settlement days." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "FaceAmount" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Face nominal amount. Default value = 100.0." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Maturity" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "maturity date." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Redemption" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Redemption value. Default value = 100." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "IssueDate" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "issue date: the bond can't be traded until then. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlZeroCouponBond" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Brownian Generator

    // qlMTBrownianGeneratorFactory

    funcMap[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ]
        =  STRFROMANSI( "qlMTBrownianGeneratorFactory" );
    funcDesc[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ]
        =  STRFROMANSI( "Construct an object of class MTBrownianGeneratorFactory and return its id" );
    argName[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "Seed" ) );
    argDesc[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "seed for random sequence generator. Default value = 0." ) );
    argName[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMTBrownianGeneratorFactory" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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

    // qlCalendarAdjust

    funcMap[ STRFROMANSI( "qlCalendarAdjust" ) ]
        =  STRFROMANSI( "qlCalendarAdjust" );
    funcDesc[ STRFROMANSI( "qlCalendarAdjust" ) ]
        =  STRFROMANSI( "Adjusts a non-business day to the appropriate near business day according to a given calendar with respect to the given convention." );
    argName[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "date to be adjusted." ) );
    argName[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "BusinessDayConvention" ) );
    argDesc[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "rolling convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarAdjust" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarBusinessDaysBetween

    funcMap[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ]
        =  STRFROMANSI( "qlCalendarBusinessDaysBetween" );
    funcDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ]
        =  STRFROMANSI( "Returns the number of business days between two dates." );
    argName[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "FirstDate" ) );
    argDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "first date of the period." ) );
    argName[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "LastDate" ) );
    argDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "last date of the period." ) );
    argName[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "IncludeFirst" ) );
    argDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "include the first date when counting business days. Default value = false." ) );
    argName[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "IncludeLast" ) );
    argDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "include the last date when counting business days. Default value = false." ) );
    argName[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarBusinessDaysBetween" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarEndOfMonth

    funcMap[ STRFROMANSI( "qlCalendarEndOfMonth" ) ]
        =  STRFROMANSI( "qlCalendarEndOfMonth" );
    funcDesc[ STRFROMANSI( "qlCalendarEndOfMonth" ) ]
        =  STRFROMANSI( "returns the last business day in the given calendar of the month to which the given date belongs." );
    argName[ STRFROMANSI( "qlCalendarEndOfMonth" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarEndOfMonth" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarEndOfMonth" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarEndOfMonth" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlCalendarEndOfMonth" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarEndOfMonth" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarHolidayList

    funcMap[ STRFROMANSI( "qlCalendarHolidayList" ) ]
        =  STRFROMANSI( "qlCalendarHolidayList" );
    funcDesc[ STRFROMANSI( "qlCalendarHolidayList" ) ]
        =  STRFROMANSI( "returns the holidays in a period between two dates according to a given holiday calendar." );
    argName[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "Calendar to use for holiday determination." ) );
    argName[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "FromDate" ) );
    argDesc[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "first date of the period." ) );
    argName[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "ToDate" ) );
    argDesc[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "last date of the period." ) );
    argName[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "IncludeWeekEnds" ) );
    argDesc[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "include week-end as holidays. Default value = false." ) );
    argName[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarHolidayList" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarIsBusinessDay

    funcMap[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ]
        =  STRFROMANSI( "qlCalendarIsBusinessDay" );
    funcDesc[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ]
        =  STRFROMANSI( "Returns TRUE if the date is a business day for the given calendar." );
    argName[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsBusinessDay" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarIsEndOfMonth

    funcMap[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ]
        =  STRFROMANSI( "qlCalendarIsEndOfMonth" );
    funcDesc[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ]
        =  STRFROMANSI( "returns TRUE if the date is last business day for the month in the given calendar." );
    argName[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsEndOfMonth" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCalendarIsHoliday

    funcMap[ STRFROMANSI( "qlCalendarIsHoliday" ) ]
        =  STRFROMANSI( "qlCalendarIsHoliday" );
    funcDesc[ STRFROMANSI( "qlCalendarIsHoliday" ) ]
        =  STRFROMANSI( "returns TRUE if the date is a holiday for the given calendar." );
    argName[ STRFROMANSI( "qlCalendarIsHoliday" ) ].push_back( STRFROMANSI( "calendar" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsHoliday" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::Calendar" ) );
    argName[ STRFROMANSI( "qlCalendarIsHoliday" ) ].push_back( STRFROMANSI( "Date" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsHoliday" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlCalendarIsHoliday" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCalendarIsHoliday" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // Correlation

    // qlCotSwapFromFwdCorrelation

    funcMap[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ]
        =  STRFROMANSI( "qlCotSwapFromFwdCorrelation" );
    funcDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ]
        =  STRFROMANSI( "Construct an object of class CotSwapFromFwdCorrelation and return its id" );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "FwdCorr" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "Forward rates correlation (i.e. PiecewiseConstantCorrelation object ID)." ) );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "curveState." ) );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "displacement. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCotSwapFromFwdCorrelation" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlExponentialCorrelations

    funcMap[ STRFROMANSI( "qlExponentialCorrelations" ) ]
        =  STRFROMANSI( "qlExponentialCorrelations" );
    funcDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ]
        =  STRFROMANSI( "Returns the correlation matrix (time dependent long term + beta + gamma exponential functional form)." );
    argName[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "rate fixing times." ) );
    argName[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "LongTermCorr" ) );
    argDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "Long term correlation . Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "Beta" ) );
    argDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "exponential decay factor. Default value = 0.24." ) );
    argName[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "Gamma" ) );
    argDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "exponent for time to go. Default value = 0.333." ) );
    argName[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "Time" ) );
    argDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "time t ." ) );
    argName[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlExponentialCorrelations" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlExponentialForwardCorrelation

    funcMap[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ]
        =  STRFROMANSI( "qlExponentialForwardCorrelation" );
    funcDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ]
        =  STRFROMANSI( "Construct an object of class ExponentialForwardCorrelation and return its id" );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "rate times." ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "LongTermCorr" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Long term correlation ." ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Beta" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "exponential decay factor." ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Gamma" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "exponent for time to go." ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Times" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "times." ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlExponentialForwardCorrelation" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMarketModelLmLinearExponentialCorrelationModel

    funcMap[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ]
        =  STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" );
    funcDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ]
        =  STRFROMANSI( "Construct an object of class LmLinearExponentialCorrelationModel and return its id" );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Size" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "size." ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Rho" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "rho." ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Beta" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "beta." ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Factors" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "factors. Default value = QuantLib::Null<QuantLib::Size>()." ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmLinearExponentialCorrelationModel" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlPiecewiseConstantCorrelationCorrelation

    funcMap[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ]
        =  STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" );
    funcDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ]
        =  STRFROMANSI( "Returns the pseudo-root of the equivalent covariance swap rates matrix." );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ].push_back( STRFROMANSI( "id of existing QuantLib::PiecewiseConstantCorrelation object" ) );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ].push_back( STRFROMANSI( "TimeIndex" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ].push_back( STRFROMANSI( "time index." ) );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationCorrelation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlPiecewiseConstantCorrelationNumberOfRates

    funcMap[ STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" ) ]
        =  STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" );
    funcDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" ) ]
        =  STRFROMANSI( "Piecewise Constant Correlation Number of Rates." );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" ) ].push_back( STRFROMANSI( "id of existing QuantLib::PiecewiseConstantCorrelation object" ) );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationNumberOfRates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlPiecewiseConstantCorrelationTimes

    funcMap[ STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" ) ]
        =  STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" );
    funcDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" ) ]
        =  STRFROMANSI( "Piecewise Constant Correlation Times." );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" ) ].push_back( STRFROMANSI( "id of existing QuantLib::PiecewiseConstantCorrelation object" ) );
    argName[ STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseConstantCorrelationTimes" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlTimeHomogeneousForwardCorrelation

    funcMap[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ]
        =  STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" );
    funcDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ]
        =  STRFROMANSI( "Construct an object of class TimeHomogeneousForwardCorrelation and return its id" );
    argName[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "FwdCorrMatrix" ) );
    argDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "forward correlation matrix." ) );
    argName[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "rate times." ) );
    argName[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlTimeHomogeneousForwardCorrelation" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Coupon Vectors

    // qlCmsCouponPricer

    funcMap[ STRFROMANSI( "qlCmsCouponPricer" ) ]
        =  STRFROMANSI( "qlCmsCouponPricer" );
    funcDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ]
        =  STRFROMANSI( "Construct an object of class CmsCouponPricer and return its id" );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "Volatility" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "Swaption Volatility Structure object ID." ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "CmsCouponPricerType" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "Cms Coupon Pricer Type (e.g ConundrumByBlack, ConundrumByNumericalIntegration, ClassicalAnaliticalFormula)." ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "YieldCurveModel" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "model of the yield curve (e.g Standard, ExactYield, ParallelShifts, NonParallelShifts)." ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "MeanReversion" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "mean reversion quote." ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCmsCouponPricer" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlCmsLeg

    funcMap[ STRFROMANSI( "qlCmsLeg" ) ]
        =  STRFROMANSI( "qlCmsLeg" );
    funcDesc[ STRFROMANSI( "qlCmsLeg" ) ]
        =  STRFROMANSI( "Construct an object of class CmsLeg and return its id" );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Nominal amount vector." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2). Default value = std::vector<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Floors" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "floor strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "underlying SwapIndex object ID." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Caps" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "cap strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCmsLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlCmsZeroLeg

    funcMap[ STRFROMANSI( "qlCmsZeroLeg" ) ]
        =  STRFROMANSI( "qlCmsZeroLeg" );
    funcDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ]
        =  STRFROMANSI( "Construct an object of class CmsZeroLeg and return its id" );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Nominal amount vector." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2). Default value = std::vector<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Floors" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "floor strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "underlying SwapIndex object ID." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Caps" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "cap strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCmsZeroLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlConundrumPricerByNumericalIntegration

    funcMap[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ]
        =  STRFROMANSI( "qlConundrumPricerByNumericalIntegration" );
    funcDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ]
        =  STRFROMANSI( "Construct an object of class NumericHaganPricer and return its id" );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "SwaptionVol" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "Volatility Cube by Sabr." ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "YieldCurveModel" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "model of the yield curve (e.g Standard, ExactYield, ParallelShifts, NonParallelShifts)." ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "MeanReversion" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "mean reversion." ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "LowerLimit" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "lower limit. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "UpperLimit" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "upper limit. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "Precision" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "precision. Default value = 1.0e-6." ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlConundrumPricerByNumericalIntegration" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlDigitalCmsLeg

    funcMap[ STRFROMANSI( "qlDigitalCmsLeg" ) ]
        =  STRFROMANSI( "qlDigitalCmsLeg" );
    funcDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ]
        =  STRFROMANSI( "Construct an object of class DigitalCmsLeg and return its id" );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "nominal amount vector." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "schedule object ID." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "fixing days. Default value = std::vector<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "swap rate index." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "CallStrikes" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "option strikes. If omitted, no call is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "CallSpecs" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "call position (Short, Long) and ATM inclusion." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "CallPayoff" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "payoff if the call ends ITM. If omitted, asset-or-nothing option is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "PutStrikes" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "option strikes. If omitted, no put is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "PutSpecs" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "put position (Short, Long) and ATM inclusion." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "PutPayoff" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "payoff if the put ends ITM. If omitted, asset-or-nothing option is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Replication" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "replication object ID." ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDigitalCmsLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlDigitalIborLeg

    funcMap[ STRFROMANSI( "qlDigitalIborLeg" ) ]
        =  STRFROMANSI( "qlDigitalIborLeg" );
    funcDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ]
        =  STRFROMANSI( "Construct an object of class DigitalIborLeg and return its id" );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "nominal amount vector." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "schedule object ID." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "fixing days. Default value = std::vector<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "floating rate index." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "CallStrikes" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "option strikes. If omitted, no call is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "CallSpecs" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "call position (Short, Long) and ATM inclusion." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "CallPayoff" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "payoff if the call ends ITM. If omitted, asset-or-nothing option is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "PutStrikes" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "option strikes. If omitted, no put is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "PutSpecs" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "put position (Short, Long) and ATM inclusion." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "PutPayoff" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "payoff if the put ends ITM. If omitted, asset-or-nothing option is assumed. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Replication" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "replication object ID." ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDigitalIborLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFixedRateLeg

    funcMap[ STRFROMANSI( "qlFixedRateLeg" ) ]
        =  STRFROMANSI( "qlFixedRateLeg" );
    funcDesc[ STRFROMANSI( "qlFixedRateLeg" ) ]
        =  STRFROMANSI( "Construct an object of class FixedRateLeg and return its id" );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Nominal amount vector." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Coupons" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "coupon fixed rates." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFixedRateLeg2

    funcMap[ STRFROMANSI( "qlFixedRateLeg2" ) ]
        =  STRFROMANSI( "qlFixedRateLeg2" );
    funcDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ]
        =  STRFROMANSI( "Construct an object of class FixedRateLeg and return its id" );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Nominal amount vector." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Coupons" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "coupon InterestRate IDs." ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFixedRateLeg2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlIborCouponPricer

    funcMap[ STRFROMANSI( "qlIborCouponPricer" ) ]
        =  STRFROMANSI( "qlIborCouponPricer" );
    funcDesc[ STRFROMANSI( "qlIborCouponPricer" ) ]
        =  STRFROMANSI( "Construct an object of class IborCouponPricer and return its id" );
    argName[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "Volatility" ) );
    argDesc[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "Caplet Volatility Structure object ID." ) );
    argName[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "IborCouponPricerType" ) );
    argDesc[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "Ibor Coupon Pricer Type (e.g IborByBlack, ..)." ) );
    argName[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlIborCouponPricer" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlIborLeg

    funcMap[ STRFROMANSI( "qlIborLeg" ) ]
        =  STRFROMANSI( "qlIborLeg" );
    funcDesc[ STRFROMANSI( "qlIborLeg" ) ]
        =  STRFROMANSI( "Construct an object of class IborLeg and return its id" );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Nominal amount vector." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2). Default value = std::vector<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "IsInArrears" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "TRUE if the fixing is in arrears. Default value = false." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Floors" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "floor strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "floating rate index." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Caps" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "cap strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlIborLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRangeAccrualLeg

    funcMap[ STRFROMANSI( "qlRangeAccrualLeg" ) ]
        =  STRFROMANSI( "qlRangeAccrualLeg" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ]
        =  STRFROMANSI( "Construct an object of class RangeAccrualLeg and return its id" );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "PaymentBDC" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "payment business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Nominals" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Nominal amount vector." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "ScheduleID" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Schedule object ID." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "fixing days (e.g. 2). Default value = std::vector<QuantLib::Natural>()." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Payment DayCounter ID." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "LowerStrikes" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "lower strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "floating rate gearings. Default value = 1.0." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "underlying IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "floating rate spreads. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "UpperStrikes" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "upper strikes. Default value = std::vector<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "ObservationsTenor" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "observations tenor period." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "ObservationsBDC" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "observations business day convention. Default value = Modified Following." ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // QuantLib Credit

    // qlBlackCdsOptionEngine

    funcMap[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ]
        =  STRFROMANSI( "qlBlackCdsOptionEngine" );
    funcDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ]
        =  STRFROMANSI( "Construct an object of class BlackCdsOptionEngine and return its id" );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "DefaultCurve" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "default term structure object ID." ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "RecoveryRate" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "constant recovery rate" ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "BlackVol" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "Black Volatility." ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackCdsOptionEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlCDSOption

    funcMap[ STRFROMANSI( "qlCDSOption" ) ]
        =  STRFROMANSI( "qlCDSOption" );
    funcDesc[ STRFROMANSI( "qlCDSOption" ) ]
        =  STRFROMANSI( "Construct an object of class CdsOption and return its id" );
    argName[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "UnderlyingCDS" ) );
    argDesc[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "The CDS underlying the option." ) );
    argName[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "Exercise" ) );
    argDesc[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "Exercise object ID." ) );
    argName[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCDSOption" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlCreditDefaultSwap

    funcMap[ STRFROMANSI( "qlCreditDefaultSwap" ) ]
        =  STRFROMANSI( "qlCreditDefaultSwap" );
    funcDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ]
        =  STRFROMANSI( "Construct an object of class CreditDefaultSwap and return its id" );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "BuyerSeller" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "BUYER for bought, SELLER for sold protection. Default value = Buyer." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Notional" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Nominal amount" ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Upfront" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "upfront in fractional units" ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "running spread in fractional units" ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "PremiumSchedule" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "premium leg Schedule object ID." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "PaymentConvention" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Payment dates' business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "premium leg day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "SettlesAccrual" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "TRUE ensures settlement of accural. Default value = true." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "PayAtDefault" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "TRUE ensures payment at default time Default value = true." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "ProtectionStart" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "protection start date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "UpfrontDate" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "upfront date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCreditDefaultSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlHazardRateCurve

    funcMap[ STRFROMANSI( "qlHazardRateCurve" ) ]
        =  STRFROMANSI( "qlHazardRateCurve" );
    funcDesc[ STRFROMANSI( "qlHazardRateCurve" ) ]
        =  STRFROMANSI( "Construct an object of class HazardRateCurve and return its id" );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "CurveDates" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "dates of the curve. First date corresponds to a survival probability of one." ) );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "CurveRates" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "hazard rates for the above dates." ) );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlHazardRateCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMidPointCdsEngine

    funcMap[ STRFROMANSI( "qlMidPointCdsEngine" ) ]
        =  STRFROMANSI( "qlMidPointCdsEngine" );
    funcDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ]
        =  STRFROMANSI( "Construct an object of class MidPointCdsEngine and return its id" );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "DefaultCurve" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "default term structure object ID." ) );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "RecoveryRate" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "constant recovery rate" ) );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMidPointCdsEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlPiecewiseFlatForwardCurve

    funcMap[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ]
        =  STRFROMANSI( "qlPiecewiseFlatForwardCurve" );
    funcDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ]
        =  STRFROMANSI( "Construct an object of class PiecewiseFlatForwardCurve and return its id" );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "ReferenceDate" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "term structure reference date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "RateHelpers" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "vector of rate-helpers." ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "Accuracy" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "Bootstrapping accuracy. Default value = 1.0e-12." ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseFlatForwardCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlPiecewiseHazardRateCurve

    funcMap[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ]
        =  STRFROMANSI( "qlPiecewiseHazardRateCurve" );
    funcDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ]
        =  STRFROMANSI( "Construct an object of class PiecewiseHazardRateCurve and return its id" );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Helpers" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "vector of default probability helpers." ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Interpolation" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "The interpolator for hazard rates." ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Accuracy" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Bootstrapping accuracy. Default value = 1.0e-12." ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlPiecewiseHazardRateCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRiskyFixedBond

    funcMap[ STRFROMANSI( "qlRiskyFixedBond" ) ]
        =  STRFROMANSI( "qlRiskyFixedBond" );
    funcDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ]
        =  STRFROMANSI( "Construct an object of class RiskyFixedBond and return its id" );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Bondname" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Bonds id." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Curency of the reference bond affected." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Recovery" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Recovery Rate." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "DefaultCurve" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "default term structure object ID." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Schedule" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Schedule." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Rate." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "premium leg day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "PaymentConvention" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Payment dates' business day convention. Default value = Following." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Notional" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Notional." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "DiscountingCurve" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "PricingDate" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "desired npv date." ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRiskyFixedBond" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSpreadCdsHelper

    funcMap[ STRFROMANSI( "qlSpreadCdsHelper" ) ]
        =  STRFROMANSI( "qlSpreadCdsHelper" );
    funcDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ]
        =  STRFROMANSI( "Construct an object of class SpreadCdsHelper and return its id" );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "RunningSpread" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "quote." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "CDS length (e.g. 5Y for five years)." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "settlement days Default value = 0." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "payment frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly)." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "PaymentConvention" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "payment leg convention (e.g. Unadjusted)." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "GenRule" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Date generation rule (Backward, Forward, ThirdWednesday, Twentieth, TwentiethIMM, Zero)." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "RecoveryRate" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "recovery rate" ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "DiscountingCurve" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "SettleAccrual" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "TRUE ensures settlement of accural. Default value = true." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "PayAtDefault" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "TRUE ensures payment at default time Default value = true." ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSpreadCdsHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlUpfrontCdsHelper

    funcMap[ STRFROMANSI( "qlUpfrontCdsHelper" ) ]
        =  STRFROMANSI( "qlUpfrontCdsHelper" );
    funcDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ]
        =  STRFROMANSI( "Construct an object of class UpfrontCdsHelper and return its id" );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "UpfrontSpread" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "upfront spread quote." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "RunningSpread" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "running spread." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "CDS length (e.g. 5Y for five years)." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "SettlementDays" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "settlement days Default value = 0." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "payment frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly)." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "PaymentConvention" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "payment leg convention (e.g. Unadjusted)." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "GenRule" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Date generation rule (Backward, Forward, ThirdWednesday, Twentieth, TwentiethIMM, Zero)." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "day counter (e.g. Actual/360)." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "RecRate" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "recovery rate" ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "DiscCurve" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "UpfSettlDays" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "upfront settlement days" ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "SettlAccr" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "TRUE ensures settlement of accural. Default value = true." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "PayAtDefault" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "TRUE ensures payment at default time Default value = true." ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlUpfrontCdsHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Daycounter

    // qlDayCounterDayCount

    funcMap[ STRFROMANSI( "qlDayCounterDayCount" ) ]
        =  STRFROMANSI( "qlDayCounterDayCount" );
    funcDesc[ STRFROMANSI( "qlDayCounterDayCount" ) ]
        =  STRFROMANSI( "calculate the number of days in a period according to a given day count convention." );
    argName[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "daycounter" ) );
    argDesc[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::DayCounter" ) );
    argName[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "start date." ) );
    argName[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "end date." ) );
    argName[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDayCounterDayCount" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlDayCounterName

    funcMap[ STRFROMANSI( "qlDayCounterName" ) ]
        =  STRFROMANSI( "qlDayCounterName" );
    funcDesc[ STRFROMANSI( "qlDayCounterName" ) ]
        =  STRFROMANSI( "returns the name of the given DayCounter." );
    argName[ STRFROMANSI( "qlDayCounterName" ) ].push_back( STRFROMANSI( "daycounter" ) );
    argDesc[ STRFROMANSI( "qlDayCounterName" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::DayCounter" ) );
    argName[ STRFROMANSI( "qlDayCounterName" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDayCounterName" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlDayCounterYearFraction

    funcMap[ STRFROMANSI( "qlDayCounterYearFraction" ) ]
        =  STRFROMANSI( "qlDayCounterYearFraction" );
    funcDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ]
        =  STRFROMANSI( "calculate a year fraction." );
    argName[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "daycounter" ) );
    argDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "ID of Enumeration of class QuantLib::DayCounter" ) );
    argName[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "start date." ) );
    argName[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "end date." ) );
    argName[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "RefPeriodStart" ) );
    argDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "start date for reference period. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "RefPeriodEnd" ) );
    argDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "end date for reference period. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDayCounterYearFraction" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // QuantLib Credit Basket

    // qlCreditBasket

    funcMap[ STRFROMANSI( "qlCreditBasket" ) ]
        =  STRFROMANSI( "qlCreditBasket" );
    funcDesc[ STRFROMANSI( "qlCreditBasket" ) ]
        =  STRFROMANSI( "Construct an object of class Basket and return its id" );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "IssuerNames" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Array containing the issuer names in the basket." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Issuers" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Array of Issuers." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Notionals" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Notional by which each name enters the basket." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "ReferenceDate" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Basket inception date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "AttachmentRatio" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Attachment ratio for losses affecting the basket." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "DettachmentRatio" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Dettachment ratio for losses affecting the basket." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Amortizing" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Whether is Quarterly amortizing." ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCreditBasket" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Default Probability Term Structures

    // qlDefaultTSDefaultProbability

    funcMap[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ]
        =  STRFROMANSI( "qlDefaultTSDefaultProbability" );
    funcDesc[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ]
        =  STRFROMANSI( "Returns the probability of default between the reference date and the given date from the given DefaultProbabilityTermStructure object." );
    argName[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "id of existing QuantLib::DefaultProbabilityTermStructure object" ) );
    argName[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "Dates" ) );
    argDesc[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "vector of dates." ) );
    argName[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "AllowExtrapolation" ) );
    argDesc[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "TRUE allows extrapolation. Default value = false." ) );
    argName[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDefaultTSDefaultProbability" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlRelinkableHandleDefaultProbabilityTermStructure

    funcMap[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ]
        =  STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" );
    funcDesc[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ]
        =  STRFROMANSI( "Construct an object of class RelinkableHandleImpl<QuantLibAddin::DefaultProbabilityTermStructure, QuantLib::DefaultProbabilityTermStructure> and return its id" );
    argName[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "CurrentLink" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "DefaultProbabilityTermStructure object ID. If omitted, nothing is linked by the RelinkableHandle. Default value = ." ) );
    argName[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRelinkableHandleDefaultProbabilityTermStructure" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Evolution Description

    // qlEvolutionDescription

    funcMap[ STRFROMANSI( "qlEvolutionDescription" ) ]
        =  STRFROMANSI( "qlEvolutionDescription" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescription" ) ]
        =  STRFROMANSI( "Construct an object of class EvolutionDescription and return its id" );
    argName[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "rate fixing times." ) );
    argName[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "EvolutionTimes" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "evolution times in the simulation." ) );
    argName[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescription" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlEvolutionDescriptionEvolutionTimes

    funcMap[ STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" ) ]
        =  STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" ) ]
        =  STRFROMANSI( "evolution times for the EvolutionDescription object." );
    argName[ STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" ) ].push_back( STRFROMANSI( "id of existing QuantLib::EvolutionDescription object" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionEvolutionTimes" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlEvolutionDescriptionFirstAliveRate

    funcMap[ STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" ) ]
        =  STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" ) ]
        =  STRFROMANSI( "first alive rate at each evolution time for the EvolutionDescription object." );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::EvolutionDescription object" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFirstAliveRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlEvolutionDescriptionFromProduct

    funcMap[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ]
        =  STRFROMANSI( "qlEvolutionDescriptionFromProduct" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ]
        =  STRFROMANSI( "Construct an object of class EvolutionDescription and return its id" );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "Product" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "ID of product object." ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionFromProduct" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlEvolutionDescriptionNumberOfRates

    funcMap[ STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" ) ]
        =  STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" ) ]
        =  STRFROMANSI( "number of rates for the EvolutionDescription object." );
    argName[ STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" ) ].push_back( STRFROMANSI( "id of existing QuantLib::EvolutionDescription object" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionNumberOfRates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlEvolutionDescriptionNumberOfSteps

    funcMap[ STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" ) ]
        =  STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" ) ]
        =  STRFROMANSI( "number of steps for the EvolutionDescription object." );
    argName[ STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" ) ].push_back( STRFROMANSI( "id of existing QuantLib::EvolutionDescription object" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionNumberOfSteps" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlEvolutionDescriptionRateTimes

    funcMap[ STRFROMANSI( "qlEvolutionDescriptionRateTimes" ) ]
        =  STRFROMANSI( "qlEvolutionDescriptionRateTimes" );
    funcDesc[ STRFROMANSI( "qlEvolutionDescriptionRateTimes" ) ]
        =  STRFROMANSI( "rates fixing times for the EvolutionDescription object." );
    argName[ STRFROMANSI( "qlEvolutionDescriptionRateTimes" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionRateTimes" ) ].push_back( STRFROMANSI( "id of existing QuantLib::EvolutionDescription object" ) );
    argName[ STRFROMANSI( "qlEvolutionDescriptionRateTimes" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEvolutionDescriptionRateTimes" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlIsInMoneyMarketMeasure

    funcMap[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ]
        =  STRFROMANSI( "qlIsInMoneyMarketMeasure" );
    funcDesc[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ]
        =  STRFROMANSI( "Returns TRUE if the numeraire vector is money market measure for the given EvolutionDescription object." );
    argName[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "ID of EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "Numeraires" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "vector of numeraires." ) );
    argName[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlIsInMoneyMarketPlusMeasure

    funcMap[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ]
        =  STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" );
    funcDesc[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ]
        =  STRFROMANSI( "Returns TRUE if the numeraire vector is money market measure plus for the given EvolutionDescription object." );
    argName[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "ID of EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "Numeraires" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "vector of numeraires." ) );
    argName[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "Offset" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "offset applied to the MoneyMarket measure. Default value = 1." ) );
    argName[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIsInMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlIsInTerminalMeasure

    funcMap[ STRFROMANSI( "qlIsInTerminalMeasure" ) ]
        =  STRFROMANSI( "qlIsInTerminalMeasure" );
    funcDesc[ STRFROMANSI( "qlIsInTerminalMeasure" ) ]
        =  STRFROMANSI( "Returns TRUE if the numeraire vector is Terminal measure for the given EvolutionDescription object." );
    argName[ STRFROMANSI( "qlIsInTerminalMeasure" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlIsInTerminalMeasure" ) ].push_back( STRFROMANSI( "ID of EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlIsInTerminalMeasure" ) ].push_back( STRFROMANSI( "Numeraires" ) );
    argDesc[ STRFROMANSI( "qlIsInTerminalMeasure" ) ].push_back( STRFROMANSI( "vector of numeraires." ) );
    argName[ STRFROMANSI( "qlIsInTerminalMeasure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIsInTerminalMeasure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMoneyMarketMeasure

    funcMap[ STRFROMANSI( "qlMoneyMarketMeasure" ) ]
        =  STRFROMANSI( "qlMoneyMarketMeasure" );
    funcDesc[ STRFROMANSI( "qlMoneyMarketMeasure" ) ]
        =  STRFROMANSI( "Returns the discretely compounded money market measure for the given EvolutionDescription object." );
    argName[ STRFROMANSI( "qlMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "ID of EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMoneyMarketMeasure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMoneyMarketPlusMeasure

    funcMap[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ]
        =  STRFROMANSI( "qlMoneyMarketPlusMeasure" );
    funcDesc[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ]
        =  STRFROMANSI( "Returns the discretely compounded money market measure for the given EvolutionDescription object." );
    argName[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "ID of EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "Offset" ) );
    argDesc[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "offset applied to the MoneyMarket measure. Default value = 1." ) );
    argName[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMoneyMarketPlusMeasure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlTerminalMeasure

    funcMap[ STRFROMANSI( "qlTerminalMeasure" ) ]
        =  STRFROMANSI( "qlTerminalMeasure" );
    funcDesc[ STRFROMANSI( "qlTerminalMeasure" ) ]
        =  STRFROMANSI( "Returns the terminal measure for the given EvolutionDescription object." );
    argName[ STRFROMANSI( "qlTerminalMeasure" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlTerminalMeasure" ) ].push_back( STRFROMANSI( "ID of EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlTerminalMeasure" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlTerminalMeasure" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlEuriborSwap

    funcMap[ STRFROMANSI( "qlEuriborSwap" ) ]
        =  STRFROMANSI( "qlEuriborSwap" );
    funcDesc[ STRFROMANSI( "qlEuriborSwap" ) ]
        =  STRFROMANSI( "Construct an object of class EuriborSwap and return its id" );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "FixingType" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "Swap index fixing type (e.g. IsdaFixA, IsdaFixB, IfrFix, IsdaFixAm, IsdaFixPm). Default value = Default." ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "index tenor (e.g. 1Y for one year)." ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "FwdCurve" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "forwarding YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "DiscCurve" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlEuriborSwapIsdaFixA

    funcMap[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ]
        =  STRFROMANSI( "qlEuriborSwapIsdaFixA" );
    funcDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ]
        =  STRFROMANSI( "Construct an object of class EuriborSwapIsdaFixA and return its id" );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "index tenor (e.g. 1Y for one year)" ) );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "FwdCurve" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "forwarding YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "DiscCurve" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlEuriborSwapIsdaFixA" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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

    // qlIndexAddFixings

    funcMap[ STRFROMANSI( "qlIndexAddFixings" ) ]
        =  STRFROMANSI( "qlIndexAddFixings" );
    funcDesc[ STRFROMANSI( "qlIndexAddFixings" ) ]
        =  STRFROMANSI( "Adds fixings for the given Index object." );
    argName[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Index object" ) );
    argName[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "FixingDates" ) );
    argDesc[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "fixing dates." ) );
    argName[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "FixingValues" ) );
    argDesc[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "fixing values." ) );
    argName[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "ForceOverwrite" ) );
    argDesc[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "Set to TRUE to force overwriting of existing fixings, if any. Default value = false." ) );
    argName[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIndexAddFixings" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlIndexClearFixings

    funcMap[ STRFROMANSI( "qlIndexClearFixings" ) ]
        =  STRFROMANSI( "qlIndexClearFixings" );
    funcDesc[ STRFROMANSI( "qlIndexClearFixings" ) ]
        =  STRFROMANSI( "Clear all fixings for the given Index object." );
    argName[ STRFROMANSI( "qlIndexClearFixings" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlIndexClearFixings" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Index object" ) );
    argName[ STRFROMANSI( "qlIndexClearFixings" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIndexClearFixings" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlIndexName

    funcMap[ STRFROMANSI( "qlIndexName" ) ]
        =  STRFROMANSI( "qlIndexName" );
    funcDesc[ STRFROMANSI( "qlIndexName" ) ]
        =  STRFROMANSI( "Returns the name for the given Index object." );
    argName[ STRFROMANSI( "qlIndexName" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlIndexName" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Index object" ) );
    argName[ STRFROMANSI( "qlIndexName" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlIndexName" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlLiborSwap

    funcMap[ STRFROMANSI( "qlLiborSwap" ) ]
        =  STRFROMANSI( "qlLiborSwap" );
    funcDesc[ STRFROMANSI( "qlLiborSwap" ) ]
        =  STRFROMANSI( "Construct an object of class LiborSwap and return its id" );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Libor swap index currency." ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "FixingType" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Swap index fixing type (e.g. IsdaFixA, IsdaFixB, IfrFix, IsdaFixAm, IsdaFixPm). Default value = Default." ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "index tenor (e.g. 1Y for one year)." ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "FwdCurve" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "forwarding YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "DiscCurve" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlLiborSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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

    // qlSonia

    funcMap[ STRFROMANSI( "qlSonia" ) ]
        =  STRFROMANSI( "qlSonia" );
    funcDesc[ STRFROMANSI( "qlSonia" ) ]
        =  STRFROMANSI( "Construct an object of class Sonia and return its id" );
    argName[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "forecasting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSonia" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSwapIndex

    funcMap[ STRFROMANSI( "qlSwapIndex" ) ]
        =  STRFROMANSI( "qlSwapIndex" );
    funcDesc[ STRFROMANSI( "qlSwapIndex" ) ]
        =  STRFROMANSI( "Construct an object of class SwapIndex and return its id" );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "FamilyName" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "index name." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "index tenor (e.g. 2D for two days, 3W for three weeks, 6M for six months, 1Y for one year)." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "swap rate fixing days (e.g. 2)." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Currency" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Index Currency." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "FixedLegTenor" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "tenor of the underlying swap's fixed leg (e.g. 6M, 1Y, 3M)." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "FixedLegBDC" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "business day convention of the underlying swap's fixed leg (e.g. ModifiedFollowing)." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "FixedLegDayCounter" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "day counter of the underlying swap's fixed leg (e.g. 30/360::BondBasis)." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "swap's floating ibor index object ID." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "DiscCurve" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSwapIndex" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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

    // Interpolation

    // qlCubicInterpolation

    funcMap[ STRFROMANSI( "qlCubicInterpolation" ) ]
        =  STRFROMANSI( "qlCubicInterpolation" );
    funcDesc[ STRFROMANSI( "qlCubicInterpolation" ) ]
        =  STRFROMANSI( "Construct an object of class CubicInterpolation and return its id" );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "XArray" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "x array." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "YArray" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "y array." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "DerApprox" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "Derivative Approximation (). Default value = Kruger." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "Monotonic" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "monotonicity constraint flag. Default value = true." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "LeftConditionType" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "left boundary condition (NotAKnot, FirstDerivative, SecondDerivative, Periodic, Lagrange). Default value = SecondDerivative." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "LeftConditionValue" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "left condition value. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "RightConditionType" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "right boundary condition (NotAKnot, FirstDerivative, SecondDerivative, Periodic, Lagrange). Default value = SecondDerivative." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "RightConditionValue" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "right condition value. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCubicInterpolation" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlInterpolation

    funcMap[ STRFROMANSI( "qlInterpolation" ) ]
        =  STRFROMANSI( "qlInterpolation" );
    funcDesc[ STRFROMANSI( "qlInterpolation" ) ]
        =  STRFROMANSI( "Construct an object of class GenericInterp and return its id" );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "InterpolationType" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "interpolation type (e.g BackwardFlat, ForwardFlat, MonotonicCubicNaturalSpline, etc.) Default value = MonotonicNaturalCubicSpline." ) );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "XArray" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "x array." ) );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "YArray" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "y array." ) );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlInterpolation" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlInterpolationInterpolate

    funcMap[ STRFROMANSI( "qlInterpolationInterpolate" ) ]
        =  STRFROMANSI( "qlInterpolationInterpolate" );
    funcDesc[ STRFROMANSI( "qlInterpolationInterpolate" ) ]
        =  STRFROMANSI( "Returns interpolated values using the given Interpolation object." );
    argName[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::Interpolation object" ) );
    argName[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "XValues" ) );
    argDesc[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "x values." ) );
    argName[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "AllowExtrapolation" ) );
    argDesc[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "allow extrapolation flag. Default value = false." ) );
    argName[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterpolationInterpolate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // QuantLib Credit Latent Models

    // qlGaussianDefaultProbLM

    funcMap[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ]
        =  STRFROMANSI( "qlGaussianDefaultProbLM" );
    funcDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ]
        =  STRFROMANSI( "Construct an object of class GaussianDefProbLM and return its id" );
    argName[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Basket" ) );
    argDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Defaultable positions basket." ) );
    argName[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Factors" ) );
    argDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Systemic model factors." ) );
    argName[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlGaussianDefaultProbLM" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Leg

    // qlInterestRate

    funcMap[ STRFROMANSI( "qlInterestRate" ) ]
        =  STRFROMANSI( "qlInterestRate" );
    funcDesc[ STRFROMANSI( "qlInterestRate" ) ]
        =  STRFROMANSI( "Construct an object of class InterestRate and return its id" );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Rate" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "rate." ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Irr DayCounter ID." ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt})." ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlInterestRate" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlInterestRateCompoundFactor

    funcMap[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ]
        =  STRFROMANSI( "qlInterestRateCompoundFactor" );
    funcDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ]
        =  STRFROMANSI( "Returns the compound factor between two dates based on the given InterestRate object." );
    argName[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "id of existing QuantLib::InterestRate object" ) );
    argName[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "compounding period start." ) );
    argName[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "compounding period end." ) );
    argName[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "RefPeriodStart" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "reference period start date needed by some daycounter. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "RefPeriodEnd" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "reference period end date needed by some daycounter. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompoundFactor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInterestRateCompounding

    funcMap[ STRFROMANSI( "qlInterestRateCompounding" ) ]
        =  STRFROMANSI( "qlInterestRateCompounding" );
    funcDesc[ STRFROMANSI( "qlInterestRateCompounding" ) ]
        =  STRFROMANSI( "Returns the Compounding in the given InterestRate object." );
    argName[ STRFROMANSI( "qlInterestRateCompounding" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompounding" ) ].push_back( STRFROMANSI( "id of existing QuantLib::InterestRate object" ) );
    argName[ STRFROMANSI( "qlInterestRateCompounding" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRateCompounding" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInterestRateDayCounter

    funcMap[ STRFROMANSI( "qlInterestRateDayCounter" ) ]
        =  STRFROMANSI( "qlInterestRateDayCounter" );
    funcDesc[ STRFROMANSI( "qlInterestRateDayCounter" ) ]
        =  STRFROMANSI( "Returns the DayCounter in the given InterestRate object." );
    argName[ STRFROMANSI( "qlInterestRateDayCounter" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDayCounter" ) ].push_back( STRFROMANSI( "id of existing QuantLib::InterestRate object" ) );
    argName[ STRFROMANSI( "qlInterestRateDayCounter" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDayCounter" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInterestRateDiscountFactor

    funcMap[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ]
        =  STRFROMANSI( "qlInterestRateDiscountFactor" );
    funcDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ]
        =  STRFROMANSI( "Returns the discount factor between two dates based on the given InterestRate object." );
    argName[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "id of existing QuantLib::InterestRate object" ) );
    argName[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "compounding period start." ) );
    argName[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "compounding period end." ) );
    argName[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "RefPeriodStart" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "reference period start date needed by some daycounter. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "RefPeriodEnd" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "reference period end date needed by some daycounter. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRateDiscountFactor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInterestRateFrequency

    funcMap[ STRFROMANSI( "qlInterestRateFrequency" ) ]
        =  STRFROMANSI( "qlInterestRateFrequency" );
    funcDesc[ STRFROMANSI( "qlInterestRateFrequency" ) ]
        =  STRFROMANSI( "Returns the Frequency in the given InterestRate object." );
    argName[ STRFROMANSI( "qlInterestRateFrequency" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRateFrequency" ) ].push_back( STRFROMANSI( "id of existing QuantLib::InterestRate object" ) );
    argName[ STRFROMANSI( "qlInterestRateFrequency" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRateFrequency" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlInterestRateRate

    funcMap[ STRFROMANSI( "qlInterestRateRate" ) ]
        =  STRFROMANSI( "qlInterestRateRate" );
    funcDesc[ STRFROMANSI( "qlInterestRateRate" ) ]
        =  STRFROMANSI( "Returns the rate in the given InterestRate object." );
    argName[ STRFROMANSI( "qlInterestRateRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterestRateRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::InterestRate object" ) );
    argName[ STRFROMANSI( "qlInterestRateRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterestRateRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlLeg

    funcMap[ STRFROMANSI( "qlLeg" ) ]
        =  STRFROMANSI( "qlLeg" );
    funcDesc[ STRFROMANSI( "qlLeg" ) ]
        =  STRFROMANSI( "Construct an object of class Leg and return its id" );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "Amounts" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "list of cash to be received/paid." ) );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "Dates" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "payment dates corresponding to amounts." ) );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "ToBeSorted" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "TRUE if the CashFlows must be sorted by ascending dates. Default value = true." ) );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlLegFromCapFloor

    funcMap[ STRFROMANSI( "qlLegFromCapFloor" ) ]
        =  STRFROMANSI( "qlLegFromCapFloor" );
    funcDesc[ STRFROMANSI( "qlLegFromCapFloor" ) ]
        =  STRFROMANSI( "Construct an object of class Leg and return its id" );
    argName[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "CapFloor" ) );
    argDesc[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "CapFloor object ID." ) );
    argName[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlLegFromCapFloor" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlLegFromSwap

    funcMap[ STRFROMANSI( "qlLegFromSwap" ) ]
        =  STRFROMANSI( "qlLegFromSwap" );
    funcDesc[ STRFROMANSI( "qlLegFromSwap" ) ]
        =  STRFROMANSI( "Construct an object of class Leg and return its id" );
    argName[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "Swap" ) );
    argDesc[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "Swap object ID." ) );
    argName[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "LegNumber" ) );
    argDesc[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "Zero based leg number (e.g. use 0 for the first leg, 1 for the second leg, etc.)." ) );
    argName[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlLegFromSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Market Model Evolvers

    // qlForwardRateIpc

    funcMap[ STRFROMANSI( "qlForwardRateIpc" ) ]
        =  STRFROMANSI( "qlForwardRateIpc" );
    funcDesc[ STRFROMANSI( "qlForwardRateIpc" ) ]
        =  STRFROMANSI( "Construct an object of class LogNormalFwdRateIpc and return its id" );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "MarketModel" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "MarketModel object ID." ) );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "BrownianGeneratorFactory" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "Brownian generator factory." ) );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "Numeraires" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "numeraire vector." ) );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlForwardRateIpc" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlForwardRateNormalPc

    funcMap[ STRFROMANSI( "qlForwardRateNormalPc" ) ]
        =  STRFROMANSI( "qlForwardRateNormalPc" );
    funcDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ]
        =  STRFROMANSI( "Construct an object of class NormalFwdRatePc and return its id" );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "MarketModel" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "MarketModel object ID." ) );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "BrownianGeneratorFactory" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "Brownian generator factory." ) );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "Numeraires" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "numeraire vector." ) );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlForwardRateNormalPc" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlForwardRatePc

    funcMap[ STRFROMANSI( "qlForwardRatePc" ) ]
        =  STRFROMANSI( "qlForwardRatePc" );
    funcDesc[ STRFROMANSI( "qlForwardRatePc" ) ]
        =  STRFROMANSI( "Construct an object of class LogNormalFwdRatePc and return its id" );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "MarketModel" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "MarketModel object ID." ) );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "BrownianGeneratorFactory" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "Brownian generator factory." ) );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "Numeraires" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "numeraire vector." ) );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlForwardRatePc" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMarketModelEvolverAdvanceStep

    funcMap[ STRFROMANSI( "qlMarketModelEvolverAdvanceStep" ) ]
        =  STRFROMANSI( "qlMarketModelEvolverAdvanceStep" );
    funcDesc[ STRFROMANSI( "qlMarketModelEvolverAdvanceStep" ) ]
        =  STRFROMANSI( "advance a single step in the current path for the MarketModelEvolver object." );
    argName[ STRFROMANSI( "qlMarketModelEvolverAdvanceStep" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverAdvanceStep" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelEvolver object" ) );
    argName[ STRFROMANSI( "qlMarketModelEvolverAdvanceStep" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverAdvanceStep" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelEvolverCurrentStep

    funcMap[ STRFROMANSI( "qlMarketModelEvolverCurrentStep" ) ]
        =  STRFROMANSI( "qlMarketModelEvolverCurrentStep" );
    funcDesc[ STRFROMANSI( "qlMarketModelEvolverCurrentStep" ) ]
        =  STRFROMANSI( "returns the current step index in the current path for the MarketModelEvolver object." );
    argName[ STRFROMANSI( "qlMarketModelEvolverCurrentStep" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverCurrentStep" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelEvolver object" ) );
    argName[ STRFROMANSI( "qlMarketModelEvolverCurrentStep" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverCurrentStep" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelEvolverNumeraires

    funcMap[ STRFROMANSI( "qlMarketModelEvolverNumeraires" ) ]
        =  STRFROMANSI( "qlMarketModelEvolverNumeraires" );
    funcDesc[ STRFROMANSI( "qlMarketModelEvolverNumeraires" ) ]
        =  STRFROMANSI( "returns the current step index in the current path for the MarketModelEvolver object." );
    argName[ STRFROMANSI( "qlMarketModelEvolverNumeraires" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverNumeraires" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelEvolver object" ) );
    argName[ STRFROMANSI( "qlMarketModelEvolverNumeraires" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverNumeraires" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelEvolverStartNewPath

    funcMap[ STRFROMANSI( "qlMarketModelEvolverStartNewPath" ) ]
        =  STRFROMANSI( "qlMarketModelEvolverStartNewPath" );
    funcDesc[ STRFROMANSI( "qlMarketModelEvolverStartNewPath" ) ]
        =  STRFROMANSI( "start a new path for the MarketModelEvolver object." );
    argName[ STRFROMANSI( "qlMarketModelEvolverStartNewPath" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverStartNewPath" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelEvolver object" ) );
    argName[ STRFROMANSI( "qlMarketModelEvolverStartNewPath" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelEvolverStartNewPath" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // MarketModels

    // qlAbcdVol

    funcMap[ STRFROMANSI( "qlAbcdVol" ) ]
        =  STRFROMANSI( "qlAbcdVol" );
    funcDesc[ STRFROMANSI( "qlAbcdVol" ) ]
        =  STRFROMANSI( "Construct an object of class AbcdVol and return its id" );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "A" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "the a coefficient in the abcd vol parametrization." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "B" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "the b coefficient in the abcd vol parametrization." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "C" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "the c coefficient in the abcd vol parametrization." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "D" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "the d coefficient in the abcd vol parametrization." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Ks" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "K_i adjustment factors in the abcd vol parametrization." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Correlations" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "PiecewiseConstantCorrelation object ID." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Factors" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "number of factors to be retained in the simulation." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "InitialRates" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "initial rates." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Displacements" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "displacements." ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlAbcdVol" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlAnnuity

    funcMap[ STRFROMANSI( "qlAnnuity" ) ]
        =  STRFROMANSI( "qlAnnuity" );
    funcDesc[ STRFROMANSI( "qlAnnuity" ) ]
        =  STRFROMANSI( "returns annuity of arbitrary swap-rate." );
    argName[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "StartIndex" ) );
    argDesc[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "start index." ) );
    argName[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "EndIndex" ) );
    argDesc[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "end index." ) );
    argName[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "NumeraireIndex" ) );
    argDesc[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "numeraire index. Default value = 0." ) );
    argName[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlAnnuity" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCmSwapForwardJacobian

    funcMap[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ]
        =  STRFROMANSI( "qlCmSwapForwardJacobian" );
    funcDesc[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ]
        =  STRFROMANSI( "Returns the jacobian between constant maturity swap rates and forward rates." );
    argName[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ].push_back( STRFROMANSI( "SpanningForwards" ) );
    argDesc[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ].push_back( STRFROMANSI( "number of forwards underlying the CMS." ) );
    argName[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCmSwapForwardJacobian" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCmSwapZedMatrix

    funcMap[ STRFROMANSI( "qlCmSwapZedMatrix" ) ]
        =  STRFROMANSI( "qlCmSwapZedMatrix" );
    funcDesc[ STRFROMANSI( "qlCmSwapZedMatrix" ) ]
        =  STRFROMANSI( "Returns the Z matrix to switch base from forward to constant maturity swap rates." );
    argName[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "SpanningForwards" ) );
    argDesc[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "number of forwards underlying the CMS." ) );
    argName[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "displacement. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCmSwapZedMatrix" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCoinitialSwapForwardJacobian

    funcMap[ STRFROMANSI( "qlCoinitialSwapForwardJacobian" ) ]
        =  STRFROMANSI( "qlCoinitialSwapForwardJacobian" );
    funcDesc[ STRFROMANSI( "qlCoinitialSwapForwardJacobian" ) ]
        =  STRFROMANSI( "Returns the jacobian between coinitial swap rates and forward rates." );
    argName[ STRFROMANSI( "qlCoinitialSwapForwardJacobian" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCoinitialSwapForwardJacobian" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlCoinitialSwapForwardJacobian" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCoinitialSwapForwardJacobian" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCoinitialSwapZedMatrix

    funcMap[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ]
        =  STRFROMANSI( "qlCoinitialSwapZedMatrix" );
    funcDesc[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ]
        =  STRFROMANSI( "Returns the Z matrix to switch base from forward to coinitial swap rates." );
    argName[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ].push_back( STRFROMANSI( "displacement. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCoinitialSwapZedMatrix" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCotSwapToFwdAdapter

    funcMap[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ]
        =  STRFROMANSI( "qlCotSwapToFwdAdapter" );
    funcDesc[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ]
        =  STRFROMANSI( "Construct an object of class CotSwapToFwdAdapter and return its id" );
    argName[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "CoterminalModel" ) );
    argDesc[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "CoTerminal Swap Market Model ID." ) );
    argName[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlCotSwapToFwdAdapter" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlCoterminalSwapForwardJacobian

    funcMap[ STRFROMANSI( "qlCoterminalSwapForwardJacobian" ) ]
        =  STRFROMANSI( "qlCoterminalSwapForwardJacobian" );
    funcDesc[ STRFROMANSI( "qlCoterminalSwapForwardJacobian" ) ]
        =  STRFROMANSI( "Returns the jacobian between coterminal swap rates and forward rates." );
    argName[ STRFROMANSI( "qlCoterminalSwapForwardJacobian" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCoterminalSwapForwardJacobian" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlCoterminalSwapForwardJacobian" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCoterminalSwapForwardJacobian" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlCoterminalSwapZedMatrix

    funcMap[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ]
        =  STRFROMANSI( "qlCoterminalSwapZedMatrix" );
    funcDesc[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ]
        =  STRFROMANSI( "Returns the Z matrix to switch base from forward to coterminal swap rates." );
    argName[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ].push_back( STRFROMANSI( "displacement. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlCoterminalSwapZedMatrix" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlFlatVol

    funcMap[ STRFROMANSI( "qlFlatVol" ) ]
        =  STRFROMANSI( "qlFlatVol" );
    funcDesc[ STRFROMANSI( "qlFlatVol" ) ]
        =  STRFROMANSI( "Construct an object of class FlatVol and return its id" );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Volatilities" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "volatilities." ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Correlations" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "PiecewiseConstantCorrelation object ID." ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "EvolutionDescription" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "EvolutionDescription object." ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Factors" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "number of factors to be retained in the simulation." ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "InitialRates" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "initial rates." ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Displacements" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "displacements." ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFlatVol" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFlatVolFactory

    funcMap[ STRFROMANSI( "qlFlatVolFactory" ) ]
        =  STRFROMANSI( "qlFlatVolFactory" );
    funcDesc[ STRFROMANSI( "qlFlatVolFactory" ) ]
        =  STRFROMANSI( "Construct an object of class FlatVolFactory and return its id" );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "LongTermCorr" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Long term correlation L in rho=L+(1-L)*exp(-beta*abs(Ti-Tj))." ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Beta" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "beta in rho=L+(1-L)*exp(-beta*abs(Ti-Tj))." ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Times" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "times." ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Volatilities" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "volatilities." ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "discounting yield term structure object ID." ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "displacement. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFlatVolFactory" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFwdPeriodAdapter

    funcMap[ STRFROMANSI( "qlFwdPeriodAdapter" ) ]
        =  STRFROMANSI( "qlFwdPeriodAdapter" );
    funcDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ]
        =  STRFROMANSI( "Construct an object of class FwdPeriodAdapter and return its id" );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "LargeModel" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "large MarketModel ID." ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "Period" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "target period (e.g. 2 if going from semiannual LMM to annual LMM)." ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "Offset" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "rate index offset, in order to pin down swaption coterminal to the last rate time." ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "Displacements" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "displacements." ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFwdPeriodAdapter" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFwdToCotSwapAdapter

    funcMap[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ]
        =  STRFROMANSI( "qlFwdToCotSwapAdapter" );
    funcDesc[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ]
        =  STRFROMANSI( "Construct an object of class FwdToCotSwapAdapter and return its id" );
    argName[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "ForwardModel" ) );
    argDesc[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "Forward Rate Market Model ID." ) );
    argName[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFwdToCotSwapAdapter" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMarketModelCovariance

    funcMap[ STRFROMANSI( "qlMarketModelCovariance" ) ]
        =  STRFROMANSI( "qlMarketModelCovariance" );
    funcDesc[ STRFROMANSI( "qlMarketModelCovariance" ) ]
        =  STRFROMANSI( "Returns the covariance matrix for the i-th step." );
    argName[ STRFROMANSI( "qlMarketModelCovariance" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelCovariance" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelCovariance" ) ].push_back( STRFROMANSI( "Index" ) );
    argDesc[ STRFROMANSI( "qlMarketModelCovariance" ) ].push_back( STRFROMANSI( "evolution step index." ) );
    argName[ STRFROMANSI( "qlMarketModelCovariance" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelCovariance" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelDisplacements

    funcMap[ STRFROMANSI( "qlMarketModelDisplacements" ) ]
        =  STRFROMANSI( "qlMarketModelDisplacements" );
    funcDesc[ STRFROMANSI( "qlMarketModelDisplacements" ) ]
        =  STRFROMANSI( "rates' displacemets for the MarketModel object." );
    argName[ STRFROMANSI( "qlMarketModelDisplacements" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelDisplacements" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelDisplacements" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelDisplacements" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelInitialRates

    funcMap[ STRFROMANSI( "qlMarketModelInitialRates" ) ]
        =  STRFROMANSI( "qlMarketModelInitialRates" );
    funcDesc[ STRFROMANSI( "qlMarketModelInitialRates" ) ]
        =  STRFROMANSI( "initial rates for the MarketModel object." );
    argName[ STRFROMANSI( "qlMarketModelInitialRates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelInitialRates" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelInitialRates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelInitialRates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelNumberOfFactors

    funcMap[ STRFROMANSI( "qlMarketModelNumberOfFactors" ) ]
        =  STRFROMANSI( "qlMarketModelNumberOfFactors" );
    funcDesc[ STRFROMANSI( "qlMarketModelNumberOfFactors" ) ]
        =  STRFROMANSI( "number of factors for the MarketModel object." );
    argName[ STRFROMANSI( "qlMarketModelNumberOfFactors" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelNumberOfFactors" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelNumberOfFactors" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelNumberOfFactors" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelNumberOfRates

    funcMap[ STRFROMANSI( "qlMarketModelNumberOfRates" ) ]
        =  STRFROMANSI( "qlMarketModelNumberOfRates" );
    funcDesc[ STRFROMANSI( "qlMarketModelNumberOfRates" ) ]
        =  STRFROMANSI( "number of rates for the MarketModel object." );
    argName[ STRFROMANSI( "qlMarketModelNumberOfRates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelNumberOfRates" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelNumberOfRates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelNumberOfRates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelNumberOfSteps

    funcMap[ STRFROMANSI( "qlMarketModelNumberOfSteps" ) ]
        =  STRFROMANSI( "qlMarketModelNumberOfSteps" );
    funcDesc[ STRFROMANSI( "qlMarketModelNumberOfSteps" ) ]
        =  STRFROMANSI( "number of steps for the MarketModel object." );
    argName[ STRFROMANSI( "qlMarketModelNumberOfSteps" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelNumberOfSteps" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelNumberOfSteps" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelNumberOfSteps" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelPseudoRoot

    funcMap[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ]
        =  STRFROMANSI( "qlMarketModelPseudoRoot" );
    funcDesc[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ]
        =  STRFROMANSI( "Returns the pseudo root for the i-th step." );
    argName[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ].push_back( STRFROMANSI( "Index" ) );
    argDesc[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ].push_back( STRFROMANSI( "evolution step index." ) );
    argName[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelPseudoRoot" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelTimeDependentVolatility

    funcMap[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ]
        =  STRFROMANSI( "qlMarketModelTimeDependentVolatility" );
    funcDesc[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ]
        =  STRFROMANSI( "Returns the time dependent vol for rate i." );
    argName[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ].push_back( STRFROMANSI( "Index" ) );
    argDesc[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ].push_back( STRFROMANSI( "rate index." ) );
    argName[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelTimeDependentVolatility" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelTotalCovariance

    funcMap[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ]
        =  STRFROMANSI( "qlMarketModelTotalCovariance" );
    funcDesc[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ]
        =  STRFROMANSI( "Returns the covariance matrix from start up to the i-th step." );
    argName[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModel object" ) );
    argName[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ].push_back( STRFROMANSI( "Index" ) );
    argDesc[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ].push_back( STRFROMANSI( "evolution step index." ) );
    argName[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelTotalCovariance" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlPseudoRootFacade

    funcMap[ STRFROMANSI( "qlPseudoRootFacade" ) ]
        =  STRFROMANSI( "qlPseudoRootFacade" );
    funcDesc[ STRFROMANSI( "qlPseudoRootFacade" ) ]
        =  STRFROMANSI( "Construct an object of class PseudoRootFacade and return its id" );
    argName[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "Calibrator" ) );
    argDesc[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "CTSMMCapletCalibration ID." ) );
    argName[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlPseudoRootFacade" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRateInstVolDifferences

    funcMap[ STRFROMANSI( "qlRateInstVolDifferences" ) ]
        =  STRFROMANSI( "qlRateInstVolDifferences" );
    funcDesc[ STRFROMANSI( "qlRateInstVolDifferences" ) ]
        =  STRFROMANSI( "Compute the differences between volatilities at each evolution step." );
    argName[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "MarketModel1" ) );
    argDesc[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "First Market Model." ) );
    argName[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "MarketModel2" ) );
    argDesc[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "Second Market Model." ) );
    argName[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "Index" ) );
    argDesc[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "Forward Rate index." ) );
    argName[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateInstVolDifferences" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRateVolDifferences

    funcMap[ STRFROMANSI( "qlRateVolDifferences" ) ]
        =  STRFROMANSI( "qlRateVolDifferences" );
    funcDesc[ STRFROMANSI( "qlRateVolDifferences" ) ]
        =  STRFROMANSI( "Compute the differences between all implied forwards volatilities." );
    argName[ STRFROMANSI( "qlRateVolDifferences" ) ].push_back( STRFROMANSI( "MarketModel1" ) );
    argDesc[ STRFROMANSI( "qlRateVolDifferences" ) ].push_back( STRFROMANSI( "First Market Model." ) );
    argName[ STRFROMANSI( "qlRateVolDifferences" ) ].push_back( STRFROMANSI( "MarketModel2" ) );
    argDesc[ STRFROMANSI( "qlRateVolDifferences" ) ].push_back( STRFROMANSI( "Second Market Model." ) );
    argName[ STRFROMANSI( "qlRateVolDifferences" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateVolDifferences" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSwapDerivative

    funcMap[ STRFROMANSI( "qlSwapDerivative" ) ]
        =  STRFROMANSI( "qlSwapDerivative" );
    funcDesc[ STRFROMANSI( "qlSwapDerivative" ) ]
        =  STRFROMANSI( "returns derivative of swap-rate to underlying forward rate." );
    argName[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "CurveState" ) );
    argDesc[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "CurveState object ID." ) );
    argName[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "StartIndex" ) );
    argDesc[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "start index." ) );
    argName[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "EndIndex" ) );
    argDesc[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "end index." ) );
    argName[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "FwdRateIndex" ) );
    argDesc[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "forward rate index. Default value = 0." ) );
    argName[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapDerivative" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Options

    // qlDividendVanillaOption

    funcMap[ STRFROMANSI( "qlDividendVanillaOption" ) ]
        =  STRFROMANSI( "qlDividendVanillaOption" );
    funcDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ]
        =  STRFROMANSI( "Construct an object of class DividendVanillaOption and return its id" );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Payoff" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "StrikedTypePayoff object ID." ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Exercise" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Exercise object ID." ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "DividendDates" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "vector of dividend dates." ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Dividends" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "vector of dividends." ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlDividendVanillaOption" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "displacement in a displaced diffusion model. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlBlackCapFloorEngine2

    funcMap[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ]
        =  STRFROMANSI( "qlBlackCapFloorEngine2" );
    funcDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ]
        =  STRFROMANSI( "Construct an object of class BlackCapFloorEngine and return its id" );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "Vol" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "Term (i.e. flat) volatility." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "displacement in a displaced diffusion model. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlBlackCapFloorEngine2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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
    argName[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine" ) ].push_back( STRFROMANSI( "displacement in a displaced diffusion model. Default value = 0.0." ) );
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
    argName[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlBlackSwaptionEngine2" ) ].push_back( STRFROMANSI( "displacement in a displaced diffusion model. Default value = 0.0." ) );
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

    // Products

    // qlMarketModelMultiProductComposite

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductComposite" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ]
        =  STRFROMANSI( "Construct an object of class MultiProductComposite and return its id" );
    argName[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductComposite" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMarketModelMultiProductCompositeAdd

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ]
        =  STRFROMANSI( "Add new product to MarketModelComposite object." );
    argName[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MultiProductComposite object" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ].push_back( STRFROMANSI( "Product" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ].push_back( STRFROMANSI( "ID of product object." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeAdd" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelMultiProductCompositeFinalize

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" ) ]
        =  STRFROMANSI( "finalize the MarketModelComposite object." );
    argName[ STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MultiProductComposite object" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductCompositeFinalize" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" ) ]
        =  STRFROMANSI( "Max number of cashflows per product per step for the MarketModelMultiProduct object." );
    argName[ STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelMultiProduct object" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductMaxNumberOfCashFlowsPerProductPerStep" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelMultiProductNumberOfProducts

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" ) ]
        =  STRFROMANSI( "number of products in the MarketModelMultiProduct object." );
    argName[ STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelMultiProduct object" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductNumberOfProducts" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelMultiProductPossibleCashFlowTimes

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" ) ]
        =  STRFROMANSI( "possible cash flow times for the MarketModelMultiProduct object." );
    argName[ STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelMultiProduct object" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductPossibleCashFlowTimes" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelMultiProductSuggestedNumeraires

    funcMap[ STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" ) ]
        =  STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" ) ]
        =  STRFROMANSI( "suggested Numeraires for the MarketModelMultiProduct object." );
    argName[ STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" ) ].push_back( STRFROMANSI( "id of existing QuantLib::MarketModelMultiProduct object" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiProductSuggestedNumeraires" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlMarketModelMultiStepRatchet

    funcMap[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ]
        =  STRFROMANSI( "qlMarketModelMultiStepRatchet" );
    funcDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ]
        =  STRFROMANSI( "Construct an object of class MultiStepRatchet and return its id" );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "rate fixing times." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "Accruals" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "accrual factors between rate fixing times." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "PaymentTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "payment times of the product." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "GearingOfFloor" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "gearing of floor." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "GearingOfFixing" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "gearing of fixing." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "SpreadOfFloor" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "spread of floor." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "SpreadOfFixing" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "spread of fixing." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "InitialFloor" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "initial floor." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "Payer" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "payer if true." ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMarketModelMultiStepRatchet" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMarketModelOneStepForwards

    funcMap[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ]
        =  STRFROMANSI( "qlMarketModelOneStepForwards" );
    funcDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ]
        =  STRFROMANSI( "Construct an object of class OneStepForwards and return its id" );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "rate fixing times." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "Accruals" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "accrual factors." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "PaymentTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "payment times of the product." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "Strikes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "forward strikes." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepForwards" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMarketModelOneStepOptionlets

    funcMap[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ]
        =  STRFROMANSI( "qlMarketModelOneStepOptionlets" );
    funcDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ]
        =  STRFROMANSI( "Construct an object of class OneStepOptionlets and return its id" );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "RateTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "rate fixing times." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "Accruals" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "accrual factors." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "PaymentTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "payment times of the product." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "Payoffs" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "striked type payoff object IDs." ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMarketModelOneStepOptionlets" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Quotes

    // qlQuoteIsValid

    funcMap[ STRFROMANSI( "qlQuoteIsValid" ) ]
        =  STRFROMANSI( "qlQuoteIsValid" );
    funcDesc[ STRFROMANSI( "qlQuoteIsValid" ) ]
        =  STRFROMANSI( "Returns TRUE if the given Quote object has a valid value." );
    argName[ STRFROMANSI( "qlQuoteIsValid" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlQuoteIsValid" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Quote object" ) );
    argName[ STRFROMANSI( "qlQuoteIsValid" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlQuoteIsValid" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // Random Sequence Generator

    // qlFaureRsg

    funcMap[ STRFROMANSI( "qlFaureRsg" ) ]
        =  STRFROMANSI( "qlFaureRsg" );
    funcDesc[ STRFROMANSI( "qlFaureRsg" ) ]
        =  STRFROMANSI( "Construct an object of class FaureRsg and return its id" );
    argName[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "dimension." ) );
    argName[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFaureRsg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlHaltonRsg

    funcMap[ STRFROMANSI( "qlHaltonRsg" ) ]
        =  STRFROMANSI( "qlHaltonRsg" );
    funcDesc[ STRFROMANSI( "qlHaltonRsg" ) ]
        =  STRFROMANSI( "Construct an object of class HaltonRsg and return its id" );
    argName[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "dimension." ) );
    argName[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "Seed" ) );
    argDesc[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "seed." ) );
    argName[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlHaltonRsg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMersenneTwisterRsg

    funcMap[ STRFROMANSI( "qlMersenneTwisterRsg" ) ]
        =  STRFROMANSI( "qlMersenneTwisterRsg" );
    funcDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ]
        =  STRFROMANSI( "Construct an object of class MersenneTwisterRsg and return its id" );
    argName[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "dimension." ) );
    argName[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "Seed" ) );
    argDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "seed." ) );
    argName[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMersenneTwisterRsg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRand

    funcMap[ STRFROMANSI( "qlRand" ) ]
        =  STRFROMANSI( "qlRand" );
    funcDesc[ STRFROMANSI( "qlRand" ) ]
        =  STRFROMANSI( "returns a random number between 0 and 1." );
    argName[ STRFROMANSI( "qlRand" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRand" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRandomize

    funcMap[ STRFROMANSI( "qlRandomize" ) ]
        =  STRFROMANSI( "qlRandomize" );
    funcDesc[ STRFROMANSI( "qlRandomize" ) ]
        =  STRFROMANSI( "initialize with the given seed the random number generator used by qlRand()." );
    argName[ STRFROMANSI( "qlRandomize" ) ].push_back( STRFROMANSI( "Seed" ) );
    argDesc[ STRFROMANSI( "qlRandomize" ) ].push_back( STRFROMANSI( "the seed used to initialize the random number generator." ) );
    argName[ STRFROMANSI( "qlRandomize" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRandomize" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSobolRsg

    funcMap[ STRFROMANSI( "qlSobolRsg" ) ]
        =  STRFROMANSI( "qlSobolRsg" );
    funcDesc[ STRFROMANSI( "qlSobolRsg" ) ]
        =  STRFROMANSI( "Construct an object of class SobolRsg and return its id" );
    argName[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "dimension." ) );
    argName[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "Seed" ) );
    argDesc[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "seed." ) );
    argName[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSobolRsg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlVariates

    funcMap[ STRFROMANSI( "qlVariates" ) ]
        =  STRFROMANSI( "qlVariates" );
    funcDesc[ STRFROMANSI( "qlVariates" ) ]
        =  STRFROMANSI( "generate variates." );
    argName[ STRFROMANSI( "qlVariates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVariates" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::RandomSequenceGenerator object" ) );
    argName[ STRFROMANSI( "qlVariates" ) ].push_back( STRFROMANSI( "Samples" ) );
    argDesc[ STRFROMANSI( "qlVariates" ) ].push_back( STRFROMANSI( "number of samples." ) );
    argName[ STRFROMANSI( "qlVariates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVariates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Range Accrual

    // qlRangeAccrualFloatersCoupon

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCoupon" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ]
        =  STRFROMANSI( "Construct an object of class RangeAccrualFloatersCoupon and return its id" );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "Nominal" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "coupon nominal." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "PaymentDate" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "payment Date." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "underlying ibor index." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "StartDate" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "start Date." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "end Date." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "FixingDays" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "fixingDays." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "DayCountID" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "day counter." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "Gearings" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "gearings." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "Spreads" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "spreads." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "RefPeriodStart" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "refPeriodStart." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "RefPeriodEnd" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "refPeriodEnd." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "ObserSchedID" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "observations schedule." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "LowerTrigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "lowerTrigger." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "UpperTrigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "upperTrigger." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCoupon" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRangeAccrualFloatersCouponEndDate

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" ) ]
        =  STRFROMANSI( "Return the accrual end Date." );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponEndDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRangeAccrualFloatersCouponFromLeg

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ]
        =  STRFROMANSI( "Construct an object of class RangeAccrualFloatersCoupon and return its id" );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "RangeAccrualLeg" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "range accrual leg object ID." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "Position" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "position." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponFromLeg" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRangeAccrualFloatersCouponObservationDates

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" ) ]
        =  STRFROMANSI( "Return the observation Dates." );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationDates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRangeAccrualFloatersCouponObservationsNo

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" ) ]
        =  STRFROMANSI( "Return the observations number." );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponObservationsNo" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRangeAccrualFloatersCouponSetPricer

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ]
        =  STRFROMANSI( "Set the coupon pricer at the given coupon object." );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ].push_back( STRFROMANSI( "RangeAccrualPricer" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ].push_back( STRFROMANSI( "RangeAccrual coupon pricer object ID." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponSetPricer" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRangeAccrualFloatersCouponStarDate

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" ) ]
        =  STRFROMANSI( "Return the accrual start Date." );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersCouponStarDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRangeAccrualFloatersPrice

    funcMap[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ]
        =  STRFROMANSI( "qlRangeAccrualFloatersPrice" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ]
        =  STRFROMANSI( "return the price of Range Accrual Floater Coupon." );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ].push_back( STRFROMANSI( "yield curve." ) );
    argName[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualFloatersPrice" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRangeAccrualPricerByBgm

    funcMap[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ]
        =  STRFROMANSI( "qlRangeAccrualPricerByBgm" );
    funcDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ]
        =  STRFROMANSI( "Construct an object of class RangeAccrualPricerByBgm and return its id" );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "Correlation" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "correlation." ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "SmileOnStartDateID" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "ID of the smile on start date object." ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "SmileOnEndDateID" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "ID of the smile on end date object." ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "WithSmile" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "with Smile?." ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "ByCallSpread" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "replicated by Call spread?." ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlRangeAccrualPricerByBgm" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSimpleFloaterPrice

    funcMap[ STRFROMANSI( "qlSimpleFloaterPrice" ) ]
        =  STRFROMANSI( "qlSimpleFloaterPrice" );
    funcDesc[ STRFROMANSI( "qlSimpleFloaterPrice" ) ]
        =  STRFROMANSI( "return the price of Simple Floater Coupon." );
    argName[ STRFROMANSI( "qlSimpleFloaterPrice" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSimpleFloaterPrice" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RangeAccrualFloatersCoupon object" ) );
    argName[ STRFROMANSI( "qlSimpleFloaterPrice" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlSimpleFloaterPrice" ) ].push_back( STRFROMANSI( "yield curve." ) );
    argName[ STRFROMANSI( "qlSimpleFloaterPrice" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSimpleFloaterPrice" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // RateHelper

    // qlBondHelper

    funcMap[ STRFROMANSI( "qlBondHelper" ) ]
        =  STRFROMANSI( "qlBondHelper" );
    funcDesc[ STRFROMANSI( "qlBondHelper" ) ]
        =  STRFROMANSI( "Construct an object of class BondHelper and return its id" );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Price" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "price." ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Bond" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Bond object ID." ) );
    argName[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "UseCleanPrice" ) );
    argDesc[ STRFROMANSI( "qlBondHelper" ) ].push_back( STRFROMANSI( "Type of price specified (TRUE clean price, FALSE for dirty price). Default value = true." ) );
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
    argName[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "DiscountingCurve" ) );
    argDesc[ STRFROMANSI( "qlDatedOISRateHelper" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
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
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Price" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Price - either clean or dirty." ) );
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
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "PaymentCalendar" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "ExCouponPeriod" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Ex-coupon period." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "ExCouponCalendar" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Ex-coupon calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "ExCouponBDC" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Ex-coupon business day convention (e.g. Modified Following)." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "ExCouponEndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Ex-coupon end of month rule (TRUE for end of month to end of month termination date, FALSE otherwise)." ) );
    argName[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "UseCleanPrice" ) );
    argDesc[ STRFROMANSI( "qlFixedRateBondHelper" ) ].push_back( STRFROMANSI( "Type of price specified (TRUE clean price, FALSE for dirty price). Default value = true." ) );
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

    // qlFuturesRateHelper

    funcMap[ STRFROMANSI( "qlFuturesRateHelper" ) ]
        =  STRFROMANSI( "qlFuturesRateHelper" );
    funcDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ]
        =  STRFROMANSI( "Construct an object of class FuturesRateHelper and return its id" );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "Price" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "price quote." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "FuturesType" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "Futures type. Default value = IMM." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "FuturesDate" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "Futures date (IborIndex start date)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "IborIndex object ID. Default value = Euribor3M." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "ConvexityAdjQuote" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "convexity adjustment quote (i.e. Forward rate = Futures rate - convexity adjustment)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFuturesRateHelper2

    funcMap[ STRFROMANSI( "qlFuturesRateHelper2" ) ]
        =  STRFROMANSI( "qlFuturesRateHelper2" );
    funcDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ]
        =  STRFROMANSI( "Construct an object of class FuturesRateHelper and return its id" );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Price" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "price quote." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "FuturesType" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Futures type. Default value = IMM." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "FuturesDate" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Futures date (IborIndex start date)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "LengthInMonths" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "future contract length in months. Default value = 3." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Convention" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "business day convention (e.g. Modified Following). Default value = Modified Following." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "EndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "End of Month rule (TRUE for end of month to end of month termination date, FALSE otherwise). Default value = true." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/360." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "ConvexityAdjQuote" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "convexity adjustment quote (i.e. Forward rate = Futures rate - convexity adjustment)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlFuturesRateHelper3

    funcMap[ STRFROMANSI( "qlFuturesRateHelper3" ) ]
        =  STRFROMANSI( "qlFuturesRateHelper3" );
    funcDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ]
        =  STRFROMANSI( "Construct an object of class FuturesRateHelper and return its id" );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "Price" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "price quote." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "FuturesType" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "Futures type. Default value = IMM." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "FuturesDate" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "Futures date (IborIndex start date)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "EndDate" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "end date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/360." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "ConvexityAdjQuote" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "convexity adjustment quote (i.e. Forward rate = Futures rate - convexity adjustment)." ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlFuturesRateHelper3" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "DiscountingCurve" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID. Default value = ." ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlOISRateHelper" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlRateHelperEarliestDate

    funcMap[ STRFROMANSI( "qlRateHelperEarliestDate" ) ]
        =  STRFROMANSI( "qlRateHelperEarliestDate" );
    funcDesc[ STRFROMANSI( "qlRateHelperEarliestDate" ) ]
        =  STRFROMANSI( "returns the earliest date for the given RateHelper object." );
    argName[ STRFROMANSI( "qlRateHelperEarliestDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRateHelperEarliestDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RateHelper object" ) );
    argName[ STRFROMANSI( "qlRateHelperEarliestDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateHelperEarliestDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRateHelperImpliedQuote

    funcMap[ STRFROMANSI( "qlRateHelperImpliedQuote" ) ]
        =  STRFROMANSI( "qlRateHelperImpliedQuote" );
    funcDesc[ STRFROMANSI( "qlRateHelperImpliedQuote" ) ]
        =  STRFROMANSI( "returns the curve implied quote of the given RateHelper object." );
    argName[ STRFROMANSI( "qlRateHelperImpliedQuote" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRateHelperImpliedQuote" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RateHelper object" ) );
    argName[ STRFROMANSI( "qlRateHelperImpliedQuote" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateHelperImpliedQuote" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRateHelperLatestDate

    funcMap[ STRFROMANSI( "qlRateHelperLatestDate" ) ]
        =  STRFROMANSI( "qlRateHelperLatestDate" );
    funcDesc[ STRFROMANSI( "qlRateHelperLatestDate" ) ]
        =  STRFROMANSI( "returns the latest date for the given RateHelper object." );
    argName[ STRFROMANSI( "qlRateHelperLatestDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRateHelperLatestDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RateHelper object" ) );
    argName[ STRFROMANSI( "qlRateHelperLatestDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateHelperLatestDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRateHelperQuoteIsValid

    funcMap[ STRFROMANSI( "qlRateHelperQuoteIsValid" ) ]
        =  STRFROMANSI( "qlRateHelperQuoteIsValid" );
    funcDesc[ STRFROMANSI( "qlRateHelperQuoteIsValid" ) ]
        =  STRFROMANSI( "returns the isValid boolean of the Quote wrapped in the given RateHelper object." );
    argName[ STRFROMANSI( "qlRateHelperQuoteIsValid" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRateHelperQuoteIsValid" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RateHelper object" ) );
    argName[ STRFROMANSI( "qlRateHelperQuoteIsValid" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateHelperQuoteIsValid" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRateHelperQuoteName

    funcMap[ STRFROMANSI( "qlRateHelperQuoteName" ) ]
        =  STRFROMANSI( "qlRateHelperQuoteName" );
    funcDesc[ STRFROMANSI( "qlRateHelperQuoteName" ) ]
        =  STRFROMANSI( "returns the objectID of the Quote wrapped in the given RateHelper object." );
    argName[ STRFROMANSI( "qlRateHelperQuoteName" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRateHelperQuoteName" ) ].push_back( STRFROMANSI( "id of existing QuantLibAddin::RateHelper object" ) );
    argName[ STRFROMANSI( "qlRateHelperQuoteName" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateHelperQuoteName" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlRateHelperQuoteValue

    funcMap[ STRFROMANSI( "qlRateHelperQuoteValue" ) ]
        =  STRFROMANSI( "qlRateHelperQuoteValue" );
    funcDesc[ STRFROMANSI( "qlRateHelperQuoteValue" ) ]
        =  STRFROMANSI( "returns the value of the Quote wrapped in the given RateHelper object." );
    argName[ STRFROMANSI( "qlRateHelperQuoteValue" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlRateHelperQuoteValue" ) ].push_back( STRFROMANSI( "id of existing QuantLib::RateHelper object" ) );
    argName[ STRFROMANSI( "qlRateHelperQuoteValue" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlRateHelperQuoteValue" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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
    argName[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "SettlDays" ) );
    argDesc[ STRFROMANSI( "qlSwapRateHelper2" ) ].push_back( STRFROMANSI( "Number of days to spot date. Default value = 2." ) );
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

    // Schedules

    // qlSchedule

    funcMap[ STRFROMANSI( "qlSchedule" ) ]
        =  STRFROMANSI( "qlSchedule" );
    funcDesc[ STRFROMANSI( "qlSchedule" ) ]
        =  STRFROMANSI( "Construct an object of class Schedule and return its id" );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "EffectiveDate" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "effective date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "TerminationDate" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "termination date." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Tenor" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "tenor (e.g. 2D for two days , 3W for three weeks, 6M for six months, 1Y for one year)." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET). Default value = NullCalendar." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Convention" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "accrual dates business day convention. Default value = Unadjusted." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "TermDateConv" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "termination date business day convention. Default value = Unadjusted." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "GenRule" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "Date generation rule (Backward, Forward, ThirdWednesday, Twentieth, TwentiethIMM, Zero). Default value = Backward." ) );
    argName[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "EndOfMonth" ) );
    argDesc[ STRFROMANSI( "qlSchedule" ) ].push_back( STRFROMANSI( "end of month convention. Ignored for Tenor below 1M. Default value = false." ) );
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

    // qlScheduleBDC

    funcMap[ STRFROMANSI( "qlScheduleBDC" ) ]
        =  STRFROMANSI( "qlScheduleBDC" );
    funcDesc[ STRFROMANSI( "qlScheduleBDC" ) ]
        =  STRFROMANSI( "returns the business day convention used to calculate the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleBDC" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleBDC" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleBDC" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleBDC" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleCalendar

    funcMap[ STRFROMANSI( "qlScheduleCalendar" ) ]
        =  STRFROMANSI( "qlScheduleCalendar" );
    funcDesc[ STRFROMANSI( "qlScheduleCalendar" ) ]
        =  STRFROMANSI( "returns the Calendar used to calculate the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleCalendar" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleCalendar" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleCalendar" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleCalendar" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleDates

    funcMap[ STRFROMANSI( "qlScheduleDates" ) ]
        =  STRFROMANSI( "qlScheduleDates" );
    funcDesc[ STRFROMANSI( "qlScheduleDates" ) ]
        =  STRFROMANSI( "returns the dates for the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleDates" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleDates" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleDates" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleDates" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleEmpty

    funcMap[ STRFROMANSI( "qlScheduleEmpty" ) ]
        =  STRFROMANSI( "qlScheduleEmpty" );
    funcDesc[ STRFROMANSI( "qlScheduleEmpty" ) ]
        =  STRFROMANSI( "returns TRUE if the given Schedule object is empty." );
    argName[ STRFROMANSI( "qlScheduleEmpty" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleEmpty" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleEmpty" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleEmpty" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleEndDate

    funcMap[ STRFROMANSI( "qlScheduleEndDate" ) ]
        =  STRFROMANSI( "qlScheduleEndDate" );
    funcDesc[ STRFROMANSI( "qlScheduleEndDate" ) ]
        =  STRFROMANSI( "returns the end date of the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleEndDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleEndDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleEndDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleEndDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleEndOfMonth

    funcMap[ STRFROMANSI( "qlScheduleEndOfMonth" ) ]
        =  STRFROMANSI( "qlScheduleEndOfMonth" );
    funcDesc[ STRFROMANSI( "qlScheduleEndOfMonth" ) ]
        =  STRFROMANSI( "returns TRUE if end-of-month convention has been used to calculate the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleEndOfMonth" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleEndOfMonth" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleEndOfMonth" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleEndOfMonth" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleFromDateVector

    funcMap[ STRFROMANSI( "qlScheduleFromDateVector" ) ]
        =  STRFROMANSI( "qlScheduleFromDateVector" );
    funcDesc[ STRFROMANSI( "qlScheduleFromDateVector" ) ]
        =  STRFROMANSI( "Construct an object of class Schedule and return its id" );
    argName[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "EffectiveDate" ) );
    argDesc[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "date vector. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlScheduleFromDateVector" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlScheduleIsRegular

    funcMap[ STRFROMANSI( "qlScheduleIsRegular" ) ]
        =  STRFROMANSI( "qlScheduleIsRegular" );
    funcDesc[ STRFROMANSI( "qlScheduleIsRegular" ) ]
        =  STRFROMANSI( "returns TRUE if the selected period in the given Schedule object is regular." );
    argName[ STRFROMANSI( "qlScheduleIsRegular" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleIsRegular" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleIsRegular" ) ].push_back( STRFROMANSI( "Index" ) );
    argDesc[ STRFROMANSI( "qlScheduleIsRegular" ) ].push_back( STRFROMANSI( "one based period index. Default value = 1." ) );
    argName[ STRFROMANSI( "qlScheduleIsRegular" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleIsRegular" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleNextDate

    funcMap[ STRFROMANSI( "qlScheduleNextDate" ) ]
        =  STRFROMANSI( "qlScheduleNextDate" );
    funcDesc[ STRFROMANSI( "qlScheduleNextDate" ) ]
        =  STRFROMANSI( "returns the lowest date in the given Schedule object following the input reference date." );
    argName[ STRFROMANSI( "qlScheduleNextDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleNextDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleNextDate" ) ].push_back( STRFROMANSI( "RefDate" ) );
    argDesc[ STRFROMANSI( "qlScheduleNextDate" ) ].push_back( STRFROMANSI( "reference date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlScheduleNextDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleNextDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSchedulePreviousDate

    funcMap[ STRFROMANSI( "qlSchedulePreviousDate" ) ]
        =  STRFROMANSI( "qlSchedulePreviousDate" );
    funcDesc[ STRFROMANSI( "qlSchedulePreviousDate" ) ]
        =  STRFROMANSI( "returns the highest date in the given Schedule object preceding the input reference date." );
    argName[ STRFROMANSI( "qlSchedulePreviousDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSchedulePreviousDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlSchedulePreviousDate" ) ].push_back( STRFROMANSI( "RefDate" ) );
    argDesc[ STRFROMANSI( "qlSchedulePreviousDate" ) ].push_back( STRFROMANSI( "reference date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlSchedulePreviousDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSchedulePreviousDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleRule

    funcMap[ STRFROMANSI( "qlScheduleRule" ) ]
        =  STRFROMANSI( "qlScheduleRule" );
    funcDesc[ STRFROMANSI( "qlScheduleRule" ) ]
        =  STRFROMANSI( "returns the DateGeneration::Rule of the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleRule" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleRule" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleRule" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleRule" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleSize

    funcMap[ STRFROMANSI( "qlScheduleSize" ) ]
        =  STRFROMANSI( "qlScheduleSize" );
    funcDesc[ STRFROMANSI( "qlScheduleSize" ) ]
        =  STRFROMANSI( "returns the number of dates in the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleSize" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleStartDate

    funcMap[ STRFROMANSI( "qlScheduleStartDate" ) ]
        =  STRFROMANSI( "qlScheduleStartDate" );
    funcDesc[ STRFROMANSI( "qlScheduleStartDate" ) ]
        =  STRFROMANSI( "returns the start date of the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleStartDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleStartDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleStartDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleStartDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleTenor

    funcMap[ STRFROMANSI( "qlScheduleTenor" ) ]
        =  STRFROMANSI( "qlScheduleTenor" );
    funcDesc[ STRFROMANSI( "qlScheduleTenor" ) ]
        =  STRFROMANSI( "returns the tenor used to calculate the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleTenor" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleTenor" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleTenor" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleTenor" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleTerminationDateBDC

    funcMap[ STRFROMANSI( "qlScheduleTerminationDateBDC" ) ]
        =  STRFROMANSI( "qlScheduleTerminationDateBDC" );
    funcDesc[ STRFROMANSI( "qlScheduleTerminationDateBDC" ) ]
        =  STRFROMANSI( "returns the business day convention used to calculate the termination date of the given Schedule object." );
    argName[ STRFROMANSI( "qlScheduleTerminationDateBDC" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleTerminationDateBDC" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Schedule object" ) );
    argName[ STRFROMANSI( "qlScheduleTerminationDateBDC" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleTerminationDateBDC" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlScheduleTruncated

    funcMap[ STRFROMANSI( "qlScheduleTruncated" ) ]
        =  STRFROMANSI( "qlScheduleTruncated" );
    funcDesc[ STRFROMANSI( "qlScheduleTruncated" ) ]
        =  STRFROMANSI( "Construct an object of class Schedule and return its id" );
    argName[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "OriginalSchedule" ) );
    argDesc[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "original schedule object ID." ) );
    argName[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "TruncationDate" ) );
    argDesc[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "truncation date." ) );
    argName[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlScheduleTruncated" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // SequenceStatistics

    // qlSequenceStatistics

    funcMap[ STRFROMANSI( "qlSequenceStatistics" ) ]
        =  STRFROMANSI( "qlSequenceStatistics" );
    funcDesc[ STRFROMANSI( "qlSequenceStatistics" ) ]
        =  STRFROMANSI( "Construct an object of class SequenceStatistics and return its id" );
    argName[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "sample dimensionality. Default value = 0." ) );
    argName[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSequenceStatistics2

    funcMap[ STRFROMANSI( "qlSequenceStatistics2" ) ]
        =  STRFROMANSI( "qlSequenceStatistics2" );
    funcDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ]
        =  STRFROMANSI( "Construct an object of class SequenceStatistics and return its id" );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "sample dimensionality. Default value = 0." ) );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Values" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Sampled values. If omitted, an empty statistics is created." ) );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Weights" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "If omitted, all sampled values have the same weight. Default value = std::vector<QuantLib::Real>()." ) );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatistics2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSequenceStatisticsAverageShortfall

    funcMap[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsAverageShortfall" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ]
        =  STRFROMANSI( "Returns the averaged shortfallness for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ].push_back( STRFROMANSI( "the target." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsAverageShortfall" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsCorrelation

    funcMap[ STRFROMANSI( "qlSequenceStatisticsCorrelation" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsCorrelation" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsCorrelation" ) ]
        =  STRFROMANSI( "Returns the correlation Matrix for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsCorrelation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsCorrelation" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsCorrelation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsCorrelation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsCovariance

    funcMap[ STRFROMANSI( "qlSequenceStatisticsCovariance" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsCovariance" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsCovariance" ) ]
        =  STRFROMANSI( "Returns the covariance Matrix for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsCovariance" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsCovariance" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsCovariance" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsCovariance" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsDownsideDeviation

    funcMap[ STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" ) ]
        =  STRFROMANSI( "Returns the square root of the downside variance for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsDownsideDeviation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsDownsideVariance

    funcMap[ STRFROMANSI( "qlSequenceStatisticsDownsideVariance" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsDownsideVariance" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsDownsideVariance" ) ]
        =  STRFROMANSI( "Returns the variance of observations below zero for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsDownsideVariance" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsDownsideVariance" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsDownsideVariance" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsDownsideVariance" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsErrorEstimate

    funcMap[ STRFROMANSI( "qlSequenceStatisticsErrorEstimate" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsErrorEstimate" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsErrorEstimate" ) ]
        =  STRFROMANSI( "Returns the error estimate on the mean value for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsErrorEstimate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsErrorEstimate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsErrorEstimate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsErrorEstimate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsGaussianAverageShortfall

    funcMap[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ]
        =  STRFROMANSI( "Returns the averaged shortfallness for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ].push_back( STRFROMANSI( "the target." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianAverageShortfall" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsGaussianPercentile

    funcMap[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ]
        =  STRFROMANSI( "Returns the x-th percentile for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ].push_back( STRFROMANSI( "X" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ].push_back( STRFROMANSI( "Must be in the range (0,1]." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPercentile" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsGaussianPotentialUpside

    funcMap[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ]
        =  STRFROMANSI( "Returns the reciprocal of VAR at a given percentile for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ].push_back( STRFROMANSI( "the percentile." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianPotentialUpside" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsGaussianShortfall

    funcMap[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ]
        =  STRFROMANSI( "Returns the probability of missing the given target for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ].push_back( STRFROMANSI( "the target." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianShortfall" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsGaussianValueAtRisk

    funcMap[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ]
        =  STRFROMANSI( "Returns the value-at-risk at a given percentile for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ].push_back( STRFROMANSI( "the percentile." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsGaussianValueAtRisk" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsInc

    funcMap[ STRFROMANSI( "qlSequenceStatisticsInc" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsInc" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsInc" ) ]
        =  STRFROMANSI( "Construct an object of class SequenceStatisticsInc and return its id" );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "sample dimensionality. Default value = 0." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSequenceStatisticsInc2

    funcMap[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsInc2" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ]
        =  STRFROMANSI( "Construct an object of class SequenceStatisticsInc and return its id" );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Dimension" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "sample dimensionality. Default value = 0." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Values" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Sampled values. If omitted, an empty statistics is created." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Weights" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "If omitted, all sampled values have the same weight. Default value = std::vector<QuantLib::Real>()." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsInc2" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSequenceStatisticsKurtosis

    funcMap[ STRFROMANSI( "qlSequenceStatisticsKurtosis" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsKurtosis" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsKurtosis" ) ]
        =  STRFROMANSI( "Returns the excess kurtosis for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsKurtosis" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsKurtosis" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsKurtosis" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsKurtosis" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsMax

    funcMap[ STRFROMANSI( "qlSequenceStatisticsMax" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsMax" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsMax" ) ]
        =  STRFROMANSI( "Returns the maximum sample value for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsMax" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsMax" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsMax" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsMax" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsMean

    funcMap[ STRFROMANSI( "qlSequenceStatisticsMean" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsMean" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsMean" ) ]
        =  STRFROMANSI( "Returns the mean for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsMean" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsMean" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsMean" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsMean" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsMin

    funcMap[ STRFROMANSI( "qlSequenceStatisticsMin" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsMin" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsMin" ) ]
        =  STRFROMANSI( "Returns the minimum sample value for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsMin" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsMin" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsMin" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsMin" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsPercentile

    funcMap[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsPercentile" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ]
        =  STRFROMANSI( "Returns the x-th percentile for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ].push_back( STRFROMANSI( "X" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ].push_back( STRFROMANSI( "Must be in the range (0,1]." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsPercentile" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsPotentialUpside

    funcMap[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsPotentialUpside" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ]
        =  STRFROMANSI( "Returns the reciprocal of VAR at a given percentile for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ].push_back( STRFROMANSI( "Centile" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ].push_back( STRFROMANSI( "the centile." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsPotentialUpside" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsRegret

    funcMap[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsRegret" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ]
        =  STRFROMANSI( "Returns the variance of observations below target for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ].push_back( STRFROMANSI( "the target." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsRegret" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsSamples

    funcMap[ STRFROMANSI( "qlSequenceStatisticsSamples" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsSamples" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsSamples" ) ]
        =  STRFROMANSI( "Returns the number of samples collected for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsSamples" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSamples" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsSamples" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSamples" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsSemiDeviation

    funcMap[ STRFROMANSI( "qlSequenceStatisticsSemiDeviation" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsSemiDeviation" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsSemiDeviation" ) ]
        =  STRFROMANSI( "Returns the square root of the semivariance for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsSemiDeviation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSemiDeviation" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsSemiDeviation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSemiDeviation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsSemiVariance

    funcMap[ STRFROMANSI( "qlSequenceStatisticsSemiVariance" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsSemiVariance" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsSemiVariance" ) ]
        =  STRFROMANSI( "Returns the variance of observations below the mean for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsSemiVariance" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSemiVariance" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsSemiVariance" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSemiVariance" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsShortfall

    funcMap[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsShortfall" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ]
        =  STRFROMANSI( "Returns the probability of missing the given target for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ].push_back( STRFROMANSI( "the target." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsShortfall" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsSize

    funcMap[ STRFROMANSI( "qlSequenceStatisticsSize" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsSize" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsSize" ) ]
        =  STRFROMANSI( "Returns the size (sample dimensionality) for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsSize" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSize" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsSize" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSize" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsSkewness

    funcMap[ STRFROMANSI( "qlSequenceStatisticsSkewness" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsSkewness" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsSkewness" ) ]
        =  STRFROMANSI( "Returns the skewness for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsSkewness" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSkewness" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsSkewness" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsSkewness" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsStandardDeviation

    funcMap[ STRFROMANSI( "qlSequenceStatisticsStandardDeviation" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsStandardDeviation" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsStandardDeviation" ) ]
        =  STRFROMANSI( "Returns the the standard deviation for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsStandardDeviation" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsStandardDeviation" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsStandardDeviation" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsStandardDeviation" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsValueAtRisk

    funcMap[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsValueAtRisk" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ]
        =  STRFROMANSI( "Returns the value-at-risk at a given percentile for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ].push_back( STRFROMANSI( "Target" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ].push_back( STRFROMANSI( "the percentile." ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsValueAtRisk" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsVariance

    funcMap[ STRFROMANSI( "qlSequenceStatisticsVariance" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsVariance" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsVariance" ) ]
        =  STRFROMANSI( "Returns the variance for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsVariance" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsVariance" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsVariance" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsVariance" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSequenceStatisticsWeightSum

    funcMap[ STRFROMANSI( "qlSequenceStatisticsWeightSum" ) ]
        =  STRFROMANSI( "qlSequenceStatisticsWeightSum" );
    funcDesc[ STRFROMANSI( "qlSequenceStatisticsWeightSum" ) ]
        =  STRFROMANSI( "Returns the sum of data weights for the given SequenceStatistics object." );
    argName[ STRFROMANSI( "qlSequenceStatisticsWeightSum" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsWeightSum" ) ].push_back( STRFROMANSI( "id of existing QuantLib::SequenceStatistics object" ) );
    argName[ STRFROMANSI( "qlSequenceStatisticsWeightSum" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSequenceStatisticsWeightSum" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Settings

    // qlSettingsEnforceTodaysHistoricFixings

    funcMap[ STRFROMANSI( "qlSettingsEnforceTodaysHistoricFixings" ) ]
        =  STRFROMANSI( "qlSettingsEnforceTodaysHistoricFixings" );
    funcDesc[ STRFROMANSI( "qlSettingsEnforceTodaysHistoricFixings" ) ]
        =  STRFROMANSI( "returns the current value of the boolean which enforce the usage of historic fixings for today's date." );
    argName[ STRFROMANSI( "qlSettingsEnforceTodaysHistoricFixings" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsEnforceTodaysHistoricFixings" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSettingsEvaluationDate

    funcMap[ STRFROMANSI( "qlSettingsEvaluationDate" ) ]
        =  STRFROMANSI( "qlSettingsEvaluationDate" );
    funcDesc[ STRFROMANSI( "qlSettingsEvaluationDate" ) ]
        =  STRFROMANSI( "returns the current value of the Evaluation Date." );
    argName[ STRFROMANSI( "qlSettingsEvaluationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsEvaluationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSettingsSetEnforceTodaysHistoricFixings

    funcMap[ STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" ) ]
        =  STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" );
    funcDesc[ STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" ) ]
        =  STRFROMANSI( "sets the value of the boolean which enforce the usage of historic fixings for today's date." );
    argName[ STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" ) ].push_back( STRFROMANSI( "Boolean" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" ) ].push_back( STRFROMANSI( "new value for the boolean which enforce the usage of historic fixings for today's date. Default value = true." ) );
    argName[ STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEnforceTodaysHistoricFixings" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSettingsSetEvaluationDate

    funcMap[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ]
        =  STRFROMANSI( "qlSettingsSetEvaluationDate" );
    funcDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ]
        =  STRFROMANSI( "sets the value of the Evaluation Date." );
    argName[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "EvalDate" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "new value for the evaluation date. If a null date is provided the current date wiil be used and midnight date change will be detected. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSettingsSetEvaluationDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // Short Rate Models

    // qlHullWhite

    funcMap[ STRFROMANSI( "qlHullWhite" ) ]
        =  STRFROMANSI( "qlHullWhite" );
    funcDesc[ STRFROMANSI( "qlHullWhite" ) ]
        =  STRFROMANSI( "Construct an object of class HullWhite and return its id" );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "A" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "a." ) );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "Sigma" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "volatility." ) );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlHullWhite" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlVasicek

    funcMap[ STRFROMANSI( "qlVasicek" ) ]
        =  STRFROMANSI( "qlVasicek" );
    funcDesc[ STRFROMANSI( "qlVasicek" ) ]
        =  STRFROMANSI( "Construct an object of class Vasicek and return its id" );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "A" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "mean reverting speed." ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "B" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "short-rate limit value." ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "Lambda" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "risk premium." ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "Sigma" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "volatility." ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlVasicek" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // Swap

    // qlMakeCms

    funcMap[ STRFROMANSI( "qlMakeCms" ) ]
        =  STRFROMANSI( "qlMakeCms" );
    funcDesc[ STRFROMANSI( "qlMakeCms" ) ]
        =  STRFROMANSI( "Construct an object of class Swap and return its id" );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "SwapTenor" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "swap tenor period." ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "SwapIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "SwapIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "IborSpread" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "spread over the ibor leg. Default value = QuantLib::Null<QuantLib::Spread>()." ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "forward start period." ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "CmsCouponPricer" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "CmsCouponPricer object ID." ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeCms" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlSwapLegBPS

    funcMap[ STRFROMANSI( "qlSwapLegBPS" ) ]
        =  STRFROMANSI( "qlSwapLegBPS" );
    funcDesc[ STRFROMANSI( "qlSwapLegBPS" ) ]
        =  STRFROMANSI( "returns the BPS of the i-th leg for the given Swap object." );
    argName[ STRFROMANSI( "qlSwapLegBPS" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapLegBPS" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swap object" ) );
    argName[ STRFROMANSI( "qlSwapLegBPS" ) ].push_back( STRFROMANSI( "LegNumber" ) );
    argDesc[ STRFROMANSI( "qlSwapLegBPS" ) ].push_back( STRFROMANSI( "Zero based leg number (e.g. use 0 for the first leg, 1 for the second leg, etc.)." ) );
    argName[ STRFROMANSI( "qlSwapLegBPS" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapLegBPS" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSwapLegNPV

    funcMap[ STRFROMANSI( "qlSwapLegNPV" ) ]
        =  STRFROMANSI( "qlSwapLegNPV" );
    funcDesc[ STRFROMANSI( "qlSwapLegNPV" ) ]
        =  STRFROMANSI( "returns the NPV of the i-th leg for the given Swap object." );
    argName[ STRFROMANSI( "qlSwapLegNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapLegNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swap object" ) );
    argName[ STRFROMANSI( "qlSwapLegNPV" ) ].push_back( STRFROMANSI( "LegNumber" ) );
    argDesc[ STRFROMANSI( "qlSwapLegNPV" ) ].push_back( STRFROMANSI( "Zero based leg number (e.g. use 0 for the first leg, 1 for the second leg, etc.)." ) );
    argName[ STRFROMANSI( "qlSwapLegNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapLegNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSwapMaturityDate

    funcMap[ STRFROMANSI( "qlSwapMaturityDate" ) ]
        =  STRFROMANSI( "qlSwapMaturityDate" );
    funcDesc[ STRFROMANSI( "qlSwapMaturityDate" ) ]
        =  STRFROMANSI( "Returns the maturity (i.e. last payment) date for the given Swap object." );
    argName[ STRFROMANSI( "qlSwapMaturityDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapMaturityDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swap object" ) );
    argName[ STRFROMANSI( "qlSwapMaturityDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapMaturityDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSwapStartDate

    funcMap[ STRFROMANSI( "qlSwapStartDate" ) ]
        =  STRFROMANSI( "qlSwapStartDate" );
    funcDesc[ STRFROMANSI( "qlSwapStartDate" ) ]
        =  STRFROMANSI( "Returns the start (i.e. first accrual) date for the given Swap object." );
    argName[ STRFROMANSI( "qlSwapStartDate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwapStartDate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swap object" ) );
    argName[ STRFROMANSI( "qlSwapStartDate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwapStartDate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlSwaptionImpliedVolatility

    funcMap[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ]
        =  STRFROMANSI( "qlSwaptionImpliedVolatility" );
    funcDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ]
        =  STRFROMANSI( "Returns the volatility implied by the given price for the given Swaption object." );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swaption object" ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Price" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Price used to infer the implied volatility." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "YieldCurve" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "discounting YieldTermStructure object ID." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Guess" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Volatility guess. Default value = 0.10." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Accuracy" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "solver accuracy. Default value = 1.0e-6." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "MaxIter" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "solver max iterations. Default value = 100." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "MinVol" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Minimum volatility, no lower solution. Default value = 1.0e-7." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "MaxVol" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Maximum volatility, no higher solution. Default value = 4.0." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Displacement" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "displacement in a displaced diffusion model. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwaptionImpliedVolatility" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSwaptionSettlementType

    funcMap[ STRFROMANSI( "qlSwaptionSettlementType" ) ]
        =  STRFROMANSI( "qlSwaptionSettlementType" );
    funcDesc[ STRFROMANSI( "qlSwaptionSettlementType" ) ]
        =  STRFROMANSI( "returns the settlement type (Cash or Delivery) for the given Swaption object." );
    argName[ STRFROMANSI( "qlSwaptionSettlementType" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwaptionSettlementType" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swaption object" ) );
    argName[ STRFROMANSI( "qlSwaptionSettlementType" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwaptionSettlementType" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlSwaptionType

    funcMap[ STRFROMANSI( "qlSwaptionType" ) ]
        =  STRFROMANSI( "qlSwaptionType" );
    funcDesc[ STRFROMANSI( "qlSwaptionType" ) ]
        =  STRFROMANSI( "returns the type (Payer or Receiver) for the given Swaption object." );
    argName[ STRFROMANSI( "qlSwaptionType" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlSwaptionType" ) ].push_back( STRFROMANSI( "id of existing QuantLib::Swaption object" ) );
    argName[ STRFROMANSI( "qlSwaptionType" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlSwaptionType" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlInterpolatedYieldCurve

    funcMap[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ]
        =  STRFROMANSI( "qlInterpolatedYieldCurve" );
    funcDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ]
        =  STRFROMANSI( "Construct an object of class InterpolatedYieldCurve and return its id" );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Dates" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "vector of dates, the first one being the one at which discount factor = 1.0" ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Data" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "data vector. Each element should be of the type selected by TraitsID" ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Calendar" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "holiday calendar (e.g. TARGET) to advance from global EvaluationDate." ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "DayCounter" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "DayCounter ID. Default value = Actual/365 (Fixed)." ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Jumps" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Jump quotes vector." ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "JumpDates" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Jump dates vector." ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "TraitsID" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Discount, ZeroYield, or ForwardRate. Default value = Discount." ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "InterpolatorID" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "BackwardFlat, ForwardFlat, Linear, LogLinear, LogParabolic, KrugerLogCubic, etc. Default value = MonotonicLogCubicNaturalSpline." ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlInterpolatedYieldCurve" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

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

    // qlYieldTSDiscount

    funcMap[ STRFROMANSI( "qlYieldTSDiscount" ) ]
        =  STRFROMANSI( "qlYieldTSDiscount" );
    funcDesc[ STRFROMANSI( "qlYieldTSDiscount" ) ]
        =  STRFROMANSI( "Returns a discount factor from the given YieldTermStructure object." );
    argName[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "id of existing QuantLib::YieldTermStructure object" ) );
    argName[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "DfDates" ) );
    argDesc[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "vector of dates." ) );
    argName[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "AllowExtrapolation" ) );
    argDesc[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "TRUE allows extrapolation. Default value = false." ) );
    argName[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlYieldTSDiscount" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlYieldTSZeroRate

    funcMap[ STRFROMANSI( "qlYieldTSZeroRate" ) ]
        =  STRFROMANSI( "qlYieldTSZeroRate" );
    funcDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ]
        =  STRFROMANSI( "Returns the zero interest rate from the given YieldTermStructure object." );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::YieldTermStructure object" ) );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "Dates" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "date." ) );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "ResultDayCounter" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "resultDayCounter." ) );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "Compounding" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "Interest rate coumpounding rule (Simple:1+rt, Compounded:(1+r)^t, Continuous:e^{rt}). Default value = Continuous." ) );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "Frequency" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "frequency (e.g. Annual, Semiannual, Every4Month, Quarterly, Bimonthly, Monthly). Default value = Annual." ) );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "AllowExtrapolation" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "TRUE allows extrapolation. Default value = false." ) );
    argName[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlYieldTSZeroRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlMakeIMMSwap

    funcMap[ STRFROMANSI( "qlMakeIMMSwap" ) ]
        =  STRFROMANSI( "qlMakeIMMSwap" );
    funcDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ]
        =  STRFROMANSI( "Construct an object of class VanillaSwap and return its id" );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "SwapTenor" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "swap tenor period (e.g. 2Y)." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "floating IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "the fixed leg rate. If missing atm rate is used. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "FirstImmDate" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "First (IMM) date. Default value = QuantLib::Date()." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "fixed leg day counter. Default value = 30/360 (Bond Basis)." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "floating leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "PricingEngineID" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "DiscountSwapEngine object ID." ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMakeIMMSwap" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

    // qlMakeVanillaSwap

    funcMap[ STRFROMANSI( "qlMakeVanillaSwap" ) ]
        =  STRFROMANSI( "qlMakeVanillaSwap" );
    funcDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ]
        =  STRFROMANSI( "Construct an object of class VanillaSwap and return its id" );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "SettlDays" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "Number of days to spot date. Default value = 2." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "SwapTenor" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "swap tenor period (e.g. 5Y)." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "IborIndex" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "floating IborIndex object ID." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "FixedRate" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "the fixed leg rate. If missing atm rate is used. Default value = QuantLib::Null<QuantLib::Rate>()." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "ForwardStart" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "forward start period (from spot date)." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "FixDayCounter" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "fixed leg day counter. Default value = DayCounter." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "Spread" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "floating leg spread. Default value = 0.0." ) );
    argName[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "PricingEngineID" ) );
    argDesc[ STRFROMANSI( "qlMakeVanillaSwap" ) ].push_back( STRFROMANSI( "DiscountSwapEngine object ID." ) );
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

    // qlVanillaSwapFixedLegNPV

    funcMap[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ]
        =  STRFROMANSI( "qlVanillaSwapFixedLegNPV" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ]
        =  STRFROMANSI( "returns the NPV of the fixed rate leg for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedLegNPV" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapFixedRate

    funcMap[ STRFROMANSI( "qlVanillaSwapFixedRate" ) ]
        =  STRFROMANSI( "qlVanillaSwapFixedRate" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapFixedRate" ) ]
        =  STRFROMANSI( "returns the fixed leg rate for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapFixedRate" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedRate" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapFixedRate" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapFixedRate" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // qlVanillaSwapNominal

    funcMap[ STRFROMANSI( "qlVanillaSwapNominal" ) ]
        =  STRFROMANSI( "qlVanillaSwapNominal" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapNominal" ) ]
        =  STRFROMANSI( "returns the swap nominal for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapNominal" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapNominal" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapNominal" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapNominal" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

    // qlVanillaSwapSpread

    funcMap[ STRFROMANSI( "qlVanillaSwapSpread" ) ]
        =  STRFROMANSI( "qlVanillaSwapSpread" );
    funcDesc[ STRFROMANSI( "qlVanillaSwapSpread" ) ]
        =  STRFROMANSI( "returns the spread over floating rate for the given VanillaSwap object." );
    argName[ STRFROMANSI( "qlVanillaSwapSpread" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapSpread" ) ].push_back( STRFROMANSI( "id of existing QuantLib::VanillaSwap object" ) );
    argName[ STRFROMANSI( "qlVanillaSwapSpread" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlVanillaSwapSpread" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );

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

    // Market Model Volatility

    // qlMarketModelLmExtLinearExponentialVolModel

    funcMap[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ]
        =  STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" );
    funcDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ]
        =  STRFROMANSI( "Construct an object of class LmExtLinearExponentialVolModel and return its id" );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "ObjectId" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "id of object to be created" ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "FixingTimes" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "rate fixing times." ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "A" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "a parameter." ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "B" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "b parameter." ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "C" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "c parameter." ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "D" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "d parameter." ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "Permanent" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "object permanent/nonpermanent" ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "Trigger" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "dependency tracking trigger" ) );
    argName[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "Overwrite" ) );
    argDesc[ STRFROMANSI( "qlMarketModelLmExtLinearExponentialVolModel" ) ].push_back( STRFROMANSI( "overwrite flag" ) );

}

