        //! Enumeration of day counting conventions for switch-on-type code.
		  enum Type { Actual365, Actual360, ActActEuro, Thirty360,
			ThirtyE360, ThirtyEplus360, ActActBond, ActActISDA,
			Thirty360ISDA, ThirtyE360ISDA };

/*
 Copyright (C) 2002 Ferdinando Ametrano

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
/*! \file qlxl.cpp
    \brief QuantLib Excel add-in

    \fullpath
    qlxl/%qlxl.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>

extern "C"
{

    using namespace QuantLib;
//    using QuantLib::DayCounters::Actual360;
//    using QuantLib::DayCounters::Actual365;
//    using QuantLib::DayCounters::ActualActual;
//    using QuantLib::DayCounters::Thirty360;
    using QuantLib::Pricers::EuropeanOption;
    using QuantLib::Pricers::McEuropean;
    using QuantLib::Pricers::FdEuropean;

    // helper function for option payoff: MAX((stike-underlying),0), etc.
    using QuantLib::Pricers::ExercisePayoff;


    LPXLOPER EXCEL_EXPORT xlQLversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(QL_VERSION);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlQLhexversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(double(QL_HEX_VERSION));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlXLWversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(XLW_VERSION);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlXLWhexversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(double(XLW_HEX_VERSION));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlQLXLversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(QLXL_VERSION);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlQLXLhexversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(double(QLXL_HEX_VERSION));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlaccrualDays(
                        XlfOper xlDate1,
                        XlfOper xlDate2,
                        XlfOper xlDayCountType)
    {
        EXCEL_BEGIN;

        std::string inputString(xlDayCountType.AsString());
        std::string s = StringFormatter::toLowercase(inputString);
        DayCounter dc = DayCounters::Actual365();


        if (s == "1" || s == "act365" || s == "act/365")
            dc = DayCounters::Actual365();
        else if (s == "2" || s == "act360" || s == "act/360")
            dc = DayCounters::Actual360();
        else if (s == "3" || s == "actacte" || s == "act/act(e)" || s == "act/act(Euro)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Euro);
        else if (s == "4" || s == "30/360" || s == "30/360us")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "5" || s == "30e/360" || s == "30/360e" || s == "30/360eu")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else if (s == "6" || s == "30/360i" || s == "30/360it")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::Italian);
        else if (s == "7" || s == "actact" || s == "act/act" || s == "act/act(b)" || s == "act/act (Bond)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Bond);
        else if (s == "8" || s == "actacth" || s == "act/act(h)" || s == "act/act (ISDA)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Historical);
        else if (s == "9" || s == "30/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "10"|| s == "30e/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else
            throw Error("Unknown day counter: " + inputString);


		Date d1(xlDate1.AsInt());
		Date d2(xlDate2.AsInt());

		double result = Functions::accrualDays(dc, d1, d2);
        return XlfOper(result);

        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlaccrualFactor(
                        XlfOper xlDate1,
                        XlfOper xlDate2,
                        XlfOper xlDayCountType,
                        XlfOper xlDate3,
                        XlfOper xlDate4)
    {
        EXCEL_BEGIN;

        std::string inputString(xlDayCountType.AsString());
        std::string s = StringFormatter::toLowercase(inputString);
        DayCounter dc = DayCounters::Actual365();


        if (s == "1" || s == "act365" || s == "act/365")
            dc = DayCounters::Actual365();
        else if (s == "2" || s == "act360" || s == "act/360")
            dc = DayCounters::Actual360();
        else if (s == "3" || s == "actacte" || s == "act/act(e)" || s == "act/act(Euro)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Euro);
        else if (s == "4" || s == "30/360" || s == "30/360us")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "5" || s == "30e/360" || s == "30/360e" || s == "30/360eu")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else if (s == "6" || s == "30/360i" || s == "30/360it")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::Italian);
        else if (s == "7" || s == "actact" || s == "act/act" || s == "act/act(b)" || s == "act/act (Bond)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Bond);
        else if (s == "8" || s == "actacth" || s == "act/act(h)" || s == "act/act (ISDA)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Historical);
        else if (s == "9" || s == "30/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "10"|| s == "30e/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else
            throw Error("Unknown day counter: " + inputString);


		Date d1(xlDate1.AsInt());
		Date d2(xlDate2.AsInt());

        Date d3 = Date();
        Date d4 = Date();
/*
        if (!xlDate3.IsMissing()) d3(xlDate3.AsInt());
        if (!xlDate4.IsMissing)() d4(xlDate4.AsInt());
*/

        double result = Functions::accrualFactor(dc, d1, d2, d3, d4);
        return XlfOper(result);

        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlBlackScholes(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        std::string temp(xltype.AsString());
        temp = StringFormatter::toLowercase(temp);

        Option::Type type;
        if (temp == "c" || temp == "call") {
            type = Option::Call;
        } else if (temp == "p" || temp == "put") {
            type = Option::Put;
        } else if (temp == "s" || temp == "straddle") {
            type = Option::Straddle;
        } else
            throw Error("Unknown option type");

        double underlying    = xlunderlying.AsDouble();
        double strike        = xlstrike.AsDouble();
        double dividendYield = xldividendYield.AsDouble();
        double riskFreeRate  = xlriskFreeRate.AsDouble();
        double maturity      = xlmaturity.AsDouble();
        double volatility    = xlvolatility.AsDouble();




        EuropeanOption eur(type, underlying, strike, dividendYield,
           riskFreeRate, maturity, volatility);
        double value = eur.value();

        return XlfOper(value);
        EXCEL_END;
    }


    long EXCEL_EXPORT xlAutoOpen()
    {
//        EXCEL_BEGIN;

        // Displays a message in the status bar.
        XlfExcel::Instance().SendMessage(
            "Registering QuantLib XL v"
            QLXL_VERSION
            " library...");

        
        
        // Registers qlversion
        XlfFuncDesc QLversion("xlQLversion","QLversion",
            "QuantLib version string","qlUtilities");
        QLversion.Register();

        // Registers qlhexversion
        XlfFuncDesc QLhexversion("xlQLhexversion","QLhexversion",
            "QuantLib version number","qlUtilities");
        QLhexversion.Register();

        // Registers xlwversion
        XlfFuncDesc XLWversion("xlXLWversion","XLWversion",
            "QuantLib version string","qlUtilities");
        XLWversion.Register();

        // Registers xlwhexversion
        XlfFuncDesc XLWhexversion("xlXLWhexversion","XLWhexversion",
            "QuantLib version number","qlUtilities");
        XLWhexversion.Register();

        // Registers qlxlversion
        XlfFuncDesc QLXLversion("xlQLXLversion","QLXLversion",
            "QuantLib version string","qlUtilities");
        QLXLversion.Register();

        // Registers qlxlhexversion
        XlfFuncDesc QLXLhexversion("xlQLXLhexversion","QLXLhexversion",
            "QuantLib version number","qlUtilities");
        QLXLhexversion.Register();

        // Registers accrual_days
        XlfArgDesc d01("date1", "first date");
        XlfArgDesc d02("date2", "second date");
        XlfArgDesc d03("date3", "reference period first date");
        XlfArgDesc d04("date4", "reference period last date");
        XlfArgDesc p03("dayCount", "accrual convention");

        XlfFuncDesc accrualDaysDesc("xlaccrualDays","qlAccrual_days",
            "Accrual days","qlDates");
        accrualDaysDesc.SetArguments(d01+d02+p03);
        accrualDaysDesc.Register();

        XlfFuncDesc accrualFactorDesc("xlaccrualFactor","qlAccrual_factor",
            "Accrual factor","qlDates");
        accrualFactorDesc.SetArguments(d01+d02+p03+d03+d04);
        accrualFactorDesc.Register();

        // Registers Black-Scholes
        XlfArgDesc          type(         "type", "Option type");
        XlfArgDesc    underlying(   "underlying", "Diameter of the circle");
        XlfArgDesc        strike(       "strike", "Option's strike");
        XlfArgDesc dividendYield("dividendYield", "Dividend yield");
        XlfArgDesc  riskFreeRate( "riskFreeRate", "Risk free rate");
        XlfArgDesc      maturity(     "maturity", "option's maturity");
        XlfArgDesc    volatility(   "volatility", "Underlying's volatility");

        XlfFuncDesc blackscholes("xlBlackScholes","BlackScholes",
            "Black Scholes formula","qlPricers");
        blackscholes.SetArguments(type+underlying+strike+dividendYield+riskFreeRate+maturity+volatility);
        blackscholes.Register();


        // Clears the status bar.
        XlfExcel::Instance().SendMessage();
        return 1;

//        EXCEL_END;
    }

}
