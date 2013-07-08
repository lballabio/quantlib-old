/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2004 Ferdinando Ametrano
 copyright (C) 2011 Bojan Nikolic <bojan@bnikolic.co.uk>

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 Example of shocking interest rate curves by using the Handle/Quote
 mechanism. Reworked using code and data from the swapvaluation.cpp
 example in QuantLib.
*/

#include <boost/format.hpp>

#include <ql/quantlib.hpp>

using namespace QuantLib;


/// A structure to hold all the market quotes for a single curve
/// together
struct CurveData {

	boost::shared_ptr<SimpleQuote>  d1wRate;
	boost::shared_ptr<SimpleQuote>  d1mRate;
	boost::shared_ptr<SimpleQuote>  d3mRate;
	boost::shared_ptr<SimpleQuote>  d6mRate;
	boost::shared_ptr<SimpleQuote>  d9mRate;
	boost::shared_ptr<SimpleQuote>  d1yRate;

	boost::shared_ptr<SimpleQuote>  fra3x6Rate;
	boost::shared_ptr<SimpleQuote>  fra6x9Rate;
	boost::shared_ptr<SimpleQuote>  fra6x12Rate;

	boost::shared_ptr<SimpleQuote>  fut1Price;
	boost::shared_ptr<SimpleQuote>  fut2Price;
	boost::shared_ptr<SimpleQuote>  fut3Price;
	boost::shared_ptr<SimpleQuote>  fut4Price;
	boost::shared_ptr<SimpleQuote>  fut5Price;
	boost::shared_ptr<SimpleQuote>  fut6Price;
	boost::shared_ptr<SimpleQuote>  fut7Price;
	boost::shared_ptr<SimpleQuote>  fut8Price;

	boost::shared_ptr<SimpleQuote>  s2yRate;
	boost::shared_ptr<SimpleQuote>  s3yRate;
	boost::shared_ptr<SimpleQuote>  s5yRate;
	boost::shared_ptr<SimpleQuote>  s10yRate;
	boost::shared_ptr<SimpleQuote>  s15yRate;

	CurveData():
		d1wRate(new SimpleQuote()),
		d1mRate(new SimpleQuote()),
		d3mRate(new SimpleQuote()),
		d6mRate(new SimpleQuote()),
		d9mRate(new SimpleQuote()),
		d1yRate(new SimpleQuote()),

		fra3x6Rate(new SimpleQuote()),
		fra6x9Rate(new SimpleQuote()),
		fra6x12Rate(new SimpleQuote()),

		fut1Price(new SimpleQuote()),
		fut2Price(new SimpleQuote()),
		fut3Price(new SimpleQuote()),
		fut4Price(new SimpleQuote()),
		fut5Price(new SimpleQuote()),
		fut6Price(new SimpleQuote()),
		fut7Price(new SimpleQuote()),
		fut8Price(new SimpleQuote()),
		s2yRate(new SimpleQuote()),
		s3yRate(new SimpleQuote()),
		s5yRate(new SimpleQuote()),
		s10yRate(new SimpleQuote()),
		s15yRate(new SimpleQuote())
	{
	}
};

// Fill out the curve with sample market data (from more optimistic
// times!)
void sampleMktData(CurveData &cd)
{
	(*cd.d1wRate)=0.0382;

	(*cd.d1mRate)=0.0372;
	(*cd.d3mRate)=0.0363;
	(*cd.d6mRate)=0.0353;
	(*cd.d9mRate)=0.0348;
	(*cd.d1yRate)=0.0345;

	(*cd.fra3x6Rate)=0.037125;
	(*cd.fra6x9Rate)=0.037125;
	(*cd.fra6x12Rate)=0.037125;

	(*cd.fut1Price)=96.2875;
	(*cd.fut2Price)=96.7875;
	(*cd.fut3Price)=96.9875;
	(*cd.fut4Price)=96.6875;
	(*cd.fut5Price)=96.4875;
	(*cd.fut6Price)=96.3875;
	(*cd.fut7Price)=96.2875;
	(*cd.fut8Price)=96.0875;
	// swaps
	(*cd.s2yRate)=0.037125;
	(*cd.s3yRate)=0.0398;
	(*cd.s5yRate)=0.0443;
	(*cd.s10yRate)=0.05165;
	(*cd.s15yRate)=0.055175;
}

/// Build a yield curve. Note that the resulting curve is linked to
/// the supplied data and will change with it
boost::shared_ptr<YieldTermStructure> buildCurve(const CurveData &cd)
{
		Calendar calendar = TARGET();
		Date settlementDate(1, September, 2010);
		// must be a business day
		settlementDate = calendar.adjust(settlementDate);

		Integer fixingDays = 2;
		Date todaysDate = calendar.advance(settlementDate, -fixingDays, Days);
		// nothing to do with Date::todaysDate
		Settings::instance().evaluationDate() = todaysDate;


		todaysDate = Settings::instance().evaluationDate();

		DayCounter depositDayCounter = Actual360();

		boost::shared_ptr<RateHelper> d1w(new DepositRateHelper(
			Handle<Quote>(cd.d1wRate),
			1*Weeks, fixingDays,
			calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> d1m(new DepositRateHelper(
			Handle<Quote>(cd.d1mRate),
			1*Months, fixingDays,
			calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> d3m(new DepositRateHelper(
			Handle<Quote>(cd.d3mRate),
			3*Months, fixingDays,
			calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> d6m(new DepositRateHelper(
			Handle<Quote>(cd.d6mRate),
			6*Months, fixingDays,
			calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> d9m(new DepositRateHelper(
			Handle<Quote>(cd.d9mRate),
			9*Months, fixingDays,
			calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> d1y(new DepositRateHelper(
			Handle<Quote>(cd.d1yRate),
			1*Years, fixingDays,
			calendar, ModifiedFollowing,
			true, depositDayCounter));


		// setup FRAs
		boost::shared_ptr<RateHelper> fra3x6(new FraRateHelper(
			Handle<Quote>(cd.fra3x6Rate),
			3, 6, fixingDays, calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> fra6x9(new FraRateHelper(
			Handle<Quote>(cd.fra6x9Rate),
			6, 9, fixingDays, calendar, ModifiedFollowing,
			true, depositDayCounter));
		boost::shared_ptr<RateHelper> fra6x12(new FraRateHelper(
			Handle<Quote>(cd.fra6x12Rate),
			6, 12, fixingDays, calendar, ModifiedFollowing,
			true, depositDayCounter));


		// setup futures
		// Rate convexityAdjustment = 0.0;
		Integer futMonths = 3;
		Date imm = IMM::nextDate(settlementDate);
		boost::shared_ptr<RateHelper> fut1(new FuturesRateHelper(
			Handle<Quote>(cd.fut1Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut2(new FuturesRateHelper(
			Handle<Quote>(cd.fut2Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut3(new FuturesRateHelper(
			Handle<Quote>(cd.fut3Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut4(new FuturesRateHelper(
			Handle<Quote>(cd.fut4Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut5(new FuturesRateHelper(
			Handle<Quote>(cd.fut5Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut6(new FuturesRateHelper(
			Handle<Quote>(cd.fut6Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut7(new FuturesRateHelper(
			Handle<Quote>(cd.fut7Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));
		imm = IMM::nextDate(imm+1);
		boost::shared_ptr<RateHelper> fut8(new FuturesRateHelper(
			Handle<Quote>(cd.fut8Price),
			imm,
			futMonths, calendar, ModifiedFollowing,
			true, depositDayCounter));


		// setup swaps
		Frequency swFixedLegFrequency = Annual;
		BusinessDayConvention swFixedLegConvention = Unadjusted;
		DayCounter swFixedLegDayCounter = Thirty360(Thirty360::European);
		boost::shared_ptr<IborIndex> swFloatingLegIndex(new Euribor6M);

		boost::shared_ptr<RateHelper> s2y(new SwapRateHelper(
			Handle<Quote>(cd.s2yRate), 2*Years,
			calendar, swFixedLegFrequency,
			swFixedLegConvention, swFixedLegDayCounter,
			swFloatingLegIndex));
		boost::shared_ptr<RateHelper> s3y(new SwapRateHelper(
			Handle<Quote>(cd.s3yRate), 3*Years,
			calendar, swFixedLegFrequency,
			swFixedLegConvention, swFixedLegDayCounter,
			swFloatingLegIndex));
		boost::shared_ptr<RateHelper> s5y(new SwapRateHelper(
			Handle<Quote>(cd.s5yRate), 5*Years,
			calendar, swFixedLegFrequency,
			swFixedLegConvention, swFixedLegDayCounter,
			swFloatingLegIndex));
		boost::shared_ptr<RateHelper> s10y(new SwapRateHelper(
			Handle<Quote>(cd.s10yRate), 10*Years,
			calendar, swFixedLegFrequency,
			swFixedLegConvention, swFixedLegDayCounter,
			swFloatingLegIndex));
		boost::shared_ptr<RateHelper> s15y(new SwapRateHelper(
			Handle<Quote>(cd.s15yRate), 15*Years,
			calendar, swFixedLegFrequency,
			swFixedLegConvention, swFixedLegDayCounter,
			swFloatingLegIndex));


		/*********************
		 **  CURVE BUILDING **
		 *********************/

		// Any DayCounter would be fine.
		// ActualActual::ISDA ensures that 30 years is 30.0
		DayCounter termStructureDayCounter =
			ActualActual(ActualActual::ISDA);


		double tolerance = 1.0e-15;

		// A depo-swap curve
		std::vector<boost::shared_ptr<RateHelper> > depoSwapInstruments;
		depoSwapInstruments.push_back(d1w);
		depoSwapInstruments.push_back(d1m);
		depoSwapInstruments.push_back(d3m);
		depoSwapInstruments.push_back(d6m);
		depoSwapInstruments.push_back(d9m);
		depoSwapInstruments.push_back(d1y);
		depoSwapInstruments.push_back(s2y);
		depoSwapInstruments.push_back(s3y);
		depoSwapInstruments.push_back(s5y);
		depoSwapInstruments.push_back(s10y);
		depoSwapInstruments.push_back(s15y);
		boost::shared_ptr<YieldTermStructure> depoSwapTermStructure(
			new PiecewiseYieldCurve<Discount,LogLinear>(
										  settlementDate, depoSwapInstruments,
										  termStructureDayCounter,
										  tolerance));


		// A depo-futures-swap curve
		std::vector<boost::shared_ptr<RateHelper> > depoFutSwapInstruments;
		depoFutSwapInstruments.push_back(d1w);
		depoFutSwapInstruments.push_back(d1m);
		depoFutSwapInstruments.push_back(fut1);
		depoFutSwapInstruments.push_back(fut2);
		depoFutSwapInstruments.push_back(fut3);
		depoFutSwapInstruments.push_back(fut4);
		depoFutSwapInstruments.push_back(fut5);
		depoFutSwapInstruments.push_back(fut6);
		depoFutSwapInstruments.push_back(fut7);
		depoFutSwapInstruments.push_back(fut8);
		depoFutSwapInstruments.push_back(s3y);
		depoFutSwapInstruments.push_back(s5y);
		depoFutSwapInstruments.push_back(s10y);
		depoFutSwapInstruments.push_back(s15y);
		boost::shared_ptr<YieldTermStructure> depoFutSwapTermStructure(
			new PiecewiseYieldCurve<Discount,LogLinear>(
									   settlementDate, depoFutSwapInstruments,
									   termStructureDayCounter,
									   tolerance));


		// A depo-FRA-swap curve
		std::vector<boost::shared_ptr<RateHelper> > depoFRASwapInstruments;
		depoFRASwapInstruments.push_back(d1w);
		depoFRASwapInstruments.push_back(d1m);
		depoFRASwapInstruments.push_back(d3m);
		depoFRASwapInstruments.push_back(fra3x6);
		depoFRASwapInstruments.push_back(fra6x9);
		depoFRASwapInstruments.push_back(fra6x12);
		depoFRASwapInstruments.push_back(s2y);
		depoFRASwapInstruments.push_back(s3y);
		depoFRASwapInstruments.push_back(s5y);
		depoFRASwapInstruments.push_back(s10y);
		depoFRASwapInstruments.push_back(s15y);
		boost::shared_ptr<YieldTermStructure> depoFRASwapTermStructure(
			new PiecewiseYieldCurve<Discount,LogLinear>(
									   settlementDate, depoFRASwapInstruments,
									   termStructureDayCounter,
									   tolerance));
	return depoFRASwapTermStructure;
}

int main(void)
{
	CurveData cd;

	// Fill out with some sample market data
	sampleMktData(cd);

	// Build a curve linked to this market data
	boost::shared_ptr<YieldTermStructure> ocurve=buildCurve(cd);

	// Print month headings
	for (size_t i=0; i<24; ++i)
		std::cout << boost::format("Month %i , ") %i;
	std::cout<<std::endl;

	// Print original curve
	std::cout << std::endl << "Print original curve" << std::endl;
	for (size_t i=0; i<24; ++i)
		std::cout << boost::format("%g , ") % ocurve->zeroRate( i / 12.0,   Compounded ).rate();
	std::cout<<std::endl;

	// Shock the 3month depo
	std::cout << std::endl << "Shock the 3month depo" << std::endl;
	(*cd.d3mRate)=cd.d3mRate->value()+0.01;

	// Print the bumped, rebuilt curve
	for (size_t i=0; i<24; ++i)
		std::cout << boost::format("%g ,") % ocurve->zeroRate( i / 12.0,   Compounded ).rate();
	std::cout<<std::endl;

}
