/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

#include "volatilitycubetest.hpp"
#include "customutilities.hpp"

#include <ql/experimental/volatility/abcdatmvolcurve.hpp>
#include <ql/experimental/volatility/blackvolsurface.hpp>
#include <ql/experimental/volatility/equityfxvolsurface.hpp>
#include <ql/experimental/volatility/extendedblackvariancecurve.hpp>
#include <ql/experimental/volatility/extendedblackvariancesurface.hpp>
#include <ql/experimental/volatility/interestratevolsurface.hpp>
#include <ql/experimental/volatility/sabrvolsurface.hpp>
#include <ql/experimental/volatility/volcube.hpp>

#include <ql/termstructures/volatility/swaption/swaptionvolcube.hpp>

#include <ql/quantlib.hpp>

using namespace QuantLib;

void VolatilityCubeTest::testVolatilityCube()
{
	/*
	VolatilityCube(const std::vector<Handle<InterestRateVolSurface> >&,
		const std::vector<Handle<AbcdAtmVolCurve> >&);

	AbcdAtmVolCurve(Natural settlementDays,
		const Calendar& cal,
		const std::vector<Period>& optionTenors,
		const std::vector<Handle<Quote> >& volsHandles,
		const std::vector<bool> inclusionInInterpolationFlag
		= std::vector<bool>(1, true),
		BusinessDayConvention bdc = Following,
		const DayCounter& dc = Actual365Fixed());

	SwaptionVolatilityCube(
		const Handle<SwaptionVolatilityStructure>& atmVolStructure,
		const std::vector<Period>& optionTenors,
		const std::vector<Period>& swapTenors,
		const std::vector<Spread>& strikeSpreads,
		const std::vector<std::vector<Handle<Quote> > >& volSpreads,
		const boost::shared_ptr<SwapIndex>& swapIndexBase,
		const boost::shared_ptr<SwapIndex>& shortSwapIndexBase,
		bool vegaWeightedSmileFit);
	*/
}